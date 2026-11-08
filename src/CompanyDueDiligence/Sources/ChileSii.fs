namespace CompanyDueDiligence

open System
open System.Collections.Generic
open System.IO
open System.IO.Compression
open System.Text
open System.Threading

/// The SII's nómina de personas jurídicas, read from PUB_NOMBRES_PJ.zip and PUB_NOM_DIRECCIONES.zip as downloaded on 2026-11-08 (files of
/// 2026-08-03): tab-delimited UTF-8 text, with no quoting.
module internal ChileSii =

    let jurisdiction = Jurisdiction.ofCode "CL"

    let coverage: RegistryCoverage =
        {
            Registry = "SII nómina de personas jurídicas (Chile)"
            Jurisdictions = [ jurisdiction ]
            Standing =
                Support.Partial
                    "Start and end of activities for tax only: an end date (término de giro) reads as closed, and anything else as registered"
            Concerns = Support.NotOffered "The nómina states no filing dates, disputes or insolvency"
            Addresses = Support.Partial "The current domicile only; branches (sucursales) aren't read"
            Search = Support.Partial "Names containing the term, over the file, up to 50"
            Officers = Support.NotOffered "The nómina lists no representatives"
            Owners = Support.NotOffered "The nómina lists no partners or shareholders"
            Cost = AccessCost.Free
            RateLimit = RateLimitPolicy.NotPublished(1, TimeSpan.FromDays 1.)
            Licence = "None stated; the SII disclaims the accuracy of the nómina"
        }

    let invalidNumber =
        "A RUT is digits and a check digit (0-9 or K), such as 76.123.456-7"

    /// The check digit of a RUT's digits: modulo 11 with weights 2 to 7 from the right, 11 reading 0 and 10 reading K.
    let checkDigit (digits: string) =
        let sum =
            digits
            |> Seq.rev
            |> Seq.mapi (fun i c -> (int c - int '0') * (2 + i % 6))
            |> Seq.sum

        match 11 - sum % 11 with
        | 11 -> '0'
        | 10 -> 'K'
        | n -> char (int '0' + n)

    /// A RUT with its check digit, dots and dash optional, as "76123456-7"; one whose check digit doesn't match isn't valid.
    let normalNumber (number: string) =
        let text =
            (if isNull number then "" else number).Trim().Replace(".", "").Replace("-", "").ToUpperInvariant()

        if text.Length >= 2 && text.Length <= 10 then
            let digits = text.Substring(0, text.Length - 1).TrimStart '0'
            let check = text[text.Length - 1]

            if
                digits <> ""
                && digits |> Seq.forall Char.IsAsciiDigit
                && checkDigit digits = check
            then
                ValueSome $"{digits}-{check}"
            else
                ValueNone
        else
            ValueNone

    /// An end of activities (FECHA_TG_VIG) is closed; without one the nómina holds no status word, so it is registered with none.
    let standing (endedOn: string voption) =
        match endedOn with
        | ValueSome day -> CompanyStanding.Closed $"Término de giro {day}"
        | ValueNone -> CompanyStanding.Registered ValueNone

    /// A line of the files as a record: they quote nothing, so a quote is text.
    let private rowOf (line: string) =
        {
            Raw = line
            Fields = BulkFile.split '\t' false line
        }

    /// A header line's columns; a header lacking a required column fails the file.
    let private headerOf (required: string list) (line: string) =
        let header = BulkFile.header (BulkFile.split '\t' false line)

        if required |> List.forall header.ContainsKey then
            header
        else
            raise (InvalidDataException "A header lacks a column the layout has")

    /// The first line of an entry's text as its header, and the lines after it, as they stream; ValueNone for an empty text.
    let private headed (entry: ZipArchiveEntry) =
        match BulkFile.splitAt (fun _ -> true) (BulkFile.entryLines BulkFile.strictUtf8 entry) with
        | struct (_, ValueSome header, lines) -> ValueSome(struct (header, lines))
        | struct (_, ValueNone, _) -> ValueNone

    /// Subtype descriptions and kinds from CODIGO_TIPO_SUBTIPO_.txt, by code.
    let private subtypes (archive: ZipArchive) =
        match
            archive.Entries
            |> Seq.tryFind (fun e -> e.Name.StartsWith("CODIGO_TIPO_SUBTIPO", StringComparison.OrdinalIgnoreCase))
        with
        | None -> Map.empty
        | Some entry ->
            match headed entry with
            | ValueNone -> Map.empty
            | ValueSome(struct (head, lines)) ->
                let header = BulkFile.header (BulkFile.split '\t' false head)

                lines
                |> Seq.choose (fun line ->
                    let row = rowOf line

                    match BulkFile.cell header "COD_SUBTIPO" row with
                    | ValueSome code ->
                        Some(code, (BulkFile.cell header "SUBTIPO" row, BulkFile.cell header "TIPO" row))
                    | ValueNone -> None)
                |> Map.ofSeq

    /// A RUT from a row's RUT and DV columns.
    let private rutOf (header: IReadOnlyDictionary<string, int>) (row: DelimitedRecord) =
        ValueOption.map2 (fun rut dv -> rut + "-" + dv) (BulkFile.cell header "RUT" row) (BulkFile.cell header "DV" row)
        |> ValueOption.bind normalNumber

    /// A current domicile's address from its row.
    let private domicile (header: IReadOnlyDictionary<string, int>) (row: DelimitedRecord) =
        let value name = BulkFile.cell header name row

        let street =
            match
                [ value "CALLE"; value "NUMERO"; value "BLOQUE"; value "DEPARTAMENTO" ]
                |> List.choose ValueOption.toOption
            with
            | [] -> ValueNone
            | parts -> ValueSome(String.concat " " parts)

        Bulk.companyAddress
            (AddressKind.Other "DOMICILIO")
            [ street; value "VILLA_POBLACION"; value "CIUDAD" ]
            (value "COMUNA")
            (value "REGION")
            ValueNone
            ValueNone

    /// Builds the register from the names zip and the addresses zip as published. Only current domiciles (VIGENCIA S) are kept. A names line
    /// with fewer fields than its header is unreadable, as a missing end date would read as registered. Each legal person is held as its lines,
    /// as UTF-8, and read when asked for: the nómina holds millions.
    let build (names: Stream) (addresses: Stream) (date: BulkFileDate) (ct: CancellationToken) : ICompanyRegistry =
        use namesZip = BulkFile.openZip names
        use addressesZip = BulkFile.openZip addresses
        let kinds = subtypes namesZip
        let index = BulkIndex<byte array>()
        // RUTs on a line too short to read, so that another line for one isn't taken alone.
        let shortRows = HashSet<string>()

        // Both files stream line by line: names first, then each current domicile is tied to its RUT.
        let namesHeader, skipped =
            match headed (BulkFile.entry namesZip "PUB_NOMBRES_PJ.txt") with
            | ValueNone -> BulkFile.header [||], 0
            | ValueSome(struct (head, lines)) ->
                let header = headerOf [ "RUT"; "DV"; "RAZON_SOCIAL"; "FECHA_TG_VIG" ] head
                let width = (header.Values |> Seq.max) + 1

                header,
                lines
                |> Seq.fold
                    (fun skipped line ->
                        ct.ThrowIfCancellationRequested()

                        if line = "" then
                            skipped
                        else
                            let row = rowOf line

                            match rutOf header row, BulkFile.cell header "RAZON_SOCIAL" row with
                            | ValueSome number, ValueSome _ when row.Fields.Length < width ->
                                shortRows.Add number |> ignore

                                if index.Withhold number then
                                    index.Repeat number

                                skipped + 1
                            | ValueSome number, ValueSome _ when index.Repeated.Contains number -> skipped
                            | ValueSome number, ValueSome _ when shortRows.Contains number ->
                                index.Repeat number
                                skipped
                            | ValueSome number, ValueSome _ ->
                                index.Add(number, Encoding.UTF8.GetBytes line)
                                skipped
                            | _ -> skipped + 1)
                    0

        let addressHeader, domiciles =
            match headed (BulkFile.entry addressesZip "PUB_NOM_DOMICILIO.txt") with
            | ValueNone -> BulkFile.header [||], readOnlyDict []
            | ValueSome(struct (head, lines)) ->
                let header = headerOf [ "RUT"; "DV"; "VIGENCIA"; "CALLE" ] head

                header,
                lines
                |> Seq.choose (fun line ->
                    ct.ThrowIfCancellationRequested()
                    let row = rowOf line

                    match rutOf header row with
                    | ValueSome number when
                        line <> ""
                        && BulkFile.cell header "VIGENCIA" row = ValueSome "S"
                        && index.Held.ContainsKey number
                        && (domicile header row).IsSome
                        ->
                        Some(struct (number, Encoding.UTF8.GetBytes line))
                    | _ -> None)
                |> Bulk.grouped

        let cell name (row: DelimitedRecord) = BulkFile.cell namesHeader name row

        let build (held: byte array) : BulkRecord =
            let row = rowOf (Encoding.UTF8.GetString held)
            let number = rutOf namesHeader row |> ValueOption.defaultValue ""
            let subtypeCode = cell "COD_SUBTIPO" row
            let endedOn = cell "FECHA_TG_VIG" row

            let subtype, kind =
                match
                    subtypeCode
                    |> ValueOption.bind (fun code -> kinds.TryFind code |> ValueOption.ofOption)
                with
                | ValueSome(subtype, kind) -> subtype, kind
                | ValueNone -> subtypeCode, ValueNone

            let current =
                Bulk.listed domiciles number
                |> List.choose (fun bytes ->
                    let line = Encoding.UTF8.GetString bytes

                    domicile addressHeader (rowOf line)
                    |> ValueOption.map (fun address -> line, address)
                    |> ValueOption.toOption)

            {
                Company =
                    {
                        Name = cell "RAZON_SOCIAL" row |> ValueOption.defaultValue ""
                        OtherNames = []
                        Number = number
                        Jurisdiction = jurisdiction
                        JurisdictionAsPublished = ValueNone
                        Standing = standing endedOn
                        Concerns = CompanyConcerns.notPublished
                        StatusAsPublished = endedOn |> ValueOption.map (sprintf "FECHA_TG_VIG %s")
                        LegalForm = subtype
                        IncorporatedOn = BulkFile.date "dd-MM-yyyy" (cell "FECHA_INICIO_VIG" row)
                        CeasedOn = BulkFile.date "dd-MM-yyyy" endedOn
                        Addresses =
                            match current with
                            | [] -> NotAvailable "The addresses file holds no current domicile for this RUT"
                            | found -> Available(found |> List.map snd)
                        Role =
                            match kind with
                            | ValueSome "SOCIEDADES EXTRANJERAS" -> RecordRole.ForeignRegistration
                            | ValueSome _ -> RecordRole.Home
                            | ValueNone -> RecordRole.NotStated
                        HomeRegistration = ValueNone
                        EstablishmentOf = ValueNone
                    }
                Officers = ValueNone
                Owners = ValueNone
                Raw = String.concat "\n" (row.Raw :: (current |> List.map fst))
            }

        let nameOf (held: byte array) =
            cell "RAZON_SOCIAL" (rowOf (Encoding.UTF8.GetString held))
            |> ValueOption.defaultValue ""

        BulkRegistry(
            coverage,
            normalNumber,
            invalidNumber,
            LazyRecords<byte array>(index.Held, build, nameOf),
            date,
            BulkAbsence.CantTell(
                $"The nómina lists legal persons known to the SII for tax, not every entity on a companies register; {skipped} lines couldn't be read"
            ),
            "The nómina lists no representatives",
            (fun _ -> NotAvailable "The nómina lists no partners or shareholders"),
            index.Repeated
        )
