namespace CompanyDueDiligence

open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions

/// The Superintendencia de Compañías, Valores y Seguros' directorio de compañías, read from directorio_companias.xlsx as downloaded on
/// 2026-11-08 ("FECHA DE ACTUALIZACION: 06/11/2026").
module internal Ecuador =

    let jurisdiction = Jurisdiction.ofCode "EC"

    let coverage: RegistryCoverage =
        {
            Registry = "Supercias directorio de compañías (Ecuador)"
            Jurisdictions = [ jurisdiction ]
            Standing = Support.Supported
            Concerns = Support.NotOffered "The directory states no filing dates, disputes or insolvency"
            Addresses = Support.Supported
            Search = Support.Partial "Names containing the term, over the file, up to 50"
            Officers = Support.Partial "The legal representative only, with the post as published and no dates"
            Owners = Support.NotOffered "The directory lists no shareholders"
            Cost = AccessCost.Free
            RateLimit = RateLimitPolicy.NotPublished(1, TimeSpan.FromDays 1.)
            Licence = "None stated; on datosabiertos.gob.ec with no licence"
        }

    let invalidNumber = "A RUC has 13 digits; a Supercias expediente has up to 7"

    /// The key of a 13-digit RUC or of an expediente number of up to 7 digits.
    let normalNumber (number: string) =
        let text = if isNull number then "" else number.Trim()

        if text <> "" && text |> Seq.forall Char.IsAsciiDigit then
            if text.Length = 13 then
                ValueSome("RUC " + text)
            elif text.Length <= 7 && text.TrimStart('0') <> "" then
                ValueSome("EXPEDIENTE " + text.TrimStart('0'))
            else
                ValueNone
        else
            ValueNone

    /// The SITUACIÓN LEGAL words in the file of 2026-11-06. Any other is Unknown.
    let standing (word: string voption) =
        match word |> ValueOption.map _.Trim() with
        | ValueNone
        | ValueSome "" -> CompanyStanding.Unknown ValueNone
        | ValueSome "ACTIVA" -> CompanyStanding.Active
        | ValueSome("INACTIVA" as word)
        | ValueSome("NO SUJETO CONTROL Y VIGILANCIA SCVS(ART. 432/OTRO)" as word) ->
            CompanyStanding.Registered(ValueSome word)
        | ValueSome("DISOLUCIÓN Y LIQUIDACIÓN OFICIO INSCRITA EN RM" as word)
        | ValueSome("DISOLUCIÓN Y LIQUIDACIÓN DE PLENO DERECHO INSC. RM" as word)
        | ValueSome("DISOLUCIÓN Y LIQUIDACIÓN OFICIO NO INSCRITA EN RM" as word)
        | ValueSome("DISOLUCIÓN Y LIQUIDACIÓN ANTICIPADA INSCRITA RM" as word)
        | ValueSome("DISOLUCIÓN Y LIQUIDACIÓ DE PLENO DERECHO NO INS RM" as word)
        | ValueSome("DISOLUCIÓN Y LIQUIDACIÓN ANTIC. NO INSCRITA EN RM" as word) -> CompanyStanding.WindingUp word
        | ValueSome("CANCELACIÓN PERMISO OPERACIÓN - OFICIO INSCRITA RM" as word)
        | ValueSome("CANCELACIÓN PERMISO OPERACIÓN - OFICIO NO INSCRITA" as word)
        | ValueSome("CANCELACIÓN PERMISO OPERACIÓN - VOLUNT INSCRITA RM" as word)
        | ValueSome("CANCELACIÓN PERMISO OPERACIÓN - VOLUNT NO INSCRITA" as word)
        | ValueSome("DEPURADO/ELIMINADO POR DUPLICIDAD" as word)
        | ValueSome("MIGRADO A SEGUROS" as word) -> CompanyStanding.Closed word
        | ValueSome other -> CompanyStanding.Unknown(ValueSome other)

    let private spaces = Regex @"\s+"

    let private asOfRegex = Regex @"FECHA DE ACTUALIZACION:\s*(\d{2}/\d{2}/\d{4})"

    /// One row with its keys, RUC and expediente, or ValueNone when it lacks an expediente or a name. The number is the RUC, or the
    /// expediente for a row without one.
    let record (header: Map<string, int>) (row: string array) : struct (BulkRecord * string list) voption =
        let column name =
            match header.TryFind name with
            | Some i -> Xlsx.cell row i
            | None -> ValueNone

        // A RUC stored as a number loses the leading zero of provinces 01 to 09.
        let ruc =
            column "RUC"
            |> ValueOption.map (fun r ->
                if r.Length = 12 && r |> Seq.forall Char.IsAsciiDigit then
                    "0" + r
                else
                    r)
            |> ValueOption.filter (fun r -> r.Length = 13 && r |> Seq.forall Char.IsAsciiDigit)

        let expediente =
            column "EXPEDIENTE"
            |> ValueOption.bind normalNumber
            |> ValueOption.filter _.StartsWith("EXPEDIENTE ")

        match expediente, column "NOMBRE" with
        | ValueSome expediente, ValueSome name ->
            let status = column "SITUACIÓN LEGAL"
            let form = column "TIPO"

            let isBranch =
                form |> ValueOption.map (fun f -> spaces.Replace(f, " ")) = ValueSome "SUCURSAL EXTRANJERA"

            let country = column "PAÍS"

            let street =
                match [ column "CALLE"; column "NÚMERO" ] |> List.choose ValueOption.toOption with
                | [] -> ValueNone
                | parts -> ValueSome(String.concat " " parts)

            let representative =
                column "REPRESENTANTE"
                |> ValueOption.map (fun person ->
                    [
                        {
                            Name = person
                            Kind = PartyKind.Unknown ValueNone
                            Role = OfficerRole.Other(column "CARGO" |> ValueOption.defaultValue "REPRESENTANTE")
                            Current = true
                            AppointedOn = ValueNone
                            EndedOn = ValueNone
                            BornIn = ValueNone
                            Nationality = ValueNone
                            CountryOfResidence = ValueNone
                            Address = ValueNone
                            Entity = ValueNone
                            IdentityVerification = ValueNone
                        }
                    ])

            let raw =
                [
                    "EXPEDIENTE"
                    "RUC"
                    "NOMBRE"
                    "SITUACIÓN LEGAL"
                    "FECHA_CONSTITUCION"
                    "TIPO"
                    "PAÍS"
                    "PROVINCIA"
                    "CIUDAD"
                    "CALLE"
                    "NÚMERO"
                    "INTERSECCIÓN"
                    "BARRIO"
                    "REPRESENTANTE"
                    "CARGO"
                ]
                |> List.choose (fun label ->
                    column label
                    |> ValueOption.map (fun v -> $"{label}: {v}")
                    |> ValueOption.toOption)
                |> String.concat "\n"

            let record =
                {
                    Company =
                        {
                            Name = name
                            OtherNames = []
                            Number = ruc |> ValueOption.defaultValue (expediente.Substring "EXPEDIENTE ".Length)
                            Jurisdiction = jurisdiction
                            JurisdictionAsPublished = if isBranch then country else ValueNone
                            Standing = standing status
                            Concerns = CompanyConcerns.notPublished
                            StatusAsPublished = status
                            LegalForm = form
                            IncorporatedOn = BulkFile.date "dd/MM/yyyy" (column "FECHA_CONSTITUCION")
                            CeasedOn = ValueNone
                            Addresses =
                                match
                                    Bulk.companyAddress
                                        (AddressKind.Other "Domicilio")
                                        [
                                            street
                                            column "INTERSECCIÓN" |> ValueOption.map (sprintf "Y %s")
                                            column "BARRIO"
                                        ]
                                        (column "CIUDAD")
                                        (column "PROVINCIA")
                                        ValueNone
                                        ValueNone
                                with
                                | ValueSome address -> Available [ address ]
                                | ValueNone -> NotAvailable "The row holds no address"
                            Role =
                                if isBranch then RecordRole.Establishment
                                elif form.IsSome then RecordRole.Home
                                else RecordRole.NotStated
                            HomeRegistration =
                                if isBranch then
                                    ValueSome
                                        {
                                            LegalName = ValueNone
                                            LegalForm = ValueNone
                                            GoverningLaw = ValueNone
                                            Country = country
                                            Register = ValueNone
                                            RegistrationNumber = ValueNone
                                        }
                                else
                                    ValueNone
                            EstablishmentOf = ValueNone
                        }
                    Officers = representative
                    Owners = ValueNone
                    Raw = raw
                }

            ValueSome(struct (record, expediente :: (ruc |> ValueOption.map ((+) "RUC ") |> ValueOption.toList)))
        | _ -> ValueNone

    /// Builds the register from directorio_companias.xlsx as published: four title rows, a header row with EXPEDIENTE, then one row a company.
    /// The title's FECHA DE ACTUALIZACION is the AsOf.
    let build (file: Stream) (date: BulkFileDate) (ct: Threading.CancellationToken) : ICompanyRegistry =
        use archive = BulkFile.openZip file

        let isHeader (struct (_, row): struct (int * string array)) =
            row |> Array.exists (fun cell -> cell.Trim() = "EXPEDIENTE")

        // The sheet streams row by row; the title and header rows come first.
        match BulkFile.splitAt isHeader (Xlsx.rows archive "xl/worksheets/sheet1.xml") with
        | struct (_, ValueNone, _) -> BulkFile.fail "The sheet has no header row with EXPEDIENTE"
        | struct (title, ValueSome(struct (_, headerRow)), rows) ->
            let columns = headerRow |> Array.mapi (fun i text -> text.Trim(), i) |> Map.ofArray

            if not (columns.ContainsKey "NOMBRE" && columns.ContainsKey "RUC") then
                BulkFile.fail "The sheet lacks a name or RUC column"

            let stated = BulkFile.date "dd/MM/yyyy" (Xlsx.lastMatch asOfRegex title)
            let index = BulkIndex<BulkRecord>()

            // Rows read without a RUC, and rows that couldn't be read.
            let struct (withoutRuc, skipped) =
                rows
                |> Seq.fold
                    (fun (struct (withoutRuc, skipped)) (struct (_, row)) ->
                        ct.ThrowIfCancellationRequested()

                        match record columns row with
                        | ValueSome(struct (found, keys)) ->
                            index.AddEach(keys, found)
                            struct ((if keys.Length = 1 then withoutRuc + 1 else withoutRuc), skipped)
                        | ValueNone when row |> Array.exists (String.IsNullOrWhiteSpace >> not) ->
                            struct (withoutRuc, skipped + 1)
                        | ValueNone -> struct (withoutRuc, skipped))
                    (struct (0, 0))

            new BulkRegistry(
                coverage,
                normalNumber,
                invalidNumber,
                index.Held,
                Bulk.asOf date stated,
                BulkAbsence.CantTell(
                    $"The directory leaves out cancelled companies, {withoutRuc} rows have no RUC and {skipped} couldn't be read; a number it doesn't hold may still be registered"
                ),
                "The row names no legal representative",
                (fun _ -> NotAvailable "The directory lists no shareholders"),
                index.Repeated
            )
