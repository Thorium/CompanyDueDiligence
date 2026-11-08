namespace CompanyDueDiligence

open System
open System.Collections.Generic
open System.IO
open System.Text
open System.Text.RegularExpressions

/// SUNAT's padrón reducido del RUC, read from padron_reducido_ruc.zip as downloaded on 2026-11-08 (file of 2026-11-07): pipe-delimited, Latin-1,
/// with a pipe inside a field written \|. Only legal entities, RUCs starting 20, are read; RUCs of people (10, 15, 17) are left out.
module internal Peru =

    let jurisdiction = Jurisdiction.ofCode "PE"

    let coverage: RegistryCoverage =
        {
            Registry = "SUNAT padrón reducido del RUC (Peru)"
            Jurisdictions = [ jurisdiction ]
            Standing =
                Support.Partial
                    "The taxpayer status for tax, which is not the company's status in the companies register (SUNARP)"
            Concerns = Support.Partial "An address SUNAT couldn't confirm (NO HABIDO, NO HALLADO) only"
            Addresses = Support.Supported
            Search = Support.Partial "Names containing the term, over the file, up to 50"
            Officers = Support.NotOffered "The padrón reducido lists no representatives"
            Owners = Support.NotOffered "The padrón reducido lists no owners"
            Cost = AccessCost.Free
            RateLimit = RateLimitPolicy.NotPublished(1, TimeSpan.FromDays 1.)
            Licence = "None stated with the file"
        }

    let invalidNumber =
        "A company's RUC has 11 digits starting 20, the last a check digit; RUCs of people (10, 15, 17) aren't read"

    /// An 11-digit RUC starting 20 whose last digit is 11 less the first ten weighted 5, 4, 3, 2, 7, 6, 5, 4, 3, 2 modulo 11, 10 reading 0 and
    /// 11 reading 1.
    let normalNumber (number: string) =
        let text = if isNull number then "" else number.Trim()

        if
            text.Length = 11
            && text.StartsWith("20", StringComparison.Ordinal)
            && text |> Seq.forall Char.IsAsciiDigit
        then
            let sum =
                Seq.zip [ 5; 4; 3; 2; 7; 6; 5; 4; 3; 2 ] text
                |> Seq.sumBy (fun (w, c) -> w * (int c - int '0'))

            let check =
                match 11 - sum % 11 with
                | 10 -> 0
                | 11 -> 1
                | n -> n

            if check = int text[10] - int '0' then
                ValueSome text
            else
                ValueNone
        else
            ValueNone

    /// The ESTADO DEL CONTRIBUYENTE words for RUCs starting 20 in the file of 2026-11-07, which SUNAT cuts at 20 characters. They are a tax
    /// registration's status, so an active one is Registered with that said, and a suspended or pending one is Unknown. Any other is Unknown.
    let standing (word: string voption) =
        match word |> ValueOption.map _.Trim() with
        | ValueNone
        | ValueSome "" -> CompanyStanding.Unknown ValueNone
        | ValueSome("ACTIVO" as word)
        | ValueSome("OTROS OBLIGADOS" as word) -> CompanyStanding.Registered(ValueSome $"{word} (tax registration)")
        | ValueSome("SUSPENSION TEMPORAL" as word)
        | ValueSome("PENDIENTE DE INI. DE" as word) -> CompanyStanding.Unknown(ValueSome $"{word} (tax registration)")
        | ValueSome("BAJA DEFINITIVA" as word)
        | ValueSome("BAJA DE OFICIO" as word)
        | ValueSome("BAJA PROV. POR OFICI" as word)
        | ValueSome("BAJA PROVISIONAL" as word)
        | ValueSome("BAJA MULT.INSCR. Y O" as word)
        | ValueSome("BAJA NO ACTIV. SISEV" as word)
        | ValueSome("INHABILITADO-VENT.UN" as word)
        | ValueSome("ANULACION - ERROR SU" as word)
        | ValueSome("ANUL.PROVI.-ACTO ILI" as word) -> CompanyStanding.Closed word
        | ValueSome other -> CompanyStanding.Unknown(ValueSome other)

    /// The CONDICIÓN DE DOMICILIO words in the same file. NO HABIDO and every NO HALLADO state the address undeliverable; HABIDO settles it; the
    /// rest leave it unconfirmed. Nothing else is published.
    let concerns (word: string voption) =
        let others =
            CompanyConcern.all
            |> List.filter ((<>) CompanyConcern.RegisteredOfficeUndeliverable)

        match word |> ValueOption.map _.Trim() with
        | ValueSome "HABIDO" -> { Stated = []; Unconfirmed = others }
        | ValueSome word when word = "NO HABIDO" || word.StartsWith("NO HALLADO", StringComparison.Ordinal) ->
            {
                Stated = [ CompanyConcern.RegisteredOfficeUndeliverable ]
                Unconfirmed = others
            }
        | _ -> CompanyConcerns.notPublished

    let private escapedPipe = Regex @"(?<!\\)\|"

    /// The fields of one line: split at pipes not escaped, with \| read as a pipe.
    let fields (line: string) =
        if line.IndexOf '\\' < 0 then
            line.Split '|'
        else
            escapedPipe.Split line |> Array.map (fun field -> field.Replace(@"\|", "|"))

    /// A field, with SUNAT's dashes for nothing read as absent.
    let private value (fields: string array) (i: int) =
        if i < fields.Length then
            BulkFile.nonBlank fields[i] |> ValueOption.filter (fun v -> v.Trim('-') <> "")
        else
            ValueNone

    /// One line of a RUC starting 20, or ValueNone when it lacks a valid RUC or a name.
    let record (line: string) : BulkRecord voption =
        let fields = fields line

        match value fields 0 |> ValueOption.bind normalNumber, value fields 1 with
        | ValueSome ruc, ValueSome name ->
            let status = value fields 2
            let condition = value fields 3

            let join (parts: string voption list) =
                match parts |> List.choose ValueOption.toOption with
                | [] -> ValueNone
                | parts -> ValueSome(String.concat " " parts)

            let street = join [ value fields 5; value fields 6; value fields 9 ]
            let interior = value fields 10 |> ValueOption.map (sprintf "INT. %s")
            let zone = join [ value fields 7; value fields 8 ]

            let plot =
                join
                    [
                        value fields 13 |> ValueOption.map (sprintf "MZ. %s")
                        value fields 11 |> ValueOption.map (sprintf "LT. %s")
                        value fields 12 |> ValueOption.map (sprintf "DPTO. %s")
                        value fields 14 |> ValueOption.map (sprintf "KM. %s")
                    ]

            let ubigeo = value fields 4 |> ValueOption.map (sprintf "UBIGEO %s")

            ValueSome
                {
                    Company =
                        {
                            Name = name
                            OtherNames = []
                            Number = ruc
                            Jurisdiction = jurisdiction
                            JurisdictionAsPublished = ValueNone
                            Standing = standing status
                            Concerns = concerns condition
                            StatusAsPublished = status
                            LegalForm = ValueNone
                            IncorporatedOn = ValueNone
                            CeasedOn = ValueNone
                            Addresses =
                                match
                                    Bulk.companyAddress
                                        (AddressKind.Other "Domicilio fiscal")
                                        [ street; interior; zone; plot; ubigeo ]
                                        ValueNone
                                        ValueNone
                                        ValueNone
                                        ValueNone
                                with
                                | ValueSome address -> Available [ address ]
                                | ValueNone -> NotAvailable "The line holds no address"
                            Role = RecordRole.NotStated
                            HomeRegistration = ValueNone
                            EstablishmentOf = ValueNone
                        }
                    Officers = ValueNone
                    Owners = ValueNone
                    Raw = line
                }
        | _ -> ValueNone

    /// Builds the register from padron_reducido_ruc.zip as published, holding one text file. The file holds every RUC, ended ones too, so a
    /// company RUC it doesn't hold isn't registered, unless some lines couldn't be read. A RUC on several lines answers NotAvailable.
    let build (zip: Stream) (date: BulkFileDate) (ct: Threading.CancellationToken) : ICompanyRegistry =
        use archive = BulkFile.openZip zip

        match
            archive.Entries
            |> Seq.tryFind (fun e -> e.FullName.EndsWith(".txt", StringComparison.OrdinalIgnoreCase))
        with
        | None -> BulkFile.fail "The zip holds no text file"
        | Some entry ->
            match BulkFile.splitAt (fun _ -> true) (BulkFile.entryLines Encoding.Latin1 entry) with
            | struct (_, ValueNone, _) -> BulkFile.fail "The text is empty"
            | struct (_, ValueSome header, lines) ->
                if not (header.StartsWith("RUC|", StringComparison.Ordinal)) then
                    raise (InvalidDataException "The text doesn't start with the RUC header")

                let tally =
                    lines
                    |> Seq.filter (fun line ->
                        ct.ThrowIfCancellationRequested()
                        line.StartsWith("20", StringComparison.Ordinal))
                    |> Seq.map (fun line ->
                        match record line with
                        | ValueSome record -> BulkRow.Record record
                        | ValueNone -> BulkRow.WithoutNumber)
                    |> Bulk.tally normalNumber

                new BulkRegistry(
                    coverage,
                    normalNumber,
                    invalidNumber,
                    tally.Held,
                    date,
                    Bulk.completeUnless tally.WithoutNumber,
                    "The padrón reducido lists no representatives",
                    (fun _ -> NotAvailable "The padrón reducido lists no owners"),
                    tally.Repeated
                )
