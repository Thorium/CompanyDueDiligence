namespace CompanyDueDiligence

open System
open System.Collections.Generic
open System.Globalization
open System.IO
open System.Text
open System.Text.RegularExpressions
open System.Threading
open System.Threading.Tasks

/// The Registro de Empresas y Sociedades' formations on datos.gob.cl ("Constituciones del año ..."), one CSV a year, read as published on
/// 2026-11-08: UTF-8 with a byte order mark, ';' between fields. They list formations under the simplified regime of Ley 20.659 only, each
/// with Tipo de actuacion CONSTITUCIÓN, so they give no status and leave out every company formed otherwise. Only 2013 and 2024 to 2026 are
/// queryable through the CKAN datastore, so the files are read in bulk.
module internal Chile =

    let coverage: RegistryCoverage =
        {
            Registry = "Registro de Empresas y Sociedades formations (Chile)"
            Jurisdictions = [ Jurisdiction.ofCode "CL" ]
            Standing =
                Support.Partial
                    "No status: a formation is Registered, and later changes or dissolution aren't in the files"
            Concerns = Support.NotOffered "The files state no overdue filings, disputes or insolvency"
            Addresses = Support.Partial "The comuna and region of the seat, without a street"
            Search = Support.Partial "50 names containing the term, among the files supplied"
            Officers = Support.NotOffered "The files list no officers"
            Owners = Support.NotOffered "The files list no owners"
            Cost = AccessCost.Free
            RateLimit = RateLimitPolicy.NotPublished(1, TimeSpan.FromDays 1.)
            Licence = "CC BY (Creative Commons Attribution), as the dataset on datos.gob.cl states"
        }

    [<Literal>]
    let leftOut =
        "The files list formations under the simplified regime of Ley 20.659 in the years supplied, so not finding this RUT proves nothing"

    let private rutRegex = Regex @"\A([0-9]{7,8})-?([0-9K])\z"

    /// The RUT's check character: the digits from the right weighted 2 to 7 and again, 11 less the sum modulo 11, with 11 as 0 and 10 as K.
    let checkCharacter (digits: string) =
        let total =
            digits
            |> Seq.rev
            |> Seq.mapi (fun position digit -> (int digit - int '0') * (2 + position % 6))
            |> Seq.sum

        match 11 - total % 11 with
        | 11 -> '0'
        | 10 -> 'K'
        | n -> char (int '0' + n)

    /// Seven or eight digits and a matching check character, dots and spaces taken out, as 78325555-3.
    let normalRut (number: string) =
        let normal =
            if isNull number then
                ""
            else
                number.Replace(".", "").Replace(" ", "").ToUpperInvariant()

        let parts = rutRegex.Match normal

        if parts.Success && checkCharacter parts.Groups[1].Value = parts.Groups[2].Value[0] then
            ValueSome $"{parts.Groups[1].Value}-{parts.Groups[2].Value}"
        else
            ValueNone

    [<Literal>]
    let Formation = "CONSTITUCIÓN"

    /// A row's standing: a formation (Tipo de actuacion CONSTITUCIÓN) is Registered with no word; any other act, or none, is Unknown.
    let standing (act: string voption) =
        match act with
        | ValueSome Formation -> CompanyStanding.Registered ValueNone
        | other -> CompanyStanding.Unknown other

    /// A row as a record. A row without a name is kept under PartyNames.NotGiven, since one such row was in the 2013 file.
    let record (header: IReadOnlyDictionary<string, int>) (row: DelimitedRecord) : BulkRecord voption =
        let column name = BulkFile.cell header name row

        column "RUT"
        |> ValueOption.bind normalRut
        |> ValueOption.map (fun rut ->
            let act = column "Tipo de actuacion"

            let company: Company =
                {
                    Name = column "Razon Social" |> ValueOption.defaultValue PartyNames.NotGiven
                    OtherNames = []
                    Number = rut
                    Jurisdiction = Jurisdiction.ofCode "CL"
                    JurisdictionAsPublished = ValueNone
                    Standing = standing act
                    Concerns = CompanyConcerns.notPublished
                    StatusAsPublished = act |> ValueOption.filter ((<>) Formation)
                    LegalForm = column "Codigo de sociedad"
                    IncorporatedOn = BulkFile.date "dd-MM-yyyy" (column "Fecha de registro (ultima firma)")
                    CeasedOn = ValueNone
                    Addresses =
                        match column "Comuna Social", column "Region Social" with
                        | ValueNone, ValueNone -> NotAvailable "The row holds no comuna or region"
                        | comuna, region ->
                            Available
                                [
                                    {
                                        Kind =
                                            AddressKind.Other
                                                "Comuna and region of the seat (Comuna Social, Region Social)"
                                        Address =
                                            {
                                                Lines = []
                                                Locality = comuna
                                                Region = region
                                                PostalCode = ValueNone
                                                Country = ValueNone
                                            }
                                    }
                                ]
                    Role = RecordRole.Home
                    HomeRegistration = ValueNone
                    EstablishmentOf = ValueNone
                }

            {
                Company = company
                Officers = ValueNone
                Owners = ValueNone
                Raw = row.Raw
            })

    /// The records of one file's lines, as they are read. A file without the RUT, Razon Social and Tipo de actuacion columns, or a row with
    /// another number of fields than the header, is unreadable; a row without a valid RUT can't be looked up and is left out.
    let readFile (lines: string seq) (ct: CancellationToken) : BulkRecord seq =
        match BulkFile.splitAt (fun _ -> true) lines with
        | struct (_, ValueNone, _) -> BulkFile.fail "A file is empty"
        | struct (_, ValueSome first, rows) ->
            let names = BulkFile.split ';' true first
            let header = BulkFile.header names

            if
                not (
                    header.ContainsKey "RUT"
                    && header.ContainsKey "Razon Social"
                    && header.ContainsKey "Tipo de actuacion"
                )
            then
                BulkFile.fail "A file has no RUT, Razon Social and Tipo de actuacion columns"

            rows
            |> Seq.mapi (fun position line -> struct (position + 2, line))
            |> Seq.choose (fun struct (lineNumber, line) ->
                ct.ThrowIfCancellationRequested()

                if line.Trim() = "" then
                    None
                else
                    let fields = BulkFile.split ';' true line

                    if fields.Length <> names.Length then
                        BulkFile.fail $"A row has {fields.Length} fields, not {names.Length}, at line {lineNumber}"

                    record header { Raw = line; Fields = fields } |> ValueOption.toOption)

    /// A register over the yearly files the host downloaded, read as UTF-8. A RUT in two rows answers NotAvailable, since they can't be told
    /// apart.
    let build (files: Stream seq) (date: BulkFileDate) (ct: CancellationToken) : ICompanyRegistry =
        let index, repeated =
            try
                files
                |> Seq.collect (fun file -> readFile (BulkFile.wholeLines ';' true BulkFile.strictUtf8 file) ct)
                |> Bulk.index (fun record -> normalRut record.Company.Number)
            with :? DecoderFallbackException ->
                BulkFile.fail "A file holds text that isn't UTF-8"

        new BulkRegistry(
            coverage,
            normalRut,
            "A RUT has seven or eight digits and a matching check character, as 78325555-3",
            index,
            date,
            BulkAbsence.CantTell leftOut,
            "The files list no officers",
            (fun _ -> NotAvailable "The files list no owners"),
            repeated
        )
