namespace CompanyDueDiligence

open System
open System.Collections.Generic
open System.IO
open System.Text
open System.Text.RegularExpressions

/// ASIC's company dataset on data.gov.au, weekly: a tab-delimited file with a header, one row per name a company holds, read from
/// company_202611.zip and the dataset's help file on 2026-11-08. It holds registered companies, those deregistered in the past year, and former
/// names of registered companies. In the ACN column a registered Australian body (type RACN) has its ARBN.
module internal AsicCompanies =

    let coverage: RegistryCoverage =
        {
            Registry = "ASIC company dataset"
            Jurisdictions = [ Jurisdiction.ofCode "AU" ]
            Standing = Support.Supported
            Concerns =
                Support.NotOffered
                    "The dataset states no overdue filings or disputes; external administration is a status"
            Addresses = Support.NotOffered "The dataset holds no address"
            Search = Support.Partial "50 current names containing the term"
            Officers = Support.NotOffered "The dataset lists no officers"
            Owners = Support.NotOffered "The dataset lists no owners"
            Cost = AccessCost.Free
            RateLimit = RateLimitPolicy.NotPublished(1, TimeSpan.FromDays 1.)
            Licence = "CC BY 3.0 AU, as the dataset's data.gov.au metadata states"
        }

    let private acnRegex = Regex @"\A[0-9]{9}\z"

    /// ASIC's ACN check digit: weights 8 to 1 over the first eight digits, and the complement of the sum's remainder modulo 10. It held for every
    /// ACN and ARBN in the file of 2026-11-02.
    let checkDigit (first8: string) =
        let total =
            first8
            |> Seq.mapi (fun position c -> (int c - int '0') * (8 - position))
            |> Seq.sum

        (10 - total % 10) % 10

    /// An ACN or ARBN: nine digits, spaces allowed, whose check digit holds.
    let normalAcn (number: string) =
        let text = if isNull number then "" else number.Replace(" ", "").Trim()

        if
            acnRegex.IsMatch text
            && checkDigit (text.Substring(0, 8)) = int text[8] - int '0'
        then
            ValueSome text
        else
            ValueNone

    [<Literal>]
    let invalidNumber = "An ACN or ARBN has nine digits, the last a check digit"

    /// Every status the help file's data dictionary lists.
    let standing (status: string) =
        match status with
        | "REGD" -> CompanyStanding.Registered(ValueSome "Registered")
        | "SOFF" -> CompanyStanding.WindingUp "Strike-off action in progress"
        | "EXAD" -> CompanyStanding.InInsolvency "External administration (in receivership/liquidation)"
        | "DRGD"
        | "DISS" -> CompanyStanding.Dissolved
        | "NOAC" -> CompanyStanding.Closed "Not active"
        | "NRGD" -> CompanyStanding.Closed "Not registered"
        | "CNCL" -> CompanyStanding.Closed "Cancelled, transferred to another law or act"
        | "DIV3" -> CompanyStanding.Closed "Organisation transferred registration via DIV3"
        | "PROV" -> CompanyStanding.Unknown(ValueSome "Provisional")
        | "PEND" -> CompanyStanding.Unknown(ValueSome "Pending - Schemes")
        | "" -> CompanyStanding.Unknown ValueNone
        | other -> CompanyStanding.Unknown(ValueSome other)

    let typeWord (code: string) =
        match code with
        | "APTY" -> "Australian proprietary company"
        | "APUB" -> "Australian public company"
        | "FNOS" -> "Foreign company"
        | "RACN" -> "Registered Australian Corporation"
        | other -> other

    let classWord (code: string) =
        match code with
        | "LMSH" -> ValueSome "limited by shares"
        | "LMGT" -> ValueSome "limited by guarantee"
        | "LMSG" -> ValueSome "limited by both shares and guarantees"
        | "NLIA" -> ValueSome "no liability"
        | "UNLM" -> ValueSome "unlimited"
        | "NONE"
        | "" -> ValueNone
        | other -> ValueSome other

    /// FNOS is a company incorporated outside Australia and registered as a foreign company; the other types are Australian.
    let role (companyType: string) =
        match companyType with
        | "FNOS" -> RecordRole.ForeignRegistration
        | "APTY"
        | "APUB"
        | "RACN" -> RecordRole.Home
        | _ -> RecordRole.NotStated

    let private date (text: string) = BulkFile.dateFrom1800 "dd/MM/yyyy" text

    let private columns =
        [
            "Company Name"
            "ACN"
            "Type"
            "Class"
            "Sub Class"
            "Status"
            "Date of Registration"
            "Date of Deregistration"
            "Current Name Indicator"
            "Current Name"
        ]

    /// The value under a column of a row's fields, trimmed; "" when the row lacks it.
    let private column (header: IReadOnlyDictionary<string, int>) (name: string) (fields: string array) =
        BulkFile.cell header name { Raw = ""; Fields = fields }
        |> ValueOption.defaultValue ""

    /// The current name among a company's rows: the row marked as the current name names it, or else the current name a former name's row gives;
    /// "" when none does.
    let private currentName (column: string -> string array -> string) (rows: (string array * string) list) =
        match
            rows
            |> List.tryFind (fun (fields, _) -> column "Current Name Indicator" fields = "Y")
        with
        | Some(fields, _) -> column "Company Name" fields
        | None ->
            rows
            |> List.map (fst >> column "Current Name")
            |> List.tryFind ((<>) "")
            |> Option.defaultValue ""

    /// One company from its rows, each its fields and line, named by currentName.
    let company (column: string -> string array -> string) (rows: (string array * string) list) : BulkRecord =
        let first = fst rows.Head
        let name = currentName column rows

        if name = "" then
            BulkFile.fail "A company's rows give no current name"

        let companyType = column "Type" first
        let status = column "Status" first

        {
            Company =
                {
                    Name = name
                    OtherNames =
                        rows
                        |> List.map (fst >> column "Company Name")
                        |> List.filter (fun held -> held <> "" && held <> name)
                        |> List.distinct
                    Number = column "ACN" first
                    Jurisdiction = Jurisdiction.ofCode "AU"
                    JurisdictionAsPublished = ValueNone
                    Standing = standing status
                    Concerns = CompanyConcerns.notPublished
                    StatusAsPublished = BulkFile.nonBlank status
                    LegalForm =
                        match BulkFile.nonBlank companyType, classWord (column "Class" first) with
                        | ValueNone, _ -> ValueNone
                        | ValueSome kind, ValueNone -> ValueSome(typeWord kind)
                        | ValueSome kind, ValueSome liability -> ValueSome $"{typeWord kind}, {liability}"
                    IncorporatedOn = date (column "Date of Registration" first)
                    CeasedOn = date (column "Date of Deregistration" first)
                    Addresses = NotAvailable "The dataset holds no address"
                    Role = role companyType
                    HomeRegistration = ValueNone
                    EstablishmentOf = ValueNone
                }
            Officers = ValueNone
            Owners = ValueNone
            Raw = rows |> List.map snd |> String.concat "\n"
        }

    /// A company as the dataset holds it: its rows' lines as UTF-8, joined by line breaks, and the header of the file they came from.
    [<Struct>]
    type Held =
        {
            Header: IReadOnlyDictionary<string, int>
            Lines: byte array
        }

    let private rowsOf (held: Held) =
        (Encoding.UTF8.GetString held.Lines).Split '\n'
        |> Array.map (fun line -> BulkFile.split '\t' false line, line)
        |> List.ofArray

    /// The companies of one file, each its ACN and rows as Held. A row without an ACN fails the file. The rows are gathered in a dictionary filled
    /// here and then only read, as the file holds millions.
    let private companies (ct: Threading.CancellationToken) (lines: string seq) =
        match BulkFile.splitAt (fun _ -> true) lines with
        | struct (_, ValueNone, _) -> BulkFile.fail "The file is empty"
        | struct (_, ValueSome headerLine, rows) ->
            let header = BulkFile.header (BulkFile.split '\t' false headerLine)

            match columns |> List.filter (header.ContainsKey >> not) with
            | [] -> ()
            | missing -> BulkFile.fail $"""The header lacks {String.concat ", " missing}"""

            let grouped = Dictionary<string, byte array>()

            rows
            |> Seq.filter (String.IsNullOrWhiteSpace >> not)
            |> Seq.iter (fun line ->
                ct.ThrowIfCancellationRequested()
                let acn = column header "ACN" (BulkFile.split '\t' false line)

                if acn = "" then
                    BulkFile.fail "A row has no ACN"

                grouped[acn] <-
                    match grouped.TryGetValue acn with
                    | true, held -> Array.concat [ held; "\n"B; Encoding.UTF8.GetBytes line ]
                    | _ -> Encoding.UTF8.GetBytes line)

            grouped
            |> Seq.map (fun pair -> pair.Key, { Header = header; Lines = pair.Value })

    /// The register over the dataset's zip or CSV. A company deregistered over a year ago, or one ASIC can't publish, isn't in it. Each company is
    /// held as its lines and read when asked for; every company's rows are checked for a current name as the file is read.
    let build (file: Stream) (date: BulkFileDate) (ct: Threading.CancellationToken) : ICompanyRegistry =
        let index = BulkIndex<Held>()

        BulkFile.perFile file (fun _ openFile -> companies ct (BulkFile.lines BulkFile.strictUtf8 (openFile ())))
        |> Seq.concat
        |> Seq.iter (fun (acn, held) ->
            if currentName (column held.Header) (rowsOf held) = "" then
                BulkFile.fail "A company's rows give no current name"

            normalAcn acn |> ValueOption.iter (fun key -> index.Add(key, held)))

        let records =
            LazyRecords<Held>(
                index.Held,
                (fun held -> company (column held.Header) (rowsOf held)),
                (fun held -> currentName (column held.Header) (rowsOf held))
            )

        BulkRegistry(
            coverage,
            normalAcn,
            invalidNumber,
            records,
            date,
            BulkAbsence.CantTell
                "The dataset holds registered companies and those deregistered in the past year, and leaves out any ASIC can't publish",
            "The dataset lists no officers",
            (fun _ -> NotAvailable "The dataset lists no owners"),
            index.Repeated
        )
