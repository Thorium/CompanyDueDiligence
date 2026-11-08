namespace CompanyDueDiligence

open System
open System.Collections.Generic
open System.Globalization
open System.IO
open System.Text.RegularExpressions
open System.Threading

/// The State Register of Legal Units of Moldova as ASP (Agenția Servicii Publice) publishes it: company.xlsx, read as downloaded on 2026-11-08
/// (data "starea pînă la 02.11.2026").
module internal Moldova =

    let jurisdiction = Jurisdiction.ofCode "MD"

    let coverage: RegistryCoverage =
        {
            Registry = "ASP State Register of Legal Units (Moldova)"
            Jurisdictions = [ jurisdiction ]
            Standing =
                Support.Partial
                    "The file has no status word: a liquidation date reads as dissolved, a liquidator or insolvency administrator among the leaders as winding up or insolvency, and anything else as registered"
            Concerns = Support.Partial "Insolvency only, from an insolvency administrator among the leaders"
            Addresses = Support.Partial "One address as published, not split into parts"
            Search = Support.Partial "Names containing the term, over the file, up to 50"
            Officers = Support.Partial "Leaders' names and roles as published, with no dates; each is read as current"
            Owners =
                Support.Partial
                    "Founders with their share of the capital as published; the file doesn't say whether a founder is a person or a company, or give its number"
            Cost = AccessCost.Free
            RateLimit = RateLimitPolicy.NotPublished(1, TimeSpan.FromDays 1.)
            Licence = "None stated; the site terms allow reuse under the law, source named"
        }

    let invalidNumber = "An IDNO has 13 digits, the last a check digit"

    /// A 13-digit IDNO whose last digit is the sum of the first twelve weighted 7, 3, 1 in turn, modulo 10.
    let normalNumber (number: string) =
        let digits = if isNull number then "" else number.Trim()

        if digits.Length = 13 && digits |> Seq.forall Char.IsAsciiDigit then
            let check =
                Seq.init 12 (fun i -> (int digits[i] - int '0') * [| 7; 3; 1 |][i % 3])
                |> Seq.sum

            if check % 10 = int digits[12] - int '0' then
                ValueSome digits
            else
                ValueNone
        else
            ValueNone

    [<Literal>]
    let InsolvencyAdministrator = "Administrator al procesului de insolvabilitate"

    [<Literal>]
    let PlanAdministrator = "Administrator al procedurii planului"

    [<Literal>]
    let Liquidator = "Lichidator"

    /// The standing the file supports: a liquidation date is dissolution; otherwise an insolvency or plan administrator is insolvency and a
    /// liquidator is winding up. The file has no status word, so anything else is registered with none.
    let standing (liquidatedOn: DateOnly voption) (roles: string list) =
        match liquidatedOn with
        | ValueSome _ -> CompanyStanding.Dissolved
        | ValueNone when
            roles
            |> List.exists (fun role -> role = InsolvencyAdministrator || role = PlanAdministrator)
            ->
            CompanyStanding.InInsolvency(
                roles
                |> List.find (fun role -> role = InsolvencyAdministrator || role = PlanAdministrator)
            )
        | ValueNone when List.contains Liquidator roles -> CompanyStanding.WindingUp Liquidator
        | ValueNone -> CompanyStanding.Registered ValueNone

    /// What the file states for the standing: the liquidation date, or the role it was read from.
    let statusAsPublished (liquidatedOn: DateOnly voption) (standing: CompanyStanding) =
        match liquidatedOn, standing with
        | ValueSome day, _ ->
            ValueSome(
                day.ToString("dd.MM.yyyy", CultureInfo.InvariantCulture)
                |> sprintf "Data lichidării %s"
            )
        | ValueNone, CompanyStanding.InInsolvency role
        | ValueNone, CompanyStanding.WindingUp role -> ValueSome role
        | ValueNone, _ -> ValueNone

    /// Leaders as "NAME [Role], NAME [Role]". "Administrator" is a director; any other role is kept as published.
    let officers (text: string voption) : Officer list =
        match text with
        | ValueNone -> []
        | ValueSome text ->
            text.Split("], ", StringSplitOptions.RemoveEmptyEntries)
            |> Array.choose (fun entry ->
                let entry = entry.Trim().TrimEnd ']'
                let at = entry.LastIndexOf " ["

                let name, role =
                    if at > 0 then
                        entry.Substring(0, at).Trim(), entry.Substring(at + 2).Trim()
                    else
                        entry.Trim(), ""

                if name = "" then
                    None
                else
                    Some
                        {
                            Name = name
                            Kind = PartyKind.Unknown ValueNone
                            Role =
                                match role with
                                | "Administrator" -> OfficerRole.Director
                                | "" -> OfficerRole.Other "(role not given)"
                                | other -> OfficerRole.Other other
                            Current = true
                            AppointedOn = ValueNone
                            EndedOn = ValueNone
                            BornIn = ValueNone
                            Nationality = ValueNone
                            CountryOfResidence = ValueNone
                            Address = ValueNone
                            Entity = ValueNone
                            IdentityVerification = ValueNone
                        })
            |> List.ofArray

    let private roleOf (officer: Officer) =
        match officer.Role with
        | OfficerRole.Other role -> role
        | OfficerRole.Director -> "Administrator"
        | OfficerRole.Secretary -> "Secretary"

    let private shareRegex =
        Regex @"\A(?<name>.+?)\s*\((?<share>\d{1,3}(?:,\d+)?)\s*%\)\z"

    /// A share as a band: a whole percentage is From = To; a fraction lies between the whole numbers either side.
    let band (share: string) =
        match Decimal.TryParse(share.Replace(',', '.'), NumberStyles.Number, CultureInfo.InvariantCulture) with
        | true, value when value >= 0m && value <= 100m ->
            ValueSome
                {
                    From = int (Math.Floor value)
                    To = int (Math.Ceiling value)
                }
        | _ -> ValueNone

    let private ownersRegex = Regex @"(?<=%\)),\s*"

    /// Founders as "NAME (14,00%), NAME (86,00%)", or names alone. Each is a current legal owner; the file doesn't say whether one is a person or a
    /// company, which leaves a gap.
    let owners (text: string voption) : OwnerListing voption =
        match text with
        | ValueNone -> ValueNone
        | ValueSome text ->
            let entries =
                if text.Contains "%)" then
                    ownersRegex.Split text
                else
                    text.Split ", "

            let owners =
                entries
                |> Array.map _.Trim()
                |> Array.filter ((<>) "")
                |> Array.map (fun entry ->
                    let found = shareRegex.Match entry

                    let name, control =
                        if found.Success then
                            let share = found.Groups["share"].Value

                            found.Groups["name"].Value.Trim(),
                            [
                                {
                                    Kind = ControlKind.Ownership
                                    Band = band share
                                    AsPublished = share + "%"
                                }
                            ]
                        else
                            entry,
                            [
                                {
                                    Kind = ControlKind.Ownership
                                    Band = ValueNone
                                    AsPublished = "Founder, no share stated"
                                }
                            ]

                    {
                        Name = ValueSome name
                        Kind = PartyKind.Unknown ValueNone
                        Control = control
                        Current = true
                        Since = ValueNone
                        EndedOn = ValueNone
                        BornIn = ValueNone
                        Nationality = ValueNone
                        CountryOfResidence = ValueNone
                        Address = ValueNone
                        Entity = ValueNone
                        IdentityVerification = ValueNone
                    })
                |> List.ofArray

            if List.isEmpty owners then
                ValueNone
            else
                ValueSome
                    {
                        Owners = owners
                        Statements = []
                        Gaps = [ OwnershipGap.OwnerUnreadable ]
                    }

    let private formRole (form: string voption) =
        match form with
        | ValueSome "Sucursala întreprinderii nerezidente străine"
        | ValueSome "Reprezentanţa întreprinderii nerezidente străine" -> RecordRole.Establishment
        | ValueSome "Sucursala întreprinderii rezidente"
        | ValueNone -> RecordRole.NotStated
        | ValueSome _ -> RecordRole.Home

    let private postalRegex = Regex @"\AMD-\d{4}"

    let private asOfRegex = Regex @"starea p[îi]n[ăa] la (\d{2}\.\d{2}\.\d{4})"

    /// Column positions by header text; the header row is the one whose first cell starts "IDNO".
    let private headerOf (row: string array) =
        row |> Array.mapi (fun i text -> text.Trim(), i) |> Map.ofArray

    /// One row as a record, or ValueNone when it lacks a valid IDNO or a name.
    let record (header: Map<string, int>) (row: string array) : BulkRecord voption =
        let column name =
            match header.TryFind name with
            | Some i -> Xlsx.cell row i
            | None -> ValueNone

        match column "IDNO/ Cod fiscal" |> ValueOption.bind normalNumber, column "Denumirea completă" with
        | ValueSome number, ValueSome name ->
            let liquidatedOn = Xlsx.serialDate (column "Data lichidării")
            let leaders = officers (column "Lista conducătorilor (cu indicarea rolurilor)")
            let standing = standing liquidatedOn (leaders |> List.map roleOf)
            let form = column "Forma org./jurid."
            let address = column "Adresa"

            let raw =
                [
                    "IDNO", ValueSome number
                    "Denumirea", ValueSome name
                    "Forma", form
                    "Adresa", address
                    "Data înregistrării", column "Data înregistrării"
                    "Conducători", column "Lista conducătorilor (cu indicarea rolurilor)"
                    "Fondatori", column "Lista fondatorilor (cu indicarea cotei părţi în capitalul social %)"
                    "Data lichidării", column "Data lichidării"
                ]
                |> List.choose (fun (label, value) ->
                    value |> ValueOption.map (fun v -> $"{label}: {v}") |> ValueOption.toOption)
                |> String.concat "\n"

            ValueSome
                {
                    Company =
                        {
                            Name = name
                            OtherNames = []
                            Number = number
                            Jurisdiction = jurisdiction
                            JurisdictionAsPublished = ValueNone
                            Standing = standing
                            Concerns = Bulk.insolvencyConcerns standing.IsInInsolvency
                            StatusAsPublished = statusAsPublished liquidatedOn standing
                            LegalForm = form
                            IncorporatedOn = Xlsx.serialDate (column "Data înregistrării")
                            CeasedOn = liquidatedOn
                            Addresses =
                                match address with
                                | ValueSome text ->
                                    Available
                                        [
                                            {
                                                Kind = AddressKind.Other "Adresa"
                                                Address =
                                                    {
                                                        Lines = [ text ]
                                                        Locality = ValueNone
                                                        Region = ValueNone
                                                        PostalCode =
                                                            let found = postalRegex.Match text
                                                            if found.Success then ValueSome found.Value else ValueNone
                                                        Country = ValueNone
                                                    }
                                            }
                                        ]
                                | ValueNone -> NotAvailable "The row holds no address"
                            Role = formRole form
                            HomeRegistration = ValueNone
                            EstablishmentOf = ValueNone
                        }
                    Officers =
                        if List.isEmpty leaders then
                            ValueNone
                        else
                            ValueSome leaders
                    Owners = owners (column "Lista fondatorilor (cu indicarea cotei părţi în capitalul social %)")
                    Raw = raw
                }
        | _ -> ValueNone

    /// The columns a row's standing and parties are read from, which the header has to hold.
    let private requiredColumns =
        [
            "IDNO/ Cod fiscal"
            "Denumirea completă"
            "Data lichidării"
            "Lista conducătorilor (cu indicarea rolurilor)"
            "Lista fondatorilor (cu indicarea cotei părţi în capitalul social %)"
        ]

    /// Builds the register from company.xlsx as published. The date in its title rows, "starea pînă la", is the AsOf.
    let build (file: Stream) (date: BulkFileDate) (ct: CancellationToken) : ICompanyRegistry =
        use archive = BulkFile.openZip file

        let isHeader (struct (_, row): struct (int * string array)) =
            match Xlsx.cell row 0 with
            | ValueSome first -> first.StartsWith("IDNO", StringComparison.Ordinal)
            | ValueNone -> false

        // The sheet streams row by row; the title and header rows come first.
        match BulkFile.splitAt isHeader (Xlsx.rows archive "xl/worksheets/sheet1.xml") with
        | struct (_, ValueNone, _) -> BulkFile.fail "The sheet has no header row starting IDNO"
        | struct (title, ValueSome(struct (_, headerRow)), rows) ->
            let columns = headerOf headerRow

            if not (requiredColumns |> List.forall columns.ContainsKey) then
                BulkFile.fail "The sheet lacks a column its standing, officers or owners are read from"

            let stated = BulkFile.date "dd.MM.yyyy" (Xlsx.lastMatch asOfRegex title)

            let tally =
                rows
                |> Seq.choose (fun struct (_, row) ->
                    ct.ThrowIfCancellationRequested()

                    match record columns row with
                    | ValueSome record -> Some(BulkRow.Record record)
                    | ValueNone when row |> Array.exists (String.IsNullOrWhiteSpace >> not) ->
                        Some BulkRow.WithoutNumber
                    | ValueNone -> None)
                |> Bulk.tally normalNumber

            new BulkRegistry(
                coverage,
                normalNumber,
                invalidNumber,
                tally.Held,
                Bulk.asOf date stated,
                BulkAbsence.CantTell
                    $"The file lists enterprises only, leaving out non-commercial organisations, and {tally.WithoutNumber} rows without a valid IDNO; a number it doesn't hold may still be registered",
                "The file lists no leaders",
                (fun _ -> NotAvailable "The file lists no founders for this company, which doesn't say it has none"),
                tally.Repeated
            )
