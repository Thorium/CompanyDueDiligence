namespace CompanyDueDiligence

open System
open System.Collections.Generic
open System.IO
open System.Text
open System.Text.RegularExpressions
open System.Threading

/// The Alaska Division of Corporations' downloads: CorporationsDownload and OfficialsDownload, CSV with a header, read as they answered on
/// 2026-11-08. No layout is published; the columns are the files' own headers. The corporations file held 49,247 entities, every one Good
/// Standing, Non-Compliant or Active Name, so it holds current entities only.
module internal Alaska =

    let coverage: RegistryCoverage =
        {
            Registry = "Alaska Division of Corporations downloads"
            Jurisdictions = [ Jurisdiction.ofCode "US-AK" ]
            Standing =
                Support.Partial "Good Standing, Non-Compliant or Active Name: the download holds current entities only"
            Concerns = Support.NotOffered "The download states no overdue filings, disputes or insolvency"
            Addresses = Support.Supported
            Search = Support.Partial "50 names containing the term"
            Officers =
                Support.Partial
                    "Officials by title from the officials download, if the host passes it; current ones only"
            Owners =
                Support.Partial
                    "Shareholders, members, owners and partners named among the officials, with no shares; Alaska asks only for those holding 5% or more"
            Cost = AccessCost.Free
            RateLimit = RateLimitPolicy.NotPublished(1, TimeSpan.FromDays 1.)
            Licence = "None found; the download page is behind a captcha"
        }

    let private numberRegex = Regex @"\A[0-9]{1,8}[A-Z]{0,2}\z"

    /// An entity number: up to eight digits, some with one or two letters after them, such as 473D or 2755PD.
    let normalNumber (number: string) =
        let text =
            if isNull number then
                ""
            else
                number.Trim().ToUpperInvariant()

        if numberRegex.IsMatch text then
            ValueSome text
        else
            ValueNone

    [<Literal>]
    let invalidNumber =
        "An Alaska entity number has up to eight digits, sometimes with one or two letters"

    /// The status words the download held on 2026-11-08.
    let standing (status: string) =
        match status with
        | "Good Standing" -> CompanyStanding.Active
        | "Non-Compliant" -> CompanyStanding.ActiveWithConcern "Non-Compliant"
        | "Active Name" -> CompanyStanding.Registered(ValueSome "Active Name")
        | ""
        | "undefined" -> CompanyStanding.Unknown ValueNone
        | other -> CompanyStanding.Unknown(ValueSome other)

    /// A home state of Alaska is a home record and any other a foreign registration; a business name registration is no entity's record.
    let role (corporationType: string) (homeState: string) =
        match corporationType, homeState with
        | "Business Name Registration", _ -> RecordRole.NotStated
        | "Foreign Corporate Name Registration", _ -> RecordRole.ForeignRegistration
        | _, "ALASKA" -> RecordRole.Home
        | _, "" -> RecordRole.NotStated
        | _ -> RecordRole.ForeignRegistration

    /// Titles of a shareholder, member, owner or partner; the rest are officers.
    let private ownerTitles =
        set
            [
                "Shareholder"
                "Member"
                "Owner"
                "Limited Partner"
                "General Partner"
                "Interest Holder"
            ]

    let isOwnerTitle (title: string) = ownerTitles.Contains title

    let officerRole (title: string) =
        match title with
        | "Director" -> OfficerRole.Director
        | "Secretary" -> OfficerRole.Secretary
        | other -> OfficerRole.Other other

    let private date (text: string) = BulkFile.dateFrom1800 "M/d/yyyy" text

    /// Rows of a CSV with its header, each with a reader of its columns, and the rows that don't split into the header's columns: the downloads
    /// hold a few with broken quoting, so a quote doesn't run past its line.
    let private table (ct: CancellationToken) (required: string list) (file: Stream) =
        let read =
            BulkFile.perFile file (fun _ openFile ->
                match
                    BulkFile.splitAt (fun _ -> true) (BulkFile.wholeLines ',' true BulkFile.strictUtf8 (openFile ()))
                with
                | struct (_, ValueNone, _) -> BulkFile.fail "The file is empty"
                | struct (_, ValueSome headerLine, lines) ->
                    let header = BulkFile.header (BulkFile.split ',' true headerLine)

                    match required |> List.filter (header.ContainsKey >> not) with
                    | [] -> ()
                    | missing -> BulkFile.fail $"""The header lacks {String.concat ", " missing}"""

                    let column name row =
                        BulkFile.cell header name row |> ValueOption.defaultValue ""

                    lines
                    |> Seq.filter (String.IsNullOrWhiteSpace >> not)
                    |> Seq.map (fun line ->
                        ct.ThrowIfCancellationRequested()

                        {
                            Raw = line
                            Fields = BulkFile.split ',' true line
                        })
                    |> List.ofSeq
                    |> List.partition (fun row -> row.Fields.Length = header.Count)
                    |> fun (whole, broken) ->
                        whole |> List.map (fun row -> column, row), broken |> List.map (fun row -> column, row))

        read |> List.collect fst, read |> List.collect snd

    let private entityColumns =
        [
            "CORPTYPE"
            "ENTITYNUMBER"
            "LEGALNAME"
            "STATUS"
            "AKFORMEDDATE"
            "HOMESTATE"
            "HOMECOUNTRY"
            "REGISTEREDAGENT"
            "ENTITYMAILINGADDRESS1"
            "ENTITYPHYSADDRESS1"
            "REGISTEREDMAILADDRESS1"
            "REGISTEREDPHYSADDRESS1"
        ]

    let private officialColumns =
        [
            "ParentEntityNumber"
            "OfficialEntityNumber"
            "OfficialLastOrEntityName"
            "OfficialFirstName"
            "OfficialTitle"
        ]

    /// An official: OfficialEntityNumber names an Alaska entity, a first name a person.
    type internal Official =
        {
            Name: string
            Kind: PartyKind
            EntityNumber: string voption
            Title: string
            Line: string
        }

    let official (column: string -> DelimitedRecord -> string) (row: DelimitedRecord) =
        let last = column "OfficialLastOrEntityName" row
        let first = column "OfficialFirstName" row
        let entity = BulkFile.nonBlank (column "OfficialEntityNumber" row)

        {
            Name = [ first; last ] |> List.filter ((<>) "") |> String.concat " "
            Kind =
                match entity, first with
                | ValueSome _, _ -> PartyKind.LegalEntity
                | ValueNone, "" -> PartyKind.Unknown ValueNone
                | ValueNone, _ -> PartyKind.Individual
            EntityNumber = entity
            Title = column "OfficialTitle" row
            Line = row.Raw
        }

    let private asOfficer (official: Official) : Officer =
        {
            Name = official.Name
            Kind = official.Kind
            Role = officerRole official.Title
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

    let private asOwner (official: Official) : Owner =
        {
            Name = BulkFile.nonBlank official.Name
            Kind = official.Kind
            Control =
                [
                    {
                        Kind = ControlKind.Ownership
                        Band = ValueNone
                        AsPublished = official.Title
                    }
                ]
            Current = true
            Since = ValueNone
            EndedOn = ValueNone
            BornIn = ValueNone
            Nationality = ValueNone
            CountryOfResidence = ValueNone
            Address = ValueNone
            Entity =
                official.EntityNumber
                |> ValueOption.map (fun number ->
                    {
                        LegalName = BulkFile.nonBlank official.Name
                        LegalForm = ValueNone
                        GoverningLaw = ValueNone
                        Country = ValueSome "ALASKA"
                        Register = ValueSome "Alaska Division of Corporations"
                        RegistrationNumber = ValueSome number
                    })
            IdentityVerification = ValueNone
        }

    /// One entity with its officials, if the officials file was passed.
    let record
        (officials: IReadOnlyDictionary<string, Official list> voption)
        (column: string -> DelimitedRecord -> string)
        (row: DelimitedRecord)
        =
        let number = column "ENTITYNUMBER" row
        let name = column "LEGALNAME" row

        if number = "" || name = "" then
            BulkFile.fail "An entity row has no number or name"

        let corporationType = column "CORPTYPE" row
        let homeState = column "HOMESTATE" row
        let status = column "STATUS" row
        let role = role corporationType homeState

        let addressOf prefix kind =
            Bulk.address
                [
                    BulkFile.nonBlank (column (prefix + "ADDRESS1") row)
                    BulkFile.nonBlank (column (prefix + "ADDRESS2") row)
                ]
                (BulkFile.nonBlank (column (prefix + "CITY") row))
                (BulkFile.nonBlank (column (prefix + "STATEPROVINCE") row))
                (BulkFile.nonBlank (column (prefix + "ZIP") row))
                (BulkFile.nonBlank (column (prefix + "COUNTRY") row))
            |> ValueOption.map (fun address -> { Kind = kind; Address = address })

        let addresses =
            [
                addressOf "ENTITYPHYS" AddressKind.PrincipalOffice
                addressOf "ENTITYMAILING" AddressKind.Postal
                addressOf "REGISTEREDPHYS" AddressKind.RegisteredAgent
                addressOf "REGISTEREDMAIL" (AddressKind.Other "Registered agent's mailing address")
            ]
            |> List.choose ValueOption.toOption

        let held =
            match officials with
            | ValueNone -> ValueNone
            | ValueSome officials ->
                match officials.TryGetValue(normalNumber number |> ValueOption.defaultValue number) with
                | true, held -> ValueSome held
                | _ -> ValueSome []

        let agent =
            BulkFile.nonBlank (column "REGISTEREDAGENT" row)
            |> ValueOption.map (fun agent ->
                { asOfficer
                      {
                          Name = agent
                          Kind = PartyKind.Unknown ValueNone
                          EntityNumber = ValueNone
                          Title = "Registered Agent"
                          Line = ""
                      } with
                    Role = OfficerRole.Other "Registered Agent"
                })
            |> ValueOption.toList

        let officers =
            held
            |> ValueOption.map (List.filter (_.Title >> isOwnerTitle >> not) >> List.map asOfficer)
            |> ValueOption.defaultValue []

        {
            Company =
                {
                    Name = name
                    OtherNames =
                        BulkFile.nonBlank (column "ASSUMEDNAME" row)
                        |> ValueOption.filter ((<>) name)
                        |> ValueOption.toList
                    Number = number
                    Jurisdiction = Jurisdiction.ofCode "US-AK"
                    JurisdictionAsPublished = BulkFile.nonBlank homeState
                    Standing = standing status
                    Concerns = CompanyConcerns.notPublished
                    StatusAsPublished = BulkFile.nonBlank status
                    LegalForm = BulkFile.nonBlank corporationType
                    IncorporatedOn = date (column "AKFORMEDDATE" row)
                    CeasedOn = ValueNone
                    Addresses =
                        match addresses with
                        | [] -> NotAvailable "The row holds no address"
                        | listed -> Available listed
                    Role = role
                    HomeRegistration =
                        match role with
                        | RecordRole.ForeignRegistration ->
                            ValueSome
                                {
                                    LegalName = ValueSome name
                                    LegalForm = ValueNone
                                    GoverningLaw = ValueNone
                                    Country =
                                        [ homeState; column "HOMECOUNTRY" row ]
                                        |> List.filter ((<>) "")
                                        |> String.concat ", "
                                        |> BulkFile.nonBlank
                                    Register = ValueNone
                                    RegistrationNumber = ValueNone
                                }
                        | _ -> ValueNone
                    EstablishmentOf = ValueNone
                }
            Officers =
                match officers @ agent with
                | [] -> ValueNone
                | listed -> ValueSome listed
            Owners =
                held
                |> ValueOption.bind (fun held ->
                    match held |> List.filter (_.Title >> isOwnerTitle) with
                    | [] -> ValueNone
                    | owners ->
                        ValueSome
                            {
                                Owners = owners |> List.map asOwner
                                Statements = []
                                // Alaska asks for owners of 5% or more, so smaller ones aren't named.
                                Gaps = [ OwnershipGap.OwnerNotIdentified ]
                            })
            Raw = String.concat "\n" (row.Raw :: (held |> ValueOption.defaultValue [] |> List.map _.Line))
        }

    /// The register over the corporations download and, if passed, the officials download. An officials row that can't be read withholds its
    /// company's officers and owners; one whose company can't be told withholds every company's.
    let build
        (corporations: Stream)
        (officials: Stream voption)
        (date: BulkFileDate)
        (ct: CancellationToken)
        : ICompanyRegistry =
        let parentOf column row =
            normalNumber (column "ParentEntityNumber" row)

        // Officials by company, and for each officials row that couldn't be read, the company it names if that can be read.
        let byEntity, broken =
            match officials with
            | ValueSome file ->
                let rows, broken = table ct officialColumns file

                let grouped =
                    rows
                    |> Seq.choose (fun (column, row) ->
                        parentOf column row
                        |> ValueOption.map (fun parent -> struct (parent, official column row))
                        |> ValueOption.toOption)
                    |> Bulk.grouped

                ValueSome grouped, broken |> List.map (fun (column, row) -> parentOf column row)
            | ValueNone -> ValueNone, []

        let brokenParents = broken |> List.choose ValueOption.toOption |> set
        let brokenWithoutParent = broken |> List.filter ValueOption.isNone |> List.length

        let records, repeated =
            fst (table ct entityColumns corporations)
            |> Seq.map (fun (column, row) -> record byEntity column row)
            |> Bulk.index (fun record -> normalNumber record.Company.Number)

        let withheld (key: string) =
            if brokenWithoutParent > 0 then
                ValueSome
                    $"{brokenWithoutParent} rows of the officials download couldn't be read or tied to a company, so any company's officials can be among them"
            elif brokenParents.Contains key then
                ValueSome
                    "A row of the officials download for this company couldn't be read, so its officials aren't all known"
            else
                ValueNone

        let registry =
            new BulkRegistry(
                coverage,
                normalNumber,
                invalidNumber,
                records,
                date,
                BulkAbsence.CantTell
                    "The download holds current entities only, so not finding this number proves nothing",
                (match officials with
                 | ValueSome _ -> "The officials download names no official and the row no registered agent"
                 | ValueNone -> "The officials download wasn't passed, and the row names no registered agent"),
                (fun _ ->
                    match officials with
                    | ValueSome _ ->
                        NotAvailable "The officials download names no shareholder, member, owner or partner"
                    | ValueNone -> NotAvailable "The officials download wasn't passed"),
                repeated
            )

        new BulkPartiesWithheld(registry, normalNumber, withheld, withheld)
