namespace CompanyDueDiligence

open System
open System.Text.Json
open System.Text.RegularExpressions
open System.Threading
open System.Threading.Tasks
open CompanyDueDiligence.Json

/// The DLCP Corporations Division's Corporate Registration (table 0), Trade Name (table 1) and Beneficial Owners (table 2) on DC GIS's
/// Business_Licensing_and_Grants feature service, read as they answered on 2026-11-08. Trade names and owners join to a registration by its file number.
module internal DistrictOfColumbia =

    let coverage: RegistryCoverage =
        {
            Registry = "District of Columbia DLCP Corporations Division"
            Jurisdictions = [ Jurisdiction.ofCode "US-DC" ]
            Standing = Support.Supported
            Concerns =
                Support.Partial "An overdue biennial report, from the next report's due date; no disputes or insolvency"
            Addresses = Support.Supported
            Search = Support.Partial "50 names starting with the term"
            Officers = Support.NotOffered "The open data lists no officers"
            Owners =
                Support.Partial
                    "Beneficial owners' names and addresses as filed, with no kind, share, nature of control or dates"
            Cost = AccessCost.Free
            RateLimit = RateLimitPolicy.NotPublished(2, TimeSpan.FromSeconds 1.)
            Licence = "CC BY 4.0, per each layer's ArcGIS item"
        }

    [<Literal>]
    let notListed =
        "The table is a copy of the register, so not finding this file number proves nothing"

    let private numberRegex =
        Regex @"\A(?:US-DC-[A-Z]{2}[0-9]{6,12}|[A-Z]?[0-9]{1,12})\z"

    /// A file number as the register holds it: digits, a letter and digits such as C00007209052, or the newer form such as US-DC-LL092606601.
    let normalNumber (number: string) =
        let normal =
            if isNull number then
                ""
            else
                number.Trim().ToUpperInvariant()

        if numberRegex.IsMatch normal then
            ValueSome normal
        else
            ValueNone

    /// The ENTITY_STATUS values the table held on 2026-11-08, all 12. Any other is Unknown.
    let standing (word: string voption) =
        match word |> ValueOption.map _.Trim() with
        | ValueNone
        | ValueSome "" -> CompanyStanding.Unknown ValueNone
        | ValueSome word ->
            match word with
            | "Active - In Good Standing" -> CompanyStanding.Active
            | "Active - Not in Good Standing" -> CompanyStanding.ActiveWithConcern "not in good standing"
            | "Active - In Good Standing - Pending Conversion" -> CompanyStanding.ActiveWithConcern "pending conversion"
            | "Dissolved" -> CompanyStanding.Dissolved
            | "Inactive - Cancelled"
            | "Revoked"
            | "Withdrawn"
            | "Terminated"
            | "Merged"
            | "Consolidated"
            | "Converted"
            | "Domesticated" -> CompanyStanding.Closed word
            | other -> CompanyStanding.Unknown(ValueSome other)

    let role (locale: string voption) =
        match locale with
        | ValueSome "Domestic" -> RecordRole.Home
        | ValueSome "Foreign" -> RecordRole.ForeignRegistration
        | _ -> RecordRole.NotStated

    let private address (lines: string list) city state zip country (kind: AddressKind) (e: JsonElement) =
        match
            lines
            |> List.choose (fun field -> OpenData.text field e |> ValueOption.toOption)
        with
        | [] -> ValueNone
        | lines ->
            ValueSome
                {
                    Kind = kind
                    Address =
                        {
                            Lines = lines
                            Locality = OpenData.text city e
                            Region = OpenData.text state e
                            PostalCode = OpenData.text zip e
                            Country = country |> ValueOption.bind (fun field -> OpenData.text field e)
                        }
                }

    /// The table spells its business address fields BUSNIESS_ADDRESS_LINE1 to 4.
    let private addresses (e: JsonElement) =
        match
            [
                address
                    [
                        "BUSNIESS_ADDRESS_LINE1"
                        "BUSNIESS_ADDRESS_LINE2"
                        "BUSNIESS_ADDRESS_LINE3"
                        "BUSNIESS_ADDRESS_LINE4"
                    ]
                    "BUSINESS_CITY"
                    "BUSINESS_STATE"
                    "ZIPCODE"
                    (ValueSome "BUSINESS_COUNTRY")
                    (AddressKind.Other "Business address on the DC corporate register")
                    e
                address
                    [ "RA_ADDRESS1"; "RA_ADDRESS2"; "RA_ADDRESS3"; "RA_ADDRESS4" ]
                    "RA_CITY"
                    "RA_STATE"
                    "RA_ZIPCODE"
                    ValueNone
                    AddressKind.RegisteredAgent
                    e
            ]
            |> List.choose ValueOption.toOption
        with
        | [] -> NotAvailable "The record holds no address"
        | found -> Available found

    /// NEXT_REPORTYEAR_DUE is text such as "04/01/2027".
    let reportDue (e: JsonElement) =
        OpenData.text "NEXT_REPORTYEAR_DUE" e
        |> ValueOption.bind (fun text ->
            match
                DateOnly.TryParseExact(
                    text,
                    "MM/dd/yyyy",
                    Globalization.CultureInfo.InvariantCulture,
                    Globalization.DateTimeStyles.None
                )
            with
            | true, date -> ValueSome date
            | _ -> ValueNone)

    /// A biennial report whose due date has passed is stated overdue. With no readable due date nothing about filings is known.
    let concerns (today: DateOnly) (e: JsonElement) =
        let others =
            CompanyConcern.all |> List.except [ CompanyConcern.AnnualFilingOverdue ]

        match reportDue e with
        | ValueSome due when due < today ->
            {
                Stated = [ CompanyConcern.AnnualFilingOverdue ]
                Unconfirmed = others
            }
        | ValueSome _ -> { Stated = []; Unconfirmed = others }
        | ValueNone -> CompanyConcerns.notPublished

    /// A foreign entity's date is the one it was organised at home, which the table gives apart from when it registered here. Concerns are
    /// judged on today.
    let companyOn (today: DateOnly) (e: JsonElement) : Company =
        let status = OpenData.text "ENTITY_STATUS" e
        let role = role (OpenData.text "LOCALE" e)

        {
            Name = required "BUSINESS_NAME" OpenData.text e
            OtherNames = []
            Number = required "FILE_NUMBER" OpenData.text e
            Jurisdiction = Jurisdiction.ofCode "US-DC"
            JurisdictionAsPublished = OpenData.text "LOCALE" e
            Standing = standing status
            Concerns = concerns today e
            StatusAsPublished = status
            LegalForm = OpenData.text "MODELTYPE" e
            IncorporatedOn =
                match role with
                | RecordRole.ForeignRegistration -> OpenData.arcgisDate "FOREIGN_DATEOF_ORGANIZATION" e
                | _ -> OpenData.arcgisDate "EFFECTIVE_DATE" e
            CeasedOn = ValueNone
            Addresses = addresses e
            Role = role
            HomeRegistration = ValueNone
            EstablishmentOf = ValueNone
        }

    /// A search hit, which carries no concerns.
    let company (e: JsonElement) =
        { companyOn DateOnly.MinValue e with
            Concerns = CompanyConcerns.notPublished
        }

    /// Every trade name filed for the entity, current or not: a former name is still a name to screen. An answer of limit rows or more, or one
    /// the service cut short, may leave a name out, so it is unreadable.
    let tradeNames (limit: int) (root: JsonElement) =
        let page = OpenData.arcgisPage root

        if page.Truncated || page.Rows.Length >= limit then
            raise (InvalidJson $"{limit} or more trade name rows, more than one answer reads")

        page.Rows
        |> List.choose (fun e -> OpenData.text "TRADE_NAME" e |> ValueOption.toOption)
        |> OpenData.distinctInOrder

    /// The record and the date it was last modified, which stands as the data's date. An answer the service cut short holds more records than
    /// it shows, so they can't be told apart.
    let found (today: DateOnly) (root: JsonElement) =
        let page = OpenData.arcgisPage root

        match page.Rows with
        | rows when page.Truncated ->
            NotAvailable $"More than {rows.Length} records carry this number, so the source can't tell which is meant",
            ValueNone
        | rows ->
            let asOf =
                match rows with
                | [ only ] -> OpenData.arcgisDate "DCS_LAST_MOD_DTTM" only
                | _ -> ValueNone

            OpenData.foundIn (NotAvailable notListed) (companyOn today) rows, asOf

    /// An owner as filed: a name and one line of address. The table gives no kind, share or date, so each owner leaves its details unconfirmed.
    let private owner (e: JsonElement) : Owner =
        let name = OpenData.text "NAME" e

        {
            Name = name
            Kind = PartyKind.Unknown ValueNone
            Control =
                [
                    {
                        Kind = ControlKind.Other
                        Band = ValueNone
                        AsPublished = "Beneficial owner"
                    }
                ]
            Current = true
            Since = ValueNone
            EndedOn = ValueNone
            BornIn = ValueNone
            Nationality = ValueNone
            CountryOfResidence = ValueNone
            Address =
                OpenData.text "ADDRESS" e
                |> ValueOption.map (fun line ->
                    {
                        Lines = [ line ]
                        Locality = ValueNone
                        Region = ValueNone
                        PostalCode = ValueNone
                        Country = ValueNone
                    })
            Entity = ValueNone
            IdentityVerification = ValueNone
        }

    let owners (limit: int) (root: JsonElement) =
        let page = OpenData.arcgisPage root

        match page.Rows with
        | _ when page.Truncated || page.Rows.Length >= limit ->
            NotAvailable $"The table holds {limit} or more owners for this file number, more than one answer reads"
        | [] ->
            NotAvailable "The table lists no beneficial owners for this file number, which doesn't say there are none"
        | rows ->
            let listed = rows |> List.map owner

            Available
                {
                    Owners = listed
                    Statements = []
                    Gaps =
                        [
                            OwnershipGap.OwnerDetailsNotConfirmed
                            if listed |> List.exists (fun owner -> owner.Name.IsNone) then
                                OwnershipGap.OwnerUnreadable
                        ]
                }

    let personSearch =
        Support.Partial
            "Beneficial owners whose name holds every word asked, up to 100, with the business name and status the owners table carries; the table has no dates, and no officers are published"

    /// The beneficial owners named as asked. Complete when the service neither cut the answer short nor reached the limit.
    let people (asked: PersonName) (limit: int) (root: JsonElement) : PersonSearchResult =
        let page = OpenData.arcgisPage root

        {
            Matches =
                page.Rows
                |> List.choose (fun e ->
                    match OpenData.text "NAME" e with
                    | ValueSome name when PersonName.matches asked name ->
                        Some
                            {
                                PersonMatch.Name = name
                                Role = ValueSome "Beneficial owner"
                                Current = ValueNone
                                CompanyName = OpenData.text "BUSINESSNAME" e
                                CompanyNumber = OpenData.text "INITIALFILENUMBER" e
                                CompanyStatus = OpenData.text "STATUS" e
                                AppointedOn = ValueNone
                                EndedOn = ValueNone
                            }
                    | _ -> None)
            TotalMatches = ValueNone
            Complete = not page.Truncated && page.Rows.Length < limit
        }

/// The District of Columbia as an ICompanyRegistry. A file number not found is NotAvailable: the table is a copy.
[<Sealed>]
type internal DistrictOfColumbiaRegistry(connection: SourceConnection) =
    static let searchLimit = 50
    static let ownerLimit = 500
    static let tradeNameLimit = 500
    static let personLimit = 100

    static let service =
        "https://maps2.dcgis.dc.gov/dcgis/rest/services/DCGIS_DATA/Business_Licensing_and_Grants_WebMercator/FeatureServer"

    static let registrations = service + "/0"
    static let tradeNameTable = service + "/1"
    static let beneficialOwners = service + "/2"

    let client =
        SourceClient(connection, SourceAnswer.pacing 2 (TimeSpan.FromSeconds 1.))

    let coverage = DistrictOfColumbia.coverage

    let offNumber number (send: string -> Task<Result<Evidence<'T>, RegistryFailure>>) =
        match DistrictOfColumbia.normalNumber number with
        | ValueSome normal -> send normal
        | ValueNone ->
            Task.FromResult(
                Error(
                    RegistryFailure.InvalidRequest
                        "A DC file number is digits with an optional letter, such as C00007209052"
                )
            )

    let get (uri: Uri) (ct: CancellationToken) =
        task {
            match! client.GetAsync(uri, ct) with
            | Ok reply when reply.Status = 404 -> return Error(RegistryFailure.RequestRefused "HTTP 404")
            | other -> return other
        }

    let search (name: string) (matching: NameMatch) (jurisdiction: Jurisdiction) (ct: CancellationToken) =
        SourceAnswer.inCoverage coverage jurisdiction (fun () ->
            NameSearch.withName name matching (fun () ->
                task {
                    match! get (OpenData.arcgisName registrations "BUSINESS_NAME" name matching searchLimit) ct with
                    | Error failure -> return Error failure
                    | Ok reply ->
                        return
                            SourceAnswer.json reply (fun root ->
                                let page = OpenData.arcgisPage root

                                let found =
                                    page.Rows
                                    |> List.map DistrictOfColumbia.company
                                    |> OpenData.nameMatches name matching searchLimit

                                { found with
                                    Complete = found.Complete && not page.Truncated
                                })
                            |> Result.map (fun found ->
                                SourceAnswer.evidence coverage reply ValueNone (Available found))
                }))

    interface INameMatchSearch with
        member _.SearchByNameAsync
            (name: string, matching: NameMatch, jurisdiction: Jurisdiction, ct: CancellationToken)
            =
            search name matching jurisdiction ct

    // The owners table carries each owner's business name and status, so one call answers.
    interface IPersonSearch with
        member _.PersonSearch = DistrictOfColumbia.personSearch

        member _.SearchPeopleAsync(name: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                PeopleSearch.withName name (fun asked ->
                    task {
                        match! get (OpenData.arcgisWords beneficialOwners "NAME" asked.Words personLimit) ct with
                        | Error failure -> return Error failure
                        | Ok reply ->
                            return
                                SourceAnswer.json reply (DistrictOfColumbia.people asked personLimit)
                                |> Result.map (fun found ->
                                    SourceAnswer.evidence coverage reply ValueNone (Available found))
                    }))

    interface ICompanyRegistry with
        member _.Coverage = coverage

        member _.TryFindAsync(number: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                offNumber number (fun id ->
                    task {
                        match! get (OpenData.arcgisWhere registrations "FILE_NUMBER" id 2) ct with
                        | Error failure -> return Error failure
                        | Ok reply ->
                            let today = DateOnly.FromDateTime reply.At.UtcDateTime

                            match SourceAnswer.json reply (DistrictOfColumbia.found today) with
                            | Error failure -> return Error failure
                            | Ok(Available(ValueSome company), asOf) ->
                                // A failed trade name read fails the lookup, so that no name goes unscreened.
                                match!
                                    get
                                        (OpenData.arcgisWhere tradeNameTable "INITIAL_FILENUMBER" id tradeNameLimit)
                                        ct
                                with
                                | Error failure -> return Error failure
                                | Ok names ->
                                    return
                                        SourceAnswer.json names (DistrictOfColumbia.tradeNames tradeNameLimit)
                                        |> Result.map (fun held ->
                                            let named =
                                                { company with
                                                    OtherNames = held |> List.filter ((<>) company.Name)
                                                }

                                            { SourceAnswer.evidence coverage reply asOf (Available(ValueSome named)) with
                                                Raw = ValueSome $"{reply.Body}\n{names.Body}"
                                            })
                            | Ok(found, asOf) -> return Ok(SourceAnswer.evidence coverage reply asOf found)
                    }))

        member _.SearchAsync(name: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            search name NameMatch.StartsWith jurisdiction ct

        member _.OfficersAsync(_: string, jurisdiction: Jurisdiction, _: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                Task.FromResult(
                    Ok(SourceAnswer.unasked coverage client.Now (NotAvailable "The open data lists no officers"))
                ))

        // The owners table is public, so both purposes get the same answer.
        member _.OwnersAsync(number: string, jurisdiction: Jurisdiction, _: OwnerPurpose, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                offNumber number (fun pid ->
                    task {
                        match! get (OpenData.arcgisWhere beneficialOwners "INITIALFILENUMBER" pid ownerLimit) ct with
                        | Error failure -> return Error failure
                        | Ok reply ->
                            return
                                SourceAnswer.json reply (DistrictOfColumbia.owners ownerLimit)
                                |> Result.map (SourceAnswer.evidence coverage reply ValueNone)
                    }))
