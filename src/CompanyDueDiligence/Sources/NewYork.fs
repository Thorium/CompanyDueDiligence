namespace CompanyDueDiligence

open System
open System.Text.Json
open System.Text.RegularExpressions
open System.Threading
open System.Threading.Tasks
open CompanyDueDiligence.Json

/// The New York Department of State's datasets on data.ny.gov, read as they answered on 2026-11-08: Active Corporations (n9v6-gdp6), monthly and
/// active only; Entity Status History (3gg2-jgnp), weekly, the status after each filing; and All Filings (63wc-4exh), weekly.
module internal NewYork =

    let coverage: RegistryCoverage =
        {
            Registry = "New York Department of State corporations"
            Jurisdictions = [ Jurisdiction.ofCode "US-NY" ]
            Standing =
                Support.Partial
                    "The status after the latest filing in the weekly status history, where it isn't Active; else Active from the monthly active corporations list"
            Concerns = Support.NotOffered "The datasets state no overdue filings, disputes or insolvency"
            Addresses =
                Support.Partial
                    "Service of process, registered agent and location addresses of an active corporation; none for an inactive one"
            Search =
                Support.Partial
                    "50 active corporations whose names start with the term, and 50 entities from All Filings, dissolved ones among them, with no status; never complete"
            Officers =
                Support.Partial
                    "The chief executive officer's name and address only, and only for an active corporation"
            Owners = Support.NotOffered "The datasets list no owners"
            Cost = AccessCost.Free
            RateLimit = RateLimitPolicy.NotPublished(2, TimeSpan.FromSeconds 1.)
            Licence = "None stated; OPEN-NY terms: revocable licence, no attribution, a no-bots clause"
        }

    [<Literal>]
    let notListed =
        "Neither the active corporations list nor the status history holds this number, which proves nothing: both are copies"

    let private numberRegex = Regex @"\A[0-9]{1,8}\z"

    /// A DOS ID: digits, held without leading zeros.
    let normalNumber (number: string) =
        let normal = if isNull number then "" else number.Trim()

        if numberRegex.IsMatch normal then
            match normal.TrimStart '0' with
            | "" -> ValueNone
            | pid -> ValueSome pid
        else
            ValueNone

    /// The status history's words on 2026-11-08, InActive among them once. Inactive covers dissolution, merger, surrender and more alike, so it is
    /// Closed with its word rather than Dissolved. Any other is Unknown.
    let standing (word: string voption) =
        match word |> ValueOption.map _.Trim() with
        | ValueNone
        | ValueSome "" -> CompanyStanding.Unknown ValueNone
        | ValueSome word ->
            match word with
            | "Active" -> CompanyStanding.Active
            | "Suspended" -> CompanyStanding.ActiveWithConcern word
            | "Inactive"
            | "InActive" -> CompanyStanding.Closed "Inactive"
            | "Discontinued" -> CompanyStanding.Closed word
            | other -> CompanyStanding.Unknown(ValueSome other)

    let private address (prefix: string) (kind: AddressKind) (e: JsonElement) =
        let field name = OpenData.text (prefix + name) e

        // The name line, such as "C/O CORPORATE CREATIONS NETWORK INC.", is part of where papers go, so it leads the lines.
        match
            [ field "name"; field "address_1"; field "address_2" ]
            |> List.choose ValueOption.toOption
        with
        | [] -> ValueNone
        | lines ->
            ValueSome
                {
                    Kind = kind
                    Address =
                        {
                            Lines = lines
                            Locality = field "city"
                            Region = field "state"
                            PostalCode = field "zip"
                            Country = ValueNone
                        }
                }

    let private roleOf (homeWord: string) (place: string voption) =
        match place with
        | ValueSome place when String.Equals(place, homeWord, StringComparison.OrdinalIgnoreCase) -> RecordRole.Home
        | ValueSome _ -> RecordRole.ForeignRegistration
        | ValueNone -> RecordRole.NotStated

    let private homeOf (role: RecordRole) (name: string) (place: string voption) =
        match role with
        | RecordRole.ForeignRegistration ->
            ValueSome
                {
                    LegalName = ValueSome name
                    LegalForm = ValueNone
                    GoverningLaw = ValueNone
                    Country = place
                    Register = ValueNone
                    RegistrationNumber = ValueNone
                }
        | _ -> ValueNone

    /// A row of Active Corporations: the list says it is active, as of the month end it was taken.
    let activeCompany (e: JsonElement) : Company =
        let name = required "current_entity_name" OpenData.text e
        let place = OpenData.text "jurisdiction" e
        let role = roleOf "New York" place

        {
            Name = name
            OtherNames = []
            Number = required "dos_id" OpenData.text e
            Jurisdiction = Jurisdiction.ofCode "US-NY"
            JurisdictionAsPublished = place
            Standing = CompanyStanding.Active
            Concerns = CompanyConcerns.notPublished
            StatusAsPublished = ValueSome "Active"
            LegalForm = OpenData.text "entity_type" e
            IncorporatedOn =
                match role with
                | RecordRole.Home -> SourceAnswer.dateOfTimestamp "initial_dos_filing_date" e
                | _ -> ValueNone
            CeasedOn = ValueNone
            Addresses =
                match
                    [
                        address "dos_process_" AddressKind.Service e
                        address "registered_agent_" AddressKind.RegisteredAgent e
                        address "location_" AddressKind.PrincipalOffice e
                    ]
                    |> List.choose ValueOption.toOption
                with
                | [] -> NotAvailable "The row holds no address"
                | found -> Available found
            Role = role
            HomeRegistration = homeOf role name place
            EstablishmentOf = ValueNone
        }

    /// The latest status row, as the query orders them: its status, the filing that set it and the date that was filed. None when the history
    /// holds no status for the number.
    let latestStatus (root: JsonElement) =
        match OpenData.socrataRows root with
        | [] -> ValueNone
        | latest :: _ ->
            ValueSome(
                required "status" OpenData.text latest,
                required "film_num" OpenData.text latest,
                SourceAnswer.dateOfTimestamp "date_filed" latest
            )

    /// The active list is monthly and the status history weekly, so the history's latest status, when it isn't Active, decides over the list.
    /// The list's row is kept for its name and addresses.
    let withLatestStatus (latest: (string * string * DateOnly voption) voption) (company: Company) =
        match latest with
        | ValueSome(status, _, filed) when not (CompanyStanding.isActive (standing (ValueSome status))) ->
            let on =
                filed
                |> ValueOption.map (fun date -> date.ToString("yyyy-MM-dd", Globalization.CultureInfo.InvariantCulture))
                |> ValueOption.defaultValue "an undated filing"

            { company with
                Standing = standing (ValueSome status)
                StatusAsPublished =
                    ValueSome
                        $"{status} in the status history, filed {on}; Active on the monthly active corporations list"
            }
        | _ -> company

    /// A company from the filing that set its latest status. The filings hold no address.
    let filedCompany (status: string) (e: JsonElement) : Company =
        let name = required "corp_name" OpenData.text e
        let place = OpenData.text "juris" e
        let role = roleOf "NY" place
        let standing = standing (ValueSome status)

        {
            Name = name
            OtherNames = []
            Number = required "corpid_num" OpenData.text e
            Jurisdiction = Jurisdiction.ofCode "US-NY"
            JurisdictionAsPublished = place
            Standing = standing
            Concerns = CompanyConcerns.notPublished
            StatusAsPublished =
                match OpenData.text "documenttype" e with
                | ValueSome document -> ValueSome $"{status}, after {document}"
                | ValueNone -> ValueSome status
            LegalForm = OpenData.text "entitytype" e
            IncorporatedOn = ValueNone
            CeasedOn =
                match standing with
                | CompanyStanding.Closed _ -> SourceAnswer.dateOfTimestamp "eff_date" e
                | _ -> ValueNone
            Addresses = NotAvailable "The filings datasets hold no address for an entity not on the active list"
            Role = role
            HomeRegistration = homeOf role name place
            EstablishmentOf = ValueNone
        }

    /// The chief executive officer the active list names, at the address it gives.
    let chiefExecutive (e: JsonElement) =
        OpenData.text "chairman_name" e
        |> ValueOption.map (fun name ->
            let lines =
                [ "chairman_address_1"; "chairman_address_2" ]
                |> List.choose (fun field -> OpenData.text field e |> ValueOption.toOption)

            {
                Name = name
                Kind = PartyKind.Individual
                Role = OfficerRole.Other "Chief executive officer"
                Current = true
                AppointedOn = ValueNone
                EndedOn = ValueNone
                BornIn = ValueNone
                Nationality = ValueNone
                CountryOfResidence = ValueNone
                Address =
                    match lines with
                    | [] -> ValueNone
                    | lines ->
                        ValueSome
                            {
                                Lines = lines
                                Locality = OpenData.text "chairman_city" e
                                Region = OpenData.text "chairman_state" e
                                PostalCode = OpenData.text "chairman_zip" e
                                Country = ValueNone
                            }
                Entity = ValueNone
                IdentityVerification = ValueNone
            })

    let officers (root: JsonElement) =
        match OpenData.socrataRows root with
        | [] ->
            NotAvailable
                "Only the active corporations list names a chief executive officer, and this number isn't on it"
        | [ only ] ->
            match chiefExecutive only with
            | ValueSome officer -> Available [ officer ]
            | ValueNone -> NotAvailable "The record names no chief executive officer"
        | several -> NotAvailable $"{several.Length} rows carry this number, so the dataset can't tell which is meant"

    /// All Filings rows grouped by DOS ID and name. They carry no status or address.
    let filedMatches (root: JsonElement) =
        OpenData.socrataRows root
        |> List.map (fun e ->
            {
                CompanyMatch.Name = required "corp_name" OpenData.text e
                Number = required "corpid_num" OpenData.text e
                StatusAsPublished = ValueNone
                Address = ValueNone
            })

    /// The active list's matches, then each DOS ID only All Filings holds, once, with no status. Never complete: each is one page of a copy of
    /// the register.
    let mergedSearch (active: CompanyMatch list) (filed: CompanyMatch list) : SearchResult =
        let onActive = active |> List.map _.Number |> set

        {
            Matches =
                active
                @ (filed
                   |> List.filter (fun found -> not (onActive.Contains found.Number))
                   |> List.distinctBy _.Number)
            TotalMatches = ValueNone
            Complete = false
        }

    let personSearch =
        Support.Partial
            "Chief executive officers named on the monthly active corporations list, whose name holds every word asked, up to 100; inactive corporations and other officers aren't in it, so Complete is never true"

    /// The active corporations whose chief executive officer is named as asked.
    let people (asked: PersonName) (root: JsonElement) : PersonSearchResult =
        {
            Matches =
                OpenData.socrataRows root
                |> List.choose (fun e ->
                    match OpenData.text "chairman_name" e with
                    | ValueSome name when PersonName.matches asked name ->
                        Some
                            {
                                PersonMatch.Name = name
                                Role = ValueSome "Chief executive officer"
                                Current = ValueNone
                                CompanyName = OpenData.text "current_entity_name" e
                                CompanyNumber = OpenData.text "dos_id" e
                                CompanyStatus = ValueSome "Active"
                                AppointedOn = ValueNone
                                EndedOn = ValueNone
                            }
                    | _ -> None)
            TotalMatches = ValueNone
            // Active corporations only, so a role at an inactive one is never among the matches.
            Complete = false
        }

/// What the first lookup found: an answer to give, or the go-ahead to read the status history.
[<RequireQualifiedAccess>]
type internal NewYorkActive =
    | Answered of Availability<Company voption>
    | NotActive

/// New York as an ICompanyRegistry. A number off the active list is looked up in the status history, and then the filing that set its status.
/// A search reads the active list and All Filings.
[<Sealed>]
type internal NewYorkRegistry(connection: SourceConnection) =
    static let searchLimit = 50
    static let personLimit = 100
    static let active = "https://data.ny.gov/resource/n9v6-gdp6.json"
    static let history = "https://data.ny.gov/resource/3gg2-jgnp.json"
    static let filings = "https://data.ny.gov/resource/63wc-4exh.json"

    let client =
        SourceClient(connection, SourceAnswer.pacing 2 (TimeSpan.FromSeconds 1.))

    let coverage = NewYork.coverage

    let offNumber number (send: string -> Task<Result<Evidence<'T>, RegistryFailure>>) =
        match NewYork.normalNumber number with
        | ValueSome normal -> send normal
        | ValueNone ->
            Task.FromResult(Error(RegistryFailure.InvalidRequest "A New York DOS ID is digits, such as 4424185"))

    let get (uri: Uri) (ct: CancellationToken) =
        task {
            match! client.GetAsync(uri, ct) with
            | Ok reply when reply.Status = 404 -> return Error(RegistryFailure.RequestRefused "HTTP 404")
            | other -> return other
        }

    let readActive root =
        match OpenData.socrataRows root with
        | [] -> NewYorkActive.NotActive
        | rows -> NewYorkActive.Answered(OpenData.foundIn (NotAvailable NewYork.notListed) NewYork.activeCompany rows)

    /// Every answer read, kept as one JSON array in the order asked.
    let combined (replies: SourceReply list) =
        "[" + (replies |> List.map _.Body |> String.concat ",") + "]"

    let withRaw (replies: SourceReply list) asOf value =
        let last = List.last replies

        {
            Source = coverage.Registry
            RetrievedAt = last.At
            AsOf = asOf
            Raw = ValueSome(combined replies)
            Value = value
        }

    /// The status history's latest row for a DOS ID. corpid_num is a number column, so the ID, digits only, goes in unquoted.
    let latest (id: string) (ct: CancellationToken) =
        let where = Uri.EscapeDataString $"corpid_num = {id} AND status IS NOT NULL"
        let order = Uri.EscapeDataString "date_filed DESC, film_num DESC"
        get (Uri $"{history}?$where={where}&$order={order}&$limit=1") ct

    /// DOS IDs and names in All Filings, dissolved entities among them, whose name matches as asked, one row per pair.
    let filedNames (name: string) (matching: NameMatch) =
        let where =
            Uri.EscapeDataString(OpenData.nameCondition "upper(corp_name)" "like" name matching)

        let pair = Uri.EscapeDataString "corpid_num,corp_name"
        Uri $"{filings}?$select={pair}&$group={pair}&$where={where}&$limit={searchLimit}"

    /// An entity on the active list, with the status history's latest status over it when that isn't Active.
    let listed (id: string) (first: SourceReply) (company: Company) (ct: CancellationToken) =
        task {
            match! latest id ct with
            | Error failure -> return Error failure
            | Ok statusReply ->
                return
                    SourceAnswer.json statusReply NewYork.latestStatus
                    |> Result.map (fun status ->
                        withRaw
                            [ first; statusReply ]
                            first.LastModified
                            (Available(ValueSome(NewYork.withLatestStatus status company))))
        }

    let inactive (id: string) (first: SourceReply) (ct: CancellationToken) =
        task {
            match! latest id ct with
            | Error failure -> return Error failure
            | Ok statusReply ->
                match SourceAnswer.json statusReply NewYork.latestStatus with
                | Error failure -> return Error failure
                | Ok ValueNone ->
                    return Ok(withRaw [ first; statusReply ] statusReply.LastModified (NotAvailable NewYork.notListed))
                | Ok(ValueSome(status, film, _)) ->
                    let where =
                        Uri.EscapeDataString $"corpid_num = {id} AND film_num = {OpenData.literal film}"

                    match! get (Uri $"{filings}?$where={where}&$limit=2") ct with
                    | Error failure -> return Error failure
                    | Ok filingReply ->
                        let replies = [ first; statusReply; filingReply ]

                        let read root =
                            match OpenData.socrataRows root with
                            | [] -> NotAvailable "The status history names a filing the filings dataset doesn't hold"
                            | rows ->
                                OpenData.foundIn (NotAvailable NewYork.notListed) (NewYork.filedCompany status) rows

                        return
                            SourceAnswer.json filingReply read
                            |> Result.map (withRaw replies statusReply.LastModified)
        }

    /// The active list and All Filings, each queried for the name as asked.
    let search (name: string) (matching: NameMatch) (jurisdiction: Jurisdiction) (ct: CancellationToken) =
        SourceAnswer.inCoverage coverage jurisdiction (fun () ->
            NameSearch.withName name matching (fun () ->
                task {
                    match! get (OpenData.socrataName active "current_entity_name" name matching searchLimit) ct with
                    | Error failure -> return Error failure
                    | Ok activeReply ->
                        match! get (filedNames name matching) ct with
                        | Error failure -> return Error failure
                        | Ok filedReply ->
                            // Each list is narrowed before the merge, so that a name matching on one list keeps its DOS ID.
                            let activeMatches root =
                                (OpenData.socrataRows root
                                 |> List.map NewYork.activeCompany
                                 |> OpenData.nameMatches name matching searchLimit)
                                    .Matches

                            let filedMatches root =
                                NewYork.filedMatches root |> OpenData.exactOnly name matching

                            return
                                SourceAnswer.json activeReply activeMatches
                                |> Result.bind (fun listed ->
                                    SourceAnswer.json filedReply filedMatches
                                    |> Result.map (fun filed ->
                                        withRaw
                                            [ activeReply; filedReply ]
                                            activeReply.LastModified
                                            (Available(NewYork.mergedSearch listed filed))))
                }))

    interface INameMatchSearch with
        member _.SearchByNameAsync
            (name: string, matching: NameMatch, jurisdiction: Jurisdiction, ct: CancellationToken)
            =
            search name matching jurisdiction ct

    interface IPersonSearch with
        member _.PersonSearch = NewYork.personSearch

        member _.SearchPeopleAsync(name: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                PeopleSearch.withName name (fun asked ->
                    task {
                        match! get (OpenData.socrataWords active [ "chairman_name" ] asked.Words personLimit) ct with
                        | Error failure -> return Error failure
                        | Ok reply ->
                            return
                                SourceAnswer.json reply (NewYork.people asked)
                                |> Result.map (fun found ->
                                    SourceAnswer.evidence coverage reply reply.LastModified (Available found))
                    }))

    interface ICompanyRegistry with
        member _.Coverage = coverage

        member _.TryFindAsync(number: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                offNumber number (fun id ->
                    task {
                        match! get (OpenData.socrataWhere active "dos_id" id 2) ct with
                        | Error failure -> return Error failure
                        | Ok reply ->
                            match SourceAnswer.json reply readActive with
                            | Error failure -> return Error failure
                            | Ok(NewYorkActive.Answered(Available(ValueSome company))) ->
                                return! listed id reply company ct
                            | Ok(NewYorkActive.Answered found) ->
                                return Ok(SourceAnswer.evidence coverage reply reply.LastModified found)
                            | Ok NewYorkActive.NotActive -> return! inactive id reply ct
                    }))

        member _.SearchAsync(name: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            search name NameMatch.StartsWith jurisdiction ct

        member _.OfficersAsync(number: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                offNumber number (fun pid ->
                    task {
                        match! get (OpenData.socrataWhere active "dos_id" pid 2) ct with
                        | Error failure -> return Error failure
                        | Ok reply ->
                            return
                                SourceAnswer.json reply NewYork.officers
                                |> Result.map (SourceAnswer.evidence coverage reply reply.LastModified)
                    }))

        member _.OwnersAsync(_: string, jurisdiction: Jurisdiction, _: OwnerPurpose, _: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                Task.FromResult(
                    Ok(SourceAnswer.unasked coverage client.Now (NotAvailable "The datasets list no owners"))
                ))
