namespace CompanyDueDiligence

open System
open System.Text.Json
open System.Text.RegularExpressions
open System.Threading
open System.Threading.Tasks
open CompanyDueDiligence.Json

/// The Pennsylvania Department of State's "Registered Businesses in PA Current by County" dataset on data.pa.gov (xvd7-5r2c), one row per governor
/// or principal officer, and its distinct-business view (3urc-uaba) for search, read as they answered on 2026-11-08. Neither has a status
/// column, and the Department warns that the data shows more active businesses than exist.
module internal Pennsylvania =

    let coverage: RegistryCoverage =
        {
            Registry = "Pennsylvania Department of State registered businesses"
            Jurisdictions = [ Jurisdiction.ofCode "US-PA" ]
            Standing =
                Support.Partial
                    "No status column: a listed business reads as Registered, since the Department of State warns that its statutory limits on removing businesses leave more listed as active than exist"
            Concerns = Support.NotOffered "The dataset states no filing dates, disputes or insolvency"
            Addresses = Support.Partial "One business address, of a kind the dataset doesn't state"
            Search = Support.Partial "50 names starting with the term, from the distinct-business view"
            Officers =
                Support.Partial
                    "Governors and principal officers as listed, with no dates, so a former one can't be told from a current one and each reads as current"
            Owners = Support.NotOffered "The dataset lists no owners"
            Cost = AccessCost.Free
            RateLimit = RateLimitPolicy.NotPublished(2, TimeSpan.FromSeconds 1.)
            Licence =
                "Public Domain U.S. Government, per the datasets' metadata. Read without a key; a Socrata app token (X-App-Token) would raise the portal's limits"
        }

    [<Literal>]
    let notListed =
        "The dataset lists businesses it holds as active, so not finding this number proves nothing"

    let private numberRegex = Regex @"\A[0-9]{1,11}\z"

    /// A filing number, held as ten digits with leading zeros; one row of eleven digits is held as written.
    let normalNumber (number: string) =
        let normal = if isNull number then "" else number.Trim()

        if not (numberRegex.IsMatch normal) then ValueNone
        elif normal.Length = 11 then ValueSome normal
        else ValueSome(normal.PadLeft(10, '0'))

    let role (registration: string voption) =
        match registration with
        | ValueSome kind when kind.StartsWith "Domestic " -> RecordRole.Home
        | ValueSome kind when kind.StartsWith "Foreign " -> RecordRole.ForeignRegistration
        | _ -> RecordRole.NotStated

    let private nameOf (e: JsonElement) =
        OpenData.text "business_name_source_" e
        |> ValueOption.orElse (OpenData.text "business_name" e)
        |> ValueOption.defaultWith (fun () -> raise (InvalidJson "Missing or invalid field 'business_name'"))

    let private address (e: JsonElement) =
        match
            [ OpenData.text "address_line1" e; OpenData.text "address_line2" e ]
            |> List.choose ValueOption.toOption
        with
        | [] -> NotAvailable "The row holds no address"
        | lines ->
            Available
                [
                    {
                        Kind = AddressKind.Other "Business address on the Department of State's listing"
                        Address =
                            {
                                Lines = lines
                                Locality = OpenData.text "city" e
                                Region = OpenData.text "state" e
                                PostalCode = OpenData.text "zip" e
                                Country = ValueNone
                            }
                    }
                ]

    let company (e: JsonElement) : Company =
        let registration = OpenData.text "typeofbusinessregistration" e
        let role = role registration

        {
            Name = nameOf e
            OtherNames = []
            Number = required "filing_number" OpenData.text e
            Jurisdiction = Jurisdiction.ofCode "US-PA"
            JurisdictionAsPublished = ValueNone
            Standing = CompanyStanding.Registered ValueNone
            Concerns = CompanyConcerns.notPublished
            StatusAsPublished = ValueNone
            LegalForm = registration
            IncorporatedOn =
                match role with
                | RecordRole.Home -> SourceAnswer.dateOfTimestamp "creationdate" e
                | _ -> ValueNone
            CeasedOn = ValueNone
            Addresses = address e
            Role = role
            HomeRegistration = ValueNone
            EstablishmentOf = ValueNone
        }

    /// The rows for one number, which repeat the business on each officer's row. Rows naming different businesses aren't told apart.
    let found (root: JsonElement) =
        match OpenData.socrataRows root with
        | [] -> NotAvailable notListed
        | first :: _ as rows ->
            match rows |> List.map nameOf |> List.distinct with
            | [ _ ] -> Available(ValueSome(company first))
            | several ->
                NotAvailable
                    $"The rows for this number name {several.Length} businesses, so the dataset can't tell which is meant"

    /// Governor is Pennsylvania's word for a director or manager.
    let officerRole (partyType: string voption) =
        match partyType |> ValueOption.map _.Trim() with
        | ValueNone -> OfficerRole.Other ""
        | ValueSome word ->
            match word.ToUpperInvariant() with
            | "GOVERNOR"
            | "DIRECTOR"
            | "BOARD MEMBER"
            | "CHAIRMAN"
            | "CHAIR" -> OfficerRole.Director
            | "SECRETARY"
            | "ASSISTANT SECRETARY" -> OfficerRole.Secretary
            | _ -> OfficerRole.Other word

    let private officer (e: JsonElement) =
        match
            [ "first_name"; "middle_name"; "last_name" ]
            |> List.choose (fun field -> OpenData.text field e |> ValueOption.toOption)
        with
        | [] -> ValueNone
        | parts ->
            ValueSome
                {
                    Name = String.concat " " parts
                    Kind = PartyKind.Individual
                    Role = officerRole (OpenData.text "party_type" e)
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

    let officers (limit: int) (root: JsonElement) =
        match OpenData.socrataRows root with
        | [] -> NotAvailable notListed
        | rows when rows.Length >= limit ->
            NotAvailable $"The dataset holds {limit} or more rows for this number, more than one answer reads"
        | rows ->
            match rows |> List.choose (officer >> ValueOption.toOption) |> List.distinct with
            | [] ->
                NotAvailable "The dataset lists no governor or officer for this business, which doesn't say it has none"
            | listed -> Available listed

    let personSearch =
        Support.Partial
            "Governors and principal officers whose name parts hold every word asked, up to 100 rows; the dataset has no dates and lists businesses it holds as active only"

    /// The officers named as asked, with rows the dataset repeats taken once.
    let people (asked: PersonName) (limit: int) (root: JsonElement) : PersonSearchResult =
        let rows = OpenData.socrataRows root

        {
            Matches =
                rows
                |> List.choose (fun e ->
                    match PeopleSearch.joined [ "first_name"; "middle_name"; "last_name" ] e with
                    | ValueSome name when PersonName.matches asked name ->
                        Some
                            {
                                PersonMatch.Name = name
                                Role = OpenData.text "party_type" e
                                Current = ValueNone
                                CompanyName =
                                    OpenData.text "business_name_source_" e
                                    |> ValueOption.orElse (OpenData.text "business_name" e)
                                CompanyNumber = OpenData.text "filing_number" e
                                CompanyStatus = ValueNone
                                AppointedOn = ValueNone
                                EndedOn = ValueNone
                            }
                    | _ -> None)
                |> List.distinct
            TotalMatches = ValueNone
            Complete = false
        }

    /// The businesses a name query returned. Never complete: the data holds active registrations only.
    let search (limit: int) (root: JsonElement) =
        let found =
            OpenData.socrataRows root
            |> List.map company
            |> OpenData.matches limit ValueNone

        { found with Complete = false }

/// Pennsylvania as an ICompanyRegistry. A listed business is Registered, never Active, and one not listed is NotAvailable.
[<Sealed>]
type internal PennsylvaniaRegistry(connection: SourceConnection) =
    static let searchLimit = 50
    static let rowLimit = 500
    static let personLimit = 100
    static let dataset = "https://data.pa.gov/resource/xvd7-5r2c.json"
    static let distinct = "https://data.pa.gov/resource/3urc-uaba.json"

    let client =
        SourceClient(connection, SourceAnswer.pacing 2 (TimeSpan.FromSeconds 1.))

    let coverage = Pennsylvania.coverage

    let byNumber number (ct: CancellationToken) (read: JsonElement -> Availability<'T>) =
        match Pennsylvania.normalNumber number with
        | ValueNone ->
            Task.FromResult(
                Error(
                    RegistryFailure.InvalidRequest
                        "A Pennsylvania filing number is up to ten digits, such as 0000000378"
                )
            )
        | ValueSome id ->
            task {
                match! client.GetAsync(OpenData.socrataWhere dataset "filing_number" id rowLimit, ct) with
                | Error failure -> return Error failure
                | Ok reply when reply.Status = 404 -> return Error(RegistryFailure.RequestRefused "HTTP 404")
                | Ok reply ->
                    return
                        SourceAnswer.json reply read
                        |> Result.map (SourceAnswer.evidence coverage reply reply.LastModified)
            }

    let search (name: string) (matching: NameMatch) (jurisdiction: Jurisdiction) (ct: CancellationToken) =
        SourceAnswer.inCoverage coverage jurisdiction (fun () ->
            NameSearch.withName name matching (fun () ->
                task {
                    match!
                        client.GetAsync(OpenData.socrataName distinct "business_name" name matching searchLimit, ct)
                    with
                    | Error failure -> return Error failure
                    | Ok reply when reply.Status = 404 -> return Error(RegistryFailure.RequestRefused "HTTP 404")
                    | Ok reply ->
                        return
                            SourceAnswer.json
                                reply
                                (Pennsylvania.search searchLimit >> OpenData.keepExact name matching)
                            |> Result.map (fun found ->
                                SourceAnswer.evidence coverage reply reply.LastModified (Available found))
                }))

    interface INameMatchSearch with
        member _.SearchByNameAsync
            (name: string, matching: NameMatch, jurisdiction: Jurisdiction, ct: CancellationToken)
            =
            search name matching jurisdiction ct

    interface IPersonSearch with
        member _.PersonSearch = Pennsylvania.personSearch

        member _.SearchPeopleAsync(name: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                PeopleSearch.withName name (fun asked ->
                    task {
                        let uri =
                            OpenData.socrataWords
                                dataset
                                [ "first_name"; "middle_name"; "last_name" ]
                                asked.Words
                                personLimit

                        match! client.GetAsync(uri, ct) with
                        | Error failure -> return Error failure
                        | Ok reply when reply.Status = 404 -> return Error(RegistryFailure.RequestRefused "HTTP 404")
                        | Ok reply ->
                            return
                                SourceAnswer.json reply (Pennsylvania.people asked personLimit)
                                |> Result.map (fun found ->
                                    SourceAnswer.evidence coverage reply reply.LastModified (Available found))
                    }))

    interface ICompanyRegistry with
        member _.Coverage = coverage

        member _.TryFindAsync(number: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () -> byNumber number ct Pennsylvania.found)

        member _.SearchAsync(name: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            search name NameMatch.StartsWith jurisdiction ct

        member _.OfficersAsync(number: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                byNumber number ct (Pennsylvania.officers rowLimit))

        member _.OwnersAsync(_: string, jurisdiction: Jurisdiction, _: OwnerPurpose, _: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                Task.FromResult(
                    Ok(SourceAnswer.unasked coverage client.Now (NotAvailable "The dataset lists no owners"))
                ))
