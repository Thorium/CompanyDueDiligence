namespace CompanyDueDiligence

open System
open System.Text.Json
open System.Text.RegularExpressions
open System.Threading
open System.Threading.Tasks
open CompanyDueDiligence.Json

/// The Connecticut Business Registry's Business Master (n7gp-d28j) and Principals (ka36-64k6) datasets on data.ct.gov, read as they answered on
/// 2026-11-08. A principal links to its business by the master row's id, not by the business's number.
module internal Connecticut =

    let coverage: RegistryCoverage =
        {
            Registry = "Connecticut Secretary of the State business registry"
            Jurisdictions = [ Jurisdiction.ofCode "US-CT" ]
            Standing = Support.Supported
            Concerns =
                Support.Partial "An annual or first report past due, from sub_status; nothing on disputes or insolvency"
            Addresses = Support.Partial "The business and mailing addresses; agents are in a dataset not read"
            Search = Support.Partial "50 names starting with the term"
            Officers =
                Support.Partial
                    "Principals as reported, with no dates, so a former one can't be told from a current one and each reads as current; identical rows are collapsed"
            Owners = Support.NotOffered "The registry's datasets list no owners"
            Cost = AccessCost.Free
            RateLimit = RateLimitPolicy.NotPublished(2, TimeSpan.FromSeconds 1.)
            Licence =
                "Public Domain, per the Business Master's metadata; the Principals dataset states none. Read without a key; a Socrata app token (X-App-Token) would raise the portal's limits"
        }

    [<Literal>]
    let notListed =
        "The dataset is a daily copy of the registry, so not finding this number proves nothing"

    let private numberRegex = Regex @"\A[0-9]{1,7}\z"

    /// A business ID of up to seven digits, with its leading zeros back as the dataset holds it.
    let normalNumber (number: string) =
        let normal = if isNull number then "" else number.Trim()

        if numberRegex.IsMatch normal then
            ValueSome(normal.PadLeft(7, '0'))
        else
            ValueNone

    let private isPastDue (subStatus: string) =
        subStatus.EndsWith("past due", StringComparison.OrdinalIgnoreCase)

    /// The status and sub_status pairs the dataset held on 2026-11-08. A sub_status qualifies an Active status only; any status or Active
    /// sub_status not listed is Unknown.
    let standing (status: string voption) (subStatus: string voption) =
        let sub = subStatus |> ValueOption.map _.Trim() |> ValueOption.defaultValue ""

        match status |> ValueOption.map _.Trim() |> ValueOption.defaultValue "" with
        | "" -> CompanyStanding.Unknown ValueNone
        | "Active" ->
            match sub with
            | ""
            | "Annual report due"
            | "First report due" -> CompanyStanding.Active
            | "Annual report past due"
            | "First report past due"
            | "Admin Dissolution Initiated"
            | "Ready for dissolution"
            | "Ready for Forfeiture"
            | "Ready for Revocation"
            | "No Agent"
            | "Fraudulent Filing Complaint Sent" -> CompanyStanding.ActiveWithConcern sub
            | other -> CompanyStanding.Unknown(ValueSome $"Active, {other}")
        | "Active - Pending Domestication"
        | "Pending Merger"
        | "Pending Conversion"
        | "Pending Domestication" as word -> CompanyStanding.ActiveWithConcern word
        | "Pending Withdrawal" as word -> CompanyStanding.WindingUp word
        | "Dissolved" -> CompanyStanding.Dissolved
        | "Forfeited"
        | "Revoked"
        | "Withdrawn"
        | "Cancelled"
        | "Merged"
        | "Expired"
        | "Renunciated"
        | "Redomesticated"
        | "Domesticated"
        | "Consolidation"
        | "Converted Out"
        | "Converted"
        | "Removed" as word -> CompanyStanding.Closed word
        | "Recorded"
        | "Registered" as word -> CompanyStanding.Registered(ValueSome word)
        // Name reservations and rejected filings: rows that are no entity.
        | "Reserved"
        | "Expired Reservation"
        | "Reserved Cancel"
        | "Rejected"
        | "Pending Filing" as word -> CompanyStanding.Unknown(ValueSome word)
        | other -> CompanyStanding.Unknown(ValueSome other)

    let concerns (subStatus: string voption) =
        match subStatus with
        | ValueSome sub when isPastDue sub ->
            {
                Stated = [ CompanyConcern.AnnualFilingOverdue ]
                Unconfirmed = CompanyConcern.all |> List.filter ((<>) CompanyConcern.AnnualFilingOverdue)
            }
        | _ -> CompanyConcerns.notPublished

    let role (citizenship: string voption) =
        match citizenship with
        | ValueSome "Domestic" -> RecordRole.Home
        | ValueSome "Foreign" -> RecordRole.ForeignRegistration
        | _ -> RecordRole.NotStated

    let private addresses (e: JsonElement) =
        let business =
            OpenData.text "billingstreet" e
            |> ValueOption.map (fun street ->
                {
                    Kind = AddressKind.Other "Business address on the Connecticut Business Registry"
                    Address =
                        {
                            Lines =
                                [ Some street; OpenData.text "billing_unit" e |> ValueOption.toOption ]
                                |> List.choose id
                            Locality = OpenData.text "billingcity" e
                            Region = OpenData.text "billingstate" e
                            PostalCode = OpenData.text "billingpostalcode" e
                            Country = OpenData.text "billingcountry" e
                        }
                })

        // The mailing address comes as one line with its parts separated by commas, empty ones included.
        let mailing =
            OpenData.text "mailing_address" e
            |> ValueOption.map (fun line ->
                let parts =
                    line.Split(',')
                    |> Array.map _.Trim()
                    |> Array.filter (fun part -> part <> "")
                    |> String.concat ", "

                {
                    Kind = AddressKind.Postal
                    Address =
                        {
                            Lines = [ parts ]
                            Locality = ValueNone
                            Region = ValueNone
                            PostalCode = ValueNone
                            Country = ValueNone
                        }
                })

        match [ business; mailing ] |> List.choose ValueOption.toOption with
        | [] -> NotAvailable "The row holds no address"
        | found -> Available found

    let company (e: JsonElement) : Company =
        let status = OpenData.text "status" e
        let subStatus = OpenData.text "sub_status" e
        let role = role (OpenData.text "citizenship" e)

        let formedIn =
            OpenData.text "state_or_territory_formation" e
            |> ValueOption.orElse (OpenData.text "country_formation" e)
            |> ValueOption.orElse (OpenData.text "formation_place" e)

        {
            Name = required "name" OpenData.text e
            OtherNames = []
            Number = required "accountnumber" OpenData.text e
            Jurisdiction = Jurisdiction.ofCode "US-CT"
            JurisdictionAsPublished = OpenData.text "formation_place" e
            Standing = standing status subStatus
            Concerns = concerns subStatus
            StatusAsPublished =
                match status, subStatus with
                | ValueSome status, ValueSome sub -> ValueSome $"{status}; {sub}"
                | status, _ -> status
            LegalForm = OpenData.text "business_type" e
            IncorporatedOn =
                match role with
                | RecordRole.Home -> SourceAnswer.dateOfTimestamp "date_registration" e
                | _ -> ValueNone
            CeasedOn = SourceAnswer.dateOfTimestamp "dissolution_date" e
            Addresses = addresses e
            Role = role
            HomeRegistration =
                match role with
                | RecordRole.ForeignRegistration ->
                    ValueSome
                        {
                            LegalName = OpenData.text "business_name_in_state_country" e
                            LegalForm = ValueNone
                            GoverningLaw = ValueNone
                            Country = formedIn
                            Register = ValueNone
                            RegistrationNumber = ValueNone
                        }
                | _ -> ValueNone
            EstablishmentOf = ValueNone
        }

    /// The master row's own key, which principals carry as business_id.
    let key (e: JsonElement) = required "id" OpenData.text e

    let private officerRole (designation: string voption) =
        match designation with
        | ValueSome word when word.Contains("Director", StringComparison.OrdinalIgnoreCase) -> OfficerRole.Director
        | ValueSome word -> OfficerRole.Other word
        | ValueNone -> OfficerRole.Other "Principal"

    /// A principal at the business address it gave. The residence address the dataset also holds isn't read.
    let officer (e: JsonElement) : Officer =
        let person =
            [ "firstname"; "middlename"; "lastname"; "suffix" ]
            |> List.choose (fun field -> OpenData.text field e |> ValueOption.toOption)
            |> String.concat " "

        let name =
            OpenData.text "name__c" e
            |> ValueOption.orElse (if person = "" then ValueNone else ValueSome person)

        let street =
            [
                "business_street_address_1"
                "business_street_address_2"
                "business_street_address_3"
            ]
            |> List.choose (fun field -> OpenData.text field e |> ValueOption.toOption)

        {
            Name = name |> ValueOption.defaultValue PartyNames.NotGiven
            Kind =
                if person <> "" then
                    PartyKind.Individual
                else
                    PartyKind.Unknown ValueNone
            Role = officerRole (OpenData.text "designation" e)
            Current = true
            AppointedOn = ValueNone
            EndedOn = ValueNone
            BornIn = ValueNone
            Nationality = ValueNone
            CountryOfResidence = ValueNone
            Address =
                match street with
                | [] -> ValueNone
                | lines ->
                    ValueSome
                        {
                            Lines = lines
                            Locality = OpenData.text "business_city" e
                            Region = OpenData.text "business_state" e
                            PostalCode = OpenData.text "business_zip_code" e
                            Country = OpenData.text "business_country" e
                        }
            Entity = ValueNone
            IdentityVerification = ValueNone
        }

    /// Principals for one business, with rows the dataset repeats word for word taken once.
    let officers (limit: int) (root: JsonElement) =
        let rows = OpenData.socrataRows root

        if rows.Length >= limit then
            NotAvailable
                $"The dataset holds {limit} or more principal rows for this business, more than one answer reads"
        else
            match rows |> List.map _.GetRawText() |> OpenData.distinctInOrder with
            | [] -> NotAvailable "The dataset lists no principals for this business, which doesn't say it has none"
            | distinct ->
                distinct
                |> List.map (fun raw ->
                    use document = JsonDocument.Parse raw
                    officer document.RootElement)
                |> Available

    let personSearch =
        Support.Partial
            "Principals whose name holds every word asked, up to 100 rows, with each business from the Business Master; the dataset has no dates, so none reads as current or former"

    /// A principal's name: name__c, or its name parts.
    let principalName (e: JsonElement) =
        OpenData.text "name__c" e
        |> ValueOption.orElse (PeopleSearch.joined [ "firstname"; "middlename"; "lastname"; "suffix" ] e)

    /// The principals named as asked, read before the business join.
    let principalsNamed (asked: PersonName) (limit: int) (root: JsonElement) =
        let rows = OpenData.socrataRows root

        let named =
            rows
            |> List.filter (fun e -> principalName e |> ValueOption.exists (PersonName.matches asked))
            |> List.map _.GetRawText()
            |> OpenData.distinctInOrder

        named, rows.Length < limit

    /// Each principal named, with the business row its business_id leads to, if read.
    let personMatches (named: string list) (root: JsonElement) =
        let businesses =
            OpenData.socrataRows root |> List.map (fun e -> key e, e) |> Map.ofList

        named
        |> List.map (fun raw ->
            use document = JsonDocument.Parse raw
            let e = document.RootElement

            let business =
                OpenData.text "business_id" e
                |> ValueOption.bind (fun pid -> Map.tryFind pid businesses |> ValueOption.ofOption)

            {
                PersonMatch.Name = principalName e |> ValueOption.defaultValue PartyNames.NotGiven
                Role = OpenData.text "designation" e |> ValueOption.orElse (ValueSome "Principal")
                Current = ValueNone
                CompanyName = business |> ValueOption.bind (OpenData.text "name")
                CompanyNumber = business |> ValueOption.bind (OpenData.text "accountnumber")
                CompanyStatus = business |> ValueOption.bind (OpenData.text "status")
                AppointedOn = ValueNone
                EndedOn = ValueNone
            })

/// Connecticut as an ICompanyRegistry. Officers take two calls: the master row for its key, then the principals carrying it.
[<Sealed>]
type internal ConnecticutRegistry(connection: SourceConnection) =
    static let searchLimit = 50
    static let principalLimit = 1000
    static let personLimit = 100
    static let master = "https://data.ct.gov/resource/n7gp-d28j.json"
    static let principals = "https://data.ct.gov/resource/ka36-64k6.json"

    let client =
        SourceClient(connection, SourceAnswer.pacing 2 (TimeSpan.FromSeconds 1.))

    let coverage = Connecticut.coverage

    let offNumber number (send: string -> Task<Result<Evidence<'T>, RegistryFailure>>) =
        match Connecticut.normalNumber number with
        | ValueSome normal -> send normal
        | ValueNone ->
            Task.FromResult(
                Error(RegistryFailure.InvalidRequest "A Connecticut business ID is up to seven digits, such as 0645848")
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
                    match! get (OpenData.socrataName master "name" name matching searchLimit) ct with
                    | Error failure -> return Error failure
                    | Ok reply ->
                        return
                            SourceAnswer.json reply (fun root ->
                                OpenData.socrataRows root
                                |> List.map Connecticut.company
                                |> OpenData.nameMatches name matching searchLimit)
                            |> Result.map (fun found ->
                                SourceAnswer.evidence coverage reply reply.LastModified (Available found))
                }))

    interface INameMatchSearch with
        member _.SearchByNameAsync
            (name: string, matching: NameMatch, jurisdiction: Jurisdiction, ct: CancellationToken)
            =
            search name matching jurisdiction ct

    // Principals whose name holds every word, then one call for the businesses of those that match. A failed business read fails the search.
    interface IPersonSearch with
        member _.PersonSearch = Connecticut.personSearch

        member _.SearchPeopleAsync(name: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                PeopleSearch.withName name (fun asked ->
                    task {
                        match! get (OpenData.socrataWords principals [ "name__c" ] asked.Words personLimit) ct with
                        | Error failure -> return Error failure
                        | Ok listed ->
                            match SourceAnswer.json listed (Connecticut.principalsNamed asked personLimit) with
                            | Error failure -> return Error failure
                            | Ok([], complete) ->
                                return
                                    Ok(
                                        PeopleSearch.evidence
                                            coverage
                                            [ listed ]
                                            listed.LastModified
                                            (Available
                                                {
                                                    PersonSearchResult.Matches = []
                                                    TotalMatches = ValueNone
                                                    Complete = complete
                                                })
                                    )
                            | Ok(named, complete) ->
                                let ids =
                                    named
                                    |> List.choose (fun raw ->
                                        use document = JsonDocument.Parse raw
                                        OpenData.text "business_id" document.RootElement |> ValueOption.toOption)
                                    |> OpenData.distinctInOrder

                                match! get (OpenData.socrataIn master "id" ids personLimit) ct with
                                | Error failure -> return Error failure
                                | Ok businesses ->
                                    return
                                        SourceAnswer.json businesses (Connecticut.personMatches named)
                                        |> Result.map (fun matches ->
                                            PeopleSearch.evidence
                                                coverage
                                                [ listed; businesses ]
                                                listed.LastModified
                                                (Available
                                                    {
                                                        PersonSearchResult.Matches = matches
                                                        TotalMatches = ValueNone
                                                        Complete = complete
                                                    }))
                    }))

    interface ICompanyRegistry with
        member _.Coverage = coverage

        member _.TryFindAsync(number: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                offNumber number (fun pid ->
                    task {
                        match! get (OpenData.socrataWhere master "accountnumber" pid 2) ct with
                        | Error failure -> return Error failure
                        | Ok reply ->
                            return
                                SourceAnswer.json
                                    reply
                                    (OpenData.socrataRows
                                     >> OpenData.foundIn (NotAvailable Connecticut.notListed) Connecticut.company)
                                |> Result.map (SourceAnswer.evidence coverage reply reply.LastModified)
                    }))

        member _.SearchAsync(name: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            search name NameMatch.StartsWith jurisdiction ct

        member _.OfficersAsync(number: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                offNumber number (fun pid ->
                    task {
                        match! get (OpenData.socrataWhere master "accountnumber" pid 2) ct with
                        | Error failure -> return Error failure
                        | Ok reply ->
                            let keyOf root =
                                match OpenData.socrataRows root with
                                | [ only ] -> Ok(Connecticut.key only)
                                | [] -> Error Connecticut.notListed
                                | several ->
                                    Error
                                        $"{several.Length} rows carry this number, so the dataset can't tell which is meant"

                            match SourceAnswer.json reply keyOf with
                            | Error failure -> return Error failure
                            | Ok(Error reason) ->
                                return
                                    Ok(SourceAnswer.evidence coverage reply reply.LastModified (NotAvailable reason))
                            | Ok(Ok key) ->
                                match! get (OpenData.socrataWhere principals "business_id" key principalLimit) ct with
                                | Error failure -> return Error failure
                                | Ok listed ->
                                    return
                                        SourceAnswer.json listed (Connecticut.officers principalLimit)
                                        |> Result.map (SourceAnswer.evidence coverage listed listed.LastModified)
                    }))

        member _.OwnersAsync(_: string, jurisdiction: Jurisdiction, _: OwnerPurpose, _: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                Task.FromResult(
                    Ok(
                        SourceAnswer.unasked
                            coverage
                            client.Now
                            (NotAvailable "The registry's datasets list no owners")
                    )
                ))
