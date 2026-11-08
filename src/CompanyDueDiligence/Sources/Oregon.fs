namespace CompanyDueDiligence

open System
open System.Text.Json
open System.Text.RegularExpressions
open System.Threading
open System.Threading.Tasks
open CompanyDueDiligence.Json

/// The Oregon Secretary of State's "Active Businesses - ALL" dataset on data.oregon.gov (tckn-sxa6), read as it answered on 2026-11-08. It holds
/// active businesses only, one row per associated name: principal place of business, mailing address, registered agent, and for an assumed
/// business name its authorized representative.
module internal Oregon =

    let coverage: RegistryCoverage =
        {
            Registry = "Oregon Secretary of State active businesses"
            Jurisdictions = [ Jurisdiction.ofCode "US-OR" ]
            Standing =
                Support.Partial
                    "Active businesses only, from a weekly copy: one listed is Active, and one not listed may be inactive or newer"
            Concerns = Support.NotOffered "The dataset states no filing dates, disputes or insolvency"
            Addresses = Support.Supported
            Search = Support.Partial "50 active businesses whose names start with the term"
            Officers =
                Support.NotOffered
                    "The dataset names registered agents, and for assumed business names an authorized representative; no officers"
            Owners = Support.NotOffered "The dataset lists no owners"
            Cost = AccessCost.Free
            RateLimit = RateLimitPolicy.NotPublished(2, TimeSpan.FromSeconds 1.)
            Licence = "Public domain, per ORS 276A.368"
        }

    [<Literal>]
    let notListed =
        "The dataset lists active businesses only, so not finding this number proves nothing"

    let private numberRegex = Regex @"\A[0-9]{1,9}\z"

    /// A registry number of up to nine digits. The register's pages write it with a hyphen before the last two, which is taken out.
    let normalNumber (number: string) =
        let normal =
            if isNull number then
                ""
            else
                number.Trim().Replace("-", "").Replace(" ", "")

        if numberRegex.IsMatch normal then
            ValueSome normal
        else
            ValueNone

    /// Entity types that are names on the register, not entities.
    let private nameFilings =
        set [ "ASSUMED BUSINESS NAME"; "RESERVED NAME"; "REGISTERED NAME" ]

    let private addressKind (nameType: string voption) =
        match nameType with
        | ValueSome "PRINCIPAL PLACE OF BUSINESS" -> AddressKind.PrincipalOffice
        | ValueSome "MAILING ADDRESS" -> AddressKind.Postal
        | ValueSome "REGISTERED AGENT" -> AddressKind.RegisteredAgent
        | ValueSome other -> AddressKind.Other other
        | ValueNone -> AddressKind.Other "Address on the Oregon register"

    let private address (e: JsonElement) =
        match
            [ OpenData.text "address" e; OpenData.text "address_continued" e ]
            |> List.choose ValueOption.toOption
        with
        | [] -> ValueNone
        | lines ->
            ValueSome
                {
                    Kind = addressKind (OpenData.text "associated_name_type" e)
                    Address =
                        {
                            Lines = lines
                            Locality = OpenData.text "city" e
                            Region = OpenData.text "state" e
                            PostalCode = OpenData.text "zip" e
                            Country = ValueNone
                        }
                }

    /// OR is a home record and any other jurisdiction a foreign one. With none, the entity type's DOMESTIC or FOREIGN decides.
    let role (jurisdiction: string voption) (entityType: string voption) =
        match jurisdiction, entityType with
        | ValueSome "OR", _ -> RecordRole.Home
        | ValueSome _, _ -> RecordRole.ForeignRegistration
        | ValueNone, ValueSome kind when kind.StartsWith "DOMESTIC " -> RecordRole.Home
        | ValueNone, ValueSome kind when kind.StartsWith "FOREIGN " -> RecordRole.ForeignRegistration
        | ValueNone, _ -> RecordRole.NotStated

    /// One company from the rows of one registry number, which repeat its name, type and date on each.
    let company (rows: JsonElement list) : Company =
        let first = List.head rows
        let name = required "business_name" OpenData.text first
        let entityType = OpenData.text "entity_type" first
        let place = OpenData.text "jurisdiction" first
        let role = role place entityType

        {
            Name = name
            OtherNames = []
            Number = required "registry_number" OpenData.text first
            Jurisdiction = Jurisdiction.ofCode "US-OR"
            JurisdictionAsPublished = place
            Standing = CompanyStanding.Active
            Concerns = CompanyConcerns.notPublished
            StatusAsPublished = ValueSome "Active"
            LegalForm = entityType
            IncorporatedOn =
                match role with
                | RecordRole.Home -> SourceAnswer.dateOfTimestamp "registry_date" first
                | _ -> ValueNone
            CeasedOn = ValueNone
            Addresses =
                match rows |> List.choose (address >> ValueOption.toOption) with
                | [] -> NotAvailable "The rows hold no address"
                | found -> Available found
            Role = role
            HomeRegistration =
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
            EstablishmentOf = ValueNone
        }

    /// The rows for one number, collapsed to one company. Rows naming more than one business, or a name filing, aren't a company.
    let found (root: JsonElement) =
        match OpenData.socrataRows root with
        | [] -> NotAvailable notListed
        | rows ->
            let identities =
                rows
                |> List.map (fun row -> OpenData.text "business_name" row, OpenData.text "entity_type" row)
                |> List.distinct

            match identities with
            | [ _, ValueSome kind ] when nameFilings.Contains kind ->
                NotAvailable $"This number is an Oregon {kind.ToLowerInvariant()}, a name and not an entity"
            | [ _ ] -> Available(ValueSome(company rows))
            | several ->
                NotAvailable
                    $"The rows for this number name {several.Length} businesses, so the dataset can't tell which is meant"

    let matchOf (e: JsonElement) : CompanyMatch =
        {
            Name = required "business_name" OpenData.text e
            Number = required "registry_number" OpenData.text e
            StatusAsPublished = ValueSome "Active"
            Address = ValueNone
        }

/// Oregon as an ICompanyRegistry. A number not found is NotAvailable: the dataset lists active businesses only.
[<Sealed>]
type internal OregonRegistry(connection: SourceConnection) =
    static let searchLimit = 50
    static let rowLimit = 50
    static let dataset = "https://data.oregon.gov/resource/tckn-sxa6.json"

    let client =
        SourceClient(connection, SourceAnswer.pacing 2 (TimeSpan.FromSeconds 1.))

    let coverage = Oregon.coverage

    let notOffered reason jurisdiction =
        SourceAnswer.inCoverage coverage jurisdiction (fun () ->
            Task.FromResult(Ok(SourceAnswer.unasked coverage client.Now (NotAvailable reason))))

    let search (name: string) (matching: NameMatch) (jurisdiction: Jurisdiction) (ct: CancellationToken) =
        SourceAnswer.inCoverage coverage jurisdiction (fun () ->
            NameSearch.withName name matching (fun () ->
                task {
                    // Rows repeat per associated name, so the query groups them to one per business.
                    let named = OpenData.socrataName dataset "business_name" name matching searchLimit
                    let fields = Uri.EscapeDataString "registry_number,business_name,entity_type"

                    let uri =
                        Uri $"{named.OriginalString}&$select={fields}&$group={fields}&$order=business_name"

                    match! client.GetAsync(uri, ct) with
                    | Error failure -> return Error failure
                    | Ok reply when reply.Status = 404 -> return Error(RegistryFailure.RequestRefused "HTTP 404")
                    | Ok reply ->
                        return
                            SourceAnswer.json reply (fun root ->
                                {
                                    Matches =
                                        OpenData.socrataRows root
                                        |> List.map Oregon.matchOf
                                        |> OpenData.exactOnly name matching
                                    TotalMatches = ValueNone
                                    // Active businesses only, so an inactive one with the name is never among the matches.
                                    Complete = false
                                })
                            |> Result.map (fun found ->
                                SourceAnswer.evidence coverage reply reply.LastModified (Available found))
                }))

    interface INameMatchSearch with
        member _.SearchByNameAsync
            (name: string, matching: NameMatch, jurisdiction: Jurisdiction, ct: CancellationToken)
            =
            search name matching jurisdiction ct

    interface ICompanyRegistry with
        member _.Coverage = coverage

        member _.TryFindAsync(number: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                match Oregon.normalNumber number with
                | ValueNone ->
                    Task.FromResult(
                        Error(
                            RegistryFailure.InvalidRequest
                                "An Oregon registry number is up to nine digits, such as 10003093"
                        )
                    )
                | ValueSome pid ->
                    task {
                        match! client.GetAsync(OpenData.socrataWhere dataset "registry_number" pid rowLimit, ct) with
                        | Error failure -> return Error failure
                        | Ok reply when reply.Status = 404 -> return Error(RegistryFailure.RequestRefused "HTTP 404")
                        | Ok reply ->
                            return
                                SourceAnswer.json reply Oregon.found
                                |> Result.map (SourceAnswer.evidence coverage reply reply.LastModified)
                    })

        member _.SearchAsync(name: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            search name NameMatch.StartsWith jurisdiction ct

        member _.OfficersAsync(_: string, jurisdiction: Jurisdiction, _: CancellationToken) =
            notOffered
                "The dataset names registered agents and authorized representatives of assumed names, no officers"
                jurisdiction

        member _.OwnersAsync(_: string, jurisdiction: Jurisdiction, _: OwnerPurpose, _: CancellationToken) =
            notOffered "The dataset lists no owners" jurisdiction
