namespace CompanyDueDiligence

open System
open System.Text.Json
open System.Text.RegularExpressions
open System.Threading
open System.Threading.Tasks
open CompanyDueDiligence.Json

/// The Colorado Department of State's "Business Entities in Colorado" dataset on data.colorado.gov (4ykn-tg5h), read as it answered on 2026-11-08.
module internal Colorado =

    let coverage: RegistryCoverage =
        {
            Registry = "Colorado Department of State business entities"
            Jurisdictions = [ Jurisdiction.ofCode "US-CO" ]
            Standing = Support.Supported
            Concerns =
                Support.NotOffered
                    "The dataset states no filing dates, disputes or insolvency; Delinquent is read into the standing"
            Addresses = Support.Supported
            Search = Support.Partial "50 names starting with the term"
            Officers = Support.NotOffered "Colorado's register collects no officers"
            Owners = Support.NotOffered "The dataset lists no owners"
            Cost = AccessCost.Free
            RateLimit = RateLimitPolicy.NotPublished(2, TimeSpan.FromSeconds 1.)
            Licence =
                "Public Domain, per the dataset's metadata. Read without a key; a Socrata app token (X-App-Token) would raise the portal's limits"
        }

    /// The dataset is a daily copy, so a number registered since, or one the copy leaves out, isn't in it.
    [<Literal>]
    let notListed =
        "The dataset is a daily copy of the register, so not finding this number proves nothing"

    let private numberRegex = Regex @"\A[0-9]{10,11}\z"

    /// A Colorado entity ID: the year and seven digits, eleven in all; one older ID of ten digits is in the dataset.
    let normalNumber (number: string) =
        let normal = if isNull number then "" else number.Trim()

        if numberRegex.IsMatch normal then
            ValueSome normal
        else
            ValueNone

    /// Every entitystatus value the dataset held on 2026-11-08, all 15. Any other is Unknown.
    let standing (word: string voption) =
        match word |> ValueOption.map _.Trim() with
        | ValueNone
        | ValueSome "" -> CompanyStanding.Unknown ValueNone
        | ValueSome word ->
            match word with
            | "Good Standing" -> CompanyStanding.Active
            | "Exists" -> CompanyStanding.Registered(ValueSome word)
            | "Delinquent"
            | "Noncompliant"
            | "Registered Agent Resigned" -> CompanyStanding.ActiveWithConcern word
            | "Voluntarily Dissolved"
            | "Administratively Dissolved"
            | "Judicially Dissolved"
            | "Dissolved (Term Expired)" -> CompanyStanding.Dissolved
            | "Withdrawn"
            | "Revoked"
            | "Merged"
            | "Converted"
            | "Consolidated"
            | "Effectiveness Prevented" -> CompanyStanding.Closed word
            | other -> CompanyStanding.Unknown(ValueSome other)

    let private address (prefix: string) (kind: AddressKind) (e: JsonElement) =
        let field name = OpenData.text (prefix + name) e

        match [ field "address1"; field "address2" ] |> List.choose ValueOption.toOption with
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
                            PostalCode = field "zipcode"
                            Country = field "country"
                        }
                }

    let private addresses (e: JsonElement) =
        match
            [
                address "principal" AddressKind.PrincipalOffice e
                address "mailing" AddressKind.Postal e
                address "agentprincipal" AddressKind.RegisteredAgent e
            ]
            |> List.choose ValueOption.toOption
        with
        | [] -> NotAvailable "The row holds no address"
        | found -> Available found

    /// The column is spelled jurisdictonofformation in the dataset. CO is a home record; any other place a registration of a foreign entity.
    let role (formedIn: string voption) =
        match formedIn with
        | ValueSome "CO" -> RecordRole.Home
        | ValueSome _ -> RecordRole.ForeignRegistration
        | ValueNone -> RecordRole.NotStated

    let company (e: JsonElement) : Company =
        let name = required "entityname" OpenData.text e
        let status = OpenData.text "entitystatus" e
        let formedIn = OpenData.text "jurisdictonofformation" e
        let role = role formedIn

        {
            Name = name
            OtherNames = []
            Number = required "entityid" OpenData.text e
            Jurisdiction = Jurisdiction.ofCode "US-CO"
            JurisdictionAsPublished = formedIn
            Standing = standing status
            Concerns = CompanyConcerns.notPublished
            StatusAsPublished = status
            LegalForm = OpenData.text "entitytype" e
            IncorporatedOn =
                match role with
                | RecordRole.Home -> SourceAnswer.dateOfTimestamp "entityformdate" e
                | _ -> ValueNone
            CeasedOn = ValueNone
            Addresses = addresses e
            Role = role
            HomeRegistration =
                match role with
                | RecordRole.ForeignRegistration ->
                    ValueSome
                        {
                            LegalName = ValueSome name
                            LegalForm = ValueNone
                            GoverningLaw = ValueNone
                            Country = formedIn
                            Register = ValueNone
                            RegistrationNumber = ValueNone
                        }
                | _ -> ValueNone
            EstablishmentOf = ValueNone
        }

/// Colorado as an ICompanyRegistry. A number not found is NotAvailable: the dataset is a copy.
[<Sealed>]
type internal ColoradoRegistry(connection: SourceConnection) =
    static let searchLimit = 50
    static let dataset = "https://data.colorado.gov/resource/4ykn-tg5h.json"

    let client =
        SourceClient(connection, SourceAnswer.pacing 2 (TimeSpan.FromSeconds 1.))

    let coverage = Colorado.coverage

    let notOffered reason jurisdiction =
        SourceAnswer.inCoverage coverage jurisdiction (fun () ->
            Task.FromResult(Ok(SourceAnswer.unasked coverage client.Now (NotAvailable reason))))

    let search (name: string) (matching: NameMatch) (jurisdiction: Jurisdiction) (ct: CancellationToken) =
        SourceAnswer.inCoverage coverage jurisdiction (fun () ->
            NameSearch.withName name matching (fun () ->
                task {
                    let uri = OpenData.socrataName dataset "entityname" name matching searchLimit

                    match! client.GetAsync(uri, ct) with
                    | Error failure -> return Error failure
                    | Ok reply when reply.Status = 404 -> return Error(RegistryFailure.RequestRefused "HTTP 404")
                    | Ok reply ->
                        return
                            SourceAnswer.json reply (fun root ->
                                OpenData.socrataRows root
                                |> List.map Colorado.company
                                |> OpenData.nameMatches name matching searchLimit)
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
                match Colorado.normalNumber number with
                | ValueNone ->
                    Task.FromResult(
                        Error(
                            RegistryFailure.InvalidRequest
                                "A Colorado entity ID is ten or eleven digits, such as 20131234567"
                        )
                    )
                | ValueSome pid ->
                    task {
                        // entityid is a number column, so it is matched with a simple filter rather than a quoted literal.
                        let uri = Uri $"{dataset}?entityid={pid}&$limit=2"

                        match! client.GetAsync(uri, ct) with
                        | Error failure -> return Error failure
                        | Ok reply when reply.Status = 404 -> return Error(RegistryFailure.RequestRefused "HTTP 404")
                        | Ok reply ->
                            return
                                SourceAnswer.json
                                    reply
                                    (OpenData.socrataRows
                                     >> OpenData.foundIn (NotAvailable Colorado.notListed) Colorado.company)
                                |> Result.map (SourceAnswer.evidence coverage reply reply.LastModified)
                    })

        member _.SearchAsync(name: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            search name NameMatch.StartsWith jurisdiction ct

        member _.OfficersAsync(_: string, jurisdiction: Jurisdiction, _: CancellationToken) =
            notOffered "Colorado's register collects no officers" jurisdiction

        member _.OwnersAsync(_: string, jurisdiction: Jurisdiction, _: OwnerPurpose, _: CancellationToken) =
            notOffered "The dataset lists no owners" jurisdiction
