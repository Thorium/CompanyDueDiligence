namespace CompanyDueDiligence

open System
open System.Text.Json
open System.Text.RegularExpressions
open System.Threading
open CompanyDueDiligence.Json

/// OrgBook BC's API v4, read as it answered on 2026-11-08, with the parameters its OpenAPI document at /api/v4/?format=openapi lists. A topic's
/// registration credential carries the BC Registries' number, names, status, type, registration date and home jurisdiction.
module internal BritishColumbia =

    let coverage: RegistryCoverage =
        {
            Registry = "OrgBook BC (British Columbia)"
            Jurisdictions = [ Jurisdiction.ofCode "CA-BC" ]
            Standing =
                Support.Partial
                    "ACT or HIS as the registration credential states it; HIS doesn't say whether dissolved, amalgamated or continued out"
            Concerns = Support.NotOffered "The credential states no overdue filings, disputes or insolvency"
            Addresses = Support.NotOffered "The registration credential carries no address"
            Search = Support.Partial "The first page of OrgBook's topic search, active and historical"
            Officers = Support.NotOffered "OrgBook lists no officers"
            Owners = Support.NotOffered "OrgBook lists no owners"
            Cost = AccessCost.Free
            RateLimit = RateLimitPolicy.NotPublished(2, TimeSpan.FromSeconds 1.)
            Licence =
                "Access Only License - British Columbia, as the API's OpenAPI document states; not an open licence"
        }

    /// OrgBook is an index of the credentials BC Registries issued, so a number not found in it can still be on the register.
    [<Literal>]
    let notListed =
        "OrgBook indexes credentials BC Registries issued, so not finding this number doesn't show the register lacks it"

    let private numberRegex = Regex @"\A[A-Z]{1,3}[0-9]{7}\z"

    /// A BC Registries number: a prefix such as BC, A, C or FM and seven digits, in upper case without spaces.
    let normalNumber (number: string) =
        let normal =
            if isNull number then
                ""
            else
                number.Trim().Replace(" ", "").ToUpperInvariant()

        if numberRegex.IsMatch normal then
            ValueSome normal
        else
            ValueNone

    /// ACT and HIS are the values seen; the API documentation names ACT and "Historical". Anything else is Unknown.
    let standing (status: string voption) =
        match status with
        | ValueSome "ACT" -> CompanyStanding.Active
        | ValueSome "HIS" -> CompanyStanding.Closed "Historical"
        | ValueSome ""
        | ValueNone -> CompanyStanding.Unknown ValueNone
        | ValueSome other -> CompanyStanding.Unknown(ValueSome other)

    /// The two entity types the API documentation names; any other stays as its code.
    let private legalForm (code: string) =
        match code with
        | "BC" -> "BC Company"
        | "SP" -> "Sole Proprietorship"
        | other -> other

    let private attribute (kind: string) (topic: JsonElement) =
        array topic "attributes"
        |> List.tryFind (fun item -> tryString "type" item = ValueSome kind)
        |> ValueOption.ofOption
        |> ValueOption.bind (SourceAnswer.trimmed "value")

    let private dateOf (kind: string) (topic: JsonElement) =
        array topic "attributes"
        |> List.tryFind (fun item -> tryString "type" item = ValueSome kind)
        |> ValueOption.ofOption
        |> ValueOption.bind (SourceAnswer.dateOfTimestamp "value")

    /// Names of the topic other than its business number, which is the CRA's and not a name.
    let private names (topic: JsonElement) =
        array topic "names"
        |> List.filter (fun name ->
            match tryString "type" name with
            | ValueSome kind -> kind.StartsWith("entity_name", StringComparison.Ordinal)
            | ValueNone -> false)
        |> List.choose (fun name -> SourceAnswer.trimmed "text" name |> ValueOption.toOption)

    let company (topic: JsonElement) : Company =
        let number = required "source_id" tryString topic

        let name =
            match names topic with
            | first :: _ -> first
            | [] -> raise (InvalidJson "The topic has no entity name")

        let status = attribute "entity_status" topic
        let home = attribute "home_jurisdiction" topic
        let standing = standing status

        {
            Name = name
            OtherNames = names topic |> List.filter ((<>) name) |> List.distinct
            Number = number
            Jurisdiction = Jurisdiction.ofCode "CA-BC"
            JurisdictionAsPublished = home
            Standing = standing
            Concerns = CompanyConcerns.notPublished
            StatusAsPublished =
                match status, attribute "reason_description" topic with
                | ValueSome status, ValueSome reason -> ValueSome $"{status} ({reason})"
                | status, _ -> status
            LegalForm = attribute "entity_type" topic |> ValueOption.map legalForm
            IncorporatedOn = dateOf "registration_date" topic
            CeasedOn =
                match standing with
                | CompanyStanding.Closed _ -> dateOf "entity_status_effective" topic
                | _ -> ValueNone
            Addresses = NotAvailable "The registration credential carries no address"
            Role =
                match home with
                | ValueSome "BC" -> RecordRole.Home
                | ValueSome _ -> RecordRole.ForeignRegistration
                | ValueNone -> RecordRole.NotStated
            HomeRegistration =
                match home with
                | ValueSome place when place <> "BC" ->
                    ValueSome
                        {
                            LegalName = ValueNone
                            LegalForm = ValueNone
                            GoverningLaw = ValueNone
                            Country = ValueSome place
                            Register = ValueNone
                            RegistrationNumber = ValueNone
                        }
                | _ -> ValueNone
            EstablishmentOf = ValueNone
        }

    let private topics (root: JsonElement) =
        match tryProp "results" root with
        | ValueSome list when list.ValueKind = JsonValueKind.Array -> list.EnumerateArray() |> List.ofSeq
        | _ -> raise (InvalidJson "The answer has no results list")

    /// The topic whose source_id is the number. The search also matches names and other numbers, so any other topic is left out.
    let lookup (number: string) (root: JsonElement) =
        let held =
            topics root
            |> List.filter (fun topic -> tryString "source_id" topic = ValueSome number)

        match held with
        | [] -> NotAvailable notListed, ValueNone
        | [ only ] -> Available(ValueSome(company only)), ValueSome(only.GetRawText())
        | several ->
            NotAvailable $"{several.Length} topics carry this number, so the source can't tell which is meant",
            ValueNone

    let search (root: JsonElement) =
        let found = topics root |> List.map company
        let total = tryInt "total" root
        OpenData.matches found.Length total found

/// British Columbia as an ICompanyRegistry, over OrgBook BC. A number not found is NotAvailable.
[<Sealed>]
type internal BritishColumbiaRegistry(connection: SourceConnection) =
    let client =
        SourceClient(connection, SourceAnswer.pacing 2 (TimeSpan.FromSeconds 1.))

    let coverage = BritishColumbia.coverage
    let baseUri = "https://orgbook.gov.bc.ca/api/v4/search/topic"

    let unasked reason =
        Threading.Tasks.Task.FromResult(Ok(SourceAnswer.unasked coverage client.Now (NotAvailable reason)))

    interface ICompanyRegistry with
        member _.Coverage = coverage

        member _.TryFindAsync(number: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                match BritishColumbia.normalNumber number with
                | ValueNone ->
                    Threading.Tasks.Task.FromResult(
                        Error(
                            RegistryFailure.InvalidRequest
                                "A BC Registries number is a prefix such as BC, A or FM and seven digits"
                        )
                    )
                | ValueSome number ->
                    task {
                        // Historical companies are left out unless inactive credentials are asked for too.
                        let uri = Uri $"{baseUri}?q={Uri.EscapeDataString number}&inactive=any"

                        match! client.GetAsync(uri, ct) with
                        | Error failure -> return Error failure
                        | Ok reply when reply.Status = 404 -> return Error(RegistryFailure.RequestRefused "HTTP 404")
                        | Ok reply ->
                            return
                                SourceAnswer.json reply (BritishColumbia.lookup number)
                                |> Result.map (fun (found, raw) ->
                                    { SourceAnswer.evidence coverage reply ValueNone found with
                                        Raw = raw |> ValueOption.orElse (ValueSome reply.Body)
                                    })
                    })

        member _.SearchAsync(name: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                if String.IsNullOrWhiteSpace name then
                    Threading.Tasks.Task.FromResult(Error(RegistryFailure.InvalidRequest "A search needs a term"))
                else
                    task {
                        let uri = Uri $"{baseUri}?q={Uri.EscapeDataString(name.Trim())}&inactive=any"

                        match! client.GetAsync(uri, ct) with
                        | Error failure -> return Error failure
                        | Ok reply when reply.Status = 404 -> return Error(RegistryFailure.RequestRefused "HTTP 404")
                        | Ok reply ->
                            return
                                SourceAnswer.json reply BritishColumbia.search
                                |> Result.map (fun found ->
                                    SourceAnswer.evidence coverage reply ValueNone (Available found))
                    })

        member _.OfficersAsync(_: string, jurisdiction: Jurisdiction, _: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () -> unasked "OrgBook lists no officers")

        member _.OwnersAsync(_: string, jurisdiction: Jurisdiction, _: OwnerPurpose, _: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () -> unasked "OrgBook lists no owners")
