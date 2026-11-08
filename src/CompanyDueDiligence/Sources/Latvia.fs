namespace CompanyDueDiligence

open System
open System.Text.Json
open System.Text.RegularExpressions
open System.Threading
open CompanyDueDiligence.Json

/// The Latvian Register of Enterprises' open data on data.gov.lv, read through CKAN's datastore as it answered on 2026-11-08: the register
/// (package uz), officers (package officers) and beneficial owners (package patiesie-labuma-guveji). Column meanings are from the register's own
/// register.txt.
module internal Latvia =

    [<Literal>]
    let RegisterResource = "25e80bf3-f107-4ab4-89ef-251b5b9374e9"

    [<Literal>]
    let OfficersResource = "e665114a-73c2-4375-9470-55874b4cfa6b"

    [<Literal>]
    let OwnersResource = "20a9b26d-d056-4dbb-ae18-9ff23c87bdee"

    let coverage: RegistryCoverage =
        {
            Registry = "Latvian Register of Enterprises on data.gov.lv"
            Jurisdictions = [ Jurisdiction.ofCode "LV" ]
            Standing =
                Support.Partial
                    "Registered, or excluded with the reason; liquidation and insolvency proceedings are in datasets not read here"
            Concerns = Support.NotOffered "Liquidation, insolvency and suspensions are in datasets not read here"
            Addresses = Support.Supported
            Search = Support.Partial "50 records matching CKAN's full-text search"
            Officers = Support.Partial "Current representatives as the dataset lists them; former ones aren't in it"
            Owners =
                Support.Partial
                    "Registered beneficial owners, without the nature of their control; members and shareholders are in datasets not read here"
            Cost = AccessCost.Free
            RateLimit = RateLimitPolicy.NotPublished(2, TimeSpan.FromSeconds 1.)
            Licence = "CC0 1.0, as each package's metadata states"
        }

    /// The datasets are a daily copy, so a number missing from them may be newer than the copy.
    [<Literal>]
    let notListed =
        "The register's open data is a daily copy, so not finding this number proves nothing"

    let private numberRegex = Regex @"\A([0-9]{11}|[0-9]{9})\z"

    /// A unified registration number of eleven digits, or the nine-digit code of a subject that has none.
    let normalNumber (number: string) =
        let normal = if isNull number then "" else number.Trim().Replace(" ", "")

        if numberRegex.IsMatch normal then
            ValueSome normal
        else
            ValueNone

    /// closed is the reason for exclusion, L for liquidation and R for reorganisation, per register.txt; blank with no exclusion date is on the
    /// register, which has no status word. Anything else is Unknown.
    let standing (closed: string voption) (terminated: DateOnly voption) =
        match closed, terminated with
        | ValueSome "L", _ -> CompanyStanding.Dissolved
        | ValueSome "R", _ -> CompanyStanding.Closed "Excluded following reorganisation"
        | ValueNone, ValueNone -> CompanyStanding.Registered ValueNone
        | ValueNone, ValueSome _ -> CompanyStanding.Closed "Excluded from the register"
        | ValueSome other, _ -> CompanyStanding.Unknown(ValueSome other)

    /// AKF is a foreign trader's branch; FIL, a branch, doesn't say whose; every other type is formed here.
    let role (entityType: string voption) =
        match entityType with
        | ValueSome "AKF" -> RecordRole.ForeignRegistration
        | ValueSome "FIL"
        | ValueNone -> RecordRole.NotStated
        | ValueSome _ -> RecordRole.Home

    let company (e: JsonElement) : Company =
        let closed = OpenData.text "closed" e
        let terminated = SourceAnswer.dateOfTimestamp "terminated" e
        let entityType = OpenData.text "type" e

        {
            Name =
                OpenData.text "name" e
                |> ValueOption.defaultWith (fun () -> raise (InvalidJson "The record has no name"))
            OtherNames = []
            Number =
                OpenData.text "regcode" e
                |> ValueOption.defaultWith (fun () -> raise (InvalidJson "The record has no registration number"))
            Jurisdiction = Jurisdiction.ofCode "LV"
            JurisdictionAsPublished = ValueNone
            Standing = standing closed terminated
            Concerns = CompanyConcerns.notPublished
            StatusAsPublished =
                match closed, terminated with
                | ValueSome code, ValueSome date ->
                    ValueSome $"""closed {code}, terminated {date.ToString "yyyy-MM-dd"}"""
                | ValueSome code, ValueNone -> ValueSome $"closed {code}"
                | ValueNone, ValueSome date -> ValueSome $"""terminated {date.ToString "yyyy-MM-dd"}"""
                | ValueNone, ValueNone -> ValueNone
            LegalForm = OpenData.text "type_text" e
            IncorporatedOn = SourceAnswer.dateOfTimestamp "registered" e
            CeasedOn = terminated
            Addresses =
                match OpenData.text "address" e with
                | ValueNone -> NotAvailable "The record holds no legal address"
                | ValueSome line ->
                    Available
                        [
                            {
                                Kind = AddressKind.RegisteredOffice
                                Address =
                                    {
                                        Lines = [ line ]
                                        Locality = ValueNone
                                        Region = ValueNone
                                        PostalCode = OpenData.text "index" e
                                        Country = ValueNone
                                    }
                            }
                        ]
            Role = role entityType
            HomeRegistration = ValueNone
            EstablishmentOf = ValueNone
        }

    /// The record carrying this number. The filter is exact, so a record with another number fails the read.
    let lookup (number: string) (root: JsonElement) =
        let records, _ = OpenData.ckanRecords root

        match records with
        | [] -> NotAvailable notListed, ValueNone
        | [ only ] ->
            let found = company only

            if found.Number = number then
                Available(ValueSome found), ValueSome(only.GetRawText())
            else
                raise (InvalidJson "The answer holds another subject")
        | several ->
            NotAvailable $"{several.Length} records carry this number, so the source can't tell which is meant",
            ValueNone

    let search (limit: int) (root: JsonElement) =
        let records, total = OpenData.ckanRecords root
        OpenData.matches limit total (records |> List.map company)

    /// Positions seen in the dataset. A blank position takes the governing body's code.
    let officerRole (position: string voption) (body: string voption) =
        match position with
        | ValueSome("BOARD_MEMBER" | "CHAIR_OF_BOARD") -> OfficerRole.Director
        | ValueSome "LIQUIDATOR" -> OfficerRole.Other "Liquidator"
        | ValueSome "OWNER" -> OfficerRole.Other "Owner"
        | ValueSome other -> OfficerRole.Other other
        | ValueNone -> OfficerRole.Other(body |> ValueOption.defaultValue "")

    let private officer (e: JsonElement) : Officer =
        let name = OpenData.text "name" e
        let isEntity = OpenData.text "entity_type" e = ValueSome "LEGAL_ENTITY"

        {
            Name = name |> ValueOption.defaultValue PartyNames.NotPublished
            Kind =
                match OpenData.text "entity_type" e, name with
                | _, ValueNone -> PartyKind.Withheld
                | ValueSome "NATURAL_PERSON", _ -> PartyKind.Individual
                | ValueSome "LEGAL_ENTITY", _ -> PartyKind.LegalEntity
                | other, _ -> PartyKind.Unknown other
            Role = officerRole (OpenData.text "position" e) (OpenData.text "governing_body" e)
            Current = true
            AppointedOn = SourceAnswer.dateOfTimestamp "registered_on" e
            EndedOn = ValueNone
            BornIn = ValueNone
            Nationality = ValueNone
            CountryOfResidence = ValueNone
            Address = ValueNone
            Entity =
                if isEntity then
                    ValueSome
                        {
                            LegalName = name
                            LegalForm = ValueNone
                            GoverningLaw = ValueNone
                            Country = ValueNone
                            Register = ValueNone
                            RegistrationNumber = OpenData.text "legal_entity_registration_number" e
                        }
                else
                    ValueNone
            IdentityVerification = ValueNone
        }

    /// More rows than came back can't be read as the whole list.
    let private partial (records: JsonElement list) (total: int voption) =
        match total with
        | ValueSome total when total > records.Length -> true
        | _ -> false

    let officers (root: JsonElement) =
        match OpenData.ckanRecords root with
        | [], _ -> NotAvailable "The dataset lists no representatives for this number, which doesn't say there are none"
        | records, total when partial records total ->
            NotAvailable $"The dataset lists more representatives than the {records.Length} read"
        | records, _ -> Available(records |> List.map officer)

    let private owner (e: JsonElement) : Owner =
        let name =
            [ OpenData.text "forename" e; OpenData.text "surname" e ]
            |> List.choose ValueOption.toOption
            |> String.concat " "

        {
            Name = if name = "" then ValueNone else ValueSome name
            Kind =
                if name = "" then
                    PartyKind.Withheld
                else
                    PartyKind.Individual
            Control =
                [
                    {
                        Kind = ControlKind.Other
                        Band = ValueNone
                        AsPublished = "Registered beneficial owner; the nature of control isn't published"
                    }
                ]
            Current = true
            Since = SourceAnswer.dateOfTimestamp "registered_on" e
            EndedOn = ValueNone
            BornIn = ValueNone
            Nationality = OpenData.text "nationality" e
            CountryOfResidence = OpenData.text "residence" e
            Address = ValueNone
            Entity = ValueNone
            IdentityVerification = ValueNone
        }

    /// Registered beneficial owners. None listed says nothing: listed companies, for one, register none.
    let owners (root: JsonElement) =
        match OpenData.ckanRecords root with
        | [], _ ->
            NotAvailable "The dataset lists no beneficial owners for this number, which doesn't say there are none"
        | records, total when partial records total ->
            NotAvailable $"The dataset lists more beneficial owners than the {records.Length} read"
        | records, _ ->
            let listed = records |> List.map owner

            Available
                {
                    Owners = listed
                    Statements = []
                    Gaps =
                        if listed |> List.exists (fun found -> found.Name.IsNone) then
                            [ OwnershipGap.OwnerUnreadable ]
                        else
                            []
                }

    let personSearch =
        Support.Partial
            "Current representatives and registered beneficial owners matching CKAN's full-text search, up to 100 of each, with each company from the register; complete when CKAN's totals say every record came back"

    /// A role read from the officers or owners resource, before the company join: its raw record, and whether its name matched.
    type Found =
        {
            Roles: (PersonMatch * string) list
            /// The records CKAN says its search found.
            Total: int voption
            Complete: bool
        }

    let private found (read: JsonElement -> (PersonMatch * string) option) (root: JsonElement) =
        let records, total = OpenData.ckanRecords root

        {
            Roles = records |> List.choose read
            Total = total
            Complete =
                match total with
                | ValueSome total -> total <= records.Length
                | ValueNone -> false
        }

    /// A representative named as asked, with the number of the company it represents.
    let officersNamed (asked: PersonName) =
        found (fun e ->
            match OpenData.text "name" e, OpenData.text "at_legal_entity_registration_number" e with
            | ValueSome name, ValueSome number when PersonName.matches asked name ->
                Some(
                    {
                        PersonMatch.Name = name
                        Role =
                            OpenData.text "position" e
                            |> ValueOption.orElse (OpenData.text "governing_body" e)
                        Current = ValueSome true
                        CompanyName = ValueNone
                        CompanyNumber = ValueSome number
                        CompanyStatus = ValueNone
                        AppointedOn = SourceAnswer.dateOfTimestamp "registered_on" e
                        EndedOn = ValueNone
                    },
                    number
                )
            | _ -> None)

    /// A beneficial owner named as asked. The dataset doesn't say whether it holds former owners.
    let ownersNamed (asked: PersonName) =
        found (fun e ->
            match
                PeopleSearch.joined [ "forename"; "surname" ] e, OpenData.text "legal_entity_registration_number" e
            with
            | ValueSome name, ValueSome number when PersonName.matches asked name ->
                Some(
                    {
                        PersonMatch.Name = name
                        Role = ValueSome "Beneficial owner"
                        Current = ValueNone
                        CompanyName = ValueNone
                        CompanyNumber = ValueSome number
                        CompanyStatus = ValueNone
                        AppointedOn = SourceAnswer.dateOfTimestamp "registered_on" e
                        EndedOn = ValueNone
                    },
                    number
                )
            | _ -> None)

    /// Each role with its company's name and status from the register records read.
    let withCompanies (roles: (PersonMatch * string) list) (root: JsonElement) =
        let records, _ = OpenData.ckanRecords root

        let companies =
            records
            |> List.map company
            |> List.map (fun found -> found.Number, found)
            |> Map.ofList

        roles
        |> List.map (fun (role, number) ->
            match Map.tryFind number companies with
            | Some found ->
                { role with
                    CompanyName = ValueSome found.Name
                    CompanyStatus = found.StatusAsPublished
                }
            | None -> role)

/// Latvia as an ICompanyRegistry. A number not found is NotAvailable.
[<Sealed>]
type internal LatviaRegistry(connection: SourceConnection) =
    static let searchLimit = 50
    static let partyLimit = 200
    static let personLimit = 100

    let client =
        SourceClient(connection, SourceAnswer.pacing 2 (TimeSpan.FromSeconds 1.))

    let coverage = Latvia.coverage
    let site = "https://data.gov.lv/dati"

    let invalid () =
        Threading.Tasks.Task.FromResult(
            Error(
                RegistryFailure.InvalidRequest
                    "A Latvian registration number has eleven digits, or nine for a subject without one"
            )
        )

    /// The rows of resource where field is the number, read by parse.
    let rowsFor resource field (number: string) limit parse ct =
        task {
            let uri = OpenData.ckanWhere site resource [ field, number ] limit

            match! client.GetAsync(uri, ct) with
            | Error failure -> return Error failure
            | Ok reply when reply.Status = 404 -> return Error(RegistryFailure.RequestRefused "HTTP 404")
            | Ok reply -> return SourceAnswer.json reply parse |> Result.map (fun value -> reply, value)
        }

    /// A full-text search of a resource, read by parse.
    let searched resource (query: string) limit parse ct =
        task {
            match! client.GetAsync(OpenData.ckanSearch site resource query limit, ct) with
            | Error failure -> return Error failure
            | Ok reply when reply.Status = 404 -> return Error(RegistryFailure.RequestRefused "HTTP 404")
            | Ok reply -> return SourceAnswer.json reply parse |> Result.map (fun value -> reply, value)
        }

    // Representatives, then beneficial owners, then the register records of the companies of those whose name matched. A failed call fails the search.
    interface IPersonSearch with
        member _.PersonSearch = Latvia.personSearch

        member _.SearchPeopleAsync(name: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                PeopleSearch.withName name (fun asked ->
                    let query = String.concat " " asked.Words

                    task {
                        match! searched Latvia.OfficersResource query personLimit (Latvia.officersNamed asked) ct with
                        | Error failure -> return Error failure
                        | Ok(officerReply, officers) ->
                            match! searched Latvia.OwnersResource query personLimit (Latvia.ownersNamed asked) ct with
                            | Error failure -> return Error failure
                            | Ok(ownerReply, owners) ->
                                let roles = officers.Roles @ owners.Roles

                                let result matches : PersonSearchResult =
                                    {
                                        PersonSearchResult.Matches = matches
                                        TotalMatches = ValueOption.map2 (+) officers.Total owners.Total
                                        Complete = officers.Complete && owners.Complete
                                    }

                                match roles |> List.map snd |> OpenData.distinctInOrder with
                                | [] ->
                                    return
                                        Ok(
                                            PeopleSearch.evidence
                                                coverage
                                                [ officerReply; ownerReply ]
                                                ValueNone
                                                (Available(result []))
                                        )
                                | numbers ->
                                    let uri =
                                        OpenData.ckanAny site Latvia.RegisterResource "regcode" numbers numbers.Length

                                    match! client.GetAsync(uri, ct) with
                                    | Error failure -> return Error failure
                                    | Ok reply when reply.Status = 404 ->
                                        return Error(RegistryFailure.RequestRefused "HTTP 404")
                                    | Ok companyReply ->
                                        return
                                            SourceAnswer.json companyReply (Latvia.withCompanies roles)
                                            |> Result.map (fun matches ->
                                                PeopleSearch.evidence
                                                    coverage
                                                    [ officerReply; ownerReply; companyReply ]
                                                    ValueNone
                                                    (Available(result matches)))
                    }))

    interface ICompanyRegistry with
        member _.Coverage = coverage

        member _.TryFindAsync(number: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                match Latvia.normalNumber number with
                | ValueNone -> invalid ()
                | ValueSome number ->
                    task {
                        // Two records are enough to show the number isn't unique.
                        match! rowsFor Latvia.RegisterResource "regcode" number 2 (Latvia.lookup number) ct with
                        | Error failure -> return Error failure
                        | Ok(reply, (found, raw)) ->
                            return
                                Ok
                                    { SourceAnswer.evidence coverage reply ValueNone found with
                                        Raw = raw |> ValueOption.orElse (ValueSome reply.Body)
                                    }
                    })

        member _.SearchAsync(name: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                if String.IsNullOrWhiteSpace name then
                    Threading.Tasks.Task.FromResult(Error(RegistryFailure.InvalidRequest "A search needs a term"))
                else
                    task {
                        let uri = OpenData.ckanSearch site Latvia.RegisterResource name searchLimit

                        match! client.GetAsync(uri, ct) with
                        | Error failure -> return Error failure
                        | Ok reply when reply.Status = 404 -> return Error(RegistryFailure.RequestRefused "HTTP 404")
                        | Ok reply ->
                            return
                                SourceAnswer.json reply (Latvia.search searchLimit)
                                |> Result.map (fun found ->
                                    SourceAnswer.evidence coverage reply ValueNone (Available found))
                    })

        member _.OfficersAsync(number: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                match Latvia.normalNumber number with
                | ValueNone -> invalid ()
                | ValueSome number ->
                    task {
                        match!
                            rowsFor
                                Latvia.OfficersResource
                                "at_legal_entity_registration_number"
                                number
                                partyLimit
                                Latvia.officers
                                ct
                        with
                        | Error failure -> return Error failure
                        | Ok(reply, found) -> return Ok(SourceAnswer.evidence coverage reply ValueNone found)
                    })

        member _.OwnersAsync(number: string, jurisdiction: Jurisdiction, _: OwnerPurpose, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                match Latvia.normalNumber number with
                | ValueNone -> invalid ()
                | ValueSome number ->
                    task {
                        match!
                            rowsFor
                                Latvia.OwnersResource
                                "legal_entity_registration_number"
                                number
                                partyLimit
                                Latvia.owners
                                ct
                        with
                        | Error failure -> return Error failure
                        | Ok(reply, found) -> return Ok(SourceAnswer.evidence coverage reply ValueNone found)
                    })
