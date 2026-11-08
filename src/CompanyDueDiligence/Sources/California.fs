namespace CompanyDueDiligence

open System
open System.Text.Json
open System.Text.RegularExpressions
open System.Threading
open System.Threading.Tasks
open CompanyDueDiligence.Json

/// The California Secretary of State's BE Public Search API, written from its guide (version 1.0.4, 21 September 2022, marked draft) and the
/// developer portal's API definition, as read on 2026-11-08. Not run live: the API needs a subscription key, which the portal says only government
/// agencies get at this time.
module internal California =

    let coverage: RegistryCoverage =
        {
            Registry = "California Secretary of State business entities"
            Jurisdictions = [ Jurisdiction.ofCode "US-CA" ]
            Standing =
                Support.Partial
                    "Only Active and Suspended - SOS are documented, by the guide's examples; any other status word is Unknown"
            Concerns = Support.NotOffered "The guide documents no overdue filings, disputes or insolvency fields"
            Addresses = Support.Partial "Principal, mailing and registered agent addresses, as the record holds them"
            Search = Support.Partial "Names containing every keyword, the top 150"
            Officers = Support.NotOffered "The API lists the registered agent, not officers"
            Owners = Support.NotOffered "The API lists no owners"
            Cost =
                AccessCost.FreeWithKey
                    "Sign up at calicodev.sos.ca.gov and subscribe to CBC API Production, which needs approval; the portal says the API is available to government agencies only at this time, and its cost is unconfirmed"
            RateLimit = RateLimitPolicy.NotPublished(2, TimeSpan.FromSeconds 1.)
            Licence = "None stated; the portal says government agencies only, approval required"
        }

    /// The search returns the top 150 matches.
    [<Literal>]
    let SearchCap = 150

    let private numberRegex = Regex @"\A[A-Z]?[0-9]{6,12}\z"

    /// An entity number such as 202150010654 or C1234567. The guide gives no format, so only the shape is checked.
    let normalNumber (number: string) =
        let normal =
            if isNull number then
                ""
            else
                number.Trim().ToUpperInvariant().Replace(" ", "")

        if numberRegex.IsMatch normal then
            ValueSome normal
        else
            ValueNone

    /// The StatusDescription words the guide shows. Any other is Unknown.
    let standing (word: string voption) =
        match word |> ValueOption.map _.Trim() with
        | ValueNone
        | ValueSome "" -> CompanyStanding.Unknown ValueNone
        | ValueSome "Active" -> CompanyStanding.Active
        | ValueSome("Suspended - SOS" as held) -> CompanyStanding.Closed held
        | ValueSome other -> CompanyStanding.Unknown(ValueSome other)

    let private address (kind: AddressKind) (prefix: string) (lines: string list) (e: JsonElement) =
        let held =
            lines
            |> List.choose (fun field -> SourceAnswer.trimmed (prefix + field) e |> ValueOption.toOption)

        if List.isEmpty held then
            ValueNone
        else
            ValueSome
                {
                    Kind = kind
                    Address =
                        {
                            Lines = held
                            Locality = SourceAnswer.trimmed (prefix + "City") e
                            Region = SourceAnswer.trimmed (prefix + "State") e
                            PostalCode = SourceAnswer.trimmed (prefix + "ZipCode") e
                            Country = ValueNone
                        }
                }

    let company (e: JsonElement) : Company =
        let status = SourceAnswer.trimmed "StatusDescription" e
        let home = SourceAnswer.trimmed "Jurisdiction" e

        let addresses =
            [
                address AddressKind.PrincipalOffice "Entity" [ "StreetAddress1"; "StreetAddress2" ] e
                address AddressKind.Postal "Mailing" [ "StreetAddress1"; "StreetAddress2" ] e
                address AddressKind.RegisteredAgent "Agent" [ "Address1"; "Address2" ] e
            ]
            |> List.choose ValueOption.toOption

        let foreign =
            home
            |> ValueOption.exists (fun place ->
                not (String.Equals(place, "CALIFORNIA", StringComparison.OrdinalIgnoreCase)))

        {
            Name = required "EntityName" tryString e
            OtherNames =
                [ SourceAnswer.trimmed "ForeignEntityName" e ]
                |> List.choose ValueOption.toOption
            Number = required "EntityID" tryText e
            Jurisdiction = Jurisdiction.ofCode "US-CA"
            JurisdictionAsPublished = home
            Standing = standing status
            Concerns = CompanyConcerns.notPublished
            StatusAsPublished = status
            LegalForm = SourceAnswer.trimmed "EntityType" e
            IncorporatedOn = SourceAnswer.dateOfTimestamp "FilingDate" e
            CeasedOn = ValueNone
            Addresses =
                match addresses with
                | [] -> NotAvailable "The record holds no address"
                | _ -> Available addresses
            Role =
                match home with
                | ValueNone -> RecordRole.NotStated
                | ValueSome _ when foreign -> RecordRole.ForeignRegistration
                | ValueSome _ -> RecordRole.Home
            HomeRegistration =
                if foreign then
                    ValueSome
                        {
                            LegalName = SourceAnswer.trimmed "ForeignEntityName" e
                            LegalForm = ValueNone
                            GoverningLaw = ValueNone
                            Country = home
                            Register = ValueNone
                            RegistrationNumber = ValueNone
                        }
                else
                    ValueNone
            EstablishmentOf = ValueNone
        }

    /// The record for this number. Another entity's record fails the read.
    let lookup (number: string) (root: JsonElement) =
        if root.ValueKind <> JsonValueKind.Object then
            raise (InvalidJson "The answer isn't an entity")

        let found = company root

        if String.Equals(found.Number, number, StringComparison.OrdinalIgnoreCase) then
            found
        else
            raise (InvalidJson "The answer holds another entity")

    let search (root: JsonElement) =
        let matches =
            array root "EntityData"
            |> List.map (fun e ->
                let found = company e

                {
                    Name = found.Name
                    Number = found.Number
                    StatusAsPublished = found.StatusAsPublished
                    Address =
                        match found.Addresses with
                        | Available(first :: _) -> ValueSome first.Address
                        | _ -> ValueNone
                })

        let total =
            match tryInt "RecordCount" root with
            | ValueSome count -> ValueSome count
            | ValueNone when (tryProp "EntityData" root).IsNone ->
                raise (InvalidJson "The answer has no EntityData list")
            | ValueNone -> ValueNone

        {
            Matches = matches
            TotalMatches = total
            Complete = total = ValueSome matches.Length && matches.Length < SearchCap
        }

/// The California BE Public Search API as an ICompanyRegistry. The guide says a 400 answers both a missing parameter and no entity found; the
/// number is checked before sending, so a 400 to a lookup is NotAvailable, never "not held". It says a bad key answers 503, which reads as
/// Unavailable.
[<Sealed>]
type internal CaliforniaRegistry(connection: SourceConnection, key: SourceKey) =
    let client =
        SourceClient(connection, SourceAnswer.pacing 2 (TimeSpan.FromSeconds 1.))

    let coverage = California.coverage
    let baseUri = "https://calico.sos.ca.gov/cbc/v1/api"
    let headers = [ "Ocp-Apim-Subscription-Key", key.Value ]

    interface ICompanyRegistry with
        member _.Coverage = coverage

        member _.TryFindAsync(number: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                match California.normalNumber number with
                | ValueNone ->
                    Task.FromResult(
                        Error(
                            RegistryFailure.InvalidRequest
                                "A California entity number is six to twelve digits, with an optional letter in front"
                        )
                    )
                | ValueSome entityNumber ->
                    task {
                        let uri =
                            Uri $"{baseUri}/BusinessEntityDetails?entity-number={Uri.EscapeDataString entityNumber}"

                        match! client.GetAsync(uri, headers, ct) with
                        | Error(RegistryFailure.RequestRefused "HTTP 400") ->
                            return
                                Ok(
                                    SourceAnswer.unasked
                                        coverage
                                        client.Now
                                        (NotAvailable
                                            "The API answered 400, which its guide gives both for no entity found and for a bad request")
                                )
                        | Error failure -> return Error failure
                        | Ok reply when reply.Status = 404 -> return Error(RegistryFailure.RequestRefused "HTTP 404")
                        | Ok reply ->
                            return
                                SourceAnswer.json reply (California.lookup entityNumber)
                                |> Result.map (fun company ->
                                    SourceAnswer.evidence coverage reply ValueNone (Available(ValueSome company)))
                    })

        member _.SearchAsync(name: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                if String.IsNullOrWhiteSpace name then
                    Task.FromResult(Error(RegistryFailure.InvalidRequest "A search needs a term"))
                else
                    task {
                        let uri =
                            Uri
                                $"{baseUri}/BusinessEntityKeywordSearch?search-term={Uri.EscapeDataString(name.Trim())}"

                        match! client.GetAsync(uri, headers, ct) with
                        | Error failure -> return Error failure
                        | Ok reply when reply.Status = 404 -> return Error(RegistryFailure.RequestRefused "HTTP 404")
                        | Ok reply ->
                            return
                                SourceAnswer.json reply California.search
                                |> Result.map (fun found ->
                                    SourceAnswer.evidence coverage reply ValueNone (Available found))
                    })

        member _.OfficersAsync(_: string, jurisdiction: Jurisdiction, _: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                Task.FromResult(
                    Ok(
                        SourceAnswer.unasked
                            coverage
                            client.Now
                            (NotAvailable "The API lists the registered agent, not officers")
                    )
                ))

        member _.OwnersAsync(_: string, jurisdiction: Jurisdiction, _: OwnerPurpose, _: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                Task.FromResult(Ok(SourceAnswer.unasked coverage client.Now (NotAvailable "The API lists no owners"))))
