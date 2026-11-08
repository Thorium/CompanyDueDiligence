namespace CompanyDueDiligence

open System
open System.Text.Json
open System.Text.RegularExpressions
open System.Threading
open System.Threading.Tasks
open CompanyDueDiligence.Json

/// Corporations Canada's Federal Corporation API, written from its OpenAPI document (corporations-en.json, version 2.0.0) as read on 2026-11-08.
/// Status words are those of the Federal Corporations open data files of 2026-11-07. Not run live: the API needs a key.
module internal Canada =

    [<Literal>]
    let noOwners =
        "The API lists no individuals with significant control; Corporations Canada publishes those through its own search"

    let coverage: RegistryCoverage =
        {
            Registry = "Corporations Canada (federal)"
            Jurisdictions = [ Jurisdiction.ofCode "CA" ]
            Standing = Support.Supported
            Concerns = Support.NotOffered "The API states no overdue filings, disputes or insolvency"
            Addresses = Support.Partial "The registered office address only"
            Search = Support.NotOffered "The API looks up by corporation or business number only"
            Officers =
                Support.Partial
                    "Directors' names and service addresses, by corporation number only, with no appointment dates; whether former directors are listed is unverified"
            Owners = Support.NotOffered noOwners
            Cost =
                AccessCost.FreeWithKey
                    "Log in to the ISED API Store (api.ised-isde.canada.ca) and subscribe to the Public Plan; the key goes in the user-key header"
            RateLimit =
                RateLimitPolicy.Published(
                    60,
                    TimeSpan.FromMinutes 1.,
                    "ISED API Store, Public Plan: 60 hits per minute"
                )
            Licence =
                "Open Government Licence - Canada, as open.canada.ca states for the Federal Corporations dataset; the API's own page states none. Not run live"
        }

    /// A number as the API takes it.
    [<RequireQualifiedAccess>]
    type Number =
        /// Up to eight digits, leading zeros dropped, as the API's integer path parameter.
        | Corporation of string
        /// The nine-digit root of a Business Number.
        | BusinessNumber of string

    let private corporationRegex = Regex @"\A[0-9]{1,8}\z"
    let private businessNumberRegex = Regex @"\A([0-9]{9})(RC[0-9]{4})?\z"

    /// Luhn's check, which a Business Number's ninth digit is.
    let luhn (digits: string) =
        let total =
            digits
            |> Seq.rev
            |> Seq.mapi (fun index digit ->
                let value = int digit - int '0'

                if index % 2 = 1 then
                    let doubled = value * 2
                    if doubled > 9 then doubled - 9 else doubled
                else
                    value)
            |> Seq.sum

        total % 10 = 0

    /// A corporation number such as 000100-7 or 1007, or a Business Number such as 106679285 or 106679285RC0001 with a valid check digit.
    /// Nine digits are always read as a Business Number.
    let numberOf (number: string) =
        let normal =
            if isNull number then
                ""
            else
                number.Trim().ToUpperInvariant().Replace("-", "").Replace(" ", "")

        let business = businessNumberRegex.Match normal

        if business.Success then
            if luhn business.Groups[1].Value then
                ValueSome(Number.BusinessNumber business.Groups[1].Value)
            else
                ValueNone
        elif corporationRegex.IsMatch normal then
            match normal.TrimStart '0' with
            | "" -> ValueNone
            | digits -> ValueSome(Number.Corporation digits)
        else
            ValueNone

    /// The words in the Federal Corporations open data files of 2026-11-07, Status and Status Detail alike. The API's status field is documented
    /// by one example, Active, so which of them it gives is unverified. Any other word is Unknown.
    let standing (word: string voption) =
        match word |> ValueOption.map (fun text -> text.Trim().Replace('–', '-')) with
        | ValueNone
        | ValueSome "" -> CompanyStanding.Unknown ValueNone
        | ValueSome text ->
            match text.ToUpperInvariant() with
            | "ACTIVE" -> CompanyStanding.Active
            | "ACTIVE - DISCONTINUANCE PENDING" -> CompanyStanding.ActiveWithConcern "Discontinuance pending"
            | "ACTIVE - DISSOLUTION PENDING (NON-COMPLIANCE)" ->
                CompanyStanding.ActiveWithConcern "Dissolution pending (non-compliance)"
            | "ACTIVE - INTENT TO DISSOLVE FILED" -> CompanyStanding.WindingUp "Intent to dissolve filed"
            | "DISSOLVED"
            | "DISSOLVED BY CORPORATIONS CANADA"
            | "DISSOLVED BY COURT ORDER"
            | "DISSOLVED BY THE CORPORATION"
            | "DISSOLVED FOR NON-COMPLIANCE"
            | "DISSOLVED - PART II OF CCA" -> CompanyStanding.Dissolved
            | "INACTIVE"
            | "INACTIVE - AMALGAMATED"
            | "INACTIVE - DISCONTINUED" -> CompanyStanding.Closed text
            | _ -> CompanyStanding.Unknown(ValueSome text)

    let private names (e: JsonElement) =
        array e "corporationNames"
        |> List.choose (fun item -> tryObject "CorporationName" item |> ValueOption.toOption)
        |> List.choose (fun name ->
            SourceAnswer.trimmed "name" name
            |> ValueOption.map (fun text -> text, tryFlag "current" name = ValueSome true)
            |> ValueOption.toOption)

    let private address (e: JsonElement) : CompanyAddress voption =
        tryObject "address" e
        |> ValueOption.map (fun held ->
            {
                Kind = AddressKind.RegisteredOffice
                Address =
                    {
                        Lines = strings held "addressLine" |> List.map _.Trim() |> List.filter ((<>) "")
                        Locality = SourceAnswer.trimmed "city" held
                        Region = SourceAnswer.trimmed "provinceCode" held
                        PostalCode = SourceAnswer.trimmed "postalCode" held
                        Country = SourceAnswer.trimmed "countryCode" held
                    }
            })

    let private incorporatedOn (e: JsonElement) =
        array e "activities"
        |> List.choose (fun item -> tryObject "activity" item |> ValueOption.toOption)
        |> List.tryFind (fun activity -> SourceAnswer.trimmed "activity" activity = ValueSome "Incorporation")
        |> ValueOption.ofOption
        |> ValueOption.bind (tryDate "date")

    let company (e: JsonElement) : Company =
        let held = names e

        let name =
            match held |> List.tryFind snd with
            | Some(name, _) -> name
            | None -> raise (InvalidJson "The answer has no current corporation name")

        let status = SourceAnswer.trimmed "status" e
        let act = SourceAnswer.trimmed "act" e
        let addresses = array e "adresses" |> List.choose (address >> ValueOption.toOption)

        {
            Name = name
            OtherNames = held |> List.map fst |> List.filter ((<>) name) |> List.distinct
            Number = required "corporationId" tryText e |> _.Trim()
            Jurisdiction = Jurisdiction.ofCode "CA"
            JurisdictionAsPublished = act
            Standing = standing status
            Concerns = CompanyConcerns.notPublished
            StatusAsPublished = status
            LegalForm = act
            IncorporatedOn = incorporatedOn e
            CeasedOn = ValueNone
            Addresses =
                match addresses with
                | [] -> NotAvailable "The answer holds no address"
                | _ -> Available addresses
            Role = if act.IsSome then RecordRole.Home else RecordRole.NotStated
            HomeRegistration = ValueNone
            EstablishmentOf = ValueNone
        }

    let private businessNumberOf (e: JsonElement) =
        tryObject "businessNumbers" e
        |> ValueOption.bind (SourceAnswer.trimmed "businessNumber")

    /// The corporation in an answer asked with lang=eng: a list holding the English record and a null, or the API's "could not find corporation"
    /// strings, which say the number isn't held. A record for another number fails the read, as does one asked by Business Number that holds
    /// none.
    let lookup (asked: Number) (root: JsonElement) =
        if root.ValueKind <> JsonValueKind.Array then
            raise (InvalidJson "The answer isn't a list")

        let items = root.EnumerateArray() |> List.ofSeq

        let records =
            items |> List.filter (fun item -> item.ValueKind = JsonValueKind.Object)

        let notFound =
            items
            |> List.exists (fun item ->
                item.ValueKind = JsonValueKind.String
                && item.GetString().StartsWith("could not find corporation", StringComparison.OrdinalIgnoreCase))

        match records with
        | [] when notFound -> Available ValueNone
        | [] -> raise (InvalidJson "The answer holds no corporation and no not-found message")
        | [ only ] ->
            let found = company only

            let matches =
                match asked with
                | Number.Corporation digits -> found.Number.TrimStart '0' = digits
                | Number.BusinessNumber digits ->
                    match businessNumberOf only with
                    | ValueSome held -> held.StartsWith(digits, StringComparison.Ordinal)
                    | ValueNone -> raise (InvalidJson "The record holds no business number to match the one asked for")

            if matches then
                Available(ValueSome(found, only.GetRawText()))
            else
                raise (InvalidJson "The answer holds another corporation")
        | several ->
            match several |> List.map (tryText "corporationId") |> List.distinct with
            | [ _ ] -> raise (InvalidJson "The answer holds the corporation more than once")
            | ids -> NotAvailable $"{ids.Length} corporations carry this number, so the API can't tell which is meant"

    let private officer (e: JsonElement) : Officer =
        let name =
            [ "firstName"; "middleName"; "lastName" ]
            |> List.choose (fun field -> SourceAnswer.trimmed field e |> ValueOption.toOption)
            |> String.concat " "

        let address =
            tryObject "serviceAddress" e
            |> ValueOption.map (fun held ->
                {
                    Lines =
                        [ SourceAnswer.trimmed "line1" held; SourceAnswer.trimmed "line2" held ]
                        |> List.choose ValueOption.toOption
                    Locality = SourceAnswer.trimmed "city" held
                    Region = SourceAnswer.trimmed "subdivisionCode" held
                    PostalCode = SourceAnswer.trimmed "postalCode" held
                    Country =
                        SourceAnswer.trimmed "countryName" held
                        |> ValueOption.orElse (SourceAnswer.trimmed "countryCode" held)
                })

        {
            Name = if name = "" then PartyNames.NotGiven else name
            Kind =
                if name = "" then
                    PartyKind.Unknown ValueNone
                else
                    PartyKind.Individual
            Role = OfficerRole.Director
            Current = true
            AppointedOn = ValueNone
            EndedOn = ValueNone
            BornIn = ValueNone
            Nationality = ValueNone
            CountryOfResidence = ValueNone
            Address = address
            Entity = ValueNone
            IdentityVerification = ValueNone
        }

    /// The v2 directors resource, in HAL form. An answer with links and no embedded list lists no directors.
    let directors (root: JsonElement) =
        match tryObject "_embedded" root, tryObject "_links" root with
        | ValueSome embedded, _ ->
            match array embedded "directors" with
            | [] -> NotAvailable "The API lists no directors for this corporation, which doesn't say it has none"
            | items -> Available(items |> List.map officer)
        | ValueNone, ValueSome _ ->
            NotAvailable "The API lists no directors for this corporation, which doesn't say it has none"
        | ValueNone, ValueNone -> raise (InvalidJson "The answer is not a directors resource")

/// Corporations Canada as an ICompanyRegistry. It answers for CA itself only: a province code is OutsideCoverage, though Jurisdiction.covers
/// would let CA stand for it, so a host lists provincial registers by their own codes.
[<Sealed>]
type internal CanadaRegistry(connection: SourceConnection, key: SourceKey) =
    let client =
        SourceClient(connection, SourceAnswer.pacing 60 (TimeSpan.FromMinutes 1.))

    let coverage = Canada.coverage

    let baseUri =
        "https://apigateway-passerelledapi.ised-isde.canada.ca/corporations/api"

    let inCanada (jurisdiction: Jurisdiction) (call: unit -> Task<Result<Evidence<'T>, RegistryFailure>>) =
        if jurisdiction.Code = "CA" then
            call ()
        else
            Task.FromResult(Error(RegistryFailure.OutsideCoverage jurisdiction))

    let offNumber number send =
        match Canada.numberOf number with
        | ValueSome normal -> send normal
        | ValueNone ->
            Task.FromResult(
                Error(
                    RegistryFailure.InvalidRequest
                        "A federal corporation number has up to eight digits, and a Business Number nine with a valid check digit"
                )
            )

    interface ICompanyRegistry with
        member _.Coverage = coverage

        member _.TryFindAsync(number: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            inCanada jurisdiction (fun () ->
                offNumber number (fun asked ->
                    task {
                        let path =
                            match asked with
                            | Canada.Number.Corporation digits
                            | Canada.Number.BusinessNumber digits -> digits

                        let uri = Uri $"{baseUri}/v1/corporations/{path}.json?lang=eng"

                        match! client.GetAsync(uri, [ "user-key", key.Value ], ct) with
                        | Error failure -> return Error failure
                        | Ok reply when reply.Status = 404 -> return Error(RegistryFailure.RequestRefused "HTTP 404")
                        | Ok reply ->
                            return
                                SourceAnswer.json reply (Canada.lookup asked)
                                |> Result.map (fun found ->
                                    match found with
                                    | Available(ValueSome(company, raw)) ->
                                        { SourceAnswer.evidence
                                              coverage
                                              reply
                                              ValueNone
                                              (Available(ValueSome company)) with
                                            Raw = ValueSome raw
                                        }
                                    | Available ValueNone ->
                                        SourceAnswer.evidence coverage reply ValueNone (Available ValueNone)
                                    | NotAvailable reason ->
                                        SourceAnswer.evidence coverage reply ValueNone (NotAvailable reason)
                                    | NeedsEntitlement entitlement ->
                                        SourceAnswer.evidence coverage reply ValueNone (NeedsEntitlement entitlement))
                    }))

        member _.SearchAsync(_: string, jurisdiction: Jurisdiction, _: CancellationToken) =
            inCanada jurisdiction (fun () ->
                Task.FromResult(
                    Ok(
                        SourceAnswer.unasked
                            coverage
                            client.Now
                            (NotAvailable "The API has no search by name; Corporations Canada's own search page has")
                    )
                ))

        member _.OfficersAsync(number: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            inCanada jurisdiction (fun () ->
                offNumber number (fun asked ->
                    match asked with
                    | Canada.Number.BusinessNumber _ ->
                        Task.FromResult(
                            Error(
                                RegistryFailure.InvalidRequest
                                    "Directors are read by the corporation number, not a Business Number"
                            )
                        )
                    | Canada.Number.Corporation digits ->
                        task {
                            let uri = Uri $"{baseUri}/v2/corporations/{digits}/directors"

                            match! client.GetAsync(uri, [ "user-key", key.Value; "Accept-Language", "en" ], ct) with
                            | Error failure -> return Error failure
                            | Ok reply when reply.Status = 404 ->
                                return
                                    Ok
                                        { SourceAnswer.evidence
                                              coverage
                                              reply
                                              ValueNone
                                              (NotAvailable "The API answered 404 for this corporation's directors") with
                                            Raw = ValueNone
                                        }
                            | Ok reply ->
                                return
                                    SourceAnswer.json reply Canada.directors
                                    |> Result.map (SourceAnswer.evidence coverage reply ValueNone)
                        }))

        member _.OwnersAsync(_: string, jurisdiction: Jurisdiction, _: OwnerPurpose, _: CancellationToken) =
            inCanada jurisdiction (fun () ->
                Task.FromResult(Ok(SourceAnswer.unasked coverage client.Now (NotAvailable Canada.noOwners))))
