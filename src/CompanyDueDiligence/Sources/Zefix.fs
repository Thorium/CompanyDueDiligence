namespace CompanyDueDiligence

open System
open System.Net.Http
open System.Text
open System.Text.Json
open System.Text.RegularExpressions
open System.Threading
open System.Threading.Tasks
open CompanyDueDiligence.Json

/// Zefix PublicREST, written from its OpenAPI document (/ZefixPublicREST/v3/api-docs, version 2.7.2.4) as read on 2026-11-08. Not run live: the API
/// needs credentials, which the Federal Registry of Commerce sends by email.
module internal Zefix =

    let coverage: RegistryCoverage =
        {
            Registry = "Zefix (Switzerland)"
            Jurisdictions = [ Jurisdiction.ofCode "CH" ]
            Standing = Support.Supported
            Concerns = Support.NotOffered "SOGC publications are kept in the raw answer, not read for concerns"
            Addresses = Support.Partial "The legal domicile address"
            Search = Support.Partial "Names starting with the term; a list too long is refused"
            Officers =
                Support.NotOffered "Zefix lists no officers; SOGC publication texts and the cantonal extract name them"
            Owners = Support.NotOffered "Zefix lists no owners"
            Cost =
                AccessCost.FreeWithKey
                    "Ask zefix@bj.admin.ch for PublicREST credentials, a user name and password used with HTTP Basic"
            RateLimit = RateLimitPolicy.NotPublished(2, TimeSpan.FromSeconds 1.)
            Licence = "OGD: open use, source must be cited, as the OpenAPI document's licence states. Not run live"
        }

    let private uidRegex = Regex @"\ACHE([0-9]{8})([0-9])\z"

    /// The UID check digit: the first eight digits weighted 5, 4, 3, 2, 7, 6, 5, 4, and 11 less the sum modulo 11, where 11 is 0 and 10 is no valid
    /// UID.
    let checkDigit (digits: string) =
        let weights = [| 5; 4; 3; 2; 7; 6; 5; 4 |]

        let total =
            digits
            |> Seq.mapi (fun index digit -> (int digit - int '0') * weights[index])
            |> Seq.sum

        match 11 - total % 11 with
        | 11 -> ValueSome 0
        | 10 -> ValueNone
        | check -> ValueSome check

    /// A UID such as CHE-107.721.785, in the API's spelling CHE107721785, with a valid check digit.
    let normalUid (number: string) =
        let normal =
            if isNull number then
                ""
            else
                number.Trim().ToUpperInvariant().Replace("-", "").Replace(".", "").Replace(" ", "")

        let found = uidRegex.Match normal

        if
            found.Success
            && checkDigit found.Groups[1].Value = ValueSome(int found.Groups[2].Value)
        then
            ValueSome normal
        else
            ValueNone

    /// The status enumeration of CompanyFull: active, in liquidation, deleted. Any other word is Unknown.
    let standing (status: string voption) =
        match status with
        | ValueSome "ACTIVE" -> CompanyStanding.Active
        | ValueSome "BEING_CANCELLED" -> CompanyStanding.WindingUp "In liquidation"
        | ValueSome "CANCELLED" -> CompanyStanding.Closed "Deleted from the commercial register"
        | ValueSome ""
        | ValueNone -> CompanyStanding.Unknown ValueNone
        | ValueSome other -> CompanyStanding.Unknown(ValueSome other)

    /// eCH-0097 code 0111 is a branch of a foreign company and 0151 a Swiss branch. Unverified: the eCH table wasn't read, and no answer was seen.
    let private role (form: JsonElement voption) =
        match form |> ValueOption.bind (SourceAnswer.trimmed "uid") with
        | ValueSome "0111" -> RecordRole.ForeignRegistration
        | ValueSome "0151" -> RecordRole.NotStated
        | ValueSome _ -> RecordRole.Home
        | ValueNone -> RecordRole.NotStated

    let private formName (form: JsonElement voption) =
        form
        |> ValueOption.bind (fun held ->
            [ "name"; "shortName" ]
            |> List.tryPick (fun field ->
                tryObject field held
                |> ValueOption.bind (fun names ->
                    SourceAnswer.trimmed "en" names
                    |> ValueOption.orElse (SourceAnswer.trimmed "de" names))
                |> ValueOption.toOption)
            |> ValueOption.ofOption)

    let private address (e: JsonElement) =
        tryObject "address" e
        |> ValueOption.map (fun held ->
            let street =
                [ SourceAnswer.trimmed "street" held; SourceAnswer.trimmed "houseNumber" held ]
                |> List.choose ValueOption.toOption
                |> String.concat " "

            {
                Kind = AddressKind.RegisteredOffice
                Address =
                    {
                        Lines =
                            [
                                SourceAnswer.trimmed "organisation" held
                                SourceAnswer.trimmed "careOf" held
                                (if street = "" then ValueNone else ValueSome street)
                                SourceAnswer.trimmed "addon" held
                                SourceAnswer.trimmed "poBox" held
                            ]
                            |> List.choose ValueOption.toOption
                        Locality = SourceAnswer.trimmed "city" held
                        Region = SourceAnswer.trimmed "canton" e
                        PostalCode = SourceAnswer.trimmed "swissZipCode" held
                        Country = ValueNone
                    }
            })

    let company (e: JsonElement) : Company =
        let form = tryObject "legalForm" e
        let status = SourceAnswer.trimmed "status" e
        let name = required "name" tryString e

        {
            Name = name
            OtherNames =
                strings e "translation"
                |> List.map _.Trim()
                |> List.filter (fun held -> held <> "" && held <> name)
                |> List.distinct
            Number = required "uid" tryString e
            Jurisdiction = Jurisdiction.ofCode "CH"
            JurisdictionAsPublished = ValueNone
            Standing = standing status
            Concerns = CompanyConcerns.notPublished
            StatusAsPublished = status
            LegalForm = formName form
            IncorporatedOn = ValueNone
            CeasedOn = tryDate "deletionDate" e
            Addresses =
                match address e with
                | ValueSome held -> Available [ held ]
                | ValueNone -> NotAvailable "The answer holds no address"
            Role = role form
            HomeRegistration = ValueNone
            EstablishmentOf = ValueNone
        }

    /// The company for this UID: a list of CompanyFull, where an empty list says Zefix doesn't hold it. Several entries aren't told apart.
    let lookup (uid: string) (root: JsonElement) =
        if root.ValueKind <> JsonValueKind.Array then
            raise (InvalidJson "The answer isn't a list")

        match root.EnumerateArray() |> List.ofSeq with
        | [] -> Available ValueNone
        | [ only ] ->
            let found = company only

            if found.Number = uid then
                Available(ValueSome(found, only.GetRawText()))
            else
                raise (InvalidJson "The answer holds another company")
        | several -> NotAvailable $"Zefix holds {several.Length} entries for this UID, so it can't tell which is meant"

    /// A 404 whose error type is NOT_FOUND, which Zefix gives for a UID it doesn't hold.
    let isNotFound (root: JsonElement) =
        tryObject "error" root |> ValueOption.bind (tryString "type") = ValueSome "NOT_FOUND"

    let search (root: JsonElement) =
        if root.ValueKind <> JsonValueKind.Array then
            raise (InvalidJson "The answer isn't a list")

        let matches =
            root.EnumerateArray()
            |> Seq.map (fun e ->
                {
                    Name = required "name" tryString e
                    Number = required "uid" tryString e
                    StatusAsPublished = SourceAnswer.trimmed "status" e
                    Address = ValueNone
                })
            |> List.ofSeq

        {
            Matches = matches
            TotalMatches = ValueNone
            Complete = false
        }

/// Zefix PublicREST as an ICompanyRegistry, with the credentials in an HTTP Basic header.
[<Sealed>]
type internal ZefixRegistry(connection: SourceConnection, credentials: ClientCredentials) =
    let client =
        SourceClient(connection, SourceAnswer.pacing 2 (TimeSpan.FromSeconds 1.))

    let coverage = Zefix.coverage
    let baseUri = "https://www.zefix.admin.ch/ZefixPublicREST/api/v1"

    let authorization =
        "Authorization",
        "Basic "
        + Convert.ToBase64String(Encoding.UTF8.GetBytes($"{credentials.Id}:{credentials.Secret}"))

    interface ICompanyRegistry with
        member _.Coverage = coverage

        member _.TryFindAsync(number: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                match Zefix.normalUid number with
                | ValueNone ->
                    Task.FromResult(
                        Error(
                            RegistryFailure.InvalidRequest
                                "A UID is CHE and nine digits, such as CHE-107.721.785, with a valid check digit"
                        )
                    )
                | ValueSome uid ->
                    task {
                        match! client.GetAsync(Uri $"{baseUri}/company/uid/{uid}", [ authorization ], ct) with
                        | Error failure -> return Error failure
                        | Ok reply when reply.Status = 404 ->
                            match SourceAnswer.json reply Zefix.isNotFound with
                            | Ok true ->
                                return
                                    Ok
                                        { SourceAnswer.evidence coverage reply ValueNone (Available ValueNone) with
                                            Raw = ValueNone
                                        }
                            | _ -> return Error(RegistryFailure.RequestRefused "HTTP 404")
                        | Ok reply ->
                            return
                                SourceAnswer.json reply (Zefix.lookup uid)
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
                    })

        member _.SearchAsync(name: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                if isNull name || name.Trim().Length < 3 then
                    Task.FromResult(
                        Error(RegistryFailure.InvalidRequest "A Zefix search needs at least three characters")
                    )
                else
                    task {
                        let body () : HttpContent =
                            new StringContent(
                                JsonSerializer.Serialize
                                    {|
                                        name = name.Trim()
                                        activeOnly = false
                                    |},
                                Encoding.UTF8,
                                "application/json"
                            )

                        match! client.PostAsync(Uri $"{baseUri}/company/search", body, [ authorization ], ct) with
                        | Error failure -> return Error failure
                        | Ok reply when reply.Status = 404 ->
                            match SourceAnswer.json reply Zefix.isNotFound with
                            | Ok true ->
                                return
                                    Ok(
                                        SourceAnswer.evidence
                                            coverage
                                            reply
                                            ValueNone
                                            (Available
                                                {
                                                    Matches = []
                                                    TotalMatches = ValueSome 0
                                                    Complete = false
                                                })
                                    )
                            | _ -> return Error(RegistryFailure.RequestRefused "HTTP 404")
                        | Ok reply ->
                            return
                                SourceAnswer.json reply Zefix.search
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
                            (NotAvailable
                                "Zefix lists no officers; SOGC publication texts and the cantonal extract name them")
                    )
                ))

        member _.OwnersAsync(_: string, jurisdiction: Jurisdiction, _: OwnerPurpose, _: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                Task.FromResult(Ok(SourceAnswer.unasked coverage client.Now (NotAvailable "Zefix lists no owners"))))
