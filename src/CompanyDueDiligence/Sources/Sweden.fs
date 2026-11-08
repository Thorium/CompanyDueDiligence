namespace CompanyDueDiligence

open System
open System.Globalization
open System.Net.Http
open System.Text
open System.Text.Json
open System.Text.RegularExpressions
open System.Threading
open System.Threading.Tasks
open CompanyDueDiligence.Json

/// Bolagsverket's Värdefulla datamängder API v1, written from its OpenAPI document, code lists and connection guide (version 1.01) on
/// portal.api.bolagsverket.se, as read on 2026-11-08. Not run live: the API needs client credentials.
module internal Sweden =

    [<Literal>]
    let noOfficers = "The API lists no officers"

    let coverage: RegistryCoverage =
        {
            Registry = "Bolagsverket Värdefulla datamängder (Sweden)"
            Jurisdictions = [ Jurisdiction.ofCode "SE" ]
            Standing =
                Support.Partial
                    "Deregistration and its reason, and liquidation or restructuring in progress; otherwise SCB's flag for whether the organisation is active"
            Concerns =
                Support.Partial
                    "Bankruptcy, reconstruction, composition and resolution are stated while in progress, or as a deregistration reason"
            Addresses = Support.Partial "The postal address only"
            Search = Support.NotOffered "The API looks up by organisation number only"
            Officers = Support.NotOffered noOfficers
            Owners = Support.NotOffered "Beneficial owners are in Bolagsverket's separate VerkligaHuvudmän API"
            Cost =
                AccessCost.FreeWithKey
                    "Apply to Bolagsverket for access to the API; the client ID and secret are sent by email, and the password by SMS"
            RateLimit =
                RateLimitPolicy.Published(
                    60,
                    TimeSpan.FromMinutes 1.,
                    "Bolagsverket: 60 requests a minute per user; not re-read, as bolagsverket.se answers automated reads with a captcha"
                )
            Licence =
                "None stated in the OpenAPI document or the code lists, and bolagsverket.se couldn't be read past its captcha. Not run live"
        }

    let private numberRegex = Regex @"\A[0-9]{10}\z"

    /// Luhn's check, which an organisation number's last digit is.
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

    /// Ten digits with a valid check digit, written with or without the hyphen. Personal identity numbers aren't taken: their third digit, the
    /// tens of a month, is 0 or 1, where an organisation number's is 2 or more.
    let normalNumber (number: string) =
        let normal =
            if isNull number then
                ""
            else
                number.Trim().Replace("-", "").Replace(" ", "")

        if numberRegex.IsMatch normal && normal[2] >= '2' && luhn normal then
            ValueSome normal
        else
            ValueNone

    let private earliestMilliseconds = DateTimeOffset.MinValue.ToUnixTimeMilliseconds()
    let private latestMilliseconds = DateTimeOffset.MaxValue.ToUnixTimeMilliseconds()

    /// A date the API sends as epoch milliseconds, as a number or a string, or as "yyyy-MM-dd". Milliseconds outside DateTimeOffset's range are
    /// no date.
    let dateOf name (e: JsonElement) =
        let ofMilliseconds (milliseconds: int64) =
            if milliseconds < earliestMilliseconds || milliseconds > latestMilliseconds then
                ValueNone
            else
                ValueSome(DateOnly.FromDateTime(DateTimeOffset.FromUnixTimeMilliseconds(milliseconds).UtcDateTime))

        match tryProp name e with
        | ValueSome value when value.ValueKind = JsonValueKind.Number ->
            match value.TryGetInt64() with
            | true, milliseconds -> ofMilliseconds milliseconds
            | _ -> ValueNone
        | ValueSome value when value.ValueKind = JsonValueKind.String ->
            let text = value.GetString().Trim()

            match Int64.TryParse(text, NumberStyles.AllowLeadingSign, CultureInfo.InvariantCulture) with
            | true, milliseconds -> ofMilliseconds milliseconds
            | _ ->
                match DateOnly.TryParseExact(text, "yyyy-MM-dd", CultureInfo.InvariantCulture, DateTimeStyles.None) with
                | true, date -> ValueSome date
                | _ -> ValueNone
        | _ -> ValueNone

    /// The Avregistreringsorsak code list, all 17 codes: completed liquidation and bankruptcy dissolve the company, and the rest close it.
    let deregistered (code: string) (text: string voption) =
        let word =
            text
            |> ValueOption.filter (fun held -> held <> "n/a")
            |> ValueOption.defaultValue code

        match code with
        | "LIAV"
        | "KKAV" -> CompanyStanding.Dissolved
        | "AKEJH"
        | "ARSEED"
        | "AVREG"
        | "BABAKEJH"
        | "DELAV"
        | "DOM"
        | "FUAV"
        | "GROMAV"
        | "NYINN"
        | "OMAV"
        | "OMBAB"
        | "OVERK"
        | "UTLKKLI"
        | "VERKUPP"
        | "VDSAK" -> CompanyStanding.Closed word
        | other -> CompanyStanding.Unknown(ValueSome other)

    /// The Pågående avvecklings- eller omstruktureringsförfarande code list, all 11 codes.
    let private proceeding (code: string) (text: string) =
        match code with
        | "KK"
        | "FR"
        | "AC"
        | "RES" -> ValueSome(0, CompanyStanding.InInsolvency text)
        | "LI"
        | "FUOL"
        | "DEOL" -> ValueSome(1, CompanyStanding.WindingUp text)
        | "FUOT"
        | "DEOT"
        | "OM"
        | "GROM" -> ValueSome(2, CompanyStanding.ActiveWithConcern text)
        | _ -> ValueNone

    /// Deregistration decides, then the gravest proceeding in progress, then SCB's active flag. A proceeding code not on the list is Unknown.
    let standing
        (deregistration: (string * string voption) voption)
        (proceedings: (string * string) list)
        (active: string voption)
        =
        match deregistration with
        | ValueSome(code, text) -> deregistered code text
        | ValueNone ->
            match proceedings |> List.tryFind (fun (code, _) -> (proceeding code "").IsNone) with
            | Some(code, _) -> CompanyStanding.Unknown(ValueSome code)
            | None ->
                match
                    proceedings
                    |> List.choose (fun (code, text) -> proceeding code text |> ValueOption.toOption)
                    |> List.sortBy fst
                with
                | (_, grave) :: _ -> grave
                | [] ->
                    match active with
                    | ValueSome "JA" -> CompanyStanding.Active
                    | ValueSome "NEJ" -> CompanyStanding.Registered(ValueSome "Not active, per SCB")
                    | _ -> CompanyStanding.Registered ValueNone

    /// Bankruptcy, reconstruction, composition and resolution, in progress or as the reason for deregistration.
    let private insolvent (deregistration: (string * string voption) voption) (proceedings: (string * string) list) =
        (deregistration |> ValueOption.exists (fun (code, _) -> code = "KKAV"))
        || proceedings
           |> List.exists (fun (code, _) -> List.contains code [ "KK"; "FR"; "AC"; "RES" ])

    /// What a block of the answer says of itself: its producer couldn't be reached, doesn't hold the organisation, or another fault.
    [<RequireQualifiedAccess>]
    type Fault =
        | NotHeld
        | Unreachable
        | Other

    /// The fault a block states in its fel, if any.
    let private faultOf (block: JsonElement voption) =
        match block |> ValueOption.bind (tryObject "fel") with
        | ValueNone -> ValueNone
        | ValueSome fel ->
            match tryString "typ" fel with
            | ValueSome "ORGANISATION_FINNS_EJ" -> ValueSome Fault.NotHeld
            | ValueSome("OTILLGANGLIG_UPPGIFTSKALLA" | "TIMEOUT") -> ValueSome Fault.Unreachable
            | _ -> ValueSome Fault.Other

    /// The blocks that decide standing and concerns. One with a fault leaves the standing Unknown unless a deregistration read elsewhere decides.
    let private decidingBlocks =
        [
            "avregistreradOrganisation"
            "avregistreringsorsak"
            "pagaendeAvvecklingsEllerOmstruktureringsforfarande"
            "verksamOrganisation"
        ]

    /// The current company name, FORETAGSNAMN, or a NAMN, or else the first listed.
    let private names (e: JsonElement) =
        let listed =
            tryObject "organisationsnamn" e
            |> ValueOption.map (fun block -> array block "organisationsnamnLista")
            |> ValueOption.defaultValue []
            |> List.choose (fun item ->
                SourceAnswer.trimmed "namn" item
                |> ValueOption.map (fun name ->
                    name, tryObject "organisationsnamntyp" item |> ValueOption.bind (tryString "kod"))
                |> ValueOption.toOption)

        let main =
            listed
            |> List.tryFind (fun (_, kind) -> kind = ValueSome "FORETAGSNAMN")
            |> Option.orElse (listed |> List.tryFind (fun (_, kind) -> kind = ValueSome "NAMN"))
            |> Option.orElse (List.tryHead listed)
            |> Option.map fst

        main, listed |> List.map fst

    let private codeText name (e: JsonElement) =
        tryObject name e
        |> ValueOption.bind (fun block ->
            SourceAnswer.trimmed "kod" block
            |> ValueOption.map (fun code -> code, SourceAnswer.trimmed "klartext" block))

    let company (e: JsonElement) : Company =
        let main, listed = names e

        let name =
            match main with
            | Some name -> name
            | None -> raise (InvalidJson "The answer has no organisation name")

        let number =
            tryObject "organisationsidentitet" e
            |> ValueOption.bind (SourceAnswer.trimmed "identitetsbeteckning")
            |> ValueOption.defaultWith (fun () -> raise (InvalidJson "The answer has no organisation number"))

        let removedOn =
            tryObject "avregistreradOrganisation" e
            |> ValueOption.bind (fun block ->
                match tryProp "avregistreringsdatum" block, dateOf "avregistreringsdatum" block with
                | ValueSome _, ValueNone -> raise (InvalidJson "The deregistration date can't be read")
                | _, date -> date)

        let unsettled =
            decidingBlocks
            |> List.filter (fun block -> (faultOf (tryObject block e)).IsSome)

        let deregistration =
            match codeText "avregistreringsorsak" e, removedOn with
            | ValueSome(code, text), _ -> ValueSome(code, text)
            | ValueNone, ValueSome _ -> ValueSome("AVREG", ValueSome "Avregistrerad")
            | ValueNone, ValueNone -> ValueNone

        let proceedings =
            tryObject "pagaendeAvvecklingsEllerOmstruktureringsforfarande" e
            |> ValueOption.map (fun block -> array block "pagaendeAvvecklingsEllerOmstruktureringsforfarandeLista")
            |> ValueOption.defaultValue []
            |> List.choose (fun item ->
                SourceAnswer.trimmed "kod" item
                |> ValueOption.map (fun code ->
                    code, SourceAnswer.trimmed "klartext" item |> ValueOption.defaultValue code)
                |> ValueOption.toOption)

        let active =
            tryObject "verksamOrganisation" e
            |> ValueOption.bind (SourceAnswer.trimmed "kod")

        let form = codeText "organisationsform" e

        let address =
            tryObject "postadressOrganisation" e
            |> ValueOption.bind (tryObject "postadress")
            |> ValueOption.map (fun held ->
                {
                    Kind = AddressKind.Postal
                    Address =
                        {
                            Lines =
                                [
                                    SourceAnswer.trimmed "coAdress" held
                                    SourceAnswer.trimmed "utdelningsadress" held
                                ]
                                |> List.choose ValueOption.toOption
                            Locality = SourceAnswer.trimmed "postort" held
                            Region = ValueNone
                            PostalCode = SourceAnswer.trimmed "postnummer" held
                            Country = SourceAnswer.trimmed "land" held
                        }
                })

        {
            Name = name
            OtherNames = listed |> List.filter ((<>) name) |> List.distinct
            Number = number
            Jurisdiction = Jurisdiction.ofCode "SE"
            JurisdictionAsPublished = codeText "registreringsland" e |> ValueOption.bind snd
            Standing =
                match deregistration, unsettled with
                | ValueNone, _ :: _ ->
                    CompanyStanding.Unknown(
                        ValueSome $"""Not settled: {String.concat ", " unsettled} answered with a fault"""
                    )
                | _ -> standing deregistration proceedings active
            Concerns =
                if insolvent deregistration proceedings then
                    {
                        Stated = [ CompanyConcern.InsolvencyHistory ]
                        Unconfirmed = CompanyConcern.all |> List.except [ CompanyConcern.InsolvencyHistory ]
                    }
                else
                    CompanyConcerns.notPublished
            StatusAsPublished =
                match deregistration, proceedings, active with
                | ValueSome(code, text), _, _ ->
                    ValueSome
                        $"""avregistreringsorsak {code}{text |> ValueOption.map (fun held -> " " + held) |> ValueOption.defaultValue ""}"""
                | ValueNone, (_ :: _ as running), _ ->
                    ValueSome(running |> List.map (fun (code, text) -> $"{code} {text}") |> String.concat "; ")
                | ValueNone, [], ValueSome flag -> ValueSome $"verksam {flag}"
                | ValueNone, [], ValueNone -> ValueNone
            LegalForm =
                form
                |> ValueOption.map (fun (code, text) -> text |> ValueOption.defaultValue code)
            IncorporatedOn =
                tryObject "organisationsdatum" e
                |> ValueOption.bind (dateOf "registreringsdatum")
            CeasedOn = removedOn
            Addresses =
                match address with
                | ValueSome held -> Available [ held ]
                | ValueNone -> NotAvailable "The answer holds no postal address"
            Role =
                match form with
                | ValueSome(("FL" | "BFL"), _) -> RecordRole.ForeignRegistration
                | ValueSome _ -> RecordRole.Home
                | ValueNone -> RecordRole.NotStated
            HomeRegistration = ValueNone
            EstablishmentOf = ValueNone
        }

    /// The one organisation in the answer. Bolagsverket's own blocks decide whether it holds the number: ORGANISATION_FINNS_EJ says only that
    /// this producer doesn't hold it in searchable form, so it is NotAvailable. A producer out of reach, for the name or a block deciding
    /// standing, fails the call.
    let lookup (root: JsonElement) =
        let organisations =
            match tryProp "organisationer" root with
            | ValueSome list when list.ValueKind = JsonValueKind.Array -> list.EnumerateArray() |> List.ofSeq
            | _ -> raise (InvalidJson "The answer has no organisationer list")

        match organisations with
        | [] -> Ok(NotAvailable "The answer lists no organisation for this number, which proves nothing")
        | [ only ] ->
            match faultOf (tryObject "organisationsnamn" only) with
            | ValueSome Fault.NotHeld ->
                Ok(
                    NotAvailable
                        "Bolagsverket doesn't hold this number in searchable form, which, as it says, doesn't prove the organisation doesn't exist"
                )
            | ValueSome Fault.Unreachable ->
                Error(RegistryFailure.Unavailable "Bolagsverket couldn't fetch the organisation's data")
            | ValueSome Fault.Other
            | ValueNone ->
                if
                    decidingBlocks
                    |> List.exists (fun block -> faultOf (tryObject block only) = ValueSome Fault.Unreachable)
                then
                    Error(
                        RegistryFailure.Unavailable
                            "A producer of the data deciding the organisation's standing couldn't be reached"
                    )
                else
                    Ok(Available(ValueSome(company only, only.GetRawText())))
        | several ->
            Ok(
                NotAvailable
                    $"{several.Length} organisations share this number, such as sole traders' businesses, so the API can't tell which is meant"
            )

/// Bolagsverket's Värdefulla datamängder as an ICompanyRegistry, over a client-credentials token that the registry holds.
[<Sealed>]
type internal SwedenRegistry(connection: SourceConnection, credentials: ClientCredentials) =
    let client =
        SourceClient(connection, SourceAnswer.pacing 60 (TimeSpan.FromMinutes 1.))

    let coverage = Sweden.coverage

    let token =
        new ClientCredentialsToken(
            client,
            Uri "https://portal.api.bolagsverket.se/oauth2/token",
            credentials,
            ValueSome "vardefulla-datamangder:read"
        )
        :> IBearerToken

    let organisationsUri =
        Uri "https://gw.api.bolagsverket.se/vardefulla-datamangder/v1/organisationer"

    interface ICompanyRegistry with
        member _.Coverage = coverage

        member _.TryFindAsync(number: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                match Sweden.normalNumber number with
                | ValueNone ->
                    Task.FromResult(
                        Error(
                            RegistryFailure.InvalidRequest
                                "An organisation number has ten digits with a valid check digit"
                        )
                    )
                | ValueSome orgnr ->
                    task {
                        let body () : HttpContent =
                            new StringContent(
                                JsonSerializer.Serialize {| identitetsbeteckning = orgnr |},
                                Encoding.UTF8,
                                "application/json"
                            )

                        let send authorization =
                            client.PostAsync(organisationsUri, body, [ authorization ], ct)

                        match! Bearer.sendAsync token send ct with
                        | Error failure -> return Error failure
                        | Ok reply when reply.Status = 404 -> return Error(RegistryFailure.RequestRefused "HTTP 404")
                        | Ok reply ->
                            match SourceAnswer.json reply Sweden.lookup with
                            | Error failure
                            | Ok(Error failure) -> return Error failure
                            | Ok(Ok(Available(ValueSome(company, raw)))) ->
                                if company.Number = orgnr then
                                    return
                                        Ok
                                            { SourceAnswer.evidence
                                                  coverage
                                                  reply
                                                  ValueNone
                                                  (Available(ValueSome company)) with
                                                Raw = ValueSome raw
                                            }
                                else
                                    return
                                        Error(RegistryFailure.UnreadableAnswer "The answer holds another organisation")
                            | Ok(Ok(Available ValueNone)) ->
                                return Ok(SourceAnswer.evidence coverage reply ValueNone (Available ValueNone))
                            | Ok(Ok(NotAvailable reason)) ->
                                return Ok(SourceAnswer.evidence coverage reply ValueNone (NotAvailable reason))
                            | Ok(Ok(NeedsEntitlement entitlement)) ->
                                return
                                    Ok(SourceAnswer.evidence coverage reply ValueNone (NeedsEntitlement entitlement))
                    })

        member _.SearchAsync(_: string, jurisdiction: Jurisdiction, _: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                Task.FromResult(
                    Ok(SourceAnswer.unasked coverage client.Now (NotAvailable "The API has no search by name"))
                ))

        member _.OfficersAsync(_: string, jurisdiction: Jurisdiction, _: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                Task.FromResult(Ok(SourceAnswer.unasked coverage client.Now (NotAvailable Sweden.noOfficers))))

        member _.OwnersAsync(_: string, jurisdiction: Jurisdiction, purpose: OwnerPurpose, _: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                Task.FromResult(
                    Ok(
                        SourceAnswer.unasked
                            coverage
                            client.Now
                            (SourceAnswer.restrictedOwners
                                purpose
                                "Unverified: those Bolagsverket grants access to its register of beneficial owners"
                                "Apply to Bolagsverket for its VerkligaHuvudmän API, or use a vendor")
                    )
                ))
