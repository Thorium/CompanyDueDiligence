namespace CompanyDueDiligence

open System
open System.Net.Http
open System.Text
open System.Text.Json
open System.Text.RegularExpressions
open System.Threading
open System.Threading.Tasks
open CompanyDueDiligence.Json

/// KRA's GavaConnect "PIN Checker by PIN", written from its documentation at developer.go.ke/apis/PIN_Validation_by_PIN on 2026-11-08. Not run
/// live: it needs an app's consumer key and secret. It validates a tax PIN in iTax; it isn't the Business Registration Service's register.
module internal Kenya =

    let coverage: RegistryCoverage =
        {
            Registry = "KRA PIN Checker (Kenya)"
            Jurisdictions = [ Jurisdiction.ofCode "KE" ]
            Standing =
                Support.Partial
                    "The tax PIN's status in iTax, not the Business Registration Service's record of the company"
            Concerns = Support.NotOffered "The checker states no overdue filings, disputes or insolvency"
            Addresses = Support.NotOffered "The checker gives no address"
            Search = Support.NotOffered "The checker validates one PIN"
            Officers = Support.NotOffered "The checker lists no officers"
            Owners = Support.NotOffered "The checker lists no owners"
            Cost =
                AccessCost.FreeWithKey
                    "Register on developer.go.ke (GavaConnect) and create an app for PIN Checker by PIN; pass its consumer key and secret"
            RateLimit = RateLimitPolicy.NotPublished(2, TimeSpan.FromSeconds 1.)
            Licence = "No licence found in the GavaConnect documentation. Not run live"
        }

    /// KRA's answer "Invalid PIN", code 19005, which doesn't say whether the PIN was never issued.
    [<Literal>]
    let invalidPin =
        "KRA answered Invalid PIN (19005), which doesn't say whether the PIN was never issued or was removed"

    let private pinRegex = Regex @"\AP[0-9]{9}[A-Z]\z"

    /// A non-individual's PIN: P, nine digits and a letter, in upper case. An individual's PIN starts with A and isn't looked up.
    let normalPin (number: string) =
        let normal =
            if isNull number then
                ""
            else
                number.Trim().ToUpperInvariant()

        if pinRegex.IsMatch normal then
            ValueSome normal
        else
            ValueNone

    /// The documented values of StatusOfPIN. Active is a tax PIN in use, which isn't legal standing; a suspended or stopped PIN doesn't say what
    /// became of the company. Any other value is Unknown.
    let standing (status: string voption) =
        match status with
        | ValueSome "Active" -> CompanyStanding.Registered(ValueSome "PIN Active")
        | ValueSome "Suspended" -> CompanyStanding.Unknown(ValueSome "PIN Suspended")
        | ValueSome "Stopped" -> CompanyStanding.Unknown(ValueSome "PIN Stopped")
        | ValueSome "Cancelled" -> CompanyStanding.Closed "PIN Cancelled"
        | other -> CompanyStanding.Unknown other

    /// The answer for this PIN: a company, Invalid PIN, or an individual's record, which isn't read. Any other answer is unreadable.
    let answer (pin: string) (root: JsonElement) =
        match SourceAnswer.trimmed "Status" root, OpenData.text "ResponseCode" root with
        | ValueSome "OK", _ ->
            let data =
                match tryObject "PINDATA" root with
                | ValueSome data -> data
                | ValueNone -> raise (InvalidJson "Missing or invalid field 'PINDATA'")

            let number = required "KRAPIN" SourceAnswer.trimmed data

            if number <> pin then
                raise (InvalidJson "The answer holds another PIN")
            elif SourceAnswer.trimmed "TypeOfTaxpayer" data = ValueSome "Individual" then
                NotAvailable "The PIN belongs to an individual, not a company"
            else
                let status = SourceAnswer.trimmed "StatusOfPIN" data

                Available(
                    ValueSome
                        {
                            Name = required "Name" SourceAnswer.trimmed data
                            OtherNames = []
                            Number = number
                            Jurisdiction = Jurisdiction.ofCode "KE"
                            JurisdictionAsPublished = ValueNone
                            Standing = standing status
                            Concerns = CompanyConcerns.notPublished
                            StatusAsPublished = status
                            LegalForm = SourceAnswer.trimmed "TypeOfTaxpayer" data
                            IncorporatedOn = ValueNone
                            CeasedOn = ValueNone
                            Addresses = NotAvailable "The checker gives no address"
                            Role = RecordRole.NotStated
                            HomeRegistration = ValueNone
                            EstablishmentOf = ValueNone
                        }
                )
        | ValueSome "NOK", ValueSome "19005" -> NotAvailable invalidPin
        | _ -> raise (InvalidJson "The answer is neither a PIN's data nor a documented exception")

/// A bearer token from KRA's token endpoint, asked for when first needed, again a minute before it expires, and again once KRA refuses it. The
/// documentation gives a GET with the grant type in the query and the key and secret in a Basic header, not the form POST of RFC 6749.
[<Sealed>]
type internal KenyaToken(client: SourceClient, credentials: ClientCredentials) =
    let gate = new SemaphoreSlim(1, 1)
    let mutable held: struct (string * DateTimeOffset) voption = ValueNone

    let tokenUri =
        Uri "https://api.kra.go.ke/v1/token/generate?grant_type=client_credentials"

    let basic =
        Convert.ToBase64String(Encoding.UTF8.GetBytes($"{credentials.Id}:{credentials.Secret}"))

    let read (e: JsonElement) =
        required "access_token" Json.tryString e, Json.tryInt "expires_in" e

    /// The header for the held token, or a new one. refused is a header KRA refused: its token is dropped if still held.
    let headerAsync
        (refused: (string * string) voption)
        (ct: CancellationToken)
        : Task<Result<string * string, RegistryFailure>> =
        task {
            do! gate.WaitAsync ct

            try
                match held, refused with
                | ValueSome(struct (token, _)), ValueSome(_, value) when value = "Bearer " + token -> held <- ValueNone
                | _ -> ()

                match held with
                | ValueSome(struct (token, until)) when client.Now < until ->
                    return Ok("Authorization", "Bearer " + token)
                | _ ->
                    match! client.GetAsync(tokenUri, [ "Authorization", "Basic " + basic ], ct) with
                    | Error failure -> return Error failure
                    | Ok reply when reply.Status = 404 ->
                        return Error(RegistryFailure.RequestRefused "HTTP 404 from the token endpoint")
                    | Ok reply ->
                        match SourceAnswer.json reply read with
                        | Error failure -> return Error failure
                        | Ok(token, lifetime) ->
                            let seconds = lifetime |> ValueOption.defaultValue 300

                            held <-
                                ValueSome(struct (token, reply.At + TimeSpan.FromSeconds(float (max 0 (seconds - 60)))))

                            return Ok("Authorization", "Bearer " + token)
            finally
                gate.Release() |> ignore
        }

    interface IBearerToken with
        member _.HeaderAsync(ct) = headerAsync ValueNone ct
        member _.RenewAsync(refused, ct) = headerAsync (ValueSome refused) ct

    interface IDisposable with
        member _.Dispose() = gate.Dispose()

/// Kenya as an ICompanyRegistry. Invalid PIN is NotAvailable, never "not held".
[<Sealed>]
type internal KenyaRegistry(connection: SourceConnection, credentials: ClientCredentials) =
    let client =
        SourceClient(connection, SourceAnswer.pacing 2 (TimeSpan.FromSeconds 1.))

    let token = new KenyaToken(client, credentials) :> IBearerToken
    let coverage = Kenya.coverage

    let notOffered reason jurisdiction =
        SourceAnswer.inCoverage coverage jurisdiction (fun () ->
            Task.FromResult(Ok(SourceAnswer.unasked coverage client.Now (NotAvailable reason))))

    interface ICompanyRegistry with
        member _.Coverage = coverage

        member _.TryFindAsync(number: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                match Kenya.normalPin number with
                | ValueNone ->
                    Task.FromResult(
                        Error(
                            RegistryFailure.InvalidRequest
                                "A company's KRA PIN is P, nine digits and a letter; an individual's PIN isn't looked up"
                        )
                    )
                | ValueSome pin ->
                    task {
                        let body () : HttpContent =
                            new StringContent(
                                JsonSerializer.Serialize(dict [ "KRAPIN", pin ]),
                                Encoding.UTF8,
                                "application/json"
                            )

                        let send header =
                            client.PostAsync(Uri "https://api.kra.go.ke/checker/v1/pinbypin", body, [ header ], ct)

                        match! Bearer.sendAsync token send ct with
                        | Error failure -> return Error failure
                        | Ok reply when reply.Status = 404 -> return Error(RegistryFailure.RequestRefused "HTTP 404")
                        | Ok reply ->
                            return
                                SourceAnswer.json reply (Kenya.answer pin)
                                |> Result.map (SourceAnswer.evidence coverage reply ValueNone)
                    })

        member _.SearchAsync(_: string, jurisdiction: Jurisdiction, _: CancellationToken) =
            notOffered "The checker validates one PIN" jurisdiction

        member _.OfficersAsync(_: string, jurisdiction: Jurisdiction, _: CancellationToken) =
            notOffered "The checker lists no officers" jurisdiction

        member _.OwnersAsync(_: string, jurisdiction: Jurisdiction, _: OwnerPurpose, _: CancellationToken) =
            notOffered "The checker lists no owners" jurisdiction
