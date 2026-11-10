namespace CompanyDueDiligence

open System
open System.Globalization
open System.Net.Http
open System.Text.Json
open System.Threading
open System.Threading.Tasks
open CompanyDueDiligence.CompaniesHouse

/// What every request names this library as: its version and address, which some sources, such as Costa Rica's Hacienda, require.
module internal UserAgent =

    let value =
        let version = Reflection.Assembly.GetExecutingAssembly().GetName().Version
        $"CompanyDueDiligence/{version.Major}.{version.Minor}.{version.Build} (+https://github.com/Thorium/CompanyDueDiligence)"

/// One HTTP connection for the sources, keyed or not; each keyed source sends its key to its own host only. Keep it for the application's lifetime; a handler passed in stays the caller's and must
/// not follow redirects, since a redirect would carry a source's key headers to another host. An HttpClientHandler or SocketsHttpHandler, alone
/// or inside a DelegatingHandler, with AllowAutoRedirect set raises ArgumentException. In WebAssembly the browser's fetch follows redirects and
/// the property can't be set, so it isn't checked there, and only sources needing no key belong there.
[<Sealed>]
type SourceConnection
    internal (handler: HttpMessageHandler, now: unit -> DateTimeOffset, delay: TimeSpan -> CancellationToken -> Task) =

    /// The answers read here are small; a larger one fails as Unavailable instead of filling memory. A choice made without data.
    static let largestAnswer = 32L * 1024L * 1024L

    static let rec followsRedirects (handler: HttpMessageHandler) =
        match handler with
        | :? HttpClientHandler as client -> client.AllowAutoRedirect
        | :? SocketsHttpHandler as sockets -> sockets.AllowAutoRedirect
        | :? DelegatingHandler as delegating when not (isNull delegating.InnerHandler) ->
            followsRedirects delegating.InnerHandler
        | _ -> false

    do
        if not (OperatingSystem.IsBrowser()) && followsRedirects handler then
            invalidArg
                (nameof handler)
                "The handler follows redirects, which would carry a source's key to another host: set AllowAutoRedirect = false."

    let http =
        let client =
            new HttpClient(
                handler,
                false,
                Timeout = TimeSpan.FromSeconds 30.,
                MaxResponseContentBufferSize = largestAnswer
            )

        client.DefaultRequestHeaders.UserAgent.ParseAdd UserAgent.value
        client

    member internal _.Http = http
    member internal _.Handler = handler
    member internal _.Now = now
    member internal _.Delay = delay

    /// A connection over a caller-owned handler, which must not follow redirects.
    new(handler) = SourceConnection(handler, (fun () -> DateTimeOffset.UtcNow), (fun wait ct -> Task.Delay(wait, ct)))

    /// A connection with its own handler, which doesn't follow redirects and keeps no cookies.
    static member Create() =
        SourceConnection(
            new SocketsHttpHandler(
                UseCookies = false,
                PooledConnectionLifetime = TimeSpan.FromMinutes 5.,
                AllowAutoRedirect = false
            )
        )

/// An answer a source gave, with a status the adapter still has to read: a success or a 404.
type internal SourceReply =
    {
        Status: int
        Body: string
        At: DateTimeOffset
        /// The Last-Modified header, for a source whose data is a dated copy.
        LastModified: DateOnly voption
    }

/// A source's calls, paced for that source. Every failure comes back as a RegistryFailure, holding no text from the answer. Cancelling ct throws.
/// answered lists further statuses the adapter reads itself, such as a 410 that states a removal or a 500 carrying a SOAP fault.
type internal SourceClient(connection: SourceConnection, pacing: Pacing, answered: Set<int>) =
    let pacer = Pacer(pacing, connection.Now, connection.Delay)

    let retryAfter (response: HttpResponseMessage) =
        match response.Headers.RetryAfter with
        | null -> ValueNone
        | header when header.Delta.HasValue -> ValueSome header.Delta.Value
        | header when header.Date.HasValue -> ValueSome(header.Date.Value - connection.Now())
        | _ -> ValueNone

    let lastModified (response: HttpResponseMessage) =
        let modified = response.Content.Headers.LastModified

        if modified.HasValue then
            ValueSome(DateOnly.FromDateTime modified.Value.UtcDateTime)
        else
            ValueNone

    /// request makes a new message for each try, since a sent one can't be sent again.
    let rec attempt
        (request: unit -> HttpRequestMessage)
        (retries: int)
        (ct: CancellationToken)
        : Task<Result<SourceReply, RegistryFailure>> =
        task {
            match! pacer.TurnAsync ct with
            | ValueSome wait -> return Error(RegistryFailure.RateLimited(ValueSome wait))
            | ValueNone ->
                use message = request ()
                use! response = connection.Http.SendAsync(message, ct)
                let status = int response.StatusCode

                if status = 429 then
                    let wait = retryAfter response |> ValueOption.defaultValue pacing.Window
                    pacer.HoldFor wait

                    if retries > 0 && wait <= pacing.LongestWait then
                        return! attempt request (retries - 1) ct
                    else
                        return Error(RegistryFailure.RateLimited(ValueSome wait))
                elif status = 401 then
                    return Error(RegistryFailure.AccessRefused "HTTP 401")
                elif response.IsSuccessStatusCode || status = 404 || answered.Contains status then
                    let! body = response.Content.ReadAsStringAsync ct

                    return
                        Ok
                            {
                                Status = status
                                Body = body
                                At = connection.Now()
                                LastModified = lastModified response
                            }
                elif status >= 500 then
                    return Error(RegistryFailure.Unavailable $"HTTP {status}")
                elif status >= 300 && status < 400 then
                    return Error(RegistryFailure.RequestRefused $"HTTP {status} redirect not followed")
                else
                    return Error(RegistryFailure.RequestRefused $"HTTP {status}")
        }

    let send (request: unit -> HttpRequestMessage) (ct: CancellationToken) =
        task {
            try
                return! attempt request pacing.Retries ct
            with error when not (ct.IsCancellationRequested && (error :? OperationCanceledException)) ->
                return Error(RegistryFailure.Unavailable error.Message)
        }

    let withHeaders (headers: (string * string) list) (message: HttpRequestMessage) =
        for name, value in headers do
            message.Headers.TryAddWithoutValidation(name, value) |> ignore

        message

    new(connection: SourceConnection, pacing: Pacing) = SourceClient(connection, pacing, Set.empty)

    member _.Now = connection.Now()

    member _.GetAsync(uri: Uri, ct: CancellationToken) =
        send (fun () -> new HttpRequestMessage(HttpMethod.Get, uri)) ct

    /// A GET carrying headers, such as a key.
    member _.GetAsync(uri: Uri, headers: (string * string) list, ct: CancellationToken) =
        send (fun () -> new HttpRequestMessage(HttpMethod.Get, uri) |> withHeaders headers) ct

    /// A POST of a body, such as a SOAP envelope or a JSON query. content makes the body afresh for each try.
    member _.PostAsync(uri: Uri, content: unit -> HttpContent, headers: (string * string) list, ct: CancellationToken) =
        send
            (fun () ->
                new HttpRequestMessage(HttpMethod.Post, uri, Content = content ())
                |> withHeaders headers)
            ct

/// Reading and answering shared by the adapters.
module internal SourceAnswer =

    /// A source's published or chosen pace as Pacing: two more tries after a 429, and no wait longer than half a minute.
    let pacing calls (per: TimeSpan) : Pacing =
        {
            Calls = calls
            Window = per
            Retries = 2
            LongestWait = TimeSpan.FromSeconds 30.
        }

    /// Runs parse over the reply's JSON. An answer that isn't JSON, lacks what parse requires, or holds a value out of range, of the wrong
    /// form or kind, or a text that isn't whole UTF-16, is UnreadableAnswer, quoting nothing from it.
    let json (reply: SourceReply) (parse: JsonElement -> 'T) =
        try
            use document = JsonDocument.Parse reply.Body
            Ok(parse document.RootElement)
        with
        | :? JsonException as error -> Error(RegistryFailure.UnreadableAnswer $"Not JSON, at line {error.LineNumber}")
        | Json.InvalidJson reason -> Error(RegistryFailure.UnreadableAnswer reason)
        | :? ArgumentOutOfRangeException ->
            Error(RegistryFailure.UnreadableAnswer "A value in the answer is out of range")
        | :? FormatException ->
            Error(RegistryFailure.UnreadableAnswer "A value in the answer isn't of the form expected")
        | :? OverflowException -> Error(RegistryFailure.UnreadableAnswer "A number in the answer is too large")
        // A text that isn't whole UTF-16, such as half a surrogate pair, or a value of another kind than the one read.
        | :? InvalidOperationException ->
            Error(RegistryFailure.UnreadableAnswer "A value in the answer can't be read as the kind expected")

    let evidence (coverage: RegistryCoverage) (reply: SourceReply) asOf value : Evidence<_> =
        {
            Source = coverage.Registry
            RetrievedAt = reply.At
            AsOf = asOf
            Raw = ValueSome reply.Body
            Value = value
        }

    /// An answer made without calling the source: what its Coverage says it doesn't offer.
    let unasked (coverage: RegistryCoverage) (now: DateTimeOffset) value : Evidence<_> =
        {
            Source = coverage.Registry
            RetrievedAt = now
            AsOf = ValueNone
            Raw = ValueNone
            Value = value
        }

    /// Runs a call in a covered jurisdiction; any other answers OutsideCoverage and sends nothing.
    let inCoverage
        (coverage: RegistryCoverage)
        jurisdiction
        (call: unit -> Task<Result<Evidence<'T>, RegistryFailure>>)
        =
        if RegistryCoverage.covers coverage jurisdiction then
            call ()
        else
            Task.FromResult(Error(RegistryFailure.OutsideCoverage jurisdiction))

    /// What a register restricting owners answers, by purpose.
    let restrictedOwners (purpose: OwnerPurpose) (who: string) (route: string) =
        match purpose with
        | OwnerPurpose.CustomerDueDiligence -> NeedsEntitlement { Who = who; Route = route }
        | OwnerPurpose.PublicRead -> NotAvailable "The register doesn't publish owners to anyone who asks"

    /// A "yyyy-MM-dd" date at the start of a longer timestamp.
    let dateOfTimestamp name e =
        Json.tryString name e
        |> ValueOption.bind (fun text ->
            if text.Length < 10 then
                ValueNone
            else
                match
                    DateOnly.TryParseExact(
                        text.Substring(0, 10),
                        "yyyy-MM-dd",
                        CultureInfo.InvariantCulture,
                        DateTimeStyles.None
                    )
                with
                | true, date -> ValueSome date
                | _ -> ValueNone)

    /// A value with its blanks trimmed; empty counts as absent.
    let trimmed name e =
        Json.tryString name e
        |> ValueOption.map _.Trim()
        |> ValueOption.bind (fun text -> if text = "" then ValueNone else ValueSome text)

    /// The text with each secret, as given and URI-escaped, taken out. For a source whose key travels where a message could quote it.
    let hidingIn (secrets: string list) (text: string) =
        secrets
        |> List.filter (String.IsNullOrEmpty >> not)
        |> List.collect (fun secret -> [ secret; Uri.EscapeDataString secret ])
        |> List.distinct
        |> List.sortByDescending _.Length
        |> List.fold (fun (text: string) secret -> text.Replace(secret, "{hidden}")) text

    /// The failure with each secret taken out of its reason.
    let hiding (secrets: string list) (failure: RegistryFailure) =
        let hide = hidingIn secrets

        match failure with
        | RegistryFailure.InvalidRequest reason -> RegistryFailure.InvalidRequest(hide reason)
        | RegistryFailure.AccessRefused reason -> RegistryFailure.AccessRefused(hide reason)
        | RegistryFailure.RequestRefused reason -> RegistryFailure.RequestRefused(hide reason)
        | RegistryFailure.Unavailable reason -> RegistryFailure.Unavailable(hide reason)
        | RegistryFailure.UnreadableAnswer reason -> RegistryFailure.UnreadableAnswer(hide reason)
        | RegistryFailure.OutsideCoverage _
        | RegistryFailure.RateLimited _ -> failure

/// A key a source issues for free. It is held out of logs and printed output.
[<Sealed>]
type SourceKey(value: string) =
    do
        if String.IsNullOrWhiteSpace value then
            invalidArg (nameof value) "A key can't be blank."

    member internal _.Value = value
    override _.ToString() = "{ key hidden }"

/// An ID and secret a source issues for free: sent as an OAuth2 client-credentials grant, an HTTP Basic user and password, or a consumer key and
/// secret, as that source's docs say. Held out of logs and printed output.
[<Sealed>]
type ClientCredentials(clientId: string, clientSecret: string) =
    do
        if String.IsNullOrWhiteSpace clientId || String.IsNullOrWhiteSpace clientSecret then
            invalidArg (nameof clientId) "Neither the client ID nor the secret can be blank."

    member internal _.Id = clientId
    member internal _.Secret = clientSecret
    override _.ToString() = "{ client credentials hidden }"

/// A bearer token a source issues, held until it expires or the source refuses it.
type internal IBearerToken =
    /// The Authorization header for the next call.
    abstract HeaderAsync: ct: CancellationToken -> Task<Result<string * string, RegistryFailure>>

    /// The header for a new token, the refused one dropped if it is still the one held.
    abstract RenewAsync:
        refused: (string * string) * ct: CancellationToken -> Task<Result<string * string, RegistryFailure>>

/// Calls made with a bearer token.
module internal Bearer =

    /// Sends with the token's header, and once more with a new token when the source answers 401, as it does for a token revoked or expired
    /// early. A second 401 stands.
    let sendAsync
        (token: IBearerToken)
        (send: string * string -> Task<Result<SourceReply, RegistryFailure>>)
        (ct: CancellationToken)
        =
        task {
            match! token.HeaderAsync ct with
            | Error failure -> return Error failure
            | Ok header ->
                match! send header with
                | Error(RegistryFailure.AccessRefused _) ->
                    match! token.RenewAsync(header, ct) with
                    | Error failure -> return Error failure
                    | Ok fresh -> return! send fresh
                | answered -> return answered
        }

/// A bearer token from a client-credentials grant, asked for when first needed, again a minute before it expires, and again once the source
/// refuses it. The credentials go in a Basic header, as RFC 6749 prefers.
[<Sealed>]
type internal ClientCredentialsToken
    (client: SourceClient, tokenUri: Uri, credentials: ClientCredentials, scope: string voption) =
    /// One request for a token at a time; the token is shared by every call.
    let gate = new SemaphoreSlim(1, 1)
    let mutable held: struct (string * DateTimeOffset) voption = ValueNone

    let form () : HttpContent =
        let fields =
            [
                yield Collections.Generic.KeyValuePair("grant_type", "client_credentials")
                match scope with
                | ValueSome scope -> yield Collections.Generic.KeyValuePair("scope", scope)
                | ValueNone -> ()
            ]

        new FormUrlEncodedContent(fields)

    let basic =
        Convert.ToBase64String(
            Text.Encoding.UTF8.GetBytes(
                Uri.EscapeDataString credentials.Id
                + ":"
                + Uri.EscapeDataString credentials.Secret
            )
        )

    let read (e: JsonElement) =
        Json.required "access_token" Json.tryString e, Json.tryInt "expires_in" e

    /// The header for the held token, or a new one. refused is a header the source refused: its token is dropped if still held.
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
                    match! client.PostAsync(tokenUri, form, [ "Authorization", "Basic " + basic ], ct) with
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
