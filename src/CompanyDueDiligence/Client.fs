namespace CompanyDueDiligence.CompaniesHouse

open System
open System.Net
open System.Net.Http
open System.Net.Http.Headers
open System.Text
open System.Threading
open System.Threading.Tasks

/// The API key of an application registered as "API Key" on the Companies House Developer Hub. Printing one (%A, ToString) hides it.
[<StructuredFormatDisplay("{Display}")>]
type internal Credentials =
    {
        ApiKey: string
    }

    member _.Display = "{ ApiKey hidden }"
    override this.ToString() = this.Display

/// Base addresses of the Public Data API and the Document API. A missing trailing '/' is added when the client is created.
type internal Endpoints = { Api: Uri; Document: Uri }

module internal Endpoints =

    /// The scheme, host and port of each address: the only places the key goes.
    let internal origins (endpoints: Endpoints) =
        set
            [
                endpoints.Api.GetLeftPart UriPartial.Authority
                endpoints.Document.GetLeftPart UriPartial.Authority
            ]

    /// The address with a trailing '/', so that relative paths resolve under it rather than replacing its last segment.
    let internal asBase (address: Uri) =
        if address.AbsolutePath.EndsWith "/" then
            address
        else
            UriBuilder(address, Path = address.AbsolutePath + "/").Uri

    let live =
        {
            Api = Uri "https://api.company-information.service.gov.uk/"
            Document = Uri "https://document-api.company-information.service.gov.uk/"
        }

/// What an answer said about the rate limit. The rate limiting guide names no headers, so each is read only if present, and one that is
/// missing or unusable changes nothing.
type internal RateLimit =
    {
        /// The Retry-After header as a delay.
        RetryAfter: TimeSpan voption
        /// The X-Ratelimit-Reset header (Unix seconds), or Retry-After as a date.
        ResetsAt: DateTimeOffset voption
        /// The X-Ratelimit-Limit header.
        Limit: int voption
        /// The X-Ratelimit-Window header, such as "5m".
        Window: string voption
        /// The X-Ratelimit-Remain or X-Ratelimit-Remaining header.
        Remaining: int voption
    }

module internal RateLimit =

    /// The rate limiting guide's period: 600 requests per application, then 429 until the period ends.
    let documentedWindow = TimeSpan.FromMinutes 5.

    /// The rate limiting guide's number of requests in that period.
    [<Literal>]
    let DocumentedCalls = 600

    let none =
        {
            RetryAfter = ValueNone
            ResetsAt = ValueNone
            Limit = ValueNone
            Window = ValueNone
            Remaining = ValueNone
        }

    /// How long to wait: what the answer said, or else the documented period.
    let wait (now: DateTimeOffset) (limit: RateLimit) =
        match limit.RetryAfter, limit.ResetsAt with
        | ValueSome delay, _ -> delay
        | ValueNone, ValueSome at when at > now -> at - now
        | ValueNone, ValueSome _ -> TimeSpan.Zero
        | ValueNone, ValueNone -> documentedWindow

/// A success answer the client could not read. Printing one (%A, ToString) leaves the body out.
[<StructuredFormatDisplay("{Display}")>]
type internal UnreadableAnswer =
    {
        HttpStatus: int
        /// The answer as sent. It can hold personal data: keep it out of logs.
        Body: string
        /// Where the answer could not be read. It quotes nothing from the answer except plain field names.
        Reason: string
    }

    member this.Display =
        $"{{ HttpStatus = {this.HttpStatus}; Reason = {this.Reason}; Body hidden }}"

    override this.ToString() = this.Display

type internal CompaniesHouseFailure =
    /// The request was not sent: an argument cannot be valid, or the address isn't one of the client's endpoints.
    | InvalidRequest of reason: string
    /// A 401: the key is wrong, revoked, or restricted away from this caller.
    | KeyRefused of body: string
    /// A 429: over the rate limit.
    | RateLimited of RateLimit
    /// A redirect the client didn't follow. reason holds no signed URL.
    | RedirectRefused of httpStatus: int * reason: string
    /// Any other non-success status. From the document host, the body is left out: it can repeat the signed URL.
    | ApiError of httpStatus: int * body: string
    /// The pages ended with fewer items than the list's own total.
    | IncompleteList of read: int * expected: int
    /// A page came back that had already been read, so following on wouldn't reach the end.
    | RepeatedPage of startIndex: int
    /// The list's total or counts disagree with its items or changed between pages. reason holds nothing from the answer.
    | InconsistentList of startIndex: int * reason: string
    /// The list needs more calls than one read makes.
    | ListTooLong of pages: int * read: int * expected: int
    /// There was no complete answer.
    | TransportError of exn
    /// A success answer the client could not read.
    | InvalidResponse of UnreadableAnswer

/// What the application should do about a failure.
[<RequireQualifiedAccess>]
type internal FailureAction =
    /// Temporary: the calls only read, so the same call can run again after a wait.
    | RetryLater
    /// The same call fails the same way until the key, the request or this client changes.
    | FixRequest

module internal CompaniesHouseFailure =

    /// One line saying what went wrong, for logs and exception messages. It holds no key and no unreadable answer's body.
    let describe failure =
        match failure with
        | InvalidRequest reason -> $"Not sent: {reason}"
        | KeyRefused _ -> "Companies House refused the API key (HTTP 401)"
        | RateLimited limit -> $"Over the Companies House rate limit: wait {RateLimit.wait DateTimeOffset.UtcNow limit}"
        | RedirectRefused(status, reason) -> $"HTTP {status} redirect not followed: {reason}"
        | ApiError(status, "") -> $"Companies House answered HTTP {status}"
        | ApiError(status, body) -> $"Companies House answered HTTP {status}: {body}"
        | IncompleteList(read, expected) -> $"The list ended after {read} of {expected} items"
        | RepeatedPage start -> $"The page at start_index {start} repeats an earlier page"
        | InconsistentList(start, reason) -> $"The list can't be relied on at start_index {start}: {reason}"
        | ListTooLong(pages, read, expected) -> $"Stopped after {pages} pages, at {read} of {expected} items"
        | TransportError e -> $"Request failed: {e.Message}"
        | InvalidResponse answer ->
            $"Companies House answered HTTP {answer.HttpStatus}, and the answer could not be read: {answer.Reason}"

    let action failure =
        match failure with
        | RateLimited _
        | TransportError _
        | RepeatedPage _
        | InconsistentList _
        | IncompleteList _ -> FailureAction.RetryLater
        | ApiError(status, _) when status >= 500 -> FailureAction.RetryLater
        | ApiError _
        | ListTooLong _
        | InvalidRequest _
        | KeyRefused _
        | RedirectRefused _
        | InvalidResponse _ -> FailureAction.FixRequest

/// Raised inside a call when a request would carry the key off the client's endpoints; calls return it as InvalidRequest.
exception internal OtherHostException of host: string

/// Adds the key to requests for the client's endpoints and refuses any other address. Calls to a signed document URL don't pass through here.
type internal KeyHandler(authorization: AuthenticationHeaderValue, origins: Set<string>, inner: HttpMessageHandler) =
    inherit DelegatingHandler(inner)

    override _.SendAsync(request, ct) =
        if origins.Contains(request.RequestUri.GetLeftPart UriPartial.Authority) then
            request.Headers.Authorization <- authorization
            base.SendAsync(request, ct)
        else
            Task.FromException<HttpResponseMessage>(OtherHostException request.RequestUri.Host)

module internal Timeouts =

    /// Refuses at creation a timeout HttpClient would refuse at the first call.
    let check (timeout: TimeSpan) =
        let longest = TimeSpan.FromMilliseconds(float Int32.MaxValue)

        if
            timeout <> Timeout.InfiniteTimeSpan
            && (timeout <= TimeSpan.Zero || timeout > longest)
        then
            raise (
                ArgumentOutOfRangeException(
                    nameof timeout,
                    timeout,
                    "A timeout must be positive and at most Int32.MaxValue milliseconds, or infinite."
                )
            )

/// How a client spaces its keyed calls and answers a 429. The limit belongs to the key: clients sharing one key need to share out Calls.
[<Struct>]
type internal Pacing =
    {
        /// At most this many calls in any Window.
        Calls: int
        Window: TimeSpan
        /// How many times a call asks again after a 429.
        Retries: int
        /// The longest a call waits at any point. A longer wait isn't made: the call is RateLimited. Zero never waits.
        LongestWait: TimeSpan
    }

module internal Pacing =

    /// The rate limiting guide's limit, two retries, and a wait of up to its period.
    let documented =
        {
            Calls = RateLimit.DocumentedCalls
            Window = RateLimit.documentedWindow
            Retries = 2
            LongestWait = RateLimit.documentedWindow
        }

    /// Refuses at creation what can't pace anything.
    let internal check (pacing: Pacing) =
        let longest = TimeSpan.FromMilliseconds(float Int32.MaxValue)

        if
            pacing.Calls < 1
            || pacing.Window <= TimeSpan.Zero
            || pacing.Retries < 0
            || pacing.LongestWait < TimeSpan.Zero
            || pacing.LongestWait > longest
        then
            raise (
                ArgumentOutOfRangeException(
                    nameof pacing,
                    "Pacing needs a positive Calls and Window, and Retries and a LongestWait that aren't negative."
                )
            )

/// Spaces one client's keyed calls: at most Calls in any Window, and none during a hold. Calls can run at once, so its state changes under a lock.
type internal Pacer(pacing: Pacing, now: unit -> DateTimeOffset, delay: TimeSpan -> CancellationToken -> Task) =
    let gate = obj ()
    let mutable sent: DateTimeOffset list = []
    let mutable heldUntil = DateTimeOffset.MinValue

    /// Zero when a call may go, having counted it; otherwise how long until one may.
    let take () =
        lock gate (fun () ->
            let at = now ()
            sent <- sent |> List.filter (fun time -> at - time < pacing.Window)

            let full =
                if sent.Length < pacing.Calls then
                    TimeSpan.Zero
                else
                    (List.item (pacing.Calls - 1) sent) + pacing.Window - at

            let wait = max full (heldUntil - at)

            if wait <= TimeSpan.Zero then
                sent <- at :: sent

            wait)

    new(pacing) = Pacer(pacing, (fun () -> DateTimeOffset.UtcNow), (fun wait ct -> Task.Delay(wait, ct)))

    member _.Now = now ()
    member _.Retries = pacing.Retries
    member _.LongestWait = pacing.LongestWait

    /// Waits for a turn. ValueNone when the call may go; ValueSome the wait left when it is past LongestWait, and the call hasn't been counted.
    member _.TurnAsync(ct: CancellationToken) =
        let rec waiting (waited: TimeSpan) : Task<TimeSpan voption> =
            task {
                let wait = take ()

                if wait <= TimeSpan.Zero then
                    return ValueNone
                elif waited + wait > pacing.LongestWait then
                    return ValueSome wait
                else
                    do! delay wait ct
                    return! waiting (waited + wait)
            }

        waiting TimeSpan.Zero

    /// No call goes for this long. A hold beyond both Window and LongestWait is cut to the longer of them, so that a wrong header can't stop the client for good.
    member _.HoldFor(wait: TimeSpan) =
        lock gate (fun () ->
            let until = now () + min wait (max pacing.Window pacing.LongestWait)

            if until > heldUntil then
                heldUntil <- until)

    /// Holds until the reset when an answer says none are left. Without both headers it changes nothing.
    member this.NoteAnswer(limit: RateLimit) =
        match limit.Remaining, limit.ResetsAt with
        | ValueSome 0, ValueSome at -> this.HoldFor(at - now ())
        | _ -> ()

/// A Companies House connection. Keep it for the application's lifetime; a handler passed in stays the caller's and is never disposed here.
/// A caller's handler must not follow redirects: CreateHandler's doesn't, so a 3xx comes back as an answer the client can act on.
[<Sealed>]
type internal CompaniesHouseClient
    internal
    (
        credentials: Credentials,
        configured: Endpoints,
        handler: HttpMessageHandler,
        timeout: TimeSpan voption,
        pacer: Pacer
    ) =
    do
        if String.IsNullOrWhiteSpace credentials.ApiKey then
            raise (ArgumentException("An API key is required.", nameof credentials))

    let endpoints =
        {
            Api = Endpoints.asBase configured.Api
            Document = Endpoints.asBase configured.Document
        }

    /// The guide's form: Basic, with the base64 of the key, a colon and an empty password.
    let authorization =
        AuthenticationHeaderValue("Basic", Convert.ToBase64String(Encoding.UTF8.GetBytes(credentials.ApiKey + ":")))

    /// Without a timeout it keeps HttpClient's default of 100 seconds.
    let limited (http: HttpClient) =
        timeout |> ValueOption.iter (fun limit -> http.Timeout <- limit)
        http

    let keyed =
        limited (new HttpClient(new KeyHandler(authorization, Endpoints.origins endpoints, handler), false))

    let keyless = limited (new HttpClient(handler, false))

    internal new(credentials, endpoints, handler, timeout) =
        CompaniesHouseClient(credentials, endpoints, handler, timeout, Pacer(Pacing.documented))

    new(credentials, endpoints, handler) = CompaniesHouseClient(credentials, endpoints, handler, ValueNone)

    member _.Endpoints = endpoints

    /// Sends the key, to the client's endpoints only.
    member internal _.Keyed = keyed

    /// Never sends the key, wherever the request goes.
    member internal _.Keyless = keyless

    /// Spaces the calls that go through Keyed. It waits outside HttpClient, so its waits don't count against a timeout.
    member internal _.Pacer = pacer

    /// A handler that doesn't follow redirects. Create it once per application.
    static member CreateHandler() : HttpMessageHandler =
        new SocketsHttpHandler(
            UseCookies = false,
            PooledConnectionLifetime = TimeSpan.FromMinutes 5.,
            AllowAutoRedirect = false
        )

    /// A client with its own handler and HttpClient's default timeout of 100 seconds.
    static member Create(credentials: Credentials, endpoints: Endpoints) =
        CompaniesHouseClient(credentials, endpoints, CompaniesHouseClient.CreateHandler())

    /// A client with its own handler. timeout limits one HTTP request, so one page of a list and each hop of a document; past it the call
    /// is a TransportError.
    static member Create(credentials: Credentials, endpoints: Endpoints, timeout: TimeSpan) =
        Timeouts.check timeout
        CompaniesHouseClient(credentials, endpoints, CompaniesHouseClient.CreateHandler(), ValueSome timeout)

    /// A client with its own handler, a timeout as above, and its own pacing in place of Pacing.documented.
    static member Create(credentials: Credentials, endpoints: Endpoints, timeout: TimeSpan, pacing: Pacing) =
        Timeouts.check timeout
        Pacing.check pacing

        CompaniesHouseClient(
            credentials,
            endpoints,
            CompaniesHouseClient.CreateHandler(),
            ValueSome timeout,
            Pacer pacing
        )

module internal CompaniesHouseClient =

    /// A client over a caller-owned handler, with Pacing.documented.
    let create credentials endpoints (handler: HttpMessageHandler) =
        CompaniesHouseClient(credentials, endpoints, handler)

    /// A client over a caller-owned handler, with its own pacing.
    let createPaced credentials endpoints (pacing: Pacing) (handler: HttpMessageHandler) =
        Pacing.check pacing
        CompaniesHouseClient(credentials, endpoints, handler, ValueNone, Pacer pacing)
