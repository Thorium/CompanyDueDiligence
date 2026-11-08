/// Task-based Companies House calls. Each returns Ok, with ValueNone for a 404, or Error CompaniesHouseFailure; cancelling the token throws.
/// A 404 on a list or a company's sub-resource says nothing is held at that address, which doesn't tell a company without records from a
/// number that doesn't exist: Companies.getAsync does.
module internal CompanyDueDiligence.CompaniesHouse.Api

open System
open System.Globalization
open System.Net.Http
open System.Net.Http.Headers
open System.Text
open System.Text.Json
open System.Text.RegularExpressions
open System.Threading
open System.Threading.Tasks

/// Text from a JsonException, which never quotes the answer. A bracketed path segment is a key that isn't a plain name, which could be any
/// text, so the path stops there.
let private reasonOf (e: JsonException) =
    let path =
        match e.Path with
        | null -> "?"
        | path ->
            match path.IndexOf("['", StringComparison.Ordinal) with
            | -1 -> path
            | at -> path.Substring(0, at) + "[?]"

    $"Invalid at {path}, line {e.LineNumber}, byte {e.BytePositionInLine}"

let private unreadable status (body: byte[]) reason =
    InvalidResponse
        {
            HttpStatus = status
            Body = Encoding.UTF8.GetString body
            Reason = reason
        }

let private read status (body: byte[]) (parse: JsonElement -> 'T) =
    try
        use document = JsonDocument.Parse(ReadOnlyMemory body)
        Ok(parse document.RootElement)
    with
    | :? JsonException as e -> Error(unreadable status body (reasonOf e))
    | CompanyDueDiligence.Json.InvalidJson reason -> Error(unreadable status body reason)

let private rateLimit (response: HttpResponseMessage) : RateLimit =
    let header (name: string) =
        match response.Headers.TryGetValues name with
        | true, values -> values |> Seq.tryHead |> ValueOption.ofOption
        | _ -> ValueNone

    let number name =
        header name
        |> ValueOption.bind (fun text ->
            match Int64.TryParse(text, NumberStyles.Integer, CultureInfo.InvariantCulture) with
            | true, n -> ValueSome n
            | _ -> ValueNone)

    let reset =
        number "X-Ratelimit-Reset"
        |> ValueOption.bind (fun seconds ->
            if
                seconds >= DateTimeOffset.MinValue.ToUnixTimeSeconds()
                && seconds <= DateTimeOffset.MaxValue.ToUnixTimeSeconds()
            then
                ValueSome(DateTimeOffset.FromUnixTimeSeconds seconds)
            else
                ValueNone)

    let delay, date =
        match response.Headers.RetryAfter with
        | null -> ValueNone, ValueNone
        | retryAfter -> ValueOption.ofNullable retryAfter.Delta, ValueOption.ofNullable retryAfter.Date

    {
        RetryAfter = delay
        ResetsAt = reset |> ValueOption.orElse date
        Limit =
            number "X-Ratelimit-Limit"
            |> ValueOption.bind (fun n ->
                if n >= 0L && n <= int64 Int32.MaxValue then
                    ValueSome(int n)
                else
                    ValueNone)
        Window = header "X-Ratelimit-Window"
        Remaining =
            number "X-Ratelimit-Remain"
            |> ValueOption.orElse (number "X-Ratelimit-Remaining")
            |> ValueOption.bind (fun n ->
                if n >= 0L && n <= int64 Int32.MaxValue then
                    ValueSome(int n)
                else
                    ValueNone)
    }

/// The failure for a status that isn't success, a 404 or an expected redirect.
let private refusal (response: HttpResponseMessage) (body: byte[]) =
    match int response.StatusCode with
    | 401 -> KeyRefused(Encoding.UTF8.GetString body)
    | 429 -> RateLimited(rateLimit response)
    | status when status >= 300 && status < 400 -> RedirectRefused(status, "this call expects no redirect")
    | status -> ApiError(status, Encoding.UTF8.GetString body)

/// Runs a call; exceptions become Error. Cancellation of the caller's token propagates, and a timeout is a TransportError.
let private guarded (ct: CancellationToken) (run: unit -> Task<Result<'T, CompaniesHouseFailure>>) =
    task {
        try
            return! run ()
        with error when not (ct.IsCancellationRequested && (error :? OperationCanceledException)) ->
            return
                match error with
                | OtherHostException host ->
                    Error(InvalidRequest $"{host} isn't one of the client's endpoints, so the key isn't sent there")
                | _ -> Error(TransportError error)
    }

/// The least a call waits before asking again after a 429, whatever the answer said.
let private shortestRetryWait = TimeSpan.FromSeconds 1.

/// Sends a keyed request when the client's pacing allows. A 429 holds the client's other calls too, and is asked again within Pacing.Retries if
/// its wait is within Pacing.LongestWait; a last 429 comes back as the answer. Without a turn within LongestWait nothing is sent.
let private sendKeyed (client: CompaniesHouseClient) (prepare: unit -> HttpRequestMessage) (ct: CancellationToken) =
    let pacer = client.Pacer

    let rec attempt retries : Task<Result<HttpResponseMessage, CompaniesHouseFailure>> =
        task {
            match! pacer.TurnAsync ct with
            | ValueSome wait ->
                return
                    Error(
                        RateLimited
                            { RateLimit.none with
                                RetryAfter = ValueSome wait
                            }
                    )
            | ValueNone ->
                use request = prepare ()
                let! response = client.Keyed.SendAsync(request, ct)
                let limit = rateLimit response
                pacer.NoteAnswer limit

                if int response.StatusCode <> 429 then
                    return Ok response
                else
                    let wait = max shortestRetryWait (RateLimit.wait pacer.Now limit)
                    pacer.HoldFor wait

                    if retries > 0 && wait <= pacer.LongestWait then
                        response.Dispose()
                        return! attempt (retries - 1)
                    else
                        return Ok response
        }

    attempt pacer.Retries

/// A GET whose 404 is ValueNone when notFound accepts its body, else ApiError.
let private getAtWith
    (notFound: byte array -> bool)
    (client: CompaniesHouseClient)
    (uri: unit -> Uri)
    (parse: JsonElement -> 'T)
    (ct: CancellationToken)
    : Task<Result<'T voption, CompaniesHouseFailure>> =
    guarded ct (fun () ->
        task {
            let prepare () =
                let request = new HttpRequestMessage(HttpMethod.Get, uri ())
                request.Headers.Accept.ParseAdd "application/json"
                request

            match! sendKeyed client prepare ct with
            | Error failure -> return Error failure
            | Ok answer ->
                use response = answer
                let! body = response.Content.ReadAsByteArrayAsync ct

                return
                    match int response.StatusCode with
                    | 404 when notFound body -> Ok ValueNone
                    | _ when response.IsSuccessStatusCode ->
                        read (int response.StatusCode) body parse |> Result.map ValueSome
                    | _ -> Error(refusal response body)
        })

let private getAt client uri parse ct =
    getAtWith (fun _ -> true) client uri parse ct

let private get client (path: string) parse ct =
    getAt client (fun () -> Uri(client.Endpoints.Api, path)) parse ct

/// Companies House's own not-found answer: an errors list naming an error ending in "not-found", such as company-profile-not-found, or a
/// message holding "Resource not found for company profile", as the sandbox answered on 2026-11-08. A 404 of another body, as a gateway or a
/// wrong endpoint answers, isn't a statement that the company doesn't exist.
let private ownNotFound (body: byte array) =
    try
        use document = JsonDocument.Parse(ReadOnlyMemory<byte> body)
        let root = document.RootElement

        match root.ValueKind with
        | JsonValueKind.Object ->
            match root.TryGetProperty "errors", root.TryGetProperty "message" with
            | (true, errors), _ when errors.ValueKind = JsonValueKind.Array ->
                errors.EnumerateArray()
                |> Seq.exists (fun error ->
                    error.ValueKind = JsonValueKind.Object
                    && (match error.TryGetProperty "error" with
                        | true, name when name.ValueKind = JsonValueKind.String ->
                            name.GetString().EndsWith("not-found", StringComparison.Ordinal)
                        | _ -> false))
            | _, (true, message) when message.ValueKind = JsonValueKind.String ->
                message.GetString().Contains("Resource not found for company profile", StringComparison.Ordinal)
            | _ -> false
        | _ -> false
    with :? JsonException ->
        false

/// Any Public Data API path this module has no function for, such as "company/00000006/exemptions": relative, with its values already escaped.
/// The element is a copy that outlives the call. A path that leaves the client's endpoints is InvalidRequest.
let getJsonAsync (client: CompaniesHouseClient) (path: string) (ct: CancellationToken) =
    get
        client
        path
        (fun root ->
            match root.ValueKind with
            | JsonValueKind.Object
            | JsonValueKind.Array -> root.Clone()
            | _ -> raise (CompanyDueDiligence.Json.InvalidJson "The answer is neither an object nor a list"))
        ct

let private invalid reason =
    Task.FromResult(Error(InvalidRequest reason))

/// A path value as one escaped segment; empty, "." and ".." are refused.
let private withSegment name (value: string) (send: string -> Task<Result<'T, CompaniesHouseFailure>>) =
    match value with
    | null
    | ""
    | "."
    | ".." -> invalid $"{name} must be a non-empty identifier"
    | _ -> send (Uri.EscapeDataString value)

let private withNumberRegex = Regex @"\A[A-Z0-9]{8}\z"

/// A company number: eight letters and digits, in upper case. A shorter number is refused rather than padded or sent, because its
/// 404 would read as "no such company" when the number has only lost its leading zeros.
let private withNumber (number: string) (send: string -> Task<Result<'T, CompaniesHouseFailure>>) =
    let normal =
        if isNull number then
            ""
        else
            number.Trim().ToUpperInvariant()

    if withNumberRegex.IsMatch normal then
        send normal
    else
        invalid "A company number has eight letters and digits, leading zeros included"

let private withPage (startIndex: int) (itemsPerPage: int) send =
    if startIndex < 0 || itemsPerPage < 1 then
        invalid "start_index must not be negative and items_per_page must be positive"
    else
        send $"start_index={startIndex}&items_per_page={itemsPerPage}"

/// Reads a list to its end. A list ends only when an answer at its own total holds nothing, or is a 404, so an understated total comes to light at
/// the cost of one more call.
module Paging =

    /// What the helpers ask for. The answer can hold fewer, because the docs state no maximum, so the next page starts after the items received.
    [<Literal>]
    let PageSize = 100

    /// The most calls one read of a list makes. The docs state no limit on a list; this one keeps a list that never ends from spending the
    /// application's rate limit.
    [<Literal>]
    let MaxPages = 100

    /// A list as far as it was read: its items in order, the counts and total it stated, and whether every item was read.
    type private Read<'T> =
        {
            Items: 'T list
            Counts: ListCounts voption
            Total: int
            Complete: bool
        }

    /// The items, in order, up to most of them. ValueNone when the first page is a 404.
    let rec private follow
        (fetch: int -> int -> Task<Result<Page<'T> voption, CompaniesHouseFailure>>)
        (maxPages: int)
        (most: int)
        (calls: int)
        (start: int)
        (seen: Set<string>)
        (pages: 'T list list)
        (first: Page<'T> voption)
        : Task<Result<Read<'T> voption, CompaniesHouseFailure>> =
        let read (listed: Page<'T>) complete =
            Ok(
                ValueSome
                    {
                        Items = pages |> List.rev |> List.concat
                        Counts = listed.Counts
                        Total = listed.Total
                        Complete = complete
                    }
            )

        let finished listed = read listed true

        task {
            match first with
            | ValueSome listed when start >= most && start < listed.Total -> return read listed false
            | ValueSome listed when calls >= maxPages -> return Error(ListTooLong(calls, start, listed.Total))
            | _ ->
                match!
                    fetch
                        start
                        (if most - start < PageSize then
                             max 1 (most - start)
                         else
                             PageSize)
                with
                | Error failure -> return Error failure
                | Ok ValueNone ->
                    return
                        match first with
                        | ValueNone -> Ok ValueNone
                        | ValueSome listed when start >= listed.Total -> finished listed
                        | ValueSome listed -> Error(IncompleteList(start, listed.Total))
                | Ok(ValueSome page) ->
                    let listed = first |> ValueOption.defaultValue page

                    // At its total the officer list answers no items and a total of 0, as the sandbox did on 2026-11-08.
                    if first.IsSome && page.Items.IsEmpty && page.Total = 0 && start >= listed.Total then
                        return finished listed
                    elif page.Total <> listed.Total || page.Counts <> listed.Counts then
                        return Error(InconsistentList(start, "the total or counts changed between pages"))
                    else
                        match page.Items with
                        | [] when start < page.Total -> return Error(IncompleteList(start, page.Total))
                        | [] -> return finished page
                        | _ when page.ItemsRaw |> List.forall seen.Contains -> return Error(RepeatedPage start)
                        | _ when page.ItemsRaw |> List.exists seen.Contains ->
                            return Error(InconsistentList(start, "an item moved onto a later page"))
                        | items when start + items.Length > page.Total ->
                            return Error(InconsistentList(start, "there are more items than the total"))
                        | items ->
                            return!
                                follow
                                    fetch
                                    maxPages
                                    most
                                    (calls + 1)
                                    (start + items.Length)
                                    (Set.union seen (Set.ofList page.ItemsRaw))
                                    (items :: pages)
                                    (ValueSome listed)
        }

    /// Every item of the list fetch start size reads, page by page from start_index 0, in at most maxPages calls. ValueNone when the first page is
    /// a 404. Part of a list is never returned as all of it: ending below the total is IncompleteList, a page seen before is RepeatedPage, a total
    /// that changes between pages or is outrun by the items is InconsistentList, and going past maxPages is ListTooLong. Pages aren't one
    /// snapshot, and a change that leaves the total and every item in place can't be seen.
    let allUpToAsync (maxPages: int) (fetch: int -> int -> Task<Result<Page<'T> voption, CompaniesHouseFailure>>) =
        task {
            if maxPages < 1 then
                return Error(InvalidRequest "maxPages must be positive")
            else
                let! result = follow fetch maxPages Int32.MaxValue 0 0 Set.empty [] ValueNone
                return result |> Result.map (ValueOption.map _.Items)
        }

    /// The first items of the list, at most most of them, in at most MaxPages calls, with the total the list states and whether that was all of it.
    /// ValueNone when the first page is a 404. The same checks as allUpToAsync apply to the pages read.
    let firstAsync
        (most: int)
        (fetch: int -> int -> Task<Result<Page<'T> voption, CompaniesHouseFailure>>)
        : Task<Result<('T list * int * bool) voption, CompaniesHouseFailure>> =
        task {
            if most < 1 then
                return Error(InvalidRequest "most must be positive")
            else
                let! result = follow fetch MaxPages most 0 0 Set.empty [] ValueNone

                return
                    result
                    |> Result.map (ValueOption.map (fun read -> read.Items, read.Total, read.Complete))
        }

    /// allUpToAsync with MaxPages.
    let allAsync (fetch: int -> int -> Task<Result<Page<'T> voption, CompaniesHouseFailure>>) =
        allUpToAsync MaxPages fetch

    /// allAsync for a list that states counts, which must add up to its total and agree with its items as isEnded reads them. Otherwise it is
    /// InconsistentList.
    let allCountedAsync
        (isEnded: 'T -> bool)
        (fetch: int -> int -> Task<Result<Page<'T> voption, CompaniesHouseFailure>>)
        : Task<Result<'T list voption, CompaniesHouseFailure>> =
        task {
            match! follow fetch MaxPages Int32.MaxValue 0 0 Set.empty [] ValueNone with
            | Error failure -> return Error failure
            | Ok ValueNone -> return Ok ValueNone
            | Ok(ValueSome { Items = items; Counts = ValueNone }) ->
                return Error(InconsistentList(items.Length, "the list states no counts"))
            | Ok(ValueSome {
                               Items = items
                               Counts = ValueSome counts
                           }) ->
                let ended = items |> List.filter isEnded |> List.length

                return
                    if counts.Active + counts.Ended + counts.Inactive <> items.Length then
                        Error(InconsistentList(items.Length, "the counts don't add up to the total"))
                    elif counts.Ended <> ended then
                        Error(InconsistentList(items.Length, "the count of ended items doesn't match the items"))
                    else
                        Ok(ValueSome items)
        }

module Companies =

    /// The company's profile; ValueNone when Companies House answers its own not-found error, and a 404 of another body ApiError. A number
    /// that can't be one is InvalidRequest.
    let getAsync (client: CompaniesHouseClient) (number: string) (ct: CancellationToken) =
        withNumber number (fun number ->
            getAtWith
                ownNotFound
                client
                (fun () -> Uri(client.Endpoints.Api, $"company/{number}"))
                Parse.companyProfile
                ct)

module Search =

    /// The most items one read of a search asks for. The docs state no limit; forum posts report a 416 at start_index 1000, which is unverified.
    [<Literal>]
    let MostRead = 1000

    /// A search's pages up to MostRead items and Paging.MaxPages calls. Stopping short of the stated total is not a failure, since
    /// the docs give searches no way to read everything: it is Complete = false. So is a 416 after the first page. ValueNone when the first page is a 404.
    let readAsync
        (fetch: int -> int -> Task<Result<Page<'T> voption, CompaniesHouseFailure>>)
        : Task<Result<SearchRead<'T> voption, CompaniesHouseFailure>> =
        let read (pages: 'T list list) (count: int) (total: int) =
            Ok(
                ValueSome
                    {
                        Found = pages |> List.rev |> List.concat
                        StatedTotal = total
                        Complete = count >= total
                    }
            )

        let rec reading
            (calls: int)
            (start: int)
            (seen: Set<string>)
            (pages: 'T list list)
            (stated: int voption)
            : Task<Result<SearchRead<'T> voption, CompaniesHouseFailure>> =
            let stoppedShort () =
                match stated with
                | ValueSome total -> read pages start total
                | ValueNone -> Ok ValueNone

            task {
                if calls >= Paging.MaxPages || start >= MostRead then
                    return stoppedShort ()
                else
                    match! fetch start (min Paging.PageSize (MostRead - start)) with
                    | Error(ApiError(416, _)) when start > 0 -> return stoppedShort ()
                    | Error failure -> return Error failure
                    | Ok ValueNone -> return stoppedShort ()
                    | Ok(ValueSome page) ->
                        match page.Items with
                        | [] -> return read pages start page.Total
                        | _ when page.ItemsRaw |> List.forall seen.Contains -> return Error(RepeatedPage start)
                        | _ when page.ItemsRaw |> List.exists seen.Contains ->
                            return Error(InconsistentList(start, "an item moved onto a later page"))
                        | items when start + items.Length >= page.Total ->
                            return read (items :: pages) (start + items.Length) page.Total
                        | items ->
                            return!
                                reading
                                    (calls + 1)
                                    (start + items.Length)
                                    (Set.union seen (Set.ofList page.ItemsRaw))
                                    (items :: pages)
                                    (ValueSome page.Total)
            }

        reading 0 0 Set.empty [] ValueNone

    /// Companies by name, one page.
    let companiesAsync
        (client: CompaniesHouseClient)
        (query: string)
        (startIndex: int)
        (itemsPerPage: int)
        (ct: CancellationToken)
        =
        if String.IsNullOrWhiteSpace query then
            invalid "A search needs a term"
        else
            withPage startIndex itemsPerPage (fun paging ->
                get
                    client
                    $"search/companies?q={Uri.EscapeDataString query}&{paging}"
                    (Parse.page Parse.companySearchItem)
                    ct)

    /// Companies by name, as many as readAsync reads.
    let companiesReadAsync (client: CompaniesHouseClient) (query: string) (ct: CancellationToken) =
        readAsync (fun start size -> companiesAsync client query start size ct)

    /// Officers by name, one page. Each hit is an officer whose appointments Appointments reads by OfficerSearchItem.OfficerId.
    let officersAsync
        (client: CompaniesHouseClient)
        (query: string)
        (startIndex: int)
        (itemsPerPage: int)
        (ct: CancellationToken)
        =
        if String.IsNullOrWhiteSpace query then
            invalid "A search needs a term"
        else
            withPage startIndex itemsPerPage (fun paging ->
                get
                    client
                    $"search/officers?q={Uri.EscapeDataString query}&{paging}"
                    (Parse.page Parse.officerSearchItem)
                    ct)

    /// Officers by name, as many as readAsync reads.
    let officersReadAsync (client: CompaniesHouseClient) (query: string) (ct: CancellationToken) =
        readAsync (fun start size -> officersAsync client query start size ct)

module Officers =

    /// One page of a company's officers, current and resigned.
    let pageAsync
        (client: CompaniesHouseClient)
        (number: string)
        (startIndex: int)
        (itemsPerPage: int)
        (ct: CancellationToken)
        =
        withNumber number (fun number ->
            withPage startIndex itemsPerPage (fun paging ->
                get client $"company/{number}/officers?{paging}" (Parse.countedPage Parse.companyOfficer) ct))

    let allAsync client number ct =
        Paging.allCountedAsync (fun (officer: CompanyOfficer) -> officer.ResignedOn.IsSome) (fun start size ->
            pageAsync client number start size ct)

/// All appointments of one officer, at any company. Take the id from CompanyOfficer.OfficerId.
module Appointments =

    let pageAsync
        (client: CompaniesHouseClient)
        (officerId: string)
        (startIndex: int)
        (itemsPerPage: int)
        (ct: CancellationToken)
        =
        withSegment "officerId" officerId (fun pid ->
            withPage startIndex itemsPerPage (fun paging ->
                get client $"officers/{pid}/appointments?{paging}" (Parse.page Parse.officerAppointment) ct))

    let allAsync client officerId ct =
        Paging.allAsync (fun start size -> pageAsync client officerId start size ct)

/// The UK establishments of a company registered overseas. The spec gives this list no paging and no total, so one answer is the whole of what
/// can be read, and its being all of them can't be checked.
module UkEstablishments =

    /// ValueNone when none is held at that address.
    let getAsync (client: CompaniesHouseClient) (number: string) (ct: CancellationToken) =
        withNumber number (fun number -> get client $"company/{number}/uk-establishments" Parse.ukEstablishments ct)

/// The register of disqualified officers. A search goes by name, which gives possible matches at most: DisqualificationSearch never says clear.
module DisqualifiedOfficers =

    /// One page of entries by name.
    let searchPageAsync
        (client: CompaniesHouseClient)
        (name: string)
        (startIndex: int)
        (itemsPerPage: int)
        (ct: CancellationToken)
        =
        if String.IsNullOrWhiteSpace name then
            invalid "A search needs a term"
        else
            withPage startIndex itemsPerPage (fun paging ->
                get
                    client
                    $"search/disqualified-officers?q={Uri.EscapeDataString name}&{paging}"
                    (Parse.page Parse.disqualifiedOfficerSearchItem)
                    ct)

    /// Entries by name, as many as Search.readAsync reads. A failed search is an Error, which says nothing about the officer.
    let searchAsync (client: CompaniesHouseClient) (name: string) (ct: CancellationToken) =
        task {
            let! read =
                Search.readAsync (fun start size -> searchPageAsync client name start size ct)

            return
                read
                |> Result.map (fun answer ->
                    match answer with
                    | ValueNone ->
                        DisqualificationSearch.Inconclusive
                            "The search answered 404, which isn't a statement that nothing matches"
                    | ValueSome { Found = []; Complete = true } -> DisqualificationSearch.NoNameMatch
                    | ValueSome { Found = []; StatedTotal = total } ->
                        DisqualificationSearch.Inconclusive $"The search states {total} entries and gave none"
                    | ValueSome found -> DisqualificationSearch.PossibleMatches found)
        }

    /// An entry's record; ValueNone when none is held at that address, which says nothing about the person.
    let detailsAsync (client: CompaniesHouseClient) (officerId: DisqualifiedOfficerId) (ct: CancellationToken) =
        let kind, pid =
            match officerId with
            | DisqualifiedOfficerId.Natural pid -> "natural", pid
            | DisqualifiedOfficerId.Corporate pid -> "corporate", pid

        withSegment "officerId" pid (fun pid ->
            get client $"disqualified-officers/{kind}/{pid}" Parse.disqualifiedOfficer ct)

/// Persons with significant control. The spec marks register_view as required although it says it defaults to false, so false is sent.
module Psc =

    let pageAsync
        (client: CompaniesHouseClient)
        (number: string)
        (startIndex: int)
        (itemsPerPage: int)
        (ct: CancellationToken)
        =
        withNumber number (fun number ->
            withPage startIndex itemsPerPage (fun paging ->
                get
                    client
                    $"company/{number}/persons-with-significant-control?{paging}&register_view=false"
                    (Parse.countedPage Parse.psc)
                    ct))

    /// A ceased_on date or the ceased flag ends a person's control.
    let isActive (psc: Psc) = psc.CeasedOn.IsNone && not psc.Ceased

    let allAsync client number ct =
        Paging.allCountedAsync (isActive >> not) (fun start size -> pageAsync client number start size ct)

/// What a company states about its persons with significant control.
module PscStatements =

    let pageAsync
        (client: CompaniesHouseClient)
        (number: string)
        (startIndex: int)
        (itemsPerPage: int)
        (ct: CancellationToken)
        =
        withNumber number (fun number ->
            withPage startIndex itemsPerPage (fun paging ->
                get
                    client
                    $"company/{number}/persons-with-significant-control-statements?{paging}&register_view=false"
                    (Parse.countedPage Parse.pscStatement)
                    ct))

    let isActive (statement: PscStatement) = statement.CeasedOn.IsNone

    let allAsync client number ct =
        Paging.allCountedAsync (isActive >> not) (fun start size -> pageAsync client number start size ct)

module Charges =

    let pageAsync
        (client: CompaniesHouseClient)
        (number: string)
        (startIndex: int)
        (itemsPerPage: int)
        (ct: CancellationToken)
        =
        withNumber number (fun number ->
            withPage startIndex itemsPerPage (fun paging ->
                get client $"company/{number}/charges?{paging}" (Parse.page Parse.charge) ct))

    let allAsync client number ct =
        Paging.allAsync (fun start size -> pageAsync client number start size ct)

module Insolvency =

    /// The company's insolvency cases; ValueNone when none is held at that address.
    let getAsync (client: CompaniesHouseClient) (number: string) (ct: CancellationToken) =
        withNumber number (fun number -> get client $"company/{number}/insolvency" Parse.insolvency ct)

module FilingHistory =

    /// One page of a company's filings; categories, such as ["incorporation"; "resolution"], narrow it, and [] asks for all.
    let pageAsync
        (client: CompaniesHouseClient)
        (number: string)
        (categories: string list)
        (startIndex: int)
        (itemsPerPage: int)
        (ct: CancellationToken)
        =
        let filter =
            match categories with
            | [] -> ""
            | _ -> "&category=" + Uri.EscapeDataString(String.concat "," categories)

        withNumber number (fun number ->
            withPage startIndex itemsPerPage (fun paging ->
                get client $"company/{number}/filing-history?{paging}{filter}" (Parse.page Parse.filingHistoryItem) ct))

    let allAsync client number categories ct =
        Paging.allAsync (fun start size -> pageAsync client number categories start size ct)

    /// The most filings recentAsync reads: ten calls. BP P.L.C. had 15,553 filings on 2026-11-08; the figure itself is a choice made without data.
    [<Literal>]
    let MostRead = 1000

    /// The newest filings, at most MostRead of them, with the total the list states and whether that was all of them. The list is newest first.
    let recentAsync client number categories ct =
        Paging.firstAsync MostRead (fun start size -> pageAsync client number categories start size ct)

/// Filed documents. The Document API answers a content request with a 302 to a signed URL on another host. The first hop carries the
/// key; the second goes through a client with no key to send. Nothing else follows redirects.
module Documents =

    let private isRedirect status =
        List.contains status [ 301; 302; 303; 307; 308 ]

    /// The redirect's address, if it isn't weaker than the endpoint it came from: https, or the document endpoint's own scheme.
    let private target (client: CompaniesHouseClient) (response: HttpResponseMessage) (requested: Uri) =
        let status = int response.StatusCode

        match response.Headers.Location with
        | null -> Error(RedirectRefused(status, "the answer has no Location"))
        | location ->
            let absolute = Uri(requested, location)

            if
                absolute.Scheme = Uri.UriSchemeHttps
                || absolute.Scheme = client.Endpoints.Document.Scheme
            then
                Ok absolute
            else
                Error(RedirectRefused(status, $"the Location on {absolute.Host} isn't https"))

    let metadataAsync (client: CompaniesHouseClient) (documentId: string) (ct: CancellationToken) =
        withSegment "documentId" documentId (fun pid ->
            getAt client (fun () -> Uri(client.Endpoints.Document, $"document/{pid}")) Parse.documentMetadata ct)

    [<return: Struct>]
    let inline private (|IsRedirect|_|) input =
        if isRedirect input then ValueSome input else ValueNone

    /// Where the document can be read as contentType, one of its metadata's ContentTypes. The URL needs no key and expires. ValueNone for a 404.
    /// An answer without a redirect is InvalidResponse.
    let locationAsync
        (client: CompaniesHouseClient)
        (documentId: string)
        (contentType: string)
        (ct: CancellationToken)
        =
        withSegment "documentId" documentId (fun pid ->
            match MediaTypeHeaderValue.TryParse contentType with
            | false, _ -> invalid "contentType must be a media type, such as application/pdf"
            | true, _ ->
                guarded ct (fun () ->
                    task {
                        let uri = Uri(client.Endpoints.Document, $"document/{pid}/content")

                        let prepare () =
                            let request = new HttpRequestMessage(HttpMethod.Get, uri)
                            request.Headers.Accept.ParseAdd contentType
                            request

                        match! sendKeyed client prepare ct with
                        | Error failure -> return Error failure
                        | Ok answer ->
                            use response = answer

                            match int response.StatusCode with
                            | IsRedirect _ ->
                                return
                                    target client response uri
                                    |> Result.map (fun at -> ValueSome { Uri = at; ContentType = contentType })
                            | 404 -> return Ok ValueNone
                            | _ ->
                                let! body = response.Content.ReadAsByteArrayAsync ct

                                return
                                    if response.IsSuccessStatusCode then
                                        Error(unreadable (int response.StatusCode) body "The answer has no redirect")
                                    else
                                        Error(refusal response body)
                    }))

    /// Reads a signed URL. Any failure is the document host's, so a 404 here is ApiError, not ValueNone, and its body is left out.
    let private fetch (client: CompaniesHouseClient) (location: DocumentLocation) (ct: CancellationToken) =
        guarded ct (fun () ->
            task {
                use request = new HttpRequestMessage(HttpMethod.Get, location.Uri)
                use! response = client.Keyless.SendAsync(request, ct)
                let status = int response.StatusCode

                if response.IsSuccessStatusCode then
                    let! content = response.Content.ReadAsByteArrayAsync ct

                    let contentType =
                        match response.Content.Headers.ContentType with
                        | null -> location.ContentType
                        | answered -> answered.ToString()

                    return
                        Ok
                            {
                                ContentType = contentType
                                Content = content
                            }
                elif status >= 300 && status < 400 then
                    return Error(RedirectRefused(status, "the document host redirected again"))
                else
                    return Error(ApiError(status, ""))
            })

    /// The document's bytes, read into memory; for a large one, read locationAsync's Uri as a stream. ValueNone when the Document API has no
    /// such document.
    let contentAsync (client: CompaniesHouseClient) (documentId: string) (contentType: string) (ct: CancellationToken) =
        task {
            match! locationAsync client documentId contentType ct with
            | Error failure -> return Error failure
            | Ok ValueNone -> return Ok ValueNone
            | Ok(ValueSome location) ->
                let! content = fetch client location ct
                return content |> Result.map ValueSome
        }
