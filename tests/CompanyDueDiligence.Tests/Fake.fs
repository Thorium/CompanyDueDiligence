module Fake

open System
open System.Net
open System.Net.Http
open System.Text
open System.Threading
open System.Threading.Tasks
open CompanyDueDiligence.CompaniesHouse

type Call =
    {
        Method: string
        /// Absolute URI as sent.
        Uri: string
        Host: string
        /// Path and query as sent.
        Target: string
        Headers: Map<string, string>
    }

type Answer =
    {
        Status: int
        Body: string
        Headers: (string * string) list
    }

let answer status body =
    {
        Status = status
        Body = body
        Headers = []
    }

let ok body = answer 200 body

let notFound =
    answer 404 """{"errors":[{"error":"company-profile-not-found","type":"ch:service"}]}"""

/// Answers each request with respond, and records the calls, including what a request carries before it is answered.
type FakeHandler(respond: Call -> Answer) =
    inherit HttpMessageHandler()
    let calls = Collections.Concurrent.ConcurrentQueue<Call>()

    member _.Calls = List.ofSeq calls

    override _.SendAsync(request, ct) =
        task {
            ct.ThrowIfCancellationRequested()

            let call =
                {
                    Method = request.Method.Method
                    Uri = request.RequestUri.AbsoluteUri
                    Host = request.RequestUri.Host
                    Target = request.RequestUri.PathAndQuery
                    Headers =
                        request.Headers
                        |> Seq.map (fun h -> h.Key, String.Join(",", h.Value))
                        |> Map.ofSeq
                }

            calls.Enqueue call
            let reply = respond call

            let response =
                new HttpResponseMessage(
                    enum<HttpStatusCode> reply.Status,
                    Content = (new StringContent(reply.Body, Encoding.UTF8, "application/json"))
                )

            for name, value in reply.Headers do
                response.Headers.TryAddWithoutValidation(name, value) |> ignore

            return response
        }

/// Never answers until cancelled.
type HangingHandler() =
    inherit HttpMessageHandler()

    override _.SendAsync(_, ct) =
        task {
            do! Task.Delay(Timeout.Infinite, ct)
            return null
        }

[<Literal>]
let Key = "the-test-key-value"

let internal credentials = { ApiKey = Key }

/// A clock that any wait moves on at once, noting the wait, so that pacing costs a test no time.
type FakeClock() =
    let mutable now = DateTimeOffset(2026, 11, 6, 12, 0, 0, TimeSpan.Zero)
    let waits = Collections.Concurrent.ConcurrentQueue<TimeSpan>()

    member _.Now = now
    member _.Waits = List.ofSeq waits

    member _.Delay (wait: TimeSpan) (_: CancellationToken) : Task =
        waits.Enqueue wait
        now <- now + wait
        Task.CompletedTask

let internal pacedWith (clock: FakeClock) (pacing: Pacing) (handler: HttpMessageHandler) =
    let pacer = Pacer(pacing, (fun () -> clock.Now), clock.Delay)
    CompaniesHouseClient(credentials, Endpoints.live, handler, ValueNone, pacer)

let internal clientWith (handler: HttpMessageHandler) =
    pacedWith (FakeClock()) Pacing.documented handler

let ct = CancellationToken.None

let internal expectOk result =
    result |> Result.defaultWith (CompaniesHouseFailure.describe >> failwith)

/// A list answer holding items, with a total under the name the list uses.
let listJson (totalName: string) (total: int) (items: string list) =
    let body = String.concat "," items
    $"""{{"{totalName}":{total},"items_per_page":{items.Length},"start_index":0,"items":[{body}]}}"""

/// A list answer with the counts officers, PSCs and statements state, under the name the list uses for those ended.
let countedJson (endedName: string) (total: int) (ended: int) (items: string list) =
    let body = String.concat "," items
    $"""{{"total_results":{total},"active_count":{total - ended},"{endedName}":{ended},"items_per_page":{items.Length},"start_index":0,"items":[{body}]}}"""

let officersJson = countedJson "resigned_count"
let pscsJson = countedJson "ceased_count"

let startIndexOf (call: Call) =
    let query = Web.HttpUtility.ParseQueryString(Uri(call.Uri).Query)
    int query["start_index"]
