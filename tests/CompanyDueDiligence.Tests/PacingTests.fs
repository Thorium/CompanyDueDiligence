/// Pacing and the bounded retry of a 429, on a clock that moves only when a call waits.
module PacingTests

open System
open System.Threading
open System.Threading.Tasks
open Xunit
open CompanyDueDiligence.CompaniesHouse
open Fake

let private company =
    """{"company_number":"00000006","company_name":"MARINE AND GENERAL","company_status":"active"}"""

let private getCompany client =
    Api.Companies.getAsync client "00000006" ct

/// Answers from the list in turn, then the last one for good.
let private inTurn (answers: Answer list) =
    let left = ref answers

    fun (_: Call) ->
        match left.Value with
        | [ last ] -> last
        | next :: rest ->
            left.Value <- rest
            next
        | [] -> failwith "No answers"

let private limited headers =
    { answer 429 "" with Headers = headers }

[<Fact>]
let ``a 429 waits what Retry-After says, then asks again`` () : Task =
    task {
        let clock = FakeClock()

        use handler =
            new FakeHandler(inTurn [ limited [ "Retry-After", "42" ]; ok company ])

        let! result = getCompany (pacedWith clock Pacing.documented handler)
        Assert.Equal("MARINE AND GENERAL", (expectOk result).Value.CompanyName)
        Assert.Equal<TimeSpan list>([ TimeSpan.FromSeconds 42. ], clock.Waits)
        Assert.Equal(2, handler.Calls.Length)
    }

[<Fact>]
let ``a 429 waits for the reset time when that is all it says`` () : Task =
    task {
        let clock = FakeClock()
        let reset = clock.Now.AddSeconds 90.

        use handler =
            new FakeHandler(
                inTurn
                    [
                        limited [ "X-Ratelimit-Reset", string (reset.ToUnixTimeSeconds()) ]
                        ok company
                    ]
            )

        let! result = getCompany (pacedWith clock Pacing.documented handler)
        expectOk result |> ignore
        Assert.Equal<TimeSpan list>([ TimeSpan.FromSeconds 90. ], clock.Waits)
    }

[<Fact>]
let ``a 429 with no headers waits the documented period, and the retries are bounded`` () : Task =
    task {
        let clock = FakeClock()
        use handler = new FakeHandler(fun _ -> answer 429 "")

        match! getCompany (pacedWith clock Pacing.documented handler) with
        | Error(RateLimited _) ->
            Assert.Equal(1 + Pacing.documented.Retries, handler.Calls.Length)
            Assert.Equal<TimeSpan list>([ TimeSpan.FromMinutes 5.; TimeSpan.FromMinutes 5. ], clock.Waits)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a 429 with unusable headers degrades to the documented period`` () : Task =
    task {
        let clock = FakeClock()

        use handler =
            new FakeHandler(
                inTurn
                    [
                        limited
                            [
                                "Retry-After", "soon"
                                "X-Ratelimit-Reset", "tomorrow"
                                "X-Ratelimit-Remain", "none"
                            ]
                        ok company
                    ]
            )

        let! result = getCompany (pacedWith clock Pacing.documented handler)
        expectOk result |> ignore
        Assert.Equal<TimeSpan list>([ TimeSpan.FromMinutes 5. ], clock.Waits)
    }

[<Fact>]
let ``a reset already past still waits a moment before asking again`` () : Task =
    task {
        let clock = FakeClock()
        let reset = clock.Now.AddSeconds -30.

        use handler =
            new FakeHandler(
                inTurn
                    [
                        limited [ "X-Ratelimit-Reset", string (reset.ToUnixTimeSeconds()) ]
                        ok company
                    ]
            )

        let! result = getCompany (pacedWith clock Pacing.documented handler)
        expectOk result |> ignore
        Assert.Equal<TimeSpan list>([ TimeSpan.FromSeconds 1. ], clock.Waits)
    }

[<Fact>]
let ``a wait past the longest isn't made, and the call is RateLimited with what it said`` () : Task =
    task {
        let clock = FakeClock()
        use handler = new FakeHandler(fun _ -> limited [ "Retry-After", "3600" ])

        match! getCompany (pacedWith clock Pacing.documented handler) with
        | Error(RateLimited limit) ->
            Assert.Equal(ValueSome(TimeSpan.FromHours 1.), limit.RetryAfter)
            Assert.Single handler.Calls |> ignore
            Assert.Empty clock.Waits
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``no retries asks once`` () : Task =
    task {
        let clock = FakeClock()
        use handler = new FakeHandler(fun _ -> answer 429 "")

        let pacing = { Pacing.documented with Retries = 0 }

        match! getCompany (pacedWith clock pacing handler) with
        | Error(RateLimited _) ->
            Assert.Single handler.Calls |> ignore
            Assert.Empty clock.Waits
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a 429 holds the next call as well, and that call isn't sent early`` () : Task =
    task {
        let clock = FakeClock()

        use handler =
            new FakeHandler(inTurn [ limited [ "Retry-After", "42" ]; ok company ])

        let pacing = { Pacing.documented with Retries = 0 }

        let client = pacedWith clock pacing handler
        let! first = getCompany client
        Assert.True(Result.isError first)
        let! second = getCompany client
        expectOk second |> ignore
        Assert.Equal<TimeSpan list>([ TimeSpan.FromSeconds 42. ], clock.Waits)
    }

[<Fact>]
let ``a 429 part way through a list keeps the pages read`` () : Task =
    task {
        let clock = FakeClock()

        let item (n: int) =
            $"""{{"name":"OFFICER {n}","officer_role":"director"}}"""

        let limitedOnce = ref true

        use handler =
            new FakeHandler(fun call ->
                match startIndexOf call with
                | 0 -> ok (officersJson 2 0 [ item 0 ])
                | 1 when limitedOnce.Value ->
                    limitedOnce.Value <- false
                    limited [ "Retry-After", "7" ]
                | 1 -> ok (officersJson 2 0 [ item 1 ])
                | _ -> ok (officersJson 2 0 []))

        let! result =
            Api.Officers.allAsync (pacedWith clock Pacing.documented handler) "00000006" ct

        Assert.Equal(2, (expectOk result).Value.Length)
        Assert.Equal<int list>([ 0; 1; 1; 2 ], handler.Calls |> List.map startIndexOf)
        Assert.Equal<TimeSpan list>([ TimeSpan.FromSeconds 7. ], clock.Waits)
    }

[<Fact>]
let ``no more than the limit go in any window, the next waiting for the oldest to leave it`` () : Task =
    task {
        let clock = FakeClock()
        use handler = new FakeHandler(fun _ -> ok company)

        let pacing =
            { Pacing.documented with
                Calls = 3
                Window = TimeSpan.FromMinutes 1.
            }

        let client = pacedWith clock pacing handler

        for _ in 1..4 do
            let! result = getCompany client
            expectOk result |> ignore

        Assert.Equal(4, handler.Calls.Length)
        Assert.Equal<TimeSpan list>([ TimeSpan.FromMinutes 1. ], clock.Waits)
    }

[<Fact>]
let ``the documented pacing lets 600 go at once and holds the 601st`` () : Task =
    task {
        let clock = FakeClock()
        use handler = new FakeHandler(fun _ -> ok company)
        let client = pacedWith clock Pacing.documented handler

        for _ in 1..600 do
            let! result = getCompany client
            expectOk result |> ignore

        Assert.Empty clock.Waits
        let! last = getCompany client
        expectOk last |> ignore
        Assert.Equal<TimeSpan list>([ TimeSpan.FromMinutes 5. ], clock.Waits)
    }

[<Fact>]
let ``a turn further off than the longest wait is RateLimited and nothing is sent`` () : Task =
    task {
        let clock = FakeClock()
        use handler = new FakeHandler(fun _ -> ok company)

        let pacing =
            { Pacing.documented with
                Calls = 1
                Window = TimeSpan.FromMinutes 1.
                LongestWait = TimeSpan.FromSeconds 10.
            }

        let client = pacedWith clock pacing handler
        let! first = getCompany client
        expectOk first |> ignore

        match! getCompany client with
        | Error(RateLimited limit) ->
            Assert.Equal(ValueSome(TimeSpan.FromMinutes 1.), limit.RetryAfter)
            Assert.Single handler.Calls |> ignore
            Assert.Empty clock.Waits
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``an answer saying none are left holds the next call until the reset`` () : Task =
    task {
        let clock = FakeClock()
        let reset = clock.Now.AddSeconds 60.

        let spent =
            { ok company with
                Headers =
                    [
                        "X-Ratelimit-Remain", "0"
                        "X-Ratelimit-Reset", string (reset.ToUnixTimeSeconds())
                    ]
            }

        use handler = new FakeHandler(inTurn [ spent; ok company ])
        let client = pacedWith clock Pacing.documented handler
        let! first = getCompany client
        expectOk first |> ignore
        Assert.Empty clock.Waits
        let! second = getCompany client
        expectOk second |> ignore
        Assert.Equal<TimeSpan list>([ TimeSpan.FromSeconds 60. ], clock.Waits)
    }

[<Fact>]
let ``a reset absurdly far off holds no longer than the window`` () : Task =
    task {
        let clock = FakeClock()
        let reset = clock.Now.AddYears 50

        let spent =
            { ok company with
                Headers =
                    [
                        "X-Ratelimit-Remain", "0"
                        "X-Ratelimit-Reset", string (reset.ToUnixTimeSeconds())
                    ]
            }

        use handler = new FakeHandler(inTurn [ spent; ok company ])
        let client = pacedWith clock Pacing.documented handler
        let! first = getCompany client
        expectOk first |> ignore
        let! second = getCompany client
        expectOk second |> ignore
        Assert.Equal<TimeSpan list>([ TimeSpan.FromMinutes 5. ], clock.Waits)
    }

[<Theory>]
[<InlineData("X-Ratelimit-Remain", "5")>]
[<InlineData("X-Ratelimit-Remain", "many")>]
[<InlineData("X-Unrelated", "0")>]
let ``an answer with some left, or no usable count, holds nothing`` (name: string, value: string) : Task =
    task {
        let clock = FakeClock()
        let reset = clock.Now.AddSeconds 60.

        let plain =
            { ok company with
                Headers = [ name, value; "X-Ratelimit-Reset", string (reset.ToUnixTimeSeconds()) ]
            }

        use handler = new FakeHandler(inTurn [ plain; ok company ])
        let client = pacedWith clock Pacing.documented handler
        let! first = getCompany client
        expectOk first |> ignore
        let! second = getCompany client
        expectOk second |> ignore
        Assert.Empty clock.Waits
    }

[<Fact>]
let ``none left without a reset holds nothing`` () : Task =
    task {
        let clock = FakeClock()

        let spent =
            { ok company with
                Headers = [ "X-Ratelimit-Remain", "0" ]
            }

        use handler = new FakeHandler(inTurn [ spent; ok company ])
        let client = pacedWith clock Pacing.documented handler
        let! first = getCompany client
        expectOk first |> ignore
        let! second = getCompany client
        expectOk second |> ignore
        Assert.Empty clock.Waits
    }

[<Fact>]
let ``cancelling during a wait throws`` () : Task =
    task {
        use cancel = new CancellationTokenSource(TimeSpan.FromMilliseconds 100.)
        use handler = new FakeHandler(fun _ -> limited [ "Retry-After", "60" ])

        let client =
            CompaniesHouseClient.createPaced credentials Endpoints.live Pacing.documented handler

        let! _ =
            Assert.ThrowsAnyAsync<OperationCanceledException>(fun () ->
                Api.Companies.getAsync client "00000006" cancel.Token :> Task)

        ()
    }

[<Fact>]
let ``the Document API is paced and retried too, and a signed URL isn't`` () : Task =
    task {
        let clock = FakeClock()
        let hosts = ref []

        let pacing =
            { Pacing.documented with
                Calls = 1
                Window = TimeSpan.FromMinutes 1.
                Retries = 1
            }

        use handler =
            new FakeHandler(fun call ->
                hosts.Value <- call.Host :: hosts.Value

                if call.Host = "signed.example" then
                    ok "bytes"
                elif hosts.Value.Length = 1 then
                    limited [ "Retry-After", "5" ]
                else
                    {
                        Status = 302
                        Body = ""
                        Headers = [ "Location", "https://signed.example/doc?signature=x" ]
                    })

        let! result =
            Api.Documents.contentAsync (pacedWith clock pacing handler) "doc-1" "application/pdf" ct

        Assert.True(Result.isOk result)
        Assert.Equal(3, handler.Calls.Length)
        Assert.Equal<TimeSpan list>([ TimeSpan.FromMinutes 1. ], clock.Waits)
    }

[<Theory>]
[<InlineData(0, 60., 2, 60.)>]
[<InlineData(1, 0., 2, 60.)>]
[<InlineData(1, 60., -1, 60.)>]
[<InlineData(1, 60., 2, -1.)>]
let ``pacing that can't work is refused at creation`` (calls: int, window: float, retries: int, longest: float) =
    use handler = new FakeHandler(fun _ -> ok company)

    let pacing =
        {
            Calls = calls
            Window = TimeSpan.FromSeconds window
            Retries = retries
            LongestWait = TimeSpan.FromSeconds longest
        }

    Assert.Throws<ArgumentOutOfRangeException>(fun () ->
        CompaniesHouseClient.createPaced credentials Endpoints.live pacing handler
        |> ignore)
    |> ignore

    Assert.Throws<ArgumentOutOfRangeException>(fun () ->
        CompaniesHouseClient.Create(credentials, Endpoints.live, TimeSpan.FromSeconds 20., pacing)
        |> ignore)
    |> ignore

[<Fact>]
let ``the documented pacing is the guide's limit`` () =
    Assert.Equal(600, Pacing.documented.Calls)
    Assert.Equal(TimeSpan.FromMinutes 5., Pacing.documented.Window)
