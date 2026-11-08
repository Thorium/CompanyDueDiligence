module ClientTests

open System
open System.IO
open System.Text.Json
open System.Threading
open System.Threading.Tasks
open Xunit
open CompanyDueDiligence.CompaniesHouse
open Fake

let private company =
    """{"company_number":"00000006","company_name":"MARINE AND GENERAL","company_status":"active"}"""

[<Fact>]
let ``the key goes as the guide's basic header, with its colon`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok company)

        let client =
            CompaniesHouseClient.create { ApiKey = "my_api_key" } Endpoints.live handler

        let! _ = Api.Companies.getAsync client "00000006" ct
        Assert.Equal("Basic bXlfYXBpX2tleTo=", handler.Calls.Head.Headers["Authorization"])
    }

/// Calls, the addresses they go to and the spec's path template for each.
let private routes: (string * string * (CompaniesHouseClient -> Task<string>)) list =
    let done' (result: Task<Result<'T, CompaniesHouseFailure>>) =
        task { let! r = result in return $"%A{r}" }

    [
        "/company/{companyNumber}",
        "https://api.company-information.service.gov.uk/company/00000006",
        fun client -> done' (Api.Companies.getAsync client "00000006" ct)

        "/search/companies",
        "https://api.company-information.service.gov.uk/search/companies?q=a%26b%20c&start_index=5&items_per_page=2",
        fun client -> done' (Api.Search.companiesAsync client "a&b c" 5 2 ct)

        "/company/{company_number}/officers",
        "https://api.company-information.service.gov.uk/company/00000006/officers?start_index=5&items_per_page=2",
        fun client -> done' (Api.Officers.pageAsync client "00000006" 5 2 ct)

        "/company/{company_number}/persons-with-significant-control",
        "https://api.company-information.service.gov.uk/company/00000006/persons-with-significant-control?start_index=5&items_per_page=2&register_view=false",
        fun client -> done' (Api.Psc.pageAsync client "00000006" 5 2 ct)

        "/company/{company_number}/persons-with-significant-control-statements",
        "https://api.company-information.service.gov.uk/company/00000006/persons-with-significant-control-statements?start_index=5&items_per_page=2&register_view=false",
        fun client -> done' (Api.PscStatements.pageAsync client "00000006" 5 2 ct)

        "/company/{company_number}/charges",
        "https://api.company-information.service.gov.uk/company/00000006/charges?start_index=5&items_per_page=2",
        fun client -> done' (Api.Charges.pageAsync client "00000006" 5 2 ct)

        "/company/{company_number}/insolvency",
        "https://api.company-information.service.gov.uk/company/00000006/insolvency",
        fun client -> done' (Api.Insolvency.getAsync client "00000006" ct)

        "/company/{company_number}/filing-history",
        "https://api.company-information.service.gov.uk/company/00000006/filing-history?start_index=5&items_per_page=2&category=incorporation%2Cresolution",
        fun client -> done' (Api.FilingHistory.pageAsync client "00000006" [ "incorporation"; "resolution" ] 5 2 ct)

        "/officers/{officer_id}/appointments",
        "https://api.company-information.service.gov.uk/officers/abc/appointments?start_index=5&items_per_page=2",
        fun client -> done' (Api.Appointments.pageAsync client "abc" 5 2 ct)

        "/company/{company_number}/uk-establishments",
        "https://api.company-information.service.gov.uk/company/00000006/uk-establishments",
        fun client -> done' (Api.UkEstablishments.getAsync client "00000006" ct)

        "/search/disqualified-officers",
        "https://api.company-information.service.gov.uk/search/disqualified-officers?q=a%26b%20c&start_index=5&items_per_page=2",
        fun client -> done' (Api.DisqualifiedOfficers.searchPageAsync client "a&b c" 5 2 ct)

        "/disqualified-officers/natural/{officer_id}",
        "https://api.company-information.service.gov.uk/disqualified-officers/natural/abc",
        fun client -> done' (Api.DisqualifiedOfficers.detailsAsync client (DisqualifiedOfficerId.Natural "abc") ct)

        "/disqualified-officers/corporate/{officer_id}",
        "https://api.company-information.service.gov.uk/disqualified-officers/corporate/abc",
        fun client -> done' (Api.DisqualifiedOfficers.detailsAsync client (DisqualifiedOfficerId.Corporate "abc") ct)
    ]

[<Fact>]
let ``each call goes to its address, a path the bundled spec lists`` () : Task =
    task {
        use document =
            JsonDocument.Parse(
                File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "../../Schemas/public-data/swagger.json"))
            )

        let paths =
            document.RootElement.GetProperty("paths").EnumerateObject()
            |> Seq.map _.Name
            |> Set.ofSeq

        for template, expected, call in routes do
            use handler = new FakeHandler(fun _ -> notFound)
            let! printed = call (clientWith handler)
            Assert.Contains("ValueNone", printed)
            Assert.Contains(template, paths)
            Assert.Equal(expected, handler.Calls.Head.Uri)
    }

[<Theory>]
[<InlineData("123456")>]
[<InlineData("")>]
[<InlineData("0000000/")>]
[<InlineData("../00000")>]
[<InlineData("000000061")>]
[<InlineData(null)>]
let ``a value that can't be a company number is InvalidRequest and nothing is sent`` (number: string) : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok company)

        match! Api.Companies.getAsync (clientWith handler) number ct with
        | Error(InvalidRequest _) -> Assert.Empty handler.Calls
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a company number is sent in upper case`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok company)
        let! _ = Api.Companies.getAsync (clientWith handler) " sc123456 " ct
        Assert.Equal("/company/SC123456", handler.Calls.Head.Target)
    }

[<Fact>]
let ``a company Companies House doesn't hold is ValueNone, not a failure`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> notFound)

        match! Api.Companies.getAsync (clientWith handler) "00000006" ct with
        | Ok ValueNone -> ()
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a held company is ValueSome`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok company)
        let! result = Api.Companies.getAsync (clientWith handler) "00000006" ct
        Assert.Equal("MARINE AND GENERAL", (expectOk result).Value.CompanyName)
    }

[<Fact>]
let ``a 401 is KeyRefused and a 500 is an ApiError to retry`` () : Task =
    task {
        use refused = new FakeHandler(fun _ -> answer 401 "{}")

        match! Api.Companies.getAsync (clientWith refused) "00000006" ct with
        | Error(KeyRefused _ as failure) -> Assert.Equal(FailureAction.FixRequest, CompaniesHouseFailure.action failure)
        | other -> failwith $"%A{other}"

        use broken = new FakeHandler(fun _ -> answer 500 "oops")

        match! Api.Companies.getAsync (clientWith broken) "00000006" ct with
        | Error(ApiError(500, "oops") as failure) ->
            Assert.Equal(FailureAction.RetryLater, CompaniesHouseFailure.action failure)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a 429 is RateLimited, with what its headers say`` () : Task =
    task {
        let reset = DateTimeOffset(2026, 11, 6, 12, 0, 0, TimeSpan.Zero)

        let headers =
            [
                "Retry-After", "42"
                "X-Ratelimit-Reset", string (reset.ToUnixTimeSeconds())
                "X-Ratelimit-Limit", "600"
                "X-Ratelimit-Window", "5m"
            ]

        use handler = new FakeHandler(fun _ -> { answer 429 "" with Headers = headers })

        match! Api.Companies.getAsync (clientWith handler) "00000006" ct with
        | Error(RateLimited limit as failure) ->
            Assert.Equal(ValueSome(TimeSpan.FromSeconds 42.), limit.RetryAfter)
            Assert.Equal(ValueSome reset, limit.ResetsAt)
            Assert.Equal(ValueSome 600, limit.Limit)
            Assert.Equal(ValueSome "5m", limit.Window)
            Assert.Equal(FailureAction.RetryLater, CompaniesHouseFailure.action failure)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a 429 without headers waits the documented period`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> answer 429 "")

        match! Api.Companies.getAsync (clientWith handler) "00000006" ct with
        | Error(RateLimited limit) -> Assert.Equal(TimeSpan.FromMinutes 5., RateLimit.wait DateTimeOffset.UtcNow limit)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a 429's unusable headers are ignored`` () : Task =
    task {
        let headers =
            [
                "Retry-After", "soon"
                "X-Ratelimit-Reset", "99999999999999999"
                "X-Ratelimit-Limit", "many"
            ]

        use handler = new FakeHandler(fun _ -> { answer 429 "" with Headers = headers })

        match! Api.Companies.getAsync (clientWith handler) "00000006" ct with
        | Error(RateLimited limit) ->
            Assert.Equal(ValueNone, limit.RetryAfter)
            Assert.Equal(ValueNone, limit.ResetsAt)
            Assert.Equal(ValueNone, limit.Limit)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``wait prefers the delay, then the reset time`` () =
    let now = DateTimeOffset(2026, 11, 6, 12, 0, 0, TimeSpan.Zero)

    let none = RateLimit.none

    Assert.Equal(
        TimeSpan.FromSeconds 7.,
        RateLimit.wait
            now
            { none with
                RetryAfter = ValueSome(TimeSpan.FromSeconds 7.)
                ResetsAt = ValueSome(now.AddHours 1.)
            }
    )

    Assert.Equal(
        TimeSpan.FromMinutes 2.,
        RateLimit.wait
            now
            { none with
                ResetsAt = ValueSome(now.AddMinutes 2.)
            }
    )

    Assert.Equal(
        TimeSpan.Zero,
        RateLimit.wait
            now
            { none with
                ResetsAt = ValueSome(now.AddMinutes -2.)
            }
    )

[<Theory>]
[<InlineData("<html>Bad gateway, Jane Doe</html>")>]
[<InlineData("")>]
[<InlineData("null")>]
[<InlineData("[]")>]
[<InlineData("""{"company_number":"00000006"}""")>]
[<InlineData("""{"company_number":"00000006","company_name":"A","has_insolvency_history":"no"}""")>]
let ``a success answer that can't be read is InvalidResponse, holding the body`` (body: string) : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok body)

        match! Api.Companies.getAsync (clientWith handler) "00000006" ct with
        | Error(InvalidResponse answer as failure) ->
            Assert.Equal(200, answer.HttpStatus)
            Assert.Equal(body, answer.Body)
            Assert.Equal(FailureAction.FixRequest, CompaniesHouseFailure.action failure)
        | other -> failwith $"%A{other}"
    }

[<Theory; InlineData("""{"Jane Doe of 1 High Street":nul}"""); InlineData("""Jane Doe of 1 High Street""")>]
let ``an unreadable answer's text stays out of describe and printing`` (body: string) : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok body)

        match! Api.Companies.getAsync (clientWith handler) "00000006" ct with
        | Error failure ->
            for printed in [ CompaniesHouseFailure.describe failure; $"%A{failure}"; string failure ] do
                Assert.DoesNotContain("Jane", printed)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a list answer without its list is InvalidResponse`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok """{"total_results":3}""")

        match! Api.Officers.pageAsync (clientWith handler) "00000006" 0 10 ct with
        | Error(InvalidResponse _) -> ()
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``an insolvency answer without its cases is InvalidResponse, not no cases`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok """{"etag":"e"}""")

        match! Api.Insolvency.getAsync (clientWith handler) "00000006" ct with
        | Error(InvalidResponse _) -> ()
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``printing credentials or the client hides the key`` () =
    use handler = new FakeHandler(fun _ -> ok "{}")
    let client = clientWith handler

    for printed in
        [
            $"%A{credentials}"
            $"{credentials}"
            string credentials
            $"%O{credentials}"
            $"%A{client}"
            string client
        ] do
        Assert.DoesNotContain(Key, printed)

    Assert.Contains("hidden", $"%A{credentials}")

[<Fact>]
let ``a failure doesn't print the key`` () : Task =
    task {
        for status in [ 401; 403; 429; 500; 302 ] do
            use handler = new FakeHandler(fun _ -> answer status "{}")

            match! Api.Companies.getAsync (clientWith handler) "00000006" ct with
            | Error failure ->
                Assert.DoesNotContain(Key, CompaniesHouseFailure.describe failure)
                Assert.DoesNotContain(Key, $"%A{failure}")
            | other -> failwith $"%A{other}"
    }

[<Theory; InlineData(""); InlineData("   ")>]
let ``a blank key is refused at creation`` (key: string) =
    use handler = new FakeHandler(fun _ -> ok "{}")

    Assert.Throws<ArgumentException>(fun () ->
        CompaniesHouseClient.create { ApiKey = key } Endpoints.live handler |> ignore)
    |> ignore

[<Theory; InlineData("https://evil.example/company/00000006"); InlineData("//evil.example/company/00000006")>]
let ``a path off the client's endpoints is refused, and the key doesn't travel`` (path: string) : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok "{}")

        match! Api.getJsonAsync (clientWith handler) path ct with
        | Error(InvalidRequest reason) ->
            Assert.Contains("evil.example", reason)
            Assert.Empty handler.Calls
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a plain http address on the same host isn't sent the key`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok "{}")

        match!
            Api.getJsonAsync (clientWith handler) "http://api.company-information.service.gov.uk/company/00000006" ct
        with
        | Error(InvalidRequest _) -> Assert.Empty handler.Calls
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``getJsonAsync reads any path into an element that outlives the call`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok """{"kind":"exemptions"}""")
        let! result = Api.getJsonAsync (clientWith handler) "company/00000006/exemptions" ct
        Assert.Equal("exemptions", (expectOk result).Value.GetProperty("kind").GetString())
        Assert.Equal("/company/00000006/exemptions", handler.Calls.Head.Target)
    }

[<Fact>]
let ``an endpoint with a path keeps it, with or without a trailing slash`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok company)

        let endpoints =
            {
                Api = Uri "https://proxy.example/ch"
                Document = Uri "https://proxy.example/doc"
            }

        let client = CompaniesHouseClient.create credentials endpoints handler
        let! _ = Api.Companies.getAsync client "00000006" ct
        Assert.Equal("https://proxy.example/ch/company/00000006", handler.Calls.Head.Uri)
    }

[<Fact>]
let ``Create without a timeout keeps the default and Create with one sets it on both paths`` () =
    let plain = CompaniesHouseClient.Create(credentials, Endpoints.live)
    Assert.Equal(TimeSpan.FromSeconds 100., plain.Keyed.Timeout)

    let limited =
        CompaniesHouseClient.Create(credentials, Endpoints.live, TimeSpan.FromSeconds 20.)

    Assert.Equal(TimeSpan.FromSeconds 20., limited.Keyed.Timeout)
    Assert.Equal(TimeSpan.FromSeconds 20., limited.Keyless.Timeout)

[<Theory; InlineData(0.); InlineData(-5.)>]
let ``Create refuses a timeout that isn't positive`` (seconds: float) =
    Assert.Throws<ArgumentOutOfRangeException>(fun () ->
        CompaniesHouseClient.Create(credentials, Endpoints.live, TimeSpan.FromSeconds seconds)
        |> ignore)
    |> ignore

[<Fact>]
let ``a stuck call past the timeout is TransportError, not an exception`` () : Task =
    task {
        let client =
            CompaniesHouseClient(
                credentials,
                Endpoints.live,
                new HangingHandler(),
                ValueSome(TimeSpan.FromMilliseconds 200.)
            )

        let call = Api.Companies.getAsync client "00000006" ct
        let! winner = Task.WhenAny(call :> Task, Task.Delay 10000)
        Assert.Same(call, winner)

        match! call with
        | Error(TransportError(:? OperationCanceledException) as failure) ->
            Assert.Equal(FailureAction.RetryLater, CompaniesHouseFailure.action failure)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``the caller's cancellation still throws with a timeout set`` () : Task =
    task {
        use cancel = new CancellationTokenSource(TimeSpan.FromMilliseconds 100.)

        let client =
            CompaniesHouseClient(credentials, Endpoints.live, new HangingHandler(), ValueSome(TimeSpan.FromSeconds 30.))

        let! _ =
            Assert.ThrowsAnyAsync<OperationCanceledException>(fun () ->
                Api.Companies.getAsync client "00000006" cancel.Token :> Task)

        ()
    }
