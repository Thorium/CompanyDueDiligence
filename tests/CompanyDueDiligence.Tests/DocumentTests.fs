/// A document's redirect is followed by hand, and the key stays on the client's endpoints.
module DocumentTests

open System
open System.IO
open System.Net
open System.Net.Sockets
open System.Text
open System.Text.Json
open System.Threading.Tasks
open Xunit
open CompanyDueDiligence.CompaniesHouse
open Fake

[<Literal>]
let private DocumentHost = "document-api.company-information.service.gov.uk"

[<Literal>]
let private Signed =
    "https://signed.example/bucket/doc?X-Amz-Signature=the-signature"

let private redirectTo (location: string) =
    { answer 302 "" with
        Headers = [ "Location", location ]
    }

/// A 302 from the Document API, then whatever signed says.
let private documentFlow (location: string) (signed: Call -> Answer) (call: Call) =
    if call.Host = DocumentHost then
        redirectTo location
    else
        signed call

let private pdf _ = { ok "%PDF-bytes" with Headers = [] }

[<Fact>]
let ``content follows the 302 by hand: the key to the API, none to the signed URL`` () : Task =
    task {
        use handler = new FakeHandler(documentFlow Signed pdf)

        match! Api.Documents.contentAsync (clientWith handler) "doc-1" "application/pdf" ct with
        | Ok(ValueSome document) -> Assert.Equal("%PDF-bytes", Encoding.UTF8.GetString document.Content)
        | other -> failwith $"%A{other}"

        match handler.Calls with
        | [ first; second ] ->
            Assert.Equal($"https://{DocumentHost}/document/doc-1/content", first.Uri)
            Assert.Equal("application/pdf", first.Headers["Accept"])
            Assert.StartsWith("Basic ", first.Headers["Authorization"])
            Assert.Equal(Signed, second.Uri)
            Assert.False(second.Headers.ContainsKey "Authorization", "The signed URL must not get the key")
            let keyInBasic = Convert.ToBase64String(Encoding.UTF8.GetBytes(Key + ":"))

            Assert.DoesNotContain(
                second.Headers,
                fun header -> header.Value.Contains Key || header.Value.Contains keyInBasic
            )
        | calls -> failwith $"%A{calls}"
    }

[<Fact>]
let ``a redirect back to the client's own API host still carries no key`` () : Task =
    task {
        let onApi = "https://api.company-information.service.gov.uk/signed/doc"
        use handler = new FakeHandler(documentFlow onApi pdf)

        let! result =
            Api.Documents.contentAsync (clientWith handler) "doc-1" "application/pdf" ct

        Assert.True((expectOk result).IsSome)
        Assert.False(handler.Calls[1].Headers.ContainsKey "Authorization")
    }

[<Fact>]
let ``a relative Location resolves against the Document API, without the key`` () : Task =
    task {
        use handler =
            new FakeHandler(fun call ->
                if call.Target.EndsWith "/content" then
                    redirectTo "/stored/doc"
                else
                    pdf call)

        let! result =
            Api.Documents.contentAsync (clientWith handler) "doc-1" "application/pdf" ct

        Assert.True((expectOk result).IsSome)
        Assert.Equal($"https://{DocumentHost}/stored/doc", handler.Calls[1].Uri)
        Assert.False(handler.Calls[1].Headers.ContainsKey "Authorization")
    }

[<Fact>]
let ``the document's Content-Type comes from the host that served it`` () : Task =
    task {
        use handler = new FakeHandler(documentFlow Signed pdf)

        let! result =
            Api.Documents.contentAsync (clientWith handler) "doc-1" "application/xml" ct

        Assert.StartsWith("application/json", (expectOk result).Value.ContentType)
    }

[<Fact>]
let ``a redirect to plain http is refused and not called`` () : Task =
    task {
        use handler =
            new FakeHandler(documentFlow "http://signed.example/doc?sig=the-signature" pdf)

        match! Api.Documents.contentAsync (clientWith handler) "doc-1" "application/pdf" ct with
        | Error(RedirectRefused(302, reason) as failure) ->
            Assert.DoesNotContain("the-signature", reason)
            Assert.DoesNotContain("the-signature", CompaniesHouseFailure.describe failure)
            Assert.Single handler.Calls |> ignore
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a redirect without a Location is RedirectRefused`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> answer 302 "")

        match! Api.Documents.locationAsync (clientWith handler) "doc-1" "application/pdf" ct with
        | Error(RedirectRefused(302, _)) -> ()
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a second redirect isn't followed`` () : Task =
    task {
        use handler =
            new FakeHandler(documentFlow Signed (fun _ -> redirectTo "https://elsewhere.example/x"))

        match! Api.Documents.contentAsync (clientWith handler) "doc-1" "application/pdf" ct with
        | Error(RedirectRefused(302, _)) -> Assert.Equal(2, handler.Calls.Length)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a failure on the signed host is an ApiError without its body, and never ValueNone`` () : Task =
    task {
        for status in [ 403; 404 ] do
            use handler =
                new FakeHandler(documentFlow Signed (fun _ -> answer status $"<Error>{Signed}</Error>"))

            match! Api.Documents.contentAsync (clientWith handler) "doc-1" "application/pdf" ct with
            | Error(ApiError(answered, "")) -> Assert.Equal(status, answered)
            | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a 404 from the Document API is ValueNone, and 401 and 429 keep their types`` () : Task =
    task {
        use missing = new FakeHandler(fun _ -> notFound)

        match! Api.Documents.contentAsync (clientWith missing) "doc-1" "application/pdf" ct with
        | Ok ValueNone -> ()
        | other -> failwith $"%A{other}"

        use refused = new FakeHandler(fun _ -> answer 401 "")

        match! Api.Documents.contentAsync (clientWith refused) "doc-1" "application/pdf" ct with
        | Error(KeyRefused _) -> ()
        | other -> failwith $"%A{other}"

        use limited = new FakeHandler(fun _ -> answer 429 "")

        match! Api.Documents.contentAsync (clientWith limited) "doc-1" "application/pdf" ct with
        | Error(RateLimited _) -> ()
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``an unsupported type's 406 is an ApiError`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> answer 406 "")

        match! Api.Documents.contentAsync (clientWith handler) "doc-1" "application/pdf" ct with
        | Error(ApiError(406, _)) -> ()
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``an answer with no redirect is InvalidResponse`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok "%PDF-bytes")

        match! Api.Documents.locationAsync (clientWith handler) "doc-1" "application/pdf" ct with
        | Error(InvalidResponse _) -> ()
        | other -> failwith $"%A{other}"
    }

[<Theory; InlineData(""); InlineData("not a type")>]
let ``a bad content type or id isn't sent`` (contentType: string) : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok "{}")

        match! Api.Documents.locationAsync (clientWith handler) "doc-1" contentType ct with
        | Error(InvalidRequest _) -> ()
        | other -> failwith $"%A{other}"

        match! Api.Documents.locationAsync (clientWith handler) ".." "application/pdf" ct with
        | Error(InvalidRequest _) -> Assert.Empty handler.Calls
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``printing a location hides the signature`` () : Task =
    task {
        use handler = new FakeHandler(documentFlow Signed pdf)

        match! Api.Documents.locationAsync (clientWith handler) "doc-1" "application/pdf" ct with
        | Ok(ValueSome location) ->
            Assert.Equal(Signed, location.Uri.AbsoluteUri)

            for printed in [ $"%A{location}"; string location ] do
                Assert.DoesNotContain("the-signature", printed)
                Assert.Contains("signed.example", printed)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``metadata lists the content types, from the Document API with the key`` () : Task =
    task {
        let body =
            """{"id":"doc-1","pages":3,"created_at":"2026-01-02T03:04:05Z","resources":{"application/pdf":{"content_length":10},"application/xhtml+xml":{"content_length":20}}}"""

        use handler = new FakeHandler(fun _ -> ok body)
        let! result = Api.Documents.metadataAsync (clientWith handler) "doc 1" ct
        let metadata = (expectOk result).Value
        Assert.Equal<string list>([ "application/pdf"; "application/xhtml+xml" ], metadata.ContentTypes)
        Assert.Equal(ValueSome 3, metadata.Pages)
        Assert.Equal("https://" + DocumentHost + "/document/doc%201", handler.Calls.Head.Uri)
        Assert.True(handler.Calls.Head.Headers.ContainsKey "Authorization")
    }

[<Fact>]
let ``a document id stays one path segment, so it can't reach another endpoint`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok """{"id":"x"}""")
        let client = clientWith handler
        let! _ = Api.Documents.metadataAsync client "a/../../company/00000006?x=1" ct
        Assert.Equal("/document/a%2F..%2F..%2Fcompany%2F00000006%3Fx%3D1", handler.Calls.Head.Target)
        let! _ = Api.Documents.locationAsync client "a/b#c" "application/pdf" ct
        Assert.Equal("/document/a%2Fb%23c/content", handler.Calls[1].Target)
    }

[<Fact>]
let ``a filing's document link gives the id, and its host gets nothing`` () : Task =
    task {
        let filing =
            """{"transaction_id":"t1","links":{"document_metadata":"https://other-host.example/document/doc-9?x=1"}}"""

        use handler =
            new FakeHandler(fun call ->
                if call.Host = DocumentHost then
                    ok """{"id":"doc-9"}"""
                else
                    ok (listJson "total_count" 1 [ filing ]))

        let client = clientWith handler
        let! list = Api.FilingHistory.pageAsync client "00000006" [] 0 1 ct
        let id = (expectOk list).Value.Items.Head.DocumentId
        Assert.Equal(ValueSome "doc-9", id)
        let! _ = Api.Documents.metadataAsync client id.Value ct
        Assert.DoesNotContain(handler.Calls, fun call -> call.Host = "other-host.example")
    }

/// A local host that answers one request and keeps its text.
type private Host(answer: string) =
    let listener = new TcpListener(IPAddress.Loopback, 0)
    let mutable received = ""
    do listener.Start()

    let served =
        task {
            try
                use! socket = listener.AcceptTcpClientAsync()
                use stream = socket.GetStream()
                let buffer = Array.zeroCreate 16384
                let! count = stream.ReadAsync(buffer, 0, buffer.Length)
                received <- Encoding.ASCII.GetString(buffer, 0, count)
                let bytes = Encoding.ASCII.GetBytes answer
                do! stream.WriteAsync(bytes, 0, bytes.Length)
            with
            | :? SocketException
            | :? ObjectDisposedException -> ()
        }

    member _.Port = (listener.LocalEndpoint :?> IPEndPoint).Port
    member _.Received = received
    member _.Served: Task = served

    interface IDisposable with
        member _.Dispose() = listener.Stop()

let private endpointsOf (port: int) =
    {
        Api = Uri $"http://127.0.0.1:{port}/"
        Document = Uri $"http://127.0.0.1:{port}/"
    }

let private redirectAnswer (port: int) =
    $"HTTP/1.1 302 Found\r\nLocation: http://127.0.0.1:{port}/signed?sig=abc\r\nContent-Length: 0\r\nConnection: close\r\n\r\n"

[<Fact>]
let ``over sockets, the real handler stops at a redirect on an ordinary call`` () : Task =
    task {
        use other =
            new Host "HTTP/1.1 200 OK\r\nContent-Length: 2\r\nConnection: close\r\n\r\n{}"

        use first = new Host(redirectAnswer other.Port)
        let client = CompaniesHouseClient.Create(credentials, endpointsOf first.Port)

        match! Api.Companies.getAsync client "00000006" ct with
        | Error(RedirectRefused(302, _)) ->
            do! first.Served
            Assert.Contains("Authorization: Basic", first.Received)
            Assert.Equal("", other.Received)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``over sockets, the document's second host sees no key`` () : Task =
    task {
        use signed =
            new Host "HTTP/1.1 200 OK\r\nContent-Type: application/pdf\r\nContent-Length: 3\r\nConnection: close\r\n\r\nPDF"

        use first = new Host(redirectAnswer signed.Port)
        let client = CompaniesHouseClient.Create(credentials, endpointsOf first.Port)

        match! Api.Documents.contentAsync client "doc-1" "application/pdf" ct with
        | Ok(ValueSome document) ->
            do! first.Served
            do! signed.Served
            Assert.Equal("PDF", Encoding.ASCII.GetString document.Content)
            Assert.Equal("application/pdf", document.ContentType)
            Assert.Contains("Authorization: Basic", first.Received)
            Assert.Contains("GET /signed?sig=abc", signed.Received)
            Assert.DoesNotContain("Authorization", signed.Received)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a stuck signed host past the timeout is TransportError`` () : Task =
    task {
        // The first request answers; the fake then hangs the second through the shared handler's cancellation.
        let handler =
            { new Net.Http.HttpMessageHandler() with
                override _.SendAsync(request, cancel) =
                    task {
                        if request.RequestUri.Host = DocumentHost then
                            let response = new Net.Http.HttpResponseMessage(HttpStatusCode.Found)
                            response.Headers.Location <- Uri Signed
                            return response
                        else
                            do! Task.Delay(Threading.Timeout.Infinite, cancel)
                            return null
                    }
            }

        let client =
            CompaniesHouseClient(credentials, Endpoints.live, handler, ValueSome(TimeSpan.FromMilliseconds 200.))

        let call = Api.Documents.contentAsync client "doc-1" "application/pdf" ct
        let! winner = Task.WhenAny(call :> Task, Task.Delay 10000)
        Assert.Same(call, winner)

        match! call with
        | Error(TransportError _) -> ()
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``the bundled Document API spec lists both paths`` () =
    use document =
        JsonDocument.Parse(File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "../../Schemas/document/swagger.json")))

    let paths =
        document.RootElement.GetProperty("paths").EnumerateObject()
        |> Seq.map _.Name
        |> Set.ofSeq

    Assert.Contains("/document/{document_id}", paths)
    Assert.Contains("/document/{document_id}/content", paths)
    Assert.Equal(DocumentHost, document.RootElement.GetProperty("host").GetString())
