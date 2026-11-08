/// The local server's API, over an in-memory test host with its own data folder.
module ServerTests

open System
open System.IO
open System.Net
open System.Net.Http
open System.Net.Http.Json
open System.Threading.Tasks
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.TestHost
open Xunit
open CompanyDueDiligence
open CompanyDueDiligence.Server
open CompanyDueDiligence.Web.Dto

/// A server on a test host, started with the extra arguments, with a data folder deleted when disposed. With --token, Client sends the token.
type private LocalServer(extra: string array) =
    let data =
        Path.Combine(Path.GetTempPath(), "cdd-server-tests", Guid.NewGuid().ToString "N")

    let app =
        App.create (Array.append [| "--data"; data |] extra) (fun builder -> builder.WebHost.UseTestServer() |> ignore)

    do app.StartAsync().GetAwaiter().GetResult()
    let client = app.GetTestClient()

    do
        if Array.contains "--token" extra then
            client.DefaultRequestHeaders.Add(App.TokenHeader, App.accessToken data)

    new() = new LocalServer([||])

    member _.Client = client
    /// A client sending no access token.
    member _.Bare() = app.GetTestClient()
    member _.Data = data

    interface IDisposable with
        member _.Dispose() =
            client.Dispose()
            app.StopAsync().GetAwaiter().GetResult()
            (app :> IAsyncDisposable).DisposeAsync().AsTask().GetAwaiter().GetResult()

            try
                Directory.Delete(data, true)
            with :? IOException ->
                ()

let private ct () = TestContext.Current.CancellationToken

let private getJson<'T> (server: LocalServer) (path: string) =
    server.Client.GetFromJsonAsync<'T>(path, ct ())

[<Fact>]
let ``api/mode says it is the server`` () : Task =
    task {
        use server = new LocalServer()
        let! mode = getJson<ModeDto> server "/api/mode"
        Assert.True mode.Server
    }

[<Fact>]
let ``every keyless source is listed, with its flag and what it searches`` () : Task =
    task {
        use server = new LocalServer()
        let! registers = getJson<RegisterDto array> server "/api/registers"

        let keylessListed =
            registers |> Array.filter (fun register -> register.Kind = Kind.Keyless)

        Assert.Equal<string list>(
            Web.Registers.keyless |> List.map _.Code,
            keylessListed |> Array.map _.Code |> List.ofArray
        )

        let france = keylessListed |> Array.find (fun register -> register.Code = "FR")
        Assert.Equal("fr", france.Flag)
        Assert.True france.SearchesCompanies
        Assert.True france.SearchesPeople
        let poland = keylessListed |> Array.find (fun register -> register.Code = "PL")
        Assert.False poland.SearchesCompanies
        Assert.DoesNotContain(registers, fun register -> register.Kind = Kind.Bulk)
    }

[<Fact>]
let ``every flag listed has its file`` () =
    let flags =
        (Web.Registers.keyless |> List.map _.Flag)
        @ (Web.Registers.keyed |> List.map _.Flag)
        @ (BulkCatalogue.all
           |> List.map (fun entry -> Web.Registers.flagCodeOf entry.Jurisdiction))

    let folder =
        Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "samples", "CompanyDueDiligence.Web", "wwwroot", "flags", "1x1")

    for flag in List.distinct flags do
        Assert.True(File.Exists(Path.Combine(folder, flag + ".svg")), $"flags/1x1/{flag}.svg")

[<Fact>]
let ``the keyed table names every setting KeyedSources reads, in order`` () =
    Assert.Equal<string list>(KeyedSources.settingNames, Web.Registers.keyed |> List.collect _.Settings)

[<Fact>]
let ``a request naming another host is refused`` () : Task =
    task {
        use server = new LocalServer()
        use request = new HttpRequestMessage(HttpMethod.Get, "/api/mode")
        request.Headers.Host <- "rebound.example"
        use! response = server.Client.SendAsync(request, ct ())
        Assert.Equal(HttpStatusCode.BadRequest, response.StatusCode)
    }

[<Fact>]
let ``a change sent from another site is refused`` () : Task =
    task {
        use server = new LocalServer()
        use request = new HttpRequestMessage(HttpMethod.Post, "/api/bulk/BA-BRC/download")
        request.Headers.Add("Origin", "https://other.example")
        use! response = server.Client.SendAsync(request, ct ())
        Assert.Equal(HttpStatusCode.Forbidden, response.StatusCode)
        let! states = getJson<BulkDto array> server "/api/bulk"
        Assert.Equal(BulkState.NotDownloaded, (states |> Array.find (fun item -> item.Id = "BA-BRC")).State)
    }

[<Fact>]
let ``with --token, an API call without the access token is refused, except api/mode`` () : Task =
    task {
        use server = new LocalServer([| "--token" |])
        use! allowed = server.Client.GetAsync("/api/keys", ct ())
        Assert.Equal(HttpStatusCode.OK, allowed.StatusCode)
        use bare = server.Bare()

        for path in [ "/api/registers"; "/api/keys"; "/api/bulk"; "/api/search?term=x" ] do
            use! refused = bare.GetAsync(path, ct ())
            Assert.Equal(HttpStatusCode.Unauthorized, refused.StatusCode)

        use wrong = new HttpRequestMessage(HttpMethod.Get, "/api/keys")
        wrong.Headers.Add(App.TokenHeader, "wrong")
        use! refused = bare.SendAsync(wrong, ct ())
        Assert.Equal(HttpStatusCode.Unauthorized, refused.StatusCode)
        use! mode = bare.GetAsync("/api/mode", ct ())
        Assert.Equal(HttpStatusCode.OK, mode.StatusCode)
        use! page = bare.GetAsync("/", ct ())
        Assert.NotEqual(HttpStatusCode.Unauthorized, page.StatusCode)
    }

[<Fact>]
let ``without --token, the API needs no token`` () : Task =
    task {
        use server = new LocalServer()
        use bare = server.Bare()
        use! answer = bare.GetAsync("/api/keys", ct ())
        Assert.Equal(HttpStatusCode.OK, answer.StatusCode)
        Assert.False(File.Exists(Path.Combine(server.Data, "token")))
    }

[<Fact>]
let ``a bulk file on a network share or a relative path is refused`` () : Task =
    task {
        use server = new LocalServer()

        for path in [ @"\othersharecompany.xlsx"; "company.xlsx" ] do
            let paths = Collections.Generic.Dictionary<string, string array>()
            paths["file"] <- [| path |]

            use! answer =
                server.Client.PostAsJsonAsync("/api/bulk/MD/files", { BulkFilesDto.Paths = paths }, ct ())

            Assert.Equal(HttpStatusCode.BadRequest, answer.StatusCode)
            let! error = answer.Content.ReadFromJsonAsync<ErrorDto>(ct ())
            Assert.StartsWith("A full path on this computer", error.Error)
    }

[<Fact>]
let ``an unknown API path is a JSON 404`` () : Task =
    task {
        use server = new LocalServer()
        use! response = server.Client.GetAsync("/api/nothing", ct ())
        Assert.Equal(HttpStatusCode.NotFound, response.StatusCode)
        let! error = response.Content.ReadFromJsonAsync<ErrorDto>(ct ())
        Assert.Equal("No such API path", error.Error)
    }

[<Fact>]
let ``a saved key lists its register, is never echoed or stored plainly, and is removed`` () : Task =
    task {
        use server = new LocalServer()
        let secret = "test-canada-" + Guid.NewGuid().ToString "N"

        use! saved =
            server.Client.PutAsJsonAsync("/api/keys/CDD_CANADA_KEY", { Value = secret }, ct ())

        Assert.Equal(HttpStatusCode.OK, saved.StatusCode)
        let! savedBody = saved.Content.ReadAsStringAsync(ct ())
        Assert.DoesNotContain(secret, savedBody)

        let! keys = server.Client.GetStringAsync("/api/keys", ct ())
        Assert.DoesNotContain(secret, keys)
        let! listed = getJson<KeyedRegisterDto array> server "/api/keys"
        let canada = listed |> Array.find (fun register -> register.Code = "CA")
        Assert.True canada.Active
        Assert.True canada.Settings[0].Saved
        Assert.DoesNotContain(secret, File.ReadAllText(Path.Combine(server.Data, "keys.json")))

        let! registers = getJson<RegisterDto array> server "/api/registers"
        Assert.Contains(registers, fun register -> register.Code = "CA" && register.Kind = Kind.Keyed)

        use! removed = server.Client.DeleteAsync("/api/keys/CDD_CANADA_KEY", ct ())
        Assert.Equal(HttpStatusCode.OK, removed.StatusCode)
        let! after = getJson<RegisterDto array> server "/api/registers"
        Assert.DoesNotContain(after, fun register -> register.Code = "CA")
    }

[<Fact>]
let ``a setting given without its pair names what is missing`` () : Task =
    task {
        use server = new LocalServer()

        use! _ =
            server.Client.PutAsJsonAsync("/api/keys/CDD_SWEDEN_CLIENT_ID", { Value = "id" }, ct ())

        let! listed = getJson<KeyedRegisterDto array> server "/api/keys"
        let sweden = listed |> Array.find (fun register -> register.Code = "SE")
        Assert.False sweden.Active
        Assert.Equal("CDD_SWEDEN_CLIENT_SECRET not given", sweden.Problem)
    }

[<Fact>]
let ``an unknown setting or a key not sent as JSON is refused`` () : Task =
    task {
        use server = new LocalServer()

        use! unknown =
            server.Client.PutAsJsonAsync("/api/keys/CDD_NOTHING", { Value = "x" }, ct ())

        Assert.Equal(HttpStatusCode.NotFound, unknown.StatusCode)

        use! plain =
            server.Client.PutAsync("/api/keys/CDD_CANADA_KEY", new StringContent("x"), ct ())

        Assert.Equal(HttpStatusCode.BadRequest, plain.StatusCode)
    }

/// Polls api/bulk until the source's state is no longer running, for up to 30 seconds.
let rec private settled (server: LocalServer) (id: string) (tries: int) =
    task {
        let! states = getJson<BulkDto array> server "/api/bulk"
        let item = states |> Array.find (fun item -> item.Id = id)

        if
            (item.State = BulkState.Downloading || item.State = BulkState.Indexing)
            && tries > 0
        then
            do! Task.Delay(100, ct ())
            return! settled server id (tries - 1)
        else
            return item
    }

[<Fact>]
let ``a resolved source built from a file already here is listed, searched, and removed`` () : Task =
    task {
        use server = new LocalServer()
        let file = Path.Combine(server.Data, "opsiprs.csv")

        File.WriteAllText(
            file,
            "Matična številka;Davčna številka;Naziv;Pravno organizacijska oblika;Ulica;Hišna številka;Dodatek k hišni številki;Naselje;Poštna številka;Poštni kraj\n"
            + "1234567000;12345678;PRIMER PODJETJE d.o.o.;Družba z omejeno odgovornostjo;Slovenska cesta;10;A;Ljubljana;1000;Ljubljana\n"
        )

        let paths = Collections.Generic.Dictionary<string, string array>()
        paths["file"] <- [| file |]

        use! started =
            server.Client.PostAsJsonAsync("/api/bulk/SI/files", { Paths = paths }, ct ())

        Assert.Equal(HttpStatusCode.Accepted, started.StatusCode)

        let! item = settled server "SI" 300
        Assert.Null item.Error
        Assert.Equal(BulkState.IndexBuilt, item.State)
        Assert.True(item.IndexBytes.Value > 0L)

        let! registers = getJson<RegisterDto array> server "/api/registers"

        Assert.Contains(
            registers,
            fun register -> register.Code = "SI" && register.Kind = Kind.Bulk && register.Flag = "si"
        )

        let! rows = getJson<SearchRowDto array> server "/api/search?term=PRIMER&codes=SI"
        Assert.Equal("1234567000", rows[0].Matches[0].Number)

        use! removed = server.Client.DeleteAsync("/api/bulk/SI", ct ())
        Assert.Equal(HttpStatusCode.OK, removed.StatusCode)
        Assert.False(Directory.Exists(Path.Combine(server.Data, "bulk", "SI")))
        let! after = getJson<RegisterDto array> server "/api/registers"
        Assert.DoesNotContain(after, fun register -> register.Code = "SI")
    }

[<Fact>]
let ``the bulk listing offers Download for resolved sources, with their listing and the manual way, and keeps the form for by-hand ones``
    ()
    : Task =
    task {
        use server = new LocalServer()
        let! states = getJson<BulkDto array> server "/api/bulk"

        let byId id =
            states |> Array.find (fun item -> item.Id = id)

        for id in [ "AR-C"; "AU-ASIC"; "BR"; "CL-RES"; "CY"; "RO"; "RU-SME"; "SI"; "UA" ] do
            let item = byId id
            Assert.Equal("resolved", item.Delivery)
            Assert.Equal(BulkState.NotDownloaded, item.State)
            Assert.True(item.Bytes.HasValue && item.Bytes.Value > 0L, id)
            Assert.False(String.IsNullOrWhiteSpace item.Portal, id)
            Assert.False(String.IsNullOrWhiteSpace item.How, id)
            Assert.NotEmpty item.Slots
            Assert.Empty item.Files

        let asic = byId "AU-ASIC"
        Assert.Equal("data.gov.au, CKAN API: asic-companies", asic.Portal)

        for id in [ "BE"; "MD"; "US-FL" ] do
            let item = byId id
            Assert.Equal("by-hand", item.Delivery)
            Assert.Null item.Portal
            Assert.False(item.Bytes.HasValue)

        use! current = server.Client.GetAsync("/api/bulk/BE/current", ct ())
        Assert.Equal(HttpStatusCode.Conflict, current.StatusCode)
    }

[<Fact>]
let ``a by-hand build names a missing file`` () : Task =
    task {
        use server = new LocalServer()
        let paths = Collections.Generic.Dictionary<string, string array>()
        paths["file"] <- [| Path.Combine(server.Data, "absent.csv") |]

        use! response =
            server.Client.PostAsJsonAsync("/api/bulk/SI/files", { Paths = paths }, ct ())

        Assert.Equal(HttpStatusCode.BadRequest, response.StatusCode)
        let! error = response.Content.ReadFromJsonAsync<ErrorDto>(ct ())
        Assert.StartsWith("No such file", error.Error)
    }
