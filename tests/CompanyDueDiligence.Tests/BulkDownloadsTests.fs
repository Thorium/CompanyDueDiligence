/// The bulk catalogue and the opt-in download cache, over a fake handler and a temporary folder.
module BulkDownloadsTests

open System
open System.IO
open System.Threading.Tasks
open Xunit
open CompanyDueDiligence
open Fake

let private fixture (name: string) =
    File.ReadAllText(Path.Combine(AppContext.BaseDirectory, "Fixtures", name))

let private fixtureBytes (name: string) =
    FileInfo(Path.Combine(AppContext.BaseDirectory, "Fixtures", name)).Length

/// The catalogue's entry with each fixed link's size set, so a small fixture isn't refused as a page sent in place of the file.
let private sized (bytes: int64) (id: string) =
    let entry = (BulkCatalogue.tryFind id).Value

    match entry.Delivery with
    | BulkDelivery.Links links ->
        { entry with
            Delivery = BulkDelivery.Links(links |> List.map (fun link -> { link with ApproximateBytes = bytes }))
        }
    | _ -> entry

let private serbiaEntry = sized (fixtureBytes "serbia-companies.json") "RS"

/// A fresh folder under the test run's temp directory, removed by the caller.
let private freshFolder () =
    let folder =
        Path.Combine(Path.GetTempPath(), "cdd-bulk-" + Guid.NewGuid().ToString "N")

    Directory.CreateDirectory folder |> ignore
    folder

type private Clock() =
    member val Now = DateTimeOffset(2026, 11, 8, 12, 0, 0, TimeSpan.Zero) with get, set

let private cacheOver (folder: string) (clock: Clock) (respond: Call -> Answer) =
    let handler = new FakeHandler(respond)
    handler, new BulkDownloads(folder, handler, (fun () -> clock.Now))

[<Fact>]
let ``the catalogue lists every bulk source once, with links, a resolver or a way to get it by hand`` () =
    let ids = BulkCatalogue.all |> List.map _.Id
    Assert.Equal(20, ids.Length)
    Assert.Equal(ids.Length, (List.distinct ids).Length)

    for entry in BulkCatalogue.all do
        match entry.Delivery with
        | BulkDelivery.Links links ->
            Assert.NotEmpty links
            Assert.Equal(links.Length, (links |> List.map _.Name |> List.distinct).Length)
        | BulkDelivery.Resolved resolver ->
            Assert.False(String.IsNullOrWhiteSpace resolver.Portal)
            Assert.False(String.IsNullOrWhiteSpace resolver.Dataset)
            Assert.False(String.IsNullOrWhiteSpace resolver.ByHand)
            Assert.True(resolver.ApproximateBytes > 0L)
        | BulkDelivery.ByHand how -> Assert.False(String.IsNullOrWhiteSpace how)

[<Fact>]
let ``nine sources are resolved through a listing, and three stay by hand`` () =
    let ids kind =
        BulkCatalogue.all
        |> List.filter (fun entry -> kind entry.Delivery)
        |> List.map _.Id
        |> List.sort

    Assert.Equal<string list>(
        [ "AR-C"; "AU-ASIC"; "BR"; "CL-RES"; "CY"; "RO"; "RU-SME"; "SI"; "UA" ],
        ids (function
            | BulkDelivery.Resolved _ -> true
            | _ -> false)
    )

    Assert.Equal<string list>(
        [ "BE"; "MD"; "US-FL" ],
        ids (function
            | BulkDelivery.ByHand _ -> true
            | _ -> false)
    )

[<Fact>]
let ``a downloaded source loads as a registry, as of its download`` () : Task =
    task {
        let folder = freshFolder ()
        let clock = Clock()

        try
            let handler, cache =
                cacheOver folder clock (fun _ -> ok (fixture "serbia-companies.json"))

            use _ = handler
            use cache = cache
            Assert.Equal(BulkCacheState.NotDownloaded, cache.StateOf serbiaEntry)

            match! cache.DownloadAsync(serbiaEntry, ValueNone, ct) with
            | Ok(BulkCacheState.Downloaded(at, bytes, stale)) ->
                Assert.Equal(clock.Now, at)
                Assert.True(bytes > 0L)
                Assert.False stale
            | other -> failwith $"%A{other}"

            match! cache.LoadAsync(serbiaEntry, ct) with
            | Ok registry ->
                use _ = registry :?> IDisposable

                match! registry.TryFindAsync("17003313", Jurisdiction.ofCode "RS", ct) with
                | Ok evidence ->
                    Assert.Equal(clock.Now, evidence.RetrievedAt)

                    match evidence.Value with
                    | Available(ValueSome company) -> Assert.Equal("17003313", company.Number)
                    | other -> failwith $"%A{other}"
                | Error failure -> failwith (RegistryFailure.describe failure)
            | Error failure -> failwith (RegistryFailure.describe failure)
        finally
            Directory.Delete(folder, true)
    }

[<Fact>]
let ``a download goes stale once older than the publisher's cadence`` () : Task =
    task {
        let folder = freshFolder ()
        let clock = Clock()

        try
            let handler, cache =
                cacheOver folder clock (fun _ -> ok (fixture "serbia-companies.json"))

            use _ = handler
            use cache = cache
            let! _ = cache.DownloadAsync(serbiaEntry, ValueNone, ct)
            clock.Now <- clock.Now + serbiaEntry.Refresh + TimeSpan.FromHours 1.

            match cache.StateOf serbiaEntry with
            | BulkCacheState.Downloaded(_, _, stale) -> Assert.True stale
            | other -> failwith $"%A{other}"
        finally
            Directory.Delete(folder, true)
    }

[<Fact>]
let ``a file far larger than expected is refused, and an earlier download stays`` () : Task =
    task {
        let folder = freshFolder ()
        let clock = Clock()
        let small = sized 4L "BA-BRC"
        let huge = String('x', 20_000_000)
        let mutable body = "<a/>"

        try
            let handler, cache = cacheOver folder clock (fun _ -> ok body)
            use _ = handler
            use cache = cache
            let! first = cache.DownloadAsync(small, ValueNone, ct)
            Assert.True(Result.isOk first)
            body <- huge

            match! cache.DownloadAsync(small, ValueNone, ct) with
            | Error(RegistryFailure.UnreadableAnswer reason) -> Assert.Contains("larger than expected", reason)
            | other -> failwith $"%A{other}"

            match cache.StateOf small with
            | BulkCacheState.Downloaded(_, bytes, _) -> Assert.Equal(4L, bytes)
            | other -> failwith $"%A{other}"

            Assert.Empty(Directory.GetFiles(Path.Combine(folder, small.Id), "*.part"))
        finally
            Directory.Delete(folder, true)
    }

[<Fact>]
let ``a refused download fails without a stamp, and a by-hand source isn't fetched`` () : Task =
    task {
        let folder = freshFolder ()
        let clock = Clock()

        try
            let handler, cache = cacheOver folder clock (fun _ -> answer 404 "gone")
            use handler = handler
            use cache = cache

            match! cache.DownloadAsync(serbiaEntry, ValueNone, ct) with
            | Error(RegistryFailure.RequestRefused _) -> ()
            | other -> failwith $"%A{other}"

            Assert.Equal(BulkCacheState.NotDownloaded, cache.StateOf serbiaEntry)
            let belgium = (BulkCatalogue.tryFind "BE").Value
            Assert.Equal(BulkCacheState.ByHand, cache.StateOf belgium)
            let calls = handler.Calls.Length

            match! cache.DownloadAsync(belgium, ValueNone, ct) with
            | Error(RegistryFailure.InvalidRequest _) -> Assert.Equal(calls, handler.Calls.Length)
            | other -> failwith $"%A{other}"
        finally
            Directory.Delete(folder, true)
    }

[<Fact>]
let ``removing a source deletes its files`` () : Task =
    task {
        let folder = freshFolder ()
        let clock = Clock()

        try
            let handler, cache =
                cacheOver folder clock (fun _ -> ok (fixture "serbia-companies.json"))

            use _ = handler
            use cache = cache
            let! _ = cache.DownloadAsync(serbiaEntry, ValueNone, ct)
            cache.Remove serbiaEntry
            Assert.Equal(BulkCacheState.NotDownloaded, cache.StateOf serbiaEntry)
        finally
            Directory.Delete(folder, true)
    }

let private serbiaCompany (registry: ICompanyRegistry) =
    task {
        match! registry.TryFindAsync("17003313", Jurisdiction.ofCode "RS", ct) with
        | Ok { Value = Available(ValueSome company) } -> return company
        | other -> return failwith $"%A{other}"
    }

[<Fact>]
let ``a load keeps an index, which the next load opens without reading the files`` () : Task =
    task {
        let folder = freshFolder ()
        let clock = Clock()

        try
            let handler, cache =
                cacheOver folder clock (fun _ -> ok (fixture "serbia-companies.json"))

            use _ = handler
            use cache = cache
            let! _ = cache.DownloadAsync(serbiaEntry, ValueNone, ct)
            Assert.Equal(BulkIndexState.NotBuilt, cache.IndexStateOf serbiaEntry)

            let! first =
                task {
                    match! cache.LoadAsync(serbiaEntry, ct) with
                    | Ok registry -> return registry
                    | Error failure -> return failwith (RegistryFailure.describe failure)
                }

            use _ = first :?> IDisposable
            let! company = serbiaCompany first

            match cache.IndexStateOf serbiaEntry with
            | BulkIndexState.Built bytes -> Assert.True(bytes > 0L)
            | other -> failwith $"%A{other}"

            // The file is overwritten with bytes no build can read, keeping its size and time: only an index that isn't rebuilt still answers.
            let file = Path.Combine(folder, serbiaEntry.Id, "companies.json")
            let manifest = Path.Combine(folder, serbiaEntry.Id, "index", "manifest.json")
            let modified = File.GetLastWriteTimeUtc file
            let written = File.GetLastWriteTimeUtc manifest
            do! File.WriteAllBytesAsync(file, Array.create (int (FileInfo(file).Length)) (byte 'x'))
            File.SetLastWriteTimeUtc(file, modified)

            match! cache.LoadAsync(serbiaEntry, ct) with
            | Ok second ->
                use _ = second :?> IDisposable
                let! again = serbiaCompany second
                Assert.Equal(company, again)
            | Error failure -> failwith (RegistryFailure.describe failure)

            Assert.Equal(written, File.GetLastWriteTimeUtc manifest)

            // A file of another time is no longer the file the index was built from.
            File.SetLastWriteTimeUtc(file, modified.AddMinutes 1.)
            Assert.Equal(BulkIndexState.NotBuilt, cache.IndexStateOf serbiaEntry)

            match! cache.LoadAsync(serbiaEntry, ct) with
            | Error(RegistryFailure.UnreadableAnswer _) -> ()
            | other -> failwith $"%A{other}"
        finally
            Directory.Delete(folder, true)
    }

[<Fact>]
let ``downloading again removes the index of the download before`` () : Task =
    task {
        let folder = freshFolder ()
        let clock = Clock()

        try
            let handler, cache =
                cacheOver folder clock (fun _ -> ok (fixture "serbia-companies.json"))

            use _ = handler
            use cache = cache
            let! _ = cache.DownloadAsync(serbiaEntry, ValueNone, ct)

            match! cache.LoadAsync(serbiaEntry, ct) with
            | Ok registry ->
                use _ = registry :?> IDisposable
                clock.Now <- clock.Now + TimeSpan.FromDays 1.
                let! _ = cache.DownloadAsync(serbiaEntry, ValueNone, ct)
                Assert.False(Directory.Exists(Path.Combine(folder, serbiaEntry.Id, "index")))
                Assert.Equal(BulkIndexState.NotBuilt, cache.IndexStateOf serbiaEntry)
                // The open index still answers from the files it holds.
                let! company = serbiaCompany registry
                Assert.Equal("17003313", company.Number)
            | Error failure -> failwith (RegistryFailure.describe failure)

            match! cache.LoadAsync(serbiaEntry, ct) with
            | Ok registry ->
                use _ = registry :?> IDisposable
                let! evidence = registry.TryFindAsync("17003313", Jurisdiction.ofCode "RS", ct)
                Assert.Equal(clock.Now, (Result.toOption evidence).Value.RetrievedAt)
            | Error failure -> failwith (RegistryFailure.describe failure)
        finally
            Directory.Delete(folder, true)
    }

// Downloads of several files, timeouts, and dates

let private alaskaEntry = sized 5L "US-AK"

[<Fact>]
let ``a second file that fails leaves the first file, the stamp and the index of the download before`` () : Task =
    task {
        let folder = freshFolder ()
        let clock = Clock()

        try
            let handler, cache = cacheOver folder clock (fun _ -> ok "first")
            use _ = handler
            use cache = cache
            let! first = cache.DownloadAsync(alaskaEntry, ValueNone, ct)
            Assert.True(Result.isOk first, $"%A{first}")
            let saved = Path.Combine(folder, alaskaEntry.Id)
            let stamp = File.ReadAllText(Path.Combine(saved, "downloaded.txt"))
            let marker = Path.Combine(saved, "index", "marker")
            Directory.CreateDirectory(Path.GetDirectoryName marker) |> ignore
            File.WriteAllText(marker, "index")
            let before = cache.StateOf alaskaEntry
            clock.Now <- clock.Now + TimeSpan.FromDays 1.

            let respondAgain (call: Call) =
                if call.Uri.EndsWith "OfficialsDownload" then
                    answer 503 "down"
                else
                    ok "second"

            let handler2, cache2 = cacheOver folder clock respondAgain
            use _ = handler2
            use cache2 = cache2

            match! cache2.DownloadAsync(alaskaEntry, ValueNone, ct) with
            | Error(RegistryFailure.Unavailable reason) -> Assert.Contains("503", reason)
            | other -> failwith $"%A{other}"

            Assert.Equal("first", File.ReadAllText(Path.Combine(saved, "corporations.csv")))
            Assert.Equal("first", File.ReadAllText(Path.Combine(saved, "officials.csv")))
            Assert.Equal(stamp, File.ReadAllText(Path.Combine(saved, "downloaded.txt")))
            Assert.True(File.Exists marker)
            Assert.Equal(before, cache2.StateOf alaskaEntry)
            Assert.Empty(Directory.GetFiles(saved, "*.part"))
        finally
            Directory.Delete(folder, true)
    }

[<Fact>]
let ``a download cancelled during its second file deletes its part files and keeps the download before`` () : Task =
    task {
        let folder = freshFolder ()
        let clock = Clock()

        try
            let handler, cache = cacheOver folder clock (fun _ -> ok "first")
            use _ = handler
            use cache = cache
            let! first = cache.DownloadAsync(alaskaEntry, ValueNone, ct)
            Assert.True(Result.isOk first, $"%A{first}")
            use cancel = new Threading.CancellationTokenSource()

            let respond (call: Call) =
                if call.Uri.EndsWith "OfficialsDownload" then
                    cancel.Cancel()

                ok "second"

            let handler2, cache2 = cacheOver folder clock respond
            use _ = handler2
            use cache2 = cache2

            let! _ =
                Assert.ThrowsAnyAsync<OperationCanceledException>(fun () ->
                    cache2.DownloadAsync(alaskaEntry, ValueNone, cancel.Token) :> Task)

            let saved = Path.Combine(folder, alaskaEntry.Id)
            Assert.Equal("first", File.ReadAllText(Path.Combine(saved, "corporations.csv")))
            Assert.Empty(Directory.GetFiles(saved, "*.part"))
            Assert.Equal(first, Ok(cache2.StateOf alaskaEntry))
        finally
            Directory.Delete(folder, true)
    }

[<Fact>]
let ``a file far smaller than its catalogued size is refused, as a page sent in its place`` () : Task =
    task {
        let folder = freshFolder ()
        let clock = Clock()
        let brcko = (BulkCatalogue.tryFind "BA-BRC").Value

        try
            let handler, cache =
                cacheOver folder clock (fun _ -> ok "<html>Are you human?</html>")

            use _ = handler
            use cache = cache

            match! cache.DownloadAsync(brcko, ValueNone, ct) with
            | Error(RegistryFailure.UnreadableAnswer reason) -> Assert.Contains("far smaller", reason)
            | other -> failwith $"%A{other}"

            Assert.Equal(BulkCacheState.NotDownloaded, cache.StateOf brcko)
            Assert.Empty(Directory.GetFiles(Path.Combine(folder, brcko.Id)))
        finally
            Directory.Delete(folder, true)
    }

/// A body that gives a few bytes, then nothing until cancelled.
type private StallingStream() =
    inherit Stream()
    let mutable given = false
    override _.CanRead = true
    override _.CanSeek = false
    override _.CanWrite = false
    override _.Length = raise (NotSupportedException())

    override _.Position
        with get () = raise (NotSupportedException())
        and set _ = raise (NotSupportedException())

    override _.Flush() = ()
    override _.Read(_: byte array, _: int, _: int) : int = raise (NotSupportedException())
    override _.Seek(_, _) = raise (NotSupportedException())
    override _.SetLength _ = raise (NotSupportedException())
    override _.Write(_, _, _) = raise (NotSupportedException())

    override _.ReadAsync(buffer: Memory<byte>, ct: Threading.CancellationToken) =
        if given then
            ValueTask<int>(
                task {
                    do! Task.Delay(Threading.Timeout.Infinite, ct)
                    return 0
                }
            )
        else
            given <- true
            buffer.Span[0] <- byte '<'
            ValueTask<int> 1

/// Answers every request with body, dated by Last-Modified when modified is given.
type private BodyHandler(body: unit -> Net.Http.HttpContent, modified: DateTimeOffset voption) =
    inherit Net.Http.HttpMessageHandler()

    override _.SendAsync(_, _) =
        let content = body ()

        modified
        |> ValueOption.iter (fun at -> content.Headers.LastModified <- Nullable at)

        Task.FromResult(new Net.Http.HttpResponseMessage(Net.HttpStatusCode.OK, Content = content))

[<Fact>]
let ``a download that receives nothing for the idle time is Unavailable, on the headers or the body`` () : Task =
    task {
        let folder = freshFolder ()
        let brcko = sized 1L "BA-BRC"
        let idle = TimeSpan.FromMilliseconds 200.

        try
            for handler in
                [
                    new HangingHandler() :> Net.Http.HttpMessageHandler
                    new BodyHandler((fun () -> new Net.Http.StreamContent(new StallingStream())), ValueNone)
                ] do
                use _ = handler

                use cache =
                    new BulkDownloads(folder, handler, (fun () -> DateTimeOffset.UtcNow), idle)

                match! cache.DownloadAsync(brcko, ValueNone, ct) with
                | Error(RegistryFailure.Unavailable reason) -> Assert.Contains("sent nothing", reason)
                | other -> failwith $"%A{other}"

                Assert.Equal(BulkCacheState.NotDownloaded, cache.StateOf brcko)
                Assert.Empty(Directory.GetFiles(Path.Combine(folder, brcko.Id), "*.part"))
        finally
            Directory.Delete(folder, true)
    }

[<Fact>]
let ``cancelling a download that is waiting throws, not a timeout`` () : Task =
    task {
        let folder = freshFolder ()

        try
            use handler = new HangingHandler()

            use cache =
                new BulkDownloads(folder, handler, (fun () -> DateTimeOffset.UtcNow), TimeSpan.FromMinutes 1.)

            use cancel = new Threading.CancellationTokenSource(TimeSpan.FromMilliseconds 100.)

            let! _ =
                Assert.ThrowsAnyAsync<OperationCanceledException>(fun () ->
                    cache.DownloadAsync(sized 1L "BA-BRC", ValueNone, cancel.Token) :> Task)

            ()
        finally
            Directory.Delete(folder, true)
    }

[<Fact>]
let ``a fixed link's Last-Modified is the download's AsOf, kept with its stamp`` () : Task =
    task {
        let folder = freshFolder ()
        let clock = Clock()
        let brcko = sized (fixtureBytes "brcko-opendata.xml") "BA-BRC"
        let modified = DateTimeOffset(2026, 10, 1, 8, 0, 0, TimeSpan.Zero)

        try
            use handler =
                new BodyHandler(
                    (fun () -> new Net.Http.StringContent(fixture "brcko-opendata.xml")),
                    ValueSome modified
                )

            use cache = new BulkDownloads(folder, handler, (fun () -> clock.Now))
            let! downloaded = cache.DownloadAsync(brcko, ValueNone, ct)
            Assert.True(Result.isOk downloaded, $"%A{downloaded}")

            // A new cache over the folder reads the date from the stamp, both building and opening the index.
            use again = new BulkDownloads(folder, handler, (fun () -> clock.Now))

            for _ in 1..2 do
                match! again.LoadAsync(brcko, ct) with
                | Ok registry ->
                    use _ = registry :?> IDisposable

                    match! registry.TryFindAsync("1-369", Jurisdiction.ofCode "BA-BRC", ct) with
                    | Ok evidence ->
                        Assert.Equal(clock.Now, evidence.RetrievedAt)
                        Assert.Equal(ValueSome(DateOnly(2026, 10, 1)), evidence.AsOf)
                    | Error failure -> failwith (RegistryFailure.describe failure)
                | Error failure -> failwith (RegistryFailure.describe failure)
        finally
            Directory.Delete(folder, true)
    }

[<Fact>]
let ``an index another library version wrote, or that names none, is rebuilt`` () : Task =
    task {
        let folder = freshFolder ()
        let clock = Clock()

        try
            let handler, cache =
                cacheOver folder clock (fun _ -> ok (fixture "serbia-companies.json"))

            use _ = handler
            use cache = cache
            let! _ = cache.DownloadAsync(serbiaEntry, ValueNone, ct)

            let load () =
                task {
                    match! cache.LoadAsync(serbiaEntry, ct) with
                    | Ok registry -> (registry :?> IDisposable).Dispose()
                    | Error failure -> failwith (RegistryFailure.describe failure)
                }

            do! load ()
            let manifest = Path.Combine(folder, serbiaEntry.Id, "index", "manifest.json")
            let library = Text.RegularExpressions.Regex "\"library\": \"[^\"]*\",\\s*"
            Assert.Matches(library, File.ReadAllText manifest)

            for replacement in [ "\"library\": \"0.0.1-older\",\n  "; "" ] do
                File.WriteAllText(manifest, library.Replace(File.ReadAllText manifest, replacement))
                Assert.Equal(BulkIndexState.NotBuilt, cache.IndexStateOf serbiaEntry)

                // An index of another version still opens: only LoadAsync's reuse depends on the version.
                match! BulkIndexes.openAsync (Path.GetDirectoryName manifest) ct with
                | Ok registry -> (registry :?> IDisposable).Dispose()
                | Error failure -> failwith (RegistryFailure.describe failure)

                do! load ()

                match BulkStore.readManifest (Path.GetDirectoryName manifest) with
                | Ok rebuilt -> Assert.Equal(ValueSome BulkStore.Library, rebuilt.Library)
                | Error failure -> failwith (RegistryFailure.describe failure)

                match cache.IndexStateOf serbiaEntry with
                | BulkIndexState.Built _ -> ()
                | other -> failwith $"%A{other}"
        finally
            Directory.Delete(folder, true)
    }
