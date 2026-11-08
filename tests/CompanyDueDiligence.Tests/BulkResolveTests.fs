/// The resolvers of bulk sources whose file names change, over the portal answers recorded on 2026-11-08, and downloads through them.
module BulkResolveTests

open System
open System.IO
open System.Threading.Tasks
open Xunit
open CompanyDueDiligence
open Fake

let private fixture (name: string) =
    File.ReadAllText(Path.Combine(AppContext.BaseDirectory, "Fixtures", name))

let private entryOf id = (BulkCatalogue.tryFind id).Value

let private freshFolder () =
    let folder =
        Path.Combine(Path.GetTempPath(), "cdd-resolve-" + Guid.NewGuid().ToString "N")

    Directory.CreateDirectory folder |> ignore
    folder

let private now = DateTimeOffset(2026, 11, 8, 12, 0, 0, TimeSpan.Zero)

/// Runs work over a cache answering by respond, in a folder removed afterwards.
let private withCache (respond: Call -> Answer) (work: FakeHandler -> BulkDownloads -> string -> Task) : Task =
    task {
        let folder = freshFolder ()

        try
            use handler = new FakeHandler(respond)
            use cache = new BulkDownloads(folder, handler, (fun () -> now))
            do! work handler cache folder
        finally
            Directory.Delete(folder, true)
    }

let private resolved (cache: BulkDownloads) id =
    task {
        match! cache.ResolveAsync(entryOf id, ct) with
        | Ok files -> return files
        | Error failure -> return failwith (RegistryFailure.describe failure)
    }

let private utc (text: string) = DateTimeOffset.Parse text

/// The listing at the call's URI, by the start of its address; anything else is a 404.
let private listing (answers: (string * string) list) (call: Call) =
    answers
    |> List.tryFind (fun (start, _) -> call.Uri.StartsWith(start, StringComparison.Ordinal))
    |> Option.map (snd >> ok)
    |> Option.defaultValue (answer 404 "not here")

[<Fact>]
let ``ASIC resolves to the current weekly zip, with the size and date data.gov.au states`` () : Task =
    let api = "https://data.gov.au/data/api/3/action/package_show?id=asic-companies"

    withCache (listing [ api, fixture "resolve-asic-package.json" ]) (fun handler cache _ ->
        task {
            let! files = resolved cache "AU-ASIC"
            let file = Assert.Single files
            Assert.Equal("company.zip", file.Link.Name)
            Assert.Equal("Company Dataset - Current", file.PortalName)
            Assert.EndsWith("/download/company_202611.zip", string file.Link.Url)
            Assert.Equal(79_298_881L, file.Link.ApproximateBytes)
            Assert.Equal(ValueSome(utc "2026-11-02T13:51:17.043135Z"), file.Modified)
            Assert.Equal<string list>([ api ], handler.Calls |> List.map _.Uri)
        })

[<Fact>]
let ``Ukraine resolves to uo.zip, not the FOP or schema zips`` () : Task =
    withCache
        (listing
            [
                "https://data.gov.ua/api/3/action/package_show?id=03cc1239", fixture "resolve-ua-package.json"
            ])
        (fun _ cache _ ->
            task {
                let! files = resolved cache "UA"
                let file = Assert.Single files
                Assert.Equal("uo.zip", file.Link.Name)
                Assert.EndsWith("/download/uo.zip", string file.Link.Url)
                Assert.Equal(328_232_580L, file.Link.ApproximateBytes)
            })

[<Fact>]
let ``Buenos Aires resolves to the newest half year by its name, with the stated size as there is none listed``
    ()
    : Task =
    withCache
        (listing
            [
                "https://datos.jus.gob.ar/api/3/action/package_show", fixture "resolve-igj-package.json"
            ])
        (fun _ cache _ ->
            task {
                let! files = resolved cache "AR-C"
                let file = Assert.Single files
                Assert.Equal("igj.zip", file.Link.Name)
                Assert.Equal("IGJ - 2026 semestre 2", file.PortalName)
                Assert.EndsWith("/download/igj-2026-semestre-2.zip", string file.Link.Url)

                match (entryOf "AR-C").Delivery with
                | BulkDelivery.Resolved resolver -> Assert.Equal(resolver.ApproximateBytes, file.Link.ApproximateBytes)
                | other -> failwith $"%A{other}"
            })

[<Fact>]
let ``Chile resolves to every yearly CSV, one per year`` () : Task =
    withCache
        (listing
            [
                "https://datos.gob.cl/api/3/action/package_show", fixture "resolve-cl-package.json"
            ])
        (fun _ cache _ ->
            task {
                let! files = resolved cache "CL-RES"

                Assert.Equal<string list>(
                    [ for year in 2013..2026 -> $"res-{year}.csv" ],
                    files |> List.map _.Link.Name
                )

                let latest = files |> List.last
                Assert.EndsWith("/download/202609-sociedades-por-fecha-rut-constitucion.csv", string latest.Link.Url)
                Assert.Equal(24_072_953L, latest.Link.ApproximateBytes)
            })

[<Fact>]
let ``Romania resolves the newest firme and nomenclatoare datasets of the onrc organisation`` () : Task =
    withCache
        (listing
            [
                "https://data.gov.ro/api/3/action/package_search", fixture "resolve-ro-search.json"
            ])
        (fun handler cache _ ->
            task {
                let! files = resolved cache "RO"

                Assert.Equal<string list>(
                    [
                        "OD_FIRME.CSV"
                        "OD_STARE_FIRMA.CSV"
                        "OD_REPREZENTANTI_LEGALI.CSV"
                        "N_STARE_FIRMA.CSV"
                    ],
                    files |> List.map _.Link.Name
                )

                for file in files |> List.take 3 do
                    Assert.Contains("/dataset/c2c88c9d-4824-45f1-90ec-656f1c0d8785/", string file.Link.Url)

                Assert.Contains("/dataset/238b4f02-19a5-446d-b4dc-780c4f3e88aa/", string files[3].Link.Url)
                Assert.Equal(696_515_538L, files[0].Link.ApproximateBytes)
                let call = Assert.Single handler.Calls
                Assert.Contains("fq=organization%3Aonrc", call.Uri)
            })

[<Fact>]
let ``Cyprus resolves the three CSVs from the dataset's DCAT-AP listing, dated by the dataset`` () : Task =
    withCache
        (listing
            [
                "https://www.data.gov.cy/en/dataset/1026/dcat-ap-2.0/xml", fixture "resolve-cy-dcat.xml"
            ])
        (fun _ cache _ ->
            task {
                let! files = resolved cache "CY"

                Assert.Equal<string list>(
                    [ "organisations.csv"; "registered_office.csv"; "organisation_officials.csv" ],
                    files |> List.map _.Link.Name
                )

                Assert.Equal(
                    "https://www.data.gov.cy/sites/default/files/organisations_103.csv",
                    string files[0].Link.Url
                )

                Assert.Equal(93_089_618L, files[0].Link.ApproximateBytes)

                for file in files do
                    Assert.Equal(ValueSome(utc "2026-11-02T14:19:13+02:00"), file.Modified)
            })

[<Fact>]
let ``Russia resolves the newest data zip meta.csv lists, without fetching it`` () : Task =
    withCache
        (listing
            [
                "https://www.nalog.gov.ru/opendata/7707329152-rsmp/meta.csv", fixture "resolve-ru-meta.csv"
            ])
        (fun handler cache _ ->
            task {
                let! files = resolved cache "RU-SME"
                let file = Assert.Single files
                Assert.Equal("rsmp.zip", file.Link.Name)
                Assert.Equal("data-10092026-structure-12052026.zip", file.PortalName)
                Assert.Equal(ValueSome(DateTimeOffset(2026, 9, 10, 0, 0, 0, TimeSpan.Zero)), file.Modified)
                Assert.Equal(2_300_000_000L, file.Link.ApproximateBytes)
                Assert.Single handler.Calls |> ignore
            })

[<Fact>]
let ``Brazil resolves the newest month's thirty zips by PROPFIND, without fetching them`` () : Task =
    let share =
        "https://arquivos.receitafederal.gov.br/public.php/dav/files/gn672Ad4CF8N6TK/Dados/Cadastros/CNPJ/"

    withCache
        (listing
            [
                share + "2026-10/", fixture "resolve-br-month.xml"
                share, fixture "resolve-br-cnpj.xml"
            ])
        (fun handler cache _ ->
            task {
                let! files = resolved cache "BR"
                Assert.Equal(30, files.Length)
                Assert.Equal("Empresas0.zip", files[0].Link.Name)
                Assert.Equal(share + "2026-10/Empresas0.zip", string files[0].Link.Url)
                Assert.Equal(570_972_127L, files[0].Link.ApproximateBytes)
                Assert.Equal(ValueSome(utc "Tue, 13 Oct 2026 22:00:54 GMT"), files[0].Modified)
                Assert.Equal("Socios9.zip", (List.last files).Link.Name)

                Assert.Equal<(string * string) list>(
                    [ "PROPFIND", share; "PROPFIND", share + "2026-10/" ],
                    handler.Calls |> List.map (fun call -> call.Method, call.Uri)
                )

                Assert.All(handler.Calls, fun call -> Assert.Equal("1", call.Headers["Depth"]))
            })

[<Fact>]
let ``Slovenia resolves opsiprs.csv, dated by its package as the resource states no change`` () : Task =
    withCache
        (listing
            [
                "https://podatki.gov.si/api/3/action/package_show", fixture "resolve-si-package.json"
            ])
        (fun _ cache _ ->
            task {
                let! files = resolved cache "SI"
                let file = Assert.Single files
                Assert.Equal("opsiprs.csv", file.Link.Name)
                Assert.Equal("Poslovni register", file.PortalName)
                Assert.EndsWith("/download/opsiprs.csv", string file.Link.Url)
                Assert.Equal(126_476_286L, file.Link.ApproximateBytes)
                Assert.Equal(ValueSome(utc "2026-11-01T00:00:29.111990Z"), file.Modified)
            })

[<Fact>]
let ``a listing that can't be read fails the download, saying how to get the files by hand, and fetches nothing``
    ()
    : Task =
    withCache (fun _ -> answer 503 "down") (fun handler cache _ ->
        task {
            let asic = entryOf "AU-ASIC"

            match! cache.DownloadAsync(asic, ValueNone, ct) with
            | Error(RegistryFailure.Unavailable reason) ->
                Assert.Contains("data.gov.au, CKAN API didn't name the current files", reason)
                Assert.Contains("By hand: Download the weekly zip", reason)
            | other -> failwith $"%A{other}"

            Assert.Single handler.Calls |> ignore
            Assert.Equal(BulkCacheState.NotDownloaded, cache.StateOf asic)
            Assert.Empty(cache.ResolvedFilesOf asic)
        })

[<Fact>]
let ``a listing that names none of the files fails, saying how to get them by hand`` () : Task =
    withCache (fun _ -> ok """{"success":true,"result":{"resources":[]}}""") (fun _ cache _ ->
        task {
            match! cache.DownloadAsync(entryOf "UA", ValueNone, ct) with
            | Error(RegistryFailure.UnreadableAnswer reason) ->
                Assert.Contains("names no uo.zip", reason)
                Assert.Contains("By hand: Download uo.zip", reason)
            | other -> failwith $"%A{other}"
        })

[<Fact>]
let ``a CKAN answer saying it failed is unreadable`` () : Task =
    withCache (fun _ -> ok """{"success":false,"error":{"message":"Not found"}}""") (fun _ cache _ ->
        task {
            match! cache.ResolveAsync(entryOf "AU-ASIC", ct) with
            | Error(RegistryFailure.UnreadableAnswer reason) -> Assert.Contains("can't be read", reason)
            | other -> failwith $"%A{other}"
        })

/// ASIC's recorded listing with its zip's size replaced.
let private asicListingSized (bytes: int64) =
    (fixture "resolve-asic-package.json").Replace("79298881", string bytes)

[<Fact>]
let ``a resolved file larger than four times the size its listing states is refused`` () : Task =
    let respond (call: Call) =
        if call.Uri.Contains "/api/3/action/" then
            ok (asicListingSized 10L)
        else
            ok (String('x', 100))

    withCache respond (fun _ cache folder ->
        task {
            let asic = entryOf "AU-ASIC"

            match! cache.DownloadAsync(asic, ValueNone, ct) with
            | Error(RegistryFailure.UnreadableAnswer reason) ->
                Assert.Contains("company.zip is larger than expected", reason)
            | other -> failwith $"%A{other}"

            Assert.Equal(BulkCacheState.NotDownloaded, cache.StateOf asic)
            Assert.Empty(Directory.GetFiles(Path.Combine(folder, asic.Id)))
        })

[<Fact>]
let ``a listing whose sizes add up to more than four times the source's stated size is refused before any download``
    ()
    : Task =
    withCache (fun _ -> ok (asicListingSized 10_000_000_000L)) (fun handler cache _ ->
        task {
            match! cache.DownloadAsync(entryOf "AU-ASIC", ValueNone, ct) with
            | Error(RegistryFailure.UnreadableAnswer reason) ->
                Assert.Contains("more than 4 times", reason)
                Assert.Contains("By hand:", reason)
            | other -> failwith $"%A{other}"

            Assert.Single handler.Calls |> ignore
        })

[<Fact>]
let ``a resolved source downloads, loads, and lists its files with their portal names and dates`` () : Task =
    let respond (call: Call) =
        if call.Uri.Contains "/api/3/action/" then
            asicListingSized (int64 (fixture "asic-companies.csv").Length) |> ok
        else
            ok (fixture "asic-companies.csv")

    withCache respond (fun handler cache _ ->
        task {
            let asic = entryOf "AU-ASIC"

            match! cache.DownloadAsync(asic, ValueNone, ct) with
            | Ok(BulkCacheState.Downloaded(at, bytes, false)) ->
                Assert.Equal(now, at)
                Assert.True(bytes > 0L)
            | other -> failwith $"%A{other}"

            Assert.EndsWith("/download/company_202611.zip", (List.last handler.Calls).Uri)
            let saved = Assert.Single(cache.ResolvedFilesOf asic)
            Assert.Equal("company.zip", saved.Link.Name)
            Assert.Equal("Company Dataset - Current", saved.PortalName)
            Assert.Equal(ValueSome(utc "2026-11-02T13:51:17.043135Z"), saved.Modified)

            match! cache.LoadAsync(asic, ct) with
            | Ok registry ->
                use _ = registry

                match! registry.TryFindAsync("000 000 019", Jurisdiction.ofCode "AU", ct) with
                | Ok({ Value = Available(ValueSome company) } as evidence) ->
                    Assert.Equal("MONAKA PTY LTD", company.Name)
                    // The listing's date for the file is the data's date, not the download's.
                    Assert.Equal(now, evidence.RetrievedAt)
                    Assert.Equal(ValueSome(DateOnly(2026, 11, 2)), evidence.AsOf)
                | other -> failwith $"%A{other}"
            | Error failure -> failwith (RegistryFailure.describe failure)

            match cache.IndexStateOf asic with
            | BulkIndexState.Built bytes -> Assert.True(bytes > 0L)
            | other -> failwith $"%A{other}"
        })

[<Fact>]
let ``downloading again removes a file the new listing no longer names`` () : Task =
    let header = ((fixture "chile-2013.csv").Split('\n')[0]).TrimEnd('\r') + "\n"

    // Without the sizes the listing states, so the short files served aren't refused as pages sent in their place.
    let mutable packages =
        Text.RegularExpressions.Regex.Replace(fixture "resolve-cl-package.json", "\"size\":[0-9]+", "\"size\":null")

    let respond (call: Call) =
        if call.Uri.Contains "/api/3/action/" then
            ok packages
        elif call.Uri.Contains "/2013-sociedades" then
            ok (fixture "chile-2013.csv")
        else
            ok header

    withCache respond (fun _ cache folder ->
        task {
            let chile = entryOf "CL-RES"
            let! first = cache.DownloadAsync(chile, ValueNone, ct)
            Assert.True(Result.isOk first, $"%A{first}")
            let year2013 = Path.Combine(folder, chile.Id, "res-2013.csv")
            Assert.True(File.Exists year2013)

            match! cache.LoadAsync(chile, ct) with
            | Ok registry ->
                use _ = registry
                let! evidence = registry.TryFindAsync("76286049-K", Jurisdiction.ofCode "CL", ct)
                Assert.True(Result.isOk evidence)
            | Error failure -> failwith (RegistryFailure.describe failure)

            packages <- packages.Replace("Constituciones del año 2013", "Notas de 2013")
            let! second = cache.DownloadAsync(chile, ValueNone, ct)
            Assert.True(Result.isOk second, $"%A{second}")
            Assert.False(File.Exists year2013)
            Assert.Equal(13, (cache.ResolvedFilesOf chile).Length)
            Assert.Equal(BulkIndexState.NotBuilt, cache.IndexStateOf chile)
        })

[<Theory>]
[<InlineData("http://data.gov.au/data/dataset/", "isn't https")>]
[<InlineData("https://files.example.com/data/dataset/", "a host other than data.gov.au")>]
let ``a listing naming its file over plain http, or on another host, fails the download, saying how to get it by hand``
    (start: string, expected: string)
    : Task =
    let listing =
        (fixture "resolve-asic-package.json").Replace("https://data.gov.au/data/dataset/", start)

    withCache (fun _ -> ok listing) (fun handler cache _ ->
        task {
            match! cache.DownloadAsync(entryOf "AU-ASIC", ValueNone, ct) with
            | Error(RegistryFailure.RequestRefused reason) ->
                Assert.Contains(expected, reason)
                Assert.Contains("By hand: Download the weekly zip", reason)
            | other -> failwith $"%A{other}"

            Assert.Single handler.Calls |> ignore
        })

[<Fact>]
let ``Russia's zips may be served from the FNS file server`` () : Task =
    let meta =
        (fixture "resolve-ru-meta.csv")
            .Replace(
                "https://www.nalog.gov.ru/opendata/7707329152-rsmp/data-",
                "https://file.nalog.ru/opendata/7707329152-rsmp/data-"
            )

    withCache (listing [ "https://www.nalog.gov.ru/opendata/7707329152-rsmp/meta.csv", meta ]) (fun _ cache _ ->
        task {
            let! files = resolved cache "RU-SME"
            let file = Assert.Single files
            Assert.Equal("file.nalog.ru", file.Link.Url.Host)
        })
