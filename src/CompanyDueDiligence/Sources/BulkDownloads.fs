namespace CompanyDueDiligence

open System
open System.IO
open System.Net.Http
open System.Threading
open System.Threading.Tasks

/// How a bulk source's files are got.
[<RequireQualifiedAccess>]
type BulkDelivery =
    /// Fixed links BulkDownloads fetches on request.
    | Links of BulkFileLink list
    /// Files whose names change, which BulkDownloads finds through the portal's official listing when asked, then fetches. When the listing
    /// can't be read, the download fails, saying how to get them by hand.
    | Resolved of BulkResolver
    /// Files a person fetches, because they need a registration or SFTP, or no listing names them. Pass them to BulkSources.
    | ByHand of how: string

/// A bulk source as a settings screen lists it.
type BulkCatalogueEntry =
    {
        /// A stable key, such as "EE" or "CL-SII".
        Id: string
        Jurisdiction: Jurisdiction
        Title: string
        /// The publisher's own page.
        Page: Uri
        Delivery: BulkDelivery
        /// How often the publisher replaces the file. A copy older than this is stale.
        Refresh: TimeSpan
        Licence: string
    }

/// The bulk sources, with how to get each one's files. A host lists them for its users to pick; most users need few of them.
module BulkCatalogue =

    let private days (n: float) = TimeSpan.FromDays n

    let private link name url bytes =
        {
            Name = name
            Url = Uri url
            ApproximateBytes = bytes
        }

    let private entry id code title page delivery refresh licence =
        {
            Id = id
            Jurisdiction = Jurisdiction.ofCode code
            Title = title
            Page = Uri page
            Delivery = delivery
            Refresh = refresh
            Licence = licence
        }

    let private estoniaFiles =
        let base' =
            "https://avaandmed.ariregister.rik.ee/sites/default/files/avaandmed/ettevotja_rekvisiidid__"

        [
            link "yldandmed.json.zip" (base' + "yldandmed.json.zip") 230_986_600L
            link "kaardile_kantud_isikud.json.zip" (base' + "kaardile_kantud_isikud.json.zip") 46_105_138L
            link "osanikud.json.zip" (base' + "osanikud.json.zip") 33_973_062L
            link "kasusaajad.json.zip" (base' + "kasusaajad.json.zip") 27_936_138L
        ]

    let private paraguayFiles =
        [
            for digit in 0..9 ->
                link $"ruc{digit}.zip" $"https://www.dnit.gov.py/documents/20123/662360/ruc{digit}.zip" 4_000_000L
        ]

    let all =
        [
            entry
                "EE"
                "EE"
                "Estonia: e-Business Register"
                "https://avaandmed.ariregister.rik.ee/en/downloading-open-data"
                (BulkDelivery.Links estoniaFiles)
                (days 1.)
                "CC BY 4.0; downloading accepts the register's terms"
            entry
                "CY"
                "CY"
                "Cyprus: Registrar of Companies"
                "https://www.data.gov.cy/en/dataset/1026"
                (BulkDelivery.Resolved BulkResolvers.cyprus)
                (days 31.)
                "CC BY 4.0"
            entry
                "RO"
                "RO"
                "Romania: ONRC"
                "https://data.gov.ro/organization/onrc"
                (BulkDelivery.Resolved BulkResolvers.romania)
                (days 15.)
                "CC BY 4.0"
            entry
                "SI"
                "SI"
                "Slovenia: AJPES business register"
                "https://podatki.gov.si/dataset/poslovni-register-slovenije"
                (BulkDelivery.Resolved BulkResolvers.slovenia)
                (days 15.)
                "CC BY 4.0"
            entry
                "BE"
                "BE"
                "Belgium: CBE/KBO open data"
                "https://kbopub.economie.fgov.be/kbo-open-data/"
                (BulkDelivery.ByHand "Register on the page, state your purpose, then download the full zip")
                (days 1.)
                "CBE licence: reuse only for the purpose registered, no direct marketing"
            entry
                "UA"
                "UA"
                "Ukraine: ЄДР legal entities"
                "https://data.gov.ua/dataset/03cc1239-3988-4451-aa0d-aadb77448714"
                (BulkDelivery.Resolved BulkResolvers.ukraine)
                (days 7.)
                "CC BY"
            entry
                "MD"
                "MD"
                "Moldova: State Register of Legal Units"
                "https://www.asp.gov.md/ro/date-deschise/date-statistice/company"
                (BulkDelivery.ByHand "Download company.xlsx from the page; its path holds the year")
                (days 31.)
                "None stated"
            entry
                "RS"
                "RS"
                "Serbia: APR companies"
                "https://data.gov.rs/"
                (BulkDelivery.Links
                    [
                        link "companies.json" "https://openapi.apr.gov.rs/api/opendata/companies" 57_700_000L
                    ])
                (days 31.)
                "Serbian Open Data Licence"
            entry
                "BA-BRC"
                "BA-BRC"
                "Bosnia and Herzegovina, Brčko: e-Registar"
                "https://bizreg.osbd.ba/Public/PublicPortal/OpenData"
                (BulkDelivery.Links
                    [
                        link
                            "opendata.xml"
                            "https://bizreg.osbd.ba/Public/PublicPortal/OpenData?handler=ExportXML"
                            4_310_246L
                    ])
                (days 31.)
                "None stated; the site says all rights reserved"
            entry
                "RU-SME"
                "RU"
                "Russia: FNS register of small and medium businesses"
                "https://www.nalog.gov.ru/opendata/7707329152-rsmp/"
                (BulkDelivery.Resolved BulkResolvers.russiaSme)
                (days 31.)
                "FNS open data terms, attribution required"
            entry
                "US-FL"
                "US-FL"
                "Florida: Division of Corporations"
                "https://dos.fl.gov/sunbiz/other-services/data-downloads/"
                (BulkDelivery.ByHand "Fetch cordata.zip and the daily files over SFTP with the public login on the page")
                (days 90.)
                "None stated"
            entry
                "US-AK"
                "US-AK"
                "Alaska: corporations downloads"
                "https://www.commerce.alaska.gov/cbp/main/DbDownload"
                (BulkDelivery.Links
                    [
                        link
                            "corporations.csv"
                            "https://www.commerce.alaska.gov/cbp/main/DbDownload/CorporationsDownload"
                            43_010_729L
                        link
                            "officials.csv"
                            "https://www.commerce.alaska.gov/cbp/main/DbDownload/OfficialsDownload"
                            40_000_000L
                    ])
                (days 7.)
                "None stated"
            entry
                "PY"
                "PY"
                "Paraguay: DNIT RUC list"
                "https://www.dnit.gov.py/web/portal-institucional/listado-de-ruc-con-sus-equivalencias"
                (BulkDelivery.Links paraguayFiles)
                (days 31.)
                "None stated"
            entry
                "BR"
                "BR"
                "Brazil: Receita Federal CNPJ"
                "https://arquivos.receitafederal.gov.br/"
                (BulkDelivery.Resolved BulkResolvers.brazil)
                (days 31.)
                "Unverified"
            entry
                "AU-ASIC"
                "AU"
                "Australia: ASIC company dataset"
                "https://data.gov.au/data/dataset/asic-companies"
                (BulkDelivery.Resolved BulkResolvers.asic)
                (days 7.)
                "CC BY 3.0 AU"
            entry
                "CL-RES"
                "CL"
                "Chile: Registro de Empresas y Sociedades"
                "https://datos.gob.cl/dataset/registro-de-empresas-y-sociedades"
                (BulkDelivery.Resolved BulkResolvers.chile)
                (days 365.)
                "CC BY"
            entry
                "CL-SII"
                "CL"
                "Chile: SII nómina de personas jurídicas"
                "https://www.sii.cl/estadisticas/nominas/"
                (BulkDelivery.Links
                    [
                        link
                            "PUB_NOMBRES_PJ.zip"
                            "https://www.sii.cl/estadisticas/nominas/PUB_NOMBRES_PJ.zip"
                            52_385_281L
                        link
                            "PUB_NOM_DIRECCIONES.zip"
                            "https://www.sii.cl/estadisticas/nominas/PUB_NOM_DIRECCIONES.zip"
                            95_040_204L
                    ])
                (days 90.)
                "None stated; SII disclaims accuracy"
            entry
                "AR-C"
                "AR-C"
                "Buenos Aires: IGJ entities"
                "https://datos.jus.gob.ar/dataset/entidades-constituidas-en-la-inspeccion-general-de-justicia-igj"
                (BulkDelivery.Resolved BulkResolvers.argentinaCaba)
                (days 183.)
                "CC BY 4.0"
            entry
                "PE"
                "PE"
                "Peru: SUNAT padrón reducido"
                "https://www.sunat.gob.pe/descargaPRR/mrc137_padron_reducido.html"
                (BulkDelivery.Links
                    [
                        link "padron_reducido_ruc.zip" "https://www2.sunat.gob.pe/padron_reducido_ruc.zip" 394_837_914L
                    ])
                (days 1.)
                "None stated"
            entry
                "EC"
                "EC"
                "Ecuador: Supercias directorio"
                "https://mercadodevalores.supercias.gob.ec/"
                (BulkDelivery.Links
                    [
                        link
                            "directorio_companias.xlsx"
                            "https://mercadodevalores.supercias.gob.ec/reportes/excel/directorio_companias.xlsx"
                            37_415_556L
                    ])
                (days 1.)
                "None stated"
        ]

    let tryFind (id: string) =
        all |> List.tryFind (fun entry -> entry.Id = id) |> ValueOption.ofOption

/// What the cache holds for one bulk source.
[<Struct; RequireQualifiedAccess>]
type BulkCacheState =
    | NotDownloaded
    /// Every file is saved. Stale once older than the entry's Refresh.
    | Downloaded of at: DateTimeOffset * bytes: int64 * stale: bool
    /// The source is fetched by hand, so the cache doesn't hold it.
    | ByHand

/// Whether a bulk source's index on disk is built from the files saved now.
[<Struct; RequireQualifiedAccess>]
type BulkIndexState =
    | NotBuilt
    /// Built from the files saved now, taking this many bytes.
    | Built of bytes: int64

/// How far a download has got.
[<Struct>]
type BulkProgress =
    {
        File: string
        Bytes: int64
        Expected: int64 voption
    }

/// Downloads bulk sources with fixed links or a resolver into a folder the host chooses, only when asked, and builds registries from what is
/// saved there, each with an index on disk beside its files. Nothing is fetched in the background: a host calls DownloadAsync from a settings
/// screen or a schedule. A download that receives nothing for idle, on its headers or a read, fails as Unavailable.
[<Sealed>]
type BulkDownloads
    internal (cacheDirectory: string, handler: HttpMessageHandler, now: unit -> DateTimeOffset, idle: TimeSpan) =

    /// When the download was saved, then the date its publisher states for it, if any.
    static let stampName = "downloaded.txt"

    /// The files a resolver named for the download saved now, one per line.
    static let listName = "files.tsv"

    static let indexName = "index"

    static let userAgent =
        "CompanyDueDiligence/0.3 (+https://github.com/Thorium/CompanyDueDiligence)"

    /// Names this library, and accepts any type: some portals' firewalls refuse a request that states no Accept.
    static let identify (client: HttpClient) =
        client.DefaultRequestHeaders.UserAgent.ParseAdd userAgent
        client.DefaultRequestHeaders.Accept.ParseAdd "*/*"
        client

    let http =
        new HttpClient(handler, false, Timeout = Timeout.InfiniteTimeSpan) |> identify

    /// For listings: a listing is small, so one larger than 32 MB is refused.
    let listings =
        new HttpClient(handler, false, Timeout = TimeSpan.FromMinutes(2.), MaxResponseContentBufferSize = 33_554_432L)
        |> identify

    let folderOf (entry: BulkCatalogueEntry) = Path.Combine(cacheDirectory, entry.Id)
    let stampOf entry = Path.Combine(folderOf entry, stampName)
    let listOf entry = Path.Combine(folderOf entry, listName)
    let indexOf entry = Path.Combine(folderOf entry, indexName)

    let invariant = Globalization.CultureInfo.InvariantCulture

    /// One line of files.tsv: the name saved, URL, size, the name on the portal and when it last changed.
    let lineOf (file: BulkResolvedFile) =
        String.Join(
            "\t",
            file.Link.Name,
            string file.Link.Url,
            file.Link.ApproximateBytes.ToString invariant,
            file.PortalName,
            file.Modified
            |> ValueOption.map _.ToString("o", invariant)
            |> ValueOption.defaultValue ""
        )

    let fileOf (line: string) =
        match line.Split '\t' with
        | [| name; url; bytes; portalName; modified |] ->
            match Uri.TryCreate(url, UriKind.Absolute), Int64.TryParse(bytes, invariant) with
            | (true, link), (true, size) ->
                ValueSome
                    {
                        Link =
                            {
                                Name = name
                                Url = link
                                ApproximateBytes = size
                            }
                        PortalName = portalName
                        Modified =
                            match
                                DateTimeOffset.TryParse(
                                    modified,
                                    invariant,
                                    Globalization.DateTimeStyles.AssumeUniversal
                                )
                            with
                            | true, at -> ValueSome at
                            | _ -> ValueNone
                    }
            | _ -> ValueNone
        | _ -> ValueNone

    /// The files a resolver named for the download saved now; empty when there is none.
    let listed (entry: BulkCatalogueEntry) =
        let list = listOf entry

        if File.Exists list then
            File.ReadAllLines list
            |> Array.choose (fileOf >> ValueOption.toOption)
            |> List.ofArray
        else
            []

    /// The files of the download saved now, or that a download saves: the fixed links, or what the resolver named.
    let savedLinks (entry: BulkCatalogueEntry) =
        match entry.Delivery with
        | BulkDelivery.Links links -> links
        | BulkDelivery.Resolved _ -> listed entry |> List.map _.Link
        | BulkDelivery.ByHand _ -> []

    let inputsOf (entry: BulkCatalogueEntry) (links: BulkFileLink list) : BulkIndexInput list =
        links
        |> List.map (fun link ->
            let info = FileInfo(Path.Combine(folderOf entry, link.Name))

            {
                Name = link.Name
                Bytes = info.Length
                Modified = DateTimeOffset info.LastWriteTimeUtc
            })

    /// The manifest of the entry's index, when this library version built it from the files saved now: the same download, and each file of the
    /// same size and time.
    let freshIndex (entry: BulkCatalogueEntry) (date: BulkFileDate) (links: BulkFileLink list) =
        match BulkStore.readManifest (indexOf entry) with
        | Ok manifest when
            manifest.Library = ValueSome BulkStore.Library
            && manifest.Source = entry.Id
            && manifest.Date.DownloadedAt = date.DownloadedAt
            && manifest.Inputs = inputsOf entry links
            ->
            ValueSome manifest
        | _ -> ValueNone

    /// The stamp's dates: when the download was saved, and the date its publisher states, from the second line when there is one.
    let stamped entry =
        let stamp = stampOf entry

        if File.Exists stamp then
            match File.ReadAllLines stamp |> List.ofArray with
            | first :: rest ->
                match DateTimeOffset.TryParse(first, invariant, Globalization.DateTimeStyles.AssumeUniversal) with
                | true, at ->
                    ValueSome
                        {
                            DownloadedAt = at
                            AsOf =
                                match rest with
                                | day :: _ ->
                                    match
                                        DateOnly.TryParseExact(
                                            day.Trim(),
                                            "yyyy-MM-dd",
                                            invariant,
                                            Globalization.DateTimeStyles.None
                                        )
                                    with
                                    | true, asOf -> ValueSome asOf
                                    | _ -> ValueNone
                                | [] -> ValueNone
                        }
                | _ -> ValueNone
            | [] -> ValueNone
        else
            ValueNone

    let partOf (folder: string) (link: BulkFileLink) =
        Path.Combine(folder, link.Name + ".part")

    /// Fetches link to its part file and leaves it there. Refuses a file larger than BulkSize.Margin times its expected size, or, when its
    /// catalogue or listing states a size, smaller than a twentieth of that, as a page sent in place of the file is. Nothing received for
    /// idle, on the headers or a read, is Unavailable; cancelling ct throws. Gives the date the answer's Last-Modified states.
    let fetch
        (link: BulkFileLink)
        (stated: int64 voption)
        (folder: string)
        (progress: IProgress<BulkProgress> voption)
        (ct: CancellationToken)
        =
        task {
            let part = partOf folder link
            // Cancelled by ct, or once idle passes after the last time it was set.
            use idleness = CancellationTokenSource.CreateLinkedTokenSource ct

            try
                idleness.CancelAfter idle

                use! response =
                    http.GetAsync(link.Url, HttpCompletionOption.ResponseHeadersRead, idleness.Token)

                if not response.IsSuccessStatusCode then
                    let status = int response.StatusCode

                    return
                        Error(
                            if status >= 500 then
                                RegistryFailure.Unavailable $"HTTP {status} for {link.Name}"
                            else
                                RegistryFailure.RequestRefused $"HTTP {status} for {link.Name}"
                        )
                else
                    let expected =
                        let length = response.Content.Headers.ContentLength

                        if length.HasValue then
                            ValueSome length.Value
                        else
                            ValueNone

                    let modified =
                        let stamp = response.Content.Headers.LastModified

                        if stamp.HasValue then ValueSome stamp.Value else ValueNone

                    // From the catalogue's size or the listing's, never from what this answer declares.
                    let limit = link.ApproximateBytes * BulkSize.Margin
                    let floor = stated |> ValueOption.map (fun bytes -> bytes / 20L)

                    let tooLarge =
                        RegistryFailure.UnreadableAnswer $"{link.Name} is larger than expected"

                    let tooSmall (bytes: int64) =
                        RegistryFailure.UnreadableAnswer
                            $"{link.Name} is {bytes} bytes, far smaller than the {stated |> ValueOption.defaultValue 0L} expected: perhaps a page in place of the file"

                    let below (bytes: int64) =
                        floor |> ValueOption.exists (fun least -> bytes < least)

                    match expected with
                    | ValueSome declared when declared > limit -> return Error tooLarge
                    | ValueSome declared when below declared -> return Error(tooSmall declared)
                    | _ ->
                        let! written =
                            if OperatingSystem.IsBrowser() then
                                // One read of the whole body: in WebAssembly a task awaiting inside a loop blocks, which the runtime refuses.
                                task {
                                    idleness.CancelAfter Timeout.InfiniteTimeSpan
                                    let! bytes = response.Content.ReadAsByteArrayAsync idleness.Token

                                    if int64 bytes.Length > limit then
                                        return Error tooLarge
                                    else
                                        do! File.WriteAllBytesAsync(part, bytes, ct)
                                        let total = int64 bytes.Length

                                        progress
                                        |> ValueOption.iter (fun report ->
                                            report.Report
                                                {
                                                    File = link.Name
                                                    Bytes = total
                                                    Expected = expected
                                                })

                                        return Ok total
                                }
                            else
                                task {
                                    idleness.CancelAfter idle
                                    use! source = response.Content.ReadAsStreamAsync idleness.Token

                                    use target =
                                        new FileStream(
                                            part,
                                            FileMode.Create,
                                            FileAccess.Write,
                                            FileShare.None,
                                            1 <<< 16,
                                            true
                                        )

                                    let buffer = Array.zeroCreate<byte>(1 <<< 16)
                                    // A loop over reads, since a recursive task can't be compiled as a state machine. Never run in WebAssembly.
                                    let mutable total = 0L
                                    let mutable outcome = ValueNone

                                    while outcome.IsNone do
                                        idleness.CancelAfter idle
                                        let! read = source.ReadAsync(buffer.AsMemory(), idleness.Token)

                                        if read = 0 then
                                            outcome <- ValueSome(Ok total)
                                        elif total + int64 read > limit then
                                            outcome <- ValueSome(Error tooLarge)
                                        else
                                            do! target.WriteAsync(buffer.AsMemory(0, read), ct)
                                            let before = total
                                            total <- total + int64 read

                                            // Once a megabyte, so a screen showing it isn't redrawn for every read.
                                            if total / 1_000_000L > before / 1_000_000L then
                                                progress
                                                |> ValueOption.iter (fun report ->
                                                    report.Report
                                                        {
                                                            File = link.Name
                                                            Bytes = total
                                                            Expected = expected
                                                        })

                                    return outcome.Value
                                }

                        return
                            match written with
                            | Ok total when below total ->
                                File.Delete part
                                Error(tooSmall total)
                            | Ok _ -> Ok modified
                            | Error failure ->
                                File.Delete part
                                Error failure
            with :? OperationCanceledException when not ct.IsCancellationRequested ->
                File.Delete part

                return Error(RegistryFailure.Unavailable $"{link.Name} sent nothing for {idle.TotalSeconds} seconds")
        }

    /// The entry's registry built from its saved files, as of the download's date.
    let build
        (entry: BulkCatalogueEntry)
        (date: BulkFileDate)
        (links: BulkFileLink list)
        (ct: CancellationToken)
        : Task<Result<ICompanyRegistry, RegistryFailure>> =
        task {
            let opened = Collections.Generic.List<Stream>()
            let names = links |> List.map _.Name

            let openFile name =
                let stream = File.OpenRead(Path.Combine(folderOf entry, name)) :> Stream
                opened.Add stream
                stream

            let maybe name =
                if names |> List.contains name then
                    ValueSome(openFile name)
                else
                    ValueNone

            let starting (prefix: string) =
                names
                |> List.filter (fun name -> name.StartsWith(prefix, StringComparison.Ordinal))
                |> List.map openFile

            try
                return!
                    match entry.Id with
                    | "EE" ->
                        BulkSources.estoniaAsync
                            (openFile "yldandmed.json.zip")
                            (ValueSome(openFile "kaardile_kantud_isikud.json.zip"))
                            (ValueSome(openFile "osanikud.json.zip"))
                            (ValueSome(openFile "kasusaajad.json.zip"))
                            date
                            ct
                    | "RS" -> BulkSources.serbiaAsync (openFile "companies.json") date ct
                    | "BA-BRC" -> BulkSources.bosniaBrckoAsync (openFile "opendata.xml") date ct
                    | "US-AK" ->
                        BulkSources.alaskaAsync
                            (openFile "corporations.csv")
                            (ValueSome(openFile "officials.csv"))
                            date
                            ct
                    | "PY" -> BulkSources.paraguayAsync [ for digit in 0..9 -> openFile $"ruc{digit}.zip" ] date ct
                    | "CL-SII" ->
                        BulkSources.chileSiiAsync
                            (openFile "PUB_NOMBRES_PJ.zip")
                            (openFile "PUB_NOM_DIRECCIONES.zip")
                            date
                            ct
                    | "PE" -> BulkSources.peruAsync (openFile "padron_reducido_ruc.zip") date ct
                    | "EC" -> BulkSources.ecuadorAsync (openFile "directorio_companias.xlsx") date ct
                    | "CY" ->
                        BulkSources.cyprusAsync
                            (openFile "organisations.csv")
                            (maybe "registered_office.csv")
                            (maybe "organisation_officials.csv")
                            date
                            ct
                    | "RO" ->
                        BulkSources.romaniaAsync
                            (openFile "OD_FIRME.CSV")
                            (openFile "OD_STARE_FIRMA.CSV")
                            (maybe "N_STARE_FIRMA.CSV")
                            (maybe "OD_REPREZENTANTI_LEGALI.CSV")
                            date
                            ct
                    | "SI" -> BulkSources.sloveniaAsync (openFile "opsiprs.csv") date ct
                    | "UA" -> BulkSources.ukraineAsync (openFile "uo.zip") date ct
                    | "RU-SME" -> BulkSources.russiaSmeAsync (openFile "rsmp.zip") date ct
                    | "BR" ->
                        BulkSources.brazilAsync
                            (starting "Empresas")
                            (starting "Estabelecimentos")
                            (starting "Socios")
                            date
                            ct
                    | "AU-ASIC" -> BulkSources.asicCompaniesAsync (openFile "company.zip") date ct
                    | "CL-RES" -> BulkSources.chileAsync (starting "res-") date ct
                    | "AR-C" -> BulkSources.argentinaCabaAsync (openFile "igj.zip") date ct
                    | other -> Task.FromResult(Error(RegistryFailure.InvalidRequest $"No loader for {other}"))
            finally
                for stream in opened do
                    stream.Dispose()
        }

    let dispose (registry: ICompanyRegistry) =
        match registry with
        | :? IDisposable as disposable -> disposable.Dispose()
        | _ -> ()

    /// Saves the entry's index from its files: Ukraine's written as uo.zip streams, any other's from the built registry, which is the answer
    /// when the index can't be written.
    let index
        (entry: BulkCatalogueEntry)
        (date: BulkFileDate)
        (links: BulkFileLink list)
        (ct: CancellationToken)
        : Task<Result<ICompanyRegistry voption, RegistryFailure>> =
        task {
            let inputs = inputsOf entry links
            let directory = indexOf entry

            if entry.Id = "UA" then
                use uo = File.OpenRead(Path.Combine(folderOf entry, "uo.zip"))

                match! BulkIndexes.saveUkraineWith inputs uo date directory ct with
                | Ok() -> return Ok ValueNone
                | Error(RegistryFailure.Unavailable _) ->
                    match! build entry date links ct with
                    | Ok built -> return Ok(ValueSome built)
                    | Error failure -> return Error failure
                | Error failure -> return Error failure
            else
                match! build entry date links ct with
                | Error failure -> return Error failure
                | Ok built ->
                    match! BulkIndexes.saveWith inputs entry.Id built directory ct with
                    | Error _ -> return Ok(ValueSome built)
                    | Ok() ->
                        dispose built
                        return Ok ValueNone
        }

    /// The resolver's failure, with how to get the files by hand.
    let withByHand (resolver: BulkResolver) (failure: RegistryFailure) =
        let byHand reason =
            $"{resolver.Portal} didn't name the current files ({reason}). By hand: {resolver.ByHand}"

        match failure with
        | RegistryFailure.Unavailable reason -> RegistryFailure.Unavailable(byHand reason)
        | RegistryFailure.RequestRefused reason -> RegistryFailure.RequestRefused(byHand reason)
        | RegistryFailure.UnreadableAnswer reason -> RegistryFailure.UnreadableAnswer(byHand reason)
        | RegistryFailure.AccessRefused reason -> RegistryFailure.AccessRefused(byHand reason)
        | RegistryFailure.InvalidRequest reason -> RegistryFailure.InvalidRequest(byHand reason)
        | RegistryFailure.RateLimited _ -> RegistryFailure.Unavailable(byHand "rate limited")
        | RegistryFailure.OutsideCoverage _ -> RegistryFailure.Unavailable(byHand "outside its coverage")

    /// The files a download of the entry fetches now, each with the size its catalogue or listing states, if one does.
    let resolve
        (entry: BulkCatalogueEntry)
        (ct: CancellationToken)
        : Task<Result<(BulkResolvedFile * int64 voption) list, RegistryFailure>> =
        match entry.Delivery with
        | BulkDelivery.ByHand how ->
            Task.FromResult(Error(RegistryFailure.InvalidRequest $"{entry.Title} is fetched by hand: {how}"))
        | BulkDelivery.Links links ->
            Task.FromResult(
                Ok(
                    links
                    |> List.map (fun link ->
                        {
                            Link = link
                            PortalName = BulkListing.fileNameOf link.Url
                            Modified = ValueNone
                        },
                        ValueSome link.ApproximateBytes)
                )
            )
        | BulkDelivery.Resolved resolver ->
            task {
                match! resolver.ResolveAsync(listings, ct) with
                | Ok files -> return Ok files
                | Error failure -> return Error(withByHand resolver failure)
            }

    /// The saved download's dates and files, when it is stamped and every file is there.
    let saved (entry: BulkCatalogueEntry) =
        match stamped entry, savedLinks entry with
        | ValueSome date, (_ :: _ as links) when
            links
            |> List.forall (fun link -> File.Exists(Path.Combine(folderOf entry, link.Name)))
            ->
            ValueSome(date, links)
        | _ -> ValueNone

    /// A cache over a caller-owned handler, giving up on a download that receives nothing for two minutes.
    new(cacheDirectory: string, handler: HttpMessageHandler, now: unit -> DateTimeOffset) =
        new BulkDownloads(cacheDirectory, handler, now, TimeSpan.FromMinutes 2.)

    /// A cache over its own handler, which follows up to five redirects, as published links often move to a signed address, and gives up
    /// on a connection not made in 30 seconds.
    static member Create(cacheDirectory: string) =
        new BulkDownloads(
            cacheDirectory,
            new SocketsHttpHandler(
                UseCookies = false,
                AllowAutoRedirect = true,
                MaxAutomaticRedirections = 5,
                ConnectTimeout = TimeSpan.FromSeconds 30.
            ),
            fun () -> DateTimeOffset.UtcNow
        )

    /// A cache over a caller-owned handler, such as a browser's.
    static member Create(cacheDirectory: string, handler: HttpMessageHandler) =
        new BulkDownloads(cacheDirectory, handler, fun () -> DateTimeOffset.UtcNow)

    member _.StateOf(entry: BulkCatalogueEntry) =
        match entry.Delivery with
        | BulkDelivery.ByHand _ -> BulkCacheState.ByHand
        | BulkDelivery.Links _
        | BulkDelivery.Resolved _ ->
            match saved entry with
            | ValueSome(date, links) ->
                let bytes =
                    links
                    |> List.sumBy (fun link -> FileInfo(Path.Combine(folderOf entry, link.Name)).Length)

                BulkCacheState.Downloaded(date.DownloadedAt, bytes, now () - date.DownloadedAt > entry.Refresh)
            | ValueNone -> BulkCacheState.NotDownloaded

    /// The files the resolver named for the download saved now, with their names on the portal and dates. Empty for an entry without a
    /// resolver, or not downloaded.
    member this.ResolvedFilesOf(entry: BulkCatalogueEntry) =
        match this.StateOf entry, entry.Delivery with
        | BulkCacheState.Downloaded _, BulkDelivery.Resolved _ -> listed entry
        | _ -> []

    /// The files a download of the entry would fetch now: its fixed links, or what its resolver's listing names, read now. Nothing is
    /// downloaded. InvalidRequest for a source fetched by hand; a listing that can't be read, or that names a file not over https or on a
    /// host the resolver doesn't allow, fails saying how to get the files by hand.
    member _.ResolveAsync
        (entry: BulkCatalogueEntry, ct: CancellationToken)
        : Task<Result<BulkResolvedFile list, RegistryFailure>> =
        task {
            match! resolve entry ct with
            | Ok files -> return Ok(files |> List.map fst)
            | Error failure -> return Error failure
        }

    /// Whether the entry's index is built, by this library version, from the files saved now. LoadAsync builds it when it isn't.
    member _.IndexStateOf(entry: BulkCatalogueEntry) =
        match entry.Delivery with
        | BulkDelivery.ByHand _ -> BulkIndexState.NotBuilt
        | BulkDelivery.Links _
        | BulkDelivery.Resolved _ ->
            match
                saved entry
                |> ValueOption.bind (fun (date, links) -> freshIndex entry date links)
            with
            | ValueSome manifest ->
                BulkIndexState.Built(manifest.RecordsBytes + manifest.KeysBytes + manifest.NamesBytes)
            | ValueNone -> BulkIndexState.NotBuilt

    /// Fetches every file of the entry, each to a part file. Once all are fetched, removes the stamp and the index of the download before,
    /// moves every part file over its file, and stamps the folder with the time and the date the publisher states. A resolver's listing is
    /// read first; when it can't be read, nothing is fetched and the failure says how to get the files by hand. A failure or cancellation
    /// deletes the part files and leaves the last complete download, its stamp and its index as they were.
    member this.DownloadAsync
        (entry: BulkCatalogueEntry, progress: IProgress<BulkProgress> voption, ct: CancellationToken)
        =
        task {
            match! resolve entry ct with
            | Error failure -> return Error failure
            | Ok resolved ->
                let folder = folderOf entry
                let files = resolved |> List.map fst
                let links = files |> List.map _.Link
                Directory.CreateDirectory folder |> ignore

                let discard () =
                    for link in links do
                        File.Delete(partOf folder link)

                // Each file after the one before, as a chain of tasks: a loop awaiting inside one task blocks in WebAssembly. Gives each
                // file's stated date, last first.
                let each () =
                    resolved
                    |> List.fold
                        (fun (previous: Task<Result<DateTimeOffset voption list, RegistryFailure>>) (file, stated) ->
                            task {
                                match! previous with
                                | Error failure -> return Error failure
                                | Ok dates ->
                                    match! fetch file.Link stated folder progress ct with
                                    | Error failure -> return Error failure
                                    | Ok modified -> return Ok((file.Modified |> ValueOption.orElse modified) :: dates)
                            })
                        (Task.FromResult(Ok []))

                try
                    match! each () with
                    | Error failure ->
                        discard ()
                        return Error failure
                    | Ok dates ->
                        let asOf =
                            match dates |> List.choose ValueOption.toOption with
                            | [] -> ValueNone
                            | known -> ValueSome(DateOnly.FromDateTime (List.min known).DateTime)

                        // Without a stamp the folder holds no download, until every file is in place.
                        File.Delete(stampOf entry)
                        let index = indexOf entry

                        if Directory.Exists index then
                            Directory.Delete(index, true)

                        for link in links do
                            File.Move(partOf folder link, Path.Combine(folder, link.Name), true)

                        match entry.Delivery with
                        | BulkDelivery.Resolved _ ->
                            let kept = links |> List.map _.Name |> Set.ofList

                            // A file of the listing before that this one doesn't name, such as an older year's, goes.
                            for previous in listed entry do
                                let path = Path.Combine(folder, previous.Link.Name)

                                if not (kept.Contains previous.Link.Name) && File.Exists path then
                                    File.Delete path

                            File.WriteAllLines(listOf entry, files |> List.map lineOf)
                        | BulkDelivery.Links _
                        | BulkDelivery.ByHand _ -> ()

                        File.WriteAllLines(
                            stampOf entry,
                            [
                                now().ToString("o", invariant)
                                asOf
                                |> ValueOption.map _.ToString("yyyy-MM-dd", invariant)
                                |> ValueOption.defaultValue ""
                            ]
                        )

                        return Ok(this.StateOf entry)
                with
                | :? OperationCanceledException when ct.IsCancellationRequested ->
                    discard ()
                    return raise (OperationCanceledException ct)
                | error ->
                    discard ()
                    return Error(RegistryFailure.Unavailable error.Message)
        }

    /// Opens the entry's index when this library version built it from the files saved now. Otherwise builds the registry from the files, as
    /// of the download's dates, saves its index, lets the built registry go and opens the index; if the index can't be written or opened, the
    /// built registry is the answer. Ukraine's index is written as uo.zip streams, without building the registry. In WebAssembly the registry
    /// is built and no index is kept. An opened index holds its files open until it is disposed.
    member _.LoadAsync
        (entry: BulkCatalogueEntry, ct: CancellationToken)
        : Task<Result<ICompanyRegistry, RegistryFailure>> =
        task {
            match entry.Delivery, saved entry with
            | BulkDelivery.ByHand _, _ ->
                return
                    Error(
                        RegistryFailure.InvalidRequest
                            $"{entry.Title} is fetched by hand: pass its files to BulkSources"
                    )
            | _, ValueNone -> return Error(RegistryFailure.InvalidRequest $"{entry.Title} isn't downloaded")
            | _, ValueSome(date, links) ->
                if OperatingSystem.IsBrowser() then
                    return! build entry date links ct
                else
                    let directory = indexOf entry

                    let! opened =
                        match freshIndex entry date links with
                        | ValueSome _ -> BulkIndexes.openAsync directory ct
                        | ValueNone -> Task.FromResult(Error(RegistryFailure.RequestRefused "No index of these files"))

                    match opened with
                    | Ok registry -> return Ok registry
                    | Error _ ->
                        match! index entry date links ct with
                        | Error failure -> return Error failure
                        | Ok(ValueSome built) -> return Ok built
                        | Ok ValueNone ->
                            match! BulkIndexes.openAsync directory ct with
                            | Ok registry -> return Ok registry
                            | Error _ -> return! build entry date links ct
        }

    /// Deletes the entry's saved files and its index.
    member _.Remove(entry: BulkCatalogueEntry) =
        let folder = folderOf entry

        if Directory.Exists folder then
            Directory.Delete(folder, true)

    interface IDisposable with
        member _.Dispose() =
            http.Dispose()
            listings.Dispose()
