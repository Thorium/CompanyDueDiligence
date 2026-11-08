namespace CompanyDueDiligence

open System
open System.Globalization
open System.Net
open System.Net.Http
open System.Text.Json
open System.Text.RegularExpressions
open System.Threading
open System.Threading.Tasks
open System.Xml.Linq

/// One file a bulk source publishes at a fixed link.
type BulkFileLink =
    {
        /// The name it is saved under.
        Name: string
        Url: Uri
        /// The size seen on 2026-11-08, or the size the portal's listing states, to show before downloading. A download larger than four times
        /// this is refused.
        ApproximateBytes: int64
    }

/// A file a portal's listing names, as BulkDownloads saves it.
type BulkResolvedFile =
    {
        /// Where it is fetched from, the name its loader reads it under, and its size: as the listing states, or the source's stated size when
        /// the listing states none.
        Link: BulkFileLink
        /// Its name on the portal, such as company_202611.zip.
        PortalName: string
        /// When the portal says it last changed.
        Modified: DateTimeOffset voption
    }

/// A file as a portal's listing states it.
type internal ListedFile =
    {
        Name: string
        Url: Uri
        Bytes: int64 voption
        Modified: DateTimeOffset voption
        Format: string
    }

/// A listed file picked for a loader, with the name the loader reads.
type internal PickedFile = { SaveAs: string; File: ListedFile }

module internal BulkSize =

    /// A download, or a listing, larger than this many times its expected size is refused, so a wrong or hostile answer can't fill the disk.
    [<Literal>]
    let Margin = 4L

/// An official, documented listing that names a bulk source's current files. BulkDownloads reads it when the source is downloaded. A file the
/// listing names must be served over https, from the listing's own host or one of hosts.
[<Sealed>]
type BulkResolver
    internal
    (
        portal: string,
        dataset: string,
        listing: Uri,
        approximateBytes: int64,
        byHand: string,
        hosts: string list,
        read: HttpClient -> CancellationToken -> Task<Result<PickedFile list, RegistryFailure>>
    ) =

    /// Why a file the listing names isn't fetched: not over https, or from a host the resolver doesn't allow.
    let refused (file: ListedFile) =
        let host = file.Url.Host

        if file.Url.Scheme <> Uri.UriSchemeHttps then
            ValueSome $"The listing names {file.Name} at {file.Url}, which isn't https"
        elif
            not (String.Equals(host, listing.Host, StringComparison.OrdinalIgnoreCase))
            && not (
                hosts
                |> List.exists (fun allowed -> String.Equals(host, allowed, StringComparison.OrdinalIgnoreCase))
            )
        then
            ValueSome $"The listing names {file.Name} on {host}, a host other than {listing.Host}"
        else
            ValueNone

    /// The portal and its API, such as "data.gov.au, CKAN API".
    member _.Portal = portal

    /// The dataset or listing read there, such as "asic-companies".
    member _.Dataset = dataset

    /// The listing's address.
    member _.Listing = listing

    /// The size of all the files, seen on 2026-11-08. A file the listing states no size for may be this large; a listing whose sizes add up to
    /// more than four times this is refused.
    member _.ApproximateBytes = approximateBytes

    /// How a person gets the files when the listing can't be read.
    member _.ByHand = byHand

    /// The files the listing names now, each with the size the listing states, or this resolver's size when it states none, and the size the
    /// listing states, if it states one.
    member internal _.ResolveAsync(http: HttpClient, ct: CancellationToken) =
        task {
            match! read http ct with
            | Error failure -> return Error failure
            | Ok [] -> return Error(RegistryFailure.UnreadableAnswer "The listing names none of the files")
            | Ok picked when picked |> List.exists (fun pick -> (refused pick.File).IsSome) ->
                return
                    picked
                    |> List.pick (fun pick -> refused pick.File |> ValueOption.toOption)
                    |> RegistryFailure.RequestRefused
                    |> Error
            | Ok picked ->
                let stated =
                    picked
                    |> List.sumBy (fun pick -> pick.File.Bytes |> ValueOption.defaultValue 0L)

                if stated > approximateBytes * BulkSize.Margin then
                    return
                        Error(
                            RegistryFailure.UnreadableAnswer
                                $"The listing's files add up to {stated} bytes, more than {BulkSize.Margin} times the {approximateBytes} expected"
                        )
                else
                    return
                        Ok(
                            picked
                            |> List.map (fun pick ->
                                {
                                    Link =
                                        {
                                            Name = pick.SaveAs
                                            Url = pick.File.Url
                                            ApproximateBytes =
                                                pick.File.Bytes |> ValueOption.defaultValue approximateBytes
                                        }
                                    PortalName = pick.File.Name
                                    Modified = pick.File.Modified
                                },
                                pick.File.Bytes)
                        )
        }

/// Reading the listings: CKAN's action API, DCAT-AP, the Russian open data standard's meta.csv and WebDAV.
module internal BulkListing =

    let private timestamp (text: string) =
        match DateTimeOffset.TryParse(text, CultureInfo.InvariantCulture, DateTimeStyles.AssumeUniversal) with
        | true, at -> ValueSome at
        | _ -> ValueNone

    /// The last segment of the URL's path, unescaped.
    let fileNameOf (url: Uri) =
        Uri.UnescapeDataString(url.Segments |> Array.last).TrimEnd '/'

    let private failureOf (status: HttpStatusCode) (what: string) =
        match int status with
        | 429 -> RegistryFailure.RateLimited ValueNone
        | code when code >= 500 -> RegistryFailure.Unavailable $"HTTP {code} for {what}"
        | code -> RegistryFailure.RequestRefused $"HTTP {code} for {what}"

    /// The answer's text, or the failure its status means. A listing that doesn't answer in a minute is Unavailable.
    let textAsync (http: HttpClient) (request: unit -> HttpRequestMessage) (ct: CancellationToken) =
        task {
            use timeout = CancellationTokenSource.CreateLinkedTokenSource ct
            timeout.CancelAfter(TimeSpan.FromMinutes 1.)
            use message = request ()
            let what = string message.RequestUri

            try
                use! response = http.SendAsync(message, timeout.Token)

                if response.IsSuccessStatusCode then
                    let! text = response.Content.ReadAsStringAsync timeout.Token
                    return Ok text
                else
                    return Error(failureOf response.StatusCode what)
            with
            | :? OperationCanceledException when not ct.IsCancellationRequested ->
                return Error(RegistryFailure.Unavailable $"No answer in a minute from {what}")
            | :? HttpRequestException as error -> return Error(RegistryFailure.Unavailable error.Message)
        }

    let getAsync (http: HttpClient) (url: Uri) ct =
        textAsync http (fun () -> new HttpRequestMessage(HttpMethod.Get, url)) ct

    /// A WebDAV PROPFIND of the folder's own entries.
    let propfindAsync (http: HttpClient) (url: Uri) ct =
        textAsync
            http
            (fun () ->
                let request = new HttpRequestMessage(HttpMethod "PROPFIND", url)
                request.Headers.Add("Depth", "1")
                request)
            ct

    let private parsed (read: unit -> 'T list) =
        try
            Ok(read ())
        with
        | :? JsonException
        | :? Json.InvalidJson
        | :? Xml.XmlException
        | :? UriFormatException
        | :? InvalidOperationException as error ->
            Error(RegistryFailure.UnreadableAnswer $"The listing can't be read: {error.GetType().Name}")

    let private int64Of name (element: JsonElement) =
        Json.tryText name element
        |> ValueOption.bind (fun text ->
            match Int64.TryParse(text, NumberStyles.Integer, CultureInfo.InvariantCulture) with
            | true, n when n > 0L -> ValueSome n
            | _ -> ValueNone)

    /// A CKAN resource: its name, URL, size, format, and last change: its last_modified, else its package's metadata_modified, else its
    /// created.
    let private ckanResource (packageModified: string voption) (resource: JsonElement) =
        Json.tryString "url" resource
        |> ValueOption.filter (String.IsNullOrWhiteSpace >> not)
        |> ValueOption.map (fun url ->
            let link = Uri url

            {
                Name =
                    Json.tryString "name" resource
                    |> ValueOption.filter (String.IsNullOrWhiteSpace >> not)
                    |> ValueOption.defaultWith (fun () -> fileNameOf link)
                Url = link
                Bytes = int64Of "size" resource
                Modified =
                    Json.tryString "last_modified" resource
                    |> ValueOption.orElse packageModified
                    |> ValueOption.orElse (Json.tryString "created" resource)
                    |> ValueOption.bind timestamp
                Format = Json.tryString "format" resource |> ValueOption.defaultValue ""
            })

    let private ckanResult (text: string) =
        use document = JsonDocument.Parse text
        let root = document.RootElement

        if Json.tryFlag "success" root <> ValueSome true then
            raise (Json.InvalidJson "success")

        Json.required "result" Json.tryObject root |> _.Clone()

    let private ckanFiles (package: JsonElement) =
        Json.array package "resources"
        |> List.choose (
            ckanResource (Json.tryString "metadata_modified" package)
            >> ValueOption.toOption
        )

    /// The resources of a package_show answer.
    let ckanPackage (text: string) =
        parsed (fun () -> ckanResult text |> ckanFiles)

    /// A package_search answer's packages, newest first by metadata_created, each with its name and resources.
    let ckanSearch (text: string) =
        parsed (fun () ->
            Json.array (ckanResult text) "results"
            |> List.map (fun package ->
                let name = Json.tryString "name" package |> ValueOption.defaultValue ""

                let created =
                    Json.tryString "metadata_created" package
                    |> ValueOption.bind timestamp
                    |> ValueOption.defaultValue DateTimeOffset.MinValue

                name, created, ckanFiles package)
            |> List.sortByDescending (fun (_, created, _) -> created))

    let private dcat = XNamespace.Get "http://www.w3.org/ns/dcat#"
    let private dct = XNamespace.Get "http://purl.org/dc/terms/"
    let private rdf = XNamespace.Get "http://www.w3.org/1999/02/22-rdf-syntax-ns#"

    /// A DCAT-AP RDF/XML document's distributions with a download URL. Each is dated as its dataset's dct:modified, since the portal stamps a
    /// distribution with the time the document was written.
    let dcatDistributions (text: string) =
        parsed (fun () ->
            let document = XDocument.Parse text

            let modified =
                document.Descendants(dcat + "Dataset")
                |> Seq.tryHead
                |> Option.bind (fun dataset -> dataset.Element(dct + "modified") |> Option.ofObj)
                |> Option.map (fun element -> timestamp element.Value)
                |> Option.defaultValue ValueNone

            document.Descendants(dcat + "Distribution")
            |> Seq.choose (fun distribution ->
                distribution.Element(dcat + "downloadURL")
                |> Option.ofObj
                |> Option.bind (fun element -> element.Attribute(rdf + "resource") |> Option.ofObj)
                |> Option.map (fun url ->
                    let link = Uri url.Value

                    {
                        Name = fileNameOf link
                        Url = link
                        Bytes =
                            distribution.Element(dcat + "byteSize")
                            |> Option.ofObj
                            |> Option.bind (fun size ->
                                match
                                    Decimal.TryParse(size.Value, NumberStyles.Number, CultureInfo.InvariantCulture)
                                with
                                | true, n when n > 0m -> Some(int64 n)
                                | _ -> None)
                            |> ValueOption.ofOption
                        Modified = modified
                        Format =
                            distribution.Element(dct + "format")
                            |> Option.ofObj
                            |> Option.map _.Value.Trim()
                            |> Option.defaultValue ""
                    }))
            |> List.ofSeq)

    let private fnsDataRow =
        Regex(@"\A(data-([0-9]{8})-structure-[0-9]{8}\.zip),(https?://\S+)\s*\z", RegexOptions.CultureInvariant)

    /// The data rows of a meta.csv in the Russian open data standard (property,value), each data-DDMMYYYY-structure-DDMMYYYY.zip dated by its
    /// first date.
    let fnsMeta (text: string) =
        parsed (fun () ->
            text.Split([| '\r'; '\n' |], StringSplitOptions.RemoveEmptyEntries)
            |> Array.choose (fun line ->
                let row = fnsDataRow.Match line

                if row.Success then
                    match
                        DateTimeOffset.TryParseExact(
                            row.Groups[2].Value,
                            "ddMMyyyy",
                            CultureInfo.InvariantCulture,
                            DateTimeStyles.AssumeUniversal
                        )
                    with
                    | true, date ->
                        Some
                            {
                                Name = row.Groups[1].Value
                                Url = Uri row.Groups[3].Value
                                Bytes = ValueNone
                                Modified = ValueSome date
                                Format = "zip"
                            }
                    | _ -> None
                else
                    None)
            |> List.ofArray)

    let private dav = XNamespace.Get "DAV:"

    /// A PROPFIND answer's entries other than the folder itself, as (absolute URL, is a folder, file): the file's name is its last segment.
    let davEntries (folder: Uri) (text: string) =
        parsed (fun () ->
            XDocument.Parse(text).Descendants(dav + "response")
            |> Seq.choose (fun response ->
                let href = response.Element(dav + "href")

                if isNull href then
                    None
                else
                    let url = Uri(folder, href.Value)
                    let props = response.Descendants(dav + "prop") |> Seq.tryHead

                    let prop name =
                        props |> Option.bind (fun props -> props.Element(dav + name) |> Option.ofObj)

                    let isFolder =
                        prop "resourcetype"
                        |> Option.exists (fun kind -> not (isNull (kind.Element(dav + "collection"))))

                    if url.AbsolutePath.TrimEnd('/') = folder.AbsolutePath.TrimEnd('/') then
                        None
                    else
                        Some(
                            url,
                            isFolder,
                            {
                                Name = fileNameOf url
                                Url = url
                                Bytes =
                                    prop "getcontentlength"
                                    |> Option.bind (fun size ->
                                        match
                                            Int64.TryParse(
                                                size.Value,
                                                NumberStyles.Integer,
                                                CultureInfo.InvariantCulture
                                            )
                                        with
                                        | true, n when n > 0L -> Some n
                                        | _ -> None)
                                    |> ValueOption.ofOption
                                Modified =
                                    prop "getlastmodified"
                                    |> Option.map (fun at -> timestamp at.Value)
                                    |> Option.defaultValue ValueNone
                                Format = ""
                            }
                        ))
            |> List.ofSeq)

/// The resolvers of the bulk sources whose portal API names the current files.
module internal BulkResolvers =

    let private newestFirst (files: ListedFile list) =
        files
        |> List.sortByDescending (fun file -> file.Modified |> ValueOption.defaultValue DateTimeOffset.MinValue)

    let private same (a: string) (b: string) =
        String.Equals(a, b, StringComparison.OrdinalIgnoreCase)

    /// Named so, by its listed name or its URL's file name.
    let private called (name: string) (file: ListedFile) =
        same file.Name name || same (BulkListing.fileNameOf file.Url) name

    let private missing (names: string list) =
        Error(RegistryFailure.UnreadableAnswer("The listing names no " + String.Join(", ", names)))

    /// The newest file passing each test, saved under its name; every required one must be found.
    let private pick
        (required: (string * (ListedFile -> bool)) list)
        (optional: (string * (ListedFile -> bool)) list)
        (files: ListedFile list)
        =
        let newest (saveAs, test) =
            newestFirst files
            |> List.tryFind test
            |> Option.map (fun file -> { SaveAs = saveAs; File = file })

        let found = required |> List.map (fun wanted -> fst wanted, newest wanted)

        match found |> List.filter (snd >> Option.isNone) |> List.map fst with
        | [] -> Ok((found |> List.choose snd) @ (optional |> List.choose newest))
        | absent -> missing absent

    let private ckanAction (site: string) (action: string) (query: string) =
        Uri $"{site.TrimEnd '/'}/api/3/action/{action}?{query}"

    /// A CKAN dataset's resources by package_show, picked by choose.
    let private ckanDataset
        (portal: string)
        (site: string)
        (dataset: string)
        (bytes: int64)
        (byHand: string)
        (choose: ListedFile list -> Result<PickedFile list, RegistryFailure>)
        =
        let listing = ckanAction site "package_show" ("id=" + Uri.EscapeDataString dataset)

        // Every CKAN portal here serves its resources from its own host.
        BulkResolver(
            portal + ", CKAN API",
            dataset,
            listing,
            bytes,
            byHand,
            [],
            fun http ct ->
                task {
                    match! BulkListing.getAsync http listing ct with
                    | Error failure -> return Error failure
                    | Ok text -> return BulkListing.ckanPackage text |> Result.bind choose
                }
        )

    /// ASIC's company dataset: the newest ZIP of "Company Dataset - Current".
    let asic =
        ckanDataset
            "data.gov.au"
            "https://data.gov.au/data"
            "asic-companies"
            79_298_881L
            "Download the weekly zip from the dataset page; its link changes each week"
            (pick
                [
                    "company.zip",
                    fun file ->
                        same file.Format "ZIP"
                        && file.Name.Contains("Current", StringComparison.OrdinalIgnoreCase)
                ]
                [])

    /// Ukraine's ЄДР: UO.zip.
    let ukraine =
        ckanDataset
            "data.gov.ua"
            "https://data.gov.ua"
            "03cc1239-3988-4451-aa0d-aadb77448714"
            328_232_580L
            "Download uo.zip from the dataset page (about 330 MB)"
            (pick [ "uo.zip", called "uo.zip" ] [])

    /// Slovenia's AJPES register: opsiprs.csv.
    let slovenia =
        ckanDataset
            "podatki.gov.si"
            "https://podatki.gov.si"
            "poslovni-register-slovenije"
            126_476_286L
            "Download opsiprs.csv from the dataset page"
            (pick [ "opsiprs.csv", called "opsiprs.csv" ] [])

    let private halfYear =
        Regex(
            @"\AIGJ\s*-\s*([0-9]{4})\s+semestre\s+([12])\b",
            RegexOptions.IgnoreCase ||| RegexOptions.CultureInvariant
        )

    /// The IGJ's newest half-yearly zip, by the year and half its name states.
    let argentinaCaba =
        ckanDataset
            "datos.jus.gob.ar"
            "https://datos.jus.gob.ar"
            "entidades-constituidas-en-la-inspeccion-general-de-justicia-igj"
            162_260_774L
            "Download the newest half-yearly zip from the dataset page"
            (fun files ->
                files
                |> List.choose (fun file ->
                    let half = halfYear.Match file.Name

                    if half.Success && same file.Format "ZIP" then
                        Some((int half.Groups[1].Value, int half.Groups[2].Value), file)
                    else
                        None)
                |> List.sortByDescending fst
                |> List.tryHead
                |> function
                    | Some(_, file) -> Ok [ { SaveAs = "igj.zip"; File = file } ]
                    | None -> missing [ "half-yearly zip" ])

    let private formationYear =
        Regex(@"Constituciones del a[ñn]o ([0-9]{4})", RegexOptions.IgnoreCase ||| RegexOptions.CultureInvariant)

    /// The Registro de Empresas y Sociedades: every yearly CSV, the newest of each year, since a formation of any year can be asked for.
    let chile =
        ckanDataset
            "datos.gob.cl"
            "https://datos.gob.cl"
            "registro-de-empresas-y-sociedades"
            250_000_000L
            "Download the yearly CSVs of the Registro de Empresas y Sociedades"
            (fun files ->
                match
                    newestFirst files
                    |> List.choose (fun file ->
                        let year = formationYear.Match file.Name

                        if year.Success && same file.Format "csv" then
                            Some(year.Groups[1].Value, file)
                        else
                            None)
                    |> List.distinctBy fst
                    |> List.sortBy fst
                with
                | [] -> missing [ "yearly CSV" ]
                | years ->
                    Ok(
                        years
                        |> List.map (fun (year, file) ->
                            {
                                SaveAs = $"res-{year}.csv"
                                File = file
                            })
                    ))

    /// ONRC: OD_FIRME.CSV, OD_STARE_FIRMA.CSV and OD_REPREZENTANTI_LEGALI.CSV from the newest "firme-" dataset, and N_STARE_FIRMA.CSV from the
    /// newest "nomenclatoare-" one, by package_search over the onrc organisation.
    let romania =
        let site = "https://data.gov.ro"

        let listing =
            ckanAction
                site
                "package_search"
                ("fq="
                 + Uri.EscapeDataString "organization:onrc"
                 + "&sort="
                 + Uri.EscapeDataString "metadata_created desc"
                 + "&rows=20")

        let newestNamed (prefix: string) (packages: (string * DateTimeOffset * ListedFile list) list) =
            packages
            |> List.tryFind (fun (name, _, _) -> name.StartsWith(prefix, StringComparison.OrdinalIgnoreCase))
            |> Option.map (fun (_, _, files) -> files)
            |> Option.defaultValue []

        BulkResolver(
            "data.gov.ro, CKAN API",
            "organization onrc, the newest firme- and nomenclatoare- datasets",
            listing,
            1_150_000_000L,
            "Open the newest \"firme\" dataset and download OD_FIRME.CSV, OD_STARE_FIRMA.CSV and OD_REPREZENTANTI_LEGALI.CSV, and N_STARE_FIRMA.CSV from the newest \"nomenclatoare\" dataset",
            [],
            fun http ct ->
                task {
                    match! BulkListing.getAsync http listing ct with
                    | Error failure -> return Error failure
                    | Ok text ->
                        return
                            BulkListing.ckanSearch text
                            |> Result.bind (fun packages ->
                                let firms = newestNamed "firme-" packages
                                let codes = newestNamed "nomenclatoare-" packages

                                pick
                                    [
                                        "OD_FIRME.CSV", called "OD_FIRME.CSV"
                                        "OD_STARE_FIRMA.CSV", called "OD_STARE_FIRMA.CSV"
                                    ]
                                    [ "OD_REPREZENTANTI_LEGALI.CSV", called "OD_REPREZENTANTI_LEGALI.CSV" ]
                                    firms
                                |> Result.map (fun picked ->
                                    picked
                                    @ (pick [] [ "N_STARE_FIRMA.CSV", called "N_STARE_FIRMA.CSV" ] codes
                                       |> Result.defaultValue [])))
                }
        )

    let private numbered (prefix: string) =
        Regex($@"\A{prefix}_[0-9]+\.csv\z", RegexOptions.IgnoreCase ||| RegexOptions.CultureInvariant)

    /// The Cyprus Registrar's three CSVs, from the dataset's DCAT-AP 2.0 RDF/XML, whose file names carry a number that changes each month.
    let cyprus =
        let listing = Uri "https://www.data.gov.cy/en/dataset/1026/dcat-ap-2.0/xml"
        let organisations = numbered "organisations"
        let offices = numbered "registered_office"
        let officials = numbered "organisation_officials"

        let matching (pattern: Regex) (file: ListedFile) = pattern.IsMatch file.Name

        BulkResolver(
            "data.gov.cy, DCAT-AP 2.0",
            "dataset 1026",
            listing,
            242_149_795L,
            "Download the organisations, registered office and officials CSVs from the dataset page; their names change each month",
            [],
            fun http ct ->
                task {
                    match! BulkListing.getAsync http listing ct with
                    | Error failure -> return Error failure
                    | Ok text ->
                        return
                            BulkListing.dcatDistributions text
                            |> Result.bind (
                                pick
                                    [ "organisations.csv", matching organisations ]
                                    [
                                        "registered_office.csv", matching offices
                                        "organisation_officials.csv", matching officials
                                    ]
                            )
                }
        )

    /// The FNS register of small and medium businesses: the newest data zip its meta.csv lists, from www.nalog.gov.ru or file.nalog.ru. meta.csv
    /// can trail the dataset page by a month.
    let russiaSme =
        let listing = Uri "https://www.nalog.gov.ru/opendata/7707329152-rsmp/meta.csv"

        BulkResolver(
            "nalog.gov.ru, open data meta.csv",
            "7707329152-rsmp",
            listing,
            2_300_000_000L,
            "Download the newest data zip (about 2.3 GB) from the dataset page; its name holds the date",
            // meta.csv lists zips on the FNS file server too.
            [ "file.nalog.ru" ],
            fun http ct ->
                task {
                    match! BulkListing.getAsync http listing ct with
                    | Error failure -> return Error failure
                    | Ok text -> return BulkListing.fnsMeta text |> Result.bind (pick [ "rsmp.zip", fun _ -> true ] [])
                }
        )

    let private month = Regex(@"\A[0-9]{4}-[0-9]{2}\z", RegexOptions.CultureInvariant)

    let private brazilParts =
        [
            for prefix in [ "Empresas"; "Estabelecimentos"; "Socios" ] do
                for part in 0..9 -> $"{prefix}{part}.zip"
        ]

    /// Receita Federal's CNPJ files: Empresas, Estabelecimentos and Socios 0 to 9 of the newest month folder holding all thirty, of the two
    /// newest, by WebDAV PROPFIND on the public share the portal's address redirects to.
    let brazil =
        let folder =
            Uri "https://arquivos.receitafederal.gov.br/public.php/dav/files/gn672Ad4CF8N6TK/Dados/Cadastros/CNPJ/"

        let monthFiles (http: HttpClient) (url: Uri) ct =
            task {
                match! BulkListing.propfindAsync http url ct with
                | Error failure -> return Error failure
                | Ok text ->
                    return
                        BulkListing.davEntries url text
                        |> Result.map (
                            List.filter (fun (_, isFolder, _) -> not isFolder)
                            >> List.map (fun (_, _, file) -> file)
                        )
                        |> Result.bind (pick [ for name in brazilParts -> name, called name ] [])
            }

        BulkResolver(
            "arquivos.receitafederal.gov.br, WebDAV",
            "Dados/Cadastros/CNPJ, the newest month",
            folder,
            7_500_000_000L,
            "Download one month's Empresas, Estabelecimentos and Socios zips (about 7.5 GB in all)",
            [],
            fun http ct ->
                task {
                    match! BulkListing.propfindAsync http folder ct with
                    | Error failure -> return Error failure
                    | Ok text ->
                        match BulkListing.davEntries folder text with
                        | Error failure -> return Error failure
                        | Ok entries ->
                            let months =
                                entries
                                |> List.choose (fun (url, isFolder, file) ->
                                    if isFolder && month.IsMatch file.Name then
                                        Some(file.Name, Uri(url.AbsoluteUri.TrimEnd('/') + "/"))
                                    else
                                        None)
                                |> List.sortByDescending fst
                                |> List.map snd

                            match months with
                            | [] -> return missing [ "month folder" ]
                            | newest :: older ->
                                match! monthFiles http newest ct with
                                | Ok files -> return Ok files
                                | Error failure ->
                                    match older with
                                    | before :: _ -> return! monthFiles http before ct
                                    | [] -> return Error failure
                }
        )
