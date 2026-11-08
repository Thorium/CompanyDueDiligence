namespace CompanyDueDiligence

/// The sources this package reads, over one connection.
module Sources =

    let malta connection : ICompanyRegistry = MaltaRegistry connection
    let france connection : ICompanyRegistry = FranceRegistry connection
    let finland connection : ICompanyRegistry = FinlandRegistry connection
    let texas connection : ICompanyRegistry = TexasRegistry connection
    let colorado connection : ICompanyRegistry = ColoradoRegistry connection
    let connecticut connection : ICompanyRegistry = ConnecticutRegistry connection
    let newYork connection : ICompanyRegistry = NewYorkRegistry connection
    let oregon connection : ICompanyRegistry = OregonRegistry connection
    let pennsylvania connection : ICompanyRegistry = PennsylvaniaRegistry connection
    let districtOfColumbia connection : ICompanyRegistry = DistrictOfColumbiaRegistry connection
    let norway connection : ICompanyRegistry = NorwayRegistry connection
    let czechia connection : ICompanyRegistry = CzechiaRegistry connection
    let slovakia connection : ICompanyRegistry = SlovakiaRegistry connection
    let ireland connection : ICompanyRegistry = IrelandRegistry connection
    let poland connection : ICompanyRegistry = PolandRegistry connection

    /// The Swiss UID register (BFS). Zefix, behind credentials, gives the commercial register's own status. Both cover CH: an investigation reads
    /// each covering register in CaseSources.Registries order, takes the first that holds the company as its record, and keeps the others' answers as evidence.
    let switzerland connection : ICompanyRegistry = SwitzerlandRegistry connection

    let britishColumbia connection : ICompanyRegistry = BritishColumbiaRegistry connection
    let israel connection : ICompanyRegistry = IsraelRegistry connection
    let singapore connection : ICompanyRegistry = SingaporeRegistry connection
    let netherlands connection : ICompanyRegistry = NetherlandsRegistry connection
    let latvia connection : ICompanyRegistry = LatviaRegistry connection
    let taiwan connection : ICompanyRegistry = TaiwanRegistry connection
    let colombia connection : ICompanyRegistry = ColombiaRegistry connection
    let costaRica connection : ICompanyRegistry = CostaRicaRegistry connection

    /// KRA's PIN checker, with the consumer key and secret of a GavaConnect app.
    let kenya (connection: SourceConnection) (credentials: ClientCredentials) : ICompanyRegistry =
        KenyaRegistry(connection, credentials)

    /// Corporations Canada, with an ISED API Store key. Federal corporations only: a province code is outside its coverage.
    let canada (connection: SourceConnection) (key: SourceKey) : ICompanyRegistry = CanadaRegistry(connection, key)

    /// The NZBN API, with a subscription key from the business.govt.nz API portal.
    let newZealand (connection: SourceConnection) (key: SourceKey) : ICompanyRegistry =
        NewZealandRegistry(connection, key)

    /// Bolagsverket's high-value datasets API, with the OAuth2 client credentials it issues.
    let sweden (connection: SourceConnection) (credentials: ClientCredentials) : ICompanyRegistry =
        SwedenRegistry(connection, credentials)

    /// The NTA corporate number Web-API, with its application ID.
    let japan (connection: SourceConnection) (applicationId: SourceKey) : ICompanyRegistry =
        JapanRegistry(connection, applicationId)

    /// Zefix PublicREST, with the user name and password the Federal Office of the Commercial Register issues, sent as HTTP Basic.
    let zefix (connection: SourceConnection) (credentials: ClientCredentials) : ICompanyRegistry =
        ZefixRegistry(connection, credentials)

    /// California's business entity API, with a subscription key. Its portal says it serves government agencies only.
    let california (connection: SourceConnection) (key: SourceKey) : ICompanyRegistry =
        CaliforniaRegistry(connection, key)

    /// ABN Lookup, with the GUID the ABR issues.
    let australia (connection: SourceConnection) (guid: SourceKey) : ICompanyRegistry =
        AustraliaRegistry(connection, guid)

    /// The NTS business status service, with a data.go.kr service key.
    let korea (connection: SourceConnection) (key: SourceKey) : ICompanyRegistry = KoreaRegistry(connection, key)

    /// The Sudski registar open data API, with its OAuth2 client credentials.
    let croatia (connection: SourceConnection) (credentials: ClientCredentials) : ICompanyRegistry =
        CroatiaRegistry(connection, credentials)

    /// GEMI open data, with its api_key.
    let greece (connection: SourceConnection) (key: SourceKey) : ICompanyRegistry = GreeceRegistry(connection, key)

    /// The Firmenbuch HVD web service, with the X-API-KEY JustizOnline issues.
    let austria (connection: SourceConnection) (key: SourceKey) : ICompanyRegistry = AustriaRegistry(connection, key)

    /// CVR system-to-system, with the user ID and password Erhvervsstyrelsen issues. Its only endpoint is plain http, so the credentials travel
    /// unencrypted.
    let denmark (connection: SourceConnection) (credentials: ClientCredentials) : ICompanyRegistry =
        DenmarkRegistry(connection, credentials)

    let gleif (connection: SourceConnection) = GleifCrossCheck connection

/// What the settings held for the sources behind a key.
type KeyedSourceSettings =
    {
        /// One registry for each source whose settings are all given.
        Registries: ICompanyRegistry list
        /// Sources with none of their settings given.
        NotConfigured: string list
        /// Settings given without their pair, such as a client ID with no secret. No registry is made for these.
        Incomplete: string list
    }

/// The sources behind a key, built from named settings such as environment variables. docs/keys.md says where each key comes from.
module KeyedSources =

    open System

    /// A source's name and the settings it needs, with how to build it from them.
    type private Keyed =
        {
            Name: string
            Settings: string list
            Build: SourceConnection -> string list -> ICompanyRegistry
        }

    let private one name setting build =
        {
            Name = name
            Settings = [ setting ]
            Build = fun connection values -> build connection (SourceKey(List.head values))
        }

    let private pair name idSetting secretSetting build =
        {
            Name = name
            Settings = [ idSetting; secretSetting ]
            Build =
                fun connection values ->
                    match values with
                    | [ pid; secret ] -> build connection (ClientCredentials(pid, secret))
                    | _ -> invalidArg (nameof values) "A pair of settings is two values."
        }

    let private companiesHouse =
        {
            Name = "Companies House"
            Settings = [ "CDD_COMPANIES_HOUSE_KEY" ]
            Build =
                fun _ values ->
                    CompaniesHouse.CompaniesHouseRegistry(
                        CompaniesHouse.CompaniesHouseClient.Create(
                            { ApiKey = List.head values },
                            CompaniesHouse.Endpoints.live,
                            TimeSpan.FromSeconds 20.
                        )
                    )
        }

    let private all =
        [
            companiesHouse
            one "Corporations Canada" "CDD_CANADA_KEY" Sources.canada
            one "NZBN" "CDD_NEW_ZEALAND_KEY" Sources.newZealand
            pair "Bolagsverket" "CDD_SWEDEN_CLIENT_ID" "CDD_SWEDEN_CLIENT_SECRET" Sources.sweden
            one "NTA corporate number" "CDD_JAPAN_APPLICATION_ID" Sources.japan
            pair "Zefix" "CDD_ZEFIX_USER" "CDD_ZEFIX_PASSWORD" Sources.zefix
            one "California SOS" "CDD_CALIFORNIA_KEY" Sources.california
            one "ABN Lookup" "CDD_AUSTRALIA_GUID" Sources.australia
            one "NTS business status" "CDD_KOREA_SERVICE_KEY" Sources.korea
            pair "Sudski registar" "CDD_CROATIA_CLIENT_ID" "CDD_CROATIA_CLIENT_SECRET" Sources.croatia
            one "GEMI" "CDD_GREECE_KEY" Sources.greece
            one "Firmenbuch HVD" "CDD_AUSTRIA_KEY" Sources.austria
            pair "CVR" "CDD_DENMARK_USER" "CDD_DENMARK_PASSWORD" Sources.denmark
            pair "KRA PIN checker" "CDD_KENYA_CONSUMER_KEY" "CDD_KENYA_CONSUMER_SECRET" Sources.kenya
        ]

    /// Every setting name read, in order.
    let settingNames = all |> List.collect _.Settings

    /// The keyed sources whose settings setting gives. A blank value counts as not given.
    let fromSettings (connection: SourceConnection) (setting: string -> string voption) : KeyedSourceSettings =
        let read name =
            setting name
            |> ValueOption.filter (String.IsNullOrWhiteSpace >> not)
            |> ValueOption.map _.Trim()

        let sorted =
            all
            |> List.map (fun keyed ->
                let values = keyed.Settings |> List.map read

                match values |> List.choose ValueOption.toOption with
                | given when given.Length = keyed.Settings.Length -> Choice1Of3(keyed.Build connection given)
                | [] -> Choice2Of3 keyed.Name
                | _ ->
                    let missing =
                        List.zip keyed.Settings values
                        |> List.filter (snd >> ValueOption.isNone)
                        |> List.map fst
                        |> String.concat ", "

                    Choice3Of3 $"{keyed.Name}: {missing} not given")

        {
            Registries =
                sorted
                |> List.choose (function
                    | Choice1Of3 registry -> Some registry
                    | Choice2Of3 _
                    | Choice3Of3 _ -> None)
            NotConfigured =
                sorted
                |> List.choose (function
                    | Choice2Of3 name -> Some name
                    | Choice1Of3 _
                    | Choice3Of3 _ -> None)
            Incomplete =
                sorted
                |> List.choose (function
                    | Choice3Of3 problem -> Some problem
                    | Choice1Of3 _
                    | Choice2Of3 _ -> None)
        }

    /// The keyed sources whose environment variables are set.
    let fromEnvironment (connection: SourceConnection) =
        fromSettings connection (Environment.GetEnvironmentVariable >> ValueOption.ofObj)

/// Registers read from files the host downloads and passes in. Nothing is called; answers are as of the file's date.
module BulkSources =

    open System.IO
    open System.Threading
    open System.Threading.Tasks

    /// Chile's yearly formation files from datos.gob.cl, read as UTF-8 and left open.
    let chileAsync
        (files: Stream seq)
        (date: BulkFileDate)
        (ct: CancellationToken)
        : Task<Result<ICompanyRegistry, RegistryFailure>> =
        Bulk.run ct (fun () -> Chile.build files date ct)

    /// Ukraine's ЄДР legal entities: uo.zip from data.gov.ua, or the UO.xml inside it.
    let ukraineAsync
        (uo: Stream)
        (date: BulkFileDate)
        (ct: CancellationToken)
        : Task<Result<ICompanyRegistry, RegistryFailure>> =
        Bulk.run ct (fun () -> Ukraine.build uo date ct)

    /// Florida's corporate data: the quarterly cordata.zip as one stream, then any daily files, zipped or not. Each stream is one file, and a
    /// later file's record replaces an earlier one's.
    let floridaAsync
        (files: Stream list)
        (date: BulkFileDate)
        (ct: CancellationToken)
        : Task<Result<ICompanyRegistry, RegistryFailure>> =
        Bulk.run ct (fun () -> Florida.build files date ct)

    /// Paraguay's RUC list: any of ruc0.zip to ruc9.zip. With all ten and every row read, a RUC not in them is not held.
    let paraguayAsync
        (files: Stream list)
        (date: BulkFileDate)
        (ct: CancellationToken)
        : Task<Result<ICompanyRegistry, RegistryFailure>> =
        Bulk.run ct (fun () -> Paraguay.build files date ct)

    /// Brazil's CNPJ open data: the Empresas, Estabelecimentos and Socios zips the host picks, of one month. With fewer than the ten Socios
    /// parts, every partner listing has a gap.
    let brazilAsync
        (empresas: Stream list)
        (estabelecimentos: Stream list)
        (socios: Stream list)
        (date: BulkFileDate)
        (ct: CancellationToken)
        : Task<Result<ICompanyRegistry, RegistryFailure>> =
        Bulk.run ct (fun () -> Brazil.build empresas estabelecimentos socios date ct)

    /// ASIC's company dataset from data.gov.au: the weekly zip or its CSV.
    let asicCompaniesAsync
        (file: Stream)
        (date: BulkFileDate)
        (ct: CancellationToken)
        : Task<Result<ICompanyRegistry, RegistryFailure>> =
        Bulk.run ct (fun () -> AsicCompanies.build file date ct)

    /// Alaska's corporations download, with the officials download if the host has it.
    let alaskaAsync
        (corporations: Stream)
        (officials: Stream voption)
        (date: BulkFileDate)
        (ct: CancellationToken)
        : Task<Result<ICompanyRegistry, RegistryFailure>> =
        Bulk.run ct (fun () -> Alaska.build corporations officials date ct)

    /// Estonia's e-Business Register open data: the general data zip (ettevotja_rekvisiidid__yldandmed.json.zip) and, if passed, the zips of
    /// persons on the registry card (__kaardile_kantud_isikud), shareholders (__osanikud) and beneficial owners (__kasusaajad), as published.
    let estoniaAsync
        (general: Stream)
        (persons: Stream voption)
        (shareholders: Stream voption)
        (beneficialOwners: Stream voption)
        (date: BulkFileDate)
        (ct: CancellationToken)
        : Task<Result<ICompanyRegistry, RegistryFailure>> =
        Bulk.runAsync (fun () -> Estonia.buildAsync general persons shareholders beneficialOwners date ct)

    /// The Cyprus Registrar of Companies' CSVs from data.gov.cy dataset 1026: organisations, and if passed, registered offices and officials.
    let cyprusAsync
        (organisations: Stream)
        (registeredOffices: Stream voption)
        (officials: Stream voption)
        (date: BulkFileDate)
        (ct: CancellationToken)
        : Task<Result<ICompanyRegistry, RegistryFailure>> =
        Bulk.run ct (fun () -> Cyprus.build organisations registeredOffices officials date ct)

    /// ONRC's CSVs from data.gov.ro: OD_FIRME.CSV and OD_STARE_FIRMA.CSV, and if passed, N_STARE_FIRMA.CSV for the state words and
    /// OD_REPREZENTANTI_LEGALI.CSV.
    let romaniaAsync
        (firms: Stream)
        (states: Stream)
        (stateNames: Stream voption)
        (representatives: Stream voption)
        (date: BulkFileDate)
        (ct: CancellationToken)
        : Task<Result<ICompanyRegistry, RegistryFailure>> =
        Bulk.run ct (fun () -> Romania.build firms states stateNames representatives date ct)

    /// AJPES's opsiprs.csv from podatki.gov.si.
    let sloveniaAsync
        (file: Stream)
        (date: BulkFileDate)
        (ct: CancellationToken)
        : Task<Result<ICompanyRegistry, RegistryFailure>> =
        Bulk.run ct (fun () -> Slovenia.build file date ct)

    /// The CBE/KBO open data full zip (KboOpenData_..._Full.zip), as published.
    let belgiumAsync
        (zip: Stream)
        (date: BulkFileDate)
        (ct: CancellationToken)
        : Task<Result<ICompanyRegistry, RegistryFailure>> =
        Bulk.run ct (fun () -> Belgium.build zip date ct)

    /// Moldova (MD): ASP's company.xlsx, the State Register of Legal Units' enterprises, by IDNO.
    let moldovaAsync
        (file: Stream)
        (date: BulkFileDate)
        (ct: CancellationToken)
        : Task<Result<ICompanyRegistry, RegistryFailure>> =
        Bulk.run ct (fun () -> Moldova.build file date ct)

    /// Serbia (RS): APR's open data companies answer, the whole JSON as downloaded, by matični broj.
    let serbiaAsync
        (file: Stream)
        (date: BulkFileDate)
        (ct: CancellationToken)
        : Task<Result<ICompanyRegistry, RegistryFailure>> =
        Bulk.run ct (fun () -> Serbia.build file date ct)

    /// Brčko District of Bosnia and Herzegovina (BA-BRC): the e-Registar's OpenData.xml export, by registration number (MBS) or tax number (JIB).
    let bosniaBrckoAsync
        (file: Stream)
        (date: BulkFileDate)
        (ct: CancellationToken)
        : Task<Result<ICompanyRegistry, RegistryFailure>> =
        Bulk.run ct (fun () -> Brcko.build file date ct)

    /// Ciudad de Buenos Aires (AR-C): the IGJ's half-yearly "entidades constituidas" zip, by IGJ número correlativo or CUIT.
    let argentinaCabaAsync
        (zip: Stream)
        (date: BulkFileDate)
        (ct: CancellationToken)
        : Task<Result<ICompanyRegistry, RegistryFailure>> =
        Bulk.run ct (fun () -> ArgentinaIgj.build zip date ct)

    /// Chile (CL): the SII's PUB_NOMBRES_PJ.zip and PUB_NOM_DIRECCIONES.zip, by RUT with its check digit.
    let chileSiiAsync
        (names: Stream)
        (addresses: Stream)
        (date: BulkFileDate)
        (ct: CancellationToken)
        : Task<Result<ICompanyRegistry, RegistryFailure>> =
        Bulk.run ct (fun () -> ChileSii.build names addresses date ct)

    /// Peru (PE): SUNAT's padron_reducido_ruc.zip, by a company's RUC (starting 20).
    let peruAsync
        (zip: Stream)
        (date: BulkFileDate)
        (ct: CancellationToken)
        : Task<Result<ICompanyRegistry, RegistryFailure>> =
        Bulk.run ct (fun () -> Peru.build zip date ct)

    /// Ecuador (EC): Supercias' directorio_companias.xlsx, by RUC or expediente.
    let ecuadorAsync
        (file: Stream)
        (date: BulkFileDate)
        (ct: CancellationToken)
        : Task<Result<ICompanyRegistry, RegistryFailure>> =
        Bulk.run ct (fun () -> Ecuador.build file date ct)

    /// Russia (RU): the FNS register of small and medium businesses, its monthly zip, by OGRN or INN. Small and medium businesses only.
    let russiaSmeAsync
        (zip: Stream)
        (date: BulkFileDate)
        (ct: CancellationToken)
        : Task<Result<ICompanyRegistry, RegistryFailure>> =
        Bulk.run ct (fun () -> RussiaSme.build zip date ct)

/// Each bulk source's own number normalisation by BulkCatalogue id. It is code, not data, so an index holds the id and opening it finds the
/// normaliser here.
module internal BulkSourceShape =

    let private shapes: Map<string, string -> string voption> =
        Map.ofList
            [
                "EE", Estonia.normalCode
                "CY", Cyprus.normalNumber
                "RO", Romania.normalWithoutFile
                "SI", Slovenia.normalNumber
                "BE", Belgium.normalNumber
                "UA", Ukraine.normalCode
                "MD", Moldova.normalNumber
                "RS", Serbia.normalNumber
                "BA-BRC", Brcko.normalNumber
                "RU-SME", RussiaSme.normalNumber
                "US-FL", Florida.normalNumber
                "US-AK", Alaska.normalNumber
                "PY", Paraguay.normalRuc
                "BR", Brazil.normalCnpj
                "AU-ASIC", AsicCompanies.normalAcn
                "CL-RES", Chile.normalRut
                "CL-SII", ChileSii.normalNumber
                "AR-C", ArgentinaIgj.normalNumber
                "PE", Peru.normalNumber
                "EC", Ecuador.normalNumber
            ]

    /// Every id with a normaliser.
    let ids = shapes |> Map.keys |> List.ofSeq

    /// The normaliser of the source with this id.
    let tryNormal (id: string) =
        shapes.TryFind id |> ValueOption.ofOption

/// Indexes on disk of bulk registers, built once per download. Opening one takes milliseconds and holds three file handles, not records, and it
/// answers exactly as the registry it was written from. Lookups read the files at an offset and are safe in parallel; a search reads the whole
/// names file, which for the largest sources is a few hundred megabytes per search. Not for WebAssembly, where bulk sources aren't built.
module BulkIndexes =

    open System
    open System.IO
    open System.Threading
    open System.Threading.Tasks

    let private written (save: unit -> Result<unit, RegistryFailure>) =
        try
            save ()
        with
        | :? IOException
        | :? UnauthorizedAccessException as error ->
            Error(RegistryFailure.Unavailable $"The index couldn't be written: {error.Message}")

    /// saveAsync, recording the files the registry was built from.
    let internal saveWith
        (inputs: BulkIndexInput list)
        (id: string)
        (registry: ICompanyRegistry)
        (directory: string)
        (ct: CancellationToken)
        : Task<Result<unit, RegistryFailure>> =
        match registry, BulkSourceShape.tryNormal id with
        | _, ValueNone -> Task.FromResult(Error(RegistryFailure.InvalidRequest $"No bulk source has the id {id}"))
        | (:? IBulkContents as built), ValueSome shape ->
            Task.Run(
                (fun () ->
                    // The register is built, so data the index can't hold is a failure to write it.
                    try
                        written (fun () -> BulkStore.write directory id shape inputs built.Contents ct)
                    with :? InvalidDataException as error ->
                        Error(RegistryFailure.Unavailable $"The index couldn't be written: {error.Message}")),
                ct
            )
        | _ ->
            Task.FromResult(
                Error(RegistryFailure.InvalidRequest "Only a registry BulkSources built can be saved as an index")
            )

    /// Writes a registry BulkSources built to an index in directory, replacing any there. id is the source's BulkCatalogue id. InvalidRequest
    /// for a registry not built by BulkSources, or by a source other than id; Unavailable when the disk refuses or the index can't hold a key.
    /// Cancelling ct throws.
    let saveAsync
        (id: string)
        (registry: ICompanyRegistry)
        (directory: string)
        (ct: CancellationToken)
        : Task<Result<unit, RegistryFailure>> =
        saveWith [] id registry directory ct

    /// saveUkraineAsync, recording the files the index was written from.
    let internal saveUkraineWith
        (inputs: BulkIndexInput list)
        (uo: Stream)
        (date: BulkFileDate)
        (directory: string)
        (ct: CancellationToken)
        : Task<Result<unit, RegistryFailure>> =
        Task.Run(
            (fun () ->
                let template =
                    (Ukraine.over (readOnlyDict []) (Collections.Generic.HashSet<string>()) date :?> IBulkContents)
                        .Contents

                try
                    written (fun () ->
                        BulkStore.writeStream
                            directory
                            "UA"
                            Ukraine.normalCode
                            inputs
                            template
                            Ukraine.keyOf
                            (Ukraine.records uo ct)
                            ct)
                with error ->
                    match Bulk.unreadable error with
                    | ValueSome reason -> Error(RegistryFailure.UnreadableAnswer reason)
                    | ValueNone -> reraise ()),
            ct
        )

    /// Writes Ukraine's index straight from uo.zip, or the UO.xml inside it, as BulkSources.ukraineAsync reads it, writing each record as it
    /// streams: the build holds each key's offset and name, never every record. The index answers as the registry ukraineAsync builds.
    let saveUkraineAsync
        (uo: Stream)
        (date: BulkFileDate)
        (directory: string)
        (ct: CancellationToken)
        : Task<Result<unit, RegistryFailure>> =
        saveUkraineWith [] uo date directory ct

    /// Opens an index saveAsync wrote. It answers exactly as the registry it was written from, and holds the index's files open until it is
    /// disposed: the registry is IDisposable. RequestRefused for a folder with no index, or one of another format or of a source this version
    /// doesn't know; UnreadableAnswer for one that is damaged or cut short.
    let openAsync (directory: string) (ct: CancellationToken) : Task<Result<ICompanyRegistry, RegistryFailure>> =
        ct.ThrowIfCancellationRequested()

        BulkStore.readManifest directory
        |> Result.bind (fun manifest ->
            match BulkSourceShape.tryNormal manifest.Source with
            | ValueSome normal -> BulkStore.openWith directory normal manifest
            | ValueNone ->
                Error(
                    RegistryFailure.RequestRefused
                        $"The index is of a bulk source this version doesn't know: {manifest.Source}"
                ))
        |> Task.FromResult
