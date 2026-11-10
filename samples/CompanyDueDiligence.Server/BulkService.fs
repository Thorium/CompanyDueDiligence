namespace CompanyDueDiligence.Server

open System
open System.Collections.Generic
open System.IO
open System.Threading
open System.Threading.Tasks
open Microsoft.Extensions.Logging
open CompanyDueDiligence
open CompanyDueDiligence.Web.Dto

/// A file a by-hand or resolved bulk source is built from on this machine.
type BulkSlot =
    {
        Name: string
        Label: string
        Required: bool
        Many: bool
    }

/// How each by-hand or resolved bulk source is built from files already on this machine, by BulkCatalogue id: its slots, and the BulkSources
/// function they feed.
module ByHand =

    let private slot name label required many =
        {
            Name = name
            Label = label
            Required = required
            Many = many
        }

    let private one name label = slot name label true false
    let private optional name label = slot name label false false

    /// The slots of each id the server can build. An id not here shows "not yet".
    let slots: Map<string, BulkSlot list> =
        Map.ofList
            [
                "CY",
                [
                    one "organisations" "Organisations CSV"
                    optional "offices" "Registered office CSV"
                    optional "officials" "Officials CSV"
                ]
                "RO",
                [
                    one "firms" "OD_FIRME.CSV"
                    one "states" "OD_STARE_FIRMA.CSV"
                    optional "stateNames" "N_STARE_FIRMA.CSV"
                    optional "representatives" "OD_REPREZENTANTI_LEGALI.CSV"
                ]
                "SI", [ one "file" "opsiprs.csv" ]
                "BE", [ one "zip" "KboOpenData full zip" ]
                "UA", [ one "uo" "uo.zip or UO.xml" ]
                "MD", [ one "file" "company.xlsx" ]
                "RU-SME", [ one "zip" "The data zip" ]
                "US-FL", [ one "cordata" "cordata.zip"; slot "daily" "Daily files" false true ]
                "BR",
                [
                    slot "empresas" "Empresas zips" true true
                    slot "estabelecimentos" "Estabelecimentos zips" true true
                    slot "socios" "Socios zips" false true
                ]
                "AU-ASIC", [ one "file" "The weekly zip or its CSV" ]
                "CL-RES", [ slot "files" "The yearly CSVs" true true ]
                "AR-C", [ one "zip" "The half-yearly zip" ]
            ]

    /// Builds the registry from streams by slot name; a slot not given is an empty list. Ukraine isn't here: its index is written as it streams.
    let build
        (id: string)
        (files: string -> Stream list)
        (date: BulkFileDate)
        (ct: CancellationToken)
        : Task<Result<ICompanyRegistry, RegistryFailure>> =
        let first name = files name |> List.head

        let maybe name =
            files name |> List.tryHead |> ValueOption.ofOption

        match id with
        | "CY" -> BulkSources.cyprusAsync (first "organisations") (maybe "offices") (maybe "officials") date ct
        | "RO" ->
            BulkSources.romaniaAsync
                (first "firms")
                (first "states")
                (maybe "stateNames")
                (maybe "representatives")
                date
                ct
        | "SI" -> BulkSources.sloveniaAsync (first "file") date ct
        | "BE" -> BulkSources.belgiumAsync (first "zip") date ct
        | "MD" -> BulkSources.moldovaAsync (first "file") date ct
        | "RU-SME" -> BulkSources.russiaSmeAsync (first "zip") date ct
        | "US-FL" -> BulkSources.floridaAsync (files "cordata" @ files "daily") date ct
        | "BR" -> BulkSources.brazilAsync (files "empresas") (files "estabelecimentos") (files "socios") date ct
        | "AU-ASIC" -> BulkSources.asicCompaniesAsync (first "file") date ct
        | "CL-RES" -> BulkSources.chileAsync (files "files") date ct
        | "AR-C" -> BulkSources.argentinaCabaAsync (first "zip") date ct
        | other -> Task.FromResult(Error(RegistryFailure.InvalidRequest $"No builder for {other}"))

/// A download or build under way, read by the API while it runs.
type private Job(expected: int64 voption) =
    let fetched = Dictionary<string, int64>()
    member val Indexing = false with get, set
    member val File = "" with get, set
    member _.Expected = expected

    member this.Report(progress: BulkProgress) =
        lock fetched (fun () ->
            fetched[progress.File] <- progress.Bytes
            this.File <- progress.File)

    /// Bytes fetched so far, across the source's files.
    member _.Done = lock fetched (fun () -> fetched.Values |> Seq.sum)

/// The bulk sources the server holds in its data folder: downloads and builds run in the background, one at a time per source, and each built
/// index is opened once and kept open until Remove or a new download.
type BulkService(folder: string, log: ILogger) =

    let downloads = BulkDownloads.Create folder
    let gate = obj ()
    let jobs = Dictionary<string, Job>()
    let errors = Dictionary<string, string>()
    let opened = Dictionary<string, ICompanyRegistry>()
    let opening = new SemaphoreSlim(1, 1)

    let indexFolder (entry: BulkCatalogueEntry) = Path.Combine(folder, entry.Id, "index")

    let isRunning id =
        lock gate (fun () -> jobs.ContainsKey id)

    let folderBytes (path: string) =
        if Directory.Exists path then
            Directory.EnumerateFiles(path, "*", SearchOption.AllDirectories)
            |> Seq.sumBy (fun file -> FileInfo(file).Length)
        else
            0L

    /// The size of the entry's index when one is built from the files saved now.
    let indexBytes (entry: BulkCatalogueEntry) =
        let fromFilesHere () =
            let index = indexFolder entry

            if File.Exists(Path.Combine(index, "manifest.json")) then
                ValueSome(folderBytes index)
            else
                ValueNone

        match entry.Delivery, downloads.IndexStateOf entry with
        | BulkDelivery.Links _, BulkIndexState.Built bytes
        | BulkDelivery.Resolved _, BulkIndexState.Built bytes -> ValueSome bytes
        | BulkDelivery.Links _, BulkIndexState.NotBuilt -> ValueNone
        | BulkDelivery.Resolved _, BulkIndexState.NotBuilt ->
            match downloads.StateOf entry with
            | BulkCacheState.Downloaded _ -> ValueNone
            | BulkCacheState.NotDownloaded
            | BulkCacheState.ByHand -> fromFilesHere ()
        | BulkDelivery.ByHand _, _ -> fromFilesHere ()

    let fail (id: string) (message: string) =
        log.LogWarning("Bulk source {Id}: {Message}", id, message)
        lock gate (fun () -> errors[id] <- message)

    let dispose (registry: ICompanyRegistry) =
        match registry with
        | :? IDisposable as disposable -> disposable.Dispose()
        | _ -> ()

    /// Runs work while holding the opening gate, so that no index is opened, closed or replaced at the same time.
    let exclusive (work: unit -> Task<'T>) =
        task {
            do! opening.WaitAsync()

            try
                return! work ()
            finally
                opening.Release() |> ignore
        }

    /// Closes the entry's registry; the caller holds the opening gate.
    let closeHeld (id: string) =
        let removed =
            lock gate (fun () ->
                match opened.TryGetValue id with
                | true, registry ->
                    opened.Remove id |> ignore
                    ValueSome registry
                | _ -> ValueNone)

        match removed with
        | ValueSome registry -> dispose registry
        | ValueNone -> ()

    let close (id: string) =
        exclusive (fun () -> task { closeHeld id })

    let keep (id: string) (registry: ICompanyRegistry) =
        exclusive (fun () -> task { lock gate (fun () -> opened[id] <- registry) })

    let openEntry (entry: BulkCatalogueEntry) =
        match downloads.StateOf entry with
        | BulkCacheState.Downloaded _ -> downloads.LoadAsync(entry, CancellationToken.None)
        | BulkCacheState.NotDownloaded
        | BulkCacheState.ByHand -> BulkIndexes.openAsync (indexFolder entry) CancellationToken.None

    /// Starts work as the entry's job, unless one runs; the job ends, and its failure is kept, when work does.
    let start (entry: BulkCatalogueEntry) (job: Job) (work: Job -> Task<Result<unit, string>>) =
        let started =
            lock gate (fun () ->
                if jobs.ContainsKey entry.Id then
                    false
                else
                    jobs[entry.Id] <- job
                    errors.Remove entry.Id |> ignore
                    true)

        if started then
            Task.Run(fun () ->
                task {
                    try
                        try
                            match! work job with
                            | Ok() -> ()
                            | Error message -> fail entry.Id message
                        with error ->
                            fail entry.Id error.Message
                    finally
                        lock gate (fun () -> jobs.Remove entry.Id |> ignore)
                }
                :> Task)
            |> ignore

            Ok()
        else
            Error $"{entry.Title} is already downloading or building"

    let download (entry: BulkCatalogueEntry) (job: Job) =
        task {
            do! close entry.Id

            // Files already saved and fresh, without an index, are built without fetching them again.
            let fetchFirst =
                match downloads.StateOf entry, downloads.IndexStateOf entry with
                | BulkCacheState.Downloaded(_, _, false), BulkIndexState.NotBuilt -> false
                | _ -> true

            let progress =
                { new IProgress<BulkProgress> with
                    member _.Report progress = job.Report progress
                }

            let! saved =
                if fetchFirst then
                    task {
                        match! downloads.DownloadAsync(entry, ValueSome progress, CancellationToken.None) with
                        | Ok _ -> return Ok()
                        | Error failure -> return Error(RegistryFailure.describe failure)
                    }
                else
                    Task.FromResult(Ok())

            match saved with
            | Error message -> return Error message
            | Ok() ->
                job.Indexing <- true

                match! downloads.LoadAsync(entry, CancellationToken.None) with
                | Ok registry ->
                    do! keep entry.Id registry
                    return Ok()
                | Error failure -> return Error(RegistryFailure.describe failure)
        }

    /// Builds the entry from the files by slot into a folder of its own, then, once built, puts that index in place and opens it. For a resolved
    /// entry the files replace any download, which is removed only then; a failed build leaves the files and index there were.
    let buildFrom (entry: BulkCatalogueEntry) (paths: Map<string, string list>) (job: Job) =
        task {
            job.Indexing <- true
            let index = Path.Combine(folder, ".building", entry.Id)

            if Directory.Exists index then
                Directory.Delete(index, true)

            Directory.CreateDirectory(Path.GetDirectoryName index) |> ignore

            let date =
                {
                    DownloadedAt =
                        paths
                        |> Map.values
                        |> Seq.concat
                        |> Seq.map (fun path -> DateTimeOffset(File.GetLastWriteTimeUtc path))
                        |> Seq.min
                    AsOf = ValueNone
                }

            let streams = List<Stream>()

            let files name =
                paths
                |> Map.tryFind name
                |> Option.defaultValue []
                |> List.map (fun path ->
                    let stream = File.OpenRead path :> Stream
                    streams.Add stream
                    stream)

            let! built =
                task {
                    try
                        return!
                            match entry.Id with
                            | "UA" ->
                                BulkIndexes.saveUkraineAsync (files "uo" |> List.head) date index CancellationToken.None
                            | id ->
                                task {
                                    match! ByHand.build id files date CancellationToken.None with
                                    | Error failure -> return Error failure
                                    | Ok built ->
                                        let! written = BulkIndexes.saveAsync id built index CancellationToken.None
                                        dispose built
                                        return written
                                }
                    finally
                        for stream in streams do
                            stream.Dispose()
                }

            match built with
            | Error failure ->
                if Directory.Exists index then
                    Directory.Delete(index, true)

                return Error(RegistryFailure.describe failure)
            | Ok() ->
                let placed = indexFolder entry

                do!
                    exclusive (fun () ->
                        task {
                            closeHeld entry.Id

                            match entry.Delivery with
                            | BulkDelivery.Resolved _ -> downloads.Remove entry
                            | BulkDelivery.Links _
                            | BulkDelivery.ByHand _ -> ()

                            if Directory.Exists placed then
                                Directory.Delete(placed, true)

                            Directory.CreateDirectory(Path.GetDirectoryName placed) |> ignore
                            Directory.Move(index, placed)
                        })

                match! BulkIndexes.openAsync placed CancellationToken.None with
                | Ok registry ->
                    do! keep entry.Id registry
                    return Ok()
                | Error failure -> return Error(RegistryFailure.describe failure)
        }

    let stateOf (entry: BulkCatalogueEntry) : BulkDto =
        let job, error =
            lock gate (fun () ->
                (match jobs.TryGetValue entry.Id with
                 | true, job -> ValueSome job
                 | _ -> ValueNone),
                (match errors.TryGetValue entry.Id with
                 | true, error -> error
                 | _ -> null))

        let cache = downloads.StateOf entry
        let built = if job.IsSome then ValueNone else indexBytes entry

        let state =
            match job, cache, built with
            | ValueSome job, _, _ when job.Indexing -> BulkState.Indexing
            | ValueSome _, _, _ -> BulkState.Downloading
            | ValueNone, _, ValueSome _ -> BulkState.IndexBuilt
            | ValueNone, BulkCacheState.Downloaded _, ValueNone -> BulkState.Downloaded
            | ValueNone, _, ValueNone -> BulkState.NotDownloaded

        let toNullable value =
            match value with
            | ValueSome value -> Nullable value
            | ValueNone -> Nullable()

        let catalogued, how, portal =
            match entry.Delivery with
            | BulkDelivery.Links links -> ValueSome(links |> List.sumBy _.ApproximateBytes), null, null
            | BulkDelivery.Resolved resolver ->
                ValueSome resolver.ApproximateBytes, resolver.ByHand, $"{resolver.Portal}: {resolver.Dataset}"
            | BulkDelivery.ByHand how -> ValueNone, how, null

        {
            Id = entry.Id
            Title = entry.Title
            Flag = entry.Jurisdiction.Country.ToLowerInvariant()
            Page = string entry.Page
            Delivery =
                match entry.Delivery with
                | BulkDelivery.Links _ -> "links"
                | BulkDelivery.Resolved _ -> "resolved"
                | BulkDelivery.ByHand _ -> "by-hand"
            Bytes = toNullable catalogued
            RefreshDays = entry.Refresh.TotalDays
            Licence = entry.Licence
            How = how
            Portal = portal
            Slots =
                ByHand.slots
                |> Map.tryFind entry.Id
                |> Option.defaultValue []
                |> List.map (fun slot ->
                    {
                        BulkSlotDto.Name = slot.Name
                        Label = slot.Label
                        Required = slot.Required
                        Many = slot.Many
                    })
                |> Array.ofList
            Files =
                (if job.IsSome then [] else downloads.ResolvedFilesOf entry)
                |> List.map bulkFileDto
                |> Array.ofList
            State = state
            File = job |> ValueOption.map _.File |> ValueOption.toObj
            Done = job |> ValueOption.map _.Done |> toNullable
            Expected = job |> ValueOption.bind _.Expected |> toNullable
            SavedBytes =
                match cache with
                | BulkCacheState.Downloaded(_, bytes, _) -> Nullable bytes
                | _ -> Nullable()
            IndexBytes = toNullable built
            DownloadedAt =
                match cache with
                | BulkCacheState.Downloaded(at, _, _) -> at.ToString("o", Globalization.CultureInfo.InvariantCulture)
                | _ -> null
            Stale =
                match cache with
                | BulkCacheState.Downloaded(_, _, stale) -> stale
                | _ -> false
            Error = error
        }

    member _.States() = BulkCatalogue.all |> List.map stateOf

    /// Starts downloading the entry's files and building its index.
    member _.StartDownload(id: string) =
        match BulkCatalogue.tryFind id with
        | ValueNone -> Error $"No bulk source has the id {id}"
        | ValueSome { Delivery = BulkDelivery.ByHand how } -> Error $"It is fetched by hand: {how}"
        | ValueSome({ Delivery = BulkDelivery.Links links } as entry) ->
            start entry (Job(ValueSome(links |> List.sumBy _.ApproximateBytes))) (download entry)
        | ValueSome({
                        Delivery = BulkDelivery.Resolved resolver
                    } as entry) -> start entry (Job(ValueSome resolver.ApproximateBytes)) (download entry)

    /// The files a download of the entry would fetch now, read from its listing without downloading anything.
    member _.CurrentFiles(id: string) =
        task {
            match BulkCatalogue.tryFind id with
            | ValueNone -> return Error $"No bulk source has the id {id}"
            | ValueSome entry ->
                match! downloads.ResolveAsync(entry, CancellationToken.None) with
                | Ok files -> return Ok files
                | Error failure -> return Error(RegistryFailure.describe failure)
        }

    /// Starts building a by-hand or resolved entry's index from files on this machine, by slot name.
    member _.StartBuild(id: string, paths: Map<string, string list>) =
        match BulkCatalogue.tryFind id, ByHand.slots.TryFind id with
        | ValueNone, _ -> Error $"No bulk source has the id {id}"
        | ValueSome _, None -> Error "Building this source from files here isn't supported yet"
        | ValueSome entry, Some slots ->
            let given name =
                paths |> Map.tryFind name |> Option.defaultValue []

            let missing =
                slots
                |> List.filter (fun slot -> slot.Required && (given slot.Name).IsEmpty)
                |> List.map _.Label

            let unknown =
                paths
                |> Map.keys
                |> Seq.filter (fun name -> slots |> List.forall (fun slot -> slot.Name <> name))
                |> List.ofSeq

            let tooMany =
                slots
                |> List.filter (fun slot -> not slot.Many && (given slot.Name).Length > 1)
                |> List.map _.Label

            // A full path on this machine's own drives: a network share or device path would have the server, as its owner, reach elsewhere.
            // Either separator, twice, opens a share's or a device's name, in the path as given and as the system resolves it.
            let local (path: string) =
                let isSeparator (c: char) = c = '/' || c = '\\'

                let opensShare (text: string) =
                    text.Length >= 2 && isSeparator text[0] && isSeparator text[1]

                try
                    Path.IsPathFullyQualified path
                    && not (opensShare path)
                    && not (opensShare (Path.GetFullPath path))
                with
                | :? ArgumentException
                | :? NotSupportedException
                | :? IOException -> false

            let notLocal =
                paths |> Map.values |> Seq.concat |> Seq.filter (local >> not) |> List.ofSeq

            let absent =
                paths
                |> Map.values
                |> Seq.concat
                |> Seq.filter (fun path -> local path && not (File.Exists path))
                |> List.ofSeq

            match missing, unknown, tooMany, absent with
            | _ when not notLocal.IsEmpty ->
                Error(
                    "A full path on this computer's own drives is needed: "
                    + String.Join(", ", notLocal)
                )
            | [], [], [], [] -> start entry (Job ValueNone) (buildFrom entry paths)
            | missing, [], [], [] -> Error("Needed: " + String.Join(", ", missing))
            | _, unknown, [], [] when not unknown.IsEmpty ->
                Error("Not a file of this source: " + String.Join(", ", unknown))
            | _, _, tooMany, [] when not tooMany.IsEmpty -> Error("One file only: " + String.Join(", ", tooMany))
            | _, _, _, absent -> Error("No such file: " + String.Join(", ", absent))

    /// The registries of every entry whose index is built, opening those not open yet. An entry that fails to open is left out, with its
    /// failure kept.
    member _.Registries() : Task<(BulkCatalogueEntry * ICompanyRegistry) list> =
        exclusive (fun () ->
            task {
                let wanted =
                    BulkCatalogue.all
                    |> List.filter (fun entry ->
                        not (isRunning entry.Id)
                        && not (lock gate (fun () -> opened.ContainsKey entry.Id))
                        && (indexBytes entry).IsSome)

                let! made =
                    wanted
                    |> List.map (fun entry ->
                        task {
                            let! result = openEntry entry
                            return entry, result
                        })
                    |> Task.WhenAll

                for entry, result in made do
                    match result with
                    | Ok registry -> lock gate (fun () -> opened[entry.Id] <- registry)
                    | Error failure -> fail entry.Id (RegistryFailure.describe failure)

                return
                    lock gate (fun () ->
                        BulkCatalogue.all
                        |> List.choose (fun entry ->
                            match opened.TryGetValue entry.Id with
                            | true, registry -> Some(entry, registry)
                            | _ -> None))
            })

    /// Closes the entry's registry and deletes its files and index.
    member _.Remove(id: string) =
        task {
            match BulkCatalogue.tryFind id with
            | ValueNone -> return Error $"No bulk source has the id {id}"
            | ValueSome _ when isRunning id -> return Error "It is downloading or building; wait until it ends"
            | ValueSome entry ->
                // Closed and deleted under the opening gate, so that Registries can't reopen the index in between.
                do!
                    exclusive (fun () ->
                        task {
                            closeHeld id

                            downloads.Remove entry
                        })

                lock gate (fun () -> errors.Remove id |> ignore)
                return Ok()
        }

    interface IDisposable with
        member _.Dispose() =
            lock gate (fun () ->
                for registry in opened.Values do
                    dispose registry

                opened.Clear())

            (downloads :> IDisposable).Dispose()
            opening.Dispose()
