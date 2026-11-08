namespace CompanyDueDiligence

open System
open System.Collections.Generic
open System.IO
open System.Text
open System.Text.Json
open System.Threading
open System.Threading.Tasks
open System.Xml

/// When the host downloaded a register's file, and the date the file says its data is as of, if it says one.
[<Struct>]
type BulkFileDate =
    {
        DownloadedAt: DateTimeOffset
        AsOf: DateOnly voption
    }

/// One company as a downloaded file holds it. ValueNone for officers or owners means the file doesn't publish them.
type internal BulkRecord =
    {
        Company: Company
        Officers: Officer list voption
        Owners: OwnerListing voption
        /// The record's own text in the file, kept as the evidence's Raw.
        Raw: string
    }

/// What a bulk register's file says about a number it doesn't hold.
[<RequireQualifiedAccess>]
type internal BulkAbsence =
    /// The file lists every company the register holds, ended ones too, so a number not in it isn't held.
    | NotHeld
    /// The file leaves some companies out, so a number not in it can still be held.
    | CantTell of reason: string

/// What one row of a file gives a register.
[<RequireQualifiedAccess>]
type internal BulkRow =
    | Record of BulkRecord
    /// A row that can't be read, with the number it is for as the file writes it, and why.
    | Unreadable of number: string * reason: string
    /// A row that can't be read and names no number that can be looked up.
    | WithoutNumber

/// Items by key, where a key given by two items holds neither and is repeated. One builder fills it and hands out Held and Repeated, which are
/// read-only views; a file of millions of rows is indexed in place rather than copied.
[<Sealed>]
type internal BulkIndex<'T>() =
    let held = Dictionary<string, 'T>()
    let repeated = HashSet<string>()

    /// Holds item under key, unless the key is already held or repeated: then it is repeated.
    member _.Add(key: string, item: 'T) =
        if not (repeated.Contains key) then
            if held.Remove key then
                repeated.Add key |> ignore
            else
                held[key] <- item

    /// Holds item under each key, as Add does; a key given twice counts once.
    member this.AddEach(keys: string list, item: 'T) =
        keys |> List.distinct |> List.iter (fun key -> this.Add(key, item))

    /// Marks key repeated, holding nothing under it.
    member _.Repeat(key: string) =
        held.Remove key |> ignore
        repeated.Add key |> ignore

    /// Holds nothing under key, without marking it repeated; true when something was held.
    member _.Withhold(key: string) = held.Remove key

    member _.Held = held :> IReadOnlyDictionary<_, _>
    member _.Repeated = repeated :> IReadOnlySet<_>

/// A file's rows as a register takes them: records by normalised number with the numbers several records hold kept apart, the count of
/// records, the rows that couldn't be read with the number each is for, and the count of those naming no number.
type internal BulkTally =
    {
        Held: IReadOnlyDictionary<string, BulkRecord>
        Repeated: IReadOnlySet<string>
        Records: int
        Unreadable: (string * string) list
        WithoutNumber: int
    }

/// Records that give each key's company name without building the record, for a search to scan.
type internal INamedRecords =
    /// Each key with its record's name.
    abstract Names: struct (string * string) seq

/// Reasons a bulk register gives in more than one place.
module internal BulkReasons =

    /// What a number several records in the file carry answers.
    [<Literal>]
    let SeveralRecords =
        "Several records in the file carry this number, so it can't tell which is meant"

/// Everything a built bulk register answers from, for writing it to an index. An answer read from these is the register's own answer.
type internal BulkContents =
    {
        Coverage: RegistryCoverage
        /// The register's number normalisation: a number to its key, or ValueNone for a number that can't be valid.
        Normal: string -> string voption
        InvalidNumber: string
        Date: BulkFileDate
        Absence: BulkAbsence
        OfficersNotOffered: string
        /// What a held company whose record lists no owners answers, by purpose.
        Owners: OwnerPurpose -> Availability<OwnerListing>
        Records: IReadOnlyDictionary<string, BulkRecord>
        /// Keys several records carry, which answer BulkReasons.SeveralRecords unless Withheld gives a reason.
        Repeated: IReadOnlySet<string>
        /// Keys answering NotAvailable with this reason before any record is read.
        Withheld: IReadOnlyDictionary<string, string>
        /// The reason a held company's Available officers answer NotAvailable instead, by key.
        OfficersWithheld: string -> string voption
        /// The reason a held company's Available owners answer NotAvailable instead, by key.
        OwnersWithheld: string -> string voption
        /// True when a search is never Complete, because the file held records the register left out, or leaves companies out by design.
        Excluded: bool
        /// Each key with its record's name, in the order a search reads them.
        Names: struct (string * string) seq
        /// Keys the source's own normaliser gives that this register answers under another key, with that key.
        Aliases: struct (string * string) seq
    }

/// A bulk register that can be written to an index.
type internal IBulkContents =
    abstract Contents: BulkContents

/// How a bulk search, built or from an index, tests the upper-cased names a file holds.
module internal BulkNames =

    /// A test of an upper-cased held name against the name asked. Contains is the upper-cased term inside it, compared ordinally; Exact and
    /// StartsWith are NameMatch.matcher's.
    let matcher (matching: NameMatch) (name: string) : string -> bool =
        match matching with
        | NameMatch.Contains ->
            let term = name.Trim().ToUpperInvariant()
            fun held -> held.Contains(term, StringComparison.Ordinal)
        | NameMatch.Exact
        | NameMatch.StartsWith -> NameMatch.matcher matching name

/// A register read from a file the host downloads on its own schedule and passes in; nothing is called. Answers are as of the file's date. A
/// number in repeated is held by several records the file can't tell apart, and answers NotAvailable whatever absence says.
[<Sealed>]
type internal BulkRegistry
    (
        coverage: RegistryCoverage,
        normal: string -> string voption,
        invalidNumber: string,
        records: IReadOnlyDictionary<string, BulkRecord>,
        date: BulkFileDate,
        absence: BulkAbsence,
        officersNotOffered: string,
        owners: OwnerPurpose -> Availability<OwnerListing>,
        repeated: IReadOnlySet<string>
    ) =

    static let searchLimit = 50

    // Each key with its record's name; records held compactly aren't built for it.
    let keyNames () =
        match records with
        | :? INamedRecords as named -> named.Names
        | _ -> records |> Seq.map (fun pair -> struct (pair.Key, pair.Value.Company.Name))

    // Upper-cased names with their keys, built at the first search.
    let names =
        lazy
            (keyNames ()
             |> Seq.map (fun struct (key, name) -> struct (name.ToUpperInvariant(), key))
             |> Array.ofSeq)

    let evidence raw value : Evidence<_> =
        {
            Source = coverage.Registry
            RetrievedAt = date.DownloadedAt
            AsOf = date.AsOf
            Raw = raw
            Value = value
        }

    let answer (number: string) jurisdiction (read: BulkRecord voption -> Evidence<Availability<'T>>) =
        SourceAnswer.inCoverage coverage jurisdiction (fun () ->
            match normal number with
            | ValueNone -> Task.FromResult(Error(RegistryFailure.InvalidRequest invalidNumber))
            | ValueSome key when repeated.Contains key ->
                Task.FromResult(Ok(evidence ValueNone (NotAvailable BulkReasons.SeveralRecords)))
            | ValueSome key ->
                match records.TryGetValue key with
                | true, record -> Task.FromResult(Ok(read (ValueSome record)))
                | _ -> Task.FromResult(Ok(read ValueNone)))

    let absent () =
        match absence with
        | BulkAbsence.NotHeld -> Available ValueNone
        | BulkAbsence.CantTell reason -> NotAvailable reason

    // A search is Complete only over a file that lists every company and holds no number twice.
    let excluded =
        repeated.Count > 0
        || (match absence with
            | BulkAbsence.NotHeld -> false
            | BulkAbsence.CantTell _ -> true)

    let search (name: string) (matching: NameMatch) jurisdiction (ct: CancellationToken) =
        SourceAnswer.inCoverage coverage jurisdiction (fun () ->
            NameSearch.withName name matching (fun () ->
                let matches = BulkNames.matcher matching name

                let found =
                    names.Value
                    |> Seq.filter (fun struct (held, _) ->
                        ct.ThrowIfCancellationRequested()
                        matches held)
                    |> Seq.map (fun struct (_, key) -> records[key])
                    // A record held under two keys, such as a number and its short form, is one match.
                    |> Seq.distinctBy _.Company.Number
                    |> Seq.truncate (searchLimit + 1)
                    |> Seq.map _.Company
                    |> List.ofSeq

                let result =
                    OpenData.matches searchLimit ValueNone (List.truncate searchLimit found)

                Task.FromResult(
                    Ok(
                        evidence
                            ValueNone
                            (Available
                                { result with
                                    Complete = found.Length <= searchLimit && not excluded
                                })
                    )
                )))

    /// A register with no number repeated in its file.
    new(coverage, normal, invalidNumber, records, date, absence, officersNotOffered, owners) =
        BulkRegistry(
            coverage,
            normal,
            invalidNumber,
            records,
            date,
            absence,
            officersNotOffered,
            owners,
            HashSet<string>()
        )

    /// How many companies the file held.
    member _.Count = records.Count

    interface IBulkContents with
        member _.Contents =
            {
                Coverage = coverage
                Normal = normal
                InvalidNumber = invalidNumber
                Date = date
                Absence = absence
                OfficersNotOffered = officersNotOffered
                Owners = owners
                Records = records
                Repeated = repeated
                Withheld = readOnlyDict []
                OfficersWithheld = fun _ -> ValueNone
                OwnersWithheld = fun _ -> ValueNone
                Excluded = excluded
                Names = keyNames ()
                Aliases = Seq.empty
            }

    interface ICompanyRegistry with
        member _.Coverage = coverage

        member _.TryFindAsync(number: string, jurisdiction: Jurisdiction, _: CancellationToken) =
            answer number jurisdiction (function
                | ValueSome record -> evidence (ValueSome record.Raw) (Available(ValueSome record.Company))
                | ValueNone -> evidence ValueNone (absent ()))

        /// Names containing the term, up to 50. Complete is false when there are more, when the file held records left out of the index as
        /// repeated, or when the file leaves companies out, so a number not in it can still be held.
        member _.SearchAsync(name: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            search name NameMatch.Contains jurisdiction ct

        member _.OfficersAsync(number: string, jurisdiction: Jurisdiction, _: CancellationToken) =
            answer number jurisdiction (function
                | ValueSome {
                                Officers = ValueSome officers
                                Raw = raw
                            } -> evidence (ValueSome raw) (Available officers)
                | ValueSome _ -> evidence ValueNone (NotAvailable officersNotOffered)
                | ValueNone ->
                    evidence
                        ValueNone
                        (match absent () with
                         | Available _ -> NotAvailable "The file holds no company with this number"
                         | NotAvailable reason -> NotAvailable reason
                         | NeedsEntitlement entitlement -> NeedsEntitlement entitlement))

        member _.OwnersAsync(number: string, jurisdiction: Jurisdiction, purpose: OwnerPurpose, _: CancellationToken) =
            answer number jurisdiction (function
                | ValueSome {
                                Owners = ValueSome listing
                                Raw = raw
                            } -> evidence (ValueSome raw) (Available listing)
                | ValueSome _ -> evidence ValueNone (owners purpose)
                | ValueNone -> evidence ValueNone (NotAvailable "The file holds no company with this number"))

    // Names matching as BulkNames.matcher tests, up to 50, with Complete as SearchAsync gives it.
    interface INameMatchSearch with
        member _.SearchByNameAsync
            (name: string, matching: NameMatch, jurisdiction: Jurisdiction, ct: CancellationToken)
            =
            search name matching jurisdiction ct

/// A bulk register whose officers or owners are withheld for some companies: an Available answer for a number the function gives a reason for is
/// NotAvailable with that reason. Every other answer is the inner register's.
[<Sealed>]
type internal BulkPartiesWithheld
    (
        inner: ICompanyRegistry,
        normal: string -> string voption,
        officersWithheld: string -> string voption,
        ownersWithheld: string -> string voption
    ) =

    let replace
        (withheld: string -> string voption)
        (number: string)
        (call: Task<Result<Evidence<Availability<'T>>, RegistryFailure>>)
        =
        task {
            match! call with
            | Ok({ Value = Available _ } as found) ->
                return
                    match normal number |> ValueOption.bind withheld with
                    | ValueSome reason ->
                        Ok
                            { found with
                                Raw = ValueNone
                                Value = NotAvailable reason
                            }
                    | ValueNone -> Ok found
            | other -> return other
        }

    // The inner register's reason comes first: it has already turned the answer into NotAvailable.
    static let after (inner: string -> string voption) (outer: string -> string voption) (key: string) =
        match inner key with
        | ValueSome reason -> ValueSome reason
        | ValueNone -> outer key

    interface IBulkContents with
        member _.Contents =
            let contents = (inner :?> IBulkContents).Contents

            { contents with
                OfficersWithheld = after contents.OfficersWithheld officersWithheld
                OwnersWithheld = after contents.OwnersWithheld ownersWithheld
            }

    interface INameMatchSearch with
        member _.SearchByNameAsync(name, matching, jurisdiction, ct) =
            Registries.searchByNameAsync inner name matching jurisdiction ct

    interface ICompanyRegistry with
        member _.Coverage = inner.Coverage

        member _.TryFindAsync(number, jurisdiction, ct) =
            inner.TryFindAsync(number, jurisdiction, ct)

        member _.SearchAsync(name, jurisdiction, ct) =
            inner.SearchAsync(name, jurisdiction, ct)

        member _.OfficersAsync(number, jurisdiction, ct) =
            replace officersWithheld number (inner.OfficersAsync(number, jurisdiction, ct))

        member _.OwnersAsync(number, jurisdiction, purpose, ct) =
            replace ownersWithheld number (inner.OwnersAsync(number, jurisdiction, purpose, ct))

/// Records held compactly, each built into a BulkRecord when asked for, so a file of millions of records stays near its own size in memory.
/// Enumerating the values builds every record in turn and keeps none; a search reads names alone, through nameOf.
[<Sealed>]
type internal LazyRecords<'C>(held: IReadOnlyDictionary<string, 'C>, build: 'C -> BulkRecord, nameOf: 'C -> string) =

    let pairs () =
        held |> Seq.map (fun pair -> KeyValuePair(pair.Key, build pair.Value))

    interface INamedRecords with
        member _.Names = held |> Seq.map (fun pair -> struct (pair.Key, nameOf pair.Value))

    interface IReadOnlyDictionary<string, BulkRecord> with
        member _.Item
            with get key = build held[key]

        member _.Keys = held.Keys
        member _.Values = held.Values |> Seq.map build
        member _.ContainsKey key = held.ContainsKey key

        member _.TryGetValue(key: string, value: byref<BulkRecord>) =
            match held.TryGetValue key with
            | true, compact ->
                value <- build compact
                true
            | _ -> false

    interface IReadOnlyCollection<KeyValuePair<string, BulkRecord>> with
        member _.Count = held.Count

    interface IEnumerable<KeyValuePair<string, BulkRecord>> with
        member _.GetEnumerator() = (pairs ()).GetEnumerator()

    interface Collections.IEnumerable with
        member _.GetEnumerator() =
            (pairs ()).GetEnumerator() :> Collections.IEnumerator

/// A bulk register that answers NotAvailable for numbers its file holds but can't answer for: one held by several records, or one whose record
/// couldn't be read. Every other number goes to the BulkRegistry. A search is Complete only when excluded is false: no record was left out.
[<Sealed>]
type internal BulkAnswers
    (
        inner: ICompanyRegistry,
        normal: string -> string voption,
        withheld: IReadOnlyDictionary<string, string>,
        excluded: bool,
        date: BulkFileDate,
        aliases: struct (string * string) seq
    ) =

    let coverage = inner.Coverage

    let withheldReason (number: string) =
        match normal number with
        | ValueSome key ->
            match withheld.TryGetValue key with
            | true, reason -> ValueSome reason
            | _ -> ValueNone
        | ValueNone -> ValueNone

    let notAvailable reason : Evidence<Availability<'T>> =
        {
            Source = coverage.Registry
            RetrievedAt = date.DownloadedAt
            AsOf = date.AsOf
            Raw = ValueNone
            Value = NotAvailable reason
        }

    let answer number jurisdiction (call: unit -> Task<Result<Evidence<Availability<'T>>, RegistryFailure>>) =
        SourceAnswer.inCoverage coverage jurisdiction (fun () ->
            match withheldReason number with
            | ValueSome reason -> Task.FromResult(Ok(notAvailable reason))
            | ValueNone -> call ())

    /// A search's answer, never Complete when records were left out.
    let incompleteWhenExcluded (search: Task<Result<Evidence<Availability<SearchResult>>, RegistryFailure>>) =
        task {
            match! search with
            | Ok({ Value = Available result } as found) when excluded ->
                return
                    Ok
                        { found with
                            Value = Available { result with Complete = false }
                        }
            | other -> return other
        }

    /// Builds the registry over a file's tally, keeping apart the numbers held twice and those of rows that couldn't be read. aliases are other
    /// keys, such as a second identifier, withheld with their reasons without being records. keyAliases are keys the source's own normaliser
    /// gives that normal answers under another key, with that key, for writing an index; they are read only then. A NotHeld absence can't tell
    /// when rows naming no number couldn't be read.
    static member Over
        (
            coverage: RegistryCoverage,
            normal: string -> string voption,
            invalidNumber: string,
            rows: BulkTally,
            date: BulkFileDate,
            absence: BulkAbsence,
            officersNotOffered: string,
            owners: OwnerPurpose -> Availability<OwnerListing>,
            ?aliases: (string * string) seq,
            ?keyAliases: struct (string * string) seq
        ) : ICompanyRegistry =
        let repeatedReason =
            "The file holds several records for this number, which can't be told apart"

        // A repeated number's reason comes first; a number also held, or already withheld, keeps what it has.
        let withheld =
            Seq.append rows.Unreadable (defaultArg aliases Seq.empty)
            |> Seq.choose (fun (number, reason) ->
                normal number
                |> ValueOption.filter (rows.Held.ContainsKey >> not)
                |> ValueOption.map (fun key -> key, reason)
                |> ValueOption.toOption)
            |> Seq.append (rows.Repeated |> Seq.map (fun key -> key, repeatedReason))
            |> Seq.distinctBy fst
            |> readOnlyDict

        let absence =
            match absence with
            | BulkAbsence.NotHeld when rows.WithoutNumber > 0 ->
                BulkAbsence.CantTell
                    $"{rows.WithoutNumber} records in the file couldn't be read, so a number not found can be one of them"
            | other -> other

        let inner =
            BulkRegistry(coverage, normal, invalidNumber, rows.Held, date, absence, officersNotOffered, owners)

        let excluded =
            rows.Repeated.Count > 0 || not rows.Unreadable.IsEmpty || rows.WithoutNumber > 0

        BulkAnswers(inner, normal, withheld, excluded, date, defaultArg keyAliases Seq.empty)

    interface IBulkContents with
        member _.Contents =
            let contents = (inner :?> IBulkContents).Contents

            // This register's reason comes before the inner one's.
            let reasons =
                if contents.Withheld.Count = 0 then
                    withheld
                else
                    Seq.append withheld contents.Withheld
                    |> Seq.distinctBy _.Key
                    |> Seq.map (fun pair -> pair.Key, pair.Value)
                    |> readOnlyDict

            { contents with
                Withheld = reasons
                Excluded = contents.Excluded || excluded
                Aliases = Seq.append aliases contents.Aliases
            }

    interface INameMatchSearch with
        member _.SearchByNameAsync
            (name: string, matching: NameMatch, jurisdiction: Jurisdiction, ct: CancellationToken)
            =
            incompleteWhenExcluded (Registries.searchByNameAsync inner name matching jurisdiction ct)

    interface ICompanyRegistry with
        member _.Coverage = coverage

        member _.TryFindAsync(number: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            answer number jurisdiction (fun () -> inner.TryFindAsync(number, jurisdiction, ct))

        member _.SearchAsync(name: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            incompleteWhenExcluded (inner.SearchAsync(name, jurisdiction, ct))

        member _.OfficersAsync(number: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            answer number jurisdiction (fun () -> inner.OfficersAsync(number, jurisdiction, ct))

        member _.OwnersAsync(number: string, jurisdiction: Jurisdiction, purpose: OwnerPurpose, ct: CancellationToken) =
            answer number jurisdiction (fun () -> inner.OwnersAsync(number, jurisdiction, purpose, ct))

/// Building a bulk register from the rows of the host's files, and running the build so that a file that can't be read is UnreadableAnswer.
module internal Bulk =

    /// The failure for a file that can't be read, holding no text from it; ValueNone for an error that isn't about the file.
    let unreadable (error: exn) =
        match error with
        | BulkFile.UnreadableFile reason
        | Json.InvalidJson reason -> ValueSome reason
        | :? JsonException as error -> ValueSome $"Not JSON, at line {error.LineNumber}"
        | :? XmlException as error -> ValueSome $"Not XML, at line {error.LineNumber}"
        | :? InvalidDataException -> ValueSome "The file isn't the zip or layout the source publishes"
        | :? DecoderFallbackException -> ValueSome "Text that isn't in the file's encoding"
        | :? IOException -> ValueSome "The file couldn't be read to its end"
        | _ -> ValueNone

    /// Runs a build off the caller's thread. A file that can't be read is UnreadableAnswer, holding no text from it. Cancelling ct throws.
    let run
        (ct: CancellationToken)
        (build: unit -> ICompanyRegistry)
        : Task<Result<ICompanyRegistry, RegistryFailure>> =
        Task.Run(
            (fun () ->
                try
                    Ok(build ())
                with error ->
                    match unreadable error with
                    | ValueSome reason -> Error(RegistryFailure.UnreadableAnswer reason)
                    | ValueNone -> reraise ()),
            ct
        )

    /// Runs a build that reads its files asynchronously, as run does.
    let runAsync (build: unit -> Task<ICompanyRegistry>) : Task<Result<ICompanyRegistry, RegistryFailure>> =
        task {
            try
                let! registry = build ()
                return Ok registry
            with error ->
                match unreadable error with
                | ValueSome reason -> return Error(RegistryFailure.UnreadableAnswer reason)
                | ValueNone ->
                    Runtime.ExceptionServices.ExceptionDispatchInfo.Capture(error).Throw()
                    return Unchecked.defaultof<_>
        }

    /// Items by every key given with each, as BulkIndex holds them; a key given twice by one item counts once.
    let indexEach (keys: 'T -> string list) (items: 'T seq) =
        let index = BulkIndex<'T>()
        items |> Seq.iter (fun item -> index.AddEach(keys item, item))
        index.Held, index.Repeated

    /// Items by their key, as BulkIndex holds them; an item without a key is left out.
    let index (key: 'T -> string voption) (items: 'T seq) =
        let index = BulkIndex<'T>()

        items
        |> Seq.iter (fun item -> key item |> ValueOption.iter (fun key -> index.Add(key, item)))

        index.Held, index.Repeated

    /// Items of several files read in order, each indexed alone: a later file's item replaces an earlier one's, as a daily file updates a full
    /// one, and a key twice in one file is repeated unless a later file settles it.
    let layered (key: 'T -> string voption) (files: 'T seq seq) =
        let held = Dictionary<string, 'T>()
        let repeated = HashSet<string>()

        files
        |> Seq.iter (fun items ->
            let found, twice = index key items

            found
            |> Seq.iter (fun pair ->
                held[pair.Key] <- pair.Value
                repeated.Remove pair.Key |> ignore)

            twice
            |> Seq.iter (fun key ->
                held.Remove key |> ignore
                repeated.Add key |> ignore))

        held :> IReadOnlyDictionary<_, _>, repeated :> IReadOnlySet<_>

    /// A file's rows tallied, with records keyed by their normalised number.
    let tally (normal: string -> string voption) (rows: BulkRow seq) : BulkTally =
        let index = BulkIndex<BulkRecord>()

        let struct (records, unreadable, withoutNumber) =
            rows
            |> Seq.fold
                (fun (struct (records, unreadable, withoutNumber)) row ->
                    match row with
                    | BulkRow.Record record ->
                        normal record.Company.Number
                        |> ValueOption.iter (fun key -> index.Add(key, record))

                        struct (records + 1, unreadable, withoutNumber)
                    | BulkRow.Unreadable(number, reason) ->
                        struct (records, (number, reason) :: unreadable, withoutNumber)
                    | BulkRow.WithoutNumber -> struct (records, unreadable, withoutNumber + 1))
                (struct (0, [], 0))

        {
            Held = index.Held
            Repeated = index.Repeated
            Records = records
            Unreadable = List.rev unreadable
            WithoutNumber = withoutNumber
        }

    /// Values by key, in the order the pairs give them. The pairs stream into the groups, so a file of millions of rows isn't held twice.
    let grouped (pairs: struct (string * 'V) seq) : IReadOnlyDictionary<string, 'V list> =
        let groups = Dictionary<string, 'V list>()

        pairs
        |> Seq.iter (fun struct (key, value) ->
            match groups.TryGetValue key with
            | true, held -> groups[key] <- value :: held
            | _ -> groups[key] <- [ value ])

        // Each group was gathered last first.
        groups.Keys
        |> Array.ofSeq
        |> Array.iter (fun key -> groups[key] <- List.rev groups[key])

        groups :> IReadOnlyDictionary<_, _>

    /// The values a dictionary of lists holds under key; none when it holds nothing.
    let listed (groups: IReadOnlyDictionary<string, 'V list>) (key: string) =
        match groups.TryGetValue key with
        | true, values -> values
        | _ -> []

    /// The host's file date, with the date the file states, when it states one, as AsOf.
    let asOf (date: BulkFileDate) (stated: DateOnly voption) =
        match stated with
        | ValueSome day -> { date with AsOf = ValueSome day }
        | ValueNone -> date

    /// What a file that lists every company says of a number it doesn't hold, unless rows were skipped, when it can't tell.
    let completeUnless (skipped: int) =
        if skipped = 0 then
            BulkAbsence.NotHeld
        else
            BulkAbsence.CantTell
                $"The file lists every company, but {skipped} rows couldn't be read, so a number not found may be one of those"

    /// Concerns of a register that states insolvency alone: stated for an insolvent company, and nothing else published.
    let insolvencyConcerns (insolvent: bool) =
        if insolvent then
            {
                Stated = [ CompanyConcern.InsolvencyHistory ]
                Unconfirmed = CompanyConcern.all |> List.except [ CompanyConcern.InsolvencyHistory ]
            }
        else
            CompanyConcerns.notPublished

    /// An address from its parts, or absent when every part is.
    let address
        (lines: string voption list)
        (locality: string voption)
        (region: string voption)
        (postalCode: string voption)
        (country: string voption)
        : Address voption =
        match lines |> List.choose ValueOption.toOption with
        | [] when locality.IsNone && region.IsNone && postalCode.IsNone && country.IsNone -> ValueNone
        | held ->
            ValueSome
                {
                    Lines = held
                    Locality = locality
                    Region = region
                    PostalCode = postalCode
                    Country = country
                }

    /// An address of this kind from its parts, or absent when every part is.
    let companyAddress (kind: AddressKind) lines locality region postalCode country : CompanyAddress voption =
        address lines locality region postalCode country
        |> ValueOption.map (fun address -> { Kind = kind; Address = address })
