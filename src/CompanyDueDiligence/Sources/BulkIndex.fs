namespace CompanyDueDiligence

open System
open System.Buffers.Binary
open System.Collections.Generic
open System.Globalization
open System.IO
open System.Text
open System.Text.Json
open System.Text.Unicode
open System.Threading
open System.Threading.Tasks
open Microsoft.Win32.SafeHandles

/// A file an index was built from, as its manifest records it.
type internal BulkIndexInput =
    {
        Name: string
        Bytes: int64
        Modified: DateTimeOffset
    }

/// What an index's manifest.json holds.
type internal BulkManifest =
    {
        Format: int
        /// The library version that wrote the index; ValueNone in a manifest that names none.
        Library: string voption
        /// The BulkCatalogue id of the source the register was built by.
        Source: string
        Coverage: RegistryCoverage
        Date: BulkFileDate
        Absence: BulkAbsence
        OfficersNotOffered: string
        InvalidNumber: string
        /// What a held company whose record lists no owners answers for customer due diligence.
        OwnersForDueDiligence: Availability<OwnerListing>
        /// What a held company whose record lists no owners answers for a public read.
        OwnersForPublicRead: Availability<OwnerListing>
        /// The reasons keys.bin gives by index.
        Reasons: string array
        Excluded: bool
        /// The longest key's UTF-8 bytes.
        KeyWidth: int
        Keys: int64
        Records: int64
        Names: int64
        /// The longest entry of names.bin, in bytes.
        LongestName: int
        RecordsBytes: int64
        KeysBytes: int64
        NamesBytes: int64
        Inputs: BulkIndexInput list
    }

/// One key of keys.bin: its record's offset in records.bin, the check of that record's number, and three indexes into the manifest's reasons,
/// each -1 for none.
[<Struct>]
type internal BulkKeyEntry =
    {
        Record: int64
        /// BulkKeyEntry.CheckOf the record's number, so a damaged offset landing on another record is caught.
        Check: uint32
        /// Why the key answers NotAvailable before its record is read: withheld, or held by several records.
        Find: int
        /// Why the record's Available officers answer NotAvailable instead.
        Officers: int
        /// Why the record's Available owners answer NotAvailable instead.
        Owners: int
    }

    /// A key the file doesn't hold.
    static member Absent =
        {
            Record = -1L
            Check = 0u
            Find = -1
            Officers = -1
            Owners = -1
        }

    /// The bytes after the key: the offset, the check and the three reasons.
    static member Tail = 24

    /// The 32-bit FNV-1a hash of a record number's UTF-8 bytes.
    static member CheckOf(number: string) =
        match number with
        | null -> 0u
        | _ ->
            Encoding.UTF8.GetBytes number
            |> Array.fold (fun hash next -> (hash ^^^ uint32 next) * 16777619u) 2166136261u

/// What a names scan tests each upper-cased name for.
[<RequireQualifiedAccess>]
type internal BulkNameTest =
    /// The upper-cased term inside the name, compared ordinally: as UTF-8 bytes when utf8 holds the term's bytes and the name is UTF-8.
    | Holds of term: string * utf8: byte array voption
    /// A test of the name's text.
    | Text of test: (string -> bool)

/// Reads names.bin's entries in order, by positional reads of its handle into a buffer that always holds a whole entry. One scan per search,
/// so searches in parallel don't share a position.
[<Sealed>]
type internal BulkNameScan(handle: SafeFileHandle, size: int64, longest: int) =
    let buffer = Array.zeroCreate<byte>(max (1 <<< 20) (longest * 2))

    // The buffer's place in the file, the next entry's place in the buffer, and how many bytes the buffer holds. A reader's own state.
    let mutable start = 0L
    let mutable position = 0
    let mutable filled = 0

    // Moves what is left to the front and reads on, until the buffer is full or the file ends.
    let refill () =
        let left = filled - position
        Buffer.BlockCopy(buffer, position, buffer, 0, left)
        start <- start + int64 position
        position <- 0
        filled <- left

        let rec more () =
            if filled < buffer.Length && start + int64 filled < size then
                match RandomAccess.Read(handle, Span(buffer, filled, buffer.Length - filled), start + int64 filled) with
                | 0 -> raise (EndOfStreamException())
                | read ->
                    filled <- filled + read
                    more ()

        more ()

    let need (count: int) =
        if count < 0 || position + count > filled then
            raise (EndOfStreamException())

    let read7Bit () =
        let rec go shift value =
            if shift > 28 then
                raise (InvalidDataException "Not a names file this version writes")

            need 1
            let next = buffer[position]
            position <- position + 1
            let value = value ||| (int (next &&& 0x7Fuy) <<< shift)

            if next &&& 0x80uy = 0uy then
                value
            else
                go (shift + 7) value

        match go 0 0 with
        | header when header < 0 -> raise (InvalidDataException "Not a names file this version writes")
        | header -> header

    /// The byte count a BulkCodec.text header gives: 0 for null, n for UTF-8, 2n for UTF-16.
    let countOf (header: int) =
        match header with
        | 0 -> 0
        | odd when odd % 2 = 1 -> odd / 2
        | even -> even - 2

    /// The text of a header and the bytes after it, as BulkCodec.text wrote it.
    let textOf (header: int) =
        let count = countOf header
        need count
        let at = position
        position <- position + count

        match header with
        | 0 -> null
        | odd when odd % 2 = 1 -> Encoding.UTF8.GetString(buffer, at, count)
        | _ -> String(Runtime.InteropServices.MemoryMarshal.Cast<byte, char>(ReadOnlySpan(buffer, at, count)))

    /// Whether the next name passes test: Holds as bytes when both are UTF-8, as text otherwise.
    let namePasses (test: BulkNameTest) =
        let header = read7Bit ()

        match test with
        | BulkNameTest.Holds(_, ValueSome bytes) when header % 2 = 1 ->
            let count = countOf header
            need count
            let at = position
            position <- position + count
            ReadOnlySpan(buffer, at, count).IndexOf(ReadOnlySpan bytes) >= 0
        | BulkNameTest.Holds(term, _) ->
            match textOf header with
            | null -> false
            | name -> name.Contains(term, StringComparison.Ordinal)
        | BulkNameTest.Text test ->
            match textOf header with
            | null -> false
            | name -> test name

    /// The next entry: whether its name passes test, its number and its record offset.
    let next (test: BulkNameTest) =
        if filled - position < longest && start + int64 filled < size then
            refill ()

        let found = namePasses test
        let number = textOf (read7Bit ())
        need 8

        let offset =
            BinaryPrimitives.ReadInt64LittleEndian(ReadOnlySpan(buffer, position, 8))

        position <- position + 8
        struct (found, number, offset)

    /// The number and record offset of each entry whose name passes test.
    member _.Matches(test: BulkNameTest, ct: CancellationToken) : struct (string * int64) seq =
        seq {
            while start + int64 position < size do
                ct.ThrowIfCancellationRequested()

                match next test with
                | struct (true, number, offset) -> yield struct (number, offset)
                | _ -> ()
        }

/// An index opened from disk: keys.bin binary searched and records.bin read at an offset, both by positional reads, so lookups in parallel
/// are safe. A search reads names.bin from start to end. It answers as the register it was written from.
[<Sealed>]
type internal IndexedRegistry
    (
        manifest: BulkManifest,
        normal: string -> string voption,
        cantTellAbsent: string -> string voption,
        records: SafeFileHandle,
        keys: SafeFileHandle,
        names: SafeFileHandle
    ) =

    static let searchLimit = 50

    /// Fills buffer from offset in the file, raising EndOfStreamException when the file ends first.
    static let readAt (handle: SafeFileHandle) (buffer: byte array) (offset: int64) =
        let rec fill start =
            if start < buffer.Length then
                match RandomAccess.Read(handle, Span(buffer, start, buffer.Length - start), offset + int64 start) with
                | 0 -> raise (EndOfStreamException())
                | read -> fill (start + read)

        fill 0

    /// True for what a damaged or cut short index raises.
    static let damaged (error: exn) =
        match error with
        | :? IOException
        | :? InvalidDataException
        | :? ArgumentException
        | :? IndexOutOfRangeException
        | :? OverflowException -> true
        | _ -> false

    static let unreadable =
        RegistryFailure.UnreadableAnswer "The index on disk is damaged or cut short: save it again"

    let coverage = manifest.Coverage
    let date = manifest.Date
    let width = 2 + manifest.KeyWidth + BulkKeyEntry.Tail

    let evidence raw value : Evidence<_> =
        {
            Source = coverage.Registry
            RetrievedAt = date.DownloadedAt
            AsOf = date.AsOf
            Raw = raw
            Value = value
        }

    let entryOf (buffer: byte array) =
        let tail = 2 + manifest.KeyWidth

        {
            Record = BinaryPrimitives.ReadInt64LittleEndian(ReadOnlySpan(buffer, tail, 8))
            Check = BinaryPrimitives.ReadUInt32LittleEndian(ReadOnlySpan(buffer, tail + 8, 4))
            Find = BinaryPrimitives.ReadInt32LittleEndian(ReadOnlySpan(buffer, tail + 12, 4))
            Officers = BinaryPrimitives.ReadInt32LittleEndian(ReadOnlySpan(buffer, tail + 16, 4))
            Owners = BinaryPrimitives.ReadInt32LittleEndian(ReadOnlySpan(buffer, tail + 20, 4))
        }

    let find (key: string) =
        let target = Encoding.UTF8.GetBytes key

        if target.Length > manifest.KeyWidth then
            BulkKeyEntry.Absent
        else
            let buffer = Array.zeroCreate<byte> width

            // Entries low to high - 1 can hold the key.
            let rec search low high =
                if low >= high then
                    BulkKeyEntry.Absent
                else
                    let middle = low + (high - low) / 2L
                    readAt keys buffer (middle * int64 width)

                    let length =
                        int (BinaryPrimitives.ReadUInt16LittleEndian(ReadOnlySpan(buffer, 0, 2)))

                    match ReadOnlySpan(target).SequenceCompareTo(ReadOnlySpan(buffer, 2, length)) with
                    | 0 -> entryOf buffer
                    | order when order < 0 -> search low middle
                    | _ -> search (middle + 1L) high

            search 0L manifest.Keys

    let recordAt (offset: int64) =
        let header = Array.zeroCreate<byte> BulkCodec.FrameHeader
        readAt records header offset
        let length = BulkCodec.frameLength (ReadOnlySpan(header, 0, 4))

        if offset + int64 BulkCodec.FrameHeader + int64 length > manifest.RecordsBytes then
            raise (EndOfStreamException())

        let body = Array.zeroCreate<byte> length
        readAt records body (offset + int64 BulkCodec.FrameHeader)
        BulkCodec.unframe header[4] body

    /// The record a key entry points at, raising InvalidDataException when its number isn't the one the entry was written for.
    let recordOf (entry: BulkKeyEntry) =
        let record = recordAt entry.Record

        if BulkKeyEntry.CheckOf record.Company.Number <> entry.Check then
            raise (InvalidDataException "A key's offset points at another record")

        record

    /// The record a names entry points at, raising InvalidDataException when its number isn't the entry's.
    let namedRecord (number: string) (offset: int64) =
        let record = recordAt offset

        if not (String.Equals(record.Company.Number, number, StringComparison.Ordinal)) then
            raise (InvalidDataException "A name's offset points at another record")

        record

    let reason index = manifest.Reasons[index]

    let answer (number: string) jurisdiction (read: BulkRecord voption -> BulkKeyEntry -> Evidence<Availability<'T>>) =
        SourceAnswer.inCoverage coverage jurisdiction (fun () ->
            match normal number with
            | ValueNone -> Task.FromResult(Error(RegistryFailure.InvalidRequest manifest.InvalidNumber))
            | ValueSome key ->
                Task.FromResult(
                    try
                        match find key with
                        | { Find = index } when index >= 0 -> Ok(evidence ValueNone (NotAvailable(reason index)))
                        | { Record = offset } as entry when offset >= 0L -> Ok(read (ValueSome(recordOf entry)) entry)
                        | entry ->
                            match cantTellAbsent key with
                            | ValueSome reason -> Ok(evidence ValueNone (NotAvailable reason))
                            | ValueNone -> Ok(read ValueNone entry)
                    with error when damaged error ->
                        Error unreadable
                ))

    let absent () =
        match manifest.Absence with
        | BulkAbsence.NotHeld -> Available ValueNone
        | BulkAbsence.CantTell reason -> NotAvailable reason

    /// found, unless a reason withholds an Available answer.
    let withheld (index: int) (found: Evidence<Availability<'T>>) =
        match found.Value with
        | Available _ when index >= 0 ->
            { found with
                Raw = ValueNone
                Value = NotAvailable(reason index)
            }
        | _ -> found

    let ownersFor purpose =
        match purpose with
        | OwnerPurpose.CustomerDueDiligence -> manifest.OwnersForDueDiligence
        | OwnerPurpose.PublicRead -> manifest.OwnersForPublicRead

    /// The test a scan runs: Contains on bytes where it can, as the built register's BulkNames.matcher would answer; Exact and StartsWith on
    /// each name's text.
    let testFor (matching: NameMatch) (name: string) =
        match matching with
        | NameMatch.Contains ->
            let term = name.Trim().ToUpperInvariant()

            let utf8 =
                let buffer = Array.zeroCreate<byte>(term.Length * 3)
                // Out parameters of the encoder.
                let mutable read = 0
                let mutable written = 0

                match Utf8.FromUtf16(term.AsSpan(), Span buffer, &read, &written, false, true) with
                | Buffers.OperationStatus.Done -> ValueSome(Array.sub buffer 0 written)
                | _ -> ValueNone

            BulkNameTest.Holds(term, utf8)
        | NameMatch.Exact
        | NameMatch.StartsWith -> BulkNameTest.Text(BulkNames.matcher matching name)

    let search (name: string) (matching: NameMatch) (ct: CancellationToken) =
        let found =
            BulkNameScan(names, manifest.NamesBytes, manifest.LongestName).Matches(testFor matching name, ct)
            // A record held under two keys, such as a number and its short form, is one match.
            |> Seq.distinctBy (fun struct (number, _) -> number)
            |> Seq.truncate (searchLimit + 1)
            |> List.ofSeq

        let companies =
            found
            |> List.truncate searchLimit
            |> List.map (fun struct (number, offset) -> (namedRecord number offset).Company)

        let result = OpenData.matches searchLimit ValueNone companies

        // A file that leaves companies out is never searched completely.
        let leavesOut =
            match manifest.Absence with
            | BulkAbsence.NotHeld -> false
            | BulkAbsence.CantTell _ -> true

        evidence
            ValueNone
            (Available
                { result with
                    Complete = found.Length <= searchLimit && not manifest.Excluded && not leavesOut
                })

    let searchAsync (name: string) (matching: NameMatch) jurisdiction (ct: CancellationToken) =
        SourceAnswer.inCoverage coverage jurisdiction (fun () ->
            NameSearch.withName name matching (fun () ->
                Task.FromResult(
                    try
                        Ok(search name matching ct)
                    with error when damaged error ->
                        Error unreadable
                )))

    // Names matching as the built register's search does, from the same scan of names.bin.
    interface INameMatchSearch with
        member _.SearchByNameAsync
            (name: string, matching: NameMatch, jurisdiction: Jurisdiction, ct: CancellationToken)
            =
            searchAsync name matching jurisdiction ct

    interface ICompanyRegistry with
        member _.Coverage = coverage

        member _.TryFindAsync(number: string, jurisdiction: Jurisdiction, _: CancellationToken) =
            answer number jurisdiction (fun record _ ->
                match record with
                | ValueSome record -> evidence (ValueSome record.Raw) (Available(ValueSome record.Company))
                | ValueNone -> evidence ValueNone (absent ()))

        /// Names containing the term, up to 50, read from names.bin start to end: a scan of the whole file per search.
        member _.SearchAsync(name: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            searchAsync name NameMatch.Contains jurisdiction ct

        member _.OfficersAsync(number: string, jurisdiction: Jurisdiction, _: CancellationToken) =
            answer number jurisdiction (fun record entry ->
                match record with
                | ValueSome {
                                Officers = ValueSome officers
                                Raw = raw
                            } -> withheld entry.Officers (evidence (ValueSome raw) (Available officers))
                | ValueSome _ -> evidence ValueNone (NotAvailable manifest.OfficersNotOffered)
                | ValueNone ->
                    evidence
                        ValueNone
                        (match absent () with
                         | Available _ -> NotAvailable "The file holds no company with this number"
                         | NotAvailable reason -> NotAvailable reason
                         | NeedsEntitlement entitlement -> NeedsEntitlement entitlement))

        member _.OwnersAsync(number: string, jurisdiction: Jurisdiction, purpose: OwnerPurpose, _: CancellationToken) =
            answer number jurisdiction (fun record entry ->
                match record with
                | ValueSome {
                                Owners = ValueSome listing
                                Raw = raw
                            } -> withheld entry.Owners (evidence (ValueSome raw) (Available listing))
                | ValueSome _ -> withheld entry.Owners (evidence ValueNone (ownersFor purpose))
                | ValueNone -> evidence ValueNone (NotAvailable "The file holds no company with this number"))

    interface IBulkRegistry

    interface IDisposable with
        member _.Dispose() =
            records.Dispose()
            keys.Dispose()
            names.Dispose()

/// Writing a built bulk register to a folder of files and opening it again.
///
/// - records.bin: each record once, as a BulkCodec frame: its length as an int32, an encoding byte (0 plain, 1 deflated), then its bytes.
/// - keys.bin: one fixed-width entry per key, sorted by the key's UTF-8 bytes: the key's length as a uint16, the key padded with zeros to the
///   manifest's KeyWidth, the record's offset as an int64 (-1 for none), BulkKeyEntry.CheckOf the record's number as a uint32 (0 for none),
///   then three int32 reason indexes (-1 for none): withheld or repeated, officers withheld and owners withheld.
/// - names.bin: per key in the order a search reads them, the upper-cased name and the record's number as BulkCodec.text, then the record's
///   offset as an int64.
/// - manifest.json: the format, the library version that wrote it, the source id, the register's coverage and owners answers as base64 of
///   BulkCodec bytes, its other settings, the reasons, the counts, the size of each file and the files the index was built from. It is
///   written last, through a temporary name, so a folder without it holds no index.
module internal BulkStore =

    open System.Reflection

    /// The library version an index records: the assembly's informational version, else its assembly version.
    let Library =
        let assembly = typeof<BulkManifest>.Assembly

        match assembly.GetCustomAttribute<AssemblyInformationalVersionAttribute>() with
        | null -> string (assembly.GetName().Version)
        | attribute -> attribute.InformationalVersion

    [<Literal>]
    let RecordsFile = "records.bin"

    [<Literal>]
    let KeysFile = "keys.bin"

    [<Literal>]
    let NamesFile = "names.bin"

    [<Literal>]
    let ManifestFile = "manifest.json"

    /// The keys the source's normaliser is checked against the register's on.
    [<Literal>]
    let private CheckedKeys = 1000

    let private compareBytes (left: byte array) (right: byte array) =
        ReadOnlySpan(left).SequenceCompareTo(ReadOnlySpan right)

    /// Writes each record once, returning each key's record offset and number, and how many records were written.
    let private writeRecords (path: string) (contents: BulkContents) (ct: CancellationToken) =
        let held =
            Dictionary<string, struct (int64 * string)>(contents.Records.Count, StringComparer.Ordinal)
        // Records held compactly are built afresh for each key, so only others can be the same record under two keys.
        let shared = not (contents.Records :? INamedRecords)
        let written = Dictionary<BulkRecord, int64>(HashIdentity.Reference)

        use stream =
            new FileStream(path, FileMode.Create, FileAccess.Write, FileShare.None, 1 <<< 20)

        let count =
            contents.Records
            |> Seq.fold
                (fun count pair ->
                    ct.ThrowIfCancellationRequested()
                    let record = pair.Value

                    match (if shared then written.TryGetValue record else (false, 0L)) with
                    | true, offset ->
                        held[pair.Key] <- struct (offset, record.Company.Number)
                        count
                    | _ ->
                        let offset = stream.Position
                        let framed = BulkCodec.frame record
                        stream.Write(framed, 0, framed.Length)

                        if shared then
                            written[record] <- offset

                        held[pair.Key] <- struct (offset, record.Company.Number)
                        count + 1L)
                0L

        held :> IReadOnlyDictionary<_, _>, count

    /// Writes each key's upper-cased name, number and record offset in the order a search reads them, returning the count and the longest entry.
    let private writeNames
        (path: string)
        (contents: BulkContents)
        (held: IReadOnlyDictionary<string, struct (int64 * string)>)
        =
        use stream =
            new FileStream(path, FileMode.Create, FileAccess.Write, FileShare.None, 1 <<< 20)

        use writer = new BinaryWriter(stream, Encoding.UTF8)

        contents.Names
        |> Seq.fold
            (fun (struct (count, longest)) (struct (key, name)) ->
                let struct (offset, number) = held[key]
                let start = stream.Position
                BulkCodec.text writer (name.ToUpperInvariant())
                BulkCodec.text writer number
                writer.Write offset
                struct (count + 1L, max longest (int (stream.Position - start))))
            (struct (0L, 0))

    /// Writes every key the register answers for differently from an absent one, sorted, returning the reasons, the key width and the count.
    /// Unavailable, writing nothing, when a key is longer than an index holds.
    let private writeKeys
        (path: string)
        (contents: BulkContents)
        (held: IReadOnlyDictionary<string, struct (int64 * string)>)
        =
        let reasons = Dictionary<string, int>(StringComparer.Ordinal)

        let reasonOf (reason: string voption) =
            match reason with
            | ValueNone -> -1
            | ValueSome text ->
                match reasons.TryGetValue text with
                | true, index -> index
                | _ ->
                    let index = reasons.Count
                    reasons[text] <- index
                    index

        let findReason key =
            match contents.Withheld.TryGetValue key with
            | true, reason -> ValueSome reason
            | _ when contents.Repeated.Contains key -> ValueSome BulkReasons.SeveralRecords
            | _ -> ValueNone

        let entryOf key =
            match held.TryGetValue key with
            | true, struct (offset, number) ->
                {
                    Record = offset
                    Check = BulkKeyEntry.CheckOf number
                    Find = reasonOf (findReason key)
                    Officers = reasonOf (contents.OfficersWithheld key)
                    Owners = reasonOf (contents.OwnersWithheld key)
                }
            | _ ->
                { BulkKeyEntry.Absent with
                    Find = reasonOf (findReason key)
                }

        let entries = Dictionary<string, BulkKeyEntry>(StringComparer.Ordinal)

        Seq.concat [ held.Keys; contents.Repeated :> string seq; contents.Withheld.Keys ]
        |> Seq.iter (fun key ->
            if not (entries.ContainsKey key) then
                entries[key] <- entryOf key)

        // An alias answers as the key the register gives for it.
        contents.Aliases
        |> Seq.toArray
        |> Array.iter (fun struct (alias, key) ->
            entries[alias] <-
                match entries.TryGetValue key with
                | true, entry -> entry
                | _ -> BulkKeyEntry.Absent)

        let sorted =
            entries
            |> Seq.map (fun pair -> struct (Encoding.UTF8.GetBytes pair.Key, pair.Value))
            |> Array.ofSeq

        Array.sortInPlaceWith (fun (struct (left, _)) (struct (right, _)) -> compareBytes left right) sorted

        let keyWidth =
            sorted |> Array.fold (fun widest (struct (key, _)) -> max widest key.Length) 0

        if keyWidth > int UInt16.MaxValue then
            Error(
                RegistryFailure.Unavailable
                    $"The index can't hold a key of {keyWidth} bytes, longer than the {UInt16.MaxValue} it holds"
            )
        else
            let width = 2 + keyWidth + BulkKeyEntry.Tail
            let buffer = Array.zeroCreate<byte> width

            use stream =
                new FileStream(path, FileMode.Create, FileAccess.Write, FileShare.None, 1 <<< 20)

            sorted
            |> Array.iter (fun (struct (key, entry)) ->
                Array.Clear buffer
                BinaryPrimitives.WriteUInt16LittleEndian(Span(buffer, 0, 2), uint16 key.Length)
                Buffer.BlockCopy(key, 0, buffer, 2, key.Length)
                let tail = 2 + keyWidth
                BinaryPrimitives.WriteInt64LittleEndian(Span(buffer, tail, 8), entry.Record)
                BinaryPrimitives.WriteUInt32LittleEndian(Span(buffer, tail + 8, 4), entry.Check)
                BinaryPrimitives.WriteInt32LittleEndian(Span(buffer, tail + 12, 4), entry.Find)
                BinaryPrimitives.WriteInt32LittleEndian(Span(buffer, tail + 16, 4), entry.Officers)
                BinaryPrimitives.WriteInt32LittleEndian(Span(buffer, tail + 20, 4), entry.Owners)
                stream.Write(buffer, 0, width))

            let table = Array.zeroCreate<string> reasons.Count
            reasons |> Seq.iter (fun pair -> table[pair.Value] <- pair.Key)
            Ok(table, keyWidth, int64 sorted.Length)

    let private writeManifest (path: string) (manifest: BulkManifest) =
        let base64 write value =
            Convert.ToBase64String(BulkCodec.encode write value)

        use stream = new FileStream(path, FileMode.Create, FileAccess.Write, FileShare.None)
        use json = new Utf8JsonWriter(stream, JsonWriterOptions(Indented = true))
        json.WriteStartObject()
        json.WriteNumber("format", manifest.Format)

        match manifest.Library with
        | ValueSome library -> json.WriteString("library", library)
        | ValueNone -> json.WriteNull "library"

        json.WriteString("source", manifest.Source)
        json.WriteString("registry", manifest.Coverage.Registry)
        json.WriteString("downloadedAt", manifest.Date.DownloadedAt.ToString("o", CultureInfo.InvariantCulture))

        match manifest.Date.AsOf with
        | ValueSome day -> json.WriteString("asOf", day.ToString("yyyy-MM-dd", CultureInfo.InvariantCulture))
        | ValueNone -> json.WriteNull "asOf"

        match manifest.Absence with
        | BulkAbsence.NotHeld -> json.WriteNull "absentCantTell"
        | BulkAbsence.CantTell reason -> json.WriteString("absentCantTell", reason)

        json.WriteString("officersNotOffered", manifest.OfficersNotOffered)
        json.WriteString("invalidNumber", manifest.InvalidNumber)
        json.WriteBoolean("excluded", manifest.Excluded)
        json.WriteString("coverage", base64 BulkCodec.coverage manifest.Coverage)
        json.WriteString("ownersForDueDiligence", base64 BulkCodec.owners manifest.OwnersForDueDiligence)
        json.WriteString("ownersForPublicRead", base64 BulkCodec.owners manifest.OwnersForPublicRead)
        json.WriteNumber("keyWidth", manifest.KeyWidth)
        json.WriteNumber("keys", manifest.Keys)
        json.WriteNumber("records", manifest.Records)
        json.WriteNumber("names", manifest.Names)
        json.WriteNumber("longestName", manifest.LongestName)
        json.WriteNumber("recordsBytes", manifest.RecordsBytes)
        json.WriteNumber("keysBytes", manifest.KeysBytes)
        json.WriteNumber("namesBytes", manifest.NamesBytes)
        json.WriteStartArray "reasons"
        manifest.Reasons |> Array.iter json.WriteStringValue
        json.WriteEndArray()
        json.WriteStartArray "inputs"

        manifest.Inputs
        |> List.iter (fun input ->
            json.WriteStartObject()
            json.WriteString("name", input.Name)
            json.WriteNumber("bytes", input.Bytes)
            json.WriteString("modified", input.Modified.ToString("o", CultureInfo.InvariantCulture))
            json.WriteEndObject())

        json.WriteEndArray()
        json.WriteEndObject()

    /// Writes an index to directory, replacing any there: the manifest goes first and comes back last. records writes records.bin to the path
    /// given and returns the contents to write the rest from, each key's record offset and number, and the count of records written. source is
    /// the BulkCatalogue id, and shape that source's normaliser, which must agree with the register's on its first keys. Raises IOException
    /// when the disk does.
    let private save
        (directory: string)
        (source: string)
        (shape: string -> string voption)
        (inputs: BulkIndexInput list)
        (records: string -> struct (BulkContents * IReadOnlyDictionary<string, struct (int64 * string)> * int64))
        : Result<unit, RegistryFailure> =
        Directory.CreateDirectory directory |> ignore
        let path name = Path.Combine(directory, name)
        // Each file is written under a temporary name, then the old one is deleted and the new one renamed. An open index shares delete, so it
        // keeps reading the old file.
        let staged name = path (name + ".tmp")
        File.Delete(path ManifestFile)
        let struct (contents, held, recordCount) = records (staged RecordsFile)
        let agrees key = shape key = contents.Normal key

        let checkedKeys =
            Seq.append held.Keys (contents.Repeated :> string seq)
            |> Seq.truncate CheckedKeys

        if not (Seq.forall agrees checkedKeys) then
            File.Delete(staged RecordsFile)
            Error(RegistryFailure.InvalidRequest $"The registry wasn't built by the bulk source {source}")
        else
            let struct (nameCount, longest) = writeNames (staged NamesFile) contents held

            match writeKeys (staged KeysFile) contents held with
            | Error failure ->
                File.Delete(staged RecordsFile)
                File.Delete(staged NamesFile)
                Error failure
            | Ok(reasons, keyWidth, keyCount) ->
                for name in [ RecordsFile; KeysFile; NamesFile ] do
                    File.Delete(path name)
                    File.Move(staged name, path name)

                let size name = FileInfo(path name).Length

                let manifest =
                    {
                        Format = BulkCodec.Format
                        Library = ValueSome Library
                        Source = source
                        Coverage = contents.Coverage
                        Date = contents.Date
                        Absence = contents.Absence
                        OfficersNotOffered = contents.OfficersNotOffered
                        InvalidNumber = contents.InvalidNumber
                        OwnersForDueDiligence = contents.Owners OwnerPurpose.CustomerDueDiligence
                        OwnersForPublicRead = contents.Owners OwnerPurpose.PublicRead
                        Reasons = reasons
                        Excluded = contents.Excluded
                        KeyWidth = keyWidth
                        Keys = keyCount
                        Records = recordCount
                        Names = nameCount
                        LongestName = longest
                        RecordsBytes = size RecordsFile
                        KeysBytes = size KeysFile
                        NamesBytes = size NamesFile
                        Inputs = inputs
                    }

                writeManifest (staged ManifestFile) manifest
                File.Move(staged ManifestFile, path ManifestFile, true)
                Ok()

    /// Writes a built register's contents to an index in directory, as save does.
    let write
        (directory: string)
        (source: string)
        (shape: string -> string voption)
        (inputs: BulkIndexInput list)
        (contents: BulkContents)
        (ct: CancellationToken)
        : Result<unit, RegistryFailure> =
        save directory source shape inputs (fun path ->
            let held, count = writeRecords path contents ct
            struct (contents, held, count))

    /// Writes an index from records as they stream, as save does, never holding them. template is the register built over no records; key
    /// gives a record's key as the register's build indexes it, where a key two records give holds neither and is repeated. Each key's record
    /// offset, number and name are indexed in the build's order, so a search reads names in the order the built register does.
    let writeStream
        (directory: string)
        (source: string)
        (shape: string -> string voption)
        (inputs: BulkIndexInput list)
        (template: BulkContents)
        (key: BulkRecord -> string voption)
        (records: BulkRecord seq)
        (ct: CancellationToken)
        : Result<unit, RegistryFailure> =
        save directory source shape inputs (fun path ->
            let index = BulkIndex<struct (int64 * string * string)>()

            let count =
                use stream =
                    new FileStream(path, FileMode.Create, FileAccess.Write, FileShare.None, 1 <<< 20)

                records
                |> Seq.fold
                    (fun count record ->
                        ct.ThrowIfCancellationRequested()

                        match key record with
                        | ValueSome held ->
                            let offset = stream.Position
                            let framed = BulkCodec.frame record
                            stream.Write(framed, 0, framed.Length)
                            index.Add(held, struct (offset, record.Company.Number, record.Company.Name))
                            count + 1L
                        | ValueNone -> count)
                    0L

            let held =
                Dictionary<string, struct (int64 * string)>(index.Held.Count, StringComparer.Ordinal)

            index.Held
            |> Seq.iter (fun pair ->
                let struct (offset, number, _) = pair.Value
                held[pair.Key] <- struct (offset, number))

            let contents =
                { template with
                    Repeated = index.Repeated
                    Excluded = template.Excluded || index.Repeated.Count > 0
                    Names =
                        index.Held
                        |> Seq.map (fun pair ->
                            let struct (_, _, name) = pair.Value
                            struct (pair.Key, name))
                }

            struct (contents, held :> IReadOnlyDictionary<_, _>, count))

    let private parseManifest (root: JsonElement) : BulkManifest =
        let text name = Json.required name Json.tryString root

        let whole name (e: JsonElement) =
            Json.tryText name e
            |> ValueOption.bind (fun value ->
                match Int64.TryParse(value, NumberStyles.Integer, CultureInfo.InvariantCulture) with
                | true, number -> ValueSome number
                | _ -> ValueNone)

        let number name = Json.required name whole root

        let moment name (e: JsonElement) =
            match
                DateTimeOffset.TryParseExact(
                    Json.required name Json.tryString e,
                    "o",
                    CultureInfo.InvariantCulture,
                    DateTimeStyles.None
                )
            with
            | true, at -> at
            | _ -> raise (Json.InvalidJson $"Field '{name}' is not a timestamp")

        let decoded read name =
            BulkCodec.decode read (Convert.FromBase64String(text name))

        {
            Format = int (number "format")
            Library = Json.tryString "library" root
            Source = text "source"
            Coverage = decoded BulkCodec.readCoverage "coverage"
            Date =
                {
                    DownloadedAt = moment "downloadedAt" root
                    AsOf = Json.tryDate "asOf" root
                }
            Absence =
                match Json.tryString "absentCantTell" root with
                | ValueSome reason -> BulkAbsence.CantTell reason
                | ValueNone -> BulkAbsence.NotHeld
            OfficersNotOffered = text "officersNotOffered"
            InvalidNumber = text "invalidNumber"
            OwnersForDueDiligence = decoded BulkCodec.readOwners "ownersForDueDiligence"
            OwnersForPublicRead = decoded BulkCodec.readOwners "ownersForPublicRead"
            Reasons =
                Json.array root "reasons"
                |> List.map (fun reason -> reason.GetString())
                |> Array.ofList
            Excluded = Json.required "excluded" Json.tryFlag root
            KeyWidth = int (number "keyWidth")
            Keys = number "keys"
            Records = number "records"
            Names = number "names"
            LongestName = int (number "longestName")
            RecordsBytes = number "recordsBytes"
            KeysBytes = number "keysBytes"
            NamesBytes = number "namesBytes"
            Inputs =
                Json.array root "inputs"
                |> List.map (fun input ->
                    {
                        Name = Json.required "name" Json.tryString input
                        Bytes = Json.required "bytes" whole input
                        Modified = moment "modified" input
                    })
        }

    /// The manifest of the index in directory. RequestRefused when there is none or it is of another format, UnreadableAnswer when it can't
    /// be read.
    let readManifest (directory: string) : Result<BulkManifest, RegistryFailure> =
        let path = Path.Combine(directory, ManifestFile)

        if not (File.Exists path) then
            Error(RegistryFailure.RequestRefused "No index has been saved in this folder")
        else
            try
                use document = JsonDocument.Parse(File.ReadAllBytes path)
                let root = document.RootElement

                match Json.tryInt "format" root with
                | ValueSome format when format = BulkCodec.Format -> Ok(parseManifest root)
                | ValueSome format ->
                    Error(
                        RegistryFailure.RequestRefused
                            $"The index is of format {format}, and this version reads format {BulkCodec.Format}: save it again"
                    )
                | ValueNone -> Error(RegistryFailure.UnreadableAnswer "The index's manifest names no format")
            with
            | :? JsonException
            | :? IOException
            | :? InvalidDataException
            | :? FormatException
            | :? ArgumentException
            | :? UnauthorizedAccessException
            | :? InvalidOperationException
            | Json.InvalidJson _ -> Error(RegistryFailure.UnreadableAnswer "The index's manifest can't be read")

    /// Opens the index in directory, whose manifest is given, answering with normal and cantTellAbsent as the register did. UnreadableAnswer
    /// when a file is missing or isn't the size the manifest gives.
    let openWith
        (directory: string)
        (normal: string -> string voption)
        (cantTellAbsent: string -> string voption)
        (manifest: BulkManifest)
        : Result<ICompanyRegistry, RegistryFailure> =
        let path name = Path.Combine(directory, name)

        let expected =
            [
                RecordsFile, manifest.RecordsBytes
                KeysFile, manifest.KeysBytes
                NamesFile, manifest.NamesBytes
            ]

        let width = 2L + int64 manifest.KeyWidth + int64 BulkKeyEntry.Tail

        match expected |> List.tryFind (fun (name, _) -> not (File.Exists(path name))) with
        | Some(name, _) -> Error(RegistryFailure.UnreadableAnswer $"The index has no {name}: save it again")
        | None ->
            match
                expected
                |> List.tryFind (fun (name, bytes) -> FileInfo(path name).Length <> bytes)
            with
            | Some(name, _) ->
                Error(
                    RegistryFailure.UnreadableAnswer
                        $"The index's {name} isn't the size its manifest gives: it is damaged or cut short"
                )
            | None when manifest.KeysBytes <> manifest.Keys * width ->
                Error(RegistryFailure.UnreadableAnswer "The index's keys.bin doesn't hold the keys its manifest counts")
            | None ->
                let opened = List<SafeFileHandle>()

                let handle name =
                    // Delete is shared, so a newer download or index can replace the files while this one is open.
                    let held =
                        File.OpenHandle(
                            path name,
                            FileMode.Open,
                            FileAccess.Read,
                            FileShare.Read ||| FileShare.Delete,
                            FileOptions.RandomAccess
                        )

                    opened.Add held
                    held

                try
                    let records = handle RecordsFile
                    let keys = handle KeysFile
                    let names = handle NamesFile
                    Ok(new IndexedRegistry(manifest, normal, cantTellAbsent, records, keys, names) :> ICompanyRegistry)
                with
                | :? IOException
                | :? UnauthorizedAccessException ->
                    for held in opened do
                        held.Dispose()

                    Error(RegistryFailure.UnreadableAnswer "The index's files can't be opened")
