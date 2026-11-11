namespace CompanyDueDiligence

open System
open System.Collections.Generic
open System.Globalization
open System.IO
open System.IO.Compression
open System.Text
open System.Threading
open System.Threading.Tasks
open System.Xml
open System.Xml.Linq

/// A read-only view of a stream that fails with InvalidDataException once more than limit bytes come out of it. Disposing it disposes the stream.
[<Sealed>]
type internal BoundedStream(inner: Stream, limit: int64) =
    inherit Stream()

    let mutable read = 0L

    let counted (count: int) =
        read <- read + int64 count

        if read > limit then
            raise (InvalidDataException $"A compressed entry decompresses past {limit} bytes")

        count

    let allowed (count: int) =
        int (min (int64 count) (limit - read + 1L))

    override _.CanRead = true
    override _.CanSeek = false
    override _.CanWrite = false
    override _.Length = raise (NotSupportedException())

    override _.Position
        with get () = read
        and set _ = raise (NotSupportedException())

    override _.Flush() = ()
    override _.Seek(_, _) = raise (NotSupportedException())
    override _.SetLength _ = raise (NotSupportedException())
    override _.Write(_, _, _) = raise (NotSupportedException())

    override _.Read(buffer: byte array, offset: int, count: int) =
        counted (inner.Read(buffer, offset, allowed count))

    override _.Read(buffer: Span<byte>) =
        counted (inner.Read(buffer.Slice(0, allowed buffer.Length)))

    override _.ReadAsync(buffer: Memory<byte>, ct: CancellationToken) =
        let reading = inner.ReadAsync(buffer.Slice(0, allowed buffer.Length), ct)

        if reading.IsCompletedSuccessfully then
            ValueTask<int>(counted reading.Result)
        else
            ValueTask<int>(
                task {
                    let! count = reading
                    return counted count
                }
            )

    override _.ReadAsync(buffer: byte array, offset: int, count: int, ct: CancellationToken) =
        task {
            let! got = inner.ReadAsync(buffer, offset, allowed count, ct)
            return counted got
        }

    override _.Dispose(disposing: bool) =
        if disposing then
            inner.Dispose()

        base.Dispose disposing

/// A read-only view of a stream that keeps the last two bytes read from it, for telling how a text ended. Disposing it leaves the stream open.
[<Sealed>]
type internal EndingStream(inner: Stream) =
    inherit Stream()

    let mutable last = 0uy
    let mutable before = 0uy
    let mutable any = false

    let seen (buffer: ReadOnlySpan<byte>) =
        if buffer.Length >= 2 then
            before <- buffer[buffer.Length - 2]
            last <- buffer[buffer.Length - 1]
            any <- true
        elif buffer.Length = 1 then
            before <- last
            last <- buffer[0]
            any <- true

    /// True once bytes have been read and the last isn't a line end, in UTF-8, Latin-1 or UTF-16.
    member _.EndsWithoutLineEnd =
        let isLineEnd (value: byte) = value = 10uy || value = 13uy
        any && not (isLineEnd last || (last = 0uy && isLineEnd before))

    override _.CanRead = true
    override _.CanSeek = false
    override _.CanWrite = false
    override _.Length = raise (NotSupportedException())

    override _.Position
        with get () = raise (NotSupportedException())
        and set _ = raise (NotSupportedException())

    override _.Flush() = ()
    override _.Seek(_, _) = raise (NotSupportedException())
    override _.SetLength _ = raise (NotSupportedException())
    override _.Write(_, _, _) = raise (NotSupportedException())

    override _.Read(buffer: byte array, offset: int, count: int) =
        let read = inner.Read(buffer, offset, count)
        seen (ReadOnlySpan(buffer, offset, read))
        read

    override _.Read(buffer: Span<byte>) =
        let read = inner.Read buffer
        seen (Span.op_Implicit (buffer.Slice(0, read)))
        read

/// One line of a text, and whether the text ends at it without a line end.
[<Struct>]
type internal TextLine = { Text: string; Unended: bool }

/// One record of a delimited file: its own text, kept as evidence, and its fields.
[<Struct>]
type internal DelimitedRecord = { Raw: string; Fields: string array }

/// Reading the files bulk registers publish: zips as published or plain files, text read strictly, delimited records with their headers, XML
/// elements, and the failure for a file of the wrong shape.
module internal BulkFile =

    /// A file that can't be read. The message holds no text from the file.
    exception UnreadableFile of string

    /// Fails the read of a file that isn't what the source publishes; Bulk.run answers it as UnreadableAnswer with this reason, which holds no
    /// text from the file.
    let fail reason : 'T = raise (UnreadableFile reason)

    /// UTF-8 that fails on bytes that aren't UTF-8, so damaged text is never read as other text. It declares the UTF-8 byte order mark as its
    /// preamble, so a reader detecting byte order marks strips that mark and keeps reading strictly rather than switching to lenient UTF-8. Only
    /// read with it: a writer would write the mark.
    let strictUtf8: Encoding = UTF8Encoding(true, true)

    /// The most a zip entry may decompress to: 16 GiB, a choice made without data, above any published file read here.
    [<Literal>]
    let LongestEntry = 17179869184L

    /// A zip entry's content, failing with InvalidDataException past the size the zip states for it or past LongestEntry, so a zip bomb can't
    /// run on. The caller disposes it.
    let openEntry (entry: ZipArchiveEntry) : Stream =
        if entry.Length > LongestEntry then
            raise (InvalidDataException $"A zip entry states {entry.Length} bytes, past the {LongestEntry} read")

        new BoundedStream(entry.Open(), entry.Length)

    /// The stream itself when it can seek; otherwise a copy in a temporary file that is deleted when the copy is disposed, with true. The host's
    /// stream is read to its end and left open.
    let seekable (stream: Stream) : struct (Stream * bool) =
        if stream.CanSeek then
            struct (stream, false)
        else
            let copy =
                new FileStream(
                    Path.GetTempFileName(),
                    FileMode.Create,
                    FileAccess.ReadWrite,
                    FileShare.None,
                    81920,
                    FileOptions.DeleteOnClose
                )

            try
                stream.CopyTo copy
                copy.Position <- 0L
                struct (copy :> Stream, true)
            with _ ->
                copy.Dispose()
                reraise ()

    /// A zip the host passed, read in place and left open for the host to close. A stream that can't seek is copied to a temporary file first,
    /// which disposing the archive deletes.
    let openZip (stream: Stream) =
        let struct (source, copied) = seekable stream
        new ZipArchive(source, ZipArchiveMode.Read, not copied)

    /// The entry with this name; InvalidDataException when the zip doesn't hold it.
    let entry (archive: ZipArchive) (name: string) =
        match archive.GetEntry name with
        | null -> raise (InvalidDataException "The zip doesn't hold a file it should")
        | entry -> entry

    /// "PK\003\004", which opens a zip file. The stream can seek.
    let private isZip (stream: Stream) =
        let start = stream.Position
        let head = Array.zeroCreate<byte> 4
        let read = stream.ReadAtLeast(head, 4, false)
        stream.Position <- start

        read = 4
        && head[0] = 0x50uy
        && head[1] = 0x4Buy
        && head[2] = 3uy
        && head[3] = 4uy

    /// The items read from each file in the stream, in turn: every file in a zip, in the zip's order, or the stream itself, named "", when it
    /// isn't a zip. read gets the file's name and an opener, which can be called again to read the file from its start; the streams it opens
    /// stay open while its items are enumerated and are closed after the file's last. The host's stream is left open. A stream that can't seek
    /// is copied to a temporary file first, and an entry is read only up to the size the zip states for it.
    let each (stream: Stream) (read: string -> (unit -> Stream) -> 'T seq) : 'T seq =
        seq {
            let struct (source, copied) = seekable stream

            try
                if isZip source then
                    use archive = new ZipArchive(source, ZipArchiveMode.Read, true)

                    for entry in archive.Entries do
                        if not (entry.FullName.EndsWith '/') then
                            let opened = List<Stream>()

                            let reopen () =
                                let content = openEntry entry
                                opened.Add content
                                content

                            try
                                yield! read entry.Name reopen
                            finally
                                opened |> Seq.iter _.Dispose()
                else
                    let start = source.Position

                    yield!
                        read "" (fun () ->
                            source.Position <- start
                            source)
            finally
                if copied then
                    source.Dispose()
        }

    /// One result for each file in the stream, read as each reads: read finishes with the file before it returns.
    let perFile (stream: Stream) (read: string -> (unit -> Stream) -> 'T) : 'T list =
        each stream (fun name reopen -> [ read name reopen ]) |> List.ofSeq

    /// Runs read over the published file: the entry whose name ends with suffix when the stream is a zip, or the stream itself when it isn't
    /// one. A stream that can't seek is copied to a temporary file first. A zip without such an entry is unreadable, and an entry is read only up
    /// to the size the zip states for it.
    let publishedAsync (stream: Stream) (suffix: string) (read: Stream -> Task<'T>) : Task<'T> =
        task {
            let struct (source, copied) = seekable stream

            try
                if isZip source then
                    use archive = new ZipArchive(source, ZipArchiveMode.Read, true)

                    let entry =
                        archive.Entries
                        |> Seq.tryFind (fun entry ->
                            entry.FullName.EndsWith(suffix, StringComparison.OrdinalIgnoreCase))

                    match entry with
                    | Some entry ->
                        use opened = openEntry entry
                        return! read opened
                    | None -> return fail $"The zip holds no {suffix}"
                else
                    return! read source
            finally
                if copied then
                    source.Dispose()
        }

    /// The lines of a text stream in the encoding given, unless a byte order mark says another. The stream is left open.
    let lines (encoding: Encoding) (stream: Stream) : string seq =
        seq {
            use reader = new StreamReader(stream, encoding, true, 65536, true)
            yield! Seq.initInfinite (fun _ -> reader.ReadLine()) |> Seq.takeWhile (isNull >> not)
        }

    /// The lines of a zip entry, as lines reads them; the entry is closed after the last.
    let entryLines (encoding: Encoding) (entry: ZipArchiveEntry) : string seq =
        seq {
            use stream = openEntry entry
            yield! lines encoding stream
        }

    /// Where a delimited text stands after a character: a field opens quoted only with a double quote at its start, and inside one a double
    /// quote either closes it or, doubled, stands for one quote.
    [<Struct; RequireQualifiedAccess>]
    type private Quoting =
        | FieldStart
        | Unquoted
        | Quoted
        | QuoteInQuoted

    let private next (delimiter: char) (state: Quoting) (c: char) =
        match state with
        | Quoting.FieldStart when c = '"' -> Quoting.Quoted
        | Quoting.Quoted when c = '"' -> Quoting.QuoteInQuoted
        | Quoting.Quoted -> Quoting.Quoted
        | Quoting.QuoteInQuoted when c = '"' -> Quoting.Quoted
        | _ when c = delimiter -> Quoting.FieldStart
        | _ -> Quoting.Unquoted

    /// The fields of one record. With quoting, a field opening with a double quote runs to its closing quote, delimiters and line breaks
    /// included, with a doubled quote read as one, and a quote still open runs to the end; without it, quotes are text.
    let split (delimiter: char) (quoting: bool) (record: string) : string array =
        if not quoting || record.IndexOf '"' < 0 then
            record.Split delimiter
        else
            let fields = List<string>()
            let field = StringBuilder()

            record
            |> Seq.fold
                (fun state c ->
                    let after = next delimiter state c

                    match after with
                    | Quoting.FieldStart ->
                        fields.Add(field.ToString())
                        field.Clear() |> ignore
                    | Quoting.QuoteInQuoted -> ()
                    | Quoting.Quoted when state = Quoting.FieldStart -> ()
                    | _ -> field.Append c |> ignore

                    after)
                Quoting.FieldStart
            |> ignore

            fields.Add(field.ToString())
            fields.ToArray()

    /// Whether a quoted field is still open at the end of a line, given whether one was open at its start.
    let private openAfter (delimiter: char) (opened: bool) (line: string) =
        if line.IndexOf '"' < 0 then
            opened
        else
            let start = if opened then Quoting.Quoted else Quoting.FieldStart
            Seq.fold (next delimiter) start line = Quoting.Quoted

    /// The longest record read. A longer one is an unclosed quote running on, and the file is unreadable.
    [<Literal>]
    let LongestRecord = 1048576

    /// A record's lines so far, last first, and whether a quoted field is still open after them.
    [<Struct>]
    type private Pending =
        {
            Lines: string list
            Length: int
            Open: bool
        }

    /// The records of delimited lines, split as split splits them; with quoting, a quoted field holding a line break joins its lines with "\n"
    /// into one record. Blank lines between records are skipped. A record running past LongestRecord, or a quoted field still open at the end,
    /// fails the file.
    let records (delimiter: char) (quoting: bool) (lines: string seq) : DelimitedRecord seq =
        let step (pending: Pending) (line: string voption) =
            match line with
            | ValueNone when pending.Open -> fail "A quoted field runs to the end of the file"
            | ValueNone -> { Lines = []; Length = 0; Open = false }
            | ValueSome line ->
                let lines, length =
                    if pending.Open then
                        line :: pending.Lines, pending.Length + 1 + line.Length
                    else
                        [ line ], line.Length

                if length > LongestRecord then
                    fail "A record runs past 1 MB, as an unclosed quote makes it"

                {
                    Lines = lines
                    Length = length
                    Open = quoting && openAfter delimiter pending.Open line
                }

        Seq.append (lines |> Seq.map ValueSome) (Seq.singleton ValueNone)
        |> Seq.scan step { Lines = []; Length = 0; Open = false }
        |> Seq.choose (fun pending ->
            match pending.Lines with
            | _ when pending.Open -> None
            | [] -> None
            | [ line ] when String.IsNullOrWhiteSpace line -> None
            | [ line ] ->
                Some
                    {
                        Raw = line
                        Fields = split delimiter quoting line
                    }
            | lines ->
                let raw = String.Join("\n", List.rev lines)

                Some
                    {
                        Raw = raw
                        Fields = split delimiter quoting raw
                    })

    /// The lines of a text stream, as lines reads them, the last one saying whether the text ended there without a line end. The stream is
    /// left open.
    let linesToEnd (encoding: Encoding) (stream: Stream) : TextLine seq =
        seq {
            use ending = new EndingStream(stream)
            use reader = new StreamReader(ending, encoding, true, 65536, true)

            // Each line is given once the one after it has been asked for, so the last is known as it is given. A loop, as a bulk file is
            // read line by line through here.
            let mutable line = reader.ReadLine()

            while not (isNull line) do
                let next = reader.ReadLine()

                yield
                    {
                        Text = line
                        Unended = isNull next && ending.EndsWithoutLineEnd
                    }

                line <- next
        }

    /// True for a line of nothing but white space, NUL padding or the Ctrl-Z that ends an old DOS text: it is not a record.
    let private isPadding (line: string) =
        line
        |> String.forall (fun c -> Char.IsWhiteSpace c || c = '\u001a' || c = '\u0000')

    /// The lines' text, failing the file when it ends without a line end in a record with fewer fields than columns, or than its first
    /// record when columns is ValueNone. Such a file is cut short, and the fields its last record lacks would read as absent. A record with
    /// fewer fields anywhere else, or before a line end, is read as the file gives it, and so is padding after the last record.
    let whole (columns: int voption) (delimiter: char) (quoting: bool) (lines: TextLine seq) : string seq =
        seq {
            // The field count to expect, and the lines so far of a record whose quoted field is still open, last first.
            let mutable expected = columns
            let mutable opened: string list = []

            for line in lines do
                let stillOpen = quoting && openAfter delimiter (not opened.IsEmpty) line.Text

                // Fields are counted only where the count is used: in the first record, and in a last one without its line end.
                if
                    not stillOpen
                    && (expected.IsNone || line.Unended)
                    && not (opened.IsEmpty && isPadding line.Text)
                then
                    let fields =
                        (split delimiter quoting (String.Join("\n", List.rev (line.Text :: opened)))).Length

                    match expected with
                    | ValueNone -> expected <- ValueSome fields
                    | ValueSome needed when line.Unended && fields < needed ->
                        fail
                            "The file ends without a line end, in a record with fewer fields than the others: it is cut short"
                    | ValueSome _ -> ()

                opened <- if stillOpen then line.Text :: opened else []
                yield line.Text
        }

    /// The lines of a delimited text stream whose first record is its header or is as wide as the rest, failing a file cut short as whole
    /// does. The stream is left open.
    let wholeLines (delimiter: char) (quoting: bool) (encoding: Encoding) (stream: Stream) : string seq =
        whole ValueNone delimiter quoting (linesToEnd encoding stream)

    /// The lines of a delimited zip entry, as wholeLines reads them; the entry is closed after the last.
    let entryWholeLines (delimiter: char) (quoting: bool) (encoding: Encoding) (entry: ZipArchiveEntry) : string seq =
        seq {
            use stream = openEntry entry
            yield! wholeLines delimiter quoting encoding stream
        }

    /// The first item and the items after it, reading the sequence once. The rest is read on from where the first stopped.
    let private rest (enumerator: IEnumerator<'T>) =
        seq {
            try
                while enumerator.MoveNext() do
                    yield enumerator.Current
            finally
                enumerator.Dispose()
        }

    /// The items before the first that isHeader accepts, that item, and the items after it, reading the sequence once. Without such an item,
    /// every item is in the first list.
    let splitAt (isHeader: 'T -> bool) (items: 'T seq) : struct ('T list * 'T voption * 'T seq) =
        let enumerator = items.GetEnumerator()

        let rec before preamble =
            if not (enumerator.MoveNext()) then
                enumerator.Dispose()
                struct (List.rev preamble, ValueNone, Seq.empty)
            elif isHeader enumerator.Current then
                struct (List.rev preamble, ValueSome enumerator.Current, rest enumerator)
            else
                before (enumerator.Current :: preamble)

        before []

    /// A header as positions by name, trimmed, with a byte order mark removed. A name given twice is the first.
    let header (names: string array) : IReadOnlyDictionary<string, int> =
        names
        |> Array.mapi (fun position name -> name.Trim().TrimStart('﻿').Trim(), position)
        |> Array.distinctBy fst
        |> readOnlyDict

    /// The header and records of a delimited file. A file with no header record is unreadable.
    let table (file: string) (records: DelimitedRecord seq) =
        match splitAt (fun _ -> true) records with
        | struct (_, ValueSome first, rows) -> header first.Fields, rows
        | struct (_, ValueNone, _) -> fail $"{file} is empty"

    /// The header and records of a delimited file in a stream, read as strict UTF-8 unless a byte order mark says otherwise. The records stream
    /// as they are read, and the stream is left open. A file cut short fails as whole fails it.
    let delimited (file: string) (delimiter: char) (quoting: bool) (stream: Stream) =
        table file (records delimiter quoting (wholeLines delimiter quoting strictUtf8 stream))

    /// The value under a column, trimmed; empty, or a column the record lacks, is absent.
    let cell (columns: IReadOnlyDictionary<string, int>) (name: string) (record: DelimitedRecord) =
        match columns.TryGetValue name with
        | true, position when position < record.Fields.Length ->
            let value = record.Fields[position].Trim()
            if value = "" then ValueNone else ValueSome value
        | _ -> ValueNone

    /// Fails the read unless every column named is in the header.
    let requireColumns (file: string) (columns: IReadOnlyDictionary<string, int>) (names: string list) =
        match names |> List.filter (columns.ContainsKey >> not) with
        | [] -> ()
        | missing -> fail $"""{file} has no column {String.Join(", ", missing)}"""

    /// Each element of this name as the reader reaches it, read whole; reading one moves the reader past it.
    let elements (name: string) (reader: XmlReader) : XElement seq =
        seq {
            while not reader.EOF do
                if reader.NodeType = XmlNodeType.Element && reader.LocalName = name then
                    yield XNode.ReadFrom reader :?> XElement
                else
                    reader.Read() |> ignore
        }

    /// A value with its blanks trimmed; empty or null is absent.
    let nonBlank (text: string) =
        if String.IsNullOrWhiteSpace text then
            ValueNone
        else
            ValueSome(text.Trim())

    /// A date in this exact format, or absent.
    let date (format: string) (text: string voption) =
        text
        |> ValueOption.bind (fun text ->
            match DateOnly.TryParseExact(text.Trim(), format, CultureInfo.InvariantCulture, DateTimeStyles.None) with
            | true, value -> ValueSome value
            | _ -> ValueNone)

    /// A date in this exact format, or absent; a year before 1800 is a source's placeholder and absent too.
    let dateFrom1800 (format: string) (text: string) =
        date format (nonBlank text)
        |> ValueOption.filter (fun value -> value.Year >= 1800)
