namespace CompanyDueDiligence

open System
open System.Globalization
open System.IO.Compression
open System.Xml

/// The first worksheet of an XLSX workbook read as rows of text. The workbook is a zip of XML parts; each part is read with XmlReader as it
/// streams, and only the shared strings are held whole, since cells refer to them by position.
module internal Xlsx =

    let private settings =
        XmlReaderSettings(
            IgnoreComments = true,
            IgnoreProcessingInstructions = true,
            DtdProcessing = DtdProcessing.Prohibit,
            CloseInput = true
        )

    /// The last column a worksheet can have, XFD, counted from 0.
    [<Literal>]
    let LastColumn = 16383

    /// The position of a cell reference's column, counted from 0: 0 for A1, 27 for AB7. A reference past XFD is an XmlException.
    let columnOf (reference: string) =
        let letters = reference |> Seq.takeWhile Char.IsAsciiLetter |> Array.ofSeq

        if letters.Length > 3 then
            raise (XmlException "A cell reference names a column past XFD")

        let column =
            (letters
             |> Array.fold (fun n c -> n * 26 + int (Char.ToUpperInvariant c) - int 'A' + 1) 0)
            - 1

        if column > LastColumn then
            raise (XmlException "A cell reference names a column past XFD")

        column

    /// What a part's reader says of one node: an element's start with whether it is empty and, for a row or cell, its r and t attributes; an
    /// element's end; or text.
    [<Struct>]
    type private Node =
        {
            Kind: XmlNodeType
            Name: string
            IsEmpty: bool
            Text: string
            Reference: string
            CellType: string
        }

    /// The nodes of a part as the reader reaches them, read once.
    let private nodes (reader: XmlReader) =
        seq {
            while reader.Read() do
                let kind = reader.NodeType
                let name = reader.LocalName
                let attributed = kind = XmlNodeType.Element && (name = "c" || name = "row")

                yield
                    {
                        Kind = kind
                        Name = name
                        IsEmpty = kind = XmlNodeType.Element && reader.IsEmptyElement
                        Text =
                            match kind with
                            | XmlNodeType.Text
                            | XmlNodeType.CDATA
                            | XmlNodeType.Whitespace
                            | XmlNodeType.SignificantWhitespace -> reader.Value
                            | _ -> null
                        Reference = if attributed then reader.GetAttribute "r" else null
                        CellType = if attributed then reader.GetAttribute "t" else null
                    }
        }

    /// Where the shared strings part stands: the strings read, last first; the current string's runs, last first; and whether the reader is in
    /// a run's text or in a phonetic run (rPh), which is left out.
    [<Struct>]
    type private Strings =
        {
            Done: string list
            Runs: string list
            InText: bool
            InPhonetic: bool
        }

    let private stringStep (state: Strings) (node: Node) =
        match node.Kind, node.Name with
        | XmlNodeType.Element, "si" when node.IsEmpty -> { state with Done = "" :: state.Done }
        | XmlNodeType.Element, "si" -> { state with Runs = [] }
        | XmlNodeType.Element, "rPh" when not node.IsEmpty -> { state with InPhonetic = true }
        | XmlNodeType.Element, "t" when not node.IsEmpty -> { state with InText = true }
        | XmlNodeType.EndElement, "si" ->
            { state with
                Done = String.Concat(List.rev state.Runs) :: state.Done
            }
        | XmlNodeType.EndElement, "rPh" -> { state with InPhonetic = false }
        | XmlNodeType.EndElement, "t" -> { state with InText = false }
        | _ when not (isNull node.Text) && state.InText && not state.InPhonetic ->
            { state with
                Runs = node.Text :: state.Runs
            }
        | _ -> state

    /// The text of each shared string, in order. A string's runs are joined, and phonetic runs (rPh) are left out.
    let sharedStrings (archive: ZipArchive) : string array =
        match archive.GetEntry "xl/sharedStrings.xml" with
        | null -> [||]
        | entry ->
            use reader = XmlReader.Create(BulkFile.openEntry entry, settings)

            let read =
                nodes reader
                |> Seq.fold
                    stringStep
                    {
                        Done = []
                        Runs = []
                        InText = false
                        InPhonetic = false
                    }

            read.Done |> List.rev |> Array.ofList

    /// Where a worksheet stands: in a row or not, the row's number and its cells read, last first, with their count; the current cell's column,
    /// type and value runs; whether the reader is in a value (v, or t of an inline string) or a phonetic run; and a row just finished.
    [<Struct>]
    type private Sheet =
        {
            InRow: bool
            Number: int
            Cells: struct (int * string) list
            Count: int
            Column: int
            CellType: string
            Value: string list
            InValue: bool
            InPhonetic: bool
            Finished: struct (int * string array) voption
        }

    let private noSheet =
        {
            InRow = false
            Number = 0
            Cells = []
            Count = 0
            Column = 0
            CellType = "n"
            Value = []
            InValue = false
            InPhonetic = false
            Finished = ValueNone
        }

    /// A row's cells by column, "" where a cell is missing.
    let private rowOf (number: int) (cells: struct (int * string) list) =
        let width =
            match cells with
            | [] -> 0
            | cells -> (cells |> List.map (fun struct (c, _) -> c) |> List.max) + 1

        let row = Array.create (max 0 width) ""
        // Cells are held last first; written in the sheet's order, the later of two in one column is kept.
        cells
        |> List.rev
        |> List.iter (fun struct (c, text) ->
            if c >= 0 then
                row[c] <- text)

        struct (number, row)

    let private cellText (strings: string array) (cellType: string) (text: string) =
        if cellType = "s" then
            match Int32.TryParse(text, NumberStyles.None, CultureInfo.InvariantCulture) with
            | true, i when i >= 0 && i < strings.Length -> strings[i]
            | _ -> raise (XmlException "A cell refers to a shared string the workbook doesn't hold")
        else
            text

    let private sheetStep (strings: string array) (state: Sheet) (node: Node) =
        let state = { state with Finished = ValueNone }

        match node.Kind, node.Name with
        | XmlNodeType.Element, "row" ->
            let number =
                match Int32.TryParse(node.Reference, NumberStyles.None, CultureInfo.InvariantCulture) with
                | true, n -> n
                | _ -> 0

            if node.IsEmpty then
                { state with
                    Finished = ValueSome(rowOf number [])
                }
            else
                { noSheet with
                    InRow = true
                    Number = number
                    CellType = ""
                }
        | _ when not state.InRow -> state
        | XmlNodeType.Element, "c" ->
            let column =
                match node.Reference with
                | null -> state.Count
                | reference -> columnOf reference

            let cellType =
                match node.CellType with
                | null -> "n"
                | t -> t

            if node.IsEmpty then
                { state with
                    Column = column
                    CellType = cellType
                    Value = []
                    Cells = struct (column, "") :: state.Cells
                    Count = state.Count + 1
                }
            else
                { state with
                    Column = column
                    CellType = cellType
                    Value = []
                }
        | XmlNodeType.Element, ("v" | "t") when not node.IsEmpty -> { state with InValue = true }
        | XmlNodeType.Element, "rPh" when not node.IsEmpty -> { state with InPhonetic = true }
        | XmlNodeType.EndElement, ("v" | "t") -> { state with InValue = false }
        | XmlNodeType.EndElement, "rPh" -> { state with InPhonetic = false }
        | XmlNodeType.EndElement, "c" ->
            let text = cellText strings state.CellType (String.Concat(List.rev state.Value))

            { state with
                Cells = struct (state.Column, text) :: state.Cells
                Count = state.Count + 1
            }
        | XmlNodeType.EndElement, "row" ->
            { state with
                InRow = false
                Finished = ValueSome(rowOf state.Number state.Cells)
            }
        | _ when not (isNull node.Text) && state.InValue && not state.InPhonetic ->
            { state with
                Value = node.Text :: state.Value
            }
        | _ -> state

    /// The rows of a worksheet part, such as "xl/worksheets/sheet1.xml", as they stream: each row's number and its cells by column, "" where a
    /// cell is missing. Raises InvalidDataException when the part is missing.
    let rows (archive: ZipArchive) (part: string) : struct (int * string array) seq =
        seq {
            let strings = sharedStrings archive

            let entry =
                match archive.GetEntry part with
                | null -> raise (IO.InvalidDataException "The workbook has no such worksheet")
                | entry -> entry

            use reader = XmlReader.Create(BulkFile.openEntry entry, settings)

            yield!
                nodes reader
                |> Seq.scan (sheetStep strings) noSheet
                |> Seq.choose (fun state -> ValueOption.toOption state.Finished)
        }

    /// The first group of the pattern's last match among the cells of rows, such as a date in a sheet's title rows; absent when none matches.
    let lastMatch (pattern: Text.RegularExpressions.Regex) (rows: struct (int * string array) seq) =
        rows
        |> Seq.collect (fun struct (_, row) -> row)
        |> Seq.map pattern.Match
        |> Seq.filter _.Success
        |> Seq.tryLast
        |> Option.map _.Groups[1].Value
        |> ValueOption.ofOption

    /// A cell's text, trimmed; empty or past the row's end is absent.
    let cell (row: string array) (column: int) =
        if column < row.Length then
            let text = row[column].Trim()
            if text = "" then ValueNone else ValueSome text
        else
            ValueNone

    /// A date stored as an Excel serial day number, as a cell without a type holds it.
    let serialDate (text: string voption) =
        text
        |> ValueOption.bind (fun text ->
            match Double.TryParse(text, NumberStyles.Float, CultureInfo.InvariantCulture) with
            | true, serial when serial >= 1. && serial < 2958466. ->
                ValueSome(DateOnly(1899, 12, 30).AddDays(int serial))
            | _ -> ValueNone)
