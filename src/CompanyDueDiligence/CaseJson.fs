namespace CompanyDueDiligence

open System
open System.Globalization
open System.IO
open System.Text
open System.Text.Json
open Microsoft.FSharp.Reflection

/// Why stored JSON couldn't be read as a case file. It holds no text from the file except field and case names.
[<RequireQualifiedAccess>]
type CaseJsonRefused =
    | NotJson of reason: string
    | WrongVersion of found: int voption
    | WrongShape of reason: string

/// A case file as JSON, for the host to store. Records are objects, a ValueNone is left out, and a union is {"case":"Name","fields":[...]}, or its
/// name alone when it holds nothing. Reading is strict: a missing field, an unknown one, or an unknown case refuses the whole file, because a host re-saving
/// what it only partly read would lose the rest.
module CaseJson =

    exception private Shape of string

    let private isVOption (kind: Type) =
        kind.IsGenericType && kind.GetGenericTypeDefinition() = typedefof<voption<_>>

    let private isList (kind: Type) =
        kind.IsGenericType && kind.GetGenericTypeDefinition() = typedefof<list<_>>

    let rec private write (writer: Utf8JsonWriter) (kind: Type) (value: obj) : unit =
        match value with
        | :? string as text -> writer.WriteStringValue text
        | :? int as number -> writer.WriteNumberValue number
        | :? decimal as number -> writer.WriteNumberValue number
        | :? float as number -> writer.WriteNumberValue number
        | :? bool as flag -> writer.WriteBooleanValue flag
        | :? DateOnly as date -> writer.WriteStringValue(date.ToString("yyyy-MM-dd", CultureInfo.InvariantCulture))
        | :? DateTimeOffset as at -> writer.WriteStringValue at
        | :? Jurisdiction as jurisdiction -> writer.WriteStringValue jurisdiction.Code
        | _ when isList kind ->
            let item = kind.GetGenericArguments()[0]
            writer.WriteStartArray()

            for element in (value :?> System.Collections.IEnumerable) do
                write writer item element

            writer.WriteEndArray()
        | _ when isVOption kind ->
            let case, fields = FSharpValue.GetUnionFields(value, kind)

            if case.Name = "ValueSome" then
                write writer (kind.GetGenericArguments()[0]) fields[0]
            else
                writer.WriteNullValue()
        | _ when FSharpType.IsTuple kind ->
            let items = FSharpValue.GetTupleFields value
            let types = FSharpType.GetTupleElements kind
            writer.WriteStartArray()

            for index in 0 .. items.Length - 1 do
                write writer types[index] items[index]

            writer.WriteEndArray()
        | _ when FSharpType.IsRecord kind ->
            writer.WriteStartObject()

            for field in FSharpType.GetRecordFields kind do
                let held = field.GetValue value

                let absent =
                    isVOption field.PropertyType
                    && (fst (FSharpValue.GetUnionFields(held, field.PropertyType))).Name = "ValueNone"

                if not absent then
                    writer.WritePropertyName field.Name
                    write writer field.PropertyType held

            writer.WriteEndObject()
        | _ when FSharpType.IsUnion kind ->
            let case, fields = FSharpValue.GetUnionFields(value, kind)

            if fields.Length = 0 then
                writer.WriteStringValue case.Name
            else
                writer.WriteStartObject()
                writer.WriteString("case", case.Name)
                writer.WritePropertyName "fields"
                writer.WriteStartArray()
                let types = case.GetFields()

                for index in 0 .. fields.Length - 1 do
                    write writer types[index].PropertyType fields[index]

                writer.WriteEndArray()
                writer.WriteEndObject()
        | _ -> raise (Shape $"The type {kind.Name} can't be written")

    let private expect (element: JsonElement) (wanted: JsonValueKind) (what: string) =
        if element.ValueKind <> wanted then
            raise (Shape $"{what} is not a {wanted}")

    let rec private read (kind: Type) (element: JsonElement) : obj =
        if kind = typeof<string> then
            expect element JsonValueKind.String "A string"
            box (element.GetString())
        elif kind = typeof<int> then
            expect element JsonValueKind.Number "An integer"
            box (element.GetInt32())
        elif kind = typeof<decimal> then
            expect element JsonValueKind.Number "A decimal"
            box (element.GetDecimal())
        elif kind = typeof<float> then
            expect element JsonValueKind.Number "A number"
            box (element.GetDouble())
        elif kind = typeof<bool> then
            box (element.GetBoolean())
        elif kind = typeof<DateOnly> then
            expect element JsonValueKind.String "A date"

            match
                DateOnly.TryParseExact(
                    element.GetString(),
                    "yyyy-MM-dd",
                    CultureInfo.InvariantCulture,
                    DateTimeStyles.None
                )
            with
            | true, date -> box date
            | _ -> raise (Shape "A date isn't in the form yyyy-MM-dd")
        elif kind = typeof<DateTimeOffset> then
            expect element JsonValueKind.String "A timestamp"
            box (element.GetDateTimeOffset())
        elif kind = typeof<Jurisdiction> then
            expect element JsonValueKind.String "A jurisdiction"

            match Jurisdiction.tryParse (element.GetString()) with
            | ValueSome jurisdiction -> box jurisdiction
            | ValueNone -> raise (Shape "A jurisdiction isn't an ISO 3166 code")
        elif isList kind then
            expect element JsonValueKind.Array "A list"
            let item = kind.GetGenericArguments()[0]

            let cons =
                FSharpType.GetUnionCases(kind) |> Array.find (fun case -> case.Name = "Cons")

            let nil =
                FSharpType.GetUnionCases(kind) |> Array.find (fun case -> case.Name = "Empty")

            element.EnumerateArray()
            |> Seq.toArray
            |> Array.rev
            |> Array.fold
                (fun tail head -> FSharpValue.MakeUnion(cons, [| read item head; tail |]))
                (FSharpValue.MakeUnion(nil, [||]))
        elif isVOption kind then
            let cases = FSharpType.GetUnionCases kind

            if element.ValueKind = JsonValueKind.Null then
                FSharpValue.MakeUnion(cases |> Array.find (fun case -> case.Name = "ValueNone"), [||])
            else
                FSharpValue.MakeUnion(
                    cases |> Array.find (fun case -> case.Name = "ValueSome"),
                    [| read (kind.GetGenericArguments()[0]) element |]
                )
        elif FSharpType.IsTuple kind then
            expect element JsonValueKind.Array "A tuple"
            let types = FSharpType.GetTupleElements kind
            let items = element.EnumerateArray() |> Seq.toArray

            if items.Length <> types.Length then
                raise (Shape "A tuple has the wrong length")

            FSharpValue.MakeTuple(Array.map2 read types items, kind)
        elif FSharpType.IsRecord kind then
            expect element JsonValueKind.Object $"A {kind.Name}"
            let fields = FSharpType.GetRecordFields kind
            let known = fields |> Array.map _.Name |> Set.ofArray

            for property in element.EnumerateObject() do
                if not (known.Contains property.Name) then
                    raise (Shape $"{kind.Name} has no field {property.Name}")

            FSharpValue.MakeRecord(
                kind,
                fields
                |> Array.map (fun field ->
                    match element.TryGetProperty field.Name with
                    | true, value -> read field.PropertyType value
                    | false, _ when isVOption field.PropertyType ->
                        FSharpValue.MakeUnion(
                            FSharpType.GetUnionCases field.PropertyType
                            |> Array.find (fun case -> case.Name = "ValueNone"),
                            [||]
                        )
                    | false, _ -> raise (Shape $"{kind.Name} lacks {field.Name}"))
            )
        elif FSharpType.IsUnion kind then
            let cases = FSharpType.GetUnionCases kind

            let find (name: string) =
                match cases |> Array.tryFind (fun case -> case.Name = name) with
                | Some case -> case
                | None -> raise (Shape $"{kind.Name} has no case {name}")

            match element.ValueKind with
            | JsonValueKind.String ->
                let case = find (element.GetString())

                if case.GetFields().Length <> 0 then
                    raise (Shape $"{kind.Name} case {case.Name} needs fields")

                FSharpValue.MakeUnion(case, [||])
            | JsonValueKind.Object ->
                match element.TryGetProperty "case", element.TryGetProperty "fields" with
                | (true, name), (true, values) when
                    name.ValueKind = JsonValueKind.String && values.ValueKind = JsonValueKind.Array
                    ->
                    let case = find (name.GetString())
                    let types = case.GetFields()
                    let items = values.EnumerateArray() |> Seq.toArray

                    if items.Length <> types.Length then
                        raise (Shape $"{kind.Name} case {case.Name} has the wrong number of fields")

                    FSharpValue.MakeUnion(
                        case,
                        Array.map2
                            (fun (field: Reflection.PropertyInfo) item -> read field.PropertyType item)
                            types
                            items
                    )
                | _ -> raise (Shape $"{kind.Name} needs a case and fields")
            | _ -> raise (Shape $"{kind.Name} is neither a case name nor an object")
        else
            raise (Shape $"The type {kind.Name} can't be read")

    /// The case file as UTF-8 JSON text, raw answers included. It can hold personal data, so it needs the care of the case itself.
    let toJson (case: CaseFile) =
        use stream = new MemoryStream()
        use writer = new Utf8JsonWriter(stream)
        write writer typeof<CaseFile> (box case)
        writer.Flush()
        Encoding.UTF8.GetString(stream.ToArray())

    /// A case file from stored JSON. A file of another schema version, or one that doesn't have this shape in every part, is refused whole.
    let ofJson (json: string) : Result<CaseFile, CaseJsonRefused> =
        try
            use document = JsonDocument.Parse(if isNull json then "" else json)
            let root = document.RootElement

            let version =
                if root.ValueKind <> JsonValueKind.Object then
                    ValueNone
                else
                    match root.TryGetProperty "SchemaVersion" with
                    | true, value when value.ValueKind = JsonValueKind.Number ->
                        match value.TryGetInt32() with
                        | true, number -> ValueSome number
                        | _ -> ValueNone
                    | _ -> ValueNone

            if version <> ValueSome CaseFile.CurrentSchemaVersion then
                Error(CaseJsonRefused.WrongVersion version)
            else
                Ok(read typeof<CaseFile> root :?> CaseFile)
        with
        | Shape reason -> Error(CaseJsonRefused.WrongShape reason)
        | :? JsonException as error -> Error(CaseJsonRefused.NotJson $"Line {error.LineNumber}")
        | :? FormatException -> Error(CaseJsonRefused.WrongShape "A value doesn't have the expected form")
        | :? InvalidOperationException -> Error(CaseJsonRefused.WrongShape "A value doesn't have the expected type")

/// A plain export for an audit file.
module CaseExport =

    let private entityName (case: CaseFile) key =
        case.Entities
        |> List.tryFind (fun entity -> entity.Key = key)
        |> Option.map _.Name
        |> Option.defaultValue key

    let private dateOf (date: DateOnly voption) =
        date
        |> ValueOption.map (fun on -> on.ToString("yyyy-MM-dd", CultureInfo.InvariantCulture))
        |> ValueOption.defaultValue "not stated"

    let private gapKindWords kind =
        match kind with
        | GapKind.NotChecked -> "not checked"
        | GapKind.NoRoute -> "no route"
        | GapKind.NeedsPaidContract -> "needs a paid contract"
        | GapKind.NeedsEntitlement -> "needs entitlement"
        | GapKind.SourceFailed -> "source failed"
        | GapKind.NotAvailableForCountry -> "not available for this country"
        | GapKind.NotPublished -> "not published"

    /// The case as text, section by section. With includeRaw, each answer as received follows its evidence, and those can hold personal data. It states what
    /// sources said and what is missing, and holds no result: the decision is the officer's.
    let toText (includeRaw: bool) (case: CaseFile) =
        let lines =
            [
                $"Case {case.CaseId}, opened {case.OpenedAt:``yyyy-MM-dd HH:mm``} UTC"
                $"About: {entityName case case.Subject} ({case.Subject})"
                "This file holds what sources said, what stands out and what is missing. The decision is the officer's."
                ""
                "REVIEW NEEDED"

                match CaseFile.reviewNeeded case with
                | [] -> "  Nothing listed, which doesn't mean anything has been cleared."
                | items ->
                    for item in items do
                        $"  [{item.Reference}] {item.Text}"

                ""
                "ENTITIES"

                for entity in case.Entities do
                    let standing =
                        entity.Standing
                        |> ValueOption.map Reading.standingWords
                        |> ValueOption.defaultValue "not read from a register"

                    $"  {entity.Name} ({entity.Key})"
                    $"    Role: %A{entity.Role}. Standing on this record: %s{standing}"

                    for identity in entity.Identities do
                        $"    {EntityIdentity.describe identity}"

                ""
                "LINKS"

                for link in case.Links do
                    $"  [{link.Id}] {entityName case link.From} -> {entityName case link.To}: {CaseLink.describeKind link.Kind}"
                    $"    Provenance: {CaseLink.describeProvenance link.Provenance}"

                    match link.Note with
                    | ValueSome note -> $"    Note: {note}"
                    | ValueNone -> ()

                ""
                "FLAGS"

                for flag in case.Flags do
                    $"  [{flag.Id}] On {entityName case flag.On}: {flag.Statement} (Source: {flag.Source})"

                ""
                "DIFFERENCES BETWEEN SOURCES"

                for found in Discrepancies.all case do
                    let label =
                        match found.Label with
                        | DiscrepancyLabel.Disagreement -> "disagreement"
                        | DiscrepancyLabel.DifferentKinds why -> $"not a contradiction: {why}"

                    $"  {entityName case found.About}, %A{found.Field}, %s{label}: %s{found.Statement}"

                ""
                "GAPS"

                for gap in case.Gaps do
                    let reach =
                        match gap.Reach with
                        | GapReach.WholeCase -> "whole case"
                        | GapReach.ThisEntity -> "this entity"

                    let closed =
                        match gap.ClosedBy with
                        | ValueSome(who, evidence) -> $" Closed by {who.By} on {who.At:``yyyy-MM-dd``} with {evidence}."
                        | ValueNone -> ""

                    $"  [{gap.Id}] {gap.Subject} ({gapKindWords gap.Kind}, {reach}): {gap.Detail}. What would close it: {gap.WouldClose}.{closed}"

                ""
                "DOCUMENTS TO READ"

                for document in case.Documents do
                    let mark = if CaseDocument.isHighlighted document then "*" else " "
                    let pages = document.Pages |> ValueOption.map string |> ValueOption.defaultValue "?"

                    let link = document.Link |> ValueOption.defaultValue "no link"
                    let form = document.FormType |> ValueOption.defaultValue "unknown form"
                    $" %s{mark}[%s{document.Id}] {entityName case document.Entity}: %s{form}, %A{document.Kind}, filed {dateOf document.FiledOn}, %s{pages} pages, %s{link}"

                ""
                "VENDOR CHECKS"

                for check in case.VendorChecks do
                    $"  {check.Vendor} {check.Product}, reference {check.Reference}, on {check.CheckedAt:``yyyy-MM-dd``}: {check.OutcomeWords}. That is this vendor's result on that date, and closes nothing else here."

                ""
                "SANCTIONS SCREENINGS"

                for screening in case.Screenings do
                    $"  Run {screening.At:``yyyy-MM-dd HH:mm``} UTC, which shows only what these names resembled on these lists on these dates."

                    for list in screening.ListsRead do
                        let inFile =
                            match list.StatedInFile with
                            | ValueSome stated -> $", the file states {stated:``yyyy-MM-dd``}"
                            | ValueNone -> ""

                        $"    Read: %A{list.List}, dated {list.Dated:``yyyy-MM-dd``}%s{inFile}, %d{list.Entries} entries"

                        for note in list.Notes do
                            $"      {note}"

                    for list in screening.ListsNotUsed do
                        $"    Not used: %A{list.List}: %s{list.Reason}"

                    $"""    Names screened: {String.concat "; " screening.NamesScreened}"""

                    for name in screening.NamesNotScreened do
                        $"    Not screened: {name.Who}: {name.Reason}"

                    for found in screening.PossibleMatches do
                        $"    Possible match for review: %s{found.ScreenedName} / %s{found.ListedName}, %A{found.List} entry %s{found.EntryId}, score {found.Score}: %s{found.ScoreReason}; birth year %A{found.BirthYear}; country %A{found.Country}"

                ""
                "EVIDENCE"

                for item in case.Evidence do
                    $"  [%s{item.Id}] {entityName case item.About}: %s{item.Topic}, from %s{item.Source} (%A{item.Origin}), retrieved {item.RetrievedAt:``yyyy-MM-dd HH:mm``}, as of {dateOf item.AsOf}"

                    match item.Attribution with
                    | ValueSome who -> $"    Entered by {who.By} on {who.At:``yyyy-MM-dd``}, from {who.Source}"
                    | ValueNone -> ()

                    match item.Note with
                    | ValueSome note -> $"    {note}"
                    | ValueNone -> ()

                    for fact in item.Facts do
                        $"    %A{fact}"

                    if includeRaw then
                        match item.Raw with
                        | ValueSome raw -> $"    Raw: {raw}"
                        | ValueNone -> ()

                ""
                "NOTES"

                for note in case.Notes do
                    $"  [{note.Id}] {note.Attribution.By} on {note.Attribution.At:``yyyy-MM-dd``}, from {note.Attribution.Source}: {note.Text}"
            ]

        String.concat "\n" lines
