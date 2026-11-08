namespace CompanyDueDiligence

open System
open System.Globalization
open System.IO
open System.Text
open System.Text.RegularExpressions
open System.Xml
open System.Xml.Linq

/// The lists a screening can state. A result names which of them were read, and their dates.
[<RequireQualifiedAccess>]
type SanctionsListName =
    | OfacSdn
    | OfacConsolidated
    | UkFcdo
    | UnConsolidated
    | EuConsolidated

[<RequireQualifiedAccess>]
type ListedParty =
    | Individual
    | Entity
    /// A vessel or aircraft, or a type the list doesn't state.
    | Other of word: string voption

/// One entry of a list, with its names and aliases.
type ListEntry =
    {
        /// The list's own id for it.
        EntryId: string
        Party: ListedParty
        /// The primary name first, then aliases.
        Names: string list
        Programme: string voption
        /// Birth years the list gives, read from whatever form its dates take.
        BirthYears: int list
        /// Countries the list gives, in its wording: nationality, birth, address.
        Countries: string list
    }

/// A list read from text the host downloaded. The library stores nothing and fetches nothing.
type LoadedList =
    {
        List: SanctionsListName
        /// The date the host says this text is from.
        Dated: DateOnly
        /// The date the file itself states, for a list that states one.
        StatedInFile: DateOnly voption
        Entries: ListEntry list
        /// What wasn't supplied or read with it, such as an alias file.
        Notes: string list
    }

/// A list's text that couldn't be read, which never counts as screened.
type UnreadableList =
    {
        List: SanctionsListName
        Dated: DateOnly
        Reason: string
    }

/// Text the host hands over, from the URLs in the README, with the date it was downloaded or published.
[<RequireQualifiedAccess>]
type ListText =
    /// OFAC's SDN files. aliases is alt.csv and addresses add.csv.
    | OfacSdn of sdn: string * aliases: string voption * addresses: string voption * dated: DateOnly
    /// OFAC's consolidated files. aliases is cons_alt.csv and addresses cons_add.csv.
    | OfacConsolidated of primary: string * aliases: string voption * addresses: string voption * dated: DateOnly
    /// UK-Sanctions-List.csv.
    | Uk of csv: string * dated: DateOnly
    /// The UN's consolidated.xml.
    | Un of xml: string * dated: DateOnly

/// The fewest entries a list may hold and still be read as a whole. A file below it counts as cut off, as the UK's has no end marker. A host lowers it if a
/// list really shrinks that far. A file cut off but still above it can't be told from a whole one.
[<Struct>]
type ListMinimums =
    {
        OfacSdn: int
        OfacConsolidated: int
        Uk: int
        Un: int
    }

module ListMinimums =

    /// 80% of each list's entries as downloaded on 2026-11-06: SDN 19,555, consolidated 486, UK 6,420 (from 58,836 rows), UN 1,007. So a list can lose a
    /// fifth of today's size before it is refused.
    let defaults =
        {
            OfacSdn = 15644
            OfacConsolidated = 388
            Uk = 5136
            Un = 805
        }

    /// Any list with an entry passes, which says nothing about whether it is whole.
    let anyEntries =
        {
            OfacSdn = 1
            OfacConsolidated = 1
            Uk = 1
            Un = 1
        }

    let internal forList (minimums: ListMinimums) (list: SanctionsListName) =
        match list with
        | SanctionsListName.OfacSdn -> minimums.OfacSdn
        | SanctionsListName.OfacConsolidated -> minimums.OfacConsolidated
        | SanctionsListName.UkFcdo -> minimums.Uk
        | SanctionsListName.UnConsolidated -> minimums.Un
        | SanctionsListName.EuConsolidated -> 1

/// Readers for the lists' file formats, as downloaded on 2026-11-06. A file that doesn't have the expected shape is unreadable as a whole: part of a list
/// must not pass for all of it.
module internal ListFiles =

    exception private BadFile of string

    /// The record starting at start: its fields and where the next one starts, or none at the end of the text.
    let private csvRowAt (text: string) (start: int) =
        let field = StringBuilder()

        let take () =
            let value = field.ToString()
            field.Clear() |> ignore
            value

        let rec read i quoted (fields: string list) =
            let record (last: string) =
                List.rev (last :: fields) |> Array.ofList

            if i >= text.Length then
                if quoted then
                    raise (BadFile "The file ends inside a quoted field")
                elif field.Length = 0 && fields.IsEmpty then
                    ValueNone
                else
                    ValueSome(record (take ()), i)
            else
                match text[i], quoted with
                | '"', true when i + 1 < text.Length && text[i + 1] = '"' ->
                    field.Append '"' |> ignore
                    read (i + 2) true fields
                | '"', true -> read (i + 1) false fields
                | '"', false -> read (i + 1) true fields
                | ',', false -> read (i + 1) false (take () :: fields)
                | '\n', false -> ValueSome(record ((take ()).TrimEnd '\r'), i + 1)
                | c, _ ->
                    field.Append c |> ignore
                    read (i + 1) quoted fields

        read start false []

    /// RFC 4180 records. Quoted fields can hold line ends and doubled quotes.
    let csvRows (text: string) =
        Seq.unfold (fun start -> csvRowAt text start |> ValueOption.toOption) 0

    /// OFAC writes a missing value as -0-, and ends a file with a Ctrl-Z.
    let private ofacValue (text: string) =
        let trimmed = text.Trim()

        if trimmed = "" || trimmed = "-0-" then
            ValueNone
        else
            ValueSome trimmed

    let private isBlankRow (row: string[]) = row.Length = 1 && row[0].Trim() = ""

    let private isOfacEnd (row: string[]) =
        row.Length = 1 && row[0].Trim() = "\u001a"

    /// The rows before the end marker. A file without one may be cut off, so none of it is read.
    let private ofacRows (name: string) (columns: int) (text: string) =
        let rows = csvRows text |> Seq.filter (isBlankRow >> not) |> List.ofSeq

        match List.tryLast rows with
        | Some last when isOfacEnd last -> ()
        | _ -> raise (BadFile $"{name}: the file doesn't end with OFAC's Ctrl-Z, so it may be cut off")

        rows
        |> List.take (rows.Length - 1)
        |> Seq.map (fun row ->
            if row.Length <> columns then
                raise (BadFile $"{name}: a row has {row.Length} columns, not {columns}")
            elif (ofacValue row[0]).IsNone then
                raise (BadFile $"{name}: a row has no id")
            else
                row)

    /// "SMITH, John" as "John SMITH". A company's comma, as in "ACME, LTD.", stays, because only individuals are written last name first.
    let private ofacName (party: ListedParty) (text: string) =
        match party, text.IndexOf ',' with
        | ListedParty.Individual, at when at > 0 -> text.Substring(at + 1).Trim() + " " + text.Substring(0, at).Trim()
        | _ -> text.Trim()

    let private dobRegex = Regex(@"DOB[^;]*?\b(\d{4})\b", RegexOptions.Compiled)

    let private countryRemarkRegex =
        Regex(@"\b(?:nationality|citizen|POB)\s+([^;.]+)", RegexOptions.Compiled ||| RegexOptions.IgnoreCase)

    let private ofacParty (word: string voption) =
        match word with
        | ValueNone -> ListedParty.Entity
        | ValueSome word when word.Equals("individual", StringComparison.OrdinalIgnoreCase) -> ListedParty.Individual
        | other -> ListedParty.Other other

    /// A companion file with nothing in it was not supplied.
    let private supplied (text: string voption) =
        text
        |> ValueOption.filter (fun held -> not (String.IsNullOrWhiteSpace held) && held.Trim() <> "\u001a")

    let private ofac
        (list: SanctionsListName)
        (primary: string)
        (aliases: string voption)
        (addresses: string voption)
        (dated: DateOnly)
        : LoadedList =
        let aliases = supplied aliases
        let addresses = supplied addresses

        let aliasesById =
            match aliases with
            | ValueNone -> Map.empty
            | ValueSome text ->
                ofacRows "aliases" 5 text
                |> Seq.choose (fun row ->
                    match ofacValue row[3] with
                    | ValueSome name -> Some(row[0].Trim(), name)
                    | ValueNone -> None)
                |> Seq.groupBy fst
                |> Seq.map (fun (pid, names) -> pid, names |> Seq.map snd |> List.ofSeq)
                |> Map.ofSeq

        let countriesById =
            match addresses with
            | ValueNone -> Map.empty
            | ValueSome text ->
                ofacRows "addresses" 6 text
                |> Seq.choose (fun row ->
                    match ofacValue row[4] with
                    | ValueSome country -> Some(row[0].Trim(), country)
                    | ValueNone -> None)
                |> Seq.groupBy fst
                |> Seq.map (fun (pid, countries) -> pid, countries |> Seq.map snd |> Seq.distinct |> List.ofSeq)
                |> Map.ofSeq

        let entries =
            ofacRows "primary" 12 primary
            |> Seq.map (fun row ->
                let id = row[0].Trim()
                let party = ofacParty (ofacValue row[2])

                let name =
                    match ofacValue row[1] with
                    | ValueSome name -> ofacName party name
                    | ValueNone -> raise (BadFile "primary: a row has no name")

                let remarks = ofacValue row[11] |> ValueOption.defaultValue ""

                {
                    EntryId = id
                    Party = party
                    Names =
                        name
                        :: (aliasesById.TryFind id |> Option.defaultValue [] |> List.map (ofacName party))
                    Programme = ofacValue row[3]
                    BirthYears = [ for found in dobRegex.Matches remarks -> int found.Groups[1].Value ]
                    Countries =
                        (countriesById.TryFind id |> Option.defaultValue [])
                        @ [
                            for found in countryRemarkRegex.Matches remarks -> found.Groups[1].Value.Trim()
                        ]
                })
            |> List.ofSeq

        {
            List = list
            Dated = dated
            StatedInFile = ValueNone
            Entries = entries
            Notes =
                [
                    if aliases.IsNone then
                        "The alias file wasn't supplied, so aliases weren't screened"

                    if addresses.IsNone then
                        "The address file wasn't supplied, so address countries weren't compared"
                ]
        }

    let private ukReportDate =
        Regex(@"\AReport Date:\s*(\d{2}-[A-Za-z]{3}-\d{4})\s*\z", RegexOptions.Compiled)

    /// The columns read from the UK file, each required in its header.
    let private ukColumns =
        [
            "Unique ID"
            "Name 6"
            "Name 1"
            "Name 2"
            "Name 3"
            "Name 4"
            "Name 5"
            "Name type"
            "Regime Name"
            "Designation Type"
            "D.O.B"
            "Nationality(/ies)"
            "Country of birth"
            "Address Country"
        ]

    let private yearInUkDate = Regex(@"(\d{4})", RegexOptions.Compiled)

    let private uk (text: string) (dated: DateOnly) : LoadedList =
        let rows = csvRows text
        use cursor = rows.GetEnumerator()

        let nextRow reason =
            if cursor.MoveNext() then
                cursor.Current
            else
                raise (BadFile reason)

        let stated =
            let first = nextRow "The file is empty"
            let found = ukReportDate.Match(first[0].TrimStart '﻿')

            if not found.Success then
                raise (BadFile "The first line isn't a report date")

            match
                DateOnly.TryParseExact(
                    found.Groups[1].Value,
                    "dd-MMM-yyyy",
                    CultureInfo.InvariantCulture,
                    DateTimeStyles.None
                )
            with
            | true, date -> date
            | _ -> raise (BadFile "The report date can't be read")

        let header = nextRow "The file has no header"

        let columnsInFile =
            ukColumns
            |> List.map (fun name ->
                match Array.IndexOf(header, name) with
                | -1 -> raise (BadFile $"The header has no column {name}")
                | index -> index)
            |> Array.ofList

        let positionOf =
            ukColumns |> List.mapi (fun position name -> name, position) |> Map.ofList

        let cell (row: string[]) name = row[positionOf[name]]

        // Only the columns read are kept: the statements of reasons are most of the file's size.
        let parsed =
            [
                while cursor.MoveNext() do
                    let row = cursor.Current

                    if row.Length <> header.Length then
                        raise (BadFile $"A row has {row.Length} columns, not {header.Length}")

                    columnsInFile |> Array.map (fun index -> row[index].Trim())
            ]

        let entries =
            parsed
            |> List.groupBy (fun row -> cell row "Unique ID")
            |> List.map (fun (id, rows) ->
                if id = "" then
                    raise (BadFile "A row has no id")

                let nameOf row =
                    [ "Name 1"; "Name 2"; "Name 3"; "Name 4"; "Name 5"; "Name 6" ]
                    |> List.map (cell row)
                    |> List.filter ((<>) "")
                    |> String.concat " "

                let isPrimary row =
                    (cell row "Name type").StartsWith("primary name", StringComparison.OrdinalIgnoreCase)
                    && not ((cell row "Name type").Contains("variation", StringComparison.OrdinalIgnoreCase))

                let names =
                    (rows |> List.filter isPrimary) @ (rows |> List.filter (isPrimary >> not))
                    |> List.map nameOf
                    |> List.filter ((<>) "")
                    |> List.distinct

                let distinctCells name =
                    rows
                    |> List.map (fun row -> cell row name)
                    |> List.filter ((<>) "")
                    |> List.distinct

                {
                    EntryId = id
                    Party =
                        match cell rows.Head "Designation Type" with
                        | "Individual" -> ListedParty.Individual
                        | "Entity" -> ListedParty.Entity
                        | "" -> ListedParty.Other ValueNone
                        | other -> ListedParty.Other(ValueSome other)
                    Names = names
                    Programme = distinctCells "Regime Name" |> List.tryHead |> ValueOption.ofOption
                    BirthYears =
                        distinctCells "D.O.B"
                        |> List.collect (fun text -> [ for found in yearInUkDate.Matches text -> int found.Value ])
                        |> List.distinct
                    Countries =
                        (distinctCells "Nationality(/ies)"
                         |> List.collect (fun text -> text.Split(',') |> List.ofArray))
                        @ distinctCells "Country of birth"
                        @ distinctCells "Address Country"
                        |> List.map _.Trim()
                        |> List.filter ((<>) "")
                        |> List.distinct
                })

        {
            List = SanctionsListName.UkFcdo
            Dated = dated
            StatedInFile = ValueSome stated
            Entries = entries
            Notes = []
        }

    let private un (text: string) (dated: DateOnly) : LoadedList =
        let settings =
            XmlReaderSettings(DtdProcessing = DtdProcessing.Prohibit, XmlResolver = null)

        use reader = XmlReader.Create(new StringReader(text), settings)
        let document = XDocument.Load reader
        let root = document.Root

        if isNull root || root.Name.LocalName <> "CONSOLIDATED_LIST" then
            raise (BadFile "The root isn't CONSOLIDATED_LIST")

        let group (name: string) =
            match root.Element(XName.Get name) with
            | null -> raise (BadFile $"The file has no {name}")
            | element -> element

        let value (element: XElement) (name: string) =
            match element.Element(XName.Get name) with
            | null -> ValueNone
            | child when String.IsNullOrWhiteSpace child.Value -> ValueNone
            | child -> ValueSome(child.Value.Trim())

        let values (element: XElement) (block: string) (name: string) =
            [
                for item in element.Elements(XName.Get block) do
                    match value item name with
                    | ValueSome text -> text
                    | ValueNone -> ()
            ]

        let entry (element: XElement) (party: ListedParty) (aliasBlock: string) (addressBlock: string) =
            let primary =
                [ "FIRST_NAME"; "SECOND_NAME"; "THIRD_NAME"; "FOURTH_NAME" ]
                |> List.choose (value element >> ValueOption.toOption)
                |> String.concat " "

            if primary = "" then
                raise (BadFile "An entry has no name")

            {
                EntryId =
                    match value element "REFERENCE_NUMBER" |> ValueOption.orElse (value element "DATAID") with
                    | ValueSome pid -> pid
                    | ValueNone -> raise (BadFile "An entry has no id")
                Party = party
                Names = primary :: values element aliasBlock "ALIAS_NAME"
                Programme = value element "UN_LIST_TYPE"
                BirthYears =
                    [
                        for birth in element.Elements(XName.Get "INDIVIDUAL_DATE_OF_BIRTH") do
                            for child in birth.Elements() do
                                if
                                    child.Name.LocalName = "YEAR"
                                    || child.Name.LocalName = "FROM_YEAR"
                                    || child.Name.LocalName = "TO_YEAR"
                                then
                                    match Int32.TryParse child.Value with
                                    | true, year -> year
                                    | _ -> ()
                                elif child.Name.LocalName = "DATE" && child.Value.Length >= 4 then
                                    match Int32.TryParse(child.Value.AsSpan(0, 4)) with
                                    | true, year -> year
                                    | _ -> ()
                    ]
                Countries =
                    [
                        for nationality in element.Elements(XName.Get "NATIONALITY") do
                            for item in nationality.Elements(XName.Get "VALUE") do
                                if not (String.IsNullOrWhiteSpace item.Value) then
                                    item.Value.Trim()
                    ]
                    @ values element addressBlock "COUNTRY"
            }

        let entries =
            [
                for element in (group "INDIVIDUALS").Elements(XName.Get "INDIVIDUAL") do
                    entry element ListedParty.Individual "INDIVIDUAL_ALIAS" "INDIVIDUAL_ADDRESS"

                for element in (group "ENTITIES").Elements(XName.Get "ENTITY") do
                    entry element ListedParty.Entity "ENTITY_ALIAS" "ENTITY_ADDRESS"
            ]

        {
            List = SanctionsListName.UnConsolidated
            Dated = dated
            StatedInFile =
                match root.Attribute(XName.Get "dateGenerated") with
                | null -> ValueNone
                | attribute ->
                    match
                        DateTimeOffset.TryParse(
                            attribute.Value,
                            CultureInfo.InvariantCulture,
                            DateTimeStyles.AssumeUniversal
                        )
                    with
                    | true, at -> ValueSome(DateOnly.FromDateTime at.UtcDateTime)
                    | _ -> ValueNone
            Entries = entries
            Notes = []
        }

    /// A list read as a whole, or why it can't be. A list with no entries, or fewer than its minimum, counts as unreadable.
    let load (minimums: ListMinimums) (text: ListText) : Result<LoadedList, UnreadableList> =
        let name, dated =
            match text with
            | ListText.OfacSdn(_, _, _, dated) -> SanctionsListName.OfacSdn, dated
            | ListText.OfacConsolidated(_, _, _, dated) -> SanctionsListName.OfacConsolidated, dated
            | ListText.Uk(_, dated) -> SanctionsListName.UkFcdo, dated
            | ListText.Un(_, dated) -> SanctionsListName.UnConsolidated, dated

        let unreadable reason =
            Error
                {
                    List = name
                    Dated = dated
                    Reason = reason
                }

        try
            let loaded =
                match text with
                | ListText.OfacSdn(primary, aliases, addresses, dated) ->
                    ofac SanctionsListName.OfacSdn primary aliases addresses dated
                | ListText.OfacConsolidated(primary, aliases, addresses, dated) ->
                    ofac SanctionsListName.OfacConsolidated primary aliases addresses dated
                | ListText.Uk(csv, dated) -> uk csv dated
                | ListText.Un(xml, dated) -> un xml dated

            let minimum = ListMinimums.forList minimums name

            if loaded.Entries.IsEmpty then
                unreadable "The file holds no entries"
            elif loaded.Entries.Length < minimum then
                unreadable
                    $"The file holds {loaded.Entries.Length} entries, below the minimum of {minimum}, so it may be cut off. Lower ListMinimums if the list really is this small"
            else
                Ok loaded
        with
        | BadFile reason -> unreadable reason
        | :? XmlException -> unreadable "The file isn't well-formed XML"
        | :? ArgumentNullException -> unreadable "The file's text is missing"

/// Whether a detail agrees between the screened name and the listed entry. Only when both sides have it does it say anything.
[<RequireQualifiedAccess>]
type DetailComparison =
    | BothSidesMatch of detail: string
    | BothSidesDiffer of ours: string * listed: string
    | NotBothKnown

/// A name on a list that looks like one screened, for a person to review. It is never a finding that this party is the listed one.
type PossibleMatch =
    {
        ScreenedName: string
        /// The case entity the name belongs to, if any.
        EntityKey: string voption
        List: SanctionsListName
        ListDated: DateOnly
        EntryId: string
        ListedName: string
        Programme: string voption
        /// 0 to 1, and only a way to sort these for review.
        Score: float
        /// Why this pair was raised.
        ScoreReason: string
        BirthYear: DetailComparison
        Country: DetailComparison
    }

/// A list as a screening used it.
type ScreenedList =
    {
        List: SanctionsListName
        Dated: DateOnly
        StatedInFile: DateOnly voption
        Entries: int
        Notes: string list
    }

/// What is known of a party to screen. The BornIn and Countries come from whichever source gave them.
type NameToScreen =
    {
        Name: string
        EntityKey: string voption
        Party: PartyKind
        BornIn: YearMonth voption
        Countries: string list
    }

/// One that couldn't be put through, and why: an owner nobody has identified has no name to screen.
type NameNotScreened = { Who: string; Reason: string }

/// A list not used, and why.
type ListNotUsed =
    {
        List: SanctionsListName
        Reason: string
        WouldClose: string
    }

/// A screening run. It has no clean result: no possible match means only that these names didn't resemble these entries on these dates.
type Screening =
    {
        At: DateTimeOffset
        ListsRead: ScreenedList list
        ListsNotUsed: ListNotUsed list
        NamesScreened: string list
        NamesNotScreened: NameNotScreened list
        PossibleMatches: PossibleMatch list
    }

/// Screening over lists the host supplied. It has no storage, makes no calls and draws no conclusion.
module SanctionsScreening =

    /// Lists no text can be supplied for yet. The EU's file URL answered 403 without a token on 2026-11-06.
    let knownGaps: ListNotUsed list =
        [
            {
                List = SanctionsListName.EuConsolidated
                Reason =
                    "The EU's Financial Sanctions Files answered 403 without a token when checked on 2026-11-06, and no parser for the file is here"
                WouldClose =
                    "A token from the EU's Financial Sanctions Files service and a parser for that file, or a vendor's screening that names the EU list"
            }
        ]

    /// The lists read, with any that couldn't be.
    let load (minimums: ListMinimums) (texts: ListText list) =
        texts |> List.map (ListFiles.load minimums)

    let private listsExpected =
        [
            SanctionsListName.OfacSdn
            SanctionsListName.OfacConsolidated
            SanctionsListName.UkFcdo
            SanctionsListName.UnConsolidated
        ]

    let private birthComparison (name: NameToScreen) (entry: ListEntry) =
        match name.BornIn, entry.BirthYears with
        | ValueNone, _
        | _, [] -> DetailComparison.NotBothKnown
        | ValueSome born, years when List.contains born.Year years -> DetailComparison.BothSidesMatch(string born.Year)
        | ValueSome born, years ->
            DetailComparison.BothSidesDiffer(string born.Year, years |> List.map string |> String.concat ", ")

    let private countryComparison (name: NameToScreen) (entry: ListEntry) =
        let plain = NameMatching.normalise

        let ours =
            name.Countries |> List.map plain |> List.filter ((<>) "") |> List.distinct

        let theirs =
            entry.Countries |> List.map plain |> List.filter ((<>) "") |> List.distinct

        match ours, theirs with
        | [], _
        | _, [] -> DetailComparison.NotBothKnown
        | _ ->
            match ours |> List.filter (fun country -> List.contains country theirs) with
            | [] -> DetailComparison.BothSidesDiffer(String.concat ", " ours, String.concat ", " theirs)
            | shared -> DetailComparison.BothSidesMatch(String.concat ", " shared)

    /// A person isn't compared with a listed company, nor a company with a listed person. A party of unknown kind goes against both.
    let private comparable (party: PartyKind) (listed: ListedParty) =
        match party, listed with
        | PartyKind.Individual, ListedParty.Entity
        | PartyKind.LegalEntity, ListedParty.Individual -> false
        | _ -> true

    /// The best match of one name against one list's entries, one per entry.
    let private matchesIn
        (isCommon: string -> bool)
        (name: NameToScreen)
        ((list, prepared): LoadedList * (ListEntry * (string * NameMatching.Prepared) list) list)
        =
        let ours = NameMatching.prepare name.Name

        prepared
        |> List.choose (fun (entry, names) ->
            if not (comparable name.Party entry.Party) then
                None
            else
                names
                |> List.choose (fun (listed, theirs) ->
                    match NameMatching.comparePrepared isCommon ours theirs with
                    | ValueSome(score, reason) -> Some(score, reason, listed)
                    | ValueNone -> None)
                |> function
                    | [] -> None
                    | found ->
                        let score, reason, listed = found |> List.maxBy (fun (score, _, _) -> score)

                        Some
                            {
                                ScreenedName = name.Name
                                EntityKey = name.EntityKey
                                List = list.List
                                ListDated = list.Dated
                                EntryId = entry.EntryId
                                ListedName = listed
                                Programme = entry.Programme
                                Score = score
                                ScoreReason = reason
                                BirthYear = birthComparison name entry
                                Country = countryComparison name entry
                            })

    /// Screens the names against the lists read. A list that couldn't be read, or wasn't supplied, is named as not used, and notScreened is stated
    /// beside the result. Nothing in the answer says any party isn't listed.
    let screen
        (lists: Result<LoadedList, UnreadableList> list)
        (names: NameToScreen list)
        (notScreened: NameNotScreened list)
        (at: DateTimeOffset)
        : Screening =
        let read =
            lists
            |> List.choose (function
                | Ok list -> Some list
                | Error _ -> None)

        let unreadable =
            lists
            |> List.choose (function
                | Error failed ->
                    Some
                        {
                            List = failed.List
                            Reason = $"The text dated {failed.Dated:``yyyy-MM-dd``} couldn't be read: {failed.Reason}"
                            WouldClose = "A fresh download of this list, read again"
                        }
                | Ok _ -> None)

        let accountedFor = (read |> List.map _.List) @ (unreadable |> List.map _.List)

        let notSupplied =
            listsExpected
            |> List.filter (fun list -> not (List.contains list accountedFor))
            |> List.map (fun list ->
                {
                    List = list
                    Reason = "No text was supplied for this list"
                    WouldClose = "Download this list and pass its text with its date"
                })

        let screenable, blank =
            names
            |> List.partition (fun name -> not (NameMatching.tokens name.Name).IsEmpty)

        let prepared =
            read
            |> List.map (fun list ->
                list,
                list.Entries
                |> List.map (fun entry -> entry, entry.Names |> List.map (fun name -> name, NameMatching.prepare name)))

        let entriesHolding (word: string) =
            prepared
            |> List.sumBy (fun (_, entries) ->
                entries
                |> List.filter (fun (_, entryNames) ->
                    entryNames |> List.exists (fun (_, held) -> held.WordSet.Contains word))
                |> List.length)

        let loneWords =
            screenable
            |> List.choose (fun name ->
                match NameMatching.tokens name.Name with
                | [ word ] -> Some(name.Name, word)
                | _ -> None)

        let commonWords =
            loneWords
            |> List.map snd
            |> List.distinct
            |> List.filter (fun word -> entriesHolding word > NameMatching.CommonWordEntries)
            |> Set.ofList

        // Raising a common word would bury the review, so such a name is compared only as a whole, and that is stated.
        let heldBack =
            loneWords
            |> List.filter (fun (_, word) -> NameMatching.saysLittleAlone word || commonWords.Contains word)
            |> List.map (fun (name, word) ->
                {
                    Who = name
                    Reason = $"Compared only as a whole name: {word} is too common in listed names to raise each one"
                })
            |> List.distinct

        {
            At = at
            ListsRead =
                read
                |> List.map (fun list ->
                    {
                        List = list.List
                        Dated = list.Dated
                        StatedInFile = list.StatedInFile
                        Entries = list.Entries.Length
                        Notes = list.Notes
                    })
            ListsNotUsed = unreadable @ notSupplied @ knownGaps
            NamesScreened = screenable |> List.map _.Name |> List.distinct
            NamesNotScreened =
                notScreened
                @ (blank
                   |> List.map (fun name ->
                       {
                           Who = name.Name
                           Reason = "The name holds nothing to compare"
                       }))
                @ heldBack
            PossibleMatches =
                screenable
                |> List.collect (fun name -> prepared |> List.collect (matchesIn commonWords.Contains name))
                |> List.sortByDescending _.Score
        }
