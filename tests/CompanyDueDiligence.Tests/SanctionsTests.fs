/// Sanctions lists in the shapes of the files downloaded on 2026-11-06, with made-up names: no real person or listed party is in this test data.
module SanctionsTests

open System
open Xunit
open CompanyDueDiligence

let private dated = DateOnly(2026, 11, 6)

let private sdn =
    "9001,\"ZEDLAND TRADING WORKS, LTD.\",-0- ,\"ZED\",-0- ,-0- ,-0- ,-0- ,-0- ,-0- ,-0- ,\"Registration Number 12345.\"\r\n"
    + "9002,\"QUILLBY, Ansel Rupert\",\"individual\",\"ZED\",\"Director\",-0- ,-0- ,-0- ,-0- ,-0- ,-0- ,\"DOB 01 Jan 1960; nationality Zedland; Gender Male.\"\r\n"
    + "9003,\"MV ZEDLAND STAR\",\"vessel\",\"ZED\",-0- ,-0- ,-0- ,-0- ,-0- ,-0- ,-0- ,-0- \r\n"
    + "\u001a"

[<Literal>]
let private alt =
    "9001,501,\"aka\",\"ZEDLAND TRADE WORKS\",-0- \r\n9002,502,\"aka\",\"QUILBY, Ansell\",-0- \r\n\u001a"

[<Literal>]
let private add =
    "9001,601,\"1 Example Road\",\"Zed City\",\"Zedland\",-0- \r\n\u001a"

[<Literal>]
let private ukHeader =
    "Last Updated,Unique ID,OFSI Group ID,UN Reference Number,Name 6,Name 1,Name 2,Name 3,Name 4,Name 5,Name type,Alias strength,Title,Name non-latin script,Non-latin script type,Non-latin script language,Regime Name,Designation Type,Designation source,Sanctions Imposed,Other Information,UK Statement of Reasons,Address Line 1,Address Line 2,Address Line 3,Address Line 4,Address Line 5,Address Line 6,Address Postal Code,Address Country,Phone number,Website,Email address,Date Designated,D.O.B,Nationality(/ies),National Identifier number,National Identifier additional information,Passport number,Passport additional information,Position,Gender,Town of birth,Country of birth,Type of entity,Subsidiaries,Parent company,Business registration number (s),IMO number,Current owner/operator (s),Previous owner/operator (s),Current believed flag of ship,Previous flags,Type of ship,Tonnage of ship,Length of ship,Year Built,Hull identification number (HIN)"

/// A UK row with the columns this test cares about, in the file's order.
let private ukRow id family given nameType designation dob nationality =
    let cells = Array.create 58 ""
    cells[1] <- id
    cells[4] <- family
    cells[5] <- given
    cells[10] <- nameType
    cells[16] <- "The Zedland (Sanctions) Regulations"
    cells[17] <- designation
    cells[21] <- "A statement of reasons, with a comma, and\na line end."
    cells[34] <- dob
    cells[35] <- nationality

    cells
    |> Array.map (fun cell ->
        if cell.Contains ',' || cell.Contains '\n' then
            "\"" + cell + "\""
        else
            cell)
    |> String.concat ","

let private uk =
    String.concat
        "\n"
        [
            "Report Date: 03-Nov-2026"
            ukHeader
            ukRow "ZED0001" "ZEDLAND MINING" "" "Primary name" "Entity" "" ""
            ukRow "ZED0001" "ZED MINING" "" "Alias" "Entity" "" ""
            ukRow "ZED0002" "QUILLBY" "Ansel" "Primary Name" "Individual" "01/01/1960" "Zedland"
            ukRow "ZED0002" "QUILBY" "Ansell" "Primary Name Variation" "Individual" "dd/mm/1960" "Zedland"
        ]

let private un =
    """<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<CONSOLIDATED_LIST dateGenerated="2026-11-06T00:00:06.849Z">
  <INDIVIDUALS>
    <INDIVIDUAL>
      <DATAID>1</DATAID>
      <FIRST_NAME>ANSEL</FIRST_NAME>
      <SECOND_NAME>QUILLBY</SECOND_NAME>
      <UN_LIST_TYPE>ZED</UN_LIST_TYPE>
      <REFERENCE_NUMBER>ZEi.001</REFERENCE_NUMBER>
      <NATIONALITY><VALUE>Zedland</VALUE></NATIONALITY>
      <INDIVIDUAL_ALIAS><QUALITY/><ALIAS_NAME/></INDIVIDUAL_ALIAS>
      <INDIVIDUAL_ALIAS><QUALITY>Good</QUALITY><ALIAS_NAME>QUILLBY THE ELDER</ALIAS_NAME></INDIVIDUAL_ALIAS>
      <INDIVIDUAL_ADDRESS><COUNTRY>Yland</COUNTRY></INDIVIDUAL_ADDRESS>
      <INDIVIDUAL_DATE_OF_BIRTH><TYPE_OF_DATE>EXACT</TYPE_OF_DATE><YEAR>1960</YEAR></INDIVIDUAL_DATE_OF_BIRTH>
    </INDIVIDUAL>
  </INDIVIDUALS>
  <ENTITIES>
    <ENTITY>
      <DATAID>2</DATAID>
      <FIRST_NAME>ZEDLAND MINING</FIRST_NAME>
      <UN_LIST_TYPE>ZED</UN_LIST_TYPE>
      <REFERENCE_NUMBER>ZEe.001</REFERENCE_NUMBER>
      <ENTITY_ALIAS><QUALITY>a.k.a.</QUALITY><ALIAS_NAME>ZM GROUP</ALIAS_NAME></ENTITY_ALIAS>
      <ENTITY_ADDRESS><COUNTRY>Zedland</COUNTRY></ENTITY_ADDRESS>
    </ENTITY>
  </ENTITIES>
</CONSOLIDATED_LIST>"""

let private ofacText = ListText.OfacSdn(sdn, ValueSome alt, ValueSome add, dated)

let private loaded text =
    match ListFiles.load ListMinimums.anyEntries text with
    | Ok list -> list
    | Error failure -> failwith failure.Reason

let private expectLoaded (result: Result<LoadedList, UnreadableList>) =
    result |> Result.defaultWith (fun failure -> failwith failure.Reason)

let private person name : NameToScreen =
    {
        Name = name
        EntityKey = ValueSome $"NAME:{name}"
        Party = PartyKind.Individual
        BornIn = ValueNone
        Countries = []
    }

let private company name =
    { person name with
        Party = PartyKind.LegalEntity
    }

[<Fact>]
let ``OFAC's SDN file is read with aliases, birth years and countries`` () =
    let list = loaded ofacText
    Assert.Equal(3, list.Entries.Length)
    Assert.Equal(ValueNone, list.StatedInFile)
    Assert.Empty list.Notes
    let firm = list.Entries |> List.find (fun entry -> entry.EntryId = "9001")
    Assert.Equal(ListedParty.Entity, firm.Party)
    Assert.Equal<string list>([ "ZEDLAND TRADING WORKS, LTD."; "ZEDLAND TRADE WORKS" ], firm.Names)
    Assert.Equal<string list>([ "Zedland" ], firm.Countries)
    let individual = list.Entries |> List.find (fun entry -> entry.EntryId = "9002")
    Assert.Equal(ListedParty.Individual, individual.Party)
    Assert.Equal("Ansel Rupert QUILLBY", individual.Names.Head)
    Assert.Equal("Ansell QUILBY", individual.Names[1])
    Assert.Equal<int list>([ 1960 ], individual.BirthYears)
    Assert.Equal<string list>([ "Zedland" ], individual.Countries)

    Assert.Equal(
        ListedParty.Other(ValueSome "vessel"),
        (list.Entries |> List.find (fun entry -> entry.EntryId = "9003")).Party
    )

[<Fact>]
let ``an OFAC file without aliases says so`` () =
    let list = loaded (ListText.OfacConsolidated(sdn, ValueNone, ValueNone, dated))
    Assert.Equal(SanctionsListName.OfacConsolidated, list.List)
    Assert.Equal(2, list.Notes.Length)
    Assert.Contains("aliases", list.Notes.Head)

[<Fact>]
let ``the UK file is read by its header, one entry per id, with its report date`` () =
    let list = loaded (ListText.Uk(uk, dated))
    Assert.Equal(ValueSome(DateOnly(2026, 11, 3)), list.StatedInFile)
    Assert.Equal(dated, list.Dated)
    Assert.Equal(2, list.Entries.Length)
    let firm = list.Entries |> List.find (fun entry -> entry.EntryId = "ZED0001")
    Assert.Equal<string list>([ "ZEDLAND MINING"; "ZED MINING" ], firm.Names)
    Assert.Equal(ListedParty.Entity, firm.Party)
    let individual = list.Entries |> List.find (fun entry -> entry.EntryId = "ZED0002")
    Assert.Equal("Ansel QUILLBY", individual.Names.Head)
    Assert.Equal<int list>([ 1960 ], individual.BirthYears)
    Assert.Equal<string list>([ "Zedland" ], individual.Countries)

[<Fact>]
let ``the UN file is read, with its generated date`` () =
    let list = loaded (ListText.Un(un, dated))
    Assert.Equal(ValueSome(DateOnly(2026, 11, 6)), list.StatedInFile)
    Assert.Equal(2, list.Entries.Length)
    let individual = list.Entries |> List.find (fun entry -> entry.EntryId = "ZEi.001")
    Assert.Equal<string list>([ "ANSEL QUILLBY"; "QUILLBY THE ELDER" ], individual.Names)
    Assert.Equal<int list>([ 1960 ], individual.BirthYears)
    Assert.Equal<string list>([ "Zedland"; "Yland" ], individual.Countries)
    let entity = list.Entries |> List.find (fun entry -> entry.EntryId = "ZEe.001")
    Assert.Equal(ListedParty.Entity, entity.Party)
    Assert.Equal<string list>([ "ZEDLAND MINING"; "ZM GROUP" ], entity.Names)

// A file with the wrong shape is unreadable as a whole, because part of a list must not pass for all of it.

let private expectUnreadable text =
    match ListFiles.load ListMinimums.anyEntries text with
    | Error failure -> Assert.NotEmpty failure.Reason
    | Ok list -> failwith $"Read {list.Entries.Length} entries"

[<Fact>]
let ``an OFAC row with the wrong columns refuses the whole file`` () =
    expectUnreadable (ListText.OfacSdn(sdn + "\r\n9004,\"SHORT\"\r\n", ValueNone, ValueNone, dated))

[<Fact>]
let ``an OFAC row with no id refuses the whole file`` () =
    expectUnreadable (
        ListText.OfacSdn(",\"X\",-0-,-0-,-0-,-0-,-0-,-0-,-0-,-0-,-0-,-0-\r\n", ValueNone, ValueNone, dated)
    )

[<Fact>]
let ``an OFAC row with no name refuses the whole file`` () =
    expectUnreadable (
        ListText.OfacSdn("9,-0-,-0-,-0-,-0-,-0-,-0-,-0-,-0-,-0-,-0-,-0-\r\n", ValueNone, ValueNone, dated)
    )

[<Fact>]
let ``a bad alias file refuses the list, not just its aliases`` () =
    expectUnreadable (ListText.OfacSdn(sdn, ValueSome "9001,501\r\n", ValueNone, dated))

[<Fact>]
let ``a file cut off inside a quoted field refuses the whole file`` () =
    expectUnreadable (ListText.OfacSdn(sdn.Substring(0, 20), ValueNone, ValueNone, dated))

    // Every column present, so that only the open quote can tell the file was cut.
    expectUnreadable (
        ListText.OfacSdn("9,\"X\",-0-,-0-,-0-,-0-,-0-,-0-,-0-,-0-,-0-,\"cut off her", ValueNone, ValueNone, dated)
    )

[<Theory; InlineData(""); InlineData("\u001a")>]
let ``an empty list is unreadable, never an empty result`` (text: string) =
    expectUnreadable (ListText.OfacSdn(text, ValueNone, ValueNone, dated))

[<Fact>]
let ``a UK file with no report date refuses the whole file`` () =
    expectUnreadable (ListText.Uk(uk.Substring(uk.IndexOf '\n' + 1), dated))

[<Fact>]
let ``a UN file declaring a DTD is refused, whatever else it holds`` () =
    let withDoctype =
        un.Replace("<CONSOLIDATED_LIST ", "<!DOCTYPE CONSOLIDATED_LIST [<!ENTITY x \"y\">]><CONSOLIDATED_LIST ")

    Assert.NotEqual<string>(un, withDoctype)
    expectUnreadable (ListText.Un(withDoctype, dated))

[<Fact>]
let ``a UK file missing a column refuses the whole file`` () =
    expectUnreadable (ListText.Uk(uk.Replace("Name 6,", "Family name,"), dated))

[<Fact>]
let ``a UK row with the wrong columns refuses the whole file`` () =
    expectUnreadable (ListText.Uk(uk + "\nZED0003,too,few", dated))

[<Fact>]
let ``a UK file of a header only is unreadable`` () =
    expectUnreadable (ListText.Uk($"Report Date: 03-Nov-2026\n{ukHeader}\n", dated))

[<Theory>]
[<InlineData("not xml at all")>]
[<InlineData("<OTHER_ROOT/>")>]
[<InlineData("<CONSOLIDATED_LIST><INDIVIDUALS/></CONSOLIDATED_LIST>")>]
[<InlineData("<CONSOLIDATED_LIST><INDIVIDUALS/><ENTITIES/></CONSOLIDATED_LIST>")>]
[<InlineData("<CONSOLIDATED_LIST><INDIVIDUALS><INDIVIDUAL><DATAID>1</DATAID></INDIVIDUAL></INDIVIDUALS><ENTITIES/></CONSOLIDATED_LIST>")>]
[<InlineData("<!DOCTYPE foo><CONSOLIDATED_LIST><INDIVIDUALS/><ENTITIES/></CONSOLIDATED_LIST>")>]
let ``a UN file of the wrong shape is unreadable`` (text: string) =
    expectUnreadable (ListText.Un(text, dated))

// Matching.

[<Theory>]
[<InlineData("Müller & Söhne GmbH", "MULLER SOHNE GMBH")>]
[<InlineData("  a.b.  c ", "AB C")>]
[<InlineData("BP P.L.C.", "BP PLC")>]
[<InlineData("O'Neil-Smith", "ONEIL SMITH")>]
[<InlineData("Straße", "STRASSE")>]
[<InlineData("Åkerström", "AKERSTROM")>]
[<InlineData(null, "")>]
let ``a name is normalised for comparing`` (name: string, expected: string) =
    Assert.Equal(expected, NameMatching.normalise name)

[<Theory>]
[<InlineData("Zedland Mining Ltd", "ZEDLAND MINING LIMITED")>]
[<InlineData("QUILLBY, Ansel", "Ansel Quillby")>]
[<InlineData("Müller Hans", "Hans MULLER")>]
let ``the same words in any order and legal form score one`` (first: string, second: string) =
    match NameMatching.compare first second with
    | ValueSome(score, reason) ->
        Assert.Equal(1., score)
        Assert.Contains("same words", reason)
    | ValueNone -> failwith "No match"

[<Fact>]
let ``a misspelling is raised, with its score in the reason`` () =
    match NameMatching.compare "Ansel Quilby" "Ansel Quillby" with
    | ValueSome(score, reason) ->
        Assert.True(score < 1. && score >= NameMatching.SpellingThreshold)
        Assert.Contains("Jaro-Winkler", reason)
    | ValueNone -> failwith "No match"

[<Fact>]
let ``the shorter name's words inside the longer one are raised`` () =
    match NameMatching.compare "Ansel Quillby" "Ansel Rupert Quillby" with
    | ValueSome(score, reason) ->
        Assert.Equal(0.9, score)
        Assert.Contains("Every word", reason)
    | ValueNone -> failwith "No match"

[<Theory>]
[<InlineData("Trading Company", "Zedland Trading Company")>]
[<InlineData("Harold Wren", "Margaret Finch")>]
[<InlineData("General Trading Company", "Other General Trading Company")>]
[<InlineData("Acme Trading", "Acs Trading")>]
[<InlineData("Zedland Trading", "Zedland Mining")>]
[<InlineData("Zedland", "Yland")>]
let ``generic words and different names aren't raised`` (first: string, second: string) =
    Assert.Equal(ValueNone, NameMatching.compare first second)

[<Fact>]
let ``Jaro-Winkler gives the textbook value`` () =
    Assert.Equal(0.961, Math.Round(NameMatching.similarity "MARTHA" "MARHTA", 3))
    Assert.Equal(0.84, Math.Round(NameMatching.similarity "DWAYNE" "DUANE", 2))
    Assert.Equal(1., NameMatching.similarity "SAME" "SAME")
    Assert.Equal(0., NameMatching.similarity "" "X")

// Screening.

let private at = DateTimeOffset(2026, 11, 6, 12, 0, 0, TimeSpan.Zero)

let private allLists =
    SanctionsScreening.load
        ListMinimums.anyEntries
        [
            ofacText
            ListText.OfacConsolidated(sdn, ValueSome alt, ValueSome add, dated)
            ListText.Uk(uk, dated)
            ListText.Un(un, dated)
        ]

[<Fact>]
let ``a screening names its lists and dates, and a possible match gives its reason`` () =
    let result = SanctionsScreening.screen allLists [ person "Ansel Quillby" ] [] at
    Assert.Equal(4, result.ListsRead.Length)
    Assert.Contains(result.ListsRead, (fun list -> list.List = SanctionsListName.UkFcdo && list.Dated = dated))
    Assert.Equal<string list>([ "Ansel Quillby" ], result.NamesScreened)
    Assert.NotEmpty result.PossibleMatches

    for found in result.PossibleMatches do
        Assert.NotEmpty found.ScoreReason
        Assert.Equal(dated, found.ListDated)
        Assert.Equal("Ansel Quillby", found.ScreenedName)

[<Fact>]
let ``date of birth and country are compared only when both sides have them`` () =
    let lists = [ Ok(loaded ofacText) ]
    let bare = SanctionsScreening.screen lists [ person "Ansel Rupert Quillby" ] [] at
    Assert.Equal(DetailComparison.NotBothKnown, bare.PossibleMatches.Head.BirthYear)
    Assert.Equal(DetailComparison.NotBothKnown, bare.PossibleMatches.Head.Country)

    let same =
        SanctionsScreening.screen
            lists
            [
                { person "Ansel Rupert Quillby" with
                    BornIn = ValueSome { Year = 1960; Month = 1 }
                    Countries = [ "Zedland" ]
                }
            ]
            []
            at

    Assert.Equal(DetailComparison.BothSidesMatch "1960", same.PossibleMatches.Head.BirthYear)
    Assert.Equal(DetailComparison.BothSidesMatch "ZEDLAND", same.PossibleMatches.Head.Country)

    let other =
        SanctionsScreening.screen
            lists
            [
                { person "Ansel Rupert Quillby" with
                    BornIn = ValueSome { Year = 1991; Month = 2 }
                    Countries = [ "Yland" ]
                }
            ]
            []
            at

    Assert.Equal(DetailComparison.BothSidesDiffer("1991", "1960"), other.PossibleMatches.Head.BirthYear)
    Assert.Equal(DetailComparison.BothSidesDiffer("YLAND", "ZEDLAND"), other.PossibleMatches.Head.Country)
    Assert.NotEmpty other.PossibleMatches

[<Fact>]
let ``a person isn't compared with a listed company, nor a company with a listed person`` () =
    let lists = [ Ok(loaded (ListText.Uk(uk, dated))) ]
    Assert.Empty (SanctionsScreening.screen lists [ person "Zedland Mining" ] [] at).PossibleMatches
    Assert.Empty (SanctionsScreening.screen lists [ company "Ansel Quillby" ] [] at).PossibleMatches

    let unknown =
        { person "Zedland Mining" with
            Party = PartyKind.Unknown ValueNone
        }

    Assert.NotEmpty (SanctionsScreening.screen lists [ unknown ] [] at).PossibleMatches

[<Fact>]
let ``no possible match states the same lists, and nothing that reads as cleared`` () =
    let result =
        SanctionsScreening.screen allLists [ company "Harbour Finch Consulting" ] [] at

    Assert.Empty result.PossibleMatches
    Assert.Equal(4, result.ListsRead.Length)
    Assert.Equal<string list>([ "Harbour Finch Consulting" ], result.NamesScreened)
    let printed = $"%A{result}".ToLowerInvariant()
    Assert.DoesNotContain("not sanctioned", printed)
    Assert.DoesNotContain("clear", printed)

[<Fact>]
let ``the EU list is always stated as not used`` () =
    let result = SanctionsScreening.screen allLists [ person "X Y" ] [] at

    let eu =
        result.ListsNotUsed
        |> List.find (fun list -> list.List = SanctionsListName.EuConsolidated)

    Assert.Contains("403", eu.Reason)
    Assert.NotEmpty eu.WouldClose

[<Fact>]
let ``a list not supplied or unreadable is stated, and never counts as read`` () =
    let lists =
        SanctionsScreening.load ListMinimums.anyEntries [ ofacText; ListText.Un("broken", dated) ]

    let result = SanctionsScreening.screen lists [ person "X Y" ] [] at
    Assert.Equal<SanctionsListName list>([ SanctionsListName.OfacSdn ], result.ListsRead |> List.map _.List)

    let notUsed = result.ListsNotUsed |> List.map _.List
    Assert.Contains(SanctionsListName.UnConsolidated, notUsed)
    Assert.Contains(SanctionsListName.UkFcdo, notUsed)
    Assert.Contains(SanctionsListName.OfacConsolidated, notUsed)

[<Fact>]
let ``names not screened are stated beside the result, and a blank name is one`` () =
    let result =
        SanctionsScreening.screen
            allLists
            [ person "Ltd"; person "Ansel Quillby" ]
            [
                {
                    Who = "owners of ZEDCO"
                    Reason = "Not identified"
                }
            ]
            at

    Assert.Equal<string list>([ "Ansel Quillby" ], result.NamesScreened)
    Assert.Equal<string list>([ "owners of ZEDCO"; "Ltd" ], result.NamesNotScreened |> List.map _.Who)

[<Fact>]
let ``the best-scoring name of an entry is the one given, and possible matches sort by score`` () =
    let lists = [ Ok(loaded ofacText) ]

    let result =
        SanctionsScreening.screen lists [ company "Zedland Trade"; person "Ansell Quilby" ] [] at

    Assert.Equal<string list>(
        [ "Ansell QUILBY"; "ZEDLAND TRADE WORKS" ],
        result.PossibleMatches |> List.map _.ListedName
    )

    Assert.Equal<float list>([ 1.; 0.9 ], result.PossibleMatches |> List.map _.Score)

// A list cut short must not pass for the whole of it.

[<Fact>]
let ``an OFAC file without its Ctrl-Z, cut at a row boundary, is unreadable`` () =
    let cut = sdn.Replace("\u001a", "")
    expectUnreadable (ListText.OfacSdn(cut, ValueSome alt, ValueSome add, dated))
    expectUnreadable (ListText.OfacConsolidated(cut, ValueNone, ValueNone, dated))

[<Fact>]
let ``an OFAC alias or address file without its Ctrl-Z is unreadable`` () =
    expectUnreadable (ListText.OfacSdn(sdn, ValueSome(alt.Replace("\u001a", "")), ValueSome add, dated))
    expectUnreadable (ListText.OfacSdn(sdn, ValueSome alt, ValueSome(add.Replace("\u001a", "")), dated))

[<Fact>]
let ``a Ctrl-Z before the end of an OFAC file is unreadable, and blank lines after it are read`` () =
    expectUnreadable (
        ListText.OfacSdn(
            sdn
            + "\r\n9004,\"LATE\",-0- ,\"ZED\",-0- ,-0- ,-0- ,-0- ,-0- ,-0- ,-0- ,-0- \r\n\u001a",
            ValueNone,
            ValueNone,
            dated
        )
    )

    Assert.Equal(3, (loaded (ListText.OfacSdn(sdn + "\r\n\r\n", ValueNone, ValueNone, dated))).Entries.Length)

[<Theory; InlineData(""); InlineData("  \r\n"); InlineData("\u001a")>]
let ``blank alias or address text is not supplied, and says so`` (blank: string) =
    let read = loaded (ListText.OfacSdn(sdn, ValueSome blank, ValueSome blank, dated))
    Assert.Contains(read.Notes, (fun note -> note.Contains "alias file"))
    Assert.Contains(read.Notes, (fun note -> note.Contains "address file"))

let private loadWith minimums text = ListFiles.load minimums text

let private belowMinimum result =
    match result with
    | Error(failure: UnreadableList) -> Assert.Contains("below the minimum", failure.Reason)
    | Ok(list: LoadedList) -> failwith $"Read {list.Entries.Length} entries"

[<Fact>]
let ``a UK file with no end marker is held to its minimum`` () =
    let any = ListMinimums.anyEntries
    Assert.Equal(2, (expectLoaded (loadWith { any with Uk = 2 } (ListText.Uk(uk, dated)))).Entries.Length)
    belowMinimum (loadWith { any with Uk = 3 } (ListText.Uk(uk, dated)))

[<Fact>]
let ``every list is held to its own minimum`` () =
    let any = ListMinimums.anyEntries
    belowMinimum (loadWith { any with OfacSdn = 4 } ofacText)

    belowMinimum (
        loadWith { any with OfacConsolidated = 4 } (ListText.OfacConsolidated(sdn, ValueNone, ValueNone, dated))
    )

    belowMinimum (loadWith { any with Un = 3 } (ListText.Un(un, dated)))
    expectLoaded (loadWith { any with OfacSdn = 3 } ofacText) |> ignore
    expectLoaded (loadWith { any with Un = 2 } (ListText.Un(un, dated))) |> ignore

/// The entries the README states for each list, as read from the files of 2026-11-06.
[<Theory>]
[<InlineData("sdn", 19555)>]
[<InlineData("cons", 486)>]
[<InlineData("uk", 6420)>]
[<InlineData("un", 1007)>]
let ``the default minimum lets a list shrink by a fifth of its measured size`` (list: string, measured: int) =
    let defaults = ListMinimums.defaults

    let minimum =
        match list with
        | "sdn" -> defaults.OfacSdn
        | "cons" -> defaults.OfacConsolidated
        | "uk" -> defaults.Uk
        | _ -> defaults.Un

    Assert.Equal(measured * 4 / 5, minimum)

[<Fact>]
let ``the defaults refuse a list far below its usual size`` () =
    belowMinimum (loadWith ListMinimums.defaults (ListText.Uk(uk, dated)))
    belowMinimum (loadWith ListMinimums.defaults ofacText)

// Titles and a single word.

[<Theory>]
[<InlineData("Mr Ansel Quillby")>]
[<InlineData("Dr. Ansel Quillby")>]
[<InlineData("Prof Dr Ansel Quillby")>]
[<InlineData("QUILLBY, Ansel, Dr")>]
[<InlineData("Ansel Quillby")>]
let ``a title is no part of a name`` (name: string) =
    match NameMatching.compare name "Ansel Rupert Quillby" with
    | ValueSome(score, _) -> Assert.Equal(0.9, score)
    | ValueNone -> failwith "No match"

[<Fact>]
let ``a name of titles alone keeps them`` () =
    Assert.Equal<string list>([ "SIR" ], NameMatching.tokens "Sir")

[<Theory>]
[<InlineData("Zedneft", "Zedneft Oil Company")>]
[<InlineData("Zedneft Ltd", "Open Joint Stock Company Zedneft Oil Company")>]
[<InlineData("Zedbank", "Zedbank of Yland")>]
let ``one distinctive word contained in a listed name is a possible match`` (screened: string, listed: string) =
    match NameMatching.compare screened listed with
    | ValueSome(score, reason) ->
        Assert.Equal(0.85, score)
        Assert.Contains("one word", reason)
    | ValueNone -> failwith "No match"

[<Theory>]
[<InlineData("Limited", "Zedland Trading Limited")>]
[<InlineData("Holdings", "Zedland Holdings Limited")>]
[<InlineData("Holdings Ltd", "Zedland Holdings Limited")>]
[<InlineData("Trading", "Zedland Trading Works")>]
[<InlineData("Bank", "Zedland Bank of Yland")>]
[<InlineData("Consulting", "Zedland Consulting Group")>]
[<InlineData("Solutions Ltd", "Zedland Solutions Works")>]
[<InlineData("Zed", "Zedland Works")>]
[<InlineData("Works Yland Capital", "Capital")>]
[<InlineData("ZT Bank", "ZT")>]
[<InlineData("Oil Trading", "Oil Holdings")>]
[<InlineData("Zedal Trading", "Zedul Holding")>]
let ``a word that says nothing, part of a word, or a short word spelled differently isn't raised``
    (screened: string, listed: string)
    =
    Assert.Equal(ValueNone, NameMatching.compare screened listed)

[<Theory>]
[<InlineData("Zedland Works", "Zedland")>]
[<InlineData("Zedneft Yland GmbH", "Zedneft")>]
[<InlineData("Yland Works ZTB", "ZTB")>]
let ``a listed name of one word inside a screened name is a possible match`` (screened: string, listed: string) =
    match NameMatching.compare screened listed with
    | ValueSome(score, reason) ->
        Assert.Equal(0.85, score)
        Assert.Contains("one word of the listed name", reason)
    | ValueNone -> failwith "No match"

[<Theory>]
[<InlineData("ZTB Bank", "ZTB")>]
[<InlineData("Zedal Trading Ltd", "Zedal")>]
[<InlineData("Zedal Group SA", "Zedal Holding")>]
[<InlineData("Zedal", "Zedal Trading")>]
let ``the same distinctive word is raised whatever its length and the generic words beside it``
    (screened: string, listed: string)
    =
    match NameMatching.compare screened listed with
    | ValueSome(score, reason) ->
        Assert.Equal(0.85, score)
        Assert.Contains("is the same", reason)
    | ValueNone -> failwith "No match"

[<Theory>]
[<InlineData("PJSC Zedal")>]
[<InlineData("Zedal OJSC")>]
[<InlineData("CJSC Zedal")>]
[<InlineData("AO Zedal")>]
[<InlineData("NAO Zedal")>]
let ``a joint-stock form is a legal form`` (screened: string) =
    Assert.Equal(ValueSome 1., NameMatching.compare screened "Zedal" |> ValueOption.map fst)

/// A list where every entry holds the words a screening must not flood on, in the shape of the real lists.
let private crowded (entries: int) (words: string list) : Result<LoadedList, UnreadableList> =
    Ok
        {
            List = SanctionsListName.OfacSdn
            Dated = dated
            StatedInFile = ValueNone
            Entries =
                [
                    for index in 1..entries do
                        for word in words do
                            {
                                EntryId = $"{word}{index}"
                                Party = ListedParty.Entity
                                Names = [ $"ZED{index} {word} WORKS" ]
                                Programme = ValueNone
                                BirthYears = []
                                Countries = []
                            }
                ]
            Notes = []
        }

[<Theory>]
[<InlineData("Limited")>]
[<InlineData("Holdings Ltd")>]
[<InlineData("Holdings")>]
[<InlineData("Trading")>]
[<InlineData("Solutions")>]
[<InlineData("Zedergy")>]
let ``common words don't flood a screening, and the name held back says so`` (name: string) =
    let lists =
        [
            crowded (NameMatching.CommonWordEntries + 1) [ "ZEDERGY"; "HOLDINGS"; "TRADING"; "LIMITED"; "SOLUTIONS" ]
        ]

    let result = SanctionsScreening.screen lists [ company name ] [] at
    Assert.Empty result.PossibleMatches
    Assert.Contains(result.NamesNotScreened, (fun held -> held.Who = name))

[<Fact>]
let ``a word held in few enough entries is raised for each of them`` () =
    let lists = [ crowded NameMatching.CommonWordEntries [ "ZEDERGY" ] ]
    let result = SanctionsScreening.screen lists [ company "Zedergy" ] [] at
    Assert.Equal(NameMatching.CommonWordEntries, result.PossibleMatches.Length)
    Assert.Empty result.NamesNotScreened

/// A list of entries each named by one word alone.
let private oneWordEntries (entries: int) (word: string) : Result<LoadedList, UnreadableList> =
    Ok
        {
            List = SanctionsListName.OfacSdn
            Dated = dated
            StatedInFile = ValueNone
            Entries =
                [
                    for index in 1..entries ->
                        {
                            EntryId = $"{word}{index}"
                            Party = ListedParty.Entity
                            Names = [ word ]
                            Programme = ValueNone
                            BirthYears = []
                            Countries = []
                        }
                ]
            Notes = []
        }

[<Fact>]
let ``a listed one-word name common in the lists doesn't flood a longer screened name`` () =
    let name = "Zedergy Works Yland"

    let few =
        SanctionsScreening.screen [ oneWordEntries NameMatching.CommonWordEntries "ZEDERGY" ] [ company name ] [] at

    let many =
        SanctionsScreening.screen
            [ oneWordEntries (NameMatching.CommonWordEntries + 1) "ZEDERGY" ]
            [ company name ]
            []
            at

    Assert.Equal(NameMatching.CommonWordEntries, few.PossibleMatches.Length)
    Assert.Empty many.PossibleMatches

[<Theory>]
[<InlineData("ООО «Зедторг»", "OOO ZEDTORG")>]
[<InlineData("Щербаков Юрий Сергеевич", "SHCHERBAKOV YURIY SERGEEVICH")>]
[<InlineData("Объединение Жёлтый Цех", "OBEDINENIE ZHELTYY TSEKH")>]
[<InlineData("Ґалаґан Євгенія Їжак", "GALAGAN YEVGENIYA YIZHAK")>]
[<InlineData("Ευστάθιος Γεωργίου", "EFSTATHIOS GEORGIOU")>]
[<InlineData("Αυγή Άγγελος Χρήστου", "AVGI ANGELOS CHRISTOU")>]
[<InlineData("Zedland Ελλάς ООО", "ZEDLAND ELLAS OOO")>]
let ``Cyrillic and Greek letters are spelled in Latin`` (name: string, expected: string) =
    Assert.Equal(expected, NameMatching.normalise name)
    Assert.False(NameMatching.holdsOtherScript name)

[<Theory>]
[<InlineData("ООО «Зедланд Мининг»")>]
[<InlineData("Ζεδλανδ Μινινγ")>]
let ``a Cyrillic or Greek name is screened in Latin letters`` (name: string) =
    let result = SanctionsScreening.screen allLists [ company name ] [] at
    Assert.Equal<string list>([ name ], result.NamesScreened)
    Assert.Empty result.NamesNotScreened
    Assert.Contains(result.PossibleMatches, (fun found -> found.ListedName = "ZEDLAND MINING"))

[<Theory>]
[<InlineData("זדלנד כרייה בע״מ")>]
[<InlineData("شركة زدلاند للتعدين")>]
[<InlineData("泽德兰矿业有限公司")>]
[<InlineData("Zedland Mining 株式会社")>]
let ``a name holding another script's letters never counts as screened`` (name: string) =
    let result = SanctionsScreening.screen allLists [ company name ] [] at
    Assert.Empty result.NamesScreened
    Assert.Contains(result.NamesNotScreened, (fun held -> held.Who = name && held.Reason.Contains "script"))

[<Fact>]
let ``the Latin words of a name holding another script are still compared`` () =
    let result =
        SanctionsScreening.screen allLists [ company "Zedland Mining 株式会社" ] [] at

    Assert.Contains(result.PossibleMatches, (fun found -> found.ListedName = "ZEDLAND MINING"))
