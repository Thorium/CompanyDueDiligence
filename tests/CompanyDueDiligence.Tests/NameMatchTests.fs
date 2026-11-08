/// Company name search by match mode: NameMatch's rules, the fallback that narrows a register's own search, and the registers whose query does
/// the matching (Socrata and ArcGIS), with the query sent for each mode, the rows read and the Complete rules.
module NameMatchTests

open System
open System.Threading.Tasks
open Xunit
open CompanyDueDiligence
open Fake

let private fixedTime = DateTimeOffset(2026, 11, 8, 12, 0, 0, TimeSpan.Zero)

let private connectionOver (handler: FakeHandler) =
    SourceConnection(handler, (fun () -> fixedTime), (fun _ _ -> Task.CompletedTask))

/// A connection whose clock a wait moves on, for a search of more calls than the pace allows in one window.
let private movingConnectionOver (handler: FakeHandler) =
    let clock = FakeClock()
    SourceConnection(handler, (fun () -> clock.Now), clock.Delay)

let private sent (handler: FakeHandler) =
    handler.Calls |> List.map (fun call -> Uri.UnescapeDataString call.Target)

let private modes = [ NameMatch.Exact; NameMatch.StartsWith; NameMatch.Contains ]

let private resultOf (answer: Result<Evidence<Availability<SearchResult>>, RegistryFailure>) =
    match answer with
    | Ok { Value = Available result } -> result
    | other -> failwith $"%A{other}"

// NameMatch

[<Fact>]
let ``Exact compares whole names after normalising, ignoring case, accents and punctuation spacing`` () =
    Assert.True(NameMatch.isMatch NameMatch.Exact "degrees group inc" "Degrees Group, Inc.")
    Assert.True(NameMatch.isMatch NameMatch.Exact "CAFE DU MONDE S.A." "Café du Monde SA")
    Assert.True(NameMatch.isMatch NameMatch.Exact "O'Neil  &  Sons" "ONEIL SONS")
    Assert.False(NameMatch.isMatch NameMatch.Exact "degrees group" "Degrees Group, Inc.")
    Assert.False(NameMatch.isMatch NameMatch.Exact "degrees group inc" "Six Degrees Group, Inc.")

[<Fact>]
let ``StartsWith and Contains compare normalised names as text`` () =
    Assert.True(NameMatch.isMatch NameMatch.StartsWith "degrees gr" "Degrees Group, Inc.")
    Assert.False(NameMatch.isMatch NameMatch.StartsWith "degrees" "Six Degrees Group, Inc.")
    Assert.True(NameMatch.isMatch NameMatch.Contains "degrees" "Degrees Group, Inc.")
    Assert.True(NameMatch.isMatch NameMatch.Contains "group inc" "Degrees Group, Inc.")
    Assert.False(NameMatch.isMatch NameMatch.Contains "groupinc" "Degrees Group, Inc.")

[<Fact>]
let ``a name with no letter or digit matches nothing`` () =
    for matching in modes do
        Assert.False(NameMatch.isMatch matching "&&&" "&&&")
        Assert.False(NameMatch.isMatch matching "" "")

[<Fact>]
let ``normalising ASCII text gives what the Unicode path gives`` () =
    let random = Random 20261108
    let printable = [| for c in ' ' .. '~' -> c |]

    for _ in 1..2000 do
        let text =
            String(Array.init (random.Next(0, 24)) (fun _ -> printable[random.Next printable.Length]))

        // A trailing é sends the text down the Unicode path, which ends it with E.
        let unicode = NameMatching.normalise (text + " é")
        let ascii = NameMatching.normalise text
        let expected = if ascii = "" then "E" else ascii + " E"
        Assert.Equal(expected, unicode)

    Assert.Equal("PLC", NameMatching.normalise "P.L.C.")
    Assert.Equal("ONEIL SONS LTD", NameMatching.normalise "  O'Neil & Sons,   Ltd. ")
    Assert.Equal("", NameMatching.normalise " .' ")

[<Fact>]
let ``comparing ASCII text as it is read answers as comparing its normalised form`` () =
    let random = Random 8112026
    // Few letters, so that names and terms often agree.
    let alphabet = "ab AB.'-,&1"

    let draw length =
        String(Array.init length (fun _ -> alphabet[random.Next alphabet.Length]))

    for _ in 1..20000 do
        let text = draw (random.Next(0, 10))
        let normal = NameMatching.normalise (draw (random.Next(1, 6)))

        if normal <> "" then
            let normalised = NameMatching.normalise text
            Assert.Equal((normalised = normal), NameMatching.asciiNormalMatches true normal text)

            Assert.Equal(
                normalised.StartsWith(normal, StringComparison.Ordinal),
                NameMatching.asciiNormalMatches false normal text
            )

// The fallback

/// A register without a name match of its own, answering every search with page and noting each term asked.
type private PagedRegistry(answer: Availability<SearchResult>) =
    let asked = Collections.Concurrent.ConcurrentQueue<string>()
    let zz = Jurisdiction.ofCode "ZZ"

    let coverage: RegistryCoverage =
        {
            Registry = "Zedland Register"
            Jurisdictions = [ zz ]
            Standing = Support.Supported
            Concerns = Support.NotOffered "None"
            Addresses = Support.NotOffered "None"
            Search = Support.Supported
            Officers = Support.NotOffered "None"
            Owners = Support.NotOffered "None"
            Cost = AccessCost.Free
            RateLimit = RateLimitPolicy.NotPublished(1, TimeSpan.FromSeconds 1.)
            Licence = "None stated"
        }

    let evidence value : Evidence<_> =
        {
            Source = coverage.Registry
            RetrievedAt = fixedTime
            AsOf = ValueNone
            Raw = ValueSome "the page"
            Value = value
        }

    let unasked jurisdiction =
        if RegistryCoverage.covers coverage jurisdiction then
            Task.FromResult(Ok(evidence (NotAvailable "Not asked")))
        else
            Task.FromResult(Error(RegistryFailure.OutsideCoverage jurisdiction))

    member _.Asked = List.ofSeq asked

    interface ICompanyRegistry with
        member _.Coverage = coverage
        member _.TryFindAsync(_, jurisdiction, _) = unasked jurisdiction

        member _.SearchAsync(name, jurisdiction, _) =
            if RegistryCoverage.covers coverage jurisdiction then
                asked.Enqueue name
                Task.FromResult(Ok(evidence answer))
            else
                Task.FromResult(Error(RegistryFailure.OutsideCoverage jurisdiction))

        member _.OfficersAsync(_, jurisdiction, _) = unasked jurisdiction
        member _.OwnersAsync(_, jurisdiction, _, _) = unasked jurisdiction

let private zz = Jurisdiction.ofCode "ZZ"

let private matchNamed (name: string) : CompanyMatch =
    {
        Name = name
        Number = name.GetHashCode().ToString()
        StatusAsPublished = ValueNone
        Address = ValueNone
    }

let private page =
    {
        Matches =
            [
                "Degrees Group, Inc."
                "DEGREES GROUP INC"
                "Degrees Group Holdings, Inc."
                "The Degrees Group, Inc."
            ]
            |> List.map matchNamed
        TotalMatches = ValueSome 4
        Complete = true
    }

[<Fact>]
let ``the fallback narrows a register's page to exact names, never complete`` () : Task =
    task {
        let registry = PagedRegistry(Available page)

        let! answer =
            Registries.searchByNameAsync registry "degrees group inc" NameMatch.Exact zz ct

        let result = resultOf answer

        Assert.Equal<string list>([ "Degrees Group, Inc."; "DEGREES GROUP INC" ], result.Matches |> List.map _.Name)

        Assert.False result.Complete
        Assert.Equal(ValueNone, result.TotalMatches)
        Assert.Equal<string list>([ "degrees group inc" ], registry.Asked)

        match answer with
        | Ok evidence -> Assert.Equal(ValueSome "the page", evidence.Raw)
        | Error failure -> failwith $"%A{failure}"
    }

[<Fact>]
let ``the fallback narrows a register's page to names starting with the term, never complete`` () : Task =
    task {
        let registry = PagedRegistry(Available page)

        let! answer =
            Registries.searchByNameAsync registry "DEGREES GROUP" NameMatch.StartsWith zz ct

        let result = resultOf answer

        Assert.Equal<string list>(
            [ "Degrees Group, Inc."; "DEGREES GROUP INC"; "Degrees Group Holdings, Inc." ],
            result.Matches |> List.map _.Name
        )

        Assert.False result.Complete
    }

[<Fact>]
let ``the fallback gives Contains the register's own answer as it is`` () : Task =
    task {
        let registry = PagedRegistry(Available page)
        let! answer = Registries.searchByNameAsync registry "zzz" NameMatch.Contains zz ct
        Assert.Equal(page, resultOf answer)
    }

[<Fact>]
let ``the fallback passes on an answer that isn't Available, and a failure`` () : Task =
    task {
        let registry = PagedRegistry(NotAvailable "No search")

        for matching in modes do
            match! Registries.searchByNameAsync registry "x" matching zz ct with
            | Ok { Value = NotAvailable "No search" } -> ()
            | other -> failwith $"%A{other}"

            let elsewhere = Jurisdiction.ofCode "GB"
            let! outside = Registries.searchByNameAsync registry "x" matching elsewhere ct
            Assert.Equal(Error(RegistryFailure.OutsideCoverage elsewhere), outside)
    }

[<Fact>]
let ``an exact or starts-with name with no letter or digit isn't sent`` () : Task =
    task {
        let registry = PagedRegistry(Available page)

        for matching in [ NameMatch.Exact; NameMatch.StartsWith ] do
            match! Registries.searchByNameAsync registry "&& !" matching zz ct with
            | Error(RegistryFailure.InvalidRequest _) -> ()
            | other -> failwith $"%A{other}"

        Assert.Empty registry.Asked
        let! contains = Registries.searchByNameAsync registry "&& !" NameMatch.Contains zz ct
        Assert.Equal(page, resultOf contains)
    }

// Socrata

let private co = Jurisdiction.ofCode "US-CO"
let private ctState = Jurisdiction.ofCode "US-CT"
let private ny = Jurisdiction.ofCode "US-NY"
let private oregon = Jurisdiction.ofCode "US-OR"
let private pa = Jurisdiction.ofCode "US-PA"
let private tx = Jurisdiction.ofCode "US-TX"
let private colombia = Jurisdiction.ofCode "CO"
let private dc = Jurisdiction.ofCode "US-DC"

/// One row every Socrata adapter here reads, under each name field it searches.
let private socrataRow (index: int) (name: string) =
    let quoted = Text.Json.JsonSerializer.Serialize name
    let number = $"{index + 1}"

    $$"""{"entityid":"{{number}}","entityname":{{quoted}},"accountnumber":"{{number}}","name":{{quoted}},"dos_id":"{{number}}","current_entity_name":{{quoted}},"corpid_num":"{{number}}","corp_name":{{quoted}},"registry_number":"{{number}}","business_name":{{quoted}},"filing_number":"{{number}}","taxpayer_number":"{{number}}","taxpayer_name":{{quoted}},"razon_social":{{quoted}},"nit":"899999068"}"""

let private socrataRows (names: string list) =
    "[" + (names |> List.mapi socrataRow |> String.concat ",") + "]"

/// The names a query for O'Neil & Sons, Inc. answers, of which the first two are the whole name.
let private oneilNames =
    [ "O'NEIL & SONS, INC."; "ONEIL SONS INC"; "O'NEIL & SONS HOLDINGS, INC." ]

/// A Socrata register: its name, how to build it, its jurisdiction, the column each of its queries compares, and whether a search can be
/// Complete.
type private SocrataCase =
    {
        Label: string
        Build: SourceConnection -> ICompanyRegistry
        Jurisdiction: Jurisdiction
        Columns: string list
        CanComplete: bool
    }

let private socrataCases =
    [
        {
            Label = "Colorado"
            Build = Sources.colorado
            Jurisdiction = co
            Columns = [ "upper(entityname)" ]
            CanComplete = true
        }
        {
            Label = "Connecticut"
            Build = Sources.connecticut
            Jurisdiction = ctState
            Columns = [ "upper(name)" ]
            CanComplete = true
        }
        {
            Label = "New York"
            Build = Sources.newYork
            Jurisdiction = ny
            Columns = [ "upper(current_entity_name)"; "upper(corp_name)" ]
            CanComplete = false
        }
        {
            Label = "Oregon"
            Build = Sources.oregon
            Jurisdiction = oregon
            Columns = [ "upper(business_name)" ]
            CanComplete = false
        }
        {
            Label = "Pennsylvania"
            Build = Sources.pennsylvania
            Jurisdiction = pa
            Columns = [ "upper(business_name)" ]
            CanComplete = false
        }
        {
            Label = "Texas"
            Build = Sources.texas
            Jurisdiction = tx
            Columns = [ "taxpayer_name" ]
            CanComplete = false
        }
        {
            Label = "Colombia"
            Build = Sources.colombia
            Jurisdiction = colombia
            Columns = [ "upper(razon_social)" ]
            CanComplete = true
        }
    ]

/// The condition each mode sends for O'Neil & Sons, Inc. on column.
let private oneilCondition (column: string) (matching: NameMatch) =
    match matching with
    | NameMatch.Exact -> $"({column} = 'O''NEIL & SONS, INC.' OR {column} like '%%O%%NEIL%%S%%ON%%S%%I%%N%%C%%')"
    | NameMatch.StartsWith -> $"{column} like '%%O%%NEIL%%S%%ON%%S%%I%%N%%C%%'"
    | NameMatch.Contains -> $"{column} like '%%O%%NEIL%%S%%ON%%S%%I%%N%%C%%'"

let private searchCase (case: SocrataCase) (answer: string) (name: string) (matching: NameMatch) =
    task {
        use handler = new FakeHandler(fun _ -> ok answer)
        let registry = case.Build(movingConnectionOver handler)
        let! found = Registries.searchByNameAsync registry name matching case.Jurisdiction ct
        return found, sent handler
    }

[<Fact>]
let ``every Socrata register sends each mode's condition, escaped, and the same as SearchAsync for StartsWith``
    ()
    : Task =
    task {
        for case in socrataCases do
            Assert.True(
                (case.Build(connectionOver (new FakeHandler(fun _ -> ok "[]")))) :? INameMatchSearch,
                case.Label
            )

            for matching in modes do
                let! _, queries = searchCase case "[]" "O'Neil & Sons, Inc." matching

                Assert.Equal(case.Columns.Length, queries.Length)

                for column, query in List.zip case.Columns queries do
                    let condition = oneilCondition column matching

                    if not (query.Contains condition) then
                        failwith $"{case.Label} {matching}: {query} lacks {condition}"

            use handler = new FakeHandler(fun _ -> ok "[]")

            let! _ =
                (case.Build(movingConnectionOver handler)).SearchAsync("O'Neil & Sons, Inc.", case.Jurisdiction, ct)

            let! _, startsWith = searchCase case "[]" "O'Neil & Sons, Inc." NameMatch.StartsWith
            Assert.Equal<string list>(startsWith, sent handler)
    }

[<Fact>]
let ``a Socrata contains search takes out what would widen the pattern`` () : Task =
    task {
        let! _, queries = searchCase socrataCases.Head "[]" " 50%_off " NameMatch.Contains
        Assert.Contains("upper(entityname) like '%5%0%O%F%F%'", queries.Head)
    }

[<Fact>]
let ``Colombia's name condition keeps to companies`` () : Task =
    task {
        let case = socrataCases |> List.find (fun case -> case.Label = "Colombia")

        for matching in modes do
            let! _, queries = searchCase case "[]" "O'Neil & Sons, Inc." matching

            Assert.Contains(
                "codigo_categoria_matricula = '01' AND "
                + oneilCondition "upper(razon_social)" matching,
                queries.Head
            )
    }

[<Fact>]
let ``every Socrata register keeps only the names matching in each mode`` () : Task =
    task {
        for case in socrataCases do
            for matching in modes do
                let! found, _ = searchCase case (socrataRows oneilNames) "o'neil & sons inc" matching
                let names = (resultOf found).Matches |> List.map _.Name

                // The third, O'NEIL & SONS HOLDINGS, INC., neither starts with nor contains ONEIL SONS INC once normalised.
                let expected = List.take 2 oneilNames

                Assert.True((expected = names), $"{case.Label} {matching}: %A{names}")
    }

[<Fact>]
let ``a Socrata search is complete only below its limit, and never for a register whose data leaves companies out``
    ()
    : Task =
    task {
        let full =
            socrataRows (
                List.init 50 (fun index ->
                    if index = 0 then
                        "ONEIL SONS INC"
                    else
                        $"ONEIL SONS INC {index}")
            )

        for case in socrataCases do
            for matching in modes do
                let! short, _ =
                    searchCase case (socrataRows oneilNames) "O'Neil & Sons, Inc." matching

                Assert.True(((resultOf short).Complete = case.CanComplete), $"{case.Label} {matching} short")

                let! long, _ = searchCase case full "O'Neil & Sons, Inc." matching
                let result = resultOf long
                Assert.False(result.Complete, $"{case.Label} {matching} full")

                if matching = NameMatch.Exact then
                    Assert.Equal<string list>([ "ONEIL SONS INC" ], result.Matches |> List.map _.Name)
    }

[<Fact>]
let ``a Socrata search refuses an exact name with no letter or digit, and a blank one, without sending`` () : Task =
    task {
        for case in socrataCases do
            use handler = new FakeHandler(fun _ -> ok "[]")
            let registry = case.Build(movingConnectionOver handler) :?> INameMatchSearch

            for name, matching in
                [
                    "&& !", NameMatch.Exact
                    "&& !", NameMatch.StartsWith
                    " ", NameMatch.Contains
                ] do
                match! registry.SearchByNameAsync(name, matching, case.Jurisdiction, ct) with
                | Error(RegistryFailure.InvalidRequest _) -> ()
                | other -> failwith $"{case.Label}: %A{other}"

            let elsewhere = Jurisdiction.ofCode "GB"
            let! outside = registry.SearchByNameAsync("x", NameMatch.Exact, elsewhere, ct)
            Assert.Equal(Error(RegistryFailure.OutsideCoverage elsewhere), outside)
            Assert.Empty handler.Calls
    }

// ArcGIS

let private arcgisAnswer (truncated: bool) (names: string list) =
    let features =
        names
        |> List.mapi (fun index name ->
            let quoted = Text.Json.JsonSerializer.Serialize name

            """{"attributes":{"BUSINESS_NAME":"""
            + quoted
            + ""","FILE_NUMBER":"C0000"""
            + string index
            + "\"}}")
        |> String.concat ","

    let flag =
        if truncated then
            ""","exceededTransferLimit":true"""
        else
            ""

    """{"features":[""" + features + "]" + flag + "}"

let private searchDc (answer: string) (name: string) (matching: NameMatch) =
    task {
        use handler = new FakeHandler(fun _ -> ok answer)
        let registry = Sources.districtOfColumbia (connectionOver handler)
        let! found = Registries.searchByNameAsync registry name matching dc ct
        return found, sent handler
    }

[<Fact>]
let ``DC sends each mode's condition in ArcGIS SQL, escaped`` () : Task =
    task {
        let expected =
            [
                NameMatch.Exact,
                "(UPPER(BUSINESS_NAME) = 'DEGREES GROUP, INC.' OR UPPER(BUSINESS_NAME) LIKE '%D%EGREE%S%G%ROUP%I%N%C%')"
                NameMatch.StartsWith, "UPPER(BUSINESS_NAME) LIKE '%D%EGREE%S%G%ROUP%I%N%C%'"
                NameMatch.Contains, "UPPER(BUSINESS_NAME) LIKE '%D%EGREE%S%G%ROUP%I%N%C%'"
            ]

        for matching, condition in expected do
            let! _, queries = searchDc (arcgisAnswer false []) "Degrees Group, Inc." matching
            Assert.Contains($"where={condition}&", queries.Head)

        let! _, quoted = searchDc (arcgisAnswer false []) "O'Neil" NameMatch.Exact

        Assert.Contains("(UPPER(BUSINESS_NAME) = 'O''NEIL' OR UPPER(BUSINESS_NAME) LIKE '%O%NEIL%')", quoted.Head)
    }

[<Fact>]
let ``DC keeps only the names matching in each mode, and is complete only when the service returned every feature below the limit``
    ()
    : Task =
    task {
        let names =
            [ "Degrees Group, Inc."; "Degrees Group Inc"; "Degrees Group Holdings, Inc." ]

        for matching in modes do
            let! found, _ = searchDc (arcgisAnswer false names) "DEGREES GROUP INC" matching
            let result = resultOf found
            Assert.True result.Complete

            Assert.Equal<string list>(List.take 2 names, result.Matches |> List.map _.Name)

            let! truncated, _ = searchDc (arcgisAnswer true names) "DEGREES GROUP INC" matching
            Assert.False((resultOf truncated).Complete)

            let! full, _ =
                searchDc (arcgisAnswer false (List.replicate 50 "Degrees Group, Inc.")) "DEGREES GROUP INC" matching

            Assert.False((resultOf full).Complete)
    }

[<Fact>]
let ``an exact name asked without full stops or apostrophes is sent loose enough to find them, and kept`` () : Task =
    task {
        let held =
            [
                "Degrees of Cooking L.L.C."
                "Degrees of Cooking Club L.L.C."
                "McDonald's USA, LLC"
            ]

        let! found, queries =
            searchDc (arcgisAnswer false held) "degrees of cooking llc" NameMatch.Exact

        Assert.Contains("UPPER(BUSINESS_NAME) LIKE '%D%EGREE%S%O%F%C%OOKING%L%L%C%'", queries.Head)
        Assert.Equal<string list>([ "Degrees of Cooking L.L.C." ], (resultOf found).Matches |> List.map _.Name)

        let! found, queries =
            searchDc (arcgisAnswer false held) "MCDONALDS USA LLC" NameMatch.Exact

        Assert.Contains("UPPER(BUSINESS_NAME) LIKE '%M%CDONALD%S%U%S%A%L%L%C%'", queries.Head)
        Assert.Equal<string list>([ "McDonald's USA, LLC" ], (resultOf found).Matches |> List.map _.Name)
    }

// Registers without a name match of their own

[<Fact>]
let ``Taiwan's contains search is narrowed for Exact and StartsWith`` () : Task =
    task {
        let rows =
            """[{"Business_Accounting_NO":"22099131","Company_Name":"台灣積體電路製造股份有限公司","Company_Status":"01"},{"Business_Accounting_NO":"54900838","Company_Name":"台積電機有限公司","Company_Status":"01"}]"""

        use handler = new FakeHandler(fun _ -> ok rows)
        let registry = Sources.taiwan (movingConnectionOver handler)
        Assert.False(registry :? INameMatchSearch)

        let! exact =
            Registries.searchByNameAsync registry "台灣積體電路製造股份有限公司" NameMatch.Exact (Jurisdiction.ofCode "TW") ct

        let result = resultOf exact
        Assert.Equal<string list>([ "22099131" ], result.Matches |> List.map _.Number)
        Assert.False result.Complete

        let! starts =
            Registries.searchByNameAsync registry "台積" NameMatch.StartsWith (Jurisdiction.ofCode "TW") ct

        Assert.Equal<string list>([ "54900838" ], (resultOf starts).Matches |> List.map _.Number)
    }
