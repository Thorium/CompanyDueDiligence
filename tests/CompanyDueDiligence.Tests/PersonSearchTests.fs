/// Person search, replayed from answers recorded on 2026-11-08 with people's names redacted to PERSON ONE and OTHER words, and for Companies
/// House from answers built from the bundled specs with made-up names.
module PersonSearchTests

open System
open System.IO
open System.Threading.Tasks
open Xunit
open CompanyDueDiligence
open CompanyDueDiligence.CompaniesHouse
open Fake

let private fixture (name: string) =
    File.ReadAllText(Path.Combine(AppContext.BaseDirectory, "Fixtures", name))

let private fixedTime = DateTimeOffset(2026, 11, 8, 12, 0, 0, TimeSpan.Zero)

/// A clock that waits move on, since a search can take more calls than one pacing window allows.
let private connectionOver (handler: FakeHandler) =
    let clock = FakeClock()
    SourceConnection(handler, (fun () -> clock.Now), clock.Delay)

let private fr = Jurisdiction.ofCode "FR"
let private ctState = Jurisdiction.ofCode "US-CT"
let private ny = Jurisdiction.ofCode "US-NY"
let private dc = Jurisdiction.ofCode "US-DC"
let private pa = Jurisdiction.ofCode "US-PA"
let private lv = Jurisdiction.ofCode "LV"
let private gb = Jurisdiction.ofCode "GB"

let private searcher (registry: ICompanyRegistry) =
    match registry with
    | :? IPersonSearch as search -> search
    | _ -> failwith $"{registry.Coverage.Registry} doesn't search people"

let private searchIn (registry: ICompanyRegistry) (name: string) (jurisdiction: Jurisdiction) =
    (searcher registry).SearchPeopleAsync(name, jurisdiction, ct)

let private found (result: Result<Evidence<Availability<PersonSearchResult>>, RegistryFailure>) =
    match result with
    | Ok { Value = Available result } -> result
    | other -> failwith $"%A{other}"

let private sent (handler: FakeHandler) =
    handler.Calls |> List.map (fun call -> Uri.UnescapeDataString call.Target)

let private unreadable (result: Result<Evidence<Availability<PersonSearchResult>>, RegistryFailure>) =
    match result with
    | Error(RegistryFailure.UnreadableAnswer _) -> ()
    | other -> failwith $"%A{other}"

let private companies (result: PersonSearchResult) =
    result.Matches
    |> List.map (fun found -> found.CompanyNumber |> ValueOption.defaultValue "")

// The name and its matching

[<Theory>]
[<InlineData("Ada Lovelace", "Ada", "Lovelace")>]
[<InlineData("  Ada   King  Lovelace ", "Ada King", "Lovelace")>]
[<InlineData("Lovelace, Ada", "Ada", "Lovelace")>]
[<InlineData("De La Cruz, Ada King", "Ada King", "De La Cruz")>]
[<InlineData("Lovelace", "", "Lovelace")>]
[<InlineData("Lovelace,", "", "Lovelace")>]
[<InlineData(", Ada", "", "Ada")>]
let ``a name is read as Firstname Lastname or Lastname, Firstname``
    (asked: string, forenames: string, surname: string)
    =
    let name = (PersonName.tryParse asked).Value
    Assert.Equal(forenames, String.concat " " name.Forenames)
    Assert.Equal(surname, name.Surname)

[<Theory>]
[<InlineData("")>]
[<InlineData("   ")>]
[<InlineData(" , ")>]
[<InlineData("%_.")>]
let ``a name with no letter or digit is no name`` (asked: string) =
    Assert.Equal(ValueNone, PersonName.tryParse asked)

[<Theory>]
[<InlineData("Ada Lovelace", "LOVELACE ADA", true)>]
[<InlineData("Lovelace, Ada", "Ada King Lovelace", true)>]
[<InlineData("Ada Lovelace", "Mrs. Ada LOVELACE", true)>]
[<InlineData("Zoë D'Arcy", "ZOE DARCY", true)>]
[<InlineData("Jean-Luc Picard", "PICARD (JEAN LUC)", true)>]
[<InlineData("Ada Lovelace", "Adah Lovelace", false)>]
[<InlineData("Ada Lovelace", "Lovelace", false)>]
[<InlineData("Person One", "PERSONA ONE", false)>]
[<InlineData("Ada Lovelace", "Ada L*******", false)>]
[<InlineData("Ada Lovelace", "A** L*******", false)>]
[<InlineData("Ada Lovelace", "", false)>]
let ``every word asked has to be a whole word of the published name, and a masked name never matches``
    (asked: string, published: string, expected: bool)
    =
    Assert.Equal(expected, PersonName.matches (PersonName.tryParse asked).Value published)

[<Fact>]
let ``only registers that publish and search people offer a person search`` () =
    use handler = new FakeHandler(fun _ -> ok "[]")
    let connection = connectionOver handler

    let offered =
        [
            Sources.france connection
            Sources.poland connection
            Sources.colorado connection
            Sources.connecticut connection
            Sources.newYork connection
            Sources.texas connection
            Sources.districtOfColumbia connection
            Sources.pennsylvania connection
            Sources.latvia connection
            Sources.norway connection
        ]
        |> Registries.personSearches
        |> List.map (fun (registry, _) -> registry.Coverage.Jurisdictions.Head.Code)

    Assert.Equal<string list>([ "FR"; "US-CT"; "US-NY"; "US-DC"; "US-PA"; "LV" ], offered)
    Assert.Empty(handler.Calls)

/// Every open data registry that searches people, for the checks every one has to pass.
let private openDataSearches (connection: SourceConnection) =
    [
        Sources.france connection, fr
        Sources.connecticut connection, ctState
        Sources.newYork connection, ny
        Sources.districtOfColumbia connection, dc
        Sources.pennsylvania connection, pa
        Sources.latvia connection, lv
    ]

[<Fact>]
let ``every person search answers outside its coverage without a call`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok "[]")

        for registry, _ in openDataSearches (connectionOver handler) do
            match! searchIn registry "Person One" gb with
            | Error(RegistryFailure.OutsideCoverage asked) -> Assert.Equal(gb, asked)
            | other -> failwith $"%A{other}"

        Assert.Empty(handler.Calls)
    }

[<Fact>]
let ``every person search refuses a name with no letter or digit without a call`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok "[]")

        for registry, jurisdiction in openDataSearches (connectionOver handler) do
            match! searchIn registry " %, " jurisdiction with
            | Error(RegistryFailure.InvalidRequest _) -> ()
            | other -> failwith $"%A{other}"

        Assert.Empty(handler.Calls)
    }

[<Fact>]
let ``every person search states how far it reaches`` () =
    use handler = new FakeHandler(fun _ -> ok "[]")

    for registry, _ in openDataSearches (connectionOver handler) do
        match (searcher registry).PersonSearch with
        | Support.Partial caveat -> Assert.NotEmpty(caveat)
        | other -> failwith $"%A{other}"

// France

[<Fact>]
let ``France keeps the officers named as asked among the companies found`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "people-fr.json"))
        let! answer = searchIn (Sources.france (connectionOver handler)) "Person One" fr
        let result = found answer

        Assert.Equal<string list>([ "788652279"; "422858688"; "424442580"; "314685454" ], companies result)

        Assert.Equal<string list>(
            [ "PERSON ONE (ONE)"; "PERSON ONE"; "PERSON ONE"; "PERSON DE ONE" ],
            result.Matches |> List.map _.Name
        )

        let first = result.Matches.Head
        Assert.Equal(ValueSome "Directeur Général", first.Role)
        Assert.Equal(ValueSome "KYMEOS (AC NEXITUP INTERNATIONAL)", first.CompanyName)
        Assert.Equal(ValueSome "C", first.CompanyStatus)
        Assert.Equal(ValueNone, first.Current)
        Assert.Equal(ValueSome 45, result.TotalMatches)
        Assert.False(result.Complete)
        Assert.Equal("recherche-entreprises.api.gouv.fr", handler.Calls.Head.Host)
        Assert.Contains("nom_personne=One&prenoms_personne=Person", (sent handler).Head)
    }

[<Fact>]
let ``France drops other officers of the same companies and companies holding no one of that name`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "people-fr.json"))
        let! answer = searchIn (Sources.france (connectionOver handler)) "One, Otherz" fr
        Assert.Empty((found answer).Matches)
    }

[<Fact>]
let ``France sends the surname alone for a one-word name, and reads the comma form`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "people-fr.json"))
        let registry = Sources.france (connectionOver handler)
        let! _ = searchIn registry "One" fr
        let! answer = searchIn registry "One, Person" fr
        let calls = sent handler
        Assert.EndsWith("nom_personne=One&page=1&per_page=25", calls[0])
        Assert.Contains("nom_personne=One&prenoms_personne=Person&", calls[1])
        Assert.Equal(4, (found answer).Matches.Length)
    }

[<Fact>]
let ``France with no company found is never complete`` () : Task =
    task {
        use handler =
            new FakeHandler(fun _ -> ok """{"results":[],"total_results":0,"page":1,"per_page":25,"total_pages":0}""")

        let! answer = searchIn (Sources.france (connectionOver handler)) "Person One" fr
        let result = found answer
        Assert.Empty(result.Matches)
        Assert.Equal(ValueSome 0, result.TotalMatches)
        Assert.False(result.Complete)
    }

[<Fact>]
let ``France's answer with no results list is unreadable`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok """{"erreur":"x"}""")
        let! answer = searchIn (Sources.france (connectionOver handler)) "Person One" fr
        unreadable answer
    }

// Connecticut

let private connecticutOver (principals: string) (businesses: string) =
    new FakeHandler(fun call ->
        if call.Target.Contains "ka36-64k6" then
            ok principals
        else
            ok businesses)

[<Fact>]
let ``Connecticut joins the principals named as asked to their businesses`` () : Task =
    task {
        use handler =
            connecticutOver (fixture "people-ct-principals.json") (fixture "people-ct-businesses.json")

        let! answer =
            searchIn (Sources.connecticut (connectionOver handler)) "Person One" ctState

        let result = found answer

        Assert.Equal<string list>(
            [ "HON. PERSON ONE"; "OTHERB (PERSON) ONE JR."; "PERSON ONE" ],
            result.Matches |> List.map _.Name
        )

        Assert.Equal<string list>([ "0239638"; "0858535"; "1308200" ], companies result)
        Assert.Equal(ValueSome "Officer", result.Matches[0].Role)
        Assert.Equal(ValueSome "Principal", result.Matches[1].Role)
        Assert.Equal(ValueSome "THE PARTNERSHIP FOR CONNECTICUT, INC.", result.Matches[2].CompanyName)
        Assert.Equal(ValueSome "Dissolved", result.Matches[2].CompanyStatus)
        Assert.True(result.Matches |> List.forall (fun found -> found.Current = ValueNone))
        Assert.True(result.Complete)
        Assert.Equal(ValueNone, result.TotalMatches)

        match sent handler with
        | [ principals; businesses ] ->
            Assert.Contains("(upper(name__c) like '%PERSON%') AND (upper(name__c) like '%ONE%')", principals)
            Assert.Contains("$limit=100", principals)
            Assert.Contains("id in ('001t000000WmcTjAAJ','001t000000hjWOiAAM','001t000000hjaHEAAY')", businesses)
        | other -> failwith $"%A{other}"

        Assert.StartsWith("[[{", (Result.defaultWith (fun _ -> failwith "no evidence") answer).Raw.Value)
    }

[<Fact>]
let ``Connecticut drops a principal whose name only contains the words`` () : Task =
    task {
        use handler =
            connecticutOver (fixture "people-ct-principals.json") (fixture "people-ct-businesses.json")

        let! answer =
            searchIn (Sources.connecticut (connectionOver handler)) "Persona One" ctState

        let result = found answer
        Assert.Equal<string list>([ "PERSONA ONE" ], result.Matches |> List.map _.Name)
        Assert.Equal(ValueNone, result.Matches.Head.CompanyNumber)
    }

[<Fact>]
let ``Connecticut with no principal of that name makes one call and is complete`` () : Task =
    task {
        use handler = connecticutOver "[]" "[]"

        let! answer =
            searchIn (Sources.connecticut (connectionOver handler)) "Person One" ctState

        let result = found answer
        Assert.Empty(result.Matches)
        Assert.True(result.Complete)
        Assert.Equal(1, handler.Calls.Length)
    }

[<Fact>]
let ``Connecticut at its row limit isn't complete`` () : Task =
    task {
        let rows =
            List.init 100 (fun i -> $"""{{"business_id":"b{i}","name__c":"PERSON ONE"}}""")
            |> String.concat ","

        use handler = connecticutOver $"[{rows}]" "[]"

        let! answer =
            searchIn (Sources.connecticut (connectionOver handler)) "Person One" ctState

        let result = found answer
        Assert.Equal(100, result.Matches.Length)
        Assert.False(result.Complete)
    }

[<Fact>]
let ``Connecticut's unreadable principals or businesses are unreadable`` () : Task =
    task {
        use first = connecticutOver """{"error":true}""" "[]"

        let! answer =
            searchIn (Sources.connecticut (connectionOver first)) "Person One" ctState

        unreadable answer

        use second =
            connecticutOver (fixture "people-ct-principals.json") """{"error":true}"""

        let! answer =
            searchIn (Sources.connecticut (connectionOver second)) "Person One" ctState

        unreadable answer
    }

// New York

[<Fact>]
let ``New York reads the chief executive named as asked on the active list`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "people-ny.json"))
        let! answer = searchIn (Sources.newYork (connectionOver handler)) "One, Person" ny
        let result = found answer
        let only = Assert.Single(result.Matches)
        Assert.Equal("PERSON OTHERA ONE", only.Name)
        Assert.Equal(ValueSome "Chief executive officer", only.Role)
        Assert.Equal(ValueSome "1475703", only.CompanyNumber)
        Assert.Equal(ValueSome "WILD APACHE PRODUCTIONS INC.", only.CompanyName)
        Assert.Equal(ValueSome "Active", only.CompanyStatus)
        Assert.False(result.Complete)

        Assert.Contains(
            "(upper(chairman_name) like '%PERSON%') AND (upper(chairman_name) like '%ONE%')",
            (sent handler).Head
        )

        Assert.Contains("n9v6-gdp6", (sent handler).Head)
    }

[<Fact>]
let ``New York drops a name lacking a word asked, and is never complete`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "people-ny.json"))
        let registry = Sources.newYork (connectionOver handler)
        let! filtered = searchIn registry "Person Two" ny
        Assert.Empty((found filtered).Matches)

        use empty = new FakeHandler(fun _ -> ok "[]")
        let! answer = searchIn (Sources.newYork (connectionOver empty)) "Person One" ny
        let result = found answer
        Assert.Empty(result.Matches)
        Assert.False(result.Complete)
    }

[<Fact>]
let ``New York's error object is unreadable`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok """{"error":true,"message":"x"}""")
        let! answer = searchIn (Sources.newYork (connectionOver handler)) "Person One" ny
        unreadable answer
    }

// District of Columbia

[<Fact>]
let ``DC reads beneficial owners named as asked with the business the table carries`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "people-dc.json"))

        let! answer =
            searchIn (Sources.districtOfColumbia (connectionOver handler)) "Person One" dc

        let result = found answer
        Assert.Equal<string list>([ "PERSON J. ONE"; "PERSON J ONE" ], result.Matches |> List.map _.Name)
        Assert.Equal<string list>([ "L44758"; "L52160" ], companies result)
        Assert.Equal(ValueSome "Revoked", result.Matches[1].CompanyStatus)
        Assert.Equal(ValueSome "EXAMPLE COMPANY 1 LLC", result.Matches[0].CompanyName)
        Assert.Equal(ValueSome "Beneficial owner", result.Matches[0].Role)
        Assert.True(result.Complete)
        Assert.Equal(1, handler.Calls.Length)
        Assert.Contains("FeatureServer/2/query", (sent handler).Head)
        Assert.Contains("(UPPER(NAME) LIKE '%PERSON%') AND (UPPER(NAME) LIKE '%ONE%')", (sent handler).Head)
    }

[<Fact>]
let ``DC drops a name lacking a word asked, and an answer cut short isn't complete`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "people-dc.json"))

        let! filtered =
            searchIn (Sources.districtOfColumbia (connectionOver handler)) "Person K One" dc

        Assert.Empty((found filtered).Matches)

        use cut =
            new FakeHandler(fun _ ->
                ok
                    """{"features":[{"attributes":{"NAME":"PERSON ONE","INITIALFILENUMBER":"L1"}}],"exceededTransferLimit":true}""")

        let! answer =
            searchIn (Sources.districtOfColumbia (connectionOver cut)) "Person One" dc

        let result = found answer
        Assert.Equal(1, result.Matches.Length)
        Assert.False(result.Complete)
    }

[<Fact>]
let ``DC with no owner of that name is complete, and its error object is unreadable`` () : Task =
    task {
        use empty = new FakeHandler(fun _ -> ok """{"features":[]}""")

        let! answer =
            searchIn (Sources.districtOfColumbia (connectionOver empty)) "Person One" dc

        let result = found answer
        Assert.Empty(result.Matches)
        Assert.True(result.Complete)

        use failing =
            new FakeHandler(fun _ -> ok """{"error":{"code":400,"message":"x"}}""")

        let! failed =
            searchIn (Sources.districtOfColumbia (connectionOver failing)) "Person One" dc

        unreadable failed
    }

// Pennsylvania

[<Fact>]
let ``Pennsylvania reads officers named as asked, in either order of the name parts`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "people-pa.json"))

        let! answer =
            searchIn (Sources.pennsylvania (connectionOver handler)) "Person One" pa

        let result = found answer
        Assert.Equal(32, result.Matches.Length)
        Assert.Contains(result.Matches, fun found -> found.Name = "ONE PERSON")
        Assert.Contains(result.Matches, fun found -> found.Name = "Mr. PERSON ONE")
        Assert.DoesNotContain(result.Matches, fun found -> found.Name.Contains "PERSONA")
        let first = result.Matches.Head
        Assert.Equal(ValueSome "Governor", first.Role)
        Assert.Equal(ValueSome "0007382764", first.CompanyNumber)
        Assert.Equal(ValueSome "Example Business 1 LLC", first.CompanyName)
        Assert.False(result.Complete, "The data holds active registrations only")

        Assert.Contains(
            "(upper(first_name) like '%PERSON%' OR upper(middle_name) like '%PERSON%' OR upper(last_name) like '%PERSON%')",
            (sent handler).Head
        )
    }

[<Fact>]
let ``Pennsylvania collapses repeated rows, and at its row limit isn't complete`` () : Task =
    task {
        let rows =
            List.replicate
                100
                """{"first_name":"PERSON","last_name":"ONE","party_type":"Governor","filing_number":"0000000001"}"""
            |> String.concat ","

        use handler = new FakeHandler(fun _ -> ok $"[{rows}]")

        let! answer =
            searchIn (Sources.pennsylvania (connectionOver handler)) "Person One" pa

        let result = found answer
        Assert.Equal(1, result.Matches.Length)
        Assert.False(result.Complete)
    }

[<Fact>]
let ``Pennsylvania with no row isn't complete, as the data holds active registrations only, and its error object is unreadable``
    ()
    : Task =
    task {
        use empty = new FakeHandler(fun _ -> ok "[]")
        let! answer = searchIn (Sources.pennsylvania (connectionOver empty)) "Person One" pa
        Assert.False((found answer).Complete)

        use failing = new FakeHandler(fun _ -> ok """{"error":true}""")

        let! failed =
            searchIn (Sources.pennsylvania (connectionOver failing)) "Person One" pa

        unreadable failed
    }

// Latvia

let private latviaOver (officers: string) (owners: string) (companies: string) =
    new FakeHandler(fun call ->
        if call.Target.Contains Latvia.OfficersResource then
            ok officers
        elif call.Target.Contains Latvia.OwnersResource then
            ok owners
        else
            ok companies)

let private latviaEmpty = """{"success":true,"result":{"records":[],"total":0}}"""

[<Fact>]
let ``Latvia reads representatives and beneficial owners named as asked, with their companies`` () : Task =
    task {
        use handler =
            latviaOver
                (fixture "people-lv-officers.json")
                (fixture "people-lv-owners.json")
                (fixture "people-lv-companies.json")

        let! answer = searchIn (Sources.latvia (connectionOver handler)) "Person One" lv
        let result = found answer

        Assert.Equal<string list>(
            [ "40008093564"; "40203761542"; "40002231651"; "40203761542"; "40008093564" ],
            companies result
        )

        Assert.Equal<string list>(
            [ "ONE PERSON"; "ONE PERSON"; "ONE PERSON"; "PERSON ONE"; "PERSON ONE" ],
            result.Matches |> List.map _.Name
        )

        Assert.Equal(ValueSome "EXECUTIVE_BOARD", result.Matches[0].Role)
        Assert.Equal(ValueSome "BOARD_MEMBER", result.Matches[1].Role)
        Assert.Equal(ValueNone, result.Matches[2].Role)
        Assert.Equal(ValueSome true, result.Matches[0].Current)
        Assert.Equal(ValueSome(DateOnly(2005, 8, 19)), result.Matches[0].AppointedOn)
        Assert.Equal(ValueSome "Beneficial owner", result.Matches[3].Role)
        Assert.Equal(ValueNone, result.Matches[3].Current)
        Assert.Equal(ValueSome "SIA Babekora", result.Matches[1].CompanyName)
        Assert.Equal(ValueSome "IK PERSON ONE", result.Matches[2].CompanyName)
        Assert.Equal(ValueSome 5, result.TotalMatches)
        Assert.True(result.Complete)

        match sent handler with
        | [ officers; owners; companies ] ->
            Assert.Contains("q=Person One", officers)
            Assert.Contains(Latvia.OwnersResource, owners)
            Assert.Contains("""filters={"regcode":["40008093564","40203761542","40002231651"]}""", companies)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``Latvia drops a name lacking a word asked, and makes no company call without a match`` () : Task =
    task {
        use handler =
            latviaOver
                (fixture "people-lv-officers.json")
                (fixture "people-lv-owners.json")
                (fixture "people-lv-companies.json")

        let! answer = searchIn (Sources.latvia (connectionOver handler)) "Person Two" lv
        let result = found answer
        Assert.Empty(result.Matches)
        Assert.True(result.Complete)
        Assert.Equal(2, handler.Calls.Length)
    }

[<Fact>]
let ``Latvia is complete only when both totals say every record came back`` () : Task =
    task {
        use empty = latviaOver latviaEmpty latviaEmpty latviaEmpty
        let! answer = searchIn (Sources.latvia (connectionOver empty)) "Person One" lv
        Assert.True((found answer).Complete)

        let more =
            """{"success":true,"result":{"records":[{"name":"ONE PERSON","at_legal_entity_registration_number":40008093564}],"total":101}}"""

        use partial = latviaOver more latviaEmpty (fixture "people-lv-companies.json")
        let! answer = searchIn (Sources.latvia (connectionOver partial)) "Person One" lv
        let result = found answer
        Assert.Equal(1, result.Matches.Length)
        Assert.Equal(ValueSome 101, result.TotalMatches)
        Assert.False(result.Complete)
    }

[<Fact>]
let ``Latvia's answer that doesn't say it succeeded is unreadable`` () : Task =
    task {
        use handler = latviaOver """{"success":false}""" latviaEmpty latviaEmpty
        let! answer = searchIn (Sources.latvia (connectionOver handler)) "Person One" lv
        unreadable answer
    }

// Companies House, from answers built from search.json and officerAppointmentList.json

let private officerItem (title: string) (id: string) =
    $"""{{"kind":"searchresults#officer","title":"{title}","description":"Total number of appointments 1","appointment_count":1,"address_snippet":"1 Example Street","address":{{"premises":"1","address_line_1":"Example Street"}},"date_of_birth":{{"month":1,"year":1970}},"links":{{"self":"/officers/{id}/appointments"}}}}"""

let private officerSearch (total: int) (items: string list) =
    $"""{{"kind":"search#officers","total_results":{total},"items_per_page":50,"start_index":0,"items":[{String.concat "," items}]}}"""

let private appointment (name: string) (role: string) (number: string) (company: string) (resigned: string voption) =
    let ended =
        match resigned with
        | ValueSome date -> $""","resigned_on":"{date}" """
        | ValueNone -> ""

    $"""{{"name":"{name}","officer_role":"{role}","appointed_on":"2015-01-01"{ended},"appointed_to":{{"company_number":"{number}","company_name":"{company}","company_status":"active"}}}}"""

let private appointments (total: int) (items: string list) =
    $"""{{"kind":"personal-appointment","total_results":{total},"items_per_page":50,"start_index":0,"items":[{String.concat "," items}]}}"""

let private quentinSearch =
    officerSearch
        3
        [
            officerItem "Quentin EXAMPLE" "q1"
            officerItem "Quentin Alan EXAMPLE" "q2"
            officerItem "Quentina EXAMPLES" "q3"
        ]

let private quentinAppointments (call: Call) =
    if call.Target.StartsWith "/officers/q1/" then
        ok (
            appointments
                2
                [
                    appointment "Quentin EXAMPLE" "director" "01234567" "EXAMPLE TRADING LIMITED" ValueNone
                    appointment
                        "Quentin EXAMPLE"
                        "secretary"
                        "07654321"
                        "EXAMPLE HOLDINGS LIMITED"
                        (ValueSome "2020-05-05")
                ]
        )
    elif call.Target.StartsWith "/officers/q2/" then
        ok (
            appointments
                1
                [
                    appointment "Quentin Alan EXAMPLE" "director" "SC123456" "EXAMPLE SCOTLAND LIMITED" ValueNone
                ]
        )
    else
        notFound

let private companiesHouseWith (handler: FakeHandler) : ICompanyRegistry =
    CompaniesHouseRegistry(clientWith handler, fun () -> fixedTime)

[<Fact>]
let ``Companies House reads the appointments of the officers named as asked`` () : Task =
    task {
        use handler =
            new FakeHandler(fun call ->
                if call.Target.StartsWith "/search/officers" then
                    ok quentinSearch
                else
                    quentinAppointments call)

        let! answer = searchIn (companiesHouseWith handler) "Quentin Example" gb
        let result = found answer

        Assert.Equal<string list>([ "01234567"; "07654321"; "SC123456" ], companies result)

        Assert.Equal<string voption list>(
            [ ValueSome "director"; ValueSome "secretary"; ValueSome "director" ],
            result.Matches |> List.map _.Role
        )

        Assert.Equal<bool voption list>(
            [ ValueSome true; ValueSome false; ValueSome true ],
            result.Matches |> List.map _.Current
        )

        Assert.Equal(ValueSome(DateOnly(2020, 5, 5)), result.Matches[1].EndedOn)
        Assert.Equal(ValueSome(DateOnly(2015, 1, 1)), result.Matches[0].AppointedOn)
        Assert.Equal(ValueSome "EXAMPLE TRADING LIMITED", result.Matches[0].CompanyName)
        Assert.Equal(ValueSome "active", result.Matches[0].CompanyStatus)
        Assert.Equal(ValueSome 3, result.TotalMatches)
        Assert.True(result.Complete)

        let calls = sent handler
        Assert.Equal("/search/officers?q=Quentin Example&start_index=0&items_per_page=50", calls.Head)
        Assert.Equal(3, calls.Length)
        Assert.DoesNotContain(calls, fun call -> call.StartsWith "/officers/q3/")
    }

[<Fact>]
let ``Companies House with no officer found is complete`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (officerSearch 0 []))
        let! answer = searchIn (companiesHouseWith handler) "Example, Quentin" gb
        let result = found answer
        Assert.Empty(result.Matches)
        Assert.True(result.Complete)
        Assert.Equal(1, handler.Calls.Length)
    }

[<Fact>]
let ``Companies House isn't complete with officers or appointments unread`` () : Task =
    task {
        use moreOfficers =
            new FakeHandler(fun call ->
                if call.Target.StartsWith "/search/officers" then
                    ok (officerSearch 80 [ officerItem "Quentin EXAMPLE" "q1" ])
                else
                    quentinAppointments call)

        let! answer = searchIn (companiesHouseWith moreOfficers) "Quentin Example" gb
        Assert.False((found answer).Complete)

        use moreAppointments =
            new FakeHandler(fun call ->
                if call.Target.StartsWith "/search/officers" then
                    ok (officerSearch 1 [ officerItem "Quentin EXAMPLE" "q1" ])
                else
                    ok (
                        appointments
                            60
                            [
                                appointment "Quentin EXAMPLE" "director" "01234567" "EXAMPLE TRADING LIMITED" ValueNone
                            ]
                    ))

        let! answer = searchIn (companiesHouseWith moreAppointments) "Quentin Example" gb
        let result = found answer
        Assert.Equal(1, result.Matches.Length)
        Assert.False(result.Complete)

        use goneOfficer =
            new FakeHandler(fun call ->
                if call.Target.StartsWith "/search/officers" then
                    ok (officerSearch 1 [ officerItem "Quentin EXAMPLE" "q9" ])
                else
                    notFound)

        let! answer = searchIn (companiesHouseWith goneOfficer) "Quentin Example" gb
        let result = found answer
        Assert.Empty(result.Matches)
        Assert.False(result.Complete)
    }

[<Fact>]
let ``Companies House fails the search when an appointments call fails`` () : Task =
    task {
        use handler =
            new FakeHandler(fun call ->
                if call.Target.StartsWith "/search/officers" then
                    ok quentinSearch
                else
                    answer 500 "")

        match! searchIn (companiesHouseWith handler) "Quentin Example" gb with
        | Error(RegistryFailure.Unavailable _) -> ()
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``Companies House's search with no total is unreadable`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok """{"items":[]}""")
        let! answer = searchIn (companiesHouseWith handler) "Quentin Example" gb
        unreadable answer
    }

[<Fact>]
let ``Companies House answers outside the United Kingdom, and a name with no letter, without a call`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok quentinSearch)
        let registry = companiesHouseWith handler

        match! searchIn registry "Quentin Example" ny with
        | Error(RegistryFailure.OutsideCoverage _) -> ()
        | other -> failwith $"%A{other}"

        match! searchIn registry " , " gb with
        | Error(RegistryFailure.InvalidRequest _) -> ()
        | other -> failwith $"%A{other}"

        Assert.Empty(handler.Calls)
    }
