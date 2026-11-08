/// The free sources, replayed from answers recorded on 2026-11-06 with people's names redacted. Anything a source could answer that isn't understood has
/// to come back as a failure or NotAvailable, never as a company or "not held".
module SourceTests

open System
open System.IO
open System.Threading
open System.Threading.Tasks
open Xunit
open CompanyDueDiligence
open Fake

let private fixture (name: string) =
    File.ReadAllText(Path.Combine(AppContext.BaseDirectory, "Fixtures", name))

let private fixedTime = DateTimeOffset(2026, 11, 6, 12, 0, 0, TimeSpan.Zero)

/// A connection that takes the time from the fixed clock and doesn't wait.
let private connectionOver (handler: FakeHandler) =
    SourceConnection(handler, (fun () -> fixedTime), (fun _ _ -> Task.CompletedTask))

let private mt = Jurisdiction.ofCode "MT"
let private fr = Jurisdiction.ofCode "FR"
let private fi = Jurisdiction.ofCode "FI"
let private tx = Jurisdiction.ofCode "US-TX"

let private findIn (registry: ICompanyRegistry) (number: string) (jurisdiction: Jurisdiction) =
    task {
        match! registry.TryFindAsync(number, jurisdiction, ct) with
        | Ok evidence -> return evidence
        | Error failure -> return failwith (RegistryFailure.describe failure)
    }

let private companyOf (evidence: Evidence<Availability<Company voption>>) =
    match evidence.Value with
    | Available(ValueSome company) -> company
    | other -> failwith $"%A{other}"

/// Every registry here, with an answer that holds nothing it can use, against the failure it has to become.
let private allRegistries (handler: FakeHandler) =
    let connection = connectionOver handler

    [
        Sources.malta connection, "C 1", mt
        Sources.france connection, "552032534", fr
        Sources.finland connection, "0112038-9", fi
        Sources.texas connection, "0800515257", tx
    ]

[<Fact>]
let ``Malta reads an active company as the register words it`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "malta-active.json"))
        let! evidence = findIn (Sources.malta (connectionOver handler)) "C 10" mt
        let company = companyOf evidence
        Assert.Equal("Malta Business Registry", evidence.Source)
        Assert.Equal(fixedTime, evidence.RetrievedAt)
        Assert.True evidence.Raw.IsSome
        Assert.Equal("BALLUT BLOCKS LIMITED", company.Name)
        Assert.Equal("C 10", company.Number)
        Assert.Equal(CompanyStanding.Active, company.Standing)
        Assert.Equal(RecordRole.Home, company.Role)
        Assert.Equal<CompanyConcern list>(CompanyConcern.all, company.Concerns.Unconfirmed)
        Assert.Equal("/api/v1/companies/C%2010", handler.Calls.Head.Target)
    }

[<Fact>]
let ``Malta's address field holding a note isn't read as an address`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "malta-struck-off.json"))
        let! evidence = findIn (Sources.malta (connectionOver handler)) "C 1" mt
        let company = companyOf evidence
        Assert.Equal(CompanyStanding.Closed "Struck Off", company.Standing)

        match company.Addresses with
        | NotAvailable _ -> ()
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``Malta's 404 reads as not held`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> answer 404 (fixture "malta-not-found.json"))
        let! evidence = findIn (Sources.malta (connectionOver handler)) "C 99999999" mt
        Assert.Equal(Available ValueNone, evidence.Value)
    }

[<Fact>]
let ``a Malta 404 without the registry's own error is refused, not read as not held`` () : Task =
    task {
        for body in [ ""; "<html>Not Found</html>"; """{"message":"no route"}""" ] do
            use handler = new FakeHandler(fun _ -> answer 404 body)

            match! (Sources.malta (connectionOver handler)).TryFindAsync("C 99999999", mt, ct) with
            | Error(RegistryFailure.RequestRefused _) -> ()
            | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``another company in Malta's answer isn't taken for the number asked`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "malta-active.json"))

        match! (Sources.malta (connectionOver handler)).TryFindAsync("C 11", mt, ct) with
        | Error(RegistryFailure.UnreadableAnswer _) -> ()
        | other -> failwith $"%A{other}"

        let! evidence = findIn (Sources.malta (connectionOver handler)) "c10" mt
        Assert.Equal("C 10", (companyOf evidence).Number)
    }

[<Theory>]
[<InlineData("Active", "Active")>]
[<InlineData("In Process of Merging", "ActiveWithConcern")>]
[<InlineData("In Process of Converting", "ActiveWithConcern")>]
[<InlineData("In Process of Dividing", "ActiveWithConcern")>]
[<InlineData("In Dissolution", "WindingUp")>]
[<InlineData("Simplified Dissolution", "WindingUp")>]
[<InlineData("Struck Off", "Closed")>]
[<InlineData("Struck Off as Defunct", "Closed")>]
[<InlineData("Struck Off Following Merger", "Closed")>]
[<InlineData("Struck Off Following Division", "Closed")>]
[<InlineData("Struck Off Following Cross-Border Conversion", "Closed")>]
[<InlineData("Struck Off Following Cross-Border Merger", "Closed")>]
[<InlineData("Converted to SE", "Closed")>]
[<InlineData("Inactive - Not Submitting Documents", "Unknown")>]
[<InlineData("A word added later", "Unknown")>]
let ``every state word in Malta's open data has a standing, and a new one is unknown``
    (word: string, expected: string)
    =
    Assert.StartsWith(expected, $"%A{Malta.standing (ValueSome word)}")

[<Fact>]
let ``no Malta word but Active reads as active`` () =
    Assert.False(CompanyStanding.isActive (Malta.standing (ValueSome "Struck Off")))
    Assert.False(CompanyStanding.isActive (Malta.standing ValueNone))
    Assert.True(CompanyStanding.isActive (Malta.standing (ValueSome "Active")))

[<Theory>]
[<InlineData("C 10", "C 10")>]
[<InlineData("c10", "C 10")>]
[<InlineData(" SV 12 ", "SV 12")>]
[<InlineData("OC 7", "OC 7")>]
[<InlineData("EEIG 3", "EEIG 3")>]
let ``a Malta number is put in the register's spelling`` (number: string, expected: string) =
    Assert.Equal(ValueSome expected, Malta.normalNumber number)

[<Theory>]
[<InlineData("")>]
[<InlineData("1234")>]
[<InlineData("C")>]
[<InlineData(null)>]
let ``a Malta number with no prefix isn't sent`` (number: string) : Task =
    task {
        use handler = new FakeHandler(fun _ -> failwith "Nothing should be sent")

        match! (Sources.malta (connectionOver handler)).TryFindAsync(number, mt, ct) with
        | Error(RegistryFailure.InvalidRequest _) -> Assert.Empty handler.Calls
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``Malta offers no officers, owners or search, and says so without calling`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> failwith "Nothing should be sent")
        let registry = Sources.malta (connectionOver handler)
        let! officers = registry.OfficersAsync("C 1", mt, ct)
        let! search = registry.SearchAsync("x", mt, ct)

        match officers, search with
        | Ok { Value = NotAvailable _ }, Ok { Value = NotAvailable _ } -> Assert.Empty handler.Calls
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``France reads a company with its officers, the source's date and the record alone as raw`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "france-active.json"))
        let registry = Sources.france (connectionOver handler)
        let! evidence = findIn registry "552032534" fr
        let company = companyOf evidence
        Assert.Equal("DANONE", company.Name)
        Assert.Equal(CompanyStanding.Active, company.Standing)
        Assert.Equal(ValueSome(DateOnly(2026, 11, 6)), evidence.AsOf)
        Assert.StartsWith("{\"siren\":\"552032534\"", evidence.Raw.Value)

        match! registry.OfficersAsync("552032534", fr, ct) with
        | Ok { Value = Available officers } ->
            Assert.Equal(13, officers.Length)

            Assert.Equal(
                2,
                officers
                |> List.filter (fun officer -> officer.Kind = PartyKind.LegalEntity)
                |> List.length
            )

            Assert.All(officers, (fun officer -> Assert.True officer.Current))

            let person =
                officers |> List.find (fun officer -> officer.Kind = PartyKind.Individual)

            Assert.Equal("REDACTED REDACTED", person.Name)
            Assert.Equal(ValueSome { Year = 1900; Month = 1 }, person.BornIn)

            let firm =
                officers |> List.find (fun officer -> officer.Kind = PartyKind.LegalEntity)

            Assert.True firm.Entity.Value.RegistrationNumber.IsSome
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``France answers any nine digits with a stub, which is never a company`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "france-stub.json"))
        let! evidence = findIn (Sources.france (connectionOver handler)) "999999999" fr

        match evidence.Value with
        | NotAvailable reason -> Assert.Contains("non-diffusible", reason)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``France not finding a number is NotAvailable, never not held`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "france-no-results.json"))
        let! evidence = findIn (Sources.france (connectionOver handler)) "552032534" fr

        match evidence.Value with
        | NotAvailable reason -> Assert.Contains("proves nothing", reason)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``France reads C as closed, with its closing date`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "france-closed.json"))
        let! evidence = findIn (Sources.france (connectionOver handler)) "479575037" fr
        let company = companyOf evidence
        Assert.StartsWith("Closed", $"%A{company.Standing}")
        Assert.Equal(ValueSome(DateOnly(2006, 3, 31)), company.CeasedOn)
    }

[<Fact>]
let ``France's coverage says A is the administrative state, which carries no collective proceedings`` () =
    use handler = new FakeHandler(fun _ -> failwith "Nothing should be sent")

    match (Sources.france (connectionOver handler)).Coverage.Standing with
    | Support.Partial caveat ->
        Assert.Contains("administrative state", caveat)
        Assert.Contains("collective proceedings", caveat)
    | other -> failwith $"%A{other}"

[<Fact>]
let ``another company's record in France's answer isn't taken for the one asked`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "france-active.json"))
        let! evidence = findIn (Sources.france (connectionOver handler)) "111111111" fr

        match evidence.Value with
        | NotAvailable _ -> ()
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``France's partial diffusion hides the address, and an unknown status is unknown`` () : Task =
    task {
        let body =
            """{"results":[{"siren":"123456782","nom_complet":"X","etat_administratif":"Z","statut_diffusion":"P","siege":{"adresse":"[ND]"},"date_creation":null}],"total_results":1}"""

        use handler = new FakeHandler(fun _ -> ok body)
        let! evidence = findIn (Sources.france (connectionOver handler)) "123456782" fr
        let company = companyOf evidence
        Assert.Equal(CompanyStanding.Unknown(ValueSome "Z"), company.Standing)

        match company.Addresses with
        | NotAvailable _ -> ()
        | other -> failwith $"%A{other}"

        let withAddress =
            """{"results":[{"siren":"123456782","nom_complet":"X","etat_administratif":"A","statut_diffusion":"P","siege":{"adresse":"1 RUE EXEMPLE 75001 PARIS"}}],"total_results":1}"""

        use shown = new FakeHandler(fun _ -> ok withAddress)
        let! evidence = findIn (Sources.france (connectionOver shown)) "123456782" fr

        match (companyOf evidence).Addresses with
        | NotAvailable reason -> Assert.Contains("partial diffusion", reason)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``France's empty officer list says nothing`` () : Task =
    task {
        let body =
            """{"results":[{"siren":"123456782","nom_complet":"X","etat_administratif":"A","dirigeants":[]}],"total_results":1}"""

        use handler = new FakeHandler(fun _ -> ok body)

        match! (Sources.france (connectionOver handler)).OfficersAsync("123456782", fr, ct) with
        | Ok { Value = NotAvailable _ } -> ()
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``France's search drops stubs and is never complete`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "france-stub.json"))

        match! (Sources.france (connectionOver handler)).SearchAsync("x", fr, ct) with
        | Ok { Value = Available found } ->
            Assert.Empty found.Matches
            Assert.False found.Complete
        | other -> failwith $"%A{other}"
    }

[<Theory>]
[<InlineData("12345678")>]
[<InlineData("1234567890")>]
[<InlineData("abcdefghi")>]
[<InlineData(null)>]
let ``a value that can't be a SIREN isn't sent`` (number: string) : Task =
    task {
        use handler = new FakeHandler(fun _ -> failwith "Nothing should be sent")

        match! (Sources.france (connectionOver handler)).TryFindAsync(number, fr, ct) with
        | Error(RegistryFailure.InvalidRequest _) -> Assert.Empty handler.Calls
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``Finland reads a company, Registered and not Active, with the source's date`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "finland-company.json"))
        let! evidence = findIn (Sources.finland (connectionOver handler)) "0112038-9" fi
        let company = companyOf evidence
        Assert.Equal("Nokia Oyj", company.Name)
        Assert.Equal(CompanyStanding.Registered(ValueSome "Registered"), company.Standing)
        Assert.False(CompanyStanding.isActive company.Standing)
        Assert.Equal(ValueSome "Public limited company", company.LegalForm)
        Assert.Equal(ValueSome(DateOnly(2026, 10, 22)), evidence.AsOf)

        match company.Addresses with
        | Available [ street; postal ] ->
            Assert.Equal(AddressKind.Other "Street address on YTJ", street.Kind)
            Assert.Equal(ValueSome "ESPOO", street.Address.Locality)
            Assert.Equal(AddressKind.Postal, postal.Kind)
        | other -> failwith $"%A{other}"
    }

[<Theory>]
[<InlineData("KONK", "InInsolvency")>]
[<InlineData("SANE", "InInsolvency")>]
[<InlineData("SELTILA", "WindingUp")>]
let ``a Finnish situation decides the standing, whatever the register says`` (code: string, expected: string) =
    Assert.StartsWith(expected, $"""%A{Finland.standing [ code ] (ValueSome "2") (ValueSome "1")}""")

[<Theory>]
[<InlineData("1", "Registered")>]
[<InlineData("2", "Closed")>]
[<InlineData("4", "Closed")>]
[<InlineData("0", "Unknown")>]
[<InlineData("3", "Unknown")>]
[<InlineData("9", "Unknown")>]
let ``a Finnish trade register status has a standing, and a new one is unknown`` (code: string, expected: string) =
    Assert.StartsWith(expected, $"""%A{Finland.standing [] (ValueSome "2") (ValueSome code)}""")

[<Fact>]
let ``an invalidated Finnish Business ID is closed`` () =
    Assert.Equal(CompanyStanding.Closed "Business ID invalidated", Finland.standing [] (ValueSome "5") (ValueSome "1"))

[<Fact>]
let ``a recorded Finnish situation that has ended is history, not a standing`` () : Task =
    task {
        let body =
            (fixture "finland-company.json")
                .Replace(
                    "\"companySituations\":[]",
                    "\"companySituations\":[{\"type\":\"KONK\",\"registrationDate\":\"2000-01-01\",\"endDate\":\"2001-01-01\",\"source\":\"1\"}]"
                )

        use handler = new FakeHandler(fun _ -> ok body)
        let! evidence = findIn (Sources.finland (connectionOver handler)) "0112038-9" fi
        let company = companyOf evidence
        Assert.Equal(CompanyStanding.Registered(ValueSome "Registered"), company.Standing)
        Assert.Equal<CompanyConcern list>([ CompanyConcern.InsolvencyHistory ], company.Concerns.Stated)
    }

[<Fact>]
let ``Finland's empty answer is not available, since the open data leaves out sole traders and entities not on the Trade Register``
    ()
    : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "finland-none.json"))
        let! evidence = findIn (Sources.finland (connectionOver handler)) "1952100-9" fi
        Assert.Equal(NotAvailable Finland.notHeld, evidence.Value)
        Assert.Contains("sole traders", Finland.notHeld)
    }

[<Fact>]
let ``an ended Finnish company is read, Closed, with its last name and form and the top-level end date`` () : Task =
    task {
        // 0357299-5 as PRH answered it on 2026-11-08: tradeRegisterStatus 4 and every name and form ended.
        use handler = new FakeHandler(fun _ -> ok (fixture "finland-ended.json"))
        let! evidence = findIn (Sources.finland (connectionOver handler)) "0357299-5" fi
        let company = companyOf evidence
        Assert.Equal(CompanyStanding.Closed "Ceased", company.Standing)
        Assert.Equal(ValueSome(DateOnly(2025, 1, 1)), company.CeasedOn)
        Assert.Equal("Arkkitehtityö Person One Oy", company.Name)
        Assert.Equal<string list>([ "Arkkitehtityö Person Two, Person One Oy" ], company.OtherNames)
        Assert.Equal(ValueSome "Limited company", company.LegalForm)
        Assert.Equal(RecordRole.Home, company.Role)
    }

[<Fact>]
let ``a Finnish search holding a ceased company reads it, and one company that can't be read is left out, not failing the answer``
    ()
    : Task =
    task {
        // The search for "Arkkitehtityö Virtanen" as PRH answered it on 2026-11-08: 0357299-5, ceased, and 1704694-5, names cut to one.
        let body = fixture "finland-search-ended.json"
        let nameless = """{"businessId":{"value":"0000001-9"},"names":[]}"""

        use handler =
            new FakeHandler(fun _ -> ok (body.Replace("\"companies\":[", "\"companies\":[" + nameless + ",")))

        match! (Sources.finland (connectionOver handler)).SearchAsync("Arkkitehtityö Virtanen", fi, ct) with
        | Ok { Value = Available found } ->
            Assert.Equal<string list>([ "0357299-5"; "1704694-5" ], found.Matches |> List.map _.Number)
            Assert.Equal("Arkkitehtityö Person One Oy", found.Matches.Head.Name)
            Assert.Equal(ValueSome "4", found.Matches.Head.StatusAsPublished)
            Assert.Equal(ValueSome 2, found.TotalMatches)
            Assert.False found.Complete
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a Finnish answer with no company name at all is unreadable`` () : Task =
    task {
        use handler =
            new FakeHandler(fun _ ->
                ok """{"totalResults":1,"companies":[{"businessId":{"value":"0357299-5"},"names":[]}]}""")

        match! (Sources.finland (connectionOver handler)).TryFindAsync("0357299-5", fi, ct) with
        | Error(RegistryFailure.UnreadableAnswer _) -> ()
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``another company in Finland's answer fails the read`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "finland-company.json"))

        match! (Sources.finland (connectionOver handler)).TryFindAsync("1234567-8", fi, ct) with
        | Error(RegistryFailure.UnreadableAnswer _) -> ()
        | other -> failwith $"%A{other}"
    }

[<Theory; InlineData("abc"); InlineData("0112038"); InlineData("01120389")>]
let ``a value that can't be a Business ID isn't sent, since the API would answer it with an empty list``
    (number: string)
    : Task =
    task {
        use handler = new FakeHandler(fun _ -> failwith "Nothing should be sent")

        match! (Sources.finland (connectionOver handler)).TryFindAsync(number, fi, ct) with
        | Error(RegistryFailure.InvalidRequest _) -> Assert.Empty handler.Calls
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``Texas reads a row, with the dataset's date as of`` () : Task =
    task {
        let row = (fixture "texas-rows.json").Trim()

        let only =
            System.Text.Json.JsonDocument.Parse(row).RootElement.EnumerateArray()
            |> Seq.item 1

        use handler = new FakeHandler(fun _ -> ok $"[{only.GetRawText()}]")
        let! evidence = findIn (Sources.texas (connectionOver handler)) "800515257" tx
        let company = companyOf evidence
        Assert.Equal("DEEP EAST TEXAS USBC ASSOCIATION", company.Name)
        Assert.Equal("0800515257", company.Number)
        Assert.Equal(CompanyStanding.Active, company.Standing)
        Assert.Equal(RecordRole.Home, company.Role)
        Assert.Contains("secretary_of_state_sos_or_coa_file_number=0800515257", handler.Calls.Head.Target)
    }

[<Fact>]
let ``absence from the Texas dataset proves nothing`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok "[]")
        let! evidence = findIn (Sources.texas (connectionOver handler)) "0000000001" tx

        match evidence.Value with
        | NotAvailable reason -> Assert.Contains("proves nothing", reason)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``two Texas rows on one file number aren't told apart`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "texas-rows.json"))
        let! evidence = findIn (Sources.texas (connectionOver handler)) "0000884780" tx

        match evidence.Value with
        | NotAvailable reason -> Assert.Contains("2 rows", reason)
        | other -> failwith $"%A{other}"
    }

[<Theory>]
[<InlineData("A", "A", "Active")>]
[<InlineData("R", "A", "Active")>]
[<InlineData("A", "N", "ActiveWithConcern")>]
[<InlineData("A", "D", "ActiveWithConcern")>]
[<InlineData("A", "U", "ActiveWithConcern")>]
[<InlineData("A", "", "ActiveWithConcern")>]
[<InlineData("F", "N", "Closed")>]
[<InlineData("K", "N", "Closed")>]
[<InlineData("L", "N", "Closed")>]
[<InlineData("N", "N", "Closed")>]
[<InlineData("P", "N", "Closed")>]
[<InlineData("D", "N", "Dissolved")>]
[<InlineData("T", "D", "Closed")>]
[<InlineData("W", "D", "Closed")>]
[<InlineData("M", "D", "Closed")>]
[<InlineData("", "A", "Registered")>]
[<InlineData("", "N", "Unknown")>]
[<InlineData("Q", "A", "Unknown")>]
let ``a Texas status and right to transact have a standing, and a new code is unknown``
    (sos: string, right: string, expected: string)
    =
    Assert.StartsWith(expected, $"%A{Texas.standing sos right}")

[<Fact>]
let ``Texas holds forfeited records under its active taxpayers' name, and they don't read as active`` () =
    Assert.False(CompanyStanding.isActive (Texas.standing "F" "N"))
    Assert.False(CompanyStanding.isActive (Texas.standing "A" "N"))
    Assert.Equal(CompanyStanding.Closed "Forfeited Franchise Tax", Texas.standing "F" "N")

[<Theory>]
[<InlineData("CF", "U", "ForeignRegistration")>]
[<InlineData("CI", "U", "ForeignRegistration")>]
[<InlineData("CT", "V", "ForeignRegistration")>]
[<InlineData("CT", "U", "Home")>]
[<InlineData("CL", "U", "Home")>]
[<InlineData("CL", "X", "NotStated")>]
let ``a Texas row says whether it is a home record or a foreign registration``
    (organisation: string, recordType: string, expected: string)
    =
    Assert.Equal(expected, $"%A{Texas.role organisation recordType}")

[<Fact>]
let ``Texas codes answered with stray spaces and line ends are read`` () : Task =
    task {
        let body =
            """[{"taxpayer_number":"32000000000","taxpayer_name":"X","sos_status_code":"A\n","right_to_transact_business_code":"A ","secretary_of_state_sos_or_coa_file_number":"0000000002","record_type_code":"U "}]"""

        use handler = new FakeHandler(fun _ -> ok body)
        let! evidence = findIn (Sources.texas (connectionOver handler)) "2" tx
        Assert.Equal(CompanyStanding.Active, (companyOf evidence).Standing)
    }

[<Fact>]
let ``a Texas name search keeps a quote from ending the query`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok "[]")

        match! (Sources.texas (connectionOver handler)).SearchAsync("o'neil%", tx, ct) with
        | Ok { Value = Available found } ->
            Assert.Empty found.Matches
            let sent = Uri.UnescapeDataString handler.Calls.Head.Target
            Assert.Contains("taxpayer_name like '%O%NEIL%'", sent)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a short Texas search isn't complete, as the dataset holds almost no dissolved companies`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "texas-rows.json"))

        match! (Sources.texas (connectionOver handler)).SearchAsync("deep east texas", tx, ct) with
        | Ok { Value = Available found } ->
            // The fake answers every row; only the name starting with the term is kept.
            Assert.Equal<string list>([ "DEEP EAST TEXAS USBC ASSOCIATION" ], found.Matches |> List.map _.Name)
            Assert.False found.Complete
        | other -> failwith $"%A{other}"
    }

[<Theory; InlineData("abc"); InlineData("123456789012"); InlineData("")>]
let ``a value that can't be a Texas number isn't sent`` (number: string) : Task =
    task {
        use handler = new FakeHandler(fun _ -> failwith "Nothing should be sent")

        match! (Sources.texas (connectionOver handler)).TryFindAsync(number, tx, ct) with
        | Error(RegistryFailure.InvalidRequest _) -> Assert.Empty handler.Calls
        | other -> failwith $"%A{other}"
    }

// Fail closed, the same for every source: whatever comes back that can't be read is a failure, never a company and never "not held".

[<Theory; InlineData("not json"); InlineData(""); InlineData("<html>Service page</html>")>]
let ``an answer that isn't JSON is unreadable, on every source`` (body: string) : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok body)

        for registry, number, jurisdiction in allRegistries handler do
            match! registry.TryFindAsync(number, jurisdiction, ct) with
            | Error(RegistryFailure.UnreadableAnswer reason) -> Assert.DoesNotContain("html", reason.ToLowerInvariant())
            | other -> failwith $"%s{registry.Coverage.Registry}: %A{other}"
    }

[<Fact>]
let ``JSON of the wrong shape is unreadable, on every source`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok """{"unexpected":true}""")

        for registry, number, jurisdiction in allRegistries handler do
            match! registry.TryFindAsync(number, jurisdiction, ct) with
            | Error(RegistryFailure.UnreadableAnswer _) -> ()
            | other -> failwith $"%s{registry.Coverage.Registry}: %A{other}"
    }

[<Fact>]
let ``a record missing what identifies it is unreadable`` () : Task =
    task {
        use malta = new FakeHandler(fun _ -> ok """{"data":{"state":"Active"}}""")

        match! (Sources.malta (connectionOver malta)).TryFindAsync("C 1", mt, ct) with
        | Error(RegistryFailure.UnreadableAnswer _) -> ()
        | other -> failwith $"%A{other}"

        use maltaNoName =
            new FakeHandler(fun _ -> ok """{"data":{"state":"Active","registration_number":"C 1"}}""")

        match! (Sources.malta (connectionOver maltaNoName)).TryFindAsync("C 1", mt, ct) with
        | Error(RegistryFailure.UnreadableAnswer _) -> ()
        | other -> failwith $"%A{other}"

        use finland =
            new FakeHandler(fun _ ->
                ok """{"totalResults":1,"companies":[{"businessId":{"value":"0112038-9"},"names":[]}]}""")

        match! (Sources.finland (connectionOver finland)).TryFindAsync("0112038-9", fi, ct) with
        | Error(RegistryFailure.UnreadableAnswer _) -> ()
        | other -> failwith $"%A{other}"

        use texas = new FakeHandler(fun _ -> ok """[{"sos_status_code":"A"}]""")

        match! (Sources.texas (connectionOver texas)).TryFindAsync("1", tx, ct) with
        | Error(RegistryFailure.UnreadableAnswer _) -> ()
        | other -> failwith $"%A{other}"

        use texasNoName =
            new FakeHandler(fun _ ->
                ok """[{"sos_status_code":"A","secretary_of_state_sos_or_coa_file_number":"0000000001"}]""")

        match! (Sources.texas (connectionOver texasNoName)).TryFindAsync("1", tx, ct) with
        | Error(RegistryFailure.UnreadableAnswer _) -> ()
        | other -> failwith $"%A{other}"

        use texasNoNumber =
            new FakeHandler(fun _ -> ok """[{"taxpayer_name":"X","sos_status_code":"A"}]""")

        match! (Sources.texas (connectionOver texasNoNumber)).TryFindAsync("1", tx, ct) with
        | Error(RegistryFailure.UnreadableAnswer _) -> ()
        | other -> failwith $"%A{other}"
    }

[<Theory; InlineData(500); InlineData(503)>]
let ``a server failure is Unavailable, on every source`` (status: int) : Task =
    task {
        use handler = new FakeHandler(fun _ -> answer status "secret-body")

        for registry, number, jurisdiction in allRegistries handler do
            match! registry.TryFindAsync(number, jurisdiction, ct) with
            | Error(RegistryFailure.Unavailable reason) -> Assert.DoesNotContain("secret-body", reason)
            | other -> failwith $"%s{registry.Coverage.Registry}: %A{other}"
    }

[<Theory>]
[<InlineData(400)>]
[<InlineData(403)>]
[<InlineData(422)>]
[<InlineData(302)>]
let ``a refusal or redirect is RequestRefused, on every source`` (status: int) : Task =
    task {
        use handler = new FakeHandler(fun _ -> answer status "secret-body")

        for registry, number, jurisdiction in allRegistries handler do
            match! registry.TryFindAsync(number, jurisdiction, ct) with
            | Error(RegistryFailure.RequestRefused reason) -> Assert.DoesNotContain("secret-body", reason)
            | other -> failwith $"%s{registry.Coverage.Registry}: %A{other}"
    }

[<Fact>]
let ``a 404 that isn't a statement of not held is refused`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> notFound)

        match! (Sources.finland (connectionOver handler)).TryFindAsync("0112038-9", fi, ct) with
        | Error(RegistryFailure.RequestRefused _) -> ()
        | other -> failwith $"%A{other}"

        match! (Sources.texas (connectionOver handler)).TryFindAsync("1", tx, ct) with
        | Error(RegistryFailure.RequestRefused _) -> ()
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``no answer is Unavailable, and a cancelled token throws`` () : Task =
    task {
        use handler = new HangingHandler()

        let connection =
            SourceConnection(handler, (fun () -> fixedTime), (fun _ _ -> Task.CompletedTask))

        use cancelled = new CancellationTokenSource()
        do! cancelled.CancelAsync()

        let! thrown =
            Assert.ThrowsAnyAsync<OperationCanceledException>(fun () ->
                (Sources.malta connection).TryFindAsync("C 1", mt, cancelled.Token) :> Task)

        Assert.NotNull thrown

        use failing =
            new FakeHandler(fun _ -> raise (Net.Http.HttpRequestException "no route"))

        match! (Sources.malta (connectionOver failing)).TryFindAsync("C 1", mt, ct) with
        | Error(RegistryFailure.Unavailable _) -> ()
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a 429 is asked again after its wait, within the retries, then fails as rate limited`` () : Task =
    task {
        let retryAfter =
            {
                Status = 429
                Body = ""
                Headers = [ "Retry-After", "2" ]
            }

        use always = new FakeHandler(fun _ -> retryAfter)
        let clock = FakeClock()
        let waiting = SourceConnection(always, (fun () -> clock.Now), clock.Delay)

        match! (Sources.france waiting).TryFindAsync("552032534", fr, ct) with
        | Error(RegistryFailure.RateLimited(ValueSome wait)) ->
            Assert.Equal(TimeSpan.FromSeconds 2., wait)
            Assert.Equal(3, always.Calls.Length)
        | other -> failwith $"%A{other}"

        let tries = ref 0

        use once =
            new FakeHandler(fun _ ->
                tries.Value <- tries.Value + 1

                if tries.Value = 1 then
                    retryAfter
                else
                    ok (fixture "france-active.json"))

        let onceClock = FakeClock()

        let! evidence =
            findIn (Sources.france (SourceConnection(once, (fun () -> onceClock.Now), onceClock.Delay))) "552032534" fr

        Assert.Equal("DANONE", (companyOf evidence).Name)
        Assert.Equal<TimeSpan list>([ TimeSpan.FromSeconds 2. ], onceClock.Waits)
    }

[<Fact>]
let ``a wait past the longest allowed isn't made`` () : Task =
    task {
        use handler =
            new FakeHandler(fun _ ->
                {
                    Status = 429
                    Body = ""
                    Headers = [ "Retry-After", "3600" ]
                })

        let clock = FakeClock()
        let connection = SourceConnection(handler, (fun () -> clock.Now), clock.Delay)

        match! (Sources.malta connection).TryFindAsync("C 1", mt, ct) with
        | Error(RegistryFailure.RateLimited _) ->
            Assert.Equal(1, handler.Calls.Length)
            Assert.Empty clock.Waits
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``France's calls keep to seven a second`` () : Task =
    task {
        let clock = FakeClock()
        use handler = new FakeHandler(fun _ -> ok (fixture "france-active.json"))
        let connection = SourceConnection(handler, (fun () -> clock.Now), clock.Delay)
        let registry = Sources.france connection

        for _ in 1..8 do
            let! _ = registry.TryFindAsync("552032534", fr, ct)
            ()

        Assert.Equal(8, handler.Calls.Length)
        Assert.Equal(1, clock.Waits.Length)
        Assert.Equal(TimeSpan.FromSeconds 1., clock.Waits.Head)

        match registry.Coverage.RateLimit with
        | RateLimitPolicy.Published(7, per, _) -> Assert.Equal(TimeSpan.FromSeconds 1., per)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``outside its coverage every source fails, and sends nothing`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> failwith "Nothing should be sent")
        let elsewhere = Jurisdiction.ofCode "ZZ"

        for registry, number, _ in allRegistries handler do
            let! found = registry.TryFindAsync(number, elsewhere, ct)
            Assert.Equal(Error(RegistryFailure.OutsideCoverage elsewhere), found)
            let! officers = registry.OfficersAsync(number, elsewhere, ct)
            Assert.Equal(Error(RegistryFailure.OutsideCoverage elsewhere), officers)
            let! owners = registry.OwnersAsync(number, elsewhere, OwnerPurpose.PublicRead, ct)
            Assert.Equal(Error(RegistryFailure.OutsideCoverage elsewhere), owners)
            let! search = registry.SearchAsync("x", elsewhere, ct)
            Assert.Equal(Error(RegistryFailure.OutsideCoverage elsewhere), search)

        Assert.Empty handler.Calls
    }

[<Fact>]
let ``owners separate not published from needing entitlement, by purpose`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> failwith "Nothing should be sent")
        let connection = connectionOver handler

        for registry, number, jurisdiction in
            [
                Sources.malta connection, "C 1", mt
                Sources.france connection, "552032534", fr
                Sources.finland connection, "0112038-9", fi
            ] do
            match! registry.OwnersAsync(number, jurisdiction, OwnerPurpose.CustomerDueDiligence, ct) with
            | Ok { Value = NeedsEntitlement entitlement } -> Assert.NotEmpty entitlement.Route
            | other -> failwith $"%A{other}"

            match! registry.OwnersAsync(number, jurisdiction, OwnerPurpose.PublicRead, ct) with
            | Ok { Value = NotAvailable _ } -> ()
            | other -> failwith $"%A{other}"

        match! (Sources.texas connection).OwnersAsync("1", tx, OwnerPurpose.CustomerDueDiligence, ct) with
        | Ok { Value = NotAvailable _ } -> ()
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``each source declares what it costs, its pace and its licence`` () =
    use handler = new FakeHandler(fun _ -> failwith "Nothing should be sent")

    for registry, _, _ in allRegistries handler do
        Assert.Equal(AccessCost.Free, registry.Coverage.Cost)
        Assert.NotEmpty registry.Coverage.Licence

    let france = (Sources.france (connectionOver handler)).Coverage
    Assert.Contains("No data licence stated", france.Licence)

[<Fact>]
let ``GLEIF finds by a reported number, in that country only, with the data's date`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "gleif-by-number.json"))
        let check = Sources.gleif (connectionOver handler)

        match! check.ByRegistrationAsync("00102498", Jurisdiction.ofCode "GB", ct) with
        | Ok evidence ->
            Assert.Equal("GLEIF (cross-check)", evidence.Source)
            Assert.Equal(ValueSome(DateOnly(2026, 11, 6)), evidence.AsOf)
            Assert.Empty evidence.Value.Possible
            let record = evidence.Value.Matching.Head
            Assert.Equal("213800LH1BZH3DI6G760", record.Lei)
            Assert.Equal("BP P.L.C.", record.LegalName)
            Assert.Equal(ValueSome "ACTIVE", record.EntityStatus)
            Assert.Equal(ValueSome "00102498", record.RegisteredAs)
            Assert.Equal(ValueSome "SW1Y 4PD", record.LegalAddress.Value.PostalCode)
        | Error failure -> failwith (RegistryFailure.describe failure)

        match! check.ByRegistrationAsync("00102498", Jurisdiction.ofCode "FR", ct) with
        | Ok evidence ->
            Assert.Empty evidence.Value.Matching
            Assert.Empty evidence.Value.Possible
        | Error failure -> failwith (RegistryFailure.describe failure)
    }

[<Fact>]
let ``a GLEIF record with no legal name or LEI is unreadable`` () : Task =
    task {
        for body in
            [
                """{"data":{"attributes":{"lei":"529900W18LQJJN6SJ336","entity":{"status":"ACTIVE"}}}}"""
                """{"data":{"attributes":{"entity":{"legalName":{"name":"X"}}}}}"""
                """{"data":{"attributes":{"lei":"529900W18LQJJN6SJ336"}}}"""
                """{"data":{"id":"529900W18LQJJN6SJ336"}}"""
            ] do
            use handler = new FakeHandler(fun _ -> ok body)

            match! (Sources.gleif (connectionOver handler)).ByLeiAsync("529900W18LQJJN6SJ336", ct) with
            | Error(RegistryFailure.UnreadableAnswer _) -> ()
            | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a GLEIF list stating more than it holds is unreadable`` () : Task =
    task {
        use handler =
            new FakeHandler(fun _ -> ok """{"meta":{"pagination":{"total":30}},"data":[]}""")

        match! (Sources.gleif (connectionOver handler)).ByRegistrationAsync("1", Jurisdiction.ofCode "GB", ct) with
        | Error(RegistryFailure.UnreadableAnswer _) -> ()
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a GLEIF number reported by thousands is read a page at a time to the most read, keeping matches and counting the rest unread``
    ()
    : Task =
    task {
        // filter[entity.registeredAs]=1 as GLEIF answered it on 2026-11-08, stating 10,395 records; fields cut to those read, names redacted.
        use handler =
            new FakeHandler(fun call ->
                if (Uri.UnescapeDataString call.Target).Contains "page[number]=2" then
                    ok (fixture "gleif-number-1-page2.json")
                else
                    ok (fixture "gleif-number-1-page1.json"))

        match! (Sources.gleif (connectionOver handler)).ByRegistrationAsync("1", Jurisdiction.ofCode "CA", ct) with
        | Ok evidence ->
            Assert.Equal(GleifCrossCheck.MostRead, 200)
            Assert.Equal(10395 - 200, evidence.Value.Unread)
            Assert.Equal(9, evidence.Value.Matching.Length)
            Assert.Equal<string list>([ "CA-BC"; "CA-ON" ], evidence.Value.Possible |> List.map _.Jurisdiction.Value)
            Assert.Equal(2, handler.Calls.Length)
            Assert.All(handler.Calls, fun call -> Assert.Contains("page[size]=100", Uri.UnescapeDataString call.Target))
            Assert.StartsWith("[{", evidence.Raw.Value)
        | Error failure -> failwith (RegistryFailure.describe failure)
    }

[<Fact>]
let ``GLEIF gives a parent, a reporting exception or nothing, and a failure is none of those`` () : Task =
    task {
        let lei = "529900W18LQJJN6SJ336"

        use withParent = new FakeHandler(fun _ -> ok (fixture "gleif-parent.json"))

        match! (Sources.gleif (connectionOver withParent)).ParentAsync(lei, LeiParentLevel.Direct, ct) with
        | Ok { Value = LeiParent.Parent parent } ->
            Assert.Equal("SOCIETE GENERALE", parent.LegalName)
            Assert.Contains("/direct-parent", withParent.Calls.Head.Target)
        | other -> failwith $"%A{other}"

        use withException =
            new FakeHandler(fun call ->
                if call.Target.EndsWith "reporting-exception" then
                    ok (fixture "gleif-reporting-exception.json")
                else
                    answer 404 (fixture "gleif-no-exception.json"))

        match! (Sources.gleif (connectionOver withException)).ParentAsync(lei, LeiParentLevel.Ultimate, ct) with
        | Ok {
                 Value = LeiParent.ReportingException reason
             } ->
            Assert.Equal("NO_KNOWN_PERSON", reason)
            Assert.Contains("/ultimate-parent", withException.Calls.Head.Target)
        | other -> failwith $"%A{other}"

        use withNothing =
            new FakeHandler(fun _ -> answer 404 (fixture "gleif-no-exception.json"))

        match! (Sources.gleif (connectionOver withNothing)).ParentAsync(lei, LeiParentLevel.Direct, ct) with
        | Ok { Value = LeiParent.NothingReported } -> ()
        | other -> failwith $"%A{other}"

        use failing =
            new FakeHandler(fun call ->
                if call.Target.EndsWith "reporting-exception" then
                    answer 500 ""
                else
                    answer 404 (fixture "gleif-no-exception.json"))

        match! (Sources.gleif (connectionOver failing)).ParentAsync(lei, LeiParentLevel.Direct, ct) with
        | Error(RegistryFailure.Unavailable _) -> ()
        | other -> failwith $"%A{other}"

        use unreadable = new FakeHandler(fun _ -> ok "{}")

        match! (Sources.gleif (connectionOver unreadable)).ParentAsync(lei, LeiParentLevel.Direct, ct) with
        | Error(RegistryFailure.UnreadableAnswer _) -> ()
        | other -> failwith $"%A{other}"
    }

[<Theory; InlineData("TOO-SHORT"); InlineData("529900W18LQJJN6SJ33/"); InlineData(null)>]
let ``a value that can't be an LEI isn't sent`` (lei: string) : Task =
    task {
        use handler = new FakeHandler(fun _ -> failwith "Nothing should be sent")

        match! (Sources.gleif (connectionOver handler)).ByLeiAsync(lei, ct) with
        | Error(RegistryFailure.InvalidRequest _) -> Assert.Empty handler.Calls
        | other -> failwith $"%A{other}"
    }

[<Theory>]
[<InlineData("Good Standing", "Active")>]
[<InlineData("good standing", "Active")>]
[<InlineData("AR delinquent", "ActiveWithConcern")>]
[<InlineData("Void", "Closed")>]
[<InlineData("Forfeited", "Unknown")>]
[<InlineData("", "Unknown")>]
let ``Delaware's words map ready for a paid source, and any other is unknown`` (word: string, expected: string) =
    Assert.StartsWith(expected, $"%A{StandingWords.delaware word}")

[<Fact>]
let ``the declared routes name a lawful way in for each register no adapter reads`` () =
    let codes = CoverageRoutes.all |> List.map _.Jurisdiction.Code |> Set.ofList
    Assert.Equal<Set<string>>(set [ "US-DE"; "DE"; "ES"; "IT"; "LU" ], codes)

    for route in CoverageRoutes.all do
        Assert.NotEmpty route.Route
        Assert.NotEmpty route.WhyNoAdapter

    Assert.Contains("Hagen", CoverageRoutes.germany.Route)
    Assert.Equal(RouteKind.Entitlement, CoverageRoutes.germany.Kind)
    Assert.Equal(RouteKind.PaidContract, CoverageRoutes.delaware.Kind)

[<Theory>]
[<InlineData("Netherlands", "NL")>]
[<InlineData("England", "GB")>]
[<InlineData("England and Wales", "GB")>]
[<InlineData("Delaware", "US-DE")>]
[<InlineData("State of Delaware", "US-DE")>]
[<InlineData("West Virginia", "US-WV")>]
[<InlineData("Malta", "MT")>]
[<InlineData("LU", "LU")>]
let ``a register's wording places a jurisdiction`` (wording: string, expected: string) =
    Assert.Equal(ValueSome expected, JurisdictionWords.place [ ValueSome wording ] |> ValueOption.map _.Code)

[<Fact>]
let ``a bare United States takes its state from another wording`` () =
    Assert.Equal(
        ValueSome "US-DE",
        JurisdictionWords.place
            [
                ValueSome "United States"
                ValueSome "Delaware Secretary of State"
                ValueNone
            ]
        |> ValueOption.map _.Code
    )

    Assert.Equal(ValueSome "US", JurisdictionWords.place [ ValueSome "USA" ] |> ValueOption.map _.Code)

[<Theory; InlineData("Georgia"); InlineData("Atlantis"); InlineData("")>]
let ``wording that can't be placed stays unplaced`` (wording: string) =
    Assert.Equal(ValueNone, JurisdictionWords.place [ ValueSome wording ])

[<Theory>]
[<InlineData("New South Wales")>]
[<InlineData("South Wales")>]
[<InlineData("New England")>]
[<InlineData("Republic of Georgia")>]
[<InlineData("Tbilisi, Georgia")>]
[<InlineData("Laws of England")>]
[<InlineData("Virginia Beach")>]
let ``only a whole wording places a jurisdiction`` (wording: string) =
    Assert.Equal(ValueNone, JurisdictionWords.place [ ValueSome wording ])

[<Theory>]
[<InlineData("Delaware, USA", "US-DE")>]
[<InlineData("Georgia, USA", "US-GA")>]
[<InlineData("State of Georgia", "US-GA")>]
[<InlineData("Commonwealth of Virginia", "US-VA")>]
[<InlineData("Delaware Division of Corporations", "US-DE")>]
[<InlineData("Northern Ireland", "GB")>]
let ``a state's name with the words it comes with is still whole`` (wording: string, expected: string) =
    Assert.Equal(ValueSome expected, JurisdictionWords.place [ ValueSome wording ] |> ValueOption.map _.Code)

let private gleifRecordIn (jurisdiction: string) =
    let field =
        if jurisdiction = "" then
            ""
        else
            $"\"jurisdiction\":\"{jurisdiction}\","

    "{\"attributes\":{\"lei\":\"AAAAAAAAAAAAAAAAAA11\",\"entity\":{\"legalName\":{\"name\":\"ZED WORKS\"},"
    + field
    + "\"registeredAs\":\"123\"}}}"

let private leiMatchesFor (recordJurisdiction: string) (asked: string) =
    task {
        use handler =
            new FakeHandler(fun _ -> ok ("{\"data\":[" + gleifRecordIn recordJurisdiction + "]}"))

        let check = Sources.gleif (connectionOver handler)

        match! check.ByRegistrationAsync("123", Jurisdiction.ofCode asked, ct) with
        | Ok evidence -> return evidence.Value.Matching.Length, evidence.Value.Possible.Length
        | Error failure -> return failwith (RegistryFailure.describe failure)
    }

[<Theory>]
[<InlineData("US-TX", "US-TX", 1, 0)>]
[<InlineData("GB", "GB", 1, 0)>]
[<InlineData("GB-SCT", "GB", 0, 1)>]
[<InlineData("CA-ON", "CA", 0, 1)>]
[<InlineData("CA", "CA-ON", 0, 1)>]
[<InlineData("US-DE", "US-TX", 0, 1)>]
[<InlineData("US", "US-TX", 0, 1)>]
[<InlineData("GB", "GB-SCT", 0, 1)>]
[<InlineData("US-DE", "US", 0, 1)>]
[<InlineData("", "US-TX", 0, 1)>]
[<InlineData("FR", "GB", 0, 0)>]
let ``a GLEIF record matches the jurisdiction asked, and is otherwise only possible``
    (stated: string, asked: string, matching: int, possible: int)
    : Task =
    task {
        let! found = leiMatchesFor stated asked
        Assert.Equal((matching, possible), found)
    }

[<Fact>]
let ``a GLEIF record's other names are read`` () : Task =
    task {
        let record =
            (gleifRecordIn "GB")
                .Replace(
                    "\"registeredAs\"",
                    "\"otherNames\":[{\"name\":\"ZED OTHER\"}],\"transliteratedOtherNames\":[{\"name\":\"ZED LATIN\"}],\"registeredAs\""
                )

        use handler = new FakeHandler(fun _ -> ok ("{\"data\":[" + record + "]}"))

        match! (Sources.gleif (connectionOver handler)).ByRegistrationAsync("123", Jurisdiction.ofCode "GB", ct) with
        | Ok evidence ->
            Assert.Equal<string list>([ "ZED OTHER"; "ZED LATIN" ], evidence.Value.Matching.Head.OtherNames)
        | Error failure -> failwith (RegistryFailure.describe failure)
    }
