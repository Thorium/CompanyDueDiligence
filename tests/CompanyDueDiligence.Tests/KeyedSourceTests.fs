/// The sources that need a key or credentials, replayed from fixtures built from each source's documented answers, with people's names redacted.
/// None was run live. A key goes only where its source documents it, and never into a failure's text.
module KeyedSourceTests

open System
open System.IO
open System.Text
open System.Threading.Tasks
open Xunit
open CompanyDueDiligence
open Fake

let private fixture (name: string) =
    File.ReadAllText(Path.Combine(AppContext.BaseDirectory, "Fixtures", name))

let private fixedTime = DateTimeOffset(2026, 11, 6, 12, 0, 0, TimeSpan.Zero)

let private connectionOver (handler: FakeHandler) =
    SourceConnection(handler, (fun () -> fixedTime), (fun _ _ -> Task.CompletedTask))

let private key = SourceKey Key

[<Literal>]
let private ClientId = "the-client-id"

[<Literal>]
let private ClientSecret = "the-client-secret"

let private clientCredentials = ClientCredentials(ClientId, ClientSecret)

let private basic =
    "Basic "
    + Convert.ToBase64String(Encoding.UTF8.GetBytes($"{ClientId}:{ClientSecret}"))

let private ca = Jurisdiction.ofCode "CA"
let private nz = Jurisdiction.ofCode "NZ"
let private se = Jurisdiction.ofCode "SE"
let private jp = Jurisdiction.ofCode "JP"
let private ch = Jurisdiction.ofCode "CH"
let private usCa = Jurisdiction.ofCode "US-CA"

/// Bolagsverket's token endpoint answers with a token; anything else with answer.
let private withToken (answer: Call -> Answer) (call: Call) =
    if call.Host = "portal.api.bolagsverket.se" then
        ok
            """{"access_token":"the-access-token","scope":"vardefulla-datamangder:read","token_type":"Bearer","expires_in":3600}"""
    else
        answer call

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

/// Every keyed registry, with a number of the right shape.
let private allKeyed (handler: FakeHandler) =
    let connection = connectionOver handler

    [
        Sources.canada connection key, "1007", ca
        Sources.newZealand connection key, "9429000106078", nz
        Sources.sweden connection clientCredentials, "5560021361", se
        Sources.japan connection key, "5111101000006", jp
        Sources.zefix connection clientCredentials, "CHE-107.721.785", ch
        Sources.california connection key, "202150010654", usCa
    ]

let private noSecretIn (text: string) =
    Assert.DoesNotContain(Key, text)
    Assert.DoesNotContain(ClientSecret, text)
    Assert.DoesNotContain("secret-body", text)

// Canada

[<Fact>]
let ``Canada reads a corporation in English, with the key in the user-key header`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "canada-corporation.json"))
        let! evidence = findIn (Sources.canada (connectionOver handler) key) "000100-7" ca
        let company = companyOf evidence
        Assert.Equal("Corporations Canada (federal)", evidence.Source)
        Assert.Equal(fixedTime, evidence.RetrievedAt)
        Assert.Equal("Abbotsford Chamber of Commerce", company.Name)
        Assert.Equal("1007", company.Number)
        Assert.Equal(CompanyStanding.Active, company.Standing)
        Assert.Equal(RecordRole.Home, company.Role)
        Assert.Equal(ValueSome(DateOnly(1947, 1, 10)), company.IncorporatedOn)
        Assert.Equal(2, company.OtherNames.Length)
        Assert.Equal(ValueSome "Boards of Trade Act - Part II", company.LegalForm)
        Assert.StartsWith("{\"corporationId\":\"1007\"", evidence.Raw.Value)
        Assert.Equal(ValueNone, evidence.AsOf)

        match company.Addresses with
        | Available [ office ] ->
            Assert.Equal(AddressKind.RegisteredOffice, office.Kind)
            Assert.Equal(ValueSome "BC", office.Address.Region)
        | other -> failwith $"%A{other}"

        let call = handler.Calls.Head
        Assert.Equal("apigateway-passerelledapi.ised-isde.canada.ca", call.Host)
        Assert.Equal("/corporations/api/v1/corporations/1007.json?lang=eng", call.Target)
        Assert.Equal(Key, call.Headers["user-key"])
    }

[<Fact>]
let ``Canada looks up a Business Number by its nine-digit root`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "canada-corporation.json"))

        let! evidence =
            findIn (Sources.canada (connectionOver handler) key) "106679285RC0001" ca

        Assert.Equal("1007", (companyOf evidence).Number)
        Assert.Equal("/corporations/api/v1/corporations/106679285.json?lang=eng", handler.Calls.Head.Target)
    }

[<Fact>]
let ``a record holding no Business Number isn't taken for the one asked`` () : Task =
    task {
        let numberless =
            Text.RegularExpressions.Regex.Replace(
                fixture "canada-corporation.json",
                "\"businessNumbers\":\\{[^}]*\\},?",
                ""
            )

        Assert.DoesNotContain("businessNumbers", numberless)
        use handler = new FakeHandler(fun _ -> ok numberless)

        match! (Sources.canada (connectionOver handler) key).TryFindAsync("106679285", ca, ct) with
        | Error(RegistryFailure.UnreadableAnswer reason) -> Assert.Contains("business number", reason)
        | other -> failwith $"%A{other}"

        let! byCorporation = findIn (Sources.canada (connectionOver handler) key) "1007" ca
        Assert.Equal("1007", (companyOf byCorporation).Number)
    }

[<Fact>]
let ``Canada's could-not-find message reads as not held`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "canada-not-found.json"))
        let! evidence = findIn (Sources.canada (connectionOver handler) key) "1234567" ca
        Assert.Equal(Available ValueNone, evidence.Value)
    }

[<Fact>]
let ``another corporation in Canada's answer fails the read`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "canada-corporation.json"))

        match! (Sources.canada (connectionOver handler) key).TryFindAsync("2008", ca, ct) with
        | Error(RegistryFailure.UnreadableAnswer _) -> ()
        | other -> failwith $"%A{other}"
    }

[<Theory>]
[<InlineData("Active", "Active")>]
[<InlineData("Active - Discontinuance Pending", "ActiveWithConcern")>]
[<InlineData("Active - Dissolution Pending (non-compliance)", "ActiveWithConcern")>]
[<InlineData("Active - Intent to Dissolve Filed", "WindingUp")>]
[<InlineData("Dissolved", "Dissolved")>]
[<InlineData("Dissolved by Corporations Canada", "Dissolved")>]
[<InlineData("Dissolved by court order", "Dissolved")>]
[<InlineData("Dissolved by the corporation", "Dissolved")>]
[<InlineData("Dissolved for non-compliance", "Dissolved")>]
[<InlineData("Dissolved – Part II of CCA", "Dissolved")>]
[<InlineData("Inactive", "Closed")>]
[<InlineData("Inactive - Amalgamated", "Closed")>]
[<InlineData("Inactive - Discontinued", "Closed")>]
[<InlineData("Actif", "Unknown")>]
[<InlineData("A word added later", "Unknown")>]
let ``every Canadian status word has a standing, and a new one is unknown`` (word: string, expected: string) =
    Assert.StartsWith(expected, $"%A{Canada.standing (ValueSome word)}")

[<Theory>]
[<InlineData("")>]
[<InlineData("abc")>]
[<InlineData("123456789")>]
[<InlineData("1234567890")>]
[<InlineData("0000000")>]
[<InlineData(null)>]
let ``a value that can't be a Canadian number isn't sent`` (number: string) : Task =
    task {
        use handler = new FakeHandler(fun _ -> failwith "Nothing should be sent")

        match! (Sources.canada (connectionOver handler) key).TryFindAsync(number, ca, ct) with
        | Error(RegistryFailure.InvalidRequest _) -> Assert.Empty handler.Calls
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``Canada answers for CA itself, not for a province`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> failwith "Nothing should be sent")
        let ontario = Jurisdiction.ofCode "CA-ON"
        let registry = Sources.canada (connectionOver handler) key
        let! found = registry.TryFindAsync("1007", ontario, ct)
        Assert.Equal(Error(RegistryFailure.OutsideCoverage ontario), found)
        let! officers = registry.OfficersAsync("1007", ontario, ct)
        Assert.Equal(Error(RegistryFailure.OutsideCoverage ontario), officers)
        Assert.Empty handler.Calls
    }

[<Fact>]
let ``Canada reads directors by corporation number, in English, and not by Business Number`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "canada-directors.json"))
        let registry = Sources.canada (connectionOver handler) key

        match! registry.OfficersAsync("1007", ca, ct) with
        | Ok { Value = Available [ director ] } ->
            Assert.Equal("PERSON ONE", director.Name)
            Assert.Equal(OfficerRole.Director, director.Role)
            Assert.Equal(PartyKind.Individual, director.Kind)
            Assert.True director.Current
            Assert.Equal(ValueSome "Canada", director.Address.Value.Country)
        | other -> failwith $"%A{other}"

        let call = handler.Calls.Head
        Assert.Equal("/corporations/api/v2/corporations/1007/directors", call.Target)
        Assert.Equal("en", call.Headers["Accept-Language"])
        Assert.Equal(Key, call.Headers["user-key"])

        use none = new FakeHandler(fun _ -> failwith "Nothing should be sent")

        match! (Sources.canada (connectionOver none) key).OfficersAsync("106679285", ca, ct) with
        | Error(RegistryFailure.InvalidRequest _) -> Assert.Empty none.Calls
        | other -> failwith $"%A{other}"

        use empty = new FakeHandler(fun _ -> ok """{"_links":{"self":{"href":"x"}}}""")

        match! (Sources.canada (connectionOver empty) key).OfficersAsync("1007", ca, ct) with
        | Ok { Value = NotAvailable _ } -> ()
        | other -> failwith $"%A{other}"
    }

// New Zealand

[<Fact>]
let ``New Zealand reads an entity, with the key in the subscription header and the source's date`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "nz-entity.json"))

        let! evidence =
            findIn (Sources.newZealand (connectionOver handler) key) "9429000106078" nz

        let company = companyOf evidence
        Assert.Equal("EXAMPLE HOLDINGS LIMITED", company.Name)
        Assert.Equal("9429000106078", company.Number)
        Assert.Equal(CompanyStanding.Registered(ValueSome "Registered"), company.Standing)
        Assert.Equal(RecordRole.Home, company.Role)
        Assert.Equal(ValueSome "NZ Limited Company", company.LegalForm)
        Assert.Equal(ValueSome(DateOnly(1998, 5, 12)), company.IncorporatedOn)
        Assert.Equal(ValueSome(DateOnly(2026, 10, 30)), evidence.AsOf)
        Assert.Equal<string list>([ "EXAMPLE TRADING" ], company.OtherNames)
        Assert.True evidence.Raw.IsSome

        match company.Addresses with
        | Available [ registered; service ] ->
            Assert.Equal(AddressKind.RegisteredOffice, registered.Kind)
            Assert.Equal<string list>([ "Level 2"; "1 Example Street"; "Wellington" ], registered.Address.Lines)
            Assert.Equal(AddressKind.Service, service.Kind)
        | other -> failwith $"%A{other}"

        let call = handler.Calls.Head
        Assert.Equal("api.business.govt.nz", call.Host)
        Assert.Equal("/gateway/nzbn/v5/entities/9429000106078", call.Target)
        Assert.Equal(Key, call.Headers["Ocp-Apim-Subscription-Key"])
    }

[<Fact>]
let ``New Zealand's 404 for a well-formed NZBN reads as not held`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> answer 404 (fixture "nz-not-found.json"))

        let! evidence =
            findIn (Sources.newZealand (connectionOver handler) key) "9429000106078" nz

        Assert.Equal(Available ValueNone, evidence.Value)
        Assert.Equal(ValueNone, evidence.Raw)
    }

[<Theory>]
[<InlineData("")>]
[<InlineData("<html>Resource not found</html>")>]
[<InlineData("""{"statusCode":404,"message":"Resource not found"}""")>]
[<InlineData("""{"status":"404","errorCode":"NZBN-400"}""")>]
let ``a New Zealand 404 without the register's NZBN-404 code isn't not held`` (body: string) : Task =
    task {
        use handler = new FakeHandler(fun _ -> answer 404 body)
        let registry = Sources.newZealand (connectionOver handler) key

        match! registry.TryFindAsync("9429000106078", nz, ct) with
        | Error(RegistryFailure.RequestRefused _) -> ()
        | other -> failwith $"%A{other}"

        match! registry.OwnersAsync("9429000106078", nz, OwnerPurpose.PublicRead, ct) with
        | Error(RegistryFailure.RequestRefused _) -> ()
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a New Zealand shareholding with no current shareholder is NotAvailable, not a listing with no owner`` () : Task =
    task {
        let unallocated =
            Text.RegularExpressions.Regex.Replace(
                fixture "nz-entity.json",
                "\"shareAllocation\":\\[.*?\\],\"historicShareholder\"",
                "\"shareAllocation\":[{\"allocation\":1000}],\"historicShareholder\""
            )

        Assert.DoesNotContain("individualShareholder\"", unallocated.Replace("historicIndividualShareholder", ""))
        use handler = new FakeHandler(fun _ -> ok unallocated)

        match!
            (Sources.newZealand (connectionOver handler) key)
                .OwnersAsync("9429000106078", nz, OwnerPurpose.PublicRead, ct)
        with
        | Ok { Value = NotAvailable reason } -> Assert.Contains("no current shareholder", reason)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a New Zealand corporate officer with an NZBN is placed in NZ, so it can be followed`` () : Task =
    task {
        let corporate =
            (fixture "nz-entity.json")
                .Replace(
                    "\"roles\":[",
                    "\"roles\":[{\"roleType\":\"Shareholder\",\"roleStatus\":\"ACTIVE\",\"roleEntity\":{\"entityName\":\"EXAMPLE PARENT LIMITED\",\"nzbn\":\"9429031645126\"}},{\"roleType\":\"Trustee\",\"roleStatus\":\"ACTIVE\",\"roleEntity\":{\"entityName\":\"UNNUMBERED TRUST\"}},"
                )

        use handler = new FakeHandler(fun _ -> ok corporate)

        match! (Sources.newZealand (connectionOver handler) key).OfficersAsync("9429000106078", nz, ct) with
        | Ok {
                 Value = Available(numbered :: unnumbered :: _)
             } ->
            Assert.Equal(ValueSome "NZ", numbered.Entity.Value.Country)
            Assert.Equal(ValueSome "9429031645126", numbered.Entity.Value.RegistrationNumber)
            Assert.Equal(ValueNone, unnumbered.Entity.Value.Country)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a New Zealand company number is found through the search, among companies only`` () : Task =
    task {
        use handler =
            new FakeHandler(fun call ->
                if call.Target.Contains "search-term" then
                    ok (fixture "nz-search-company-number.json")
                else
                    ok (fixture "nz-entity.json"))

        let! evidence = findIn (Sources.newZealand (connectionOver handler) key) "123456" nz
        Assert.Equal("9429000106078", (companyOf evidence).Number)

        match handler.Calls with
        | [ search; entity ] ->
            Assert.Equal("/gateway/nzbn/v5/entities?search-term=123456&page-size=50", search.Target)
            Assert.Equal("/gateway/nzbn/v5/entities/9429000106078", entity.Target)
            Assert.Equal(Key, entity.Headers["Ocp-Apim-Subscription-Key"])
        | other -> failwith $"%A{other}"

        use nothing = new FakeHandler(fun _ -> ok """{"totalItems":0,"items":[]}""")
        let! missing = findIn (Sources.newZealand (connectionOver nothing) key) "999" nz

        match missing.Value with
        | NotAvailable reason -> Assert.Contains("proves nothing", reason)
        | other -> failwith $"%A{other}"
    }

[<Theory>]
[<InlineData("50", "Registered")>]
[<InlineData("55", "InInsolvency")>]
[<InlineData("56", "InInsolvency")>]
[<InlineData("57", "InInsolvency")>]
[<InlineData("60", "InInsolvency")>]
[<InlineData("61", "InInsolvency")>]
[<InlineData("62", "Closed")>]
[<InlineData("63", "InInsolvency")>]
[<InlineData("64", "InInsolvency")>]
[<InlineData("65", "InInsolvency")>]
[<InlineData("66", "InInsolvency")>]
[<InlineData("70", "InInsolvency")>]
[<InlineData("71", "InInsolvency")>]
[<InlineData("72", "InInsolvency")>]
[<InlineData("80", "Closed")>]
[<InlineData("90", "Registered")>]
[<InlineData("91", "Closed")>]
[<InlineData("99", "Unknown")>]
let ``every NZBN status code has a standing, and a new one is unknown`` (code: string, expected: string) =
    Assert.StartsWith(expected, $"%A{NewZealand.standing (ValueSome code) ValueNone false}")

[<Fact>]
let ``a registered New Zealand company with removal commenced has a concern`` () =
    Assert.Equal(
        CompanyStanding.ActiveWithConcern "Removal commenced",
        NewZealand.standing (ValueSome "50") ValueNone true
    )

[<Fact>]
let ``New Zealand reads directors and shareholders, current and former`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "nz-entity.json"))
        let registry = Sources.newZealand (connectionOver handler) key

        match! registry.OfficersAsync("9429000106078", nz, ct) with
        | Ok {
                 Value = Available [ current; former ]
             } ->
            Assert.Equal("PERSON ONE", current.Name)
            Assert.True current.Current
            Assert.Equal(OfficerRole.Director, current.Role)
            Assert.Equal("PERSON M TWO", former.Name)
            Assert.False former.Current
            Assert.Equal(ValueSome(DateOnly(2015, 7, 1)), former.EndedOn)
        | other -> failwith $"%A{other}"

        match! registry.OwnersAsync("9429000106078", nz, OwnerPurpose.CustomerDueDiligence, ct) with
        | Ok { Value = Available listing } ->
            Assert.True(OwnerListing.isComplete listing)

            match listing.Owners with
            | [ person; parent; formerOwner ] ->
                Assert.Equal(ValueSome "PERSON ONE", person.Name)
                Assert.Equal(PartyKind.Individual, person.Kind)
                Assert.Equal(ValueSome { From = 60; To = 60 }, person.Control.Head.Band)
                Assert.Equal("600 of 1000 shares", person.Control.Head.AsPublished)
                Assert.Equal(PartyKind.LegalEntity, parent.Kind)
                Assert.Equal(ValueSome "9429031645126", parent.Entity.Value.RegistrationNumber)
                Assert.Equal(ValueSome "NZ", parent.Entity.Value.Country)
                Assert.Equal(ValueNone, person.Entity)
                Assert.False formerOwner.Current
                Assert.Equal(ValueSome "PERSON THREE", formerOwner.Name)
            | other -> failwith $"%A{other}"
        | other -> failwith $"%A{other}"

        let extensive =
            (fixture "nz-entity.json").Replace("\"extensiveShareholding\":false", "\"extensiveShareholding\":true")

        use listed = new FakeHandler(fun _ -> ok extensive)

        match!
            (Sources.newZealand (connectionOver listed) key)
                .OwnersAsync("9429000106078", nz, OwnerPurpose.PublicRead, ct)
        with
        | Ok { Value = Available listing } ->
            Assert.Equal<OwnershipGap list>([ OwnershipGap.OwnerNotIdentified ], listing.Gaps)
        | other -> failwith $"%A{other}"
    }

[<Theory>]
[<InlineData("9429000106077")>]
[<InlineData("94290001060")>]
[<InlineData("abc")>]
[<InlineData("123456789")>]
[<InlineData("")>]
let ``a value that can't be an NZBN or company number isn't sent`` (number: string) : Task =
    task {
        use handler = new FakeHandler(fun _ -> failwith "Nothing should be sent")

        match! (Sources.newZealand (connectionOver handler) key).TryFindAsync(number, nz, ct) with
        | Error(RegistryFailure.InvalidRequest _) -> Assert.Empty handler.Calls
        | other -> failwith $"%A{other}"
    }

// Sweden

[<Fact>]
let ``Sweden posts the number with a bearer token got by client credentials`` () : Task =
    task {
        use handler =
            new FakeHandler(withToken (fun _ -> ok (fixture "sweden-active.json")))

        let! evidence =
            findIn (Sources.sweden (connectionOver handler) clientCredentials) "556002-1361" se

        let company = companyOf evidence
        Assert.Equal("Testbolaget AB", company.Name)
        Assert.Equal("5560021361", company.Number)
        Assert.Equal(CompanyStanding.Active, company.Standing)
        Assert.Equal(RecordRole.Home, company.Role)
        Assert.Equal(ValueSome "Aktiebolag", company.LegalForm)
        Assert.Equal(ValueSome(DateOnly(1898, 2, 28)), company.IncorporatedOn)
        Assert.Equal(ValueSome "Sverige", company.JurisdictionAsPublished)
        Assert.StartsWith("{\"organisationsidentitet\"", evidence.Raw.Value)

        match company.Addresses with
        | Available [ postal ] ->
            Assert.Equal(AddressKind.Postal, postal.Kind)
            Assert.Equal(ValueSome "SUNDSVALL", postal.Address.Locality)
        | other -> failwith $"%A{other}"

        match handler.Calls with
        | [ token; lookup ] ->
            Assert.Equal("/oauth2/token", token.Target)
            Assert.Equal(basic, token.Headers["Authorization"])
            Assert.Equal("gw.api.bolagsverket.se", lookup.Host)
            Assert.Equal("/vardefulla-datamangder/v1/organisationer", lookup.Target)
            Assert.Equal("Bearer the-access-token", lookup.Headers["Authorization"])
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``Sweden's documented example is dissolved by liquidation, with bankruptcy stated`` () : Task =
    task {
        use handler =
            new FakeHandler(withToken (fun _ -> ok (fixture "sweden-aktiebolag.json")))

        let! evidence =
            findIn (Sources.sweden (connectionOver handler) clientCredentials) "5299999994" se

        let company = companyOf evidence
        Assert.Equal("Cykelbolaget AB", company.Name)
        Assert.Equal<string list>([ "Mopedbolaget AB" ], company.OtherNames)
        Assert.Equal(CompanyStanding.Dissolved, company.Standing)
        Assert.Equal(ValueSome(DateOnly(2023, 5, 5)), company.CeasedOn)
        Assert.Equal(ValueSome(DateOnly(2000, 1, 23)), company.IncorporatedOn)
        Assert.Equal<CompanyConcern list>([ CompanyConcern.InsolvencyHistory ], company.Concerns.Stated)
        Assert.Equal(ValueSome "avregistreringsorsak LIAV Likvidation", company.StatusAsPublished)
    }

[<Fact>]
let ``Sweden's ORGANISATION_FINNS_EJ isn't not held, and a producer out of reach fails`` () : Task =
    task {
        use notHeld =
            new FakeHandler(withToken (fun _ -> ok (fixture "sweden-not-held.json")))

        let! evidence =
            findIn (Sources.sweden (connectionOver notHeld) clientCredentials) "5560021361" se

        match evidence.Value with
        | NotAvailable reason -> Assert.Contains("doesn't prove", reason)
        | other -> failwith $"%A{other}"

        use down =
            new FakeHandler(withToken (fun _ -> ok (fixture "sweden-producer-down.json")))

        match! (Sources.sweden (connectionOver down) clientCredentials).TryFindAsync("5560021361", se, ct) with
        | Error(RegistryFailure.Unavailable _) -> ()
        | other -> failwith $"%A{other}"
    }

[<Theory>]
[<InlineData("pagaendeAvvecklingsEllerOmstruktureringsforfarande")>]
[<InlineData("avregistreringsorsak")>]
[<InlineData("avregistreradOrganisation")>]
[<InlineData("verksamOrganisation")>]
let ``a Swedish block deciding standing that answers with a fault leaves the standing unknown, not active``
    (block: string)
    : Task =
    task {
        let faulted (kind: string) =
            let body = fixture "sweden-active.json"

            let faultBlock =
                $"\"{block}\":{{\"fel\":{{\"typ\":\"{kind}\",\"felBeskrivning\":\"x\"}},\"dataproducent\":\"Bolagsverket\"}}"

            match block with
            | "verksamOrganisation" ->
                body.Replace(
                    "\"verksamOrganisation\":{\"kod\":\"JA\",\"fel\":null,\"dataproducent\":\"SCB\"}",
                    faultBlock
                )
            | _ -> body.Replace($"\"{block}\":null", faultBlock)

        Assert.Contains("\"felBeskrivning\":\"x\"", faulted "ANNAT_FEL")

        use other = new FakeHandler(withToken (fun _ -> ok (faulted "ANNAT_FEL")))

        let! evidence =
            findIn (Sources.sweden (connectionOver other) clientCredentials) "5560021361" se

        let company = companyOf evidence

        match company.Standing with
        | CompanyStanding.Unknown(ValueSome reason) -> Assert.Contains(block, reason)
        | standing -> failwith $"%A{standing}"

        use down =
            new FakeHandler(withToken (fun _ -> ok (faulted "OTILLGANGLIG_UPPGIFTSKALLA")))

        match! (Sources.sweden (connectionOver down) clientCredentials).TryFindAsync("5560021361", se, ct) with
        | Error(RegistryFailure.Unavailable _) -> ()
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a Swedish date out of range is no date, and a deregistration date out of range is unreadable`` () : Task =
    task {
        let farIncorporation =
            (fixture "sweden-active.json")
                .Replace("\"registreringsdatum\":\"-2267049600000\"", "\"registreringsdatum\":\"9000000000000000000\"")

        Assert.Contains("9000000000000000000", farIncorporation)
        use handler = new FakeHandler(withToken (fun _ -> ok farIncorporation))

        let! evidence =
            findIn (Sources.sweden (connectionOver handler) clientCredentials) "5560021361" se

        Assert.Equal(ValueNone, (companyOf evidence).IncorporatedOn)

        let farRemoval =
            (fixture "sweden-active.json")
                .Replace(
                    "\"avregistreradOrganisation\":null",
                    "\"avregistreradOrganisation\":{\"avregistreringsdatum\":-9000000000000000000}"
                )

        use removed = new FakeHandler(withToken (fun _ -> ok farRemoval))

        match! (Sources.sweden (connectionOver removed) clientCredentials).TryFindAsync("5560021361", se, ct) with
        | Error(RegistryFailure.UnreadableAnswer _) -> ()
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``Sweden asks for a new token once when the gateway refuses the held one`` () : Task =
    task {
        let tokens =
            Collections.Concurrent.ConcurrentQueue<string>([ "first-token"; "second-token" ])

        use handler =
            new FakeHandler(fun call ->
                if call.Host = "portal.api.bolagsverket.se" then
                    match tokens.TryDequeue() with
                    | true, token -> ok $"""{{"access_token":"{token}","token_type":"Bearer","expires_in":3600}}"""
                    | _ -> answer 401 "no more tokens"
                elif call.Headers["Authorization"] = "Bearer second-token" then
                    ok (fixture "sweden-active.json")
                else
                    answer 401 "token revoked")

        let registry = Sources.sweden (connectionOver handler) clientCredentials
        let! evidence = findIn registry "5560021361" se
        Assert.Equal("5560021361", (companyOf evidence).Number)

        Assert.Equal<string list>(
            [
                "portal.api.bolagsverket.se"
                "gw.api.bolagsverket.se"
                "portal.api.bolagsverket.se"
                "gw.api.bolagsverket.se"
            ],
            handler.Calls |> List.map _.Host
        )

        let! _ = findIn registry "5560021361" se
        Assert.Equal(5, handler.Calls.Length)

        use refusing = new FakeHandler(withToken (fun _ -> answer 401 "secret-body"))

        match! (Sources.sweden (connectionOver refusing) clientCredentials).TryFindAsync("5560021361", se, ct) with
        | Error(RegistryFailure.AccessRefused reason) ->
            noSecretIn reason
            Assert.Equal(4, refusing.Calls.Length)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``Sweden's token is asked for once and shared`` () : Task =
    task {
        use handler =
            new FakeHandler(withToken (fun _ -> ok (fixture "sweden-active.json")))

        let registry = Sources.sweden (connectionOver handler) clientCredentials
        let! _ = findIn registry "5560021361" se
        let! _ = findIn registry "5560021361" se

        Assert.Equal(
            1,
            handler.Calls
            |> List.filter (fun call -> call.Host = "portal.api.bolagsverket.se")
            |> List.length
        )
    }

[<Theory>]
[<InlineData("LIAV", "Dissolved")>]
[<InlineData("KKAV", "Dissolved")>]
[<InlineData("AKEJH", "Closed")>]
[<InlineData("ARSEED", "Closed")>]
[<InlineData("AVREG", "Closed")>]
[<InlineData("BABAKEJH", "Closed")>]
[<InlineData("DELAV", "Closed")>]
[<InlineData("DOM", "Closed")>]
[<InlineData("FUAV", "Closed")>]
[<InlineData("GROMAV", "Closed")>]
[<InlineData("NYINN", "Closed")>]
[<InlineData("OMAV", "Closed")>]
[<InlineData("OMBAB", "Closed")>]
[<InlineData("OVERK", "Closed")>]
[<InlineData("UTLKKLI", "Closed")>]
[<InlineData("VERKUPP", "Closed")>]
[<InlineData("VDSAK", "Closed")>]
[<InlineData("NYKOD", "Unknown")>]
let ``every Swedish deregistration reason has a standing, and a new one is unknown`` (code: string, expected: string) =
    Assert.StartsWith(expected, $"""%A{Sweden.standing (ValueSome(code, ValueNone)) [] (ValueSome "JA")}""")

[<Theory>]
[<InlineData("KK", "InInsolvency")>]
[<InlineData("FR", "InInsolvency")>]
[<InlineData("AC", "InInsolvency")>]
[<InlineData("RES", "InInsolvency")>]
[<InlineData("LI", "WindingUp")>]
[<InlineData("FUOL", "WindingUp")>]
[<InlineData("DEOL", "WindingUp")>]
[<InlineData("FUOT", "ActiveWithConcern")>]
[<InlineData("DEOT", "ActiveWithConcern")>]
[<InlineData("OM", "ActiveWithConcern")>]
[<InlineData("GROM", "ActiveWithConcern")>]
[<InlineData("NY", "Unknown")>]
let ``every Swedish proceeding has a standing, and a new one is unknown`` (code: string, expected: string) =
    Assert.StartsWith(expected, $"""%A{Sweden.standing ValueNone [ code, code ] (ValueSome "JA")}""")

[<Fact>]
let ``the gravest Swedish proceeding decides, and SCB's flag only when there is none`` () =
    Assert.Equal(
        CompanyStanding.InInsolvency "Konkurs",
        Sweden.standing ValueNone [ "LI", "Likvidation"; "KK", "Konkurs" ] ValueNone
    )

    Assert.Equal(CompanyStanding.Active, Sweden.standing ValueNone [] (ValueSome "JA"))
    Assert.False(CompanyStanding.isActive (Sweden.standing ValueNone [] (ValueSome "NEJ")))
    Assert.Equal(CompanyStanding.Registered ValueNone, Sweden.standing ValueNone [] ValueNone)

[<Theory>]
[<InlineData("5560021362")>]
[<InlineData("556002136")>]
[<InlineData("194009272719")>]
[<InlineData("811218-9876")>]
[<InlineData("abc")>]
[<InlineData(null)>]
let ``a value that can't be a Swedish organisation number isn't sent`` (number: string) : Task =
    task {
        use handler = new FakeHandler(fun _ -> failwith "Nothing should be sent")

        match! (Sources.sweden (connectionOver handler) clientCredentials).TryFindAsync(number, se, ct) with
        | Error(RegistryFailure.InvalidRequest _) -> Assert.Empty handler.Calls
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``Sweden refused credentials at the token endpoint are AccessRefused, holding no secret`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> answer 401 "secret-body")

        match! (Sources.sweden (connectionOver handler) clientCredentials).TryFindAsync("5560021361", se, ct) with
        | Error(RegistryFailure.AccessRefused reason as failure) ->
            noSecretIn reason
            noSecretIn (RegistryFailure.describe failure)
            Assert.Equal("portal.api.bolagsverket.se", handler.Calls.Head.Host)
        | other -> failwith $"%A{other}"
    }

// Japan

[<Fact>]
let ``Japan reads the documented sample, with the application ID in the query only`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "japan-corporation.xml"))

        let! evidence =
            findIn (Sources.japan (connectionOver handler) key) "5111101000006" jp

        let company = companyOf evidence
        Assert.Equal("株式会社検索対象除外", company.Name)
        Assert.Equal("5111101000006", company.Number)
        Assert.Equal(CompanyStanding.Registered ValueNone, company.Standing)
        Assert.Equal(RecordRole.Home, company.Role)
        Assert.Equal(ValueSome "Kabushiki kaisha", company.LegalForm)
        Assert.Equal(ValueSome "process 01", company.StatusAsPublished)
        Assert.Equal(ValueSome(DateOnly(2019, 4, 5)), evidence.AsOf)
        Assert.StartsWith("<corporation>", evidence.Raw.Value)
        Assert.DoesNotContain(Key, evidence.Raw.Value)

        match company.Addresses with
        | Available [ office ] ->
            Assert.Equal(AddressKind.RegisteredOffice, office.Kind)
            Assert.Equal<string list>([ "東京都千代田区（東京市神田区小川町一丁目１０番地）" ], office.Address.Lines)
            Assert.Equal(ValueSome "1000000", office.Address.PostalCode)
        | other -> failwith $"%A{other}"

        let call = handler.Calls.Head
        Assert.Equal("api.houjin-bangou.nta.go.jp", call.Host)
        Assert.Equal($"/4/num?id={Key}&number=5111101000006&type=12&history=0", call.Target)
        Assert.False(call.Headers.ContainsKey "Authorization")
    }

[<Fact>]
let ``Japan reads a closed record with its closure date and cause`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "japan-closed.xml"))

        let! evidence =
            findIn (Sources.japan (connectionOver handler) key) "4111101000007" jp

        let company = companyOf evidence
        Assert.Equal(CompanyStanding.Dissolved, company.Standing)
        Assert.Equal(ValueSome(DateOnly(2026, 10, 30)), company.CeasedOn)
        Assert.Contains("Mihon Co., Ltd.", company.OtherNames)
    }

[<Fact>]
let ``Japan's empty answer and a withdrawn number aren't not held`` () : Task =
    task {
        let empty =
            """<?xml version="1.0" encoding="UTF-8"?><corporations><lastUpdateDate>2026-11-06</lastUpdateDate><count>0</count><divideNumber>1</divideNumber><divideSize>1</divideSize></corporations>"""

        use none = new FakeHandler(fun _ -> ok empty)
        let! evidence = findIn (Sources.japan (connectionOver none) key) "5111101000006" jp

        match evidence.Value with
        | NotAvailable reason -> Assert.Contains("proves nothing", reason)
        | other -> failwith $"%A{other}"

        let withdrawn =
            """<?xml version="1.0" encoding="UTF-8"?><corporations><lastUpdateDate>2026-11-06</lastUpdateDate><count>1</count><divideNumber>1</divideNumber><divideSize>1</divideSize><corporation><sequenceNumber>1</sequenceNumber><corporateNumber>5111101000006</corporateNumber><process>99</process><updateDate>2026-11-05</updateDate></corporation></corporations>"""

        use gone = new FakeHandler(fun _ -> ok withdrawn)
        let! evidence = findIn (Sources.japan (connectionOver gone) key) "5111101000006" jp

        match evidence.Value with
        | NotAvailable reason -> Assert.Contains("withdrawn", reason)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``Japan's 404 is an application ID refused, and no failure holds the ID`` () : Task =
    task {
        for status in [ 404; 400; 403; 500 ] do
            use handler = new FakeHandler(fun _ -> answer status "secret-body")

            match! (Sources.japan (connectionOver handler) key).TryFindAsync("5111101000006", jp, ct) with
            | Error failure ->
                noSecretIn (RegistryFailure.describe failure)

                if status = 404 then
                    Assert.StartsWith("AccessRefused", $"%A{failure}")
            | other -> failwith $"%A{other}"
    }

[<Theory>]
[<InlineData("01", "", "Registered")>]
[<InlineData("11", "", "Registered")>]
[<InlineData("12", "", "Registered")>]
[<InlineData("13", "", "Registered")>]
[<InlineData("22", "", "Registered")>]
[<InlineData("71", "", "Registered")>]
[<InlineData("72", "", "Registered")>]
[<InlineData("81", "", "Closed")>]
[<InlineData("21", "", "Closed")>]
[<InlineData("21", "01", "Dissolved")>]
[<InlineData("21", "11", "Closed")>]
[<InlineData("21", "21", "Closed")>]
[<InlineData("21", "31", "Closed")>]
[<InlineData("21", "41", "Unknown")>]
[<InlineData("99", "", "Unknown")>]
[<InlineData("55", "", "Unknown")>]
let ``every Japanese process and closure cause has a standing, and a new one is unknown``
    (code: string, cause: string, expected: string)
    =
    let closeCause = if cause = "" then ValueNone else ValueSome cause
    Assert.StartsWith(expected, $"%A{Japan.standing (ValueSome code) closeCause}")

[<Theory>]
[<InlineData("5111101000006", true)>]
[<InlineData("4111101000007", true)>]
[<InlineData("6111101000006", false)>]
[<InlineData("511110100000", false)>]
[<InlineData("abcdefghijklm", false)>]
let ``a Japanese corporate number's check digit is checked`` (number: string, valid: bool) =
    Assert.Equal(valid, (Japan.normalNumber number).IsSome)

[<Fact>]
let ``Japan's terms require its attribution sentence, which the coverage carries`` () =
    use handler = new FakeHandler(fun _ -> failwith "Nothing should be sent")
    let coverage = (Sources.japan (connectionOver handler) key).Coverage
    Assert.Contains(Japan.attribution, coverage.Licence)
    Assert.Contains("国税庁", coverage.Licence)

[<Fact>]
let ``Japan's name search reads every match and the data's date`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "japan-corporation.xml"))

        match! (Sources.japan (connectionOver handler) key).SearchAsync("検索", jp, ct) with
        | Ok { Value = Available found; AsOf = asOf } ->
            Assert.Equal("5111101000006", found.Matches.Head.Number)
            Assert.True found.Complete
            Assert.Equal(ValueSome(DateOnly(2019, 4, 5)), asOf)
            Assert.StartsWith($"/4/name?id={Key}&name=", handler.Calls.Head.Target)
            // mode 2 is a partial match, which a Contains search needs; the default is a prefix match.
            Assert.EndsWith("&type=12&mode=2", handler.Calls.Head.Target)
        | other -> failwith $"%A{other}"
    }

// Zefix (Switzerland)

[<Fact>]
let ``Zefix reads a company by UID with Basic credentials`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "zefix-company.json"))

        let! evidence =
            findIn (Sources.zefix (connectionOver handler) clientCredentials) "CHE-107.721.785" ch

        let company = companyOf evidence
        Assert.Equal("Beispiel AG", company.Name)
        Assert.Equal("CHE107721785", company.Number)
        Assert.Equal(CompanyStanding.Active, company.Standing)
        Assert.Equal(RecordRole.Home, company.Role)
        Assert.Equal(ValueSome "Company limited by shares", company.LegalForm)
        Assert.Equal<string list>([ "Exemple SA" ], company.OtherNames)
        Assert.StartsWith("{\"name\":\"Beispiel AG\"", evidence.Raw.Value)

        match company.Addresses with
        | Available [ office ] ->
            Assert.Equal<string list>([ "Beispiel AG"; "Bundesgasse 1" ], office.Address.Lines)
            Assert.Equal(ValueSome "Bern", office.Address.Locality)
            Assert.Equal(ValueSome "3003", office.Address.PostalCode)
            Assert.Equal(ValueSome "BE", office.Address.Region)
        | other -> failwith $"%A{other}"

        let call = handler.Calls.Head
        Assert.Equal("www.zefix.admin.ch", call.Host)
        Assert.Equal("/ZefixPublicREST/api/v1/company/uid/CHE107721785", call.Target)
        Assert.Equal(basic, call.Headers["Authorization"])
    }

[<Fact>]
let ``Zefix's NOT_FOUND and an empty list read as not held, and another 404 is refused`` () : Task =
    task {
        use missing = new FakeHandler(fun _ -> answer 404 (fixture "zefix-not-found.json"))

        let! evidence =
            findIn (Sources.zefix (connectionOver missing) clientCredentials) "CHE107721785" ch

        Assert.Equal(Available ValueNone, evidence.Value)

        use empty = new FakeHandler(fun _ -> ok "[]")

        let! evidence =
            findIn (Sources.zefix (connectionOver empty) clientCredentials) "CHE107721785" ch

        Assert.Equal(Available ValueNone, evidence.Value)

        use other = new FakeHandler(fun _ -> notFound)

        match! (Sources.zefix (connectionOver other) clientCredentials).TryFindAsync("CHE107721785", ch, ct) with
        | Error(RegistryFailure.RequestRefused _) -> ()
        | result -> failwith $"%A{result}"
    }

[<Theory>]
[<InlineData("ACTIVE", "Active")>]
[<InlineData("BEING_CANCELLED", "WindingUp")>]
[<InlineData("CANCELLED", "Closed")>]
[<InlineData("DELETED", "Unknown")>]
let ``every Zefix status has a standing, and a new one is unknown`` (status: string, expected: string) =
    Assert.StartsWith(expected, $"%A{Zefix.standing (ValueSome status)}")

[<Theory>]
[<InlineData("CHE-107.721.784")>]
[<InlineData("CHE10772178")>]
[<InlineData("107721785")>]
[<InlineData("DEU107721785")>]
[<InlineData("")>]
let ``a value that can't be a UID isn't sent`` (number: string) : Task =
    task {
        use handler = new FakeHandler(fun _ -> failwith "Nothing should be sent")

        match! (Sources.zefix (connectionOver handler) clientCredentials).TryFindAsync(number, ch, ct) with
        | Error(RegistryFailure.InvalidRequest _) -> Assert.Empty handler.Calls
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``Zefix's search posts the name, and a short one isn't sent`` () : Task =
    task {
        use handler =
            new FakeHandler(fun _ -> ok """[{"name":"Beispiel AG","uid":"CHE107721785","status":"ACTIVE"}]""")

        let registry = Sources.zefix (connectionOver handler) clientCredentials

        match! registry.SearchAsync("Beispiel", ch, ct) with
        | Ok { Value = Available found } ->
            Assert.Equal("CHE107721785", found.Matches.Head.Number)
            Assert.False found.Complete
            Assert.Equal("/ZefixPublicREST/api/v1/company/search", handler.Calls.Head.Target)
            Assert.Equal(basic, handler.Calls.Head.Headers["Authorization"])
        | other -> failwith $"%A{other}"

        match! registry.SearchAsync("Be", ch, ct) with
        | Error(RegistryFailure.InvalidRequest _) -> Assert.Equal(1, handler.Calls.Length)
        | other -> failwith $"%A{other}"
    }

// California

[<Fact>]
let ``California reads the guide's sample, with the key in the subscription header`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "california-entity.json"))

        let! evidence =
            findIn (Sources.california (connectionOver handler) key) "202150010654" usCa

        let company = companyOf evidence
        Assert.Equal("Pure Moon LLC", company.Name)
        Assert.Equal("202150010654", company.Number)
        Assert.Equal(CompanyStanding.Closed "Suspended - SOS", company.Standing)
        Assert.Equal(RecordRole.Home, company.Role)
        Assert.Equal(ValueSome "Limited Liability Company - CA", company.LegalForm)
        Assert.Equal(ValueSome(DateOnly(2021, 12, 6)), company.IncorporatedOn)
        Assert.True evidence.Raw.IsSome

        match company.Addresses with
        | Available [ mailing; agent ] ->
            Assert.Equal(AddressKind.Postal, mailing.Kind)
            Assert.Equal(AddressKind.RegisteredAgent, agent.Kind)
            Assert.Equal<string list>([ "3243 PEARL ST" ], agent.Address.Lines)
        | other -> failwith $"%A{other}"

        let call = handler.Calls.Head
        Assert.Equal("calico.sos.ca.gov", call.Host)
        Assert.Equal("/cbc/v1/api/BusinessEntityDetails?entity-number=202150010654", call.Target)
        Assert.Equal(Key, call.Headers["Ocp-Apim-Subscription-Key"])
    }

[<Fact>]
let ``California's 400, which its guide gives for no entity found, is NotAvailable`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> answer 400 "secret-body")

        let! evidence =
            findIn (Sources.california (connectionOver handler) key) "202150010654" usCa

        match evidence.Value with
        | NotAvailable reason -> noSecretIn reason
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``California's keyword search is the top 150, and says when it holds less than the count`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "california-search.json"))

        match! (Sources.california (connectionOver handler) key).SearchAsync("Pure", usCa, ct) with
        | Ok { Value = Available found } ->
            Assert.Equal(2, found.Matches.Length)
            Assert.Equal(ValueSome 10, found.TotalMatches)
            Assert.False found.Complete
            Assert.Equal("/cbc/v1/api/BusinessEntityKeywordSearch?search-term=Pure", handler.Calls.Head.Target)
        | other -> failwith $"%A{other}"
    }

[<Theory>]
[<InlineData("Active", "Active")>]
[<InlineData("Suspended - SOS", "Closed")>]
[<InlineData("Suspended - FTB", "Unknown")>]
[<InlineData("", "Unknown")>]
let ``California's documented status words have a standing, and any other is unknown``
    (word: string, expected: string)
    =
    Assert.StartsWith(expected, $"%A{California.standing (ValueSome word)}")

[<Theory; InlineData("abc"); InlineData("12345"); InlineData("AB1234567"); InlineData("")>]
let ``a value that can't be a California entity number isn't sent`` (number: string) : Task =
    task {
        use handler = new FakeHandler(fun _ -> failwith "Nothing should be sent")

        match! (Sources.california (connectionOver handler) key).TryFindAsync(number, usCa, ct) with
        | Error(RegistryFailure.InvalidRequest _) -> Assert.Empty handler.Calls
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``California's coverage carries the government-only caveat and no terms`` () =
    use handler = new FakeHandler(fun _ -> failwith "Nothing should be sent")
    let coverage = (Sources.california (connectionOver handler) key).Coverage

    match coverage.Cost with
    | AccessCost.FreeWithKey how -> Assert.Contains("government agencies only", how)
    | other -> failwith $"%A{other}"

    Assert.Contains("Terms not found", coverage.Licence)

// Every keyed source, the same way

[<Fact>]
let ``what no keyed source publishes is NotAvailable, without a call`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> failwith "Nothing should be sent")
        let connection = connectionOver handler

        for registry, number, jurisdiction in
            [
                Sources.canada connection key, "1007", ca
                Sources.sweden connection clientCredentials, "5560021361", se
                Sources.japan connection key, "5111101000006", jp
                Sources.zefix connection clientCredentials, "CHE107721785", ch
                Sources.california connection key, "202150010654", usCa
            ] do
            match! registry.OwnersAsync(number, jurisdiction, OwnerPurpose.PublicRead, ct) with
            | Ok { Value = NotAvailable _ } -> ()
            | other -> failwith $"%s{registry.Coverage.Registry}: %A{other}"

        for registry, number, jurisdiction in
            [
                Sources.sweden connection clientCredentials, "5560021361", se
                Sources.japan connection key, "5111101000006", jp
                Sources.zefix connection clientCredentials, "CHE107721785", ch
                Sources.california connection key, "202150010654", usCa
            ] do
            match! registry.OfficersAsync(number, jurisdiction, ct) with
            | Ok { Value = NotAvailable _ } -> ()
            | other -> failwith $"%s{registry.Coverage.Registry}: %A{other}"

        for registry, jurisdiction in
            [
                Sources.canada connection key, ca
                Sources.sweden connection clientCredentials, se
            ] do
            match! registry.SearchAsync("x", jurisdiction, ct) with
            | Ok { Value = NotAvailable _ } -> ()
            | other -> failwith $"%s{registry.Coverage.Registry}: %A{other}"

        match!
            (Sources.sweden connection clientCredentials)
                .OwnersAsync("5560021361", se, OwnerPurpose.CustomerDueDiligence, ct)
        with
        | Ok { Value = NeedsEntitlement entitlement } -> Assert.Contains("VerkligaHuvudmän", entitlement.Route)
        | other -> failwith $"%A{other}"

        Assert.Empty handler.Calls
    }

[<Fact>]
let ``outside its coverage every keyed source fails, and sends nothing`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> failwith "Nothing should be sent")
        let elsewhere = Jurisdiction.ofCode "ZZ"

        for registry, number, _ in allKeyed handler do
            let! found = registry.TryFindAsync(number, elsewhere, ct)
            Assert.Equal(Error(RegistryFailure.OutsideCoverage elsewhere), found)
            let! officers = registry.OfficersAsync(number, elsewhere, ct)
            Assert.Equal(Error(RegistryFailure.OutsideCoverage elsewhere), officers)
            let! owners = registry.OwnersAsync(number, elsewhere, OwnerPurpose.PublicRead, ct)
            Assert.Equal(Error(RegistryFailure.OutsideCoverage elsewhere), owners)
            let! search = registry.SearchAsync("xyz", elsewhere, ct)
            Assert.Equal(Error(RegistryFailure.OutsideCoverage elsewhere), search)

        Assert.Empty handler.Calls
    }

[<Theory; InlineData("not json"); InlineData(""); InlineData("<html>Service page</html>")>]
let ``an answer that can't be read is unreadable, on every keyed source`` (body: string) : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok body)

        for registry, number, jurisdiction in allKeyed handler do
            match! registry.TryFindAsync(number, jurisdiction, ct) with
            | Error(RegistryFailure.UnreadableAnswer reason) -> Assert.DoesNotContain("html", reason.ToLowerInvariant())
            | other -> failwith $"%s{registry.Coverage.Registry}: %A{other}"
    }

[<Fact>]
let ``JSON of the wrong shape is unreadable, on every keyed source`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok """{"unexpected":true}""")

        for registry, number, jurisdiction in allKeyed handler do
            match! registry.TryFindAsync(number, jurisdiction, ct) with
            | Error(RegistryFailure.UnreadableAnswer _) -> ()
            | other -> failwith $"%s{registry.Coverage.Registry}: %A{other}"
    }

[<Fact>]
let ``a record missing its name or number is unreadable`` () : Task =
    task {
        let unreadable (registry: ICompanyRegistry) number jurisdiction =
            task {
                match! registry.TryFindAsync(number, jurisdiction, ct) with
                | Error(RegistryFailure.UnreadableAnswer _) -> ()
                | other -> failwith $"%s{registry.Coverage.Registry}: %A{other}"
            }

        use canada =
            new FakeHandler(fun _ -> ok """[{"corporationId":"1007","status":"Active","corporationNames":[]},null]""")

        do! unreadable (Sources.canada (connectionOver canada) key) "1007" ca

        use canadaNoNumber =
            new FakeHandler(fun _ ->
                ok
                    """[{"status":"Active","corporationNames":[{"CorporationName":{"name":"X","current":true}}]},null]""")

        do! unreadable (Sources.canada (connectionOver canadaNoNumber) key) "1007" ca

        use newZealand =
            new FakeHandler(fun _ -> ok """{"nzbn":"9429000106078","entityStatusCode":"50"}""")

        do! unreadable (Sources.newZealand (connectionOver newZealand) key) "9429000106078" nz

        use newZealandNoNumber =
            new FakeHandler(fun _ -> ok """{"entityName":"X","entityStatusCode":"50"}""")

        do! unreadable (Sources.newZealand (connectionOver newZealandNoNumber) key) "9429000106078" nz

        use sweden =
            new FakeHandler(
                withToken (fun _ ->
                    ok """{"organisationer":[{"organisationsidentitet":{"identitetsbeteckning":"5560021361"}}]}""")
            )

        do! unreadable (Sources.sweden (connectionOver sweden) clientCredentials) "5560021361" se

        use swedenNoNumber =
            new FakeHandler(
                withToken (fun _ ->
                    ok
                        """{"organisationer":[{"organisationsnamn":{"organisationsnamnLista":[{"namn":"X","organisationsnamntyp":{"kod":"FORETAGSNAMN"}}]}}]}""")
            )

        do! unreadable (Sources.sweden (connectionOver swedenNoNumber) clientCredentials) "5560021361" se

        use japan =
            new FakeHandler(fun _ ->
                ok
                    "<corporations><corporation><corporateNumber>5111101000006</corporateNumber><process>01</process></corporation></corporations>")

        do! unreadable (Sources.japan (connectionOver japan) key) "5111101000006" jp

        use zefix =
            new FakeHandler(fun _ -> ok """[{"uid":"CHE107721785","status":"ACTIVE"}]""")

        do! unreadable (Sources.zefix (connectionOver zefix) clientCredentials) "CHE107721785" ch

        use zefixNoNumber =
            new FakeHandler(fun _ -> ok """[{"name":"X","status":"ACTIVE"}]""")

        do! unreadable (Sources.zefix (connectionOver zefixNoNumber) clientCredentials) "CHE107721785" ch

        use california =
            new FakeHandler(fun _ -> ok """{"EntityID":"202150010654","StatusDescription":"Active"}""")

        do! unreadable (Sources.california (connectionOver california) key) "202150010654" usCa

        use californiaNoNumber =
            new FakeHandler(fun _ -> ok """{"EntityName":"X","StatusDescription":"Active"}""")

        do! unreadable (Sources.california (connectionOver californiaNoNumber) key) "202150010654" usCa
    }

[<Theory; InlineData(500); InlineData(503)>]
let ``a server failure is Unavailable, on every keyed source`` (status: int) : Task =
    task {
        use handler = new FakeHandler(fun _ -> answer status "secret-body")

        for registry, number, jurisdiction in allKeyed handler do
            match! registry.TryFindAsync(number, jurisdiction, ct) with
            | Error(RegistryFailure.Unavailable reason) -> noSecretIn reason
            | other -> failwith $"%s{registry.Coverage.Registry}: %A{other}"
    }

[<Theory>]
[<InlineData(400)>]
[<InlineData(403)>]
[<InlineData(422)>]
[<InlineData(302)>]
let ``a refusal or redirect is RequestRefused, on every keyed source but California's 400`` (status: int) : Task =
    task {
        use handler = new FakeHandler(fun _ -> answer status "secret-body")

        for registry, number, jurisdiction in allKeyed handler do
            match! registry.TryFindAsync(number, jurisdiction, ct) with
            | Error(RegistryFailure.RequestRefused reason) -> noSecretIn reason
            | Ok { Value = NotAvailable _ } when status = 400 && jurisdiction = usCa -> ()
            | other -> failwith $"%s{registry.Coverage.Registry}: %A{other}"
    }

[<Fact>]
let ``a refused key is AccessRefused, on every keyed source, and no failure holds it`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> answer 401 "secret-body")

        for registry, number, jurisdiction in allKeyed handler do
            match! registry.TryFindAsync(number, jurisdiction, ct) with
            | Error(RegistryFailure.AccessRefused reason as failure) ->
                noSecretIn reason
                noSecretIn (RegistryFailure.describe failure)
            | other -> failwith $"%s{registry.Coverage.Registry}: %A{other}"
    }

[<Fact>]
let ``keys and credentials print hidden`` () =
    Assert.DoesNotContain(Key, string key)
    Assert.DoesNotContain(ClientSecret, string clientCredentials)
    Assert.DoesNotContain(Key, $"%A{key}")

[<Fact>]
let ``each keyed source declares a free key, its pace and that it wasn't run live`` () =
    use handler = new FakeHandler(fun _ -> failwith "Nothing should be sent")

    for registry, _, _ in allKeyed handler do
        match registry.Coverage.Cost with
        | AccessCost.FreeWithKey how -> Assert.NotEmpty how
        | other -> failwith $"%s{registry.Coverage.Registry}: %A{other}"

        Assert.Contains("Not run live", registry.Coverage.Licence)

    let connection = connectionOver handler

    for registry in [ Sources.canada connection key; Sources.sweden connection clientCredentials ] do
        match registry.Coverage.RateLimit with
        | RateLimitPolicy.Published(60, per, _) -> Assert.Equal(TimeSpan.FromMinutes 1., per)
        | other -> failwith $"%A{other}"
