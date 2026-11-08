/// OrgBook BC, the Israeli Companies Registrar, ACRA on data.gov.sg, KVK's open dataset and the Latvian Register of Enterprises, replayed from
/// answers recorded on 2026-11-08 with people's names redacted. KVK answered 429 to every call that day, so its fixtures follow its OpenAPI document.
module OpenDataLookupSourceTests

open System
open System.IO
open System.Threading.Tasks
open Xunit
open CompanyDueDiligence
open Fake

let private fixture (name: string) =
    File.ReadAllText(Path.Combine(AppContext.BaseDirectory, "Fixtures", name))

let private fixedTime = DateTimeOffset(2026, 11, 8, 12, 0, 0, TimeSpan.Zero)

let private connectionOver (handler: FakeHandler) =
    SourceConnection(handler, (fun () -> fixedTime), (fun _ _ -> Task.CompletedTask))

let private bc = Jurisdiction.ofCode "CA-BC"
let private il = Jurisdiction.ofCode "IL"
let private sg = Jurisdiction.ofCode "SG"
let private nl = Jurisdiction.ofCode "NL"
let private lv = Jurisdiction.ofCode "LV"

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

let private notAvailable (evidence: Evidence<Availability<Company voption>>) (says: string) =
    match evidence.Value with
    | NotAvailable reason -> Assert.Contains(says, reason)
    | other -> failwith $"%A{other}"

let private nothingSent () =
    new FakeHandler(fun _ -> failwith "Nothing should be sent")

/// Every registry here, with a number of the right shape.
let private allRegistries (handler: FakeHandler) =
    let connection = connectionOver handler

    [
        Sources.britishColumbia connection, "BC0772006", bc
        Sources.israel connection, "515532778", il
        Sources.singapore connection, "S94FC4771H", sg
        Sources.netherlands connection, "59581883", nl
        Sources.latvia connection, "40003000642", lv
    ]

/// A CKAN datastore answer holding these records.
let private ckanAnswer (records: string list) =
    let body = String.concat "," records
    $"""{{"success":true,"result":{{"records":[{body}],"total":{records.Length}}}}}"""

// OrgBook BC

[<Fact>]
let ``British Columbia reads an active company from OrgBook, asking for historical ones too`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "britishcolumbia-active.json"))

        let! evidence =
            findIn (Sources.britishColumbia (connectionOver handler)) " bc0772006 " bc

        let company = companyOf evidence
        Assert.Equal("OrgBook BC (British Columbia)", evidence.Source)
        Assert.Equal(fixedTime, evidence.RetrievedAt)
        Assert.Equal(ValueNone, evidence.AsOf)
        Assert.StartsWith("{\"id\":789282", evidence.Raw.Value)
        Assert.Equal("U3 POWER CORP.", company.Name)
        Assert.Empty company.OtherNames
        Assert.Equal("BC0772006", company.Number)
        Assert.Equal(CompanyStanding.Active, company.Standing)
        Assert.Equal(ValueSome "ACT (Filing:ICORP)", company.StatusAsPublished)
        Assert.Equal(ValueSome "BC Company", company.LegalForm)
        Assert.Equal(ValueSome(DateOnly(2006, 10, 17)), company.IncorporatedOn)
        Assert.Equal(RecordRole.Home, company.Role)
        Assert.Equal<CompanyConcern list>(CompanyConcern.all, company.Concerns.Unconfirmed)

        match company.Addresses with
        | NotAvailable _ -> ()
        | other -> failwith $"%A{other}"

        Assert.Equal("/api/v4/search/topic?q=BC0772006&inactive=any", handler.Calls.Head.Target)
    }

[<Fact>]
let ``British Columbia reads a historical company as closed, with the date its status took effect`` () : Task =
    task {
        use handler =
            new FakeHandler(fun _ -> ok (fixture "britishcolumbia-historical.json"))

        let! evidence =
            findIn (Sources.britishColumbia (connectionOver handler)) "BC0400243" bc

        let company = companyOf evidence
        Assert.Equal(CompanyStanding.Closed "Historical", company.Standing)
        Assert.Equal(ValueSome "HIS (Event:SYSDL)", company.StatusAsPublished)
        Assert.Equal(ValueSome(DateOnly(2004, 8, 7)), company.CeasedOn)
    }

[<Fact>]
let ``an extraprovincial registration in British Columbia names its home jurisdiction`` () : Task =
    task {
        use handler =
            new FakeHandler(fun _ -> ok (fixture "britishcolumbia-extraprovincial.json"))

        let! evidence =
            findIn (Sources.britishColumbia (connectionOver handler)) "A0046648" bc

        let company = companyOf evidence
        Assert.Equal("HOUSTON POWER CORP.", company.Name)
        Assert.Equal(RecordRole.ForeignRegistration, company.Role)
        Assert.Equal(ValueSome "AB", company.HomeRegistration.Value.Country)
        Assert.StartsWith("{\"id\":", evidence.Raw.Value)
        Assert.Contains("\"source_id\":\"A0046648\"", evidence.Raw.Value)
    }

[<Fact>]
let ``another topic in OrgBook's answer isn't taken for the number asked`` () : Task =
    task {
        use handler =
            new FakeHandler(fun _ -> ok (fixture "britishcolumbia-extraprovincial.json"))

        let! evidence =
            findIn (Sources.britishColumbia (connectionOver handler)) "BC0000001" bc

        notAvailable evidence "doesn't show"
    }

[<Fact>]
let ``OrgBook not finding a number is NotAvailable, never not held`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "britishcolumbia-none.json"))

        let! evidence =
            findIn (Sources.britishColumbia (connectionOver handler)) "BC9999999" bc

        notAvailable evidence "doesn't show the register lacks it"
    }

[<Fact>]
let ``two OrgBook topics with one number aren't told apart`` () : Task =
    task {
        let answer = fixture "britishcolumbia-active.json"
        use document = Text.Json.JsonDocument.Parse answer
        let results = document.RootElement.GetProperty "results"
        let topic = results[0].GetRawText()

        use handler =
            new FakeHandler(fun _ -> ok $"""{{"total":2,"results":[{topic},{topic}]}}""")

        let! evidence =
            findIn (Sources.britishColumbia (connectionOver handler)) "BC0772006" bc

        notAvailable evidence "2 topics"
    }

[<Theory>]
[<InlineData("ACT", "Active")>]
[<InlineData("HIS", "Closed")>]
[<InlineData("LIQ", "Unknown")>]
[<InlineData("", "Unknown")>]
let ``every OrgBook entity status has a standing, and a new one is unknown`` (status: string, expected: string) =
    Assert.StartsWith(expected, $"%A{BritishColumbia.standing (ValueSome status)}")

[<Theory>]
[<InlineData("")>]
[<InlineData("0772006")>]
[<InlineData("BC077200")>]
[<InlineData("BC07720061")>]
[<InlineData("ABCD0772006")>]
[<InlineData(null)>]
let ``a value that can't be a BC Registries number isn't sent`` (number: string) : Task =
    task {
        use handler = nothingSent ()

        match! (Sources.britishColumbia (connectionOver handler)).TryFindAsync(number, bc, ct) with
        | Error(RegistryFailure.InvalidRequest _) -> Assert.Empty handler.Calls
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``British Columbia's search reads OrgBook's first page with its total`` () : Task =
    task {
        use handler =
            new FakeHandler(fun _ -> ok (fixture "britishcolumbia-extraprovincial.json"))

        match! (Sources.britishColumbia (connectionOver handler)).SearchAsync("power corp", bc, ct) with
        | Ok { Value = Available found } ->
            Assert.Equal(3, found.Matches.Length)
            Assert.Equal(ValueSome 2550, found.TotalMatches)
            Assert.False found.Complete
            Assert.Contains("inactive=any", handler.Calls.Head.Target)
        | other -> failwith $"%A{other}"
    }

// Israel

[<Fact>]
let ``Israel reads an active company from the Registrar's list, filtering on the company number`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "israel-active.json"))
        let! evidence = findIn (Sources.israel (connectionOver handler)) "515532778" il
        let company = companyOf evidence
        Assert.Equal("Israeli Companies Registrar on data.gov.il", evidence.Source)
        Assert.StartsWith("{\"_id\":549248", evidence.Raw.Value)
        Assert.Equal("פסגות טרייד יאכטות בע~מ", company.Name)
        Assert.Equal<string list>([ "PSAGOT TRADE YACHTS LTD" ], company.OtherNames)
        Assert.Equal("515532778", company.Number)
        Assert.Equal(CompanyStanding.Active, company.Standing)
        Assert.Equal(ValueSome "פעילה", company.StatusAsPublished)
        Assert.Equal(ValueSome "ישראלית חברה פרטית", company.LegalForm)
        Assert.Equal(ValueSome(DateOnly(2016, 10, 26)), company.IncorporatedOn)
        Assert.Equal(RecordRole.Home, company.Role)

        match company.Addresses with
        | Available [ address ] ->
            Assert.Equal(ValueSome "נהריה", address.Address.Locality)
            Assert.Equal(ValueSome "2231103", address.Address.PostalCode)
            Assert.Contains("היוצרים 2", address.Address.Lines)
        | other -> failwith $"%A{other}"

        let sent = Uri.UnescapeDataString handler.Calls.Head.Target
        Assert.Contains("resource_id=f004176c-b85f-4542-8901-7b3176f9a054", sent)
        Assert.Contains("\"515532778\"", sent)
    }

[<Fact>]
let ``a struck-off Israeli company is closed, with no address`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "israel-struck-off.json"))
        let! evidence = findIn (Sources.israel (connectionOver handler)) "510000045" il
        let company = companyOf evidence
        Assert.Equal(CompanyStanding.Closed "Struck off", company.Standing)
        Assert.Equal(ValueSome(DateOnly(1935, 5, 1)), company.IncorporatedOn)

        match company.Addresses with
        | NotAvailable _ -> ()
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``Israel not finding a number is NotAvailable, never not held`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "israel-none.json"))
        let! evidence = findIn (Sources.israel (connectionOver handler)) "519999999" il
        notAvailable evidence "proves nothing"
    }

[<Fact>]
let ``another company in Israel's answer fails the read`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "israel-active.json"))

        match! (Sources.israel (connectionOver handler)).TryFindAsync("515532779", il, ct) with
        | Error(RegistryFailure.UnreadableAnswer _) -> ()
        | other -> failwith $"%A{other}"
    }

[<Theory>]
[<InlineData("פעילה", "", "Active")>]
[<InlineData("פעילה", "בפרוק מרצון בהליך מזורז", "WindingUp")>]
[<InlineData("פעילה זמנית", "", "ActiveWithConcern")>]
[<InlineData("פעילה זמנית", "בקשת חברה בהחייאה", "ActiveWithConcern")>]
[<InlineData("פעילה זמנית", "בקשת נושים בהחייאה", "ActiveWithConcern")>]
[<InlineData("פעילה/בפירוק - בכינוס נכסים", "", "InInsolvency")>]
[<InlineData("פעילה/בפירוק - הליך פשרה או הסדר/פירוק זמני", "", "InInsolvency")>]
[<InlineData("בפרוק מרצון", "", "WindingUp")>]
[<InlineData("בפרוק ע~י בימ~ש", "", "InInsolvency")>]
[<InlineData("בפרוק ע\"י בימ\"ש", "", "InInsolvency")>]
[<InlineData("מחוקה", "", "Closed")>]
[<InlineData("מחוקה", "מחוסלת עקב מחיקה", "Dissolved")>]
[<InlineData("מחוסלת מרצון", "", "Dissolved")>]
[<InlineData("חיסול ע~י בימ~ש", "", "Dissolved")>]
[<InlineData("מחוסלת עקב מיזוג", "", "Closed")>]
[<InlineData("נגרעה מהמרשם", "", "Closed")>]
[<InlineData("פעילה", "תת סטטוס חדש", "Unknown")>]
[<InlineData("סטטוס חדש", "", "Unknown")>]
[<InlineData("", "", "Unknown")>]
let ``every Israeli status and sub-status seen or documented has a standing, and a new one is unknown``
    (status: string, subStatus: string, expected: string)
    =
    Assert.StartsWith(expected, $"%A{Israel.standing (ValueSome status) (ValueSome subStatus) false}")

[<Fact>]
let ``a violating Israeli company is active with a concern`` () =
    Assert.StartsWith("ActiveWithConcern", $"""%A{Israel.standing (ValueSome "פעילה") ValueNone true}""")
    Assert.False(CompanyStanding.isActive (Israel.standing (ValueSome "פעילה") ValueNone true))

[<Fact>]
let ``an Israeli company in liquidation by the court states its insolvency`` () : Task =
    task {
        let body =
            (fixture "israel-active.json").Replace("\"סטטוס חברה\":\"פעילה\"", "\"סטטוס חברה\":\"בפרוק ע~י בימ~ש\"")

        use handler = new FakeHandler(fun _ -> ok body)
        let! evidence = findIn (Sources.israel (connectionOver handler)) "515532778" il
        let company = companyOf evidence
        Assert.Equal(CompanyStanding.InInsolvency "Liquidation by the court", company.Standing)
        Assert.Equal<CompanyConcern list>([ CompanyConcern.InsolvencyHistory ], company.Concerns.Stated)
    }

[<Theory>]
[<InlineData("זרה", "ForeignRegistration")>]
[<InlineData("חברה זרה", "ForeignRegistration")>]
[<InlineData("ממשלתית", "NotStated")>]
let ``an Israeli corporation type says whether the company is foreign`` (kind: string, expected: string) : Task =
    task {
        let body =
            (fixture "israel-active.json").Replace("\"סוג תאגיד\":\"ישראלית חברה פרטית\"", $"\"סוג תאגיד\":\"{kind}\"")

        use handler = new FakeHandler(fun _ -> ok body)
        let! evidence = findIn (Sources.israel (connectionOver handler)) "515532778" il
        Assert.Equal(expected, $"%A{(companyOf evidence).Role}")
        Assert.Equal(RecordRole.NotStated, Israel.role ValueNone)
    }

[<Theory; InlineData("51553277"); InlineData("5155327781"); InlineData("abcdefghi"); InlineData(null)>]
let ``a value that can't be an Israeli company number isn't sent`` (number: string) : Task =
    task {
        use handler = nothingSent ()

        match! (Sources.israel (connectionOver handler)).TryFindAsync(number, il, ct) with
        | Error(RegistryFailure.InvalidRequest _) -> Assert.Empty handler.Calls
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``Israel's search reads the datastore's records with its total`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "israel-active.json"))

        match! (Sources.israel (connectionOver handler)).SearchAsync("PSAGOT", il, ct) with
        | Ok { Value = Available found } ->
            Assert.Equal("515532778", found.Matches.Head.Number)
            Assert.True found.Complete
            Assert.Contains("q=PSAGOT", handler.Calls.Head.Target)
        | other -> failwith $"%A{other}"
    }

// Singapore

[<Theory>]
[<InlineData("Arkkitehtityö Virtanen", "Arkkitehtityo Virtanen")>]
[<InlineData("  Société Générale ", "Societe Generale")>]
[<InlineData("DBS BANK LTD.", "DBS BANK LTD.")>]
let ``a Singapore search drops accents, which data.gov.sg refuses`` (name: string, expected: string) : Task =
    task {
        Assert.Equal(ValueSome expected, Singapore.searchTerm name)

        use handler =
            new FakeHandler(fun _ -> ok """{"success":true,"result":{"records":[],"total":0}}""")

        let! _ = (Sources.singapore (connectionOver handler)).SearchAsync(name, sg, ct)
        Assert.Contains(Uri.EscapeDataString expected, handler.Calls.Head.Uri)
    }

[<Fact>]
let ``a Singapore search in another script is refused without sending`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> failwith "Nothing should be sent")

        match! (Sources.singapore (connectionOver handler)).SearchAsync("株式会社", sg, ct) with
        | Error(RegistryFailure.InvalidRequest _) -> Assert.Empty handler.Calls
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``Singapore reads a foreign company's branch, registered and not stated live`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "singapore-foreign-branch.json"))
        let! evidence = findIn (Sources.singapore (connectionOver handler)) "s94fc4771h" sg
        let company = companyOf evidence
        Assert.Equal("ACRA entities on data.gov.sg (Singapore)", evidence.Source)
        Assert.Equal("CHINA SOUTHERN AIRLINES COMPANY LIMITED", company.Name)
        Assert.Equal("S94FC4771H", company.Number)
        Assert.Equal(CompanyStanding.Registered(ValueSome "Registered"), company.Standing)
        Assert.False(CompanyStanding.isActive company.Standing)
        Assert.Equal(RecordRole.ForeignRegistration, company.Role)
        Assert.Equal(ValueSome "Foreign Company Branch", company.LegalForm)
        Assert.StartsWith("{\"_id\":872", evidence.Raw.Value)

        match company.Addresses with
        | Available [ address ] ->
            Assert.Equal(AddressKind.RegisteredOffice, address.Kind)
            Assert.Equal<string list>([ "BEACH ROAD" ], address.Address.Lines)
            Assert.Equal(ValueSome "189721", address.Address.PostalCode)
        | other -> failwith $"%A{other}"

        let sent = Uri.UnescapeDataString handler.Calls.Head.Target
        Assert.StartsWith("/api/action/datastore_search?resource_id=d_3f960c10fed6145404ca7b821f263b87", sent)
        Assert.Contains("{\"uen\":\"S94FC4771H\"}", sent)
    }

[<Fact>]
let ``a deregistered Singapore company is closed`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "singapore-deregistered.json"))
        let! evidence = findIn (Sources.singapore (connectionOver handler)) "201201936C" sg
        let company = companyOf evidence
        Assert.Equal(CompanyStanding.Closed "Deregistered", company.Standing)
        Assert.Equal(RecordRole.Home, company.Role)
    }

[<Fact>]
let ``Singapore's "na" is no value`` () : Task =
    task {
        let body =
            ckanAnswer
                [
                    """{"uen":"S78FC2694C","uen_status_desc":"Deregistered","entity_name":"X","entity_type_desc":"Foreign Company Branch","reg_street_name":"na","reg_postal_code":"na"}"""
                ]

        use handler = new FakeHandler(fun _ -> ok body)
        let! evidence = findIn (Sources.singapore (connectionOver handler)) "S78FC2694C" sg

        match (companyOf evidence).Addresses with
        | NotAvailable _ -> ()
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``Singapore not finding a UEN is NotAvailable, never not held`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "singapore-none.json"))
        let! evidence = findIn (Sources.singapore (connectionOver handler)) "199999999Z" sg
        notAvailable evidence "monthly copy"
    }

[<Theory>]
[<InlineData("Registered", "Registered")>]
[<InlineData("Deregistered", "Closed")>]
[<InlineData("Live", "Unknown")>]
let ``every Singapore UEN status has a standing, and a new one is unknown`` (status: string, expected: string) =
    Assert.StartsWith(expected, $"%A{Singapore.standing (ValueSome status)}")

[<Theory>]
[<InlineData("Foreign Company Branch", "ForeignRegistration")>]
[<InlineData("Local Company", "Home")>]
[<InlineData("Sole Proprietorship/ Partnership", "Home")>]
[<InlineData("Limited Liability Partnership", "NotStated")>]
let ``a Singapore entity type says whether it is a home record`` (entityType: string, expected: string) =
    Assert.Equal(expected, $"%A{Singapore.role (ValueSome entityType)}")

[<Theory>]
[<InlineData("201201936c", "201201936C")>]
[<InlineData("53250767C", "53250767C")>]
[<InlineData(" T08LL0001A ", "T08LL0001A")>]
let ``a UEN is put in upper case`` (number: string, expected: string) =
    Assert.Equal(ValueSome expected, Singapore.normalUen number)

[<Theory; InlineData("12345"); InlineData("2012019360"); InlineData("X08LL0001A"); InlineData(null)>]
let ``a value that can't be a UEN isn't sent`` (number: string) : Task =
    task {
        use handler = nothingSent ()

        match! (Sources.singapore (connectionOver handler)).TryFindAsync(number, sg, ct) with
        | Error(RegistryFailure.InvalidRequest _) -> Assert.Empty handler.Calls
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``Singapore keeps to data.gov.sg's published pace`` () =
    match Singapore.coverage.RateLimit with
    | RateLimitPolicy.Published(4, per, _) -> Assert.Equal(TimeSpan.FromSeconds 10., per)
    | other -> failwith $"%A{other}"

// The Netherlands

[<Fact>]
let ``the Netherlands reads a BV from KVK's open dataset, with its name not published`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "netherlands-active.json"))
        let! evidence = findIn (Sources.netherlands (connectionOver handler)) "59581883" nl
        let company = companyOf evidence
        Assert.Equal("KVK open dataset Basis Bedrijfsgegevens (Netherlands)", evidence.Source)
        Assert.Equal(PartyNames.NotPublished, company.Name)
        Assert.True(PartyNames.isPlaceholder company.Name)
        Assert.Equal("59581883", company.Number)
        Assert.Equal(CompanyStanding.Active, company.Standing)
        Assert.Equal(ValueSome "actief J", company.StatusAsPublished)
        Assert.Equal(ValueSome "Besloten vennootschap (BV)", company.LegalForm)
        Assert.Equal(RecordRole.Home, company.Role)
        Assert.True evidence.Raw.IsSome

        match company.Addresses with
        | NotAvailable _ -> ()
        | other -> failwith $"%A{other}"

        Assert.Equal("/api/v1/hvds/basisbedrijfsgegevens/kvknummer/59581883", handler.Calls.Head.Target)
    }

[<Fact>]
let ``KVK's 404 is NotAvailable, since the dataset holds BVs and NVs only`` () : Task =
    task {
        use handler =
            new FakeHandler(fun _ -> answer 404 (fixture "netherlands-not-found.json"))

        let! evidence = findIn (Sources.netherlands (connectionOver handler)) "12345678" nl
        notAvailable evidence "only BVs and NVs"
    }

[<Theory>]
[<InlineData("J", "", "Active")>]
[<InlineData("N", "", "Closed")>]
[<InlineData("J", "FAIL", "InInsolvency")>]
[<InlineData("J", "SSAN", "InInsolvency")>]
[<InlineData("N", "SURS", "InInsolvency")>]
[<InlineData("J", "XXXX", "Unknown")>]
[<InlineData("X", "", "Unknown")>]
let ``every KVK active indicator and insolvency code has a standing, and a new one is unknown``
    (active: string, insolvency: string, expected: string)
    =
    let insolvency = if insolvency = "" then ValueNone else ValueSome insolvency
    Assert.StartsWith(expected, $"%A{Netherlands.standing (ValueSome active) insolvency}")

[<Fact>]
let ``a bankrupt Dutch company states its insolvency`` () : Task =
    task {
        let body =
            (fixture "netherlands-active.json")
                .Replace("\"actief\":\"J\"", "\"actief\":\"J\",\"insolventieCode\":\"FAIL\"")

        use handler = new FakeHandler(fun _ -> ok body)
        let! evidence = findIn (Sources.netherlands (connectionOver handler)) "59581883" nl
        let company = companyOf evidence
        Assert.StartsWith("InInsolvency", $"%A{company.Standing}")
        Assert.Equal<CompanyConcern list>([ CompanyConcern.InsolvencyHistory ], company.Concerns.Stated)
    }

[<Theory; InlineData("1234567"); InlineData("123456789"); InlineData("abcdefgh"); InlineData(null)>]
let ``a value that can't be a KVK number isn't sent`` (number: string) : Task =
    task {
        use handler = nothingSent ()

        match! (Sources.netherlands (connectionOver handler)).TryFindAsync(number, nl, ct) with
        | Error(RegistryFailure.InvalidRequest _) -> Assert.Empty handler.Calls
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``the Netherlands keeps to KVK's published pace`` () =
    match Netherlands.coverage.RateLimit with
    | RateLimitPolicy.Published(1, per, _) -> Assert.Equal(TimeSpan.FromMinutes 1., per)
    | other -> failwith $"%A{other}"

[<Fact>]
let ``a second KVK call within the minute waits for its turn rather than being sent`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "netherlands-active.json"))
        let registry = Sources.netherlands (connectionOver handler)
        let! _ = findIn registry "59581883" nl

        match! registry.TryFindAsync("59581883", nl, ct) with
        | Error(RegistryFailure.RateLimited _) -> Assert.Equal(1, handler.Calls.Length)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a KVK 429 is RateLimited at once`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> answer 429 "")

        match! (Sources.netherlands (connectionOver handler)).TryFindAsync("59581883", nl, ct) with
        | Error(RegistryFailure.RateLimited _) -> Assert.Equal(1, handler.Calls.Length)
        | other -> failwith $"%A{other}"
    }

// Latvia

[<Fact>]
let ``Latvia reads a company from the register, on the register and not called active`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "latvia-company.json"))
        let! evidence = findIn (Sources.latvia (connectionOver handler)) "40003000642" lv
        let company = companyOf evidence
        Assert.Equal("Latvian Register of Enterprises on data.gov.lv", evidence.Source)
        Assert.StartsWith("{\"_id\":474559", evidence.Raw.Value)
        Assert.Equal("Akciju sabiedrība \"Latvijas Gāze\"", company.Name)
        Assert.Equal("40003000642", company.Number)
        Assert.Equal(CompanyStanding.Registered ValueNone, company.Standing)
        Assert.False(CompanyStanding.isActive company.Standing)
        Assert.Equal(ValueNone, company.StatusAsPublished)
        Assert.Equal(ValueSome "Akciju sabiedrība", company.LegalForm)
        Assert.Equal(ValueSome(DateOnly(1991, 3, 25)), company.IncorporatedOn)
        Assert.Equal(RecordRole.Home, company.Role)

        match company.Addresses with
        | Available [ address ] ->
            Assert.Equal(AddressKind.RegisteredOffice, address.Kind)
            Assert.Equal<string list>([ "Rīga, Aristida Briāna iela 6" ], address.Address.Lines)
            Assert.Equal(ValueSome "1001", address.Address.PostalCode)
        | other -> failwith $"%A{other}"

        let sent = Uri.UnescapeDataString handler.Calls.Head.Target
        Assert.StartsWith("/dati/api/3/action/datastore_search?resource_id=25e80bf3-f107-4ab4-89ef-251b5b9374e9", sent)
        Assert.Contains("{\"regcode\":\"40003000642\"}", sent)
    }

[<Fact>]
let ``a Latvian subject excluded after liquidation is dissolved, with the date`` () : Task =
    task {
        let body =
            ckanAnswer
                [
                    """{"regcode":41202013815,"name":"IK \"X\"","type":"IK","type_text":"Individuālais komersants","registered":"1998-02-26T00:00:00","terminated":"2014-04-10T00:00:00","closed":"L"}"""
                ]

        use handler = new FakeHandler(fun _ -> ok body)
        let! evidence = findIn (Sources.latvia (connectionOver handler)) "41202013815" lv
        let company = companyOf evidence
        Assert.Equal(CompanyStanding.Dissolved, company.Standing)
        Assert.Equal(ValueSome(DateOnly(2014, 4, 10)), company.CeasedOn)
        Assert.Equal(ValueSome "closed L, terminated 2014-04-10", company.StatusAsPublished)
    }

[<Theory>]
[<InlineData("L", true, "Dissolved")>]
[<InlineData("R", true, "Closed")>]
[<InlineData("", false, "Registered")>]
[<InlineData("", true, "Closed")>]
[<InlineData("X", true, "Unknown")>]
let ``every Latvian exclusion reason has a standing, and a new one is unknown``
    (closed: string, terminated: bool, expected: string)
    =
    let closed = if closed = "" then ValueNone else ValueSome closed

    let terminated =
        if terminated then
            ValueSome(DateOnly(2020, 1, 1))
        else
            ValueNone

    Assert.StartsWith(expected, $"%A{Latvia.standing closed terminated}")

[<Theory>]
[<InlineData("AKF", "ForeignRegistration")>]
[<InlineData("FIL", "NotStated")>]
[<InlineData("SIA", "Home")>]
let ``a Latvian type says whether it is a home record`` (entityType: string, expected: string) =
    Assert.Equal(expected, $"%A{Latvia.role (ValueSome entityType)}")

[<Fact>]
let ``Latvia not finding a number is NotAvailable, never not held`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "latvia-none.json"))
        let! evidence = findIn (Sources.latvia (connectionOver handler)) "40000000000" lv
        notAvailable evidence "daily copy"
    }

[<Fact>]
let ``Latvia reads current representatives from the officers dataset`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "latvia-officers.json"))

        match! (Sources.latvia (connectionOver handler)).OfficersAsync("40003000642", lv, ct) with
        | Ok { Value = Available officers } ->
            Assert.Equal(3, officers.Length)
            let chair = officers.Head
            Assert.Equal("PERSON ONE", chair.Name)
            Assert.Equal(PartyKind.Individual, chair.Kind)
            Assert.Equal(OfficerRole.Director, chair.Role)
            Assert.True chair.Current
            Assert.Equal(ValueSome(DateOnly(2026, 7, 20)), chair.AppointedOn)
            let sent = Uri.UnescapeDataString handler.Calls.Head.Target
            Assert.Contains("resource_id=e665114a-73c2-4375-9470-55874b4cfa6b", sent)
            Assert.Contains("{\"at_legal_entity_registration_number\":\"40003000642\"}", sent)
        | other -> failwith $"%A{other}"
    }

[<Theory>]
[<InlineData("BOARD_MEMBER", "Director")>]
[<InlineData("CHAIR_OF_BOARD", "Director")>]
[<InlineData("LIQUIDATOR", "Other \"Liquidator\"")>]
[<InlineData("OWNER", "Other \"Owner\"")>]
[<InlineData("NEW_POSITION", "Other \"NEW_POSITION\"")>]
let ``every Latvian position seen has a role`` (position: string, expected: string) =
    Assert.Equal(expected, $"%A{Latvia.officerRole (ValueSome position) ValueNone}")

[<Fact>]
let ``Latvia reads registered beneficial owners`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "latvia-owners.json"))

        match!
            (Sources.latvia (connectionOver handler))
                .OwnersAsync("40203022553", lv, OwnerPurpose.CustomerDueDiligence, ct)
        with
        | Ok { Value = Available listing } ->
            let owner = List.exactlyOne listing.Owners
            Assert.Equal(ValueSome "PERSON ONE", owner.Name)
            Assert.Equal(PartyKind.Individual, owner.Kind)
            Assert.Equal(ValueSome "LV", owner.Nationality)
            Assert.Equal(ValueSome(DateOnly(2019, 7, 15)), owner.Since)
            Assert.True(OwnerListing.isComplete listing)
            Assert.Contains("resource_id=20a9b26d-d056-4dbb-ae18-9ff23c87bdee", handler.Calls.Head.Target)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``no Latvian officers or owners listed says nothing`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (ckanAnswer []))
        let registry = Sources.latvia (connectionOver handler)
        let! officers = registry.OfficersAsync("40003000642", lv, ct)
        let! owners = registry.OwnersAsync("40003000642", lv, OwnerPurpose.PublicRead, ct)

        match officers, owners with
        | Ok { Value = NotAvailable _ }, Ok { Value = NotAvailable _ } -> ()
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``more Latvian officers than were read aren't taken for the whole list`` () : Task =
    task {
        let row =
            """{"entity_type":"NATURAL_PERSON","position":"BOARD_MEMBER","name":"PERSON ONE"}"""

        use handler =
            new FakeHandler(fun _ -> ok $"""{{"success":true,"result":{{"records":[{row}],"total":500}}}}""")

        match! (Sources.latvia (connectionOver handler)).OfficersAsync("40003000642", lv, ct) with
        | Ok { Value = NotAvailable reason } -> Assert.Contains("more representatives", reason)
        | other -> failwith $"%A{other}"
    }

[<Theory; InlineData("4000300064"); InlineData("400030006421"); InlineData("LV40003000642"); InlineData(null)>]
let ``a value that can't be a Latvian number isn't sent, for any call`` (number: string) : Task =
    task {
        use handler = nothingSent ()
        let registry = Sources.latvia (connectionOver handler)
        let! company = registry.TryFindAsync(number, lv, ct)
        let! officers = registry.OfficersAsync(number, lv, ct)
        let! owners = registry.OwnersAsync(number, lv, OwnerPurpose.PublicRead, ct)

        match company, officers, owners with
        | Error(RegistryFailure.InvalidRequest _),
          Error(RegistryFailure.InvalidRequest _),
          Error(RegistryFailure.InvalidRequest _) -> Assert.Empty handler.Calls
        | other -> failwith $"%A{other}"
    }

// What the sources don't publish, answered without calling

[<Fact>]
let ``sources without officers or owners say so without calling`` () : Task =
    task {
        use handler = nothingSent ()
        let connection = connectionOver handler

        for registry, number, jurisdiction in
            [
                Sources.britishColumbia connection, "BC0772006", bc
                Sources.israel connection, "515532778", il
                Sources.singapore connection, "S94FC4771H", sg
                Sources.netherlands connection, "59581883", nl
            ] do
            let! officers = registry.OfficersAsync(number, jurisdiction, ct)

            let! owners =
                registry.OwnersAsync(number, jurisdiction, OwnerPurpose.CustomerDueDiligence, ct)

            match officers, owners with
            | Ok {
                     Value = NotAvailable _
                     Raw = ValueNone
                 },
              Ok {
                     Value = NotAvailable _
                     Raw = ValueNone
                 } -> ()
            | other -> failwith $"%s{registry.Coverage.Registry}: %A{other}"

        match! (Sources.netherlands connection).SearchAsync("x", nl, ct) with
        | Ok { Value = NotAvailable _ } -> Assert.Empty handler.Calls
        | other -> failwith $"%A{other}"
    }

// Fail closed, the same for every source

[<Fact>]
let ``outside its jurisdiction every source answers OutsideCoverage and sends nothing`` () : Task =
    task {
        use handler = nothingSent ()
        let elsewhere = Jurisdiction.ofCode "CA-ON"

        for registry, number, _ in allRegistries handler do
            let! company = registry.TryFindAsync(number, elsewhere, ct)
            let! officers = registry.OfficersAsync(number, elsewhere, ct)
            let! search = registry.SearchAsync("x", elsewhere, ct)

            match company, officers, search with
            | Error(RegistryFailure.OutsideCoverage _),
              Error(RegistryFailure.OutsideCoverage _),
              Error(RegistryFailure.OutsideCoverage _) -> ()
            | other -> failwith $"%s{registry.Coverage.Registry}: %A{other}"

        Assert.Empty handler.Calls
    }

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
let ``CKAN's success false is unreadable`` () : Task =
    task {
        use handler =
            new FakeHandler(fun _ -> ok """{"success":false,"error":{"message":"x"}}""")

        let connection = connectionOver handler

        for registry, number, jurisdiction in
            [
                Sources.israel connection, "515532778", il
                Sources.singapore connection, "S94FC4771H", sg
                Sources.latvia connection, "40003000642", lv
            ] do
            match! registry.TryFindAsync(number, jurisdiction, ct) with
            | Error(RegistryFailure.UnreadableAnswer _) -> ()
            | other -> failwith $"%s{registry.Coverage.Registry}: %A{other}"
    }

[<Fact>]
let ``a record missing what identifies it is unreadable`` () : Task =
    task {
        let cases: (string * Jurisdiction * string) list =
            [
                "BC0772006", bc, """{"total":1,"results":[{"source_id":"BC0772006","names":[],"attributes":[]}]}"""
                "515532778", il, ckanAnswer [ """{"מספר חברה":515532778,"סטטוס חברה":"פעילה"}""" ]
                "515532778", il, ckanAnswer [ """{"שם חברה":"X","סטטוס חברה":"פעילה"}""" ]
                "S94FC4771H", sg, ckanAnswer [ """{"uen":"S94FC4771H","entity_name":"na"}""" ]
                "S94FC4771H", sg, ckanAnswer [ """{"entity_name":"X"}""" ]
                "59581883", nl, """{"rechtsvormCode":"BV","lidstaat":"NL"}"""
                "40003000642", lv, ckanAnswer [ """{"regcode":40003000642}""" ]
                "40003000642", lv, ckanAnswer [ """{"name":"X"}""" ]
            ]

        for number, jurisdiction, body in cases do
            use handler = new FakeHandler(fun _ -> ok body)

            let registry =
                allRegistries handler
                |> List.find (fun (_, _, scope) -> scope = jurisdiction)
                |> fun (registry, _, _) -> registry

            match! registry.TryFindAsync(number, jurisdiction, ct) with
            | Error(RegistryFailure.UnreadableAnswer _) -> ()
            | other -> failwith $"%s{registry.Coverage.Registry} %s{body}: %A{other}"
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
let ``a 404 that isn't a statement of not available is refused`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> notFound)
        let connection = connectionOver handler

        for registry, number, jurisdiction in
            [
                Sources.britishColumbia connection, "BC0772006", bc
                Sources.israel connection, "515532778", il
                Sources.singapore connection, "S94FC4771H", sg
                Sources.latvia connection, "40003000642", lv
            ] do
            match! registry.TryFindAsync(number, jurisdiction, ct) with
            | Error(RegistryFailure.RequestRefused _) -> ()
            | other -> failwith $"%s{registry.Coverage.Registry}: %A{other}"
    }
