/// The US state registers on open data portals, replayed from answers recorded on 2026-11-08 with people's names redacted.
module UsStateSourceTests

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

/// A connection whose clock a wait moves on, for a lookup of more calls than the pace allows in one window.
let private movingConnectionOver (handler: FakeHandler) =
    let clock = FakeClock()
    SourceConnection(handler, (fun () -> clock.Now), clock.Delay)

let private co = Jurisdiction.ofCode "US-CO"
let private ctState = Jurisdiction.ofCode "US-CT"
let private ny = Jurisdiction.ofCode "US-NY"
let private oregon = Jurisdiction.ofCode "US-OR"
let private pa = Jurisdiction.ofCode "US-PA"
let private dc = Jurisdiction.ofCode "US-DC"

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

let private notAvailable (expected: string) (value: Availability<'T>) =
    match value with
    | NotAvailable reason -> Assert.Contains(expected, reason)
    | other -> failwith $"%A{other}"

let private sent (handler: FakeHandler) =
    handler.Calls |> List.map (fun call -> Uri.UnescapeDataString call.Target)

let private addressOf (kind: AddressKind) (company: Company) =
    match company.Addresses with
    | Available addresses -> (addresses |> List.find (fun address -> address.Kind = kind)).Address
    | other -> failwith $"%A{other}"

/// Every registry here, with a number of the right shape, for the checks every source has to pass.
let private allRegistries (handler: FakeHandler) =
    let connection = connectionOver handler

    [
        Sources.colorado connection, "20268371343", co
        Sources.connecticut connection, "0645848", ctState
        Sources.newYork connection, "5251439", ny
        Sources.oregon connection, "10003093", oregon
        Sources.pennsylvania connection, "0000000378", pa
        Sources.districtOfColumbia connection, "952891", dc
    ]

// Colorado

[<Fact>]
let ``Colorado reads a home record with its addresses and the dataset's date`` () : Task =
    task {
        use handler =
            new FakeHandler(fun _ ->
                { ok (fixture "colorado-home.json") with
                    Headers = []
                })

        let! evidence = findIn (Sources.colorado (connectionOver handler)) " 20268371343 " co
        let company = companyOf evidence
        Assert.Equal("Colorado Department of State business entities", evidence.Source)
        Assert.Equal(fixedTime, evidence.RetrievedAt)
        Assert.StartsWith("[{\"entityid\":\"20268371343\"", evidence.Raw.Value)
        Assert.Equal("Conscious Reality Psychotherapy and Somatic Healing, LLC", company.Name)
        Assert.Equal("20268371343", company.Number)
        Assert.Equal(CompanyStanding.Active, company.Standing)
        Assert.Equal(ValueSome "Good Standing", company.StatusAsPublished)
        Assert.Equal(RecordRole.Home, company.Role)
        Assert.Equal(ValueSome "DLLC", company.LegalForm)
        Assert.Equal(ValueSome(DateOnly(2026, 11, 6)), company.IncorporatedOn)
        Assert.Equal("Denver", (addressOf AddressKind.PrincipalOffice company).Locality.Value)
        Assert.Equal<string list>([ "1500 N GRANT ST"; "STE R" ], (addressOf AddressKind.RegisteredAgent company).Lines)
        Assert.Contains("entityid=20268371343", handler.Calls.Head.Target)
        Assert.Equal("data.colorado.gov", handler.Calls.Head.Host)
    }

[<Fact>]
let ``Colorado reads a foreign registration with its home state`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "colorado-foreign.json"))
        let! evidence = findIn (Sources.colorado (connectionOver handler)) "18991000331" co
        let company = companyOf evidence
        Assert.Equal(RecordRole.ForeignRegistration, company.Role)
        Assert.Equal(ValueSome "KS", company.HomeRegistration.Value.Country)
        Assert.Equal(CompanyStanding.Closed "Revoked", company.Standing)
        Assert.Equal(ValueNone, company.IncorporatedOn)

        match company.Addresses with
        | NotAvailable _ -> ()
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``Colorado not finding a number proves nothing`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok "[]")
        let! evidence = findIn (Sources.colorado (connectionOver handler)) "20131234567" co
        notAvailable "proves nothing" evidence.Value
    }

[<Theory>]
[<InlineData("Good Standing", "Active")>]
[<InlineData("Exists", "Registered")>]
[<InlineData("Delinquent", "ActiveWithConcern")>]
[<InlineData("Noncompliant", "ActiveWithConcern")>]
[<InlineData("Registered Agent Resigned", "ActiveWithConcern")>]
[<InlineData("Voluntarily Dissolved", "Dissolved")>]
[<InlineData("Administratively Dissolved", "Dissolved")>]
[<InlineData("Judicially Dissolved", "Dissolved")>]
[<InlineData("Dissolved (Term Expired)", "Dissolved")>]
[<InlineData("Withdrawn", "Closed")>]
[<InlineData("Revoked", "Closed")>]
[<InlineData("Merged", "Closed")>]
[<InlineData("Converted", "Closed")>]
[<InlineData("Consolidated", "Closed")>]
[<InlineData("Effectiveness Prevented", "Closed")>]
[<InlineData("A word added later", "Unknown")>]
let ``every Colorado entity status has a standing, and a new one is unknown`` (word: string, expected: string) =
    Assert.StartsWith(expected, $"%A{Colorado.standing (ValueSome word)}")

[<Theory; InlineData("abc"); InlineData("123456789"); InlineData("123456789012"); InlineData(""); InlineData(null)>]
let ``a value that can't be a Colorado entity ID isn't sent`` (number: string) : Task =
    task {
        use handler = new FakeHandler(fun _ -> failwith "Nothing should be sent")

        match! (Sources.colorado (connectionOver handler)).TryFindAsync(number, co, ct) with
        | Error(RegistryFailure.InvalidRequest _) -> Assert.Empty handler.Calls
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``Colorado offers no officers or owners, and says so without calling`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> failwith "Nothing should be sent")
        let registry = Sources.colorado (connectionOver handler)
        let! officers = registry.OfficersAsync("20268371343", co, ct)

        let! owners =
            registry.OwnersAsync("20268371343", co, OwnerPurpose.CustomerDueDiligence, ct)

        match officers, owners with
        | Ok { Value = NotAvailable reason }, Ok { Value = NotAvailable _ } ->
            Assert.Contains("collects no officers", reason)
            Assert.Empty handler.Calls
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a Colorado name search sends normalised words, quote and wildcard taken out, and keeps the names matching``
    ()
    : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "colorado-home.json"))

        match! (Sources.colorado (connectionOver handler)).SearchAsync("conscious' reality%", co, ct) with
        | Ok { Value = Available found } ->
            Assert.Equal("20268371343", found.Matches.Head.Number)
            Assert.True found.Complete
            Assert.Contains("upper(entityname) like '%C%ONSCIOU%S%R%EALITY%'", (sent handler).Head)
        | other -> failwith $"%A{other}"
    }

// Connecticut

[<Fact>]
let ``Connecticut reads a foreign business with its home state`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "connecticut-foreign.json"))

        let! evidence =
            findIn (Sources.connecticut (connectionOver handler)) "645848" ctState

        let company = companyOf evidence
        Assert.Equal("AMERICAN CINEMA NETWORK, INC.", company.Name)
        Assert.Equal("0645848", company.Number)
        Assert.Equal(CompanyStanding.Closed "Revoked", company.Standing)
        Assert.Equal(RecordRole.ForeignRegistration, company.Role)
        Assert.Equal(ValueSome "CA", company.HomeRegistration.Value.Country)
        Assert.Equal(ValueSome "Stock", company.LegalForm)
        Assert.True evidence.Raw.IsSome
        Assert.Contains("accountnumber = '0645848'", (sent handler).Head)
    }

[<Fact>]
let ``Connecticut reads an active domestic business and its addresses`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "connecticut-active.json"))

        let! evidence =
            findIn (Sources.connecticut (connectionOver handler)) "3546983" ctState

        let company = companyOf evidence
        Assert.Equal(CompanyStanding.Active, company.Standing)
        Assert.Equal(RecordRole.Home, company.Role)
        Assert.Equal(ValueSome(DateOnly(2026, 11, 7)), company.IncorporatedOn)

        Assert.Equal(
            ValueSome "Trumbull",
            (addressOf (AddressKind.Other "Business address on the Connecticut Business Registry") company).Locality
        )

        Assert.Equal<string list>(
            [ "751 Daniels Farm Rd, Trumbull, CT, United States, 06611" ],
            (addressOf AddressKind.Postal company).Lines
        )
    }

[<Fact>]
let ``Connecticut not finding a number proves nothing`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok "[]")
        let! evidence = findIn (Sources.connecticut (connectionOver handler)) "1" ctState
        notAvailable "proves nothing" evidence.Value
    }

[<Theory>]
[<InlineData("Active", "", "Active")>]
[<InlineData("Active", "Annual report due", "Active")>]
[<InlineData("Active", "First report due", "Active")>]
[<InlineData("Active", "Annual report past due", "ActiveWithConcern")>]
[<InlineData("Active", "First report past due", "ActiveWithConcern")>]
[<InlineData("Active", "Admin Dissolution Initiated", "ActiveWithConcern")>]
[<InlineData("Active", "Ready for dissolution", "ActiveWithConcern")>]
[<InlineData("Active", "Ready for Forfeiture", "ActiveWithConcern")>]
[<InlineData("Active", "Ready for Revocation", "ActiveWithConcern")>]
[<InlineData("Active", "No Agent", "ActiveWithConcern")>]
[<InlineData("Active", "Fraudulent Filing Complaint Sent", "ActiveWithConcern")>]
[<InlineData("Active", "A sub-status added later", "Unknown")>]
[<InlineData("Active - Pending Domestication", "Annual report past due", "ActiveWithConcern")>]
[<InlineData("Pending Merger", "", "ActiveWithConcern")>]
[<InlineData("Pending Conversion", "", "ActiveWithConcern")>]
[<InlineData("Pending Domestication", "", "ActiveWithConcern")>]
[<InlineData("Pending Withdrawal", "", "WindingUp")>]
[<InlineData("Dissolved", "Annual report past due", "Dissolved")>]
[<InlineData("Forfeited", "", "Closed")>]
[<InlineData("Revoked", "", "Closed")>]
[<InlineData("Withdrawn", "", "Closed")>]
[<InlineData("Cancelled", "", "Closed")>]
[<InlineData("Merged", "", "Closed")>]
[<InlineData("Expired", "", "Closed")>]
[<InlineData("Renunciated", "", "Closed")>]
[<InlineData("Redomesticated", "", "Closed")>]
[<InlineData("Domesticated", "", "Closed")>]
[<InlineData("Consolidation", "", "Closed")>]
[<InlineData("Converted Out", "", "Closed")>]
[<InlineData("Converted", "", "Closed")>]
[<InlineData("Removed", "", "Closed")>]
[<InlineData("Recorded", "", "Registered")>]
[<InlineData("Registered", "", "Registered")>]
[<InlineData("Reserved", "", "Unknown")>]
[<InlineData("Expired Reservation", "", "Unknown")>]
[<InlineData("Reserved Cancel", "", "Unknown")>]
[<InlineData("Rejected", "", "Unknown")>]
[<InlineData("Pending Filing", "", "Unknown")>]
[<InlineData("A status added later", "", "Unknown")>]
let ``every Connecticut status and sub-status has a standing, and a new one is unknown``
    (status: string, sub: string, expected: string)
    =
    let sub = if sub = "" then ValueNone else ValueSome sub
    Assert.StartsWith(expected, $"%A{Connecticut.standing (ValueSome status) sub}")

[<Fact>]
let ``a Connecticut report past due is stated as a concern`` () =
    let concerns = Connecticut.concerns (ValueSome "Annual report past due")
    Assert.Equal<CompanyConcern list>([ CompanyConcern.AnnualFilingOverdue ], concerns.Stated)
    Assert.DoesNotContain(CompanyConcern.AnnualFilingOverdue, concerns.Unconfirmed)
    Assert.Equal(CompanyConcerns.notPublished, Connecticut.concerns (ValueSome "Annual report due"))

[<Fact>]
let ``Connecticut reads principals by the master row's key, once each`` () : Task =
    task {
        use handler =
            new FakeHandler(fun call ->
                if call.Target.Contains "n7gp-d28j" then
                    ok (fixture "connecticut-foreign.json")
                else
                    ok (fixture "connecticut-principals.json"))

        match! (Sources.connecticut (connectionOver handler)).OfficersAsync("0645848", ctState, ct) with
        | Ok { Value = Available officers } ->
            Assert.Equal(2, officers.Length)
            let first = officers.Head
            Assert.Equal("PERSON ONE", first.Name)
            Assert.Equal(PartyKind.Individual, first.Kind)
            Assert.Equal(OfficerRole.Other "Principal", first.Role)
            Assert.True first.Current
            Assert.Equal(ValueSome "SAN JOSE", first.Address.Value.Locality)
            Assert.Equal(OfficerRole.Director, officers[1].Role)
            Assert.Contains("business_id = '001t000000WmqC5AAJ'", (sent handler)[1])
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``Connecticut principals for a number not listed are NotAvailable, with one call`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok "[]")

        match! (Sources.connecticut (connectionOver handler)).OfficersAsync("1", ctState, ct) with
        | Ok { Value = NotAvailable reason } ->
            Assert.Contains("proves nothing", reason)
            Assert.Single handler.Calls |> ignore
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``Connecticut listing no principals doesn't say there are none`` () : Task =
    task {
        use handler =
            new FakeHandler(fun call ->
                if call.Target.Contains "n7gp-d28j" then
                    ok (fixture "connecticut-foreign.json")
                else
                    ok "[]")

        match! (Sources.connecticut (connectionOver handler)).OfficersAsync("0645848", ctState, ct) with
        | Ok { Value = value } -> notAvailable "doesn't say it has none" value
        | other -> failwith $"%A{other}"
    }

[<Theory; InlineData("abc"); InlineData("12345678"); InlineData(""); InlineData(null)>]
let ``a value that can't be a Connecticut business ID isn't sent`` (number: string) : Task =
    task {
        use handler = new FakeHandler(fun _ -> failwith "Nothing should be sent")
        let registry = Sources.connecticut (connectionOver handler)

        match! registry.TryFindAsync(number, ctState, ct) with
        | Error(RegistryFailure.InvalidRequest _) -> ()
        | other -> failwith $"%A{other}"

        match! registry.OfficersAsync(number, ctState, ct) with
        | Error(RegistryFailure.InvalidRequest _) -> Assert.Empty handler.Calls
        | other -> failwith $"%A{other}"
    }

// New York

[<Fact>]
let ``New York reads an active foreign corporation with its addresses`` () : Task =
    task {
        use handler =
            new FakeHandler(fun call ->
                if call.Target.Contains "3gg2-jgnp" then
                    ok "[]"
                else
                    ok (fixture "newyork-active.json"))

        let! evidence = findIn (Sources.newYork (connectionOver handler)) "05251439" ny
        let company = companyOf evidence
        Assert.Equal("VOORWINDEN GREENHOUSE BUILDING, LTD.", company.Name)
        Assert.Equal("5251439", company.Number)
        Assert.Equal(CompanyStanding.Active, company.Standing)
        Assert.Equal(RecordRole.ForeignRegistration, company.Role)
        Assert.Equal(ValueSome "Michigan", company.HomeRegistration.Value.Country)
        Assert.Equal(ValueSome "FOREIGN BUSINESS CORPORATION", company.LegalForm)
        Assert.Equal("C/O CORPORATE CREATIONS NETWORK INC.", (addressOf AddressKind.Service company).Lines.Head)
        Assert.Equal(ValueSome "HARRISON", (addressOf AddressKind.RegisteredAgent company).Locality)
        Assert.Equal(2, handler.Calls.Length)
        Assert.Contains("dos_id = '5251439'", (sent handler).Head)
        Assert.Contains("corpid_num = 5251439 AND status IS NOT NULL", (sent handler)[1])
    }

[<Fact>]
let ``a later status other than Active in New York's weekly history overrides the monthly active list`` () : Task =
    task {
        let history =
            """[{"corpid_num":"5251439","film_num":"261102000001","date_filed":"2026-11-02T00:00:00.000","status":"Inactive"}]"""

        use handler =
            new FakeHandler(fun call ->
                if call.Target.Contains "3gg2-jgnp" then
                    ok history
                else
                    ok (fixture "newyork-active.json"))

        let! evidence = findIn (Sources.newYork (connectionOver handler)) "5251439" ny
        let company = companyOf evidence
        Assert.Equal(CompanyStanding.Closed "Inactive", company.Standing)

        Assert.Equal(
            ValueSome "Inactive in the status history, filed 2026-11-02; Active on the monthly active corporations list",
            company.StatusAsPublished
        )

        Assert.Contains(history, evidence.Raw.Value)

        use active =
            new FakeHandler(fun call ->
                if call.Target.Contains "3gg2-jgnp" then
                    ok (history.Replace("Inactive", "Active"))
                else
                    ok (fixture "newyork-active.json"))

        let! evidence = findIn (Sources.newYork (connectionOver active)) "5251439" ny
        Assert.Equal(CompanyStanding.Active, (companyOf evidence).Standing)
    }

[<Fact>]
let ``New York's search reads the active list and All Filings, takes a DOS ID on both once, and is never complete``
    ()
    : Task =
    task {
        use handler =
            new FakeHandler(fun call ->
                if call.Target.Contains "63wc-4exh" then
                    ok (fixture "newyork-filings-search.json")
                else
                    ok (fixture "newyork-active.json"))

        match! (Sources.newYork (movingConnectionOver handler)).SearchAsync("VOORWINDEN", ny, ct) with
        | Ok {
                 Value = Available found
                 Raw = ValueSome raw
             } ->
            Assert.Equal<string list>([ "5251439" ], found.Matches |> List.map _.Number)
            Assert.Equal(ValueSome "Active", found.Matches.Head.StatusAsPublished)
            Assert.False found.Complete
            Assert.Contains("\"corpid_num\":\"5251439\"", raw)

            Assert.Contains(
                "$select=corpid_num,corp_name&$group=corpid_num,corp_name&$where=upper(corp_name) like '%V%OORWINDEN%'",
                Uri.UnescapeDataString handler.Calls[1].Target
            )
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a dissolved New York entity only in All Filings is a search match with no status`` () : Task =
    task {
        // DIFFO AUTO GLASS INC., DOS ID 2398608: on All Filings and not on the active list, as answered on 2026-11-08.
        use handler =
            new FakeHandler(fun call ->
                if call.Target.Contains "63wc-4exh" then
                    ok (fixture "newyork-filings-dissolved.json")
                else
                    ok "[]")

        match! (Sources.newYork (movingConnectionOver handler)).SearchAsync("diffo auto glass", ny, ct) with
        | Ok { Value = Available found } ->
            match found.Matches with
            | [ only ] ->
                Assert.Equal("2398608", only.Number)
                Assert.Equal("DIFFO AUTO GLASS INC.", only.Name)
                Assert.Equal(ValueNone, only.StatusAsPublished)
            | other -> failwith $"%A{other}"

            Assert.False found.Complete
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``New York reads an inactive entity from its status history and the filing that set it`` () : Task =
    task {
        use handler =
            new FakeHandler(fun call ->
                if call.Target.Contains "n9v6-gdp6" then
                    ok "[]"
                elif call.Target.Contains "3gg2-jgnp" then
                    ok (fixture "newyork-history.json")
                else
                    ok (fixture "newyork-filing.json"))

        let! evidence = findIn (Sources.newYork (movingConnectionOver handler)) "416717" ny
        let company = companyOf evidence
        Assert.Equal("RAINBOW GOLF CLUB, INC.", company.Name)
        Assert.Equal("416717", company.Number)
        Assert.Equal(CompanyStanding.Closed "Inactive", company.Standing)
        Assert.Equal(ValueSome "Inactive, after CERTIFICATE OF DISSOLUTION-CANCELLATION", company.StatusAsPublished)
        Assert.Equal(ValueSome(DateOnly(2023, 11, 28)), company.CeasedOn)
        Assert.Equal(RecordRole.Home, company.Role)
        Assert.StartsWith("[[]", evidence.Raw.Value)

        match company.Addresses with
        | NotAvailable _ -> ()
        | other -> failwith $"%A{other}"

        let calls = sent handler
        Assert.Equal(3, calls.Length)
        Assert.Contains("corpid_num = 416717 AND status IS NOT NULL", calls[1])
        Assert.Contains("date_filed DESC, film_num DESC", calls[1])
        Assert.Contains("film_num = '240102004837'", calls[2])
    }

[<Fact>]
let ``New York not finding a number anywhere proves nothing`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok "[]")
        let! evidence = findIn (Sources.newYork (connectionOver handler)) "99999999" ny
        notAvailable "proves nothing" evidence.Value
        Assert.Equal(2, handler.Calls.Length)
    }

[<Fact>]
let ``a New York status whose filing isn't held is NotAvailable`` () : Task =
    task {
        use handler =
            new FakeHandler(fun call ->
                if call.Target.Contains "3gg2-jgnp" then
                    ok (fixture "newyork-history.json")
                else
                    ok "[]")

        let! evidence = findIn (Sources.newYork (movingConnectionOver handler)) "416717" ny
        notAvailable "filings dataset doesn't hold" evidence.Value
    }

[<Theory>]
[<InlineData("Active", "Active")>]
[<InlineData("Suspended", "ActiveWithConcern")>]
[<InlineData("Inactive", "Closed")>]
[<InlineData("InActive", "Closed")>]
[<InlineData("Discontinued", "Closed")>]
[<InlineData("A word added later", "Unknown")>]
let ``every New York status history word has a standing, and a new one is unknown`` (word: string, expected: string) =
    Assert.StartsWith(expected, $"%A{NewYork.standing (ValueSome word)}")

[<Fact>]
let ``New York gives the chief executive officer only`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "newyork-active.json"))

        match! (Sources.newYork (connectionOver handler)).OfficersAsync("5251439", ny, ct) with
        | Ok { Value = Available [ officer ] } ->
            Assert.Equal("PERSON ONE", officer.Name)
            Assert.Equal(OfficerRole.Other "Chief executive officer", officer.Role)
            Assert.Equal(ValueSome "LANSING", officer.Address.Value.Locality)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``New York has no officers for a corporation off the active list`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok "[]")

        match! (Sources.newYork (connectionOver handler)).OfficersAsync("416717", ny, ct) with
        | Ok { Value = value } -> notAvailable "active corporations list" value
        | other -> failwith $"%A{other}"
    }

[<Theory; InlineData("abc"); InlineData("123456789"); InlineData("000"); InlineData(""); InlineData(null)>]
let ``a value that can't be a New York DOS ID isn't sent`` (number: string) : Task =
    task {
        use handler = new FakeHandler(fun _ -> failwith "Nothing should be sent")

        match! (Sources.newYork (connectionOver handler)).TryFindAsync(number, ny, ct) with
        | Error(RegistryFailure.InvalidRequest _) -> Assert.Empty handler.Calls
        | other -> failwith $"%A{other}"
    }

// Oregon

[<Fact>]
let ``Oregon collapses one business's rows to one company`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "oregon-rows.json"))
        let! evidence = findIn (Sources.oregon (connectionOver handler)) "100030-93" oregon
        let company = companyOf evidence
        Assert.Equal("OXFORD HOUSE, INC.", company.Name)
        Assert.Equal("10003093", company.Number)
        Assert.Equal(CompanyStanding.Active, company.Standing)
        Assert.Equal(RecordRole.ForeignRegistration, company.Role)
        Assert.Equal(ValueSome "DE", company.HomeRegistration.Value.Country)
        Assert.Equal(ValueSome "SILVER SPRING", (addressOf AddressKind.PrincipalOffice company).Locality)
        Assert.Equal(ValueSome "SALEM", (addressOf AddressKind.RegisteredAgent company).Locality)
        Assert.Equal(ValueSome "MD", (addressOf AddressKind.Postal company).Region)
        Assert.Contains("registry_number = '10003093'", (sent handler).Head)
    }

[<Fact>]
let ``Oregon not finding a number proves nothing, since it lists active businesses only`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok "[]")
        let! evidence = findIn (Sources.oregon (connectionOver handler)) "1" oregon
        notAvailable "active businesses only" evidence.Value
    }

[<Fact>]
let ``an Oregon assumed business name is no company`` () : Task =
    task {
        let body =
            """[{"registry_number":"1","business_name":"X","entity_type":"ASSUMED BUSINESS NAME","associated_name_type":"AUTHORIZED REPRESENTATIVE","first_name":"PERSON","last_name":"ONE"}]"""

        use handler = new FakeHandler(fun _ -> ok body)
        let! evidence = findIn (Sources.oregon (connectionOver handler)) "1" oregon
        notAvailable "assumed business name" evidence.Value
    }

[<Fact>]
let ``Oregon rows naming two businesses aren't told apart`` () : Task =
    task {
        let body =
            """[{"registry_number":"1","business_name":"X","entity_type":"DOMESTIC BUSINESS CORPORATION"},{"registry_number":"1","business_name":"Y","entity_type":"DOMESTIC BUSINESS CORPORATION"}]"""

        use handler = new FakeHandler(fun _ -> ok body)
        let! evidence = findIn (Sources.oregon (connectionOver handler)) "1" oregon
        notAvailable "2 businesses" evidence.Value
    }

[<Theory>]
[<InlineData("OR", "FOREIGN LIMITED LIABILITY COMPANY", "Home")>]
[<InlineData("WA", "DOMESTIC BUSINESS CORPORATION", "ForeignRegistration")>]
[<InlineData("", "DOMESTIC BUSINESS CORPORATION", "Home")>]
[<InlineData("", "FOREIGN BUSINESS CORPORATION", "ForeignRegistration")>]
[<InlineData("", "COOPERATIVE", "NotStated")>]
let ``an Oregon record's jurisdiction decides its role, and its type when it has none``
    (place: string, kind: string, expected: string)
    =
    let place = if place = "" then ValueNone else ValueSome place
    Assert.Equal(expected, $"%A{Oregon.role place (ValueSome kind)}")

[<Fact>]
let ``an Oregon search groups rows to one per business`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "oregon-search.json"))

        match! (Sources.oregon (connectionOver handler)).SearchAsync("oxford house", oregon, ct) with
        | Ok { Value = Available found } ->
            Assert.Equal(5, found.Matches.Length)
            Assert.Equal("10003093", found.Matches[1].Number)
            // Active businesses only, so never every business with the name.
            Assert.False found.Complete
            let query = (sent handler).Head
            Assert.Contains("upper(business_name) like '%O%XFORD%H%OUSE%'", query)
            Assert.Contains("$group=registry_number,business_name,entity_type", query)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``Oregon offers no officers or owners, and says so without calling`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> failwith "Nothing should be sent")
        let registry = Sources.oregon (connectionOver handler)
        let! officers = registry.OfficersAsync("10003093", oregon, ct)
        let! owners = registry.OwnersAsync("10003093", oregon, OwnerPurpose.PublicRead, ct)

        match officers, owners with
        | Ok { Value = NotAvailable _ }, Ok { Value = NotAvailable _ } -> Assert.Empty handler.Calls
        | other -> failwith $"%A{other}"
    }

[<Theory; InlineData("abc"); InlineData("1234567890"); InlineData(""); InlineData(null)>]
let ``a value that can't be an Oregon registry number isn't sent`` (number: string) : Task =
    task {
        use handler = new FakeHandler(fun _ -> failwith "Nothing should be sent")

        match! (Sources.oregon (connectionOver handler)).TryFindAsync(number, oregon, ct) with
        | Error(RegistryFailure.InvalidRequest _) -> Assert.Empty handler.Calls
        | other -> failwith $"%A{other}"
    }

// Pennsylvania

[<Fact>]
let ``Pennsylvania reads a listed business as registered, never active`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "pennsylvania-rows.json"))
        let! evidence = findIn (Sources.pennsylvania (connectionOver handler)) "378" pa
        let company = companyOf evidence
        Assert.Equal("ABSG CONSULTING INC.", company.Name)
        Assert.Equal("0000000378", company.Number)
        Assert.Equal(CompanyStanding.Registered ValueNone, company.Standing)
        Assert.False(CompanyStanding.isActive company.Standing)
        Assert.Equal(ValueNone, company.StatusAsPublished)
        Assert.Equal(RecordRole.ForeignRegistration, company.Role)
        Assert.Equal(ValueNone, company.HomeRegistration)

        Assert.Equal(
            ValueSome "Harrisburg",
            (addressOf (AddressKind.Other "Business address on the Department of State's listing") company).Locality
        )

        Assert.Contains("filing_number = '0000000378'", (sent handler).Head)

        match Pennsylvania.coverage.Standing with
        | Support.Partial caveat -> Assert.Contains("more listed as active than exist", caveat)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``Pennsylvania not finding a number proves nothing`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok "[]")
        let! evidence = findIn (Sources.pennsylvania (connectionOver handler)) "1" pa
        notAvailable "proves nothing" evidence.Value
    }

[<Fact>]
let ``Pennsylvania reads governors and officers from the rows`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "pennsylvania-rows.json"))

        match! (Sources.pennsylvania (connectionOver handler)).OfficersAsync("0000000378", pa, ct) with
        | Ok { Value = Available officers } ->
            Assert.Equal(6, officers.Length)

            Assert.Equal(
                3,
                officers
                |> List.filter (fun officer -> officer.Role = OfficerRole.Director)
                |> List.length
            )

            Assert.Contains(officers, (fun officer -> officer.Role = OfficerRole.Secretary))
            Assert.Contains(officers, (fun officer -> officer.Role = OfficerRole.Other "Treasurer"))
            Assert.Equal("PERSON ONE", officers.Head.Name)
        | other -> failwith $"%A{other}"
    }

[<Theory>]
[<InlineData("Governor", "Director")>]
[<InlineData("DIRECTOR", "Director")>]
[<InlineData("Board Member", "Director")>]
[<InlineData("Chairman", "Director")>]
[<InlineData("Assistant Secretary", "Secretary")>]
[<InlineData("President", "Other")>]
[<InlineData("Organizer", "Other")>]
let ``a Pennsylvania party type has an officer role`` (word: string, expected: string) =
    Assert.StartsWith(expected, $"%A{Pennsylvania.officerRole (ValueSome word)}")

[<Fact>]
let ``a Pennsylvania search reads the distinct view, and one short list isn't complete, as the data holds active registrations only``
    ()
    : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "pennsylvania-search.json"))

        match! (Sources.pennsylvania (connectionOver handler)).SearchAsync("absg cons", pa, ct) with
        | Ok { Value = Available found } ->
            Assert.Equal("0000000378", found.Matches.Head.Number)
            Assert.Single found.Matches |> ignore
            Assert.False found.Complete
            Assert.Contains("3urc-uaba", handler.Calls.Head.Target)
        | other -> failwith $"%A{other}"
    }

[<Theory; InlineData("abc"); InlineData("123456789012"); InlineData(""); InlineData(null)>]
let ``a value that can't be a Pennsylvania filing number isn't sent`` (number: string) : Task =
    task {
        use handler = new FakeHandler(fun _ -> failwith "Nothing should be sent")

        match! (Sources.pennsylvania (connectionOver handler)).TryFindAsync(number, pa, ct) with
        | Error(RegistryFailure.InvalidRequest _) -> Assert.Empty handler.Calls
        | other -> failwith $"%A{other}"
    }

// District of Columbia

[<Fact>]
let ``DC reads a foreign registration with its addresses and the record's date`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "dc-registration.json"))

        let! evidence =
            findIn (Sources.districtOfColumbia (connectionOver handler)) "952891" dc

        let company = companyOf evidence
        Assert.Equal("National General Motor Club, Inc.", company.Name)
        Assert.Equal("952891", company.Number)
        Assert.Equal(CompanyStanding.Active, company.Standing)
        Assert.Equal(RecordRole.ForeignRegistration, company.Role)
        Assert.Equal(ValueSome "Foreign Business Corporation", company.LegalForm)
        Assert.Equal(ValueSome(DateOnly(1995, 8, 29)), company.IncorporatedOn)
        Assert.Equal(ValueSome(DateOnly(2026, 11, 6)), evidence.AsOf)

        Assert.Equal(
            ValueSome "Winston-Salem",
            (addressOf (AddressKind.Other "Business address on the DC corporate register") company).Locality
        )

        Assert.Equal<string list>(
            [ "1156 15TH STREET NW"; "Suite 605" ],
            (addressOf AddressKind.RegisteredAgent company).Lines
        )

        Assert.Contains("FILE_NUMBER = '952891'", (sent handler).Head)
    }

[<Fact>]
let ``DC not finding a file number proves nothing`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok """{"features":[]}""")

        let! evidence =
            findIn (Sources.districtOfColumbia (connectionOver handler)) "C00000000001" dc

        notAvailable "proves nothing" evidence.Value
    }

/// The registration table answers the first query and the trade name table the second.
let private dcTables (registration: string) (tradeNames: Fake.Answer) =
    new FakeHandler(fun call ->
        if call.Target.Contains "/1/query" then
            tradeNames
        else
            ok registration)

[<Fact>]
let ``DC reads every trade name filed for the entity as another name, with both answers as raw`` () : Task =
    task {
        use handler =
            dcTables (fixture "dc-registration-trading.json") (ok (fixture "dc-trade-names.json"))

        let! evidence =
            findIn (Sources.districtOfColumbia (connectionOver handler)) "L00008210480" dc

        let company = companyOf evidence
        Assert.Equal("Northeast Towing & Recovery Limited Liability Company", company.Name)
        Assert.Equal<string list>([ "202 towing" ], company.OtherNames)
        Assert.Equal(RecordRole.Home, company.Role)
        Assert.Contains("INITIAL_FILENUMBER = 'L00008210480'", (sent handler)[1])
        Assert.Contains("TN0008542392", evidence.Raw.Value)
        Assert.Contains("Kenilworth", evidence.Raw.Value)
    }

[<Fact>]
let ``a DC biennial report past its due date is stated overdue, and one not yet due is no concern`` () : Task =
    task {
        let read (due: string) =
            task {
                let registration =
                    (fixture "dc-registration-trading.json").Replace("\"04/01/2027\"", $"\"{due}\"")

                use handler = dcTables registration (ok """{"features":[]}""")

                let! evidence =
                    findIn (Sources.districtOfColumbia (connectionOver handler)) "L00008210480" dc

                return (companyOf evidence).Concerns
            }

        let! overdue = read "04/01/2025"
        Assert.Equal<CompanyConcern list>([ CompanyConcern.AnnualFilingOverdue ], overdue.Stated)
        let! notDue = read "04/01/2027"
        Assert.Empty notDue.Stated
        Assert.DoesNotContain(CompanyConcern.AnnualFilingOverdue, notDue.Unconfirmed)
        let! unreadable = read "next year"
        Assert.Equal(CompanyConcerns.notPublished, unreadable)
    }

[<Fact>]
let ``Connecticut and Pennsylvania say their officers can't be told current from former`` () =
    use handler = new FakeHandler(fun _ -> failwith "Nothing should be sent")
    let connection = connectionOver handler

    for registry in [ Sources.connecticut connection; Sources.pennsylvania connection ] do
        match registry.Coverage.Officers with
        | Support.Partial caveat -> Assert.Contains("can't be told from a current one", caveat)
        | other -> failwith $"%s{registry.Coverage.Registry}: %A{other}"

[<Fact>]
let ``a DC trade name read that fails fails the lookup, so no name goes unscreened`` () : Task =
    task {
        use handler = dcTables (fixture "dc-registration-trading.json") (answer 503 "down")

        match! (Sources.districtOfColumbia (connectionOver handler)).TryFindAsync("L00008210480", dc, ct) with
        | Error(RegistryFailure.Unavailable _) -> ()
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``DC's trade name guard counts rows, so one name filed 500 times still fails the lookup`` () : Task =
    task {
        let row index =
            $"""{{"attributes":{{"TRADE_NAME":"202 towing","FILE_NUMBER":"TN{index}","INITIAL_FILENUMBER":"L00008210480"}}}}"""

        let names = $"""{{"features":[{String.concat "," (List.init 500 row)}]}}"""
        use handler = dcTables (fixture "dc-registration-trading.json") (ok names)

        match! (Sources.districtOfColumbia (connectionOver handler)).TryFindAsync("L00008210480", dc, ct) with
        | Error(RegistryFailure.UnreadableAnswer _) -> ()
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a DC answer the service cut short is never read as the whole of it`` () : Task =
    task {
        let cut (body: string) =
            body.Substring(0, body.LastIndexOf '}') + ",\"exceededTransferLimit\":true}"

        use names =
            dcTables (fixture "dc-registration-trading.json") (ok (cut (fixture "dc-trade-names.json")))

        match! (Sources.districtOfColumbia (connectionOver names)).TryFindAsync("L00008210480", dc, ct) with
        | Error(RegistryFailure.UnreadableAnswer _) -> ()
        | other -> failwith $"%A{other}"

        use registration =
            new FakeHandler(fun _ -> ok (cut (fixture "dc-registration-trading.json")))

        let! evidence =
            findIn (Sources.districtOfColumbia (connectionOver registration)) "L00008210480" dc

        notAvailable "More than 1 records" evidence.Value

        use owners = new FakeHandler(fun _ -> ok (cut (fixture "dc-owners.json")))

        match!
            (Sources.districtOfColumbia (connectionOver owners)).OwnersAsync("952891", dc, OwnerPurpose.PublicRead, ct)
        with
        | Ok { Value = value } -> notAvailable "more than one answer reads" value
        | other -> failwith $"%A{other}"

        use search =
            new FakeHandler(fun _ -> ok (cut (fixture "dc-registration-trading.json")))

        match! (Sources.districtOfColumbia (connectionOver search)).SearchAsync("Northeast", dc, ct) with
        | Ok { Value = Available found } ->
            Assert.Single found.Matches |> ignore
            Assert.False found.Complete
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a DC date beyond what a date can hold is no date, not a crash`` () : Task =
    task {
        let registration =
            (fixture "dc-registration-trading.json")
                .Replace("\"EFFECTIVE_DATE\":1732251600000", "\"EFFECTIVE_DATE\":9223372036854775807")

        use handler = dcTables registration (ok """{"features":[]}""")

        let! evidence =
            findIn (Sources.districtOfColumbia (connectionOver handler)) "L00008210480" dc

        Assert.Equal(ValueNone, (companyOf evidence).IncorporatedOn)
    }

[<Fact>]
let ``DC answering 200 with an error object is unreadable`` () : Task =
    task {
        use handler =
            new FakeHandler(fun _ -> ok """{"error":{"code":400,"message":"Invalid query"}}""")

        match! (Sources.districtOfColumbia (connectionOver handler)).TryFindAsync("952891", dc, ct) with
        | Error(RegistryFailure.UnreadableAnswer _) -> ()
        | other -> failwith $"%A{other}"
    }

[<Theory>]
[<InlineData("Active - In Good Standing", "Active")>]
[<InlineData("Active - Not in Good Standing", "ActiveWithConcern")>]
[<InlineData("Active - In Good Standing - Pending Conversion", "ActiveWithConcern")>]
[<InlineData("Dissolved", "Dissolved")>]
[<InlineData("Inactive - Cancelled", "Closed")>]
[<InlineData("Revoked", "Closed")>]
[<InlineData("Withdrawn", "Closed")>]
[<InlineData("Terminated", "Closed")>]
[<InlineData("Merged", "Closed")>]
[<InlineData("Consolidated", "Closed")>]
[<InlineData("Converted", "Closed")>]
[<InlineData("Domesticated", "Closed")>]
[<InlineData("A word added later", "Unknown")>]
let ``every DC entity status has a standing, and a new one is unknown`` (word: string, expected: string) =
    Assert.StartsWith(expected, $"%A{DistrictOfColumbia.standing (ValueSome word)}")

[<Fact>]
let ``DC reads beneficial owners as filed, with their details unconfirmed`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "dc-owners.json"))

        for purpose in [ OwnerPurpose.PublicRead; OwnerPurpose.CustomerDueDiligence ] do
            match! (Sources.districtOfColumbia (connectionOver handler)).OwnersAsync("952891", dc, purpose, ct) with
            | Ok { Value = Available listing } ->
                let owner = listing.Owners.Head
                Assert.Equal(ValueSome "The Allstate Corporation", owner.Name)
                Assert.Equal(PartyKind.Unknown ValueNone, owner.Kind)
                Assert.Equal(ControlKind.Other, owner.Control.Head.Kind)
                Assert.True owner.Current
                Assert.StartsWith("3100 Sanders Road", owner.Address.Value.Lines.Head)
                Assert.Equal<OwnershipGap list>([ OwnershipGap.OwnerDetailsNotConfirmed ], listing.Gaps)
                Assert.False(OwnerListing.isComplete listing)
            | other -> failwith $"%A{other}"

        Assert.Contains("INITIALFILENUMBER = '952891'", (sent handler).Head)
        Assert.Contains("/FeatureServer/2/query", handler.Calls.Head.Target)
    }

[<Fact>]
let ``DC listing no owners doesn't say there are none`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok """{"features":[]}""")

        match!
            (Sources.districtOfColumbia (connectionOver handler)).OwnersAsync("952891", dc, OwnerPurpose.PublicRead, ct)
        with
        | Ok { Value = value } -> notAvailable "doesn't say there are none" value
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``DC offers no officers, and says so without calling`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> failwith "Nothing should be sent")

        match! (Sources.districtOfColumbia (connectionOver handler)).OfficersAsync("952891", dc, ct) with
        | Ok { Value = NotAvailable _ } -> Assert.Empty handler.Calls
        | other -> failwith $"%A{other}"
    }

[<Theory>]
[<InlineData("952891", "952891")>]
[<InlineData(" c00007209052 ", "C00007209052")>]
[<InlineData("US-DC-LL092606601", "US-DC-LL092606601")>]
[<InlineData("P26052", "P26052")>]
let ``a DC file number is put in the register's spelling`` (number: string, expected: string) =
    Assert.Equal(ValueSome expected, DistrictOfColumbia.normalNumber number)

[<Theory; InlineData("abc"); InlineData("CC123"); InlineData("1' OR '1'='1"); InlineData(""); InlineData(null)>]
let ``a value that can't be a DC file number isn't sent`` (number: string) : Task =
    task {
        use handler = new FakeHandler(fun _ -> failwith "Nothing should be sent")
        let registry = Sources.districtOfColumbia (connectionOver handler)

        match! registry.TryFindAsync(number, dc, ct) with
        | Error(RegistryFailure.InvalidRequest _) -> ()
        | other -> failwith $"%A{other}"

        match! registry.OwnersAsync(number, dc, OwnerPurpose.PublicRead, ct) with
        | Error(RegistryFailure.InvalidRequest _) -> Assert.Empty handler.Calls
        | other -> failwith $"%A{other}"
    }

// Every source fails closed the same way.

[<Theory; InlineData("not json"); InlineData(""); InlineData("<html>Service page</html>")>]
let ``an answer that isn't JSON is unreadable, on every US state source`` (body: string) : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok body)

        for registry, number, jurisdiction in allRegistries handler do
            match! registry.TryFindAsync(number, jurisdiction, ct) with
            | Error(RegistryFailure.UnreadableAnswer reason) -> Assert.DoesNotContain("html", reason.ToLowerInvariant())
            | other -> failwith $"%s{registry.Coverage.Registry}: %A{other}"
    }

[<Fact>]
let ``JSON of the wrong shape is unreadable, on every US state source`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok """{"unexpected":true}""")

        for registry, number, jurisdiction in allRegistries handler do
            match! registry.TryFindAsync(number, jurisdiction, ct) with
            | Error(RegistryFailure.UnreadableAnswer _) -> ()
            | other -> failwith $"%s{registry.Coverage.Registry}: %A{other}"
    }

[<Fact>]
let ``a record missing its name or number is unreadable, on every US state source`` () : Task =
    task {
        let socrataRow = """[{"status":"Active","entitystatus":"Good Standing"}]"""
        let arcgisRow = """{"features":[{"attributes":{"ENTITY_STATUS":"Revoked"}}]}"""

        for registry, number, jurisdiction in
            allRegistries (
                new FakeHandler(fun call -> ok (if call.Host.Contains "dcgis" then arcgisRow else socrataRow))
            ) do
            match! registry.TryFindAsync(number, jurisdiction, ct) with
            | Error(RegistryFailure.UnreadableAnswer _) -> ()
            | other -> failwith $"%s{registry.Coverage.Registry}: %A{other}"

        use nameOnly =
            new FakeHandler(fun _ ->
                ok """[{"entityname":"X","name":"X","current_entity_name":"X","business_name":"X"}]""")

        for registry, number, jurisdiction in allRegistries nameOnly |> List.take 5 do
            match! registry.TryFindAsync(number, jurisdiction, ct) with
            | Error(RegistryFailure.UnreadableAnswer _) -> ()
            | other -> failwith $"%s{registry.Coverage.Registry}: %A{other}"
    }

[<Fact>]
let ``several records on one number aren't told apart, on every US state source`` () : Task =
    task {
        let row (number: string) =
            $"""{{"entityid":"{number}","entityname":"X","accountnumber":"{number}","name":"X","dos_id":"{number}","current_entity_name":"X","registry_number":"{number}","filing_number":"{number}","FILE_NUMBER":"{number}","BUSINESS_NAME":"X"}}"""

        let socrata = $"""[{row "1"},{row "2"}]"""

        let arcgis =
            $"""{{"features":[{{"attributes":{row "1"}}},{{"attributes":{row "2"}}}]}}"""

        // Oregon and Pennsylvania collapse rows that name one business, so these two rows have different names there.
        let named =
            """[{"registry_number":"1","business_name":"X","filing_number":"1"},{"registry_number":"1","business_name":"Y","filing_number":"1"}]"""

        use handler =
            new FakeHandler(fun call ->
                if call.Host.Contains "dcgis" then
                    ok arcgis
                elif call.Host.Contains "oregon" || call.Host.Contains "pa.gov" then
                    ok named
                else
                    ok socrata)

        for registry, number, jurisdiction in allRegistries handler do
            let! evidence = findIn registry number jurisdiction

            match evidence.Value with
            | NotAvailable reason -> Assert.Contains("2 ", reason)
            | other -> failwith $"%s{registry.Coverage.Registry}: %A{other}"
    }

[<Theory; InlineData(500); InlineData(503)>]
let ``a server failure is Unavailable, on every US state source`` (status: int) : Task =
    task {
        use handler = new FakeHandler(fun _ -> answer status "secret-body")

        for registry, number, jurisdiction in allRegistries handler do
            match! registry.TryFindAsync(number, jurisdiction, ct) with
            | Error(RegistryFailure.Unavailable reason) -> Assert.DoesNotContain("secret-body", reason)
            | other -> failwith $"%s{registry.Coverage.Registry}: %A{other}"
    }

[<Theory; InlineData(400); InlineData(403); InlineData(404); InlineData(302)>]
let ``a refusal or redirect is RequestRefused, on every US state source`` (status: int) : Task =
    task {
        use handler = new FakeHandler(fun _ -> answer status "secret-body")

        for registry, number, jurisdiction in allRegistries handler do
            match! registry.TryFindAsync(number, jurisdiction, ct) with
            | Error(RegistryFailure.RequestRefused reason) -> Assert.DoesNotContain("secret-body", reason)
            | other -> failwith $"%s{registry.Coverage.Registry}: %A{other}"
    }

[<Fact>]
let ``a 429 is RateLimited, on every US state source`` () : Task =
    task {
        use handler =
            new FakeHandler(fun _ ->
                { answer 429 "" with
                    Headers = [ "Retry-After", "120" ]
                })

        for registry, number, jurisdiction in allRegistries handler do
            match! registry.TryFindAsync(number, jurisdiction, ct) with
            | Error(RegistryFailure.RateLimited _) -> ()
            | other -> failwith $"%s{registry.Coverage.Registry}: %A{other}"
    }

[<Fact>]
let ``outside its coverage every US state source fails, and sends nothing`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> failwith "Nothing should be sent")
        let elsewhere = Jurisdiction.ofCode "US-TX"

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
let ``every US state source is free, keyless, paced and covers its own state only`` () =
    use handler = new FakeHandler(fun _ -> failwith "Nothing should be sent")

    for registry, _, jurisdiction in allRegistries handler do
        let coverage = registry.Coverage
        Assert.Equal<Jurisdiction list>([ jurisdiction ], coverage.Jurisdictions)
        Assert.Equal(AccessCost.Free, coverage.Cost)
        Assert.Equal(RateLimitPolicy.NotPublished(2, TimeSpan.FromSeconds 1.), coverage.RateLimit)
        Assert.False(String.IsNullOrWhiteSpace coverage.Licence)

[<Fact>]
let ``a blank search term isn't sent, on every US state source`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> failwith "Nothing should be sent")

        for registry, _, jurisdiction in allRegistries handler do
            match! registry.SearchAsync(" ", jurisdiction, ct) with
            | Error(RegistryFailure.InvalidRequest _) -> ()
            | other -> failwith $"%s{registry.Coverage.Registry}: %A{other}"

        Assert.Empty handler.Calls
    }
