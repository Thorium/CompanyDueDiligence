/// The keyed sources of wave E: Australia, Korea, Croatia, Greece, Austria and Denmark. None was run live, as each needs a key or credentials;
/// the fixtures follow each source's documented shapes and examples. The key has to go where the source documents it and nowhere else.
module KeyedSourceTests2

open System
open System.IO
open System.Net.Http
open System.Threading.Tasks
open Microsoft.FSharp.Reflection
open Xunit
open CompanyDueDiligence
open Fake

let private fixture (name: string) =
    File.ReadAllText(Path.Combine(AppContext.BaseDirectory, "Fixtures", name))

let private fixedTime = DateTimeOffset(2026, 11, 6, 12, 0, 0, TimeSpan.Zero)

let private connectionOver (handler: FakeHandler) =
    SourceConnection(handler, (fun () -> fixedTime), (fun _ _ -> Task.CompletedTask))

/// Holds characters a query string escapes, so that both spellings are checked.
[<Literal>]
let private SecretKey = "secret-key+Ab/9="

[<Literal>]
let private ClientId = "client-id-value"

[<Literal>]
let private ClientSecret = "client-secret-value.."

let private key = SourceKey SecretKey
let private credentials = ClientCredentials(ClientId, ClientSecret)

let private au = Jurisdiction.ofCode "AU"
let private kr = Jurisdiction.ofCode "KR"
let private hr = Jurisdiction.ofCode "HR"
let private gr = Jurisdiction.ofCode "GR"
let private at = Jurisdiction.ofCode "AT"
let private dk = Jurisdiction.ofCode "DK"

let private tokenJson =
    """{"access_token":"the-bearer-token","token_type":"bearer","expires_in":21600}"""

/// Croatia answers the token endpoint with a token, and anything else with data.
let private croatiaAnswering (data: Call -> Answer) (call: Call) =
    if call.Target.StartsWith("/api/oauth/token", StringComparison.Ordinal) then
        ok tokenJson
    else
        data call

/// Greece answers its status codebook from the fixture, and anything else with data.
let private greeceAnswering (data: Call -> Answer) (call: Call) =
    if call.Target.EndsWith("/metadata/companyStatuses", StringComparison.Ordinal) then
        ok (fixture "gr-statuses.json")
    else
        data call

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

let private caseOf (standing: CompanyStanding) =
    (fst (FSharpValue.GetUnionFields(standing, typeof<CompanyStanding>))).Name

/// Every registry here, with a number of the right shape.
let private allRegistries (handler: FakeHandler) =
    let connection = connectionOver handler

    [
        Sources.australia connection key, "26008672179", au
        Sources.korea connection key, "123-45-67891", kr
        Sources.croatia connection credentials, "12345678903", hr
        Sources.greece connection key, "123456701000", gr
        Sources.austria connection key, "58468h", at
        Sources.denmark connection credentials, "36936126", dk
    ]

let private secrets =
    [ SecretKey; Uri.EscapeDataString SecretKey; ClientId; ClientSecret ]

let private holdsNoSecret (text: string) =
    for secret in secrets do
        Assert.DoesNotContain(secret, text)

// Australia

[<Fact>]
let ``Australia reads an ABN record inside its callback, with business names and the GUID in the query`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "au-abn-active.txt"))

        let! evidence =
            findIn (Sources.australia (connectionOver handler) key) "26 008 672 179" au

        let company = companyOf evidence
        Assert.Equal("ABN Lookup (Australian Business Register)", evidence.Source)
        Assert.Equal(fixedTime, evidence.RetrievedAt)
        Assert.Equal("EXAMPLE HOLDINGS PTY LTD", company.Name)
        Assert.Equal("26008672179", company.Number)
        Assert.Equal(CompanyStanding.Registered(ValueSome "ABN Active"), company.Standing)
        Assert.False(CompanyStanding.isActive company.Standing)
        Assert.Equal(RecordRole.NotStated, company.Role)
        Assert.Equal(ValueSome "Australian Private Company", company.LegalForm)
        Assert.Equal<string list>([ "EXAMPLE TRADING"; "EXAMPLE STORES" ], company.OtherNames)
        Assert.Equal(ValueNone, evidence.AsOf)
        Assert.StartsWith("{\"Abn\":\"26008672179\"", evidence.Raw.Value)

        match company.Addresses with
        | Available [ address ] ->
            Assert.Equal(ValueSome "VIC", address.Address.Region)
            Assert.Equal(ValueSome "3000", address.Address.PostalCode)
        | other -> failwith $"%A{other}"

        let call = handler.Calls.Head
        Assert.StartsWith("/json/AbnDetails.aspx?abn=26008672179&callback=callback&guid=", call.Target)
        Assert.EndsWith("guid=" + Uri.EscapeDataString SecretKey, call.Target)
    }

[<Fact>]
let ``Australia looks up an ACN and checks the answer is that company's`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "au-abn-active.txt"))
        let registry = Sources.australia (connectionOver handler) key
        let! evidence = findIn registry "008 672 179" au
        Assert.Equal("26008672179", (companyOf evidence).Number)
        Assert.StartsWith("/json/AcnDetails.aspx?acn=008672179&", handler.Calls.Head.Target)

        use other = new FakeHandler(fun _ -> ok (fixture "au-abn-cancelled.txt"))

        match! (Sources.australia (connectionOver other) key).TryFindAsync("008672179", au, ct) with
        | Error(RegistryFailure.UnreadableAnswer _) -> ()
        | result -> failwith $"%A{result}"
    }

[<Fact>]
let ``Australia reads a cancelled ABN as closed on its status date`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "au-abn-cancelled.txt"))

        let! evidence =
            findIn (Sources.australia (connectionOver handler) key) "74172177893" au

        let company = companyOf evidence
        Assert.Equal(CompanyStanding.Closed "ABN cancelled", company.Standing)
        Assert.Equal(ValueSome(DateOnly(2015, 6, 30)), company.CeasedOn)
    }

[<Fact>]
let ``Australia's No record found is not held, and a suppressed record is NotAvailable`` () : Task =
    task {
        use none = new FakeHandler(fun _ -> ok (fixture "au-no-record.txt"))
        let! evidence = findIn (Sources.australia (connectionOver none) key) "74172177893" au
        Assert.Equal(Available ValueNone, evidence.Value)

        use suppressed = new FakeHandler(fun _ -> ok (fixture "au-suppressed.txt"))

        let! hidden =
            findIn (Sources.australia (connectionOver suppressed) key) "74172177893" au

        match hidden.Value with
        | NotAvailable reason -> Assert.Contains("suppresses", reason)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``Australia's No record found for an ACN is NotAvailable, as many ACNs have no ABN`` () : Task =
    task {
        use none = new FakeHandler(fun _ -> ok (fixture "au-no-record.txt"))
        let! evidence = findIn (Sources.australia (connectionOver none) key) "008 672 179" au

        match evidence.Value with
        | NotAvailable reason -> Assert.Contains("ASIC", reason)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``Australia's refused GUID is AccessRefused, and the GUID is in no failure or raw answer`` () : Task =
    task {
        let refused =
            """callback({"Abn":"","AbnStatus":"","Message":"The GUID entered is not recognised as a Registered Party"})"""

        use handler = new FakeHandler(fun _ -> ok refused)

        match! (Sources.australia (connectionOver handler) key).TryFindAsync("74172177893", au, ct) with
        | Error(RegistryFailure.AccessRefused reason) -> holdsNoSecret reason
        | other -> failwith $"%A{other}"

        let echoed =
            (fixture "au-abn-cancelled.txt").Replace("\"Message\":\"\"", $"\"Message\":\"\",\"Echo\":\"{SecretKey}\"")

        use echoing = new FakeHandler(fun _ -> ok echoed)

        let! evidence =
            findIn (Sources.australia (connectionOver echoing) key) "74172177893" au

        holdsNoSecret evidence.Raw.Value

        use other =
            new FakeHandler(fun _ -> ok """callback({"Message":"Something else went wrong"})""")

        match! (Sources.australia (connectionOver other) key).TryFindAsync("74172177893", au, ct) with
        | Error(RegistryFailure.RequestRefused _) -> ()
        | result -> failwith $"%A{result}"
    }

[<Theory>]
[<InlineData("Active", "Registered")>]
[<InlineData("Cancelled", "Closed")>]
[<InlineData("Suspended", "Unknown")>]
[<InlineData("", "Unknown")>]
let ``every ABN status has a standing, and a new one is unknown`` (word: string, expected: string) =
    Assert.Equal(expected, caseOf (Australia.standing (ValueSome word)))

[<Theory; InlineData("74172177893", true); InlineData("74172177894", false); InlineData("26008672179", true)>]
let ``an ABN passes its check digit or fails it`` (abn: string, valid: bool) =
    Assert.Equal(valid, Australia.isAbn abn)

[<Theory; InlineData("008672179", true); InlineData("008672178", false)>]
let ``an ACN passes its check digit or fails it`` (acn: string, valid: bool) =
    Assert.Equal(valid, Australia.isAcn acn)

[<Fact>]
let ``Australia's name search reads the documented Names list`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "au-names.txt"))

        match! (Sources.australia (connectionOver handler) key).SearchAsync("example", au, ct) with
        | Ok { Value = Available found } ->
            Assert.Equal(2, found.Matches.Length)
            Assert.Equal("26008672179", found.Matches.Head.Number)
            Assert.False found.Complete

            Assert.StartsWith(
                "/json/MatchingNames.aspx?name=example&maxResults=20&callback=callback&guid=",
                handler.Calls.Head.Target
            )
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``an answer outside the callback is unreadable`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok """{"Abn":"74172177893","Message":""}""")

        match! (Sources.australia (connectionOver handler) key).TryFindAsync("74172177893", au, ct) with
        | Error(RegistryFailure.UnreadableAnswer _) -> ()
        | other -> failwith $"%A{other}"
    }

// Korea

[<Fact>]
let ``Korea posts the number and reads a continuing business, with no name published`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "kr-status.json"))
        let! evidence = findIn (Sources.korea (connectionOver handler) key) "123-45-67891" kr
        let company = companyOf evidence
        Assert.Equal(PartyNames.NotPublished, company.Name)
        Assert.Equal("1234567891", company.Number)
        Assert.Equal(CompanyStanding.Registered(ValueSome "계속사업자 (tax registration)"), company.Standing)
        Assert.False(CompanyStanding.isActive company.Standing)
        Assert.Equal(ValueSome "계속사업자", company.StatusAsPublished)
        Assert.Equal(RecordRole.NotStated, company.Role)
        Assert.StartsWith("{\"b_no\":\"1234567891\"", evidence.Raw.Value)

        match company.Addresses with
        | NotAvailable _ -> ()
        | other -> failwith $"%A{other}"

        let call = handler.Calls.Head

        Assert.Equal(
            "/api/nts-businessman/v1/status?serviceKey="
            + Uri.EscapeDataString SecretKey
            + "&returnType=JSON",
            call.Target
        )
    }

[<Fact>]
let ``Korea reads a closed business with its closing date, and an unregistered number as not held`` () : Task =
    task {
        use closed = new FakeHandler(fun _ -> ok (fixture "kr-closed.json"))
        let! evidence = findIn (Sources.korea (connectionOver closed) key) "1234567891" kr
        let company = companyOf evidence
        Assert.Equal(CompanyStanding.Closed "폐업자: business closed", company.Standing)
        Assert.Equal(ValueSome(DateOnly(2024, 1, 31)), company.CeasedOn)

        use unregistered = new FakeHandler(fun _ -> ok (fixture "kr-unregistered.json"))
        let! none = findIn (Sources.korea (connectionOver unregistered) key) "1234567891" kr
        Assert.Equal(Available ValueNone, none.Value)
    }

[<Fact>]
let ``a Korean answer for another number, or not OK, is unreadable`` () : Task =
    task {
        use other =
            new FakeHandler(fun _ -> ok ((fixture "kr-status.json").Replace("1234567891", "1111111111")))

        match! (Sources.korea (connectionOver other) key).TryFindAsync("1234567891", kr, ct) with
        | Error(RegistryFailure.UnreadableAnswer _) -> ()
        | result -> failwith $"%A{result}"

        use failed = new FakeHandler(fun _ -> ok """{"status_code":"BAD_JSON_REQUEST"}""")

        match! (Sources.korea (connectionOver failed) key).TryFindAsync("1234567891", kr, ct) with
        | Error(RegistryFailure.UnreadableAnswer _) -> ()
        | result -> failwith $"%A{result}"
    }

[<Theory>]
[<InlineData("01", "Registered")>]
[<InlineData("02", "Unknown")>]
[<InlineData("03", "Closed")>]
[<InlineData("04", "Unknown")>]
[<InlineData("", "Unknown")>]
let ``every NTS status code has a standing, and a new one is unknown`` (code: string, expected: string) =
    Assert.Equal(expected, caseOf (Korea.standing (ValueSome code) ValueNone))

[<Theory>]
[<InlineData("1248100998", true)>]
[<InlineData("1248100997", false)>]
[<InlineData("1234567891", true)>]
[<InlineData("1234567890", false)>]
let ``a Korean business registration number passes its check digit or fails it`` (number: string, valid: bool) =
    Assert.Equal(valid, Korea.hasCheckDigit number)
    Assert.Equal(valid, (Korea.normalNumber number).IsSome)

// Croatia

[<Fact>]
let ``Croatia reads a subject by OIB with a bearer token from the client-credentials grant`` () : Task =
    task {
        use handler =
            new FakeHandler(croatiaAnswering (fun _ -> ok (fixture "hr-subject.json")))

        let registry = Sources.croatia (connectionOver handler) credentials
        let! evidence = findIn registry "12345678903" hr
        let company = companyOf evidence
        Assert.Equal("Sudski registar open data (Croatia)", evidence.Source)
        Assert.Equal("PRIMJER d.d. za trgovinu", company.Name)
        Assert.Equal<string list>([ "PRIMJER d.d." ], company.OtherNames)
        Assert.Equal("080000014", company.Number)
        Assert.Equal(CompanyStanding.Active, company.Standing)
        Assert.Equal(RecordRole.Home, company.Role)
        Assert.Equal(ValueSome "dioničko društvo", company.LegalForm)
        Assert.Equal(ValueSome(DateOnly(1995, 6, 20)), company.IncorporatedOn)
        Assert.StartsWith("{\"mbs\":80000014", evidence.Raw.Value)

        match company.Addresses with
        | Available [ address ] ->
            Assert.Equal<string list>([ "Ilica 1 A" ], address.Address.Lines)
            Assert.Equal(ValueSome "Zagreb", address.Address.Locality)
            Assert.Equal(ValueSome "10000", address.Address.PostalCode)
            Assert.Equal(AddressKind.RegisteredOffice, address.Kind)
        | other -> failwith $"%A{other}"

        match handler.Calls with
        | [ tokenCall; dataCall ] ->
            Assert.Equal("/api/oauth/token", tokenCall.Target)

            let basic =
                Convert.ToBase64String(Text.Encoding.UTF8.GetBytes(ClientId + ":" + Uri.EscapeDataString ClientSecret))

            Assert.Equal("Basic " + basic, tokenCall.Headers["Authorization"])

            Assert.Equal(
                "/api/javni/detalji_subjekta?tip_identifikatora=oib&identifikator=12345678903&expand_relations=1&no_data_error=0",
                dataCall.Target
            )

            Assert.Equal("Bearer the-bearer-token", dataCall.Headers["Authorization"])
        | calls -> failwith $"%A{calls}"

        let! _ = findIn registry "080000014" hr
        Assert.Equal(3, handler.Calls.Length)
        Assert.Contains("tip_identifikatora=mbs&identifikator=080000014", handler.Calls[2].Target)
    }

[<Fact>]
let ``Croatia's empty object is not held, and an MBS is sent with its leading zeros`` () : Task =
    task {
        use handler = new FakeHandler(croatiaAnswering (fun _ -> ok "{}"))

        let! evidence =
            findIn (Sources.croatia (connectionOver handler) credentials) "80000014" hr

        Assert.Equal(Available ValueNone, evidence.Value)
        Assert.Contains("identifikator=080000014", handler.Calls[1].Target)
    }

[<Fact>]
let ``a Croatian branch of a foreign company names its home register`` () : Task =
    task {
        use handler =
            new FakeHandler(croatiaAnswering (fun _ -> ok (fixture "hr-foreign-branch.json")))

        let! evidence =
            findIn (Sources.croatia (connectionOver handler) credentials) "98765432106" hr

        let company = companyOf evidence
        Assert.Equal(RecordRole.ForeignRegistration, company.Role)
        let home = company.HomeRegistration.Value
        Assert.Equal(ValueSome "HRB 123456", home.RegistrationNumber)
        Assert.Equal(ValueSome "Handelsregister", home.Register)
        Assert.Equal(ValueSome "DE", home.Country)
    }

[<Fact>]
let ``a Croatian proceeding whose kind isn't given leaves the standing unknown, not active`` () : Task =
    task {
        let unnamed =
            (fixture "hr-subject.json")
                .Replace(
                    "\"tvrtka\":",
                    "\"postupak\":{\"postupak\":{},\"datum_stecaja\":\"2026-01-15T00:00:00\"},\"tvrtka\":"
                )

        use handler = new FakeHandler(croatiaAnswering (fun _ -> ok unnamed))

        let! evidence =
            findIn (Sources.croatia (connectionOver handler) credentials) "12345678903" hr

        let company = companyOf evidence
        Assert.Equal(CompanyStanding.Unknown(ValueSome "Proceeding of a kind not given"), company.Standing)

        let coded =
            (fixture "hr-subject.json")
                .Replace(
                    "\"tvrtka\":",
                    "\"postupak\":{\"postupak_povijest\":[{\"status\":1,\"prbu_od\":1,\"upis_id_od\":1,\"postupak\":3}]},\"tvrtka\":"
                )

        use codedHandler = new FakeHandler(croatiaAnswering (fun _ -> ok coded))

        let! codedEvidence =
            findIn (Sources.croatia (connectionOver codedHandler) credentials) "12345678903" hr

        Assert.Equal(CompanyStanding.Unknown(ValueSome "Proceeding code 3"), (companyOf codedEvidence).Standing)
    }

[<Fact>]
let ``Croatia asks for a new token once when the register refuses the held one`` () : Task =
    task {
        let tokens =
            Collections.Concurrent.ConcurrentQueue<string>([ "first-token"; "second-token" ])

        use handler =
            new FakeHandler(fun call ->
                if call.Target.StartsWith("/api/oauth/token", StringComparison.Ordinal) then
                    match tokens.TryDequeue() with
                    | true, token -> ok $"""{{"access_token":"{token}","token_type":"bearer","expires_in":21600}}"""
                    | _ -> answer 401 "no more tokens"
                elif call.Headers["Authorization"] = "Bearer second-token" then
                    ok (fixture "hr-subject.json")
                else
                    answer 401 "token revoked")

        let registry = Sources.croatia (connectionOver handler) credentials
        let! evidence = findIn registry "12345678903" hr
        Assert.Equal("080000014", (companyOf evidence).Number)

        Assert.Equal<string list>(
            [
                "/api/oauth/token"
                "/api/javni/detalji_subjekta"
                "/api/oauth/token"
                "/api/javni/detalji_subjekta"
            ],
            handler.Calls |> List.map (fun call -> call.Target.Split('?')[0])
        )

        use refusing =
            new FakeHandler(fun call ->
                if call.Target.StartsWith("/api/oauth/token", StringComparison.Ordinal) then
                    ok tokenJson
                else
                    answer 401 "token revoked")

        match! (Sources.croatia (connectionOver refusing) credentials).TryFindAsync("12345678903", hr, ct) with
        | Error(RegistryFailure.AccessRefused _) -> Assert.Equal(4, refusing.Calls.Length)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a deleted Croatian subject is closed with its date, and a subject of another OIB is unreadable`` () : Task =
    task {
        let deleted =
            (fixture "hr-subject.json")
                .Replace("\"datum_osnivanja\"", "\"datum_brisanja\":\"2020-02-03T00:00:00\",\"datum_osnivanja\"")

        use handler = new FakeHandler(croatiaAnswering (fun _ -> ok deleted))

        let! evidence =
            findIn (Sources.croatia (connectionOver handler) credentials) "12345678903" hr

        let company = companyOf evidence
        Assert.Equal(CompanyStanding.Closed "Deleted from the register (brisan)", company.Standing)
        Assert.Equal(ValueSome(DateOnly(2020, 2, 3)), company.CeasedOn)

        use other =
            new FakeHandler(croatiaAnswering (fun _ -> ok (fixture "hr-subject.json")))

        match! (Sources.croatia (connectionOver other) credentials).TryFindAsync("98765432106", hr, ct) with
        | Error(RegistryFailure.UnreadableAnswer _) -> ()
        | result -> failwith $"%A{result}"
    }

[<Fact>]
let ``a Croatian subject in bankruptcy states it as a concern`` () : Task =
    task {
        let bankrupt =
            (fixture "hr-subject.json")
                .Replace(
                    "\"tvrtka\":",
                    "\"postupak\":{\"postupak\":{\"znacenje\":\"stečaj\"},\"datum_stecaja\":\"2026-01-15T00:00:00\"},\"tvrtka\":"
                )

        use handler = new FakeHandler(croatiaAnswering (fun _ -> ok bankrupt))

        let! evidence =
            findIn (Sources.croatia (connectionOver handler) credentials) "12345678903" hr

        let company = companyOf evidence
        Assert.Equal(CompanyStanding.InInsolvency "stečaj", company.Standing)
        Assert.Equal<CompanyConcern list>([ CompanyConcern.InsolvencyHistory ], company.Concerns.Stated)
    }

[<Fact>]
let ``Croatia reads the current entry when the answer gives a history`` () : Task =
    task {
        let historic =
            (fixture "hr-subject.json")
                .Replace(
                    "\"tvrtka\":{\"ime\":\"PRIMJER d.d. za trgovinu\",\"naznaka_imena\":\"PRIMJER\"}",
                    "\"tvrtka\":{\"tvrtka_povijest_ex\":[{\"status\":0,\"ime\":\"STARO IME d.d.\"},{\"status\":1,\"ime\":\"PRIMJER d.d. za trgovinu\"}]}"
                )

        use handler = new FakeHandler(croatiaAnswering (fun _ -> ok historic))

        let! evidence =
            findIn (Sources.croatia (connectionOver handler) credentials) "12345678903" hr

        Assert.Equal("PRIMJER d.d. za trgovinu", (companyOf evidence).Name)
    }

[<Theory>]
[<InlineData(false, 1, false, false, "", "Active")>]
[<InlineData(true, 1, false, false, "", "Closed")>]
[<InlineData(false, 5, false, false, "", "Closed")>]
[<InlineData(false, 0, false, false, "", "Unknown")>]
[<InlineData(false, 8, false, false, "", "Unknown")>]
[<InlineData(false, 1, true, false, "", "InInsolvency")>]
[<InlineData(false, 1, false, true, "", "WindingUp")>]
[<InlineData(false, 1, false, false, "stečaj", "InInsolvency")>]
[<InlineData(false, 1, false, false, "likvidacija", "WindingUp")>]
[<InlineData(false, 1, false, false, "a proceeding added later", "Unknown")>]
let ``every Croatian status and flag has a standing, and an unknown proceeding is unknown``
    (deleted: bool, status: int, bankrupt: bool, liquidating: bool, proceeding: string, expected: string)
    =
    let proceeding = if proceeding = "" then ValueNone else ValueSome proceeding
    Assert.Equal(expected, caseOf (Croatia.standing deleted (ValueSome status) bankrupt liquidating proceeding))

[<Theory; InlineData("12345678903", true); InlineData("12345678904", false); InlineData("98765432106", true)>]
let ``an OIB passes its ISO 7064 check digit or fails it`` (oib: string, valid: bool) =
    Assert.Equal(valid, Croatia.isOib oib)

[<Fact>]
let ``Croatia's refused credentials are AccessRefused, and the secret is in no failure`` () : Task =
    task {
        use handler =
            new FakeHandler(fun _ -> answer 401 $"{{\"error\":\"{ClientSecret}\"}}")

        match! (Sources.croatia (connectionOver handler) credentials).TryFindAsync("12345678903", hr, ct) with
        | Error(RegistryFailure.AccessRefused reason) -> holdsNoSecret reason
        | other -> failwith $"%A{other}"
    }

// Greece

[<Fact>]
let ``Greece reads a company by GEMI number, its status by the codebook and the key in api_key`` () : Task =
    task {
        use handler =
            new FakeHandler(greeceAnswering (fun _ -> ok (fixture "gr-company.json")))

        let registry = Sources.greece (connectionOver handler) key
        let! evidence = findIn registry "123456701000" gr
        let company = companyOf evidence
        Assert.Equal("GEMI open data (Greece)", evidence.Source)
        Assert.Equal("ΠΑΡΑΔΕΙΓΜΑ ΑΝΩΝΥΜΗ ΕΤΑΙΡΕΙΑ", company.Name)
        Assert.Equal("123456701000", company.Number)
        Assert.Equal(CompanyStanding.Active, company.Standing)
        Assert.Equal(ValueSome "Ενεργή", company.StatusAsPublished)
        Assert.Equal(RecordRole.Home, company.Role)
        Assert.Equal(ValueSome "ΑΕ", company.LegalForm)
        Assert.Equal(ValueSome(DateOnly(2005, 3, 14)), company.IncorporatedOn)
        Assert.Contains("PARADEIGMA SOCIETE ANONYME", company.OtherNames)
        Assert.StartsWith("{\"arGemi\":123456701000", evidence.Raw.Value)

        match company.Addresses with
        | Available [ address ] ->
            Assert.Equal<string list>([ "ΠΑΝΕΠΙΣΤΗΜΙΟΥ 10" ], address.Address.Lines)
            Assert.Equal(ValueSome "10671", address.Address.PostalCode)
            Assert.Equal(ValueSome "ΑΤΤΙΚΗΣ", address.Address.Region)
        | other -> failwith $"%A{other}"

        match handler.Calls with
        | [ codebook; record ] ->
            Assert.Equal("/api/opendata/v1/metadata/companyStatuses", codebook.Target)
            Assert.Equal("/api/opendata/v1/companies/123456701000", record.Target)
            Assert.Equal(SecretKey, record.Headers["api_key"])
        | calls -> failwith $"%A{calls}"

        let! _ = findIn registry "000123456701000" gr
        Assert.Equal(3, handler.Calls.Length)
    }

[<Fact>]
let ``Greece's 404 on a GEMI number is not held, and an AFM no company carries is NotAvailable`` () : Task =
    task {
        use missing =
            new FakeHandler(
                greeceAnswering (fun _ -> answer 404 """[{"code":"404","message":"Δεν βρέθηκε εταιρεία"}]""")
            )

        let! evidence = findIn (Sources.greece (connectionOver missing) key) "999" gr
        Assert.Equal(Available ValueNone, evidence.Value)

        for body in [ ""; "<html>Not Found</html>"; """{"message":"no route"}"""; "[]" ] do
            use unmarked = new FakeHandler(greeceAnswering (fun _ -> answer 404 body))

            match! (Sources.greece (connectionOver unmarked) key).TryFindAsync("999", gr, ct) with
            | Error(RegistryFailure.RequestRefused _) -> ()
            | other -> failwith $"%A{other}"

            match! (Sources.greece (connectionOver unmarked) key).SearchAsync("example", gr, ct) with
            | Error(RegistryFailure.RequestRefused _) -> ()
            | other -> failwith $"%A{other}"

        use empty =
            new FakeHandler(greeceAnswering (fun _ -> ok """{"searchMetadata":{"totalCount":0},"searchResults":[]}"""))

        let! none = findIn (Sources.greece (connectionOver empty) key) "EL094000000" gr

        match none.Value with
        | NotAvailable reason -> Assert.Contains("not every holder", reason)
        | other -> failwith $"%A{other}"

        Assert.Equal("/api/opendata/v1/companies?afm=094000000&resultsSize=2", empty.Calls[1].Target)
    }

[<Fact>]
let ``Greece finds by AFM, and a status the codebook doesn't flag active is unknown`` () : Task =
    task {
        use handler =
            new FakeHandler(greeceAnswering (fun _ -> ok (fixture "gr-afm-search.json")))

        let! evidence =
            findIn (Sources.greece (connectionOver handler) key) "AFM 094000000" gr

        let company = companyOf evidence
        Assert.Equal("123456701000", company.Number)
        Assert.Equal(CompanyStanding.Unknown(ValueSome "Διαγραμμένη"), company.Standing)
    }

[<Fact>]
let ``Greece reads persons as officers, ended ones as not current`` () : Task =
    task {
        use handler =
            new FakeHandler(greeceAnswering (fun _ -> ok (fixture "gr-company.json")))

        match! (Sources.greece (connectionOver handler) key).OfficersAsync("123456701000", gr, ct) with
        | Ok { Value = Available officers } ->
            Assert.Equal(3, officers.Length)
            let chair = officers |> List.find (fun officer -> officer.Name = "PERSON ONE")
            Assert.True chair.Current
            Assert.Equal(OfficerRole.Other "Πρόεδρος Δ.Σ.", chair.Role)
            Assert.Equal(PartyKind.Individual, chair.Kind)
            let former = officers |> List.find (fun officer -> officer.Name = "PERSON TWO")
            Assert.False former.Current
            Assert.Equal(ValueSome(DateOnly(2020, 6, 30)), former.EndedOn)

            let firm =
                officers |> List.find (fun officer -> officer.Kind = PartyKind.LegalEntity)

            Assert.Equal("ΜΕΤΟΧΟΣ ΑΕ", firm.Name)
        | other -> failwith $"%A{other}"

        Assert.Equal(1, handler.Calls.Length)
    }

[<Theory; InlineData(1, "Active"); InlineData(3, "Unknown"); InlineData(42, "Unknown")>]
let ``a GEMI status is active only when the codebook flags it, and one not in it is unknown``
    (id: int, expected: string)
    =
    let codebook = Map.ofList [ 1, true; 3, false ]
    Assert.Equal(expected, caseOf (Greece.standing codebook (ValueSome id) (ValueSome "word")))

[<Theory>]
[<InlineData("Υπό εκκαθάριση", "WindingUp")>]
[<InlineData("ΥΠΟ ΕΚΚΑΘΑΡΙΣΗ", "WindingUp")>]
[<InlineData("Under liquidation", "WindingUp")>]
[<InlineData("Σε πτώχευση", "InInsolvency")>]
[<InlineData("Ενεργή", "Active")>]
let ``a GEMI word of liquidation or bankruptcy decides, even where the codebook flags it active``
    (word: string, expected: string)
    =
    let codebook = Map.ofList [ 7, true ]
    Assert.Equal(expected, caseOf (Greece.standing codebook (ValueSome 7) (ValueSome word)))

[<Theory>]
[<InlineData("123456701000", "Gemi")>]
[<InlineData("EL094000000", "Afm")>]
[<InlineData("afm 094000000", "Afm")>]
let ``a Greek number is a GEMI number, or an AFM with its prefix`` (number: string, expected: string) =
    match Greece.lookupOf number with
    | Ok lookup -> Assert.StartsWith(expected, $"%A{lookup}")
    | Error reason -> failwith reason

[<Theory; InlineData("094000000", true); InlineData("094000001", false); InlineData("123456701", false)>]
let ``nine bare digits passing the AFM check are refused, as they could be a GEMI number``
    (number: string, refused: bool)
    : Task =
    task {
        Assert.Equal(refused, Greece.isAfm number)

        use handler =
            new FakeHandler(greeceAnswering (fun _ -> ok (fixture "gr-company.json")))

        match! (Sources.greece (connectionOver handler) key).TryFindAsync(number, gr, ct) with
        | Error(RegistryFailure.InvalidRequest reason) when refused ->
            Assert.Contains("EL", reason)
            Assert.Empty handler.Calls
        | Ok _
        | Error(RegistryFailure.UnreadableAnswer _) when not refused -> Assert.NotEmpty handler.Calls
        | other -> failwith $"%A{other}"
    }

// Austria

[<Fact>]
let ``Austria reads the HVD extract over SOAP, with the key in X-API-KEY`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "at-auszug.xml"))
        let! evidence = findIn (Sources.austria (connectionOver handler) key) "058468 H" at
        let company = companyOf evidence
        Assert.Equal("Firmenbuch HVD web service (Austria)", evidence.Source)
        Assert.Equal("EDV-Technik Dipl.-Ing. Went Gesellschaft m.b.H.", company.Name)
        Assert.Equal("58468h", company.Number)
        Assert.Equal(CompanyStanding.Registered ValueNone, company.Standing)
        Assert.Equal(RecordRole.Home, company.Role)
        Assert.Equal(ValueSome "Gesellschaft mit beschränkter Haftung", company.LegalForm)
        Assert.Equal(ValueSome(DateOnly(2026, 11, 6)), evidence.AsOf)
        Assert.StartsWith("<ns6:AUSZUG_V2_RESPONSE", evidence.Raw.Value)
        Assert.DoesNotContain(CompanyConcern.RegisteredOfficeUndeliverable, company.Concerns.Unconfirmed)
        Assert.Empty company.Concerns.Stated

        match company.Addresses with
        | Available [ address ] ->
            Assert.Equal<string list>([ "Kärntner Straße 337" ], address.Address.Lines)
            Assert.Equal(ValueSome "Graz", address.Address.Locality)
            Assert.Equal(ValueSome "8054", address.Address.PostalCode)
        | other -> failwith $"%A{other}"

        let call = handler.Calls.Head
        Assert.Equal("/jop/api/at.gv.justiz.fbw/ws", call.Target)
        Assert.Equal(SecretKey, call.Headers["X-API-KEY"])
    }

[<Fact>]
let ``Austria asks for the Kurzinformation extract as of today`` () =
    let envelope = Austria.auszugRequest "58468h" (DateOnly(2026, 11, 6))
    Assert.Contains("<aus:FNR>58468h</aus:FNR>", envelope)
    Assert.Contains("<aus:STICHTAG>2026-11-06</aus:STICHTAG>", envelope)
    Assert.Contains("<aus:UMFANG>Kurzinformation</aus:UMFANG>", envelope)

[<Fact>]
let ``an Austrian company deleted from the register is closed, and its undeliverable address is stated`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "at-deleted.xml"))
        let! evidence = findIn (Sources.austria (connectionOver handler) key) "145733p" at
        let company = companyOf evidence
        Assert.Equal(CompanyStanding.Closed "gelöscht", company.Standing)
        Assert.Equal(ValueSome "gelöscht", company.StatusAsPublished)
        Assert.Equal("\"A & S\" Mayer OEG", company.Name)
        Assert.Equal<CompanyConcern list>([ CompanyConcern.RegisteredOfficeUndeliverable ], company.Concerns.Stated)
    }

[<Fact>]
let ``an Austrian SOAP fault is refused, and an extract of another number is unreadable`` () : Task =
    task {
        use faulting = new FakeHandler(fun _ -> ok (fixture "at-fault.xml"))

        match! (Sources.austria (connectionOver faulting) key).TryFindAsync("58468h", at, ct) with
        | Error(RegistryFailure.RequestRefused reason) -> Assert.DoesNotContain("Stichtag", reason)
        | other -> failwith $"%A{other}"

        use other = new FakeHandler(fun _ -> ok (fixture "at-auszug.xml"))

        match! (Sources.austria (connectionOver other) key).TryFindAsync("1a", at, ct) with
        | Error(RegistryFailure.UnreadableAnswer _) -> ()
        | result -> failwith $"%A{result}"
    }

[<Fact>]
let ``Austria's search reads hits with their status words`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "at-search.xml"))

        match! (Sources.austria (connectionOver handler) key).SearchAsync("mayer & söhne", at, ct) with
        | Ok { Value = Available found } ->
            Assert.Equal(2, found.Matches.Length)
            Assert.Equal(ValueSome "gelöscht", found.Matches.Head.StatusAsPublished)
            Assert.Equal("Mayer Handels Gesellschaft m.b.H.", found.Matches[1].Name)
            Assert.False found.Complete
        | other -> failwith $"%A{other}"

        Assert.Contains("mayer &amp; söhne", Austria.searchRequest "mayer & söhne")
    }

[<Theory>]
[<InlineData("", "Registered")>]
[<InlineData("gelöscht", "Closed")>]
[<InlineData("historisch", "Unknown")>]
[<InlineData("a word added later", "Unknown")>]
let ``every Firmenbuch search status has a standing, and a new one is unknown`` (word: string, expected: string) =
    Assert.Equal(expected, caseOf (Austria.searchStanding (if word = "" then ValueNone else ValueSome word)))

[<Fact>]
let ``a Firmenbuch extract is registered unless deleted`` () =
    Assert.Equal(CompanyStanding.Registered ValueNone, Austria.standing false)
    Assert.Equal(CompanyStanding.Closed "gelöscht", Austria.standing true)

[<Theory; InlineData("123456a", "123456a"); InlineData("000187A", "187a"); InlineData(" 58468 h ", "58468h")>]
let ``a Firmenbuchnummer is put in one spelling`` (number: string, expected: string) =
    Assert.Equal(ValueSome expected, Austria.normalNumber number)

// Denmark

[<Fact>]
let ``Denmark reads a CVR record with HTTP Basic credentials, as the guide's example shows`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "dk-company.json"))

        let! evidence =
            findIn (Sources.denmark (connectionOver handler) credentials) "DK 36936126" dk

        let company = companyOf evidence
        Assert.Equal("CVR system-to-system (Denmark)", evidence.Source)
        Assert.Equal("Nøddebazaren IVS", company.Name)
        Assert.Equal("36936126", company.Number)
        Assert.Equal(CompanyStanding.Active, company.Standing)
        Assert.Equal(ValueSome "NORMAL", company.StatusAsPublished)
        Assert.Equal(ValueSome(DateOnly(2015, 7, 1)), company.IncorporatedOn)
        Assert.Equal(RecordRole.NotStated, company.Role)
        Assert.StartsWith("{\"cvrNummer\":36936126", evidence.Raw.Value)

        match company.Addresses with
        | Available [ address ] ->
            Assert.Equal<string list>([ "c/o Greencubator"; "Nørrebrogade 20 1." ], address.Address.Lines)
            Assert.Equal(ValueSome "København N", address.Address.Locality)
            Assert.Equal(ValueSome "2200", address.Address.PostalCode)
            Assert.Equal(ValueSome "DK", address.Address.Country)
        | other -> failwith $"%A{other}"

        let call = handler.Calls.Head

        Assert.Equal(
            "http://distribution.virk.dk/cvr-permanent/virksomhed/_search?allow_partial_search_results=false",
            call.Uri
        )

        let basic =
            Convert.ToBase64String(Text.Encoding.UTF8.GetBytes($"{ClientId}:{ClientSecret}"))

        Assert.Equal("Basic " + basic, call.Headers["Authorization"])
    }

[<Fact>]
let ``Denmark's query is the guide's term query, and a search term stays a string`` () =
    let query = Denmark.lookupQuery "36936126"
    Assert.Contains("\"term\":{\"Vrvirksomhed.cvrNummer\":\"36936126\"}", query)
    let search = Denmark.searchQuery "a\" OR *" 20
    Assert.Contains("\"query\":\"a\\u0022 OR *\"", search)
    Assert.Contains("\"operator\":\"and\"", search)

[<Fact>]
let ``Denmark's no hits is not held, as the guide's example shows, and two hits aren't told apart`` () : Task =
    task {
        use none = new FakeHandler(fun _ -> ok (fixture "dk-none.json"))

        let! evidence =
            findIn (Sources.denmark (connectionOver none) credentials) "36936126" dk

        Assert.Equal(Available ValueNone, evidence.Value)

        let hit =
            let body = fixture "dk-company.json"
            let start = body.IndexOf "{\"_index\""
            let finish = body.LastIndexOf "]}}"
            body.Substring(start, finish - start)

        use two =
            new FakeHandler(fun _ -> ok $"{{\"hits\":{{\"total\":2,\"hits\":[{hit},{hit}]}}}}")

        let! both = findIn (Sources.denmark (connectionOver two) credentials) "36936126" dk

        match both.Value with
        | NotAvailable _ -> ()
        | other -> failwith $"%A{other}"
    }

[<Theory>]
[<InlineData("\"timed_out\":true,\"_shards\":{\"total\":3,\"successful\":3,\"failed\":0}")>]
[<InlineData("\"timed_out\":false,\"_shards\":{\"total\":3,\"successful\":2,\"failed\":1}")>]
[<InlineData("\"timed_out\":false,\"_shards\":{\"total\":3,\"successful\":2,\"failed\":0}")>]
let ``a CVR answer that may be partial is Unavailable, not not held or a complete search`` (status: string) : Task =
    task {
        let partial =
            $"{{\"took\":1,{status},\"hits\":{{\"total\":0,\"max_score\":null,\"hits\":[]}}}}"

        use handler = new FakeHandler(fun _ -> ok partial)
        let registry = Sources.denmark (connectionOver handler) credentials

        match! registry.TryFindAsync("36936126", dk, ct) with
        | Error(RegistryFailure.Unavailable _) -> ()
        | other -> failwith $"%A{other}"

        match! registry.SearchAsync("parken sport", dk, ct) with
        | Error(RegistryFailure.Unavailable _) -> ()
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``Denmark's search reads the guide's example answer`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "dk-search.json"))

        match! (Sources.denmark (connectionOver handler) credentials).SearchAsync("parken sport", dk, ct) with
        | Ok { Value = Available found } ->
            Assert.Equal("15107707", found.Matches.Head.Number)
            Assert.Equal("PARKEN SPORT & ENTERTAINMENT A/S", found.Matches.Head.Name)
            Assert.True found.Complete
        | other -> failwith $"%A{other}"
    }

[<Theory>]
[<InlineData("NORMAL", "open", "Active")>]
[<InlineData("normal", "open", "Active")>]
[<InlineData("UNDER KONKURS", "open", "InInsolvency")>]
[<InlineData("UNDER KONKURS", "ended", "InInsolvency")>]
[<InlineData("NORMAL", "ended", "Closed")>]
[<InlineData("OPLØST EFTER KONKURS", "ended", "Closed")>]
[<InlineData("", "ended", "Closed")>]
[<InlineData("A word added later", "open", "Unknown")>]
[<InlineData("NORMAL", "none", "Unknown")>]
[<InlineData("", "open", "Unknown")>]
let ``every CVR status the guide names has a standing, with the life cycle, and any other is unknown``
    (word: string, life: string, expected: string)
    =
    let lifeOpen =
        match life with
        | "open" -> ValueSome true
        | "ended" -> ValueSome false
        | _ -> ValueNone

    Assert.Equal(expected, caseOf (Denmark.standing (if word = "" then ValueNone else ValueSome word) lifeOpen))

// Every source

[<Theory>]
[<InlineData("")>]
[<InlineData("x-1")>]
[<InlineData("not a number")>]
[<InlineData(null)>]
let ``a value that can't be a number isn't sent, on every source`` (number: string) : Task =
    task {
        use handler = new FakeHandler(fun _ -> failwith "Nothing should be sent")

        for registry, _, jurisdiction in allRegistries handler do
            match! registry.TryFindAsync(number, jurisdiction, ct) with
            | Error(RegistryFailure.InvalidRequest _) -> ()
            | other -> failwith $"%s{registry.Coverage.Registry}: %A{other}"

        Assert.Empty handler.Calls
    }

[<Theory>]
[<InlineData("74172177894", "AU")>]
[<InlineData("008672178", "AU")>]
[<InlineData("12345678904", "HR")>]
[<InlineData("1234567890", "HR")>]
[<InlineData("12345", "KR")>]
[<InlineData("123-45-67890", "KR")>]
[<InlineData("094000000X", "GR")>]
[<InlineData("1234567", "AT")>]
[<InlineData("1234567a", "AT")>]
[<InlineData("1234567", "DK")>]
let ``a number failing its source's shape or check digit isn't sent`` (number: string, country: string) : Task =
    task {
        use handler = new FakeHandler(fun _ -> failwith "Nothing should be sent")
        let jurisdiction = Jurisdiction.ofCode country

        let registry, _, _ =
            allRegistries handler |> List.find (fun (_, _, scope) -> scope = jurisdiction)

        match! registry.TryFindAsync(number, jurisdiction, ct) with
        | Error(RegistryFailure.InvalidRequest _) -> Assert.Empty handler.Calls
        | other -> failwith $"%A{other}"
    }

[<Theory; InlineData("not json"); InlineData(""); InlineData("<html>Service page</html>")>]
let ``an answer that can't be read is unreadable, on every source`` (body: string) : Task =
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
let ``a record missing its name or number is unreadable, on every source`` () : Task =
    task {
        let cases: (ICompanyRegistry * string * Jurisdiction) list =
            let over (respond: Call -> Answer) =
                connectionOver (new FakeHandler(respond))

            [
                Sources.australia
                    (over (fun _ -> ok """callback({"Abn":"74172177893","EntityTypeCode":"PRV","Message":""})"""))
                    key,
                "74172177893",
                au
                Sources.australia (over (fun _ -> ok """callback({"EntityName":"X","Message":""})""")) key,
                "74172177893",
                au
                Sources.korea (over (fun _ -> ok """{"status_code":"OK","data":[{"b_stt_cd":"01"}]}""")) key,
                "1234567891",
                kr
                Sources.croatia
                    (over (croatiaAnswering (fun _ -> ok """{"mbs":80000014,"potpuni_oib":"12345678903","status":1}""")))
                    credentials,
                "12345678903",
                hr
                Sources.croatia
                    (over (
                        croatiaAnswering (fun _ ->
                            ok """{"potpuni_oib":"12345678903","status":1,"tvrtka":{"ime":"X"}}""")
                    ))
                    credentials,
                "12345678903",
                hr
                Sources.greece (over (greeceAnswering (fun _ -> ok """{"arGemi":5}"""))) key, "5", gr
                Sources.greece (over (greeceAnswering (fun _ -> ok """{"coNameEl":"X"}"""))) key, "5", gr
                Sources.austria (over (fun _ -> ok ((fixture "at-auszug.xml").Replace("ns6:FNR=\"58468 h\"", "")))) key,
                "58468h",
                at
                Sources.austria (over (fun _ -> ok ((fixture "at-auszug.xml").Replace("BEZEICHNUNG", "ANDERES")))) key,
                "58468h",
                at
                Sources.denmark
                    (over (fun _ ->
                        ok """{"hits":{"total":1,"hits":[{"_source":{"Vrvirksomhed":{"cvrNummer":36936126}}}]}}"""))
                    credentials,
                "36936126",
                dk
                Sources.denmark
                    (over (fun _ ->
                        ok
                            """{"hits":{"total":1,"hits":[{"_source":{"Vrvirksomhed":{"virksomhedMetadata":{"nyesteNavn":{"navn":"X"}}}}}]}}"""))
                    credentials,
                "36936126",
                dk
            ]

        for registry, number, jurisdiction in cases do
            match! registry.TryFindAsync(number, jurisdiction, ct) with
            | Error(RegistryFailure.UnreadableAnswer _) -> ()
            | other -> failwith $"%s{registry.Coverage.Registry}: %A{other}"
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
let ``a 401 is AccessRefused, on every source`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> answer 401 SecretKey)

        for registry, number, jurisdiction in allRegistries handler do
            match! registry.TryFindAsync(number, jurisdiction, ct) with
            | Error(RegistryFailure.AccessRefused reason) -> holdsNoSecret reason
            | other -> failwith $"%s{registry.Coverage.Registry}: %A{other}"
    }

[<Fact>]
let ``a key quoted in a transport failure is taken out, on every source`` () : Task =
    task {
        let keyed =
            $"failed at ?guid={Uri.EscapeDataString SecretKey}&serviceKey={SecretKey}"

        let signedIn = $"failed for {ClientId}:{ClientSecret}"
        use keyedHandler = new FakeHandler(fun _ -> raise (HttpRequestException keyed))

        use signedInHandler =
            new FakeHandler(fun _ -> raise (HttpRequestException signedIn))

        let cases =
            [
                yield!
                    allRegistries keyedHandler
                    |> List.filter (fun (_, _, scope) -> scope <> hr && scope <> dk)
                yield!
                    allRegistries signedInHandler
                    |> List.filter (fun (_, _, scope) -> scope = hr || scope = dk)
            ]

        Assert.Equal(6, cases.Length)

        for registry, number, jurisdiction in cases do
            match! registry.TryFindAsync(number, jurisdiction, ct) with
            | Error(RegistryFailure.Unavailable reason) ->
                holdsNoSecret reason
                Assert.Contains("{hidden}", reason)
            | other -> failwith $"%s{registry.Coverage.Registry}: %A{other}"
    }

[<Fact>]
let ``a key never shows in a registry's printed form`` () =
    Assert.DoesNotContain(SecretKey, $"%A{key}")
    Assert.DoesNotContain(ClientSecret, $"%A{credentials}")

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
let ``what a source doesn't publish is NotAvailable, without a call`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> failwith "Nothing should be sent")

        for registry, number, jurisdiction in allRegistries handler do
            if not (registry.Coverage.Registry.StartsWith "GEMI") then
                match! registry.OfficersAsync(number, jurisdiction, ct) with
                | Ok {
                         Value = NotAvailable _
                         Raw = ValueNone
                     } -> ()
                | other -> failwith $"%s{registry.Coverage.Registry}: %A{other}"

            match! registry.OwnersAsync(number, jurisdiction, OwnerPurpose.PublicRead, ct) with
            | Ok { Value = NotAvailable _ } -> ()
            | other -> failwith $"%s{registry.Coverage.Registry}: %A{other}"

        let connection = connectionOver handler

        for registry, number, jurisdiction in
            [
                Sources.croatia connection credentials, "12345678903", hr
                Sources.greece connection key, "1", gr
                Sources.austria connection key, "1a", at
            ] do
            match! registry.OwnersAsync(number, jurisdiction, OwnerPurpose.CustomerDueDiligence, ct) with
            | Ok { Value = NeedsEntitlement entitlement } -> Assert.NotEmpty entitlement.Route
            | other -> failwith $"%A{other}"

        for registry, jurisdiction in [ Sources.korea connection key, kr; Sources.croatia connection credentials, hr ] do
            match! registry.SearchAsync("x", jurisdiction, ct) with
            | Ok { Value = NotAvailable _ } -> ()
            | other -> failwith $"%A{other}"

        Assert.Empty handler.Calls
    }

[<Fact>]
let ``each source declares a key, its pace and a licence`` () =
    use handler = new FakeHandler(fun _ -> failwith "Nothing should be sent")

    for registry, _, _ in allRegistries handler do
        match registry.Coverage.Cost with
        | AccessCost.FreeWithKey how -> Assert.NotEmpty how
        | other -> failwith $"%s{registry.Coverage.Registry}: %A{other}"

        Assert.NotEmpty registry.Coverage.Licence

[<Fact>]
let ``Greece keeps to eight calls a minute`` () : Task =
    task {
        let clock = FakeClock()

        use handler =
            new FakeHandler(greeceAnswering (fun _ -> ok (fixture "gr-company.json")))

        let registry =
            Sources.greece (SourceConnection(handler, (fun () -> clock.Now), clock.Delay)) key

        for _ in 1..7 do
            let! _ = registry.TryFindAsync("123456701000", gr, ct)
            ()

        // The codebook and seven records are eight calls in the minute; a ninth would wait past the longest wait allowed.
        Assert.Equal(8, handler.Calls.Length)

        match! registry.TryFindAsync("123456701000", gr, ct) with
        | Error(RegistryFailure.RateLimited(ValueSome _)) -> Assert.Equal(8, handler.Calls.Length)
        | other -> failwith $"%A{other}"
    }

// Kenya

[<Fact>]
let ``Kenya asks for a new token once when KRA refuses the held one`` () : Task =
    task {
        let tokens =
            Collections.Concurrent.ConcurrentQueue<string>([ "first-token"; "second-token" ])

        use handler =
            new FakeHandler(fun call ->
                if call.Target.StartsWith("/v1/token/generate", StringComparison.Ordinal) then
                    match tokens.TryDequeue() with
                    | true, token -> ok $"""{{"access_token":"{token}","expires_in":"3599"}}"""
                    | _ -> answer 401 "no more tokens"
                elif call.Headers["Authorization"] = "Bearer second-token" then
                    ok (fixture "kenya-pin.json")
                else
                    answer 401 """{"errorCode":"401","errorMessage":"Invalid Access Token"}""")

        let clock = FakeClock()

        let registry =
            Sources.kenya (SourceConnection(handler, (fun () -> clock.Now), clock.Delay)) credentials

        let! evidence = findIn registry "P318295670X" (Jurisdiction.ofCode "KE")
        Assert.Equal("P318295670X", (companyOf evidence).Number)

        Assert.Equal<string list>(
            [
                "/v1/token/generate"
                "/checker/v1/pinbypin"
                "/v1/token/generate"
                "/checker/v1/pinbypin"
            ],
            handler.Calls |> List.map (fun call -> call.Target.Split('?')[0])
        )
    }
