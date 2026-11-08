/// Norway, Czechia, Slovakia, Ireland, Poland and Switzerland, replayed from answers recorded on 2026-11-08 with people's names redacted.
/// Norway's 410 body follows the API's documented GoneEnhet schema, as none was met live.
module EuropeLookupSourceTests

open System
open System.IO
open System.Text.Json
open System.Threading.Tasks
open Xunit
open CompanyDueDiligence
open Fake

let private fixture (name: string) =
    File.ReadAllText(Path.Combine(AppContext.BaseDirectory, "Fixtures", name))

/// FakeClock starts here.
let private fixedTime = DateTimeOffset(2026, 11, 6, 12, 0, 0, TimeSpan.Zero)

/// A connection on its own clock, which any wait moves on at once, so a source making two calls is paced without taking time.
let private connectionOver (handler: FakeHandler) =
    let clock = FakeClock()
    SourceConnection(handler, (fun () -> clock.Now), clock.Delay)

let private no = Jurisdiction.ofCode "NO"
let private cz = Jurisdiction.ofCode "CZ"
let private sk = Jurisdiction.ofCode "SK"
let private ie = Jurisdiction.ofCode "IE"
let private pl = Jurisdiction.ofCode "PL"
let private ch = Jurisdiction.ofCode "CH"

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

let private officersOf (registry: ICompanyRegistry) (number: string) (jurisdiction: Jurisdiction) =
    task {
        match! registry.OfficersAsync(number, jurisdiction, ct) with
        | Ok { Value = Available officers } -> return officers
        | other -> return failwith $"%A{other}"
    }

let private ownersOf (registry: ICompanyRegistry) (number: string) (jurisdiction: Jurisdiction) =
    task {
        match! registry.OwnersAsync(number, jurisdiction, OwnerPurpose.CustomerDueDiligence, ct) with
        | Ok { Value = Available listing } -> return listing
        | other -> return failwith $"%A{other}"
    }

let private addressesOf (company: Company) =
    match company.Addresses with
    | Available addresses -> addresses
    | other -> failwith $"%A{other}"

let private nothingSent () =
    new FakeHandler(fun _ -> failwith "Nothing should be sent")

let private json (text: string) =
    JsonDocument.Parse(text).RootElement.Clone()

/// Every registry here, with a number of the right shape.
let private allRegistries (handler: FakeHandler) =
    let connection = connectionOver handler

    [
        Sources.norway connection, "923609016", no
        Sources.czechia connection, "00177041", cz
        Sources.slovakia connection, "35763469", sk
        Sources.ireland connection, "104547", ie
        Sources.poland connection, "0000019193", pl
        Sources.switzerland connection, "CHE-101.237.723", ch
    ]

// Norway

[<Fact>]
let ``Norway reads an entity, Registered with no word, with its former names and both addresses`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "norway-company.json"))
        let! evidence = findIn (Sources.norway (connectionOver handler)) "923 609 016" no
        let company = companyOf evidence
        Assert.Equal("Brønnøysund Enhetsregisteret (Norway)", evidence.Source)
        Assert.Equal(fixedTime, evidence.RetrievedAt)
        Assert.StartsWith("{\"_links\"", evidence.Raw.Value)
        Assert.Equal(ValueNone, evidence.AsOf)
        Assert.Equal("EQUINOR ASA", company.Name)
        Assert.Equal("923609016", company.Number)
        Assert.Equal(CompanyStanding.Registered ValueNone, company.Standing)
        Assert.Equal(RecordRole.Home, company.Role)
        Assert.Equal(ValueSome "Allmennaksjeselskap", company.LegalForm)
        Assert.Equal(ValueSome(DateOnly(1972, 9, 18)), company.IncorporatedOn)
        Assert.Contains("STATOIL ASA", company.OtherNames)
        Assert.Equal("/enhetsregisteret/api/enheter/923609016", handler.Calls.Head.Target)

        match addressesOf company with
        | [ business; postal ] ->
            Assert.Equal(ValueSome "4035", business.Address.PostalCode)
            Assert.Equal<string list>([ "Forusbeen 50" ], business.Address.Lines)
            Assert.Equal(AddressKind.Postal, postal.Kind)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``Norway's two empty 404s are NotAvailable, as an entity removed for legal reasons answers them too`` () : Task =
    task {
        // 509100675, the entity removed for legal reasons, answered an empty 404 on both paths on 2026-11-08.
        use handler = new FakeHandler(fun _ -> answer 404 "")
        let! evidence = findIn (Sources.norway (connectionOver handler)) "509100675" no

        match evidence.Value with
        | NotAvailable reason -> Assert.Contains("removed for legal reasons", reason)
        | other -> failwith $"%A{other}"

        Assert.Equal("/enhetsregisteret/api/underenheter/509100675", handler.Calls[1].Target)
    }

[<Fact>]
let ``Norway's error object on both paths is not held, and one naming another path or with an empty 404 isn't``
    ()
    : Task =
    let errorObject (path: string) =
        $$"""{"tidsstempel":1794133521091,"status":404,"feilmelding":"Ikke funnet","sti":"{{path}}"}"""

    task {
        use own = new FakeHandler(fun call -> answer 404 (errorObject call.Target))
        let! evidence = findIn (Sources.norway (connectionOver own)) "999999999" no
        Assert.Equal(Available ValueNone, evidence.Value)

        use mixed =
            new FakeHandler(fun call ->
                if call.Target.Contains "/underenheter/" then
                    answer 404 ""
                else
                    answer 404 (errorObject call.Target))

        let! evidence = findIn (Sources.norway (connectionOver mixed)) "999999999" no
        Assert.StartsWith("NotAvailable", $"%A{evidence.Value}")

        use elsewhere =
            new FakeHandler(fun _ -> answer 404 (errorObject "/enhetsregisteret/api/enheter/923609016"))

        match! (Sources.norway (connectionOver elsewhere)).TryFindAsync("999999999", no, ct) with
        | Error(RegistryFailure.RequestRefused _) -> ()
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a Norwegian sub-unit's number is NotAvailable, naming its parent`` () : Task =
    task {
        let subUnit =
            """{"organisasjonsnummer":"973861883","navn":"EQUINOR ASA AVD CCB SOTRA","overordnetEnhet":"923609016","organisasjonsform":{"kode":"BEDR"}}"""

        use handler =
            new FakeHandler(fun call ->
                if call.Target.Contains "/underenheter/" then
                    ok subUnit
                else
                    answer 404 "")

        let! evidence = findIn (Sources.norway (connectionOver handler)) "973861883" no

        match evidence.Value with
        | NotAvailable reason ->
            Assert.Contains("sub-unit", reason)
            Assert.Contains("923609016", reason)
        | other -> failwith $"%A{other}"

        Assert.Equal(ValueSome subUnit, evidence.Raw)
    }

[<Fact>]
let ``a 404 not in Brønnøysund's own shape is refused, not read as not held`` () : Task =
    task {
        for body in [ "<html>Not Found</html>"; notFound.Body ] do
            use handler = new FakeHandler(fun _ -> answer 404 body)

            match! (Sources.norway (connectionOver handler)).TryFindAsync("999999999", no, ct) with
            | Error(RegistryFailure.RequestRefused _) -> Assert.Single handler.Calls |> ignore
            | other -> failwith $"%A{other}"

        use subUnitRefused =
            new FakeHandler(fun call ->
                if call.Target.Contains "/underenheter/" then
                    answer 404 "<html/>"
                else
                    answer 404 "")

        match! (Sources.norway (connectionOver subUnitRefused)).TryFindAsync("999999999", no, ct) with
        | Error(RegistryFailure.RequestRefused _) -> ()
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a Norwegian role resigned from, or held by a person recorded as dead, isn't current`` () : Task =
    task {
        let body =
            """{"rollegrupper":[{"roller":[
                {"type":{"kode":"MEDL"},"person":{"navn":{"fornavn":"A"},"erDoed":false},"avregistrert":false,"fratraadt":true},
                {"type":{"kode":"MEDL"},"person":{"navn":{"fornavn":"B"},"erDoed":true},"avregistrert":false},
                {"type":{"kode":"MEDL"},"person":{"navn":{"fornavn":"C"},"erDoed":false},"avregistrert":false}]}]}"""

        use handler = new FakeHandler(fun _ -> ok body)
        let! officers = officersOf (Sources.norway (connectionOver handler)) "923609016" no
        Assert.Equal<string list>([ "C" ], officers |> List.filter _.Current |> List.map _.Name)
    }

[<Fact>]
let ``a struck-off Norwegian entity is closed, with its deletion date`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "norway-deleted.json"))
        let! evidence = findIn (Sources.norway (connectionOver handler)) "933528383" no
        let company = companyOf evidence
        Assert.Equal(CompanyStanding.Closed "Slettet (struck off)", company.Standing)
        Assert.Equal(ValueSome(DateOnly(2026, 11, 2)), company.CeasedOn)
        Assert.Equal(ValueSome "slettet 2026-11-02", company.StatusAsPublished)
    }

[<Fact>]
let ``a Norwegian 410 is an entity removed for legal reasons, kept with the answer`` () : Task =
    task {
        let body = fixture "norway-gone.json"
        use handler = new FakeHandler(fun _ -> answer 410 body)
        let! evidence = findIn (Sources.norway (connectionOver handler)) "509100675" no
        let company = companyOf evidence
        Assert.StartsWith("Closed", $"%A{company.Standing}")
        Assert.Equal(ValueSome(DateOnly(2024, 3, 9)), company.CeasedOn)
        Assert.True(PartyNames.isPlaceholder company.Name)
        Assert.Equal(ValueSome body, evidence.Raw)
    }

[<Theory>]
[<InlineData("""{"respons_klasse":"Enhet","konkurs":true,"underAvvikling":false,"underTvangsavviklingEllerTvangsopplosning":false}""",
             "InInsolvency")>]
[<InlineData("""{"respons_klasse":"Enhet","konkurs":false,"underAvvikling":true,"underTvangsavviklingEllerTvangsopplosning":false}""",
             "WindingUp")>]
[<InlineData("""{"respons_klasse":"Enhet","konkurs":false,"underAvvikling":false,"underTvangsavviklingEllerTvangsopplosning":true}""",
             "WindingUp")>]
[<InlineData("""{"respons_klasse":"Enhet","konkurs":false,"underAvvikling":false,"underTvangsavviklingEllerTvangsopplosning":false,"underRekonstruksjonsforhandlingDato":"2026-01-01"}""",
             "InInsolvency")>]
[<InlineData("""{"respons_klasse":"Enhet","konkurs":false,"underAvvikling":false,"underTvangsavviklingEllerTvangsopplosning":false,"underUtenlandskInsolvensbehandlingDato":"2026-01-01"}""",
             "InInsolvency")>]
[<InlineData("""{"respons_klasse":"Enhet","konkurs":false,"underAvvikling":false,"underTvangsavviklingEllerTvangsopplosning":false}""",
             "Registered")>]
[<InlineData("""{"respons_klasse":"SlettetEnhet","slettedato":"2024-03-09"}""", "Closed")>]
[<InlineData("""{"respons_klasse":"Enhet"}""", "Unknown")>]
let ``every Norwegian marker has a standing, and an entity without them is unknown`` (body: string, expected: string) =
    Assert.StartsWith(expected, $"%A{Norway.standing (json body)}")

[<Theory>]
[<InlineData("92360901")>]
[<InlineData("923609017")>]
[<InlineData("abcdefghi")>]
[<InlineData(null)>]
let ``a value that can't be a Norwegian organisation number isn't sent`` (number: string) : Task =
    task {
        use handler = nothingSent ()

        match! (Sources.norway (connectionOver handler)).TryFindAsync(number, no, ct) with
        | Error(RegistryFailure.InvalidRequest _) -> Assert.Empty handler.Calls
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``Norway reads roles: the manager and board as directors, a resigned member as former, an auditor firm as an entity``
    ()
    : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "norway-roles.json"))
        let! officers = officersOf (Sources.norway (connectionOver handler)) "923609016" no
        Assert.Equal("/enhetsregisteret/api/enheter/923609016/roller", handler.Calls.Head.Target)
        Assert.Equal(6, officers.Length)
        let manager = officers.Head
        Assert.Equal("PERSON ONE", manager.Name)
        Assert.Equal(OfficerRole.Director, manager.Role)
        Assert.Equal(ValueSome { Year = 1900; Month = 1 }, manager.BornIn)

        Assert.Single(officers |> List.filter (fun officer -> not officer.Current))
        |> ignore

        Assert.Contains(officers, (fun officer -> officer.Role = OfficerRole.Other "Varamedlem"))

        let auditor =
            officers |> List.find (fun officer -> officer.Kind = PartyKind.LegalEntity)

        Assert.Equal("ERNST & YOUNG AS", auditor.Name)
        Assert.Equal(ValueSome "976389387", auditor.Entity.Value.RegistrationNumber)
    }

[<Fact>]
let ``Norway's 404 for roles says nothing about officers`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> answer 404 "")

        match! (Sources.norway (connectionOver handler)).OfficersAsync("933528383", no, ct) with
        | Ok { Value = NotAvailable _ } -> ()
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``Norway's search reads a page with its total, and an empty page without its list`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "norway-search.json"))

        match! (Sources.norway (connectionOver handler)).SearchAsync("equinor asa", no, ct) with
        | Ok { Value = Available found } ->
            Assert.Equal(2, found.Matches.Length)
            Assert.Equal(ValueSome 409, found.TotalMatches)
            Assert.False found.Complete
            Assert.Contains("navn=equinor%20asa", handler.Calls.Head.Target)
        | other -> failwith $"%A{other}"

        use empty =
            new FakeHandler(fun _ -> ok """{"page":{"number":0,"size":100,"totalElements":0,"totalPages":0}}""")

        match! (Sources.norway (connectionOver empty)).SearchAsync("zzzz", no, ct) with
        | Ok { Value = Available found } ->
            Assert.Empty found.Matches
            Assert.Equal(ValueSome 0, found.TotalMatches)
            Assert.False(found.Complete, "The search leaves out deleted entities")
        | other -> failwith $"%A{other}"
    }

// Czechia

[<Fact>]
let ``Czechia reads an economic subject, with ARES's date and both addresses`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "czechia-company.json"))
        let! evidence = findIn (Sources.czechia (connectionOver handler)) "00177041" cz
        let company = companyOf evidence
        Assert.Equal("ARES (Czechia)", evidence.Source)
        Assert.Equal("Škoda Auto a.s.", company.Name)
        Assert.Equal("00177041", company.Number)
        Assert.Equal(CompanyStanding.Active, company.Standing)
        Assert.Equal(ValueSome "VR AKTIVNI; RES AKTIVNI", company.StatusAsPublished)
        Assert.Equal(RecordRole.Home, company.Role)
        Assert.Equal(ValueSome "Akciová společnost", company.LegalForm)
        Assert.Equal(ValueSome(DateOnly(1990, 11, 20)), company.IncorporatedOn)
        Assert.Equal(ValueSome(DateOnly(2026, 10, 20)), evidence.AsOf)
        Assert.True evidence.Raw.IsSome
        Assert.Equal("/ekonomicke-subjekty-v-be/rest/ekonomicke-subjekty/00177041", handler.Calls.Head.Target)

        match addressesOf company with
        | [ seat; postal ] ->
            Assert.Equal(AddressKind.RegisteredOffice, seat.Kind)
            Assert.Equal(ValueSome "29301", seat.Address.PostalCode)
            Assert.Equal(AddressKind.Postal, postal.Kind)
            Assert.Equal(3, postal.Address.Lines.Length)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``Czechia's NENALEZENO is NotAvailable, as a company deleted from the register answers it, and another 404 is refused``
    ()
    : Task =
    task {
        // 47116102 was deleted from the commercial register; ARES answered this on 2026-11-08.
        use handler = new FakeHandler(fun _ -> answer 404 (fixture "czechia-deleted.json"))
        let! evidence = findIn (Sources.czechia (connectionOver handler)) "47116102" cz

        match evidence.Value with
        | NotAvailable reason -> Assert.Contains("deleted from the register", reason)
        | other -> failwith $"%A{other}"

        use other = new FakeHandler(fun _ -> notFound)

        match! (Sources.czechia (connectionOver other)).TryFindAsync("00000019", cz, ct) with
        | Error(RegistryFailure.RequestRefused _) -> ()
        | result -> failwith $"%A{result}"
    }

[<Theory>]
[<InlineData("AKTIVNI", "AKTIVNI", "NEEXISTUJICI", "X s.r.o.", "Active")>]
[<InlineData("AKTIVNI", "AKTIVNI", "NEEXISTUJICI", "X s.r.o. v likvidaci", "WindingUp")>]
[<InlineData("AKTIVNI", "AKTIVNI", "AKTIVNI", "X s.r.o.", "ActiveWithConcern")>]
[<InlineData("POZASTAVENY", "AKTIVNI", "NEEXISTUJICI", "X", "ActiveWithConcern")>]
[<InlineData("ZANIKLY", "ZANIKLY", "NEEXISTUJICI", "X", "Closed")>]
[<InlineData("HISTORICKY", "AKTIVNI", "NEEXISTUJICI", "X", "Closed")>]
[<InlineData("LOGICKY_SMAZANY", "AKTIVNI", "NEEXISTUJICI", "X", "Closed")>]
[<InlineData("NEEXISTUJICI", "AKTIVNI", "NEEXISTUJICI", "X", "Active")>]
[<InlineData("NEEXISTUJICI", "ZANIKLY", "NEEXISTUJICI", "X", "Closed")>]
[<InlineData("BUDOUCI", "AKTIVNI", "NEEXISTUJICI", "X", "Unknown")>]
[<InlineData("NEEXISTUJICI", "NEEXISTUJICI", "NEEXISTUJICI", "X", "Unknown")>]
[<InlineData("NOVY_STAV", "AKTIVNI", "NEEXISTUJICI", "X", "Unknown")>]
let ``every ARES state has a standing, and a new one is unknown``
    (vr: string, res: string, ir: string, name: string, expected: string)
    =
    Assert.StartsWith(expected, $"%A{Czechia.standing (ValueSome vr) (ValueSome res) (ValueSome ir) name}")

[<Theory>]
[<InlineData("421", "ForeignRegistration")>]
[<InlineData("426", "ForeignRegistration")>]
[<InlineData("121", "Home")>]
let ``a Czech legal form says whether it is a foreign branch`` (form: string, expected: string) =
    Assert.Equal(expected, $"%A{Czechia.role (ValueSome form)}")

[<Theory>]
[<InlineData("1234567")>]
[<InlineData("123456789")>]
[<InlineData("abcdefgh")>]
[<InlineData(null)>]
let ``a value that can't be an IČO isn't sent to ARES`` (number: string) : Task =
    task {
        use handler = nothingSent ()

        match! (Sources.czechia (connectionOver handler)).TryFindAsync(number, cz, ct) with
        | Error(RegistryFailure.InvalidRequest _) -> Assert.Empty handler.Calls
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``Czechia reads the board as directors, prokura and the supervisory board by title, and a former member as ended``
    ()
    : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "czechia-vr.json"))
        let! officers = officersOf (Sources.czechia (connectionOver handler)) "00177041" cz
        Assert.Equal("/ekonomicke-subjekty-v-be/rest/ekonomicke-subjekty-vr/00177041", handler.Calls.Head.Target)
        Assert.Equal(6, officers.Length)

        Assert.Equal(
            3,
            officers
            |> List.filter (fun officer -> officer.Role = OfficerRole.Director)
            |> List.length
        )

        let former = officers |> List.find (fun officer -> not officer.Current)
        Assert.Equal(ValueSome(DateOnly(2021, 10, 13)), former.EndedOn)
        Assert.Equal("PERSON FOUR", former.Name)

        Assert.Equal(
            2,
            officers
            |> List.filter (fun officer -> officer.Role = OfficerRole.Other "Prokura")
            |> List.length
        )

        Assert.Contains(officers, (fun officer -> officer.Role = OfficerRole.Other "člen dozorčí rady"))
    }

[<Fact>]
let ``Czechia reads a sole shareholder, and one without an IČO leaves a gap`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "czechia-vr.json"))
        let! listing = ownersOf (Sources.czechia (connectionOver handler)) "00177041" cz
        let current = listing.Owners |> List.filter _.Current
        Assert.Equal(ValueSome "VOLKSWAGEN FINANCE LUXEMBURG", current.Head.Name)
        Assert.Equal(ValueSome "LU", current.Head.Entity.Value.Country)
        Assert.Equal(1, current.Length)
        Assert.Equal(2, listing.Owners.Length)
        Assert.Equal<OwnershipGap list>([ OwnershipGap.EntityNotIdentified ], listing.Gaps)
    }

[<Fact>]
let ``Czechia reads an s.r.o.'s partner with its share, and its former directors as ended`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "czechia-vr-sro.json"))
        let registry = Sources.czechia (connectionOver handler)
        let! listing = ownersOf registry "06387233" cz
        let partner = listing.Owners |> List.find _.Current
        Assert.Equal(ValueSome "Seznam.cz, a.s.", partner.Name)
        Assert.Equal(ValueSome "26168685", partner.Entity.Value.RegistrationNumber)
        Assert.Equal(ValueSome "CZ", partner.Entity.Value.Country)
        Assert.Equal(ValueSome { From = 100; To = 100 }, partner.Control.Head.Band)
        Assert.True(OwnerListing.isComplete listing)
        let! officers = officersOf registry "06387233" cz
        Assert.All(officers, (fun officer -> Assert.False officer.Current))
    }

[<Fact>]
let ``an entity outside Czechia's public register has no bodies or owners there`` () : Task =
    task {
        use handler =
            new FakeHandler(fun _ -> answer 404 (fixture "czechia-not-found.json"))

        let registry = Sources.czechia (connectionOver handler)

        match! registry.OfficersAsync("00000019", cz, ct) with
        | Ok { Value = NotAvailable _ } -> ()
        | other -> failwith $"%A{other}"

        match! registry.OwnersAsync("00000019", cz, OwnerPurpose.PublicRead, ct) with
        | Ok { Value = NotAvailable _ } -> ()
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``Czechia's search posts the name and reads the matches with their total`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "czechia-search.json"))

        match! (Sources.czechia (connectionOver handler)).SearchAsync("Seznam.cz", cz, ct) with
        | Ok { Value = Available found } ->
            Assert.Equal(3, found.Matches.Length)
            Assert.Equal(ValueSome 5, found.TotalMatches)
            Assert.False found.Complete
            Assert.Equal("/ekonomicke-subjekty-v-be/rest/ekonomicke-subjekty/vyhledat", handler.Calls.Head.Target)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``Czechia's search is never complete, as ARES leaves out deleted entities, whether it lists none or answers NENALEZENO``
    ()
    : Task =
    task {
        for status, body in
            [
                200, """{"pocetCelkem":0,"ekonomickeSubjekty":[]}"""
                404, fixture "czechia-deleted.json"
            ] do
            use handler = new FakeHandler(fun _ -> answer status body)

            match! (Sources.czechia (connectionOver handler)).SearchAsync("zzqqxx", cz, ct) with
            | Ok { Value = Available found } ->
                Assert.Empty found.Matches
                Assert.Equal(ValueSome 0, found.TotalMatches)
                Assert.False found.Complete
            | other -> failwith $"%A{other}"
    }

// Slovakia

let private slovakiaHandler (search: string) (entity: string) =
    new FakeHandler(fun call ->
        if call.Target.Contains "/search?" then
            ok search
        else
            ok entity)

[<Fact>]
let ``Slovakia finds the record by IČO, then reads it, with the database's date`` () : Task =
    task {
        use handler =
            slovakiaHandler (fixture "slovakia-search-one.json") (fixture "slovakia-entity.json")

        let! evidence = findIn (Sources.slovakia (connectionOver handler)) "35763469" sk
        let company = companyOf evidence
        Assert.Equal("RPO (Slovakia)", evidence.Source)
        Assert.Equal("Slovak Telekom, a.s.", company.Name)
        Assert.Equal("35763469", company.Number)
        Assert.Equal(CompanyStanding.Registered ValueNone, company.Standing)
        Assert.Equal(RecordRole.Home, company.Role)
        Assert.Equal(ValueSome "Akciová spoločnosť", company.LegalForm)
        Assert.Equal(ValueSome(DateOnly(2026, 10, 16)), evidence.AsOf)
        Assert.Contains("Slovak Telecom, a.s.", company.OtherNames)
        Assert.StartsWith("{\"id\":1013242", evidence.Raw.Value)
        Assert.Equal("/rpo/v1/search?identifier=35763469", handler.Calls.Head.Target)
        Assert.Equal("/rpo/v1/entity/1013242?showHistoricalData=true", handler.Calls[1].Target)

        match addressesOf company with
        | [ seat ] ->
            Assert.Equal<string list>([ "Bajkalská 28" ], seat.Address.Lines)
            Assert.Equal(ValueSome "81762", seat.Address.PostalCode)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``Slovakia with no record for an IČO proves nothing, as a nightly replica, and with two can't tell them apart``
    ()
    : Task =
    task {
        use none = new FakeHandler(fun _ -> ok """{"results":[],"license":"cc-by 4.0"}""")
        let! evidence = findIn (Sources.slovakia (connectionOver none)) "99999999" sk

        match evidence.Value with
        | NotAvailable reason -> Assert.Contains("nightly replica", reason)
        | other -> failwith $"%A{other}"

        Assert.Single none.Calls |> ignore

        use two = new FakeHandler(fun _ -> ok (fixture "slovakia-search-two.json"))
        let! evidence = findIn (Sources.slovakia (connectionOver two)) "00000001" sk

        match evidence.Value with
        | NotAvailable reason -> Assert.Contains("4 records", reason)
        | other -> failwith $"%A{other}"

        Assert.Single two.Calls |> ignore
    }

[<Fact>]
let ``a Slovak entity in liquidation is winding up, with that against it, and its partners are its owners`` () : Task =
    task {
        use handler =
            slovakiaHandler """{"results":[{"id":192777}]}""" (fixture "slovakia-liquidation.json")

        let registry = Sources.slovakia (connectionOver handler)
        let! evidence = findIn registry "46914544" sk
        let company = companyOf evidence
        Assert.Equal(CompanyStanding.WindingUp "V likvidácii", company.Standing)
        Assert.Equal<CompanyConcern list>([ CompanyConcern.InsolvencyHistory ], company.Concerns.Stated)
        let! listing = ownersOf registry "46914544" sk
        Assert.Equal(1, listing.Owners |> List.filter _.Current |> List.length)
        Assert.Equal(2, listing.Owners.Length)
        Assert.True(OwnerListing.isComplete listing)
        let! officers = officersOf registry "46914544" sk
        Assert.Contains(officers, (fun officer -> officer.Role = OfficerRole.Other "Likvidátor"))
        Assert.Contains(officers, (fun officer -> officer.Role = OfficerRole.Director && not officer.Current))
    }

[<Theory; InlineData("040", "AT"); InlineData("703", "SK"); InlineData("898", "")>]
let ``a Slovak corporate owner's registration country is the ISO code of RPO's numeric one``
    (code: string, expected: string)
    : Task =
    task {
        let entity =
            (fixture "slovakia-liquidation.json")
                .Replace(
                    "\"validFrom\":\"2022-10-26\",\"address\":{\"formatedAddress\":\"ADDRESS REDACTED\",\"country\":{\"value\":\"Slovenská republika\",\"code\":\"703\"",
                    $"\"validFrom\":\"2022-10-26\",\"address\":{{\"formatedAddress\":\"ADDRESS REDACTED\",\"country\":{{\"value\":\"X\",\"code\":\"{code}\""
                )
                .Replace(
                    "\"personName\":{\"formatedName\":\"PERSON FIVE\",\"givenNames\":[\"PERSON\"],\"familyNames\":[\"FIVE\"]}",
                    "\"fullName\":\"OWNER GMBH\",\"identifier\":\"FN 1\""
                )

        use handler = slovakiaHandler """{"results":[{"id":192777}]}""" entity
        let! listing = ownersOf (Sources.slovakia (connectionOver handler)) "46914544" sk
        let owner = listing.Owners |> List.find _.Current
        Assert.Equal(PartyKind.LegalEntity, owner.Kind)
        Assert.Equal((if expected = "" then ValueNone else ValueSome expected), owner.Entity.Value.Country)
    }

[<Theory>]
[<InlineData("3", "V konkurze", "InInsolvency")>]
[<InlineData("2", "V likvidácii", "WindingUp")>]
[<InlineData("", "V reštrukturalizácii", "InInsolvency")>]
[<InlineData("", "A status added later", "Unknown")>]
let ``a Slovak legal status has a standing, and a new one is unknown`` (code: string, word: string, expected: string) =
    let code = if code = "" then ValueNone else ValueSome code
    Assert.StartsWith(expected, $"%A{Slovakia.statusStanding code (ValueSome word)}")

[<Fact>]
let ``a Slovak termination closes the entity, and no status leaves it registered without a word`` () =
    Assert.StartsWith("Closed", $"%A{Slovakia.standing (ValueSome(DateOnly(2021, 10, 22))) []}")
    Assert.Equal(CompanyStanding.Registered ValueNone, Slovakia.standing ValueNone [])

    Assert.StartsWith(
        "InInsolvency",
        $"""%A{Slovakia.standing
                   ValueNone
                   [
                       ValueSome "2", ValueSome "V likvidácii"
                       ValueSome "3", ValueSome "V konkurze"
                   ]}"""
    )

[<Fact>]
let ``Slovakia reads statutory bodies as directors and a prokurist by type, and lists no owners of a joint-stock company``
    ()
    : Task =
    task {
        use handler =
            slovakiaHandler (fixture "slovakia-search-one.json") (fixture "slovakia-entity.json")

        let registry = Sources.slovakia (connectionOver handler)
        let! officers = officersOf registry "35763469" sk
        Assert.Equal(4, officers.Length)

        Assert.Equal(
            2,
            officers
            |> List.filter (fun officer -> officer.Role = OfficerRole.Director && officer.Current)
            |> List.length
        )

        Assert.Contains(officers, (fun officer -> officer.Role = OfficerRole.Other "Prokurista"))
        Assert.All(officers, (fun officer -> Assert.StartsWith("PERSON ", officer.Name)))

        match! registry.OwnersAsync("35763469", sk, OwnerPurpose.PublicRead, ct) with
        | Ok { Value = NotAvailable _ } -> ()
        | other -> failwith $"%A{other}"
    }

[<Theory>]
[<InlineData("1234567")>]
[<InlineData("123456789")>]
[<InlineData("abcdefgh")>]
[<InlineData(null)>]
let ``a value that can't be a Slovak IČO isn't sent`` (number: string) : Task =
    task {
        use handler = nothingSent ()

        match! (Sources.slovakia (connectionOver handler)).TryFindAsync(number, sk, ct) with
        | Error(RegistryFailure.InvalidRequest _) -> Assert.Empty handler.Calls
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``Slovakia's name search needs three characters, and reads what it finds`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "slovakia-search-one.json"))
        let registry = Sources.slovakia (connectionOver handler)

        match! registry.SearchAsync("ab", sk, ct) with
        | Error(RegistryFailure.InvalidRequest _) -> Assert.Empty handler.Calls
        | other -> failwith $"%A{other}"

        match! registry.SearchAsync("Slovak Telekom", sk, ct) with
        | Ok { Value = Available found } ->
            Assert.Equal("35763469", found.Matches.Head.Number)
            Assert.True found.Complete
            Assert.Equal("/rpo/v1/search?fullName=Slovak%20Telekom", handler.Calls.Head.Target)
        | other -> failwith $"%A{other}"
    }

// Ireland

[<Fact>]
let ``Ireland reads a company from the snapshot, with the record alone as raw`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "ireland-company.json"))
        let! evidence = findIn (Sources.ireland (connectionOver handler)) "0104547" ie
        let company = companyOf evidence
        Assert.Equal("CRO open data (Ireland)", evidence.Source)
        Assert.Equal("RYANAIR DESIGNATED ACTIVITY COMPANY", company.Name)
        Assert.Equal("104547", company.Number)
        Assert.Equal(CompanyStanding.Active, company.Standing)
        Assert.Equal(ValueSome "Normal", company.StatusAsPublished)
        Assert.Equal(RecordRole.Home, company.Role)
        Assert.Equal(ValueSome(DateOnly(1984, 11, 28)), company.IncorporatedOn)
        Assert.StartsWith("{\"_id\":100906", evidence.Raw.Value)
        Assert.Contains("\"company_num\":\"104547\"", Uri.UnescapeDataString handler.Calls.Head.Target)

        match addressesOf company with
        | [ office ] -> Assert.Equal(4, office.Address.Lines.Length)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a number missing from Ireland's snapshot proves nothing`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "ireland-none.json"))
        let! evidence = findIn (Sources.ireland (connectionOver handler)) "999999" ie

        match evidence.Value with
        | NotAvailable reason -> Assert.Contains("isn't the register saying", reason)
        | other -> failwith $"%A{other}"
    }

[<Theory>]
[<InlineData("Normal ", "Active")>]
[<InlineData("Normal", "Active")>]
[<InlineData("Strike Off Listed", "ActiveWithConcern")>]
[<InlineData("Liquidation", "WindingUp")>]
[<InlineData("Liquidation (UK)", "WindingUp")>]
[<InlineData("Liquidation stayed", "WindingUp")>]
[<InlineData("Examinership", "InInsolvency")>]
[<InlineData("Administration", "InInsolvency")>]
[<InlineData("Administration (UK)", "InInsolvency")>]
[<InlineData("Dissolved", "Dissolved")>]
[<InlineData("Dissolved-20 years", "Dissolved")>]
[<InlineData("Dissolved PostMerger", "Dissolved")>]
[<InlineData("Dissolved by Divis", "Dissolved")>]
[<InlineData("Struck Off", "Closed")>]
[<InlineData("Ceased", "Closed")>]
[<InlineData("Ceased IRL", "Closed")>]
[<InlineData("Conv. to IndProvSoc", "Closed")>]
[<InlineData("Deleted CB Merger", "Closed")>]
[<InlineData("Deleted per SE merge", "Closed")>]
[<InlineData("Deleted as per SE Tr", "Closed")>]
[<InlineData("Deleted CB Conversion", "Closed")>]
[<InlineData("De-Registered ICAV", "Closed")>]
[<InlineData("De-Registered Investment Company", "Closed")>]
[<InlineData("A status added later", "Unknown")>]
let ``every status in Ireland's snapshot has a standing, and a new one is unknown`` (word: string, expected: string) =
    Assert.StartsWith(expected, $"%A{Ireland.standing (ValueSome word)}")

[<Fact>]
let ``an Irish company whose liquidation the court stayed is winding up, not active`` () : Task =
    task {
        // 47441, the one "Liquidation stayed" record in the snapshot on 2026-11-08.
        use handler =
            new FakeHandler(fun _ -> ok (fixture "ireland-liquidation-stayed.json"))

        let! evidence = findIn (Sources.ireland (connectionOver handler)) "47441" ie
        let company = companyOf evidence
        Assert.Equal(CompanyStanding.WindingUp "Liquidation stayed", company.Standing)
        Assert.False(CompanyStanding.isActive company.Standing)
    }

[<Theory>]
[<InlineData(1128, "ForeignRegistration")>]
[<InlineData(1139, "ForeignRegistration")>]
[<InlineData(1100, "NotStated")>]
[<InlineData(1150, "Home")>]
let ``an Irish company type says whether it is an external company`` (code: int, expected: string) =
    Assert.Equal(expected, $"%A{Ireland.role (ValueSome code)}")

[<Theory>]
[<InlineData("12345678")>]
[<InlineData("abc")>]
[<InlineData("0")>]
[<InlineData("")>]
let ``a value that can't be a CRO number isn't sent`` (number: string) : Task =
    task {
        use handler = nothingSent ()

        match! (Sources.ireland (connectionOver handler)).TryFindAsync(number, ie, ct) with
        | Error(RegistryFailure.InvalidRequest _) -> Assert.Empty handler.Calls
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``Ireland's search reads the matches with their total`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "ireland-search.json"))

        match! (Sources.ireland (connectionOver handler)).SearchAsync("ryanair designated", ie, ct) with
        | Ok { Value = Available found } ->
            Assert.Equal(2, found.Matches.Length)
            Assert.Equal(ValueSome 2, found.TotalMatches)
            Assert.True found.Complete
            Assert.Contains("\"company_name\":\"ryanair designated\"", Uri.UnescapeDataString handler.Calls.Head.Target)
        | other -> failwith $"%A{other}"
    }

// Poland

[<Fact>]
let ``Poland reads a current extract, Registered with no word, padding the number`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "poland-current.json"))
        let! evidence = findIn (Sources.poland (connectionOver handler)) "19193" pl
        let company = companyOf evidence
        Assert.Equal("KRS (Poland)", evidence.Source)
        Assert.Equal("POLSKIE KOLEJE PAŃSTWOWE SPÓŁKA AKCYJNA", company.Name)
        Assert.Equal("0000019193", company.Number)
        Assert.Equal(CompanyStanding.Registered ValueNone, company.Standing)
        Assert.Equal(ValueSome "dzial6: polaczeniePodzialPrzeksztalcenie", company.StatusAsPublished)
        Assert.Equal(ValueSome "SPÓŁKA AKCYJNA", company.LegalForm)
        Assert.Equal(RecordRole.Home, company.Role)
        Assert.Equal(ValueSome(DateOnly(2026, 10, 8)), evidence.AsOf)
        Assert.Equal("/api/krs/OdpisAktualny/0000019193?rejestr=P&format=json", handler.Calls.Head.Target)

        match addressesOf company with
        | [ seat ] -> Assert.Equal(ValueSome "02-305", seat.Address.PostalCode)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a Polish entity with no current extract is read from its full extract as deleted`` () : Task =
    task {
        use handler =
            new FakeHandler(fun call ->
                if call.Target.Contains "OdpisAktualny" then
                    answer 204 ""
                else
                    ok (fixture "poland-deleted-full.json"))

        let registry = Sources.poland (connectionOver handler)
        let! evidence = findIn registry "680630" pl
        let company = companyOf evidence
        Assert.Equal(CompanyStanding.Closed "WYKREŚLENIE Z KRAJOWEGO REJESTRU SĄDOWEGO", company.Standing)
        Assert.Equal(ValueSome(DateOnly(2026, 8, 6)), company.CeasedOn)
        Assert.Equal("IDOK SPÓŁKA Z OGRANICZONĄ ODPOWIEDZIALNOŚCIĄ W LIKWIDACJI", company.Name)
        Assert.Contains("IDOK SPÓŁKA Z OGRANICZONĄ ODPOWIEDZIALNOŚCIĄ", company.OtherNames)
        Assert.Equal("/api/krs/OdpisPelny/0000680630?rejestr=P&format=json", handler.Calls[1].Target)

        match! registry.OfficersAsync("680630", pl, ct) with
        | Ok { Value = NotAvailable _ } -> ()
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``Poland asks the associations' register after a 404, and two 404s prove nothing`` () : Task =
    task {
        use none =
            new FakeHandler(fun _ -> answer 404 """{"title":"Not Found","status":404}""")

        let! evidence = findIn (Sources.poland (connectionOver none)) "1" pl

        match evidence.Value with
        | NotAvailable reason -> Assert.Contains("doesn't say", reason)
        | other -> failwith $"%A{other}"

        Assert.Equal("/api/krs/OdpisAktualny/0000000001?rejestr=S&format=json", none.Calls[1].Target)

        use association =
            new FakeHandler(fun call ->
                if call.Target.Contains "rejestr=P" then
                    answer 404 ""
                else
                    ok (fixture "poland-current.json"))

        let! evidence = findIn (Sources.poland (connectionOver association)) "19193" pl
        Assert.Equal("0000019193", (companyOf evidence).Number)
    }

[<Theory>]
[<InlineData("", "Registered")>]
[<InlineData("polaczeniePodzialPrzeksztalcenie", "Registered")>]
[<InlineData("likwidacja", "WindingUp")>]
[<InlineData("likwidacja,rozwiazanieUniewaznienie", "WindingUp")>]
[<InlineData("rozwiazanieUniewaznienie", "WindingUp")>]
[<InlineData("postepowanieUpadlosciowe", "InInsolvency")>]
[<InlineData("informacjaOOtwarciuPostepowaniaUpadlosciowego,likwidacja", "InInsolvency")>]
[<InlineData("postepowanieRestrukturyzacyjne", "InInsolvency")>]
[<InlineData("postepowanieNaprawcze", "InInsolvency")>]
[<InlineData("sekcjaDodanaPozniej", "Unknown")>]
let ``a section in part 6 of a Polish extract has a standing, and a new one is unknown``
    (sections: string, expected: string)
    =
    let names =
        sections.Split(',', StringSplitOptions.RemoveEmptyEntries) |> List.ofArray

    Assert.StartsWith(expected, $"%A{Poland.standing names}")

[<Fact>]
let ``a Polish current extract with a liquidation in part 6 is winding up`` () : Task =
    task {
        let current = fixture "poland-current.json"
        let at = current.IndexOf "\"dzial6\":"

        let body =
            current.Substring(0, at)
            + "\"dzial6\":{\"likwidacja\":{\"likwidatorzy\":[{\"nazwa\":\"LIKWIDATOR SP. Z O.O.\"}]}}}}}"

        use handler = new FakeHandler(fun _ -> ok body)
        let registry = Sources.poland (connectionOver handler)
        let! evidence = findIn registry "19193" pl
        Assert.Equal(CompanyStanding.WindingUp "Likwidacja (liquidation)", (companyOf evidence).Standing)
        let! officers = officersOf registry "19193" pl

        Assert.Contains(
            officers,
            (fun officer ->
                officer.Role = OfficerRole.Other "LIKWIDATOR"
                && officer.Kind = PartyKind.LegalEntity)
        )
    }

[<Fact>]
let ``Poland's masked names come through as withheld, with their functions`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "poland-current.json"))
        let! officers = officersOf (Sources.poland (connectionOver handler)) "19193" pl
        Assert.Equal(7, officers.Length)
        Assert.All(officers, (fun officer -> Assert.Equal(PartyKind.Withheld, officer.Kind)))
        Assert.All(officers, (fun officer -> Assert.True(PartyNames.isPlaceholder officer.Name)))

        Assert.Equal(
            3,
            officers
            |> List.filter (fun officer -> officer.Role = OfficerRole.Director)
            |> List.length
        )

        Assert.Equal(
            2,
            officers
            |> List.filter (fun officer -> officer.Role = OfficerRole.Other "RADA NADZORCZA")
            |> List.length
        )

        Assert.Equal(
            2,
            officers
            |> List.filter (fun officer -> officer.Role = OfficerRole.Other "PROKURENT (ŁĄCZNA)")
            |> List.length
        )
    }

[<Fact>]
let ``Poland's sole shareholder without a KRS number leaves a gap`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "poland-current.json"))
        let! listing = ownersOf (Sources.poland (connectionOver handler)) "19193" pl
        let holder = listing.Owners.Head
        Assert.Equal(ValueSome "SKARB PAŃSTWA", holder.Name)
        Assert.Equal(PartyKind.LegalEntity, holder.Kind)

        Assert.Equal<OwnershipGap list>(
            [ OwnershipGap.OwnerNotIdentified; OwnershipGap.EntityNotIdentified ],
            listing.Gaps
        )
    }

/// A current extract whose part 1 holds the given partners of an sp. z o.o. in place of the sole shareholder.
let private polandWithPartners (partners: string) =
    let current = fixture "poland-current.json"
    let at = current.IndexOf "\"jedynyAkcjonariusz\":"
    let after = current.IndexOf("],", at) + 2

    current.Substring(0, at)
    + $"\"wspolnicySpzoo\":[{partners}],"
    + current.Substring after

[<Fact>]
let ``Poland's partners leave a gap below KRS's 10% threshold, unless a sole partner holds every share`` () : Task =
    task {
        let partner (name: string) (whole: bool) (krs: string) =
            $"""{{"nazwa":"{name}","krs":{{"krs":"{krs}"}},"posiadaneUdzialy":"50 UDZIAŁÓW","czyPosiadaCaloscUdzialow":{(if whole then "true" else "false")}}}"""

        use two =
            new FakeHandler(fun _ ->
                ok (
                    polandWithPartners (
                        partner "A SP. Z O.O." false "0000000001"
                        + ","
                        + partner "B SP. Z O.O." false "0000000002"
                    )
                ))

        let! listing = ownersOf (Sources.poland (connectionOver two)) "19193" pl
        Assert.Equal(2, listing.Owners.Length)
        Assert.Equal<OwnershipGap list>([ OwnershipGap.OwnerNotIdentified ], listing.Gaps)
        let entity = listing.Owners.Head.Entity.Value
        Assert.Equal(ValueSome "PL", entity.Country)
        Assert.Equal(ValueSome "0000000001", entity.RegistrationNumber)

        use sole =
            new FakeHandler(fun _ -> ok (polandWithPartners (partner "A SP. Z O.O." true "0000000001")))

        let! listing = ownersOf (Sources.poland (connectionOver sole)) "19193" pl
        Assert.True(OwnerListing.isComplete listing)
        Assert.Equal(ValueSome { From = 100; To = 100 }, listing.Owners.Head.Control.Head.Band)
    }

[<Theory>]
[<InlineData("12345678901")>]
[<InlineData("abc")>]
[<InlineData("0000000000")>]
[<InlineData("")>]
let ``a value that can't be a KRS number isn't sent`` (number: string) : Task =
    task {
        use handler = nothingSent ()

        match! (Sources.poland (connectionOver handler)).TryFindAsync(number, pl, ct) with
        | Error(RegistryFailure.InvalidRequest _) -> Assert.Empty handler.Calls
        | other -> failwith $"%A{other}"
    }

// Switzerland

let private soapHeader (call: Call) =
    call.Headers |> Map.tryFind "SOAPAction" |> Option.defaultValue ""

[<Fact>]
let ``Switzerland reads a UID entry, active in the commercial register, with the record alone as raw`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "switzerland-company.xml"))

        let! evidence =
            findIn (Sources.switzerland (connectionOver handler)) "CHE-101.237.723" ch

        let company = companyOf evidence
        Assert.Equal("UID register (Switzerland)", evidence.Source)
        Assert.Equal("Nestlé Suisse S.A.", company.Name)
        Assert.Equal("CHE-101.237.723", company.Number)
        Assert.Equal(CompanyStanding.Active, company.Standing)
        Assert.Equal(ValueSome "UID 3; HR 2/1; MWST 2", company.StatusAsPublished)
        Assert.Equal(ValueSome "Aktiengesellschaft", company.LegalForm)
        Assert.Equal(RecordRole.Home, company.Role)
        Assert.DoesNotContain(CompanyConcern.RegisteredOfficeUndeliverable, company.Concerns.Unconfirmed)
        Assert.Empty company.Concerns.Stated
        Assert.StartsWith("<organisationType", evidence.Raw.Value)
        Assert.Equal("/V5.0/PublicServices.svc", handler.Calls.Head.Target)

        Assert.Equal(
            "\"http://www.uid.admin.ch/xmlns/uid-wse/IPublicServices/GetByUID\"",
            soapHeader handler.Calls.Head
        )

        match addressesOf company with
        | [ office ] ->
            Assert.Equal(AddressKind.RegisteredOffice, office.Kind)
            Assert.Equal(ValueSome "Vevey", office.Address.Locality)
            Assert.Equal(ValueSome "1800", office.Address.PostalCode)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a deleted Swiss entry is closed, and its undeliverable address is stated`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "switzerland-deleted.xml"))

        let! evidence =
            findIn (Sources.switzerland (connectionOver handler)) "CHE-103.208.336" ch

        let company = companyOf evidence
        Assert.StartsWith("Closed", $"%A{company.Standing}")
        Assert.Equal<CompanyConcern list>([ CompanyConcern.RegisteredOfficeUndeliverable ], company.Concerns.Stated)
    }

[<Fact>]
let ``a Swiss UID held at two seats can't be told apart, and none is no public entry`` () : Task =
    task {
        use two = new FakeHandler(fun _ -> ok (fixture "switzerland-two-seats.xml"))

        let! evidence =
            findIn (Sources.switzerland (connectionOver two)) "CHE-105.909.036" ch

        match evidence.Value with
        | NotAvailable reason -> Assert.Contains("2 records", reason)
        | other -> failwith $"%A{other}"

        use none = new FakeHandler(fun _ -> ok (fixture "switzerland-none.xml"))

        let! evidence =
            findIn (Sources.switzerland (connectionOver none)) "CHE-100.000.006" ch

        match evidence.Value with
        | NotAvailable reason -> Assert.Contains("non-public", reason)
        | other -> failwith $"%A{other}"
    }

[<Theory>]
[<InlineData("3", "2", "1", "X AG", "Active")>]
[<InlineData("4", "2", "1", "X AG", "Active")>]
[<InlineData("3", "2", "1", "X AG in Liquidation", "WindingUp")>]
[<InlineData("3", "2", "1", "X SA en liquidation", "WindingUp")>]
[<InlineData("3", "2", "2", "X AG", "Closed")>]
[<InlineData("3", "2", "3", "X AG", "Registered")>]
[<InlineData("3", "3", "", "X", "Registered")>]
[<InlineData("1", "2", "1", "X AG", "Registered")>]
[<InlineData("2", "2", "1", "X AG", "Unknown")>]
[<InlineData("5", "2", "2", "X AG", "Closed")>]
[<InlineData("6", "", "", "X AG", "Closed")>]
[<InlineData("7", "", "", "X AG", "Closed")>]
[<InlineData("9", "2", "1", "X AG", "Unknown")>]
let ``every UID and commercial register status has a standing, and a new one is unknown``
    (uid: string, register: string, entry: string, name: string, expected: string)
    =
    let given (text: string) =
        if text = "" then ValueNone else ValueSome text

    Assert.StartsWith(expected, $"%A{Switzerland.standing (given uid) (given register) (given entry) name}")

[<Theory>]
[<InlineData("CHE-101.237.723")>]
[<InlineData("CHE101237723")>]
[<InlineData("che-101.237.723")>]
[<InlineData("101237723")>]
let ``a UID is read however it is written`` (number: string) =
    Assert.Equal(ValueSome "101237723", Switzerland.uidDigits number)

[<Theory>]
[<InlineData("CHE-101.237.724")>]
[<InlineData("CHE-12.345.678")>]
[<InlineData("abc")>]
[<InlineData(null)>]
let ``a value that can't be a UID isn't sent`` (number: string) : Task =
    task {
        use handler = nothingSent ()

        match! (Sources.switzerland (connectionOver handler)).TryFindAsync(number, ch, ct) with
        | Error(RegistryFailure.InvalidRequest _) -> Assert.Empty handler.Calls
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a SOAP fault for a bad request is refused, and one for the request limit is rate limiting`` () : Task =
    task {
        use refused = new FakeHandler(fun _ -> answer 500 (fixture "switzerland-fault.xml"))

        match! (Sources.switzerland (connectionOver refused)).TryFindAsync("CHE-101.237.723", ch, ct) with
        | Error(RegistryFailure.RequestRefused reason) -> Assert.Contains("Data_validation_failed", reason)
        | other -> failwith $"%A{other}"

        let limited =
            (fixture "switzerland-fault.xml").Replace(">Data_validation_failed<", ">Request_limit_exceeded<")

        use limit = new FakeHandler(fun _ -> answer 500 limited)

        match! (Sources.switzerland (connectionOver limit)).TryFindAsync("CHE-101.237.723", ch, ct) with
        | Error(RegistryFailure.RateLimited _) -> ()
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``after a request limit fault Switzerland sends nothing for the minute it holds`` () : Task =
    task {
        let limited =
            (fixture "switzerland-fault.xml").Replace(">Data_validation_failed<", ">Request_limit_exceeded<")

        use handler = new FakeHandler(fun _ -> answer 500 limited)
        let registry = Sources.switzerland (connectionOver handler)

        match! registry.TryFindAsync("CHE-101.237.723", ch, ct) with
        | Error(RegistryFailure.RateLimited(ValueSome wait)) -> Assert.Equal(TimeSpan.FromMinutes 1., wait)
        | other -> failwith $"%A{other}"

        match! registry.SearchAsync("Nestlé", ch, ct) with
        | Error(RegistryFailure.RateLimited(ValueSome wait)) -> Assert.True(wait > TimeSpan.Zero)
        | other -> failwith $"%A{other}"

        Assert.Single handler.Calls |> ignore
    }

[<Fact>]
let ``a Swiss UID in the answer that isn't nine digits is unreadable, not a crash`` () : Task =
    task {
        let short =
            (fixture "switzerland-company.xml").Replace("<uidOrganisationId>101237723<", "<uidOrganisationId>1012<")

        use handler = new FakeHandler(fun _ -> ok short)

        match! (Sources.switzerland (connectionOver handler)).TryFindAsync("CHE-101.237.723", ch, ct) with
        | Error(RegistryFailure.UnreadableAnswer _) -> ()
        | other -> failwith $"%A{other}"

        let shortSearch =
            (fixture "switzerland-search.xml").Replace("<uidOrganisationId>105909036<", "<uidOrganisationId>12<")

        use search = new FakeHandler(fun _ -> ok shortSearch)

        match! (Sources.switzerland (connectionOver search)).SearchAsync("Nestlé", ch, ct) with
        | Error(RegistryFailure.UnreadableAnswer _) -> ()
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``Switzerland's search reads every entry it answers`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "switzerland-search.xml"))

        match! (Sources.switzerland (connectionOver handler)).SearchAsync("Nestlé S.A.", ch, ct) with
        | Ok { Value = Available found } ->
            Assert.Equal(7, found.Matches.Length)
            Assert.Equal("CHE-105.909.036", found.Matches.Head.Number)
            Assert.True found.Complete
            Assert.EndsWith("/Search\"", soapHeader handler.Calls.Head)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``Switzerland keeps to the published twenty a minute`` () =
    use handler = nothingSent ()

    match (Sources.switzerland (connectionOver handler)).Coverage.RateLimit with
    | RateLimitPolicy.Published(20, per, _) -> Assert.Equal(TimeSpan.FromMinutes 1., per)
    | other -> failwith $"%A{other}"

// What these sources don't publish, and failing closed, the same for each.

[<Fact>]
let ``what a source doesn't publish is NotAvailable, and nothing is sent`` () : Task =
    task {
        use handler = nothingSent ()
        let connection = connectionOver handler

        for registry, number, jurisdiction in
            [
                Sources.norway connection, "923609016", no
                Sources.ireland connection, "104547", ie
                Sources.switzerland connection, "CHE-101.237.723", ch
            ] do
            match! registry.OwnersAsync(number, jurisdiction, OwnerPurpose.CustomerDueDiligence, ct) with
            | Ok { Value = NotAvailable _ } -> ()
            | other -> failwith $"%A{other}"

        for registry, number, jurisdiction in
            [
                Sources.ireland connection, "104547", ie
                Sources.switzerland connection, "CHE-101.237.723", ch
            ] do
            match! registry.OfficersAsync(number, jurisdiction, ct) with
            | Ok { Value = NotAvailable _ } -> ()
            | other -> failwith $"%A{other}"

        match! (Sources.poland connection).SearchAsync("x", pl, ct) with
        | Ok { Value = NotAvailable _ } -> ()
        | other -> failwith $"%A{other}"

        Assert.Empty handler.Calls
    }

[<Fact>]
let ``a record missing what identifies it is unreadable, on these sources`` () : Task =
    task {
        for registry, number, jurisdiction, body in
            [
                Sources.norway, "923609016", no, """{"navn":"X","konkurs":false}"""
                Sources.norway, "923609016", no, """{"organisasjonsnummer":"923609016"}"""
                Sources.czechia, "00177041", cz, """{"obchodniJmeno":"X"}"""
                Sources.czechia, "00177041", cz, """{"ico":"00177041"}"""
                Sources.ireland,
                "104547",
                ie,
                """{"success":true,"result":{"records":[{"company_num":104547}],"total":1}}"""
                Sources.ireland,
                "104547",
                ie,
                """{"success":true,"result":{"records":[{"company_name":"X"}],"total":1}}"""
                Sources.poland,
                "19193",
                pl,
                """{"odpis":{"naglowekA":{"numerKRS":"0000019193"},"dane":{"dzial1":{"danePodmiotu":{}}}}}"""
            ] do
            use handler = new FakeHandler(fun _ -> ok body)

            match! (registry (connectionOver handler)).TryFindAsync(number, jurisdiction, ct) with
            | Error(RegistryFailure.UnreadableAnswer _) -> ()
            | other -> failwith $"%A{other}"

        use slovakia =
            slovakiaHandler """{"results":[{"id":1}]}""" """{"id":1,"identifiers":[{"value":"35763469"}]}"""

        match! (Sources.slovakia (connectionOver slovakia)).TryFindAsync("35763469", sk, ct) with
        | Error(RegistryFailure.UnreadableAnswer _) -> ()
        | other -> failwith $"%A{other}"

        let nameless =
            (fixture "switzerland-company.xml")
                .Replace(
                    "<organisationName xmlns=\"http://www.ech.ch/xmlns/eCH-0097/5\">Nestlé Suisse S.A.</organisationName>",
                    ""
                )

        use switzerland = new FakeHandler(fun _ -> ok nameless)

        match! (Sources.switzerland (connectionOver switzerland)).TryFindAsync("CHE-101.237.723", ch, ct) with
        | Error(RegistryFailure.UnreadableAnswer _) -> ()
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``another entity's record isn't taken for the one asked`` () : Task =
    task {
        use norway = new FakeHandler(fun _ -> ok (fixture "norway-company.json"))

        match! (Sources.norway (connectionOver norway)).TryFindAsync("999999999", no, ct) with
        | Error(RegistryFailure.UnreadableAnswer _) -> ()
        | other -> failwith $"%A{other}"

        use switzerland = new FakeHandler(fun _ -> ok (fixture "switzerland-company.xml"))

        match! (Sources.switzerland (connectionOver switzerland)).TryFindAsync("CHE-105.909.036", ch, ct) with
        | Error(RegistryFailure.UnreadableAnswer _) -> ()
        | other -> failwith $"%A{other}"
    }

[<Theory; InlineData("not json"); InlineData(""); InlineData("<html>Service page</html>")>]
let ``an answer that can't be read is unreadable, on these sources`` (body: string) : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok body)

        for registry, number, jurisdiction in allRegistries handler do
            match! registry.TryFindAsync(number, jurisdiction, ct) with
            | Error(RegistryFailure.UnreadableAnswer reason) -> Assert.DoesNotContain("html", reason.ToLowerInvariant())
            | other -> failwith $"%s{registry.Coverage.Registry}: %A{other}"
    }

[<Fact>]
let ``JSON of the wrong shape is unreadable, on these sources`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok """{"unexpected":true}""")

        for registry, number, jurisdiction in allRegistries handler do
            match! registry.TryFindAsync(number, jurisdiction, ct) with
            | Error(RegistryFailure.UnreadableAnswer _) -> ()
            | other -> failwith $"%s{registry.Coverage.Registry}: %A{other}"
    }

[<Theory; InlineData(500); InlineData(503)>]
let ``a server failure is Unavailable, on these sources`` (status: int) : Task =
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
let ``a refusal or redirect is RequestRefused, on these sources`` (status: int) : Task =
    task {
        use handler = new FakeHandler(fun _ -> answer status "secret-body")

        for registry, number, jurisdiction in allRegistries handler do
            match! registry.TryFindAsync(number, jurisdiction, ct) with
            | Error(RegistryFailure.RequestRefused reason) -> Assert.DoesNotContain("secret-body", reason)
            | other -> failwith $"%s{registry.Coverage.Registry}: %A{other}"
    }

[<Fact>]
let ``outside their coverage these sources fail, and send nothing`` () : Task =
    task {
        use handler = nothingSent ()
        let elsewhere = Jurisdiction.ofCode "ZZ"

        for registry, number, _ in allRegistries handler do
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

[<Fact>]
let ``each of these sources is free and declares its pace and licence`` () =
    use handler = nothingSent ()

    for registry, _, _ in allRegistries handler do
        Assert.Equal(AccessCost.Free, registry.Coverage.Cost)
        Assert.NotEmpty registry.Coverage.Licence

    let licenceOf (registry: ICompanyRegistry) = registry.Coverage.Licence
    let connection = connectionOver handler
    Assert.Contains("NLOD 2.0", licenceOf (Sources.norway connection))
    Assert.Contains("500 a minute", licenceOf (Sources.czechia connection))
    Assert.Contains("CC BY 4.0", licenceOf (Sources.slovakia connection))
    Assert.Contains("CC BY 4.0", licenceOf (Sources.ireland connection))
    Assert.Contains("art. 24", licenceOf (Sources.poland connection))
