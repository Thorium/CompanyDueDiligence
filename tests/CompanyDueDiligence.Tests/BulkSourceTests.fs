/// The bulk sources, built from fixture files trimmed from the published files (Estonia 2026-11-07, Cyprus 2026-11-02, Romania 2026-11-04) with
/// people's names redacted, and from hand-built files following the documented layout (Belgium's cookbook, Slovenia's attribute names). Nothing
/// is called: a build reads streams only.
module BulkSourceTests

open System
open System.IO
open System.IO.Compression
open System.Text
open System.Threading.Tasks
open Xunit
open CompanyDueDiligence
open Fake

let private fixtureBytes (name: string) =
    File.ReadAllBytes(Path.Combine(AppContext.BaseDirectory, "Fixtures", name))

let private fixtureText (name: string) =
    File.ReadAllText(Path.Combine(AppContext.BaseDirectory, "Fixtures", name))

let private streamOf (bytes: byte array) = new MemoryStream(bytes) :> Stream
let private textStream (text: string) = streamOf (Encoding.UTF8.GetBytes text)
let private fixture name = streamOf (fixtureBytes name)

let private zipOf (entries: (string * byte array) list) =
    let buffer = new MemoryStream()

    do
        use archive = new ZipArchive(buffer, ZipArchiveMode.Create, true)

        for name, bytes in entries do
            use entry = archive.CreateEntry(name).Open()
            entry.Write(bytes, 0, bytes.Length)

    buffer.Position <- 0L
    buffer :> Stream

let private downloaded = DateTimeOffset(2026, 11, 8, 6, 0, 0, TimeSpan.Zero)

let private date =
    {
        DownloadedAt = downloaded
        AsOf = ValueSome(DateOnly(2026, 11, 7))
    }

let private ee = Jurisdiction.ofCode "EE"
let private cy = Jurisdiction.ofCode "CY"
let private ro = Jurisdiction.ofCode "RO"
let private si = Jurisdiction.ofCode "SI"
let private be = Jurisdiction.ofCode "BE"
let private gb = Jurisdiction.ofCode "GB"

let private built (result: Task<Result<ICompanyRegistry, RegistryFailure>>) =
    task {
        match! result with
        | Ok registry -> return registry
        | Error failure -> return failwith (RegistryFailure.describe failure)
    }

let private failed (result: Task<Result<ICompanyRegistry, RegistryFailure>>) =
    task {
        match! result with
        | Ok _ -> return failwith "The build read a file it should have refused"
        | Error failure -> return failure
    }

let private find (registry: ICompanyRegistry) (number: string) jurisdiction =
    task {
        match! registry.TryFindAsync(number, jurisdiction, ct) with
        | Ok evidence -> return evidence
        | Error failure -> return failwith (RegistryFailure.describe failure)
    }

let private companyOf (evidence: Evidence<Availability<Company voption>>) =
    match evidence.Value with
    | Available(ValueSome company) -> company
    | other -> failwith $"%A{other}"

let private officersOf (registry: ICompanyRegistry) number jurisdiction =
    task {
        match! registry.OfficersAsync(number, jurisdiction, ct) with
        | Ok evidence -> return evidence.Value
        | Error failure -> return failwith (RegistryFailure.describe failure)
    }

let private ownersOf (registry: ICompanyRegistry) number jurisdiction purpose =
    task {
        match! registry.OwnersAsync(number, jurisdiction, purpose, ct) with
        | Ok evidence -> return evidence.Value
        | Error failure -> return failwith (RegistryFailure.describe failure)
    }

let private isNotAvailable value =
    match value with
    | NotAvailable _ -> true
    | Available _
    | NeedsEntitlement _ -> false

let private invalid (registry: ICompanyRegistry) (number: string) jurisdiction =
    task {
        match! registry.TryFindAsync(number, jurisdiction, ct) with
        | Error(RegistryFailure.InvalidRequest _) -> ()
        | other -> failwith $"%A{other}"
    }

// Estonia

let private estoniaFiles () =
    zipOf [ "ettevotja_rekvisiidid__yldandmed.json", fixtureBytes "estonia-general.json" ],
    zipOf
        [
            "ettevotja_rekvisiidid__kaardile_kantud_isikud.json", fixtureBytes "estonia-persons.json"
        ],
    zipOf
        [
            "ettevotja_rekvisiidid__osanikud.json", fixtureBytes "estonia-shareholders.json"
        ],
    zipOf
        [
            "ettevotja_rekvisiidid__kasusaajad.json", fixtureBytes "estonia-beneficial-owners.json"
        ]

let private estonia () =
    let general, persons, shareholders, owners = estoniaFiles ()
    built (BulkSources.estoniaAsync general (ValueSome persons) (ValueSome shareholders) (ValueSome owners) date ct)

[<Fact>]
let ``Estonia reads a company from the general data zip as published`` () : Task =
    task {
        let! registry = estonia ()
        let! evidence = find registry "16752073" ee
        let company = companyOf evidence
        Assert.Equal("e-Business Register open data (Estonia)", evidence.Source)
        Assert.Equal(downloaded, evidence.RetrievedAt)
        Assert.Equal(ValueSome(DateOnly(2026, 11, 7)), evidence.AsOf)
        Assert.Equal("007 Agent & Partners OÜ", company.Name)
        Assert.Equal("16752073", company.Number)
        Assert.Equal(CompanyStanding.Registered(ValueSome "Registrisse kantud"), company.Standing)
        Assert.Equal(ValueSome "Osaühing", company.LegalForm)
        Assert.Equal(RecordRole.Home, company.Role)
        Assert.Equal(ValueSome(DateOnly(2023, 6, 5)), company.IncorporatedOn)

        match company.Addresses with
        | Available [ office ] ->
            Assert.Equal(AddressKind.RegisteredOffice, office.Kind)
            Assert.Equal<string list>([ "Regati pst 12" ], office.Address.Lines)
            Assert.Equal(ValueSome "11911", office.Address.PostalCode)
        | other -> failwith $"%A{other}"

        let raw = evidence.Raw.Value
        Assert.Contains("\"ariregistri_kood\":16752073", raw)
        Assert.DoesNotContain("kapitalid", raw)
    }

[<Fact>]
let ``Estonia reads the JSON itself when the host passes it unzipped`` () : Task =
    task {
        let! registry =
            built (BulkSources.estoniaAsync (fixture "estonia-general.json") ValueNone ValueNone ValueNone date ct)

        let! evidence = find registry "16752073" ee
        Assert.Equal("007 Agent & Partners OÜ", (companyOf evidence).Name)
    }

[<Fact>]
let ``an Estonian foreign company's branch is a foreign registration`` () : Task =
    task {
        let! registry = estonia ()
        let! evidence = find registry "17292225" ee
        Assert.Equal(RecordRole.ForeignRegistration, (companyOf evidence).Role)
    }

[<Fact>]
let ``an Estonian company in bankruptcy states its insolvency`` () : Task =
    task {
        let! registry = estonia ()
        let! bankrupt = find registry "11876557" ee
        let company = companyOf bankrupt
        Assert.Equal(CompanyStanding.InInsolvency "Pankrotis (bankrupt)", company.Standing)
        Assert.Equal<CompanyConcern list>([ CompanyConcern.InsolvencyHistory ], company.Concerns.Stated)
        let! liquidating = find registry "11979116" ee
        Assert.Equal(CompanyStanding.WindingUp "Likvideerimisel (in liquidation)", (companyOf liquidating).Standing)
    }

[<Fact>]
let ``a number missing from Estonia's files can't be told apart from a deleted company`` () : Task =
    task {
        let! registry = estonia ()
        let! evidence = find registry "10000000" ee
        Assert.True(isNotAvailable evidence.Value)
    }

[<Theory>]
[<InlineData("R", "Registered")>]
[<InlineData("L", "WindingUp")>]
[<InlineData("N", "InInsolvency")>]
[<InlineData("K", "Closed")>]
[<InlineData("X", "Unknown")>]
let ``every Estonian status code has a standing, and a new one is unknown`` (code: string, expected: string) =
    Assert.StartsWith(expected, $"%A{Estonia.standing (ValueSome code) ValueNone}")

[<Theory; InlineData("1234567"); InlineData("123456789"); InlineData("abc"); InlineData("")>]
let ``an Estonian registry code of the wrong shape isn't looked up`` (number: string) : Task =
    task {
        let! registry = estonia ()
        do! invalid registry number ee
    }

[<Fact>]
let ``Estonia reads persons on the registry card as officers`` () : Task =
    task {
        let! registry = estonia ()

        match! officersOf registry "11876557" ee with
        | Available [ board; trustee ] ->
            Assert.Equal("PERSON THREE", board.Name)
            Assert.Equal(OfficerRole.Director, board.Role)
            Assert.Equal(PartyKind.Individual, board.Kind)
            Assert.True board.Current
            Assert.Equal(OfficerRole.Other "Pankrotihaldur", trustee.Role)
        | other -> failwith $"%A{other}"

        match! officersOf registry "17292225" ee with
        | Available officers ->
            let entity =
                officers |> List.find (fun officer -> officer.Kind = PartyKind.LegalEntity)

            Assert.Equal("Dalanta OÜ", entity.Name)
            Assert.True entity.Entity.IsSome
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``Estonia lists shareholders and beneficial owners, with a board member named for want of an owner as a gap``
    ()
    : Task =
    task {
        let! registry = estonia ()

        match! ownersOf registry "11979116" ee OwnerPurpose.PublicRead with
        | Available listing ->
            let shareholders =
                listing.Owners |> List.filter (fun owner -> owner.Kind = PartyKind.LegalEntity)

            Assert.Equal(4, shareholders.Length)
            Assert.True(shareholders |> List.forall (fun owner -> owner.Entity.IsSome))
            Assert.Contains(OwnershipGap.OwnerNotIdentified, listing.Gaps)
        | other -> failwith $"%A{other}"

        match! ownersOf registry "16752073" ee OwnerPurpose.CustomerDueDiligence with
        | Available listing ->
            Assert.Equal(2, listing.Owners.Length)
            Assert.Empty listing.Gaps
            Assert.Equal(ControlKind.Ownership, listing.Owners.Head.Control.Head.Kind)
        | other -> failwith $"%A{other}"

        let! none = ownersOf registry "17292225" ee OwnerPurpose.PublicRead
        Assert.True(isNotAvailable none)
    }

[<Fact>]
let ``Estonia without the persons file answers officers NotAvailable and says so in its coverage`` () : Task =
    task {
        let general, _, _, _ = estoniaFiles ()

        let! registry =
            built (BulkSources.estoniaAsync general ValueNone ValueNone ValueNone date ct)

        let! officers = officersOf registry "16752073" ee
        Assert.True(isNotAvailable officers)
        Assert.StartsWith("NotOffered", $"%A{registry.Coverage.Officers}")
        let! owners = ownersOf registry "16752073" ee OwnerPurpose.PublicRead
        Assert.True(isNotAvailable owners)
    }

[<Theory; InlineData("not json"); InlineData("{\"ariregistri_kood\": 1}"); InlineData("[]")>]
let ``an unreadable Estonian file is refused`` (text: string) : Task =
    task {
        let! failure =
            failed (BulkSources.estoniaAsync (textStream text) ValueNone ValueNone ValueNone date ct)

        Assert.StartsWith("UnreadableAnswer", $"%A{failure}")
    }

[<Fact>]
let ``a zip without the JSON is refused`` () : Task =
    task {
        let zip = zipOf [ "readme.txt", "nothing"B ]

        let! failure =
            failed (BulkSources.estoniaAsync zip ValueNone ValueNone ValueNone date ct)

        Assert.StartsWith("UnreadableAnswer", $"%A{failure}")
    }

[<Fact>]
let ``an Estonian record without a name answers NotAvailable for its number`` () : Task =
    task {
        let text =
            """[{"ariregistri_kood":12345678,"nimi":null,"yldandmed":{"staatus":"R"}},{"ariregistri_kood":16752073,"nimi":"A OÜ","yldandmed":{"staatus":"R"}}]"""

        let! registry =
            built (BulkSources.estoniaAsync (textStream text) ValueNone ValueNone ValueNone date ct)

        let! unnamed = find registry "12345678" ee
        Assert.True(isNotAvailable unnamed.Value)
        let! named = find registry "16752073" ee
        Assert.Equal("A OÜ", (companyOf named).Name)
    }

// Cyprus

let private withoutMalformedRows (text: string) =
    text.Split '\n'
    |> Array.filter (fun line -> not (line.StartsWith "LAMAGAZINE" || line.StartsWith "B,"))
    |> String.concat "\n"

let private cyprusWith (organisations: Stream) =
    built (
        BulkSources.cyprusAsync
            organisations
            (ValueSome(fixture "cyprus-registered-office.csv"))
            (ValueSome(fixture "cyprus-officials.csv"))
            date
            ct
    )

let private cyprus () =
    cyprusWith (fixture "cyprus-organisations.csv")

[<Fact>]
let ``Cyprus reads a company with its registered office`` () : Task =
    task {
        let! registry = cyprus ()
        let! evidence = find registry "HE 400000" cy
        let company = companyOf evidence
        Assert.Equal("Registrar of Companies open data (Cyprus)", evidence.Source)
        Assert.Equal(ValueSome(DateOnly(2026, 11, 7)), evidence.AsOf)
        Assert.Equal("Α.Χ.Π. ΚΑΛΟΥΠΙΑ ΛΤΔ", company.Name)
        Assert.Equal("HE 400000", company.Number)
        Assert.Equal(CompanyStanding.Registered(ValueSome "Εγγεγραμμένη"), company.Standing)
        Assert.Equal(ValueSome "Εταιρεία, Ιδιωτική", company.LegalForm)
        Assert.Equal(ValueSome(DateOnly(2019, 7, 15)), company.IncorporatedOn)
        Assert.Equal(RecordRole.Home, company.Role)
        Assert.StartsWith("Α.Χ.Π. ΚΑΛΟΥΠΙΑ ΛΤΔ,400000,C,", evidence.Raw.Value)

        match company.Addresses with
        | Available [ office ] ->
            Assert.Equal<string list>([ "Γεωργίου Γρίβα Διγενή, 49" ], office.Address.Lines)
            Assert.Equal(ValueSome "Λιοπέτρι, 5320, Αμμόχωστος, Κύπρος", office.Address.Locality)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a Cypriot overseas company is a foreign registration, with the date it was struck off`` () : Task =
    task {
        let! registry = cyprus ()
        let! evidence = find registry "AE 1" cy
        let company = companyOf evidence
        Assert.Equal("CYPRUS MINES CORPORATION", company.Name)
        Assert.Equal(RecordRole.ForeignRegistration, company.Role)
        Assert.Equal(CompanyStanding.Closed "Struck off (Διαγραμμένη)", company.Standing)
        Assert.Equal(ValueSome(DateOnly(1974, 8, 16)), company.CeasedOn)
        let! sameNumber = find registry "HE 1" cy
        Assert.Equal("CYPRUS POPULAR BANK PUBLIC CO LTD", (companyOf sameNumber).Name)
        Assert.Equal<CompanyConcern list>([ CompanyConcern.InsolvencyHistory ], (companyOf sameNumber).Concerns.Stated)
    }

[<Theory>]
[<InlineData("Εγγεγραμμένη", "Registered")>]
[<InlineData("Στάληκε επιστολή Υπενθύμισης", "ActiveWithConcern")>]
[<InlineData("Δημοσιεύτηκε η Τρίμηνη", "ActiveWithConcern")>]
[<InlineData("Διαγραμμένη", "Closed")>]
[<InlineData("Διάλυση λόγω Ολοκλήρωσης Εκούσιας Εκκαθάρισης", "Dissolved")>]
[<InlineData("Διάλυση λόγω Ολοκλήρωσης Εκκαθάρισης", "Dissolved")>]
[<InlineData("Διαλύθηκε λόγω συγχώνευσης", "Closed")>]
[<InlineData("Αφαιρέθηκε από το Μητρώο ΕΕ με απόφαση Ανωτάτου Δικαστηρίου", "Closed")>]
[<InlineData("Εκούσια Εκκαθάριση από Μετόχους", "WindingUp")>]
[<InlineData("Υπο Εκκαθάριση", "WindingUp")>]
[<InlineData("Εκούσια Εκκαθάριση από Πιστωτές", "InInsolvency")>]
[<InlineData("Εκκαθάριση με Διάταγμα Δικαστηρίου", "InInsolvency")>]
[<InlineData("Εκκαθάριση με Διάταγμα Δικαστηρίου& Διορ.Ειδικού Διαχειριστή", "InInsolvency")>]
[<InlineData("Διορισμός Προσωρινού Εκκαθαριστή", "InInsolvency")>]
[<InlineData("Υπο Διαχείριση", "InInsolvency")>]
[<InlineData("Υπο Διαχείριση & Υπο Εκκαθάριση", "InInsolvency")>]
[<InlineData("Ευρωπαϊκή Εταιρεία", "Unknown")>]
[<InlineData("1418200", "Unknown")>]
[<InlineData("A word added later", "Unknown")>]
let ``every status word in Cyprus's file has a standing, and a new one is unknown`` (word: string, expected: string) =
    Assert.StartsWith(expected, $"%A{Cyprus.standing (ValueSome word)}")

[<Theory>]
[<InlineData("HE 12345", "HE 12345")>]
[<InlineData("he12345", "HE 12345")>]
[<InlineData("ΗΕ 012345", "HE 12345")>]
[<InlineData("C 5", "HE 5")>]
[<InlineData("AE 7", "AE 7")>]
[<InlineData("O7", "AE 7")>]
[<InlineData("B 13", "B 13")>]
[<InlineData("P 1", "P 1")>]
let ``a Cyprus number keeps its type`` (number: string, expected: string) =
    Assert.Equal(ValueSome expected, Cyprus.normalNumber number)

[<Theory; InlineData("12345"); InlineData("XY 1"); InlineData("HE 0"); InlineData("")>]
let ``a Cyprus number without a type isn't looked up`` (number: string) : Task =
    task {
        let! registry = cyprus ()
        do! invalid registry number cy
    }

[<Fact>]
let ``a number missing from Cyprus's file is not held, unless some rows couldn't be read`` () : Task =
    task {
        let! clean =
            cyprusWith (textStream (withoutMalformedRows (fixtureText "cyprus-organisations.csv")))

        let! notHeld = find clean "HE 999999" cy
        Assert.Equal(Available ValueNone, notHeld.Value)

        let! registry = cyprus ()
        let! cantTell = find registry "HE 999999" cy

        match cantTell.Value with
        | NotAvailable reason -> Assert.StartsWith("2 records", reason)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``Cyprus reads officials as officers`` () : Task =
    task {
        let! registry = cyprus ()

        match! officersOf registry "HE 400000" cy with
        | Available [ secretary; director ] ->
            Assert.Equal("PERSON TWO", secretary.Name)
            Assert.Equal(OfficerRole.Secretary, secretary.Role)
            Assert.Equal(OfficerRole.Director, director.Role)
        | other -> failwith $"%A{other}"

        let! none = officersOf registry "AE 1" cy
        Assert.True(isNotAvailable none)
    }

[<Fact>]
let ``Cyprus owners need an entitlement for due diligence and aren't published to anyone`` () : Task =
    task {
        let! registry = cyprus ()
        let! diligence = ownersOf registry "HE 400000" cy OwnerPurpose.CustomerDueDiligence
        Assert.StartsWith("NeedsEntitlement", $"%A{diligence}")
        let! publicRead = ownersOf registry "HE 400000" cy OwnerPurpose.PublicRead
        Assert.True(isNotAvailable publicRead)
    }

[<Fact>]
let ``a Cyprus number held by two rows answers NotAvailable`` () : Task =
    task {
        let text = fixtureText "cyprus-organisations.csv"
        let line = text.Split('\n') |> Array.find (fun line -> line.Contains ",400000,C,")
        let! registry = cyprusWith (textStream (text.TrimEnd() + "\n" + line))
        let! evidence = find registry "HE 400000" cy
        Assert.True(isNotAvailable evidence.Value)
    }

[<Theory; InlineData(""); InlineData("NAME,NUMBER\nA,1")>]
let ``an unreadable Cyprus file is refused`` (text: string) : Task =
    task {
        let! failure =
            failed (BulkSources.cyprusAsync (textStream text) ValueNone ValueNone date ct)

        Assert.StartsWith("UnreadableAnswer", $"%A{failure}")
    }

[<Fact>]
let ``a file opening with a byte order mark is still read as strict UTF-8`` () : Task =
    task {
        let bytes = fixtureBytes "cyprus-organisations.csv"
        Assert.Equal<byte array>([| 0xEFuy; 0xBBuy; 0xBFuy |], bytes[0..2])

        let! registry =
            built (BulkSources.cyprusAsync (streamOf bytes) ValueNone ValueNone date ct)

        let! evidence = find registry "HE 400000" cy
        Assert.Equal("Α.Χ.Π. ΚΑΛΟΥΠΙΑ ΛΤΔ", (companyOf evidence).Name)

        let at =
            Text.Encoding.UTF8.GetString(bytes).IndexOf("400000,C,", StringComparison.Ordinal)

        let offset =
            Text.Encoding.UTF8.GetByteCount(Text.Encoding.UTF8.GetString(bytes).Substring(0, at))

        let damaged = Array.concat [ bytes[.. offset - 1]; [| 0xFFuy |]; bytes[offset..] ]

        let! failure =
            failed (BulkSources.cyprusAsync (streamOf damaged) ValueNone ValueNone date ct)

        Assert.StartsWith("UnreadableAnswer", $"%A{failure}")
    }

// Romania

let private romaniaWith (firms: Stream) (states: Stream) =
    built (
        BulkSources.romaniaAsync
            firms
            states
            (ValueSome(fixture "romania-state-names.csv"))
            (ValueSome(fixture "romania-representatives.csv"))
            date
            ct
    )

let private romania () =
    romaniaWith (fixture "romania-firms.csv") (fixture "romania-states.csv")

[<Fact>]
let ``Romania reads a firm in operation`` () : Task =
    task {
        let! registry = romania ()
        let! evidence = find registry "J2019003266407" ro
        let company = companyOf evidence
        Assert.Equal("ONRC open data (Romania)", evidence.Source)
        Assert.Equal(ValueSome(DateOnly(2026, 11, 7)), evidence.AsOf)
        Assert.Equal("NEW CONT EXPERT S.R.L.", company.Name)
        Assert.Equal("J2019003266407", company.Number)
        Assert.Equal(CompanyStanding.Active, company.Standing)
        Assert.Equal(ValueSome "funcțiune", company.StatusAsPublished)
        Assert.Equal(ValueSome "SRL", company.LegalForm)
        Assert.Equal(ValueSome(DateOnly(2019, 3, 12)), company.IncorporatedOn)
        Assert.Equal(RecordRole.Home, company.Role)
        Assert.StartsWith("NEW CONT EXPERT S.R.L.^17416873^", evidence.Raw.Value)

        match company.Addresses with
        | Available [ office ] ->
            Assert.Equal(ValueSome "Bucureşti Sectorul 3", office.Address.Locality)
            Assert.Equal(ValueSome "31422", office.Address.PostalCode)
        | other -> failwith $"%A{other}"
    }

[<Theory; InlineData("17416873"); InlineData("RO17416873"); InlineData(" 17416873 ")>]
let ``a Romanian firm is found by its CUI`` (cui: string) : Task =
    task {
        let! registry = romania ()
        let! evidence = find registry cui ro
        Assert.Equal("J2019003266407", (companyOf evidence).Number)
    }

[<Fact>]
let ``an old Romanian registration number is found with or without leading zeros`` () : Task =
    task {
        let! registry = romania ()
        let! plain = find registry "J1/966/2017" ro
        let! padded = find registry "J01/966/2017" ro
        Assert.Equal((companyOf plain).Name, (companyOf padded).Name)
        Assert.Equal(CompanyStanding.WindingUp "dizolvare", (companyOf plain).Standing)
    }

[<Fact>]
let ``a Romanian branch of a foreign company is an establishment of a company in that country`` () : Task =
    task {
        let! registry = romania ()
        let! evidence = find registry "J40/10029/2018" ro
        let company = companyOf evidence
        Assert.Equal(RecordRole.Establishment, company.Role)
        Assert.Equal(ValueSome "Italia", company.HomeRegistration |> ValueOption.bind _.Country)
    }

[<Fact>]
let ``Romania's state codes decide the standing`` () : Task =
    task {
        let! registry = romania ()
        let! removed = find registry "J40/1116/1991" ro
        Assert.Equal(CompanyStanding.Closed "radiată (removed from the register)", (companyOf removed).Standing)
        let! concern = find registry "J23/2747/2012" ro
        Assert.Equal(CompanyStanding.ActiveWithConcern "sediu expirat", (companyOf concern).Standing)
        let! insolvent = find registry "J27/552/2001" ro
        Assert.Equal(CompanyStanding.InInsolvency "insolvență", (companyOf insolvent).Standing)
        Assert.Equal<CompanyConcern list>([ CompanyConcern.InsolvencyHistory ], (companyOf insolvent).Concerns.Stated)
        let! interrupted = find registry "J24/673/2015" ro

        Assert.Equal(
            CompanyStanding.Unknown(ValueSome "întrerupere temporară de activitate"),
            (companyOf interrupted).Standing
        )
    }

[<Fact>]
let ``every code in N_STARE_FIRMA is placed`` () =
    let codes =
        (fixtureText "romania-state-names.csv").Split '\n'
        |> Array.skip 1
        |> Array.choose (fun line ->
            match line.Split '^' with
            | [| code; _ |] -> Some(int (code.Trim()))
            | _ -> None)

    Assert.Equal(197, codes.Length)
    Assert.Empty(codes |> Array.filter (fun code -> (Romania.state code).IsNone))

[<Theory>]
[<InlineData("1048", "Active")>]
[<InlineData("1048,2065", "ActiveWithConcern")>]
[<InlineData("1112", "ActiveWithConcern")>]
[<InlineData("1048,1084", "Closed")>]
[<InlineData("1048,1070", "InInsolvency")>]
[<InlineData("1048,1139", "InInsolvency")>]
[<InlineData("1048,1052", "WindingUp")>]
[<InlineData("1081", "Unknown")>]
[<InlineData("1048,9999", "Unknown")>]
[<InlineData("2080", "Unknown")>]
[<InlineData("", "Unknown")>]
let ``a Romanian firm's codes give one standing`` (codes: string, expected: string) =
    let codes =
        codes.Split(',', StringSplitOptions.RemoveEmptyEntries)
        |> Array.map int
        |> List.ofArray

    Assert.StartsWith(expected, $"%A{Romania.standing codes}")

[<Theory; InlineData("J40/1116"); InlineData("abc"); InlineData("0"); InlineData("X40/1/2000"); InlineData("")>]
let ``a Romanian number that can't be one isn't looked up`` (number: string) : Task =
    task {
        let! registry = romania ()
        do! invalid registry number ro
    }

[<Fact>]
let ``a CUI missing from Romania's file is not held`` () : Task =
    task {
        let! registry = romania ()
        let! byCui = find registry "99999999" ro
        Assert.Equal(Available ValueNone, byCui.Value)
    }

// The file holds J2019003266407 and J40/1116/1991, each in one form only.
[<Theory>]
[<InlineData("J40/99999/2020")>]
[<InlineData("J40/3266/2019")>]
[<InlineData("J1991001116400")>]
let ``a Romanian registration number the file doesn't hold in that form can't be told absent`` (number: string) : Task =
    task {
        let! registry = romania ()
        let! found = find registry number ro

        match found.Value with
        | NotAvailable reason -> Assert.Contains("two forms", reason)
        | other -> failwith $"%A{other}"

        match! registry.OfficersAsync(number, ro, ct) with
        | Ok { Value = NotAvailable reason } -> Assert.Contains("two forms", reason)
        | other -> failwith $"%A{other}"

        match! registry.OwnersAsync(number, ro, OwnerPurpose.CustomerDueDiligence, ct) with
        | Ok { Value = NotAvailable reason } -> Assert.Contains("two forms", reason)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``Romania reads legal representatives as officers`` () : Task =
    task {
        let! registry = romania ()

        match! officersOf registry "J2019003266407" ro with
        | Available [ administrator ] ->
            Assert.Equal("PERSON ONE", administrator.Name)
            Assert.Equal(OfficerRole.Director, administrator.Role)
            Assert.Equal(ValueSome { Year = 1979; Month = 10 }, administrator.BornIn)
        | other -> failwith $"%A{other}"

        match! officersOf registry "J40/20659/2004" ro with
        | Available [ liquidator; representative ] ->
            Assert.Equal(OfficerRole.Other "lichidator", liquidator.Role)
            Assert.Equal(OfficerRole.Other "reprezentant al persoanei juridice", representative.Role)
        | other -> failwith $"%A{other}"

        let! none = officersOf registry "J1/966/2017" ro
        Assert.True(isNotAvailable none)
        let! owners = ownersOf registry "J1/966/2017" ro OwnerPurpose.PublicRead
        Assert.True(isNotAvailable owners)
    }

[<Fact>]
let ``a CUI or registration number held by two Romanian firms answers NotAvailable`` () : Task =
    task {
        let firms = fixtureText "romania-firms.csv"
        let header = (Array.head (firms.Split '\n')).TrimEnd '\r'

        let text =
            String.concat
                "\n"
                [
                    header
                    "FIRST SRL^555^J40/1/2001^01/01/2001^^SRL^România^^^^^^^^^^^^^"
                    "SECOND SRL^555^J40/2/2002^01/01/2002^^SRL^România^^^^^^^^^^^^^"
                    "THIRD SRL^556^J40/3/2003^01/01/2003^^SRL^România^^^^^^^^^^^^^"
                    "FOURTH SRL^557^J40/3/2003^01/01/2003^^SRL^România^^^^^^^^^^^^^"
                ]

        let! registry =
            romaniaWith (textStream text) (textStream "COD_INMATRICULARE^COD\nJ40/1/2001^1048")

        let! cui = find registry "555" ro
        Assert.True(isNotAvailable cui.Value)
        let! code = find registry "J40/3/2003" ro
        Assert.True(isNotAvailable code.Value)
        let! first = find registry "J40/1/2001" ro
        Assert.Equal("FIRST SRL", (companyOf first).Name)
    }

[<Fact>]
let ``a Romanian registration date followed by a time is read`` () : Task =
    task {
        let firms = fixtureText "romania-firms.csv"
        let header = (Array.head (firms.Split '\n')).TrimEnd '\r'

        let text =
            String.concat
                "\n"
                [
                    header
                    "NEW SRL^55549236^J2026012345402^07/09/2026 13:50:16^^SRL^România^^^^^^^^^^^^^"
                ]

        let! registry =
            romaniaWith (textStream text) (textStream "COD_INMATRICULARE^COD\nJ2026012345402^1048")

        let! found = find registry "55549236" ro
        Assert.Equal(ValueSome(DateOnly(2026, 9, 7)), (companyOf found).IncorporatedOn)
    }

[<Fact>]
let ``Romania searches the file by name`` () : Task =
    task {
        let! registry = romania ()

        match! registry.SearchAsync("expert", ro, ct) with
        | Ok { Value = Available result } ->
            Assert.Equal<string list>([ "J2019003266407" ], result.Matches |> List.map _.Number)
            Assert.True result.Complete
        | other -> failwith $"%A{other}"
    }

[<Theory; InlineData("COD_INMATRICULARE^COD\nJ40/1/2001^abc"); InlineData("NUMBER^STATE\nJ40/1/2001^1048")>]
let ``an unreadable Romanian state file is refused`` (states: string) : Task =
    task {
        let! failure =
            failed (
                BulkSources.romaniaAsync (fixture "romania-firms.csv") (textStream states) ValueNone ValueNone date ct
            )

        Assert.StartsWith("UnreadableAnswer", $"%A{failure}")
    }

// Slovenia

let private slovenia () =
    built (BulkSources.sloveniaAsync (fixture "slovenia-opsiprs.csv") date ct)

[<Fact>]
let ``Slovenia reads an entity from the PRS list, as registered with no word`` () : Task =
    task {
        let! registry = slovenia ()
        let! evidence = find registry "1234567" si
        let company = companyOf evidence
        Assert.Equal("AJPES PRS open data (Slovenia)", evidence.Source)
        Assert.Equal("PRIMER PODJETJE d.o.o.", company.Name)
        Assert.Equal("1234567000", company.Number)
        Assert.Equal(CompanyStanding.Registered ValueNone, company.Standing)
        Assert.Equal(ValueSome "Družba z omejeno odgovornostjo", company.LegalForm)
        Assert.Equal(RecordRole.NotStated, company.Role)
        Assert.StartsWith("1234567000;", evidence.Raw.Value)

        match company.Addresses with
        | Available [ address ] ->
            Assert.Equal<string list>([ "Slovenska cesta 10 A" ], address.Address.Lines)
            Assert.Equal(ValueSome "1000", address.Address.PostalCode)
        | other -> failwith $"%A{other}"

        let! quoted = find registry "7654321000" si
        Assert.Equal("PRIMER; DRUGO d.d.", (companyOf quoted).Name)
    }

[<Fact>]
let ``Slovenia can't tell a number it doesn't list, nor read one without a name`` () : Task =
    task {
        let! registry = slovenia ()
        let! missing = find registry "9999999000" si
        Assert.True(isNotAvailable missing.Value)
        let! unnamed = find registry "1111111000" si
        Assert.True(isNotAvailable unnamed.Value)
    }

[<Fact>]
let ``Slovenia offers no officers or owners`` () : Task =
    task {
        let! registry = slovenia ()
        let! officers = officersOf registry "1234567000" si
        Assert.True(isNotAvailable officers)
        let! owners = ownersOf registry "1234567000" si OwnerPurpose.CustomerDueDiligence
        Assert.True(isNotAvailable owners)
    }

[<Theory; InlineData("123456"); InlineData("12345678901"); InlineData("abc")>]
let ``a Slovenian registration number of the wrong shape isn't looked up`` (number: string) : Task =
    task {
        let! registry = slovenia ()
        do! invalid registry number si
    }

[<Fact>]
let ``Slovenia finds columns by the PRS attribute codes too`` () : Task =
    task {
        let text =
            "Ident_stevilka,Popolno_ime,Oblika\n5555555000,ENTITY BY CODE d.o.o.,d.o.o.\n"

        let! registry = built (BulkSources.sloveniaAsync (textStream text) date ct)
        let! evidence = find registry "5555555000" si
        Assert.Equal("ENTITY BY CODE d.o.o.", (companyOf evidence).Name)
    }

[<Fact>]
let ``a Slovenian file without a number column is refused, naming what it looked for`` () : Task =
    task {
        match! failed (BulkSources.sloveniaAsync (textStream "Id;Name\n1;A\n") date ct) with
        | RegistryFailure.UnreadableAnswer reason -> Assert.Contains("Matična številka", reason)
        | other -> failwith $"%A{other}"
    }

// Belgium

let private belgiumEntries () =
    [
        "meta.csv", fixtureBytes "belgium-meta.csv"
        "code.csv", fixtureBytes "belgium-code.csv"
        "enterprise.csv", fixtureBytes "belgium-enterprise.csv"
        "denomination.csv", fixtureBytes "belgium-denomination.csv"
        "address.csv", fixtureBytes "belgium-address.csv"
    ]

let private belgium () =
    built (
        BulkSources.belgiumAsync
            (zipOf (belgiumEntries ()))
            {
                DownloadedAt = downloaded
                AsOf = ValueNone
            }
            ct
    )

[<Fact>]
let ``Belgium reads an enterprise from the full zip, dated by its snapshot`` () : Task =
    task {
        let! registry = belgium ()
        let! evidence = find registry "0417.497.106" be
        let company = companyOf evidence
        Assert.Equal("CBE/KBO open data (Belgium)", evidence.Source)
        Assert.Equal(ValueSome(DateOnly(2026, 11, 7)), evidence.AsOf)
        Assert.Equal("EXAMPLE BREWING", company.Name)
        Assert.Equal<string list>([ "EXAMPLE BROUWERIJ"; "EXB" ], company.OtherNames)
        Assert.Equal("0417.497.106", company.Number)
        Assert.Equal(CompanyStanding.Active, company.Standing)
        Assert.Equal(ValueSome "Situation normale", company.StatusAsPublished)
        Assert.Equal(ValueSome "Société anonyme", company.LegalForm)
        Assert.Equal(ValueSome(DateOnly(1977, 8, 2)), company.IncorporatedOn)
        Assert.Equal(RecordRole.Home, company.Role)
        Assert.StartsWith("\"0417.497.106\",\"AC\"", evidence.Raw.Value)

        match company.Addresses with
        | Available [ office ] ->
            Assert.Equal(AddressKind.RegisteredOffice, office.Kind)
            Assert.Equal<string list>([ "Grand-Place 1" ], office.Address.Lines)
            Assert.Equal(ValueSome "Bruxelles", office.Address.Locality)
        | other -> failwith $"%A{other}"
    }

[<Theory>]
[<InlineData("0417497106")>]
[<InlineData("BE0417497106")>]
[<InlineData("417497106")>]
[<InlineData("0417 497 106")>]
let ``a Belgian enterprise number is read in its usual spellings`` (number: string) =
    Assert.Equal(ValueSome "0417.497.106", Belgium.normalNumber number)

[<Theory; InlineData("0417.497.107"); InlineData("2417.497.106"); InlineData("12345"); InlineData("")>]
let ``a Belgian number failing its check isn't looked up`` (number: string) : Task =
    task {
        let! registry = belgium ()
        do! invalid registry number be
    }

[<Fact>]
let ``a bankrupt Belgian enterprise with a struck-off office states both`` () : Task =
    task {
        let! registry = belgium ()
        let! evidence = find registry "0203.014.169" be
        let company = companyOf evidence
        Assert.Equal(CompanyStanding.InInsolvency "Bankruptcy opened (Ouverture de faillite)", company.Standing)
        Assert.Equal(ValueSome "Société à responsabilité limitée", company.LegalForm)

        Assert.Equal<CompanyConcern list>(
            [
                CompanyConcern.InsolvencyHistory
                CompanyConcern.RegisteredOfficeUndeliverable
            ],
            company.Concerns.Stated
        )
    }

[<Fact>]
let ``Belgium gives no registered address for a natural person, and a foreign entity is a foreign registration``
    ()
    : Task =
    task {
        let! registry = belgium ()
        let! person = find registry "0765.432.146" be
        Assert.True(isNotAvailable (companyOf person).Addresses)
        let! foreign = find registry "0123.456.749" be
        Assert.Equal(RecordRole.ForeignRegistration, (companyOf foreign).Role)
        let! liquidating = find registry "0876.543.270" be
        Assert.StartsWith("WindingUp", $"%A{(companyOf liquidating).Standing}")
    }

[<Fact>]
let ``Belgium answers NotAvailable for an enterprise without a name and for one not in the file`` () : Task =
    task {
        let! registry = belgium ()
        let! unnamed = find registry "0555.555.523" be
        Assert.True(isNotAvailable unnamed.Value)
        let! missing = find registry "0999.999.922" be
        Assert.True(isNotAvailable missing.Value)
    }

[<Theory>]
[<InlineData("AC", "000", "Active")>]
[<InlineData("AC", "001", "Active")>]
[<InlineData("AC", "002", "Active")>]
[<InlineData("AC", "003", "Active")>]
[<InlineData("AC", "006", "Closed")>]
[<InlineData("AC", "010", "WindingUp")>]
[<InlineData("AC", "011", "Closed")>]
[<InlineData("AC", "012", "WindingUp")>]
[<InlineData("AC", "013", "WindingUp")>]
[<InlineData("AC", "014", "Dissolved")>]
[<InlineData("AC", "015", "Closed")>]
[<InlineData("AC", "016", "Closed")>]
[<InlineData("AC", "017", "Closed")>]
[<InlineData("AC", "018", "Closed")>]
[<InlineData("AC", "019", "Closed")>]
[<InlineData("AC", "020", "Active")>]
[<InlineData("AC", "021", "Closed")>]
[<InlineData("AC", "022", "Closed")>]
[<InlineData("AC", "023", "Closed")>]
[<InlineData("AC", "024", "Closed")>]
[<InlineData("AC", "025", "Closed")>]
[<InlineData("AC", "026", "Closed")>]
[<InlineData("AC", "030", "InInsolvency")>]
[<InlineData("AC", "031", "InInsolvency")>]
[<InlineData("AC", "040", "InInsolvency")>]
[<InlineData("AC", "041", "InInsolvency")>]
[<InlineData("AC", "042", "ActiveWithConcern")>]
[<InlineData("AC", "043", "ActiveWithConcern")>]
[<InlineData("AC", "048", "InInsolvency")>]
[<InlineData("AC", "049", "InInsolvency")>]
[<InlineData("AC", "050", "InInsolvency")>]
[<InlineData("AC", "051", "Closed")>]
[<InlineData("AC", "052", "Closed")>]
[<InlineData("AC", "053", "Closed")>]
[<InlineData("AC", "090", "Active")>]
[<InlineData("AC", "091", "InInsolvency")>]
[<InlineData("AC", "100", "Registered")>]
[<InlineData("AC", "111", "Closed")>]
[<InlineData("AC", "112", "WindingUp")>]
[<InlineData("AC", "999", "Closed")>]
[<InlineData("AC", "777", "Unknown")>]
[<InlineData("AF", "000", "Closed")>]
[<InlineData("AN", "000", "Closed")>]
[<InlineData("ST", "000", "Closed")>]
[<InlineData("BK", "000", "Registered")>]
[<InlineData("JU", "000", "Registered")>]
[<InlineData("ZZ", "000", "Unknown")>]
let ``every Belgian status and juridical situation code has a standing, and a new one is unknown``
    (status: string, situation: string, expected: string)
    =
    Assert.StartsWith(expected, $"%A{Belgium.standing (ValueSome status) (ValueSome situation) ValueNone}")

[<Fact>]
let ``Belgium offers no officers, and owners need an entitlement`` () : Task =
    task {
        let! registry = belgium ()
        let! officers = officersOf registry "0417.497.106" be
        Assert.True(isNotAvailable officers)
        let! owners = ownersOf registry "0417.497.106" be OwnerPurpose.CustomerDueDiligence
        Assert.StartsWith("NeedsEntitlement", $"%A{owners}")
    }

[<Fact>]
let ``a Belgian zip without enterprise.csv, or a file that isn't a zip, is refused`` () : Task =
    task {
        let entries = belgiumEntries () |> List.filter (fst >> (<>) "enterprise.csv")
        let! missing = failed (BulkSources.belgiumAsync (zipOf entries) date ct)
        Assert.StartsWith("UnreadableAnswer", $"%A{missing}")

        let! notZip =
            failed (BulkSources.belgiumAsync (textStream "EnterpriseNumber\n") date ct)

        Assert.StartsWith("UnreadableAnswer", $"%A{notZip}")
    }

// Every bulk source

let private allBulk () =
    task {
        let! estonia = estonia ()
        let! cyprus = cyprus ()
        let! romania = romania ()
        let! slovenia = slovenia ()
        let! belgium = belgium ()

        return
            [
                estonia, "16752073"
                cyprus, "HE 400000"
                romania, "J2019003266407"
                slovenia, "1234567000"
                belgium, "0417.497.106"
            ]
    }

[<Fact>]
let ``every bulk source answers OutsideCoverage outside its country`` () : Task =
    task {
        let! registries = allBulk ()

        for registry, number in registries do
            match! registry.TryFindAsync(number, gb, ct) with
            | Error(RegistryFailure.OutsideCoverage _) -> ()
            | other -> failwith $"%s{registry.Coverage.Registry}: %A{other}"

            match! registry.OfficersAsync(number, gb, ct) with
            | Error(RegistryFailure.OutsideCoverage _) -> ()
            | other -> failwith $"%s{registry.Coverage.Registry}: %A{other}"

            match! registry.OwnersAsync(number, gb, OwnerPurpose.PublicRead, ct) with
            | Error(RegistryFailure.OutsideCoverage _) -> ()
            | other -> failwith $"%s{registry.Coverage.Registry}: %A{other}"

            match! registry.SearchAsync("A", gb, ct) with
            | Error(RegistryFailure.OutsideCoverage _) -> ()
            | other -> failwith $"%s{registry.Coverage.Registry}: %A{other}"
    }

[<Fact>]
let ``every bulk source refuses a blank search and states its licence`` () : Task =
    task {
        let! registries = allBulk ()

        for registry, _ in registries do
            let coverage = registry.Coverage
            Assert.NotEmpty coverage.Licence
            Assert.False(String.IsNullOrWhiteSpace coverage.Registry)

            match! registry.SearchAsync(" ", coverage.Jurisdictions.Head, ct) with
            | Error(RegistryFailure.InvalidRequest _) -> ()
            | other -> failwith $"%s{coverage.Registry}: %A{other}"
    }


// Fixes: fail-closed reading of the files

[<Fact>]
let ``Estonian beneficial owners without the shareholders file leave the direct owners unidentified`` () : Task =
    task {
        let general, _, _, owners = estoniaFiles ()

        let! registry =
            built (BulkSources.estoniaAsync general ValueNone ValueNone (ValueSome owners) date ct)

        match! ownersOf registry "16752073" ee OwnerPurpose.PublicRead with
        | Available listing ->
            Assert.NotEmpty listing.Owners
            Assert.Contains(OwnershipGap.OwnerNotIdentified, listing.Gaps)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``Estonian shareholders carry their percentage as a band, and an Estonian company owner its country code``
    ()
    : Task =
    task {
        let! registry = estonia ()

        match! ownersOf registry "16752073" ee OwnerPurpose.PublicRead with
        | Available listing -> Assert.Equal(ValueSome { From = 100; To = 100 }, listing.Owners.Head.Control.Head.Band)
        | other -> failwith $"%A{other}"

        match! ownersOf registry "11979116" ee OwnerPurpose.PublicRead with
        | Available listing ->
            let company =
                listing.Owners |> List.find (fun owner -> owner.Name = ValueSome "PP Invest OÜ")

            Assert.Equal(ValueSome { From = 5; To = 5 }, company.Control.Head.Band)
            Assert.Equal(ValueSome "EE", company.Entity |> ValueOption.bind _.Country)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a percentage between whole numbers is a band between them`` () =
    Assert.Equal(ValueSome { From = 23; To = 24 }, Estonia.percentBand (ValueSome "23.75"))
    Assert.Equal(ValueNone, Estonia.percentBand (ValueSome "abc"))
    Assert.Equal(ValueNone, Estonia.percentBand ValueNone)

let private romaniaRows (rows: string list) (states: string) =
    let firms = fixtureText "romania-firms.csv"
    let header = (Array.head (firms.Split '\n')).TrimEnd '\r'
    romaniaWith (textStream (String.concat "\n" (header :: rows))) (textStream states)

[<Fact>]
let ``a Romanian firm's CUI on a repeated or nameless row isn't read as not held`` () : Task =
    task {
        let! registry =
            romaniaRows
                [
                    "FIRST SRL^555^J40/1/2001^01/01/2001^^SRL^România^^^^^^^^^^^^^"
                    "THIRD SRL^556^J40/3/2003^01/01/2003^^SRL^România^^^^^^^^^^^^^"
                    "FOURTH SRL^557^J40/3/2003^01/01/2003^^SRL^România^^^^^^^^^^^^^"
                    "^558^J40/4/2004^01/01/2004^^SRL^România^^^^^^^^^^^^^"
                    "FIFTH SRL^559^J40/5/2005^01/01/2005^^SRL^România^^^^^^^^^^^^^"
                    "^560^J40/5/2005^01/01/2005^^SRL^România^^^^^^^^^^^^^"
                ]
                "COD_INMATRICULARE^COD\nJ40/1/2001^1048"

        for cui in [ "556"; "557"; "558"; "559"; "560" ] do
            let! evidence = find registry cui ro
            Assert.True(isNotAvailable evidence.Value, cui)

        // A nameless row for a number makes another row for it a repeat.
        let! fifth = find registry "J40/5/2005" ro
        Assert.True(isNotAvailable fifth.Value)
    }

[<Fact>]
let ``a Romanian state row that can't be read withholds its firm, and one naming no firm is stated in the coverage``
    ()
    : Task =
    task {
        let! registry =
            romaniaRows
                [
                    "FIRST SRL^555^J40/1/2001^01/01/2001^^SRL^România^^^^^^^^^^^^^"
                    "SECOND SRL^556^J40/2/2002^01/01/2002^^SRL^România^^^^^^^^^^^^^"
                ]
                "COD_INMATRICULARE^COD\nJ40/1/2001^1048\nJ40/1/2001^\nJ40/2/2002^1048\n^1084"

        let! first = find registry "J40/1/2001" ro
        Assert.True(isNotAvailable first.Value)
        let! second = find registry "J40/2/2002" ro
        Assert.Equal(CompanyStanding.Active, (companyOf second).Standing)
        Assert.StartsWith("Partial", $"%A{registry.Coverage.Standing}")
    }

[<Fact>]
let ``a Romanian search over a file with repeated numbers says it isn't complete`` () : Task =
    task {
        let! registry =
            romaniaRows
                [
                    "FIRST SRL^555^J40/1/2001^01/01/2001^^SRL^România^^^^^^^^^^^^^"
                    "SECOND SRL^556^J40/2/2002^01/01/2002^^SRL^România^^^^^^^^^^^^^"
                    "SECOND SRL^556^J40/2/2002^01/01/2002^^SRL^România^^^^^^^^^^^^^"
                ]
                "COD_INMATRICULARE^COD\nJ40/1/2001^1048"

        match! registry.SearchAsync("SRL", ro, ct) with
        | Ok { Value = Available result } ->
            Assert.Equal<string list>([ "J40/1/2001" ], result.Matches |> List.map _.Number)
            Assert.False result.Complete
        | other -> failwith $"%A{other}"
    }

// Files cut short

/// The text up to the end of its last record's first field, with no line end after it.
let private cutInLastRecord (delimiter: char) (text: string) =
    let whole = text.TrimEnd('\r', '\n')
    let lastRecord = whole.LastIndexOf '\n' + 1
    whole.Substring(0, whole.IndexOf(delimiter, lastRecord))

let private expectCutShort (build: Task<Result<ICompanyRegistry, RegistryFailure>>) =
    task {
        match! build with
        | Error(RegistryFailure.UnreadableAnswer reason) -> Assert.Contains("cut short", reason)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a Romanian file cut short in its last record is refused, whichever file it is`` () : Task =
    task {
        let build (firms: Stream) (states: Stream) (representatives: Stream) =
            BulkSources.romaniaAsync
                firms
                states
                (ValueSome(fixture "romania-state-names.csv"))
                (ValueSome representatives)
                date
                ct

        let cut name =
            textStream (cutInLastRecord '^' (fixtureText name))

        do!
            expectCutShort (
                build (cut "romania-firms.csv") (fixture "romania-states.csv") (fixture "romania-representatives.csv")
            )

        do!
            expectCutShort (
                build (fixture "romania-firms.csv") (cut "romania-states.csv") (fixture "romania-representatives.csv")
            )

        do!
            expectCutShort (
                build (fixture "romania-firms.csv") (fixture "romania-states.csv") (cut "romania-representatives.csv")
            )
    }

[<Fact>]
let ``a Cyprus organisations file cut short in its last record is refused`` () : Task =
    expectCutShort (
        BulkSources.cyprusAsync
            (textStream (cutInLastRecord ',' (fixtureText "cyprus-organisations.csv")))
            (ValueSome(fixture "cyprus-registered-office.csv"))
            (ValueSome(fixture "cyprus-officials.csv"))
            date
            ct
    )

[<Fact>]
let ``a short last record before a line end, or a whole one without, is read as the file gives it`` () : Task =
    task {
        let firm = "FIRST SRL^555^J40/1/2001^01/01/2001^^SRL^România^^^^^^^^^^^^^"

        // A whole last record with no line end after it.
        let! unended = romaniaRows [ firm ] "COD_INMATRICULARE^COD\nJ40/1/2001^1048"
        let! found = find unended "J40/1/2001" ro
        Assert.Equal("FIRST SRL", (companyOf found).Name)

        // A short last record with its line end: a ragged row, whose missing columns are absent.
        let! ragged =
            romaniaRows [ firm; "SECOND SRL^556^J40/2/2002\n" ] "COD_INMATRICULARE^COD\nJ40/1/2001^1048\n"

        let! short = find ragged "J40/2/2002" ro
        Assert.Equal("SECOND SRL", (companyOf short).Name)
    }

[<Theory>]
[<InlineData("a,b,c\n1,2,3\n4,5", true)>]
[<InlineData("a,b,c\n1,2,3\n4,5\n", false)>]
[<InlineData("a,b,c\n1,2,3\n4,5\r\n", false)>]
[<InlineData("a,b,c\n1,2,3\n4,5,6", false)>]
[<InlineData("a,b,c\n1,2\n4,5,6", false)>]
[<InlineData("a,b,c\n1,2,3\n\"4,5\",6", true)>]
[<InlineData("a,b,c\n1,2,3\n\"4\n5\",6,7", false)>]
[<InlineData("a,b,c\n1,2,3\n\"4\n5\",6", true)>]
[<InlineData("a,b,c\n1,2,3\n  ", false)>]
[<InlineData("a,b,c", false)>]
[<InlineData("", false)>]
let ``a delimited text is cut short only when its last record is short and has no line end`` (text: string, cut: bool) =
    let read () =
        BulkFile.wholeLines ',' true BulkFile.strictUtf8 (textStream text)
        |> BulkFile.records ',' true
        |> List.ofSeq

    if cut then
        match
            (try
                Ok(read ())
             with BulkFile.UnreadableFile reason ->
                 Error reason)
        with
        | Error reason -> Assert.Contains("cut short", reason)
        | Ok records -> failwith $"%A{records}"
    else
        read () |> ignore
