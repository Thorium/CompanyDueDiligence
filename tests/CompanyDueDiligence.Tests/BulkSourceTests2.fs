/// Bulk sources read from files the host passes in: Ukraine, Florida, Paraguay, Brazil, ASIC and Alaska. Fixtures are real rows with people's
/// names redacted, except Florida's and Brazil's, built by hand from the published layouts. Nothing is called.
module BulkSourceTests2

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

/// A zip holding the entries, as a publisher's file.
let private zipOf (entries: (string * byte array) list) : Stream =
    let memory = new MemoryStream()

    do
        use archive = new ZipArchive(memory, ZipArchiveMode.Create, true)

        for name, bytes in entries do
            use content = archive.CreateEntry(name).Open()
            content.Write(bytes, 0, bytes.Length)

    memory.Position <- 0L
    memory

let private plain (bytes: byte array) : Stream = new MemoryStream(bytes)
let private utf8 (text: string) = Encoding.UTF8.GetBytes text

let private date =
    {
        DownloadedAt = DateTimeOffset(2026, 11, 8, 9, 0, 0, TimeSpan.Zero)
        AsOf = ValueSome(DateOnly(2026, 11, 3))
    }

let private ua = Jurisdiction.ofCode "UA"
let private fl = Jurisdiction.ofCode "US-FL"
let private py = Jurisdiction.ofCode "PY"
let private br = Jurisdiction.ofCode "BR"
let private au = Jurisdiction.ofCode "AU"
let private ak = Jurisdiction.ofCode "US-AK"
let private gb = Jurisdiction.ofCode "GB"

let private built (result: Task<Result<ICompanyRegistry, RegistryFailure>>) =
    task {
        match! result with
        | Ok registry -> return registry
        | Error failure -> return failwith (RegistryFailure.describe failure)
    }

let private unreadable (result: Task<Result<ICompanyRegistry, RegistryFailure>>) =
    task {
        match! result with
        | Error(RegistryFailure.UnreadableAnswer _) -> ()
        | other -> failwith $"%A{other}"
    }

let private find (registry: ICompanyRegistry) (number: string) (jurisdiction: Jurisdiction) =
    task {
        match! registry.TryFindAsync(number, jurisdiction, ct) with
        | Ok evidence -> return evidence
        | Error failure -> return failwith (RegistryFailure.describe failure)
    }

let private companyIn (registry: ICompanyRegistry) (number: string) (jurisdiction: Jurisdiction) =
    task {
        let! evidence = find registry number jurisdiction

        return
            match evidence.Value with
            | Available(ValueSome company) -> company, evidence
            | other -> failwith $"%A{other}"
    }

let private officersOf (registry: ICompanyRegistry) (number: string) (jurisdiction: Jurisdiction) =
    task {
        match! registry.OfficersAsync(number, jurisdiction, ct) with
        | Ok evidence -> return evidence.Value
        | Error failure -> return failwith (RegistryFailure.describe failure)
    }

let private ownersOf (registry: ICompanyRegistry) (number: string) (jurisdiction: Jurisdiction) =
    task {
        match! registry.OwnersAsync(number, jurisdiction, OwnerPurpose.CustomerDueDiligence, ct) with
        | Ok evidence -> return evidence.Value
        | Error failure -> return failwith (RegistryFailure.describe failure)
    }

let private available (value: Availability<'T>) =
    match value with
    | Available held -> held
    | other -> failwith $"%A{other}"

let private isNotAvailable (value: Availability<'T>) =
    match value with
    | NotAvailable _ -> ()
    | other -> failwith $"%A{other}"

let private notSent (registry: ICompanyRegistry) (number: string) (jurisdiction: Jurisdiction) =
    task {
        match! registry.TryFindAsync(number, jurisdiction, ct) with
        | Error(RegistryFailure.InvalidRequest _) -> ()
        | other -> failwith $"%A{other}"
    }

let private outside (registry: ICompanyRegistry) (number: string) =
    task {
        match! registry.TryFindAsync(number, gb, ct) with
        | Error(RegistryFailure.OutsideCoverage _) -> ()
        | other -> failwith $"%A{other}"

        match! registry.OfficersAsync(number, gb, ct) with
        | Error(RegistryFailure.OutsideCoverage _) -> ()
        | other -> failwith $"%A{other}"
    }

// Ukraine

let private ukraine () =
    built (BulkSources.ukraineAsync (zipOf [ "UO.xml", fixtureBytes "ukraine-uo.xml" ]) date ct)

[<Fact>]
let ``Ukraine reads a registered company from the windows-1251 XML in uo.zip, with the host's date`` () : Task =
    task {
        let! registry = ukraine ()
        let! company, evidence = companyIn registry "34414228" ua
        Assert.Equal("Unified State Register of Ukraine (ЄДР) open data", evidence.Source)
        Assert.Equal(date.DownloadedAt, evidence.RetrievedAt)
        Assert.Equal(date.AsOf, evidence.AsOf)
        Assert.Equal("ТОВАРИСТВО З ОБМЕЖЕНОЮ ВІДПОВІДАЛЬНІСТЮ \"ОПТ-ТРЕЙД ЮНІОН\"", company.Name)
        Assert.Equal<string list>([ "ТОВ \"ОПТ-ТРЕЙД ЮНІОН\"" ], company.OtherNames)
        Assert.Equal(CompanyStanding.Registered(ValueSome "зареєстровано"), company.Standing)
        Assert.Equal(ValueSome "зареєстровано", company.StatusAsPublished)
        Assert.Equal(ValueSome "ТОВАРИСТВО З ОБМЕЖЕНОЮ ВІДПОВІДАЛЬНІСТЮ", company.LegalForm)
        Assert.Equal(ValueSome(DateOnly(2006, 6, 5)), company.IncorporatedOn)
        Assert.Equal(RecordRole.Home, company.Role)
        isNotAvailable company.Addresses
        Assert.Contains("<EDRPOU>34414228</EDRPOU>", evidence.Raw.Value)
        Assert.DoesNotContain("EXCHANGE_DATA", evidence.Raw.Value)
    }

[<Fact>]
let ``Ukraine reads signers, founders and beneficial owners from the register's text`` () : Task =
    task {
        let! registry = ukraine ()
        let! officers = officersOf registry "34414228" ua
        let signer = (available officers).Head
        Assert.Equal("PERSON ONE", signer.Name)
        Assert.Equal(OfficerRole.Other "керівник", signer.Role)
        Assert.Equal(PartyKind.Individual, signer.Kind)
        let! owners = ownersOf registry "34414228" ua
        let listing = available owners
        Assert.Equal(2, listing.Owners.Length)
        let founder = listing.Owners[0]
        Assert.Equal(ValueSome "PERSON ONE", founder.Name)
        Assert.Equal(PartyKind.Individual, founder.Kind)
        Assert.Equal(ValueSome "Україна", founder.Nationality)
        Assert.Equal("засновник (учасник); розмір частки - 36000,00 грн.", founder.Control.Head.AsPublished)
        let beneficiary = listing.Owners[1]
        Assert.Equal(ValueSome { From = 100; To = 100 }, beneficiary.Control.Head.Band)
        // A founder's share is in hryvnias, not a percentage, so the listing isn't complete.
        Assert.Equal<OwnershipGap list>([ OwnershipGap.OwnerDetailsNotConfirmed ], listing.Gaps)
    }

[<Fact>]
let ``a Ukrainian bankruptcy names the insolvency officer, and a legal entity founder carries its code`` () : Task =
    task {
        let! registry = ukraine ()
        let! company, _ = companyIn registry "32035139" ua
        Assert.Equal(CompanyStanding.InInsolvency "порушено справу про банкрутство", company.Standing)
        Assert.Equal<CompanyConcern list>([ CompanyConcern.InsolvencyHistory ], company.Concerns.Stated)
        let! officers = officersOf registry "32035139" ua
        let listed = available officers
        Assert.Equal(ValueSome(DateOnly(2025, 12, 9)), listed.Head.AppointedOn)
        Assert.Equal("PERSON TWO", listed.Head.Name)

        Assert.Contains(
            listed,
            fun officer ->
                officer.Name = "PERSON FOUR"
                && officer.Role = OfficerRole.Other "розпорядник майна"
        )

        Assert.DoesNotContain(listed, fun officer -> officer.Name = "PERSON THREE")
        let! owners = ownersOf registry "32035139" ua
        let listing = available owners
        let founder = listing.Owners.Head
        Assert.Equal(PartyKind.LegalEntity, founder.Kind)
        Assert.Equal(ValueSome "31899285", founder.Entity |> ValueOption.bind _.RegistrationNumber)
        Assert.Equal(ValueSome "UA", founder.Entity |> ValueOption.bind _.Country)

        match listing.Statements, listing.Gaps with
        | [ {
                Meaning = OwnerStatementMeaning.LeavesGap(OwnershipGap.StatementNotUnderstood _)
            } ],
          [ OwnershipGap.OwnerDetailsNotConfirmed; OwnershipGap.StatementNotUnderstood _ ] -> ()
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a Ukrainian company terminated by bankruptcy is closed, with its date and insolvency history`` () : Task =
    task {
        let! registry = ukraine ()
        let! company, _ = companyIn registry "30705428" ua
        Assert.Equal(CompanyStanding.Closed "припинено", company.Standing)
        Assert.Equal(ValueSome(DateOnly(2006, 12, 5)), company.CeasedOn)
        Assert.Equal<CompanyConcern list>([ CompanyConcern.InsolvencyHistory ], company.Concerns.Stated)
        let! officers = officersOf registry "30705428" ua
        isNotAvailable officers
        let! owners = ownersOf registry "30705428" ua
        isNotAvailable owners
    }

[<Fact>]
let ``a Ukrainian company being terminated is winding up, with its liquidator`` () : Task =
    task {
        let! registry = ukraine ()
        let! company, _ = companyIn registry "22788425" ua
        Assert.Equal(CompanyStanding.WindingUp "в стані припинення", company.Standing)
        let! officers = officersOf registry "22788425" ua
        let liquidator = (available officers).Head
        Assert.Equal("PERSON FIVE", liquidator.Name)
        Assert.Equal(OfficerRole.Other "голова комісії з припинення", liquidator.Role)
    }

[<Fact>]
let ``no person meeting the beneficial owner definition is no one to list, and board members are officers`` () : Task =
    task {
        let! registry = ukraine ()
        let! owners = ownersOf registry "24340640" ua
        let listing = available owners
        Assert.Empty listing.Owners

        Assert.Equal<OwnerStatementMeaning list>(
            [ OwnerStatementMeaning.NoOwnerToList ],
            listing.Statements |> List.map _.Meaning
        )

        Assert.Empty listing.Gaps
        let! officers = officersOf registry "39293285" ua

        Assert.Contains(
            available officers,
            fun officer -> officer.Name = "PERSON SEVEN" && officer.Role = OfficerRole.Other "Член Ради"
        )
    }

[<Fact>]
let ``two Ukrainian records on one code aren't told apart, and a record without a code is left out`` () : Task =
    task {
        let! registry = ukraine ()
        let! evidence = find registry "13577770" ua
        isNotAvailable evidence.Value
        let! officers = officersOf registry "13577770" ua
        isNotAvailable officers

        match! registry.SearchAsync("СЕДАН", ua, ct) with
        | Ok found -> Assert.Empty (available found.Value).Matches
        | Error failure -> failwith (RegistryFailure.describe failure)

        match! registry.SearchAsync("апельсин", ua, ct) with
        | Ok found -> Assert.Equal("24340640", (available found.Value).Matches.Head.Number)
        | Error failure -> failwith (RegistryFailure.describe failure)
    }

[<Fact>]
let ``a code not in uo.zip can still be held`` () : Task =
    task {
        let! registry = ukraine ()
        let! evidence = find registry "00032106" ua
        isNotAvailable evidence.Value
    }

[<Fact>]
let ``Ukraine reads the XML unzipped too`` () : Task =
    task {
        let! registry =
            built (BulkSources.ukraineAsync (plain (fixtureBytes "ukraine-uo.xml")) date ct)

        let! company, _ = companyIn registry "39293285" ua
        Assert.Equal("ГРОМАДСЬКА ОРГАНІЗАЦІЯ \"ВСЕУКРАЇНСЬКЕ ОБ'ЄДНАННЯ \"УКРАЇНСЬКІ РУБЕЖІ\"", company.Name)
    }

[<Theory>]
[<InlineData("зареєстровано", "Registered")>]
[<InlineData("зареєстровано, свідоцтво про державну реєстрацію недійсне", "Registered")>]
[<InlineData("в стані припинення", "WindingUp")>]
[<InlineData("порушено справу про банкрутство", "InInsolvency")>]
[<InlineData("порушено справу про банкрутство (санація)", "InInsolvency")>]
[<InlineData("припинено", "Closed")>]
[<InlineData("скасовано", "Closed")>]
[<InlineData("новий стан", "Unknown")>]
let ``every Ukrainian state has a standing, and a new one is unknown`` (state: string, expected: string) =
    let standing = Ukraine.standing state
    Assert.StartsWith(expected, $"%A{standing}".Replace("CompanyStanding.", ""))
    Assert.False(CompanyStanding.isActive standing)

[<Theory; InlineData("3441422"); InlineData("344142281"); InlineData("ABCDEFGH")>]
let ``a value that can't be an EDRPOU code isn't looked up`` (number: string) : Task =
    task {
        let! registry = ukraine ()
        do! notSent registry number ua
    }

[<Fact>]
let ``Ukraine answers outside its coverage with nothing read`` () : Task =
    task {
        let! registry = ukraine ()
        do! outside registry "34414228"
    }

[<Fact>]
let ``a Ukrainian file that isn't XML, or a record without a name, is unreadable`` () : Task =
    task {
        do! unreadable (BulkSources.ukraineAsync (zipOf [ "UO.xml", utf8 "not xml <<" ]) date ct)

        let nameless =
            "<?xml version=\"1.0\" encoding=\"UTF-8\"?><DATA><SUBJECT><RECORD>1</RECORD><EDRPOU>34414228</EDRPOU><STAN>зареєстровано</STAN></SUBJECT></DATA>"

        do! unreadable (BulkSources.ukraineAsync (plain (utf8 nameless)) date ct)
        do! unreadable (BulkSources.ukraineAsync (plain [| 0x50uy; 0x4Buy; 3uy; 4uy; 0uy; 1uy |]) date ct)
    }

// Florida

let private florida () =
    built (
        BulkSources.floridaAsync
            [
                zipOf [ "cordata0.txt", fixtureBytes "florida-cordata.txt" ]
                plain (fixtureBytes "florida-daily.txt")
            ]
            date
            ct
    )

[<Fact>]
let ``Florida reads a fixed-width record, its addresses and the host's date`` () : Task =
    task {
        let! registry = florida ()
        let! company, evidence = companyIn registry "L20000012345" fl
        Assert.Equal("Florida Division of Corporations data file", evidence.Source)
        Assert.Equal(date.AsOf, evidence.AsOf)
        Assert.Equal("SUNSHINE COAST HOLDINGS LLC", company.Name)
        Assert.Equal(CompanyStanding.Active, company.Standing)
        Assert.Equal(RecordRole.Home, company.Role)
        Assert.Equal(ValueSome "Florida Limited Liability Co.", company.LegalForm)
        Assert.Equal(ValueSome(DateOnly(2020, 1, 15)), company.IncorporatedOn)
        let addresses = available company.Addresses

        Assert.Equal<AddressKind list>(
            [ AddressKind.PrincipalOffice; AddressKind.Postal; AddressKind.RegisteredAgent ],
            addresses |> List.map _.Kind
        )

        Assert.Equal<string list>([ "100 MAIN STREET"; "SUITE 200" ], addresses.Head.Address.Lines)
        Assert.Equal(ValueSome "33101", addresses.Head.Address.PostalCode)
        Assert.StartsWith("L20000012345", evidence.Raw.Value)
    }

[<Fact>]
let ``Florida reads officers by title code, and the registered agent`` () : Task =
    task {
        let! registry = florida ()
        let! officers = officersOf registry "l20000012345" fl
        let listed = available officers

        Assert.Equal<OfficerRole list>(
            [
                OfficerRole.Other "President"
                OfficerRole.Other "Vice President, Director"
                OfficerRole.Other "AMBR"
                OfficerRole.Other "Registered agent"
            ],
            listed |> List.map _.Role
        )

        Assert.Equal(PartyKind.LegalEntity, listed[2].Kind)
        Assert.Equal(ValueSome "TAMPA", listed[2].Address |> ValueOption.bind _.Locality)
        let! owners = ownersOf registry "L20000012345" fl
        isNotAvailable owners
    }

[<Fact>]
let ``a Florida foreign filing is a foreign registration, with its home state`` () : Task =
    task {
        let! registry = florida ()
        let! company, _ = companyIn registry "F19000004567" fl
        Assert.Equal(RecordRole.ForeignRegistration, company.Role)
        Assert.Equal(CompanyStanding.Closed "Inactive", company.Standing)
        Assert.Equal(ValueSome "DE", company.HomeRegistration |> ValueOption.bind _.Country)
        // The record's flag says it has more than six officers, so the two it lists aren't the whole list.
        let! officers = officersOf registry "F19000004567" fl

        match officers with
        | NotAvailable reason -> Assert.Contains("more than six officers", reason)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a later Florida file replaces an earlier one's record, and absence proves nothing`` () : Task =
    task {
        let! registry = florida ()
        let! company, _ = companyIn registry "N99000000777" fl
        Assert.Equal(CompanyStanding.Closed "Inactive", company.Standing)
        isNotAvailable company.Addresses
        let! evidence = find registry "P98000099999" fl
        isNotAvailable evidence.Value
    }

[<Theory; InlineData("A", "Active"); InlineData("I", "Closed"); InlineData("X", "Unknown")>]
let ``every Florida status has a standing, and a new one is unknown`` (status: string, expected: string) =
    Assert.StartsWith(expected, $"%A{Florida.standing status}".Replace("CompanyStanding.", ""))

[<Theory>]
[<InlineData("DOMP", "Home")>]
[<InlineData("DOMNP", "Home")>]
[<InlineData("DOMLP", "Home")>]
[<InlineData("FLAL", "Home")>]
[<InlineData("FORP", "ForeignRegistration")>]
[<InlineData("FORNP", "ForeignRegistration")>]
[<InlineData("FORLP", "ForeignRegistration")>]
[<InlineData("FORL", "ForeignRegistration")>]
[<InlineData("NPREG", "NotStated")>]
[<InlineData("TRUST", "NotStated")>]
[<InlineData("AGENT", "NotStated")>]
let ``a Florida filing type says whether it is a home record or a foreign registration``
    (filingType: string, expected: string)
    =
    Assert.Equal(expected, $"%A{Florida.role filingType}".Replace("RecordRole.", ""))

[<Theory; InlineData("ABC"); InlineData("L2000001234567"); InlineData("")>]
let ``a value that can't be a Florida document number isn't looked up`` (number: string) : Task =
    task {
        let! registry = florida ()
        do! notSent registry number fl
    }

[<Fact>]
let ``Florida answers outside its coverage with nothing read`` () : Task =
    task {
        let! registry = florida ()
        do! outside registry "L20000012345"
    }

[<Fact>]
let ``a Florida line past 1440 characters, or a record without a name, is unreadable`` () : Task =
    task {
        do! unreadable (BulkSources.floridaAsync [ plain (utf8 (String('X', 1441))) ] date ct)
        do! unreadable (BulkSources.floridaAsync [ plain (utf8 "L20000012345") ] date ct)
    }

// Paraguay

let private paraguay (files: Stream list) =
    built (BulkSources.paraguayAsync files date ct)

let private ruc0 () =
    zipOf [ "ruc0.txt", fixtureBytes "paraguay-ruc0.txt" ]

[<Fact>]
let ``Paraguay reads a row, by RUC with or without its check digit`` () : Task =
    task {
        let! registry = paraguay [ ruc0 () ]
        let! company, evidence = companyIn registry "80000170-2" py
        Assert.Equal("DNIT RUC list (Paraguay)", evidence.Source)
        Assert.Equal(date.AsOf, evidence.AsOf)
        Assert.Equal("PARAPORT S.R.L.", company.Name)
        Assert.Equal("80000170-2", company.Number)
        Assert.Equal(CompanyStanding.Registered(ValueSome "Activo (tax registration)"), company.Standing)
        Assert.Equal(RecordRole.NotStated, company.Role)
        Assert.Equal(ValueSome "80000170|PARAPORT S.R.L.|2|PARB728471M|ACTIVO|", evidence.Raw)
        let! again, _ = companyIn registry "80000170" py
        Assert.Equal(company, again)
        let! person, _ = companyIn registry "1023860a-1" py
        Assert.Equal(CompanyStanding.Unknown(ValueSome "Suspensión temporal (tax registration)"), person.Standing)
    }

[<Fact>]
let ``with one of the ten Paraguayan files, a RUC not in it can still be held`` () : Task =
    task {
        let! registry = paraguay [ ruc0 () ]
        let! evidence = find registry "80000180" py

        match evidence.Value with
        | NotAvailable reason -> Assert.Contains("ruc9.zip", reason)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``with all ten Paraguayan files, a RUC not in them isn't held`` () : Task =
    task {
        let others =
            [ 1..9 ]
            |> List.map (fun digit ->
                let ruc = $"8000000{digit}"

                zipOf
                    [
                        $"ruc{digit}.txt", utf8 $"{ruc}|EMPRESA {digit} SA|{Paraguay.checkDigit ruc}||ACTIVO|\n"
                    ])

        let! registry = paraguay (ruc0 () :: others)
        let! evidence = find registry "80000180" py
        Assert.Equal(Available ValueNone, evidence.Value)
        let! company, _ = companyIn registry "80000005" py
        Assert.Equal("EMPRESA 5 SA", company.Name)
    }

[<Theory>]
[<InlineData("80000170", 2)>]
[<InlineData("80000030", 7)>]
[<InlineData("1023860A", 1)>]
[<InlineData("1529440", 4)>]
let ``the Paraguayan check digit matches the list`` (ruc: string, digit: int) =
    Assert.Equal(digit, Paraguay.checkDigit ruc)

[<Theory>]
[<InlineData("ACTIVO", "Registered")>]
[<InlineData("SUSPENSION TEMPORAL", "Unknown")>]
[<InlineData("BLOQUEADO", "Unknown")>]
[<InlineData("CANCELADO", "Closed")>]
[<InlineData("CANCELADO DEFINITIVO", "Closed")>]
[<InlineData("INACTIVO", "Unknown")>]
let ``every Paraguayan status has a standing, and a new one is unknown`` (status: string, expected: string) =
    Assert.StartsWith(expected, $"%A{Paraguay.standing status}".Replace("CompanyStanding.", ""))

[<Theory; InlineData("80000170-3"); InlineData("123456789"); InlineData("ABC")>]
let ``a RUC of the wrong shape or check digit isn't looked up`` (number: string) : Task =
    task {
        let! registry = paraguay [ ruc0 () ]
        do! notSent registry number py
    }

[<Fact>]
let ``Paraguay lists no officers or owners, and answers outside its coverage with nothing read`` () : Task =
    task {
        let! registry = paraguay [ ruc0 () ]
        let! officers = officersOf registry "80000170" py
        isNotAvailable officers
        let! owners = ownersOf registry "80000170" py
        isNotAvailable owners
        do! outside registry "80000170"
    }

[<Fact>]
let ``files with no Paraguayan row that can be read are refused`` () : Task =
    task {
        do! unreadable (BulkSources.paraguayAsync [ plain (utf8 "80000170|PARAPORT S.R.L.|2\n") ] date ct)
        do! unreadable (BulkSources.paraguayAsync [ plain (utf8 "80000170|PARAPORT S.R.L.|3||ACTIVO|\n") ] date ct)
        do! unreadable (BulkSources.paraguayAsync [ plain (utf8 "80000170||2||ACTIVO|\n") ] date ct)
    }

/// All ten files, each with one readable row, and the extra rows given.
let private paraguayWith (extra: string) =
    List.init 10 (fun digit ->
        let ruc = $"8000000{digit}"
        let rows = $"{ruc}|EMPRESA {digit} SA|{Paraguay.checkDigit ruc}||ACTIVO|\n"
        zipOf [ $"ruc{digit}.txt", utf8 (if digit = 1 then rows + extra else rows) ])
    |> paraguay

[<Fact>]
let ``a Paraguayan row with a stray pipe answers NotAvailable for its RUC, and the rest still read`` () : Task =
    task {
        // Rows of the real ruc1.txt of 2026-11-08: a company name followed by an empty field, and a person's name with a pipe for a letter.
        let! registry =
            paraguayWith
                "80099501|ROAL GROUP SOCIEDAD ANONIMA||5|RGRA177150M|ACTIVO|\n238851|M|LLER, PERSON ONE|0|XXXX410800I|CANCELADO|\n"

        let! company = find registry "80099501" py
        isNotAvailable company.Value
        let! person = find registry "238851" py
        isNotAvailable person.Value
        let! other, _ = companyIn registry "80000001" py
        Assert.Equal("EMPRESA 1 SA", other.Name)
        // Both rows name their RUC, so a RUC in no file is still not held.
        let! absent = find registry "80000180" py
        Assert.Equal(Available ValueNone, absent.Value)

        match! registry.SearchAsync("EMPRESA", py, ct) with
        | Ok { Value = Available result } -> Assert.False result.Complete
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a Paraguayan row whose check digit doesn't match leaves a RUC not found unsettled`` () : Task =
    task {
        // A row of the real ruc5.txt of 2026-11-08, name redacted: the RUC or its check digit is wrong, so the right RUC can't be told.
        let! registry = paraguayWith "80045405|PERSON, ONE EIRL|1|XXXX9451402|CANCELADO|\n"
        let! absent = find registry "80000180" py
        isNotAvailable absent.Value
        let! other, _ = companyIn registry "80000001" py
        Assert.Equal("EMPRESA 1 SA", other.Name)
    }

[<Fact>]
let ``a RUC on an unreadable Paraguayan row isn't answered by another row for it`` () : Task =
    task {
        let digit = Paraguay.checkDigit "80000011"

        let! registry =
            paraguayWith $"80000011|OTHER SA|{digit}||ACTIVO|\n80000011|OTHER|SA|{digit}||ACTIVO|\n"

        let! held = find registry "80000011" py
        isNotAvailable held.Value
    }

// Brazil

let private brazil () =
    built (
        BulkSources.brazilAsync
            [
                zipOf [ "K3241.K03200Y9.D61010.EMPRECSV", fixtureBytes "brazil-empresas.csv" ]
            ]
            [
                zipOf [ "K3241.K03200Y9.D61010.ESTABELE", fixtureBytes "brazil-estabelecimentos.csv" ]
            ]
            [ zipOf [ "K3241.K03200Y9.D61010.SOCIOCSV", fixtureBytes "brazil-socios.csv" ] ]
            date
            ct
    )

[<Fact>]
let ``Brazil reads a head office with its company's name, nature and address`` () : Task =
    task {
        let! registry = brazil ()
        let! company, evidence = companyIn registry "11.222.333/0001-81" br
        Assert.Equal("Receita Federal CNPJ open data (Brazil)", evidence.Source)
        Assert.Equal(date.AsOf, evidence.AsOf)
        Assert.Equal("EXEMPLO COMÉRCIO DE ALIMENTOS LTDA", company.Name)
        Assert.Equal("11222333000181", company.Number)
        Assert.Equal<string list>([ "MERCADO EXEMPLO" ], company.OtherNames)
        Assert.Equal(CompanyStanding.Active, company.Standing)
        Assert.Equal(RecordRole.Home, company.Role)
        Assert.Equal(ValueSome "Sociedade Empresária Limitada", company.LegalForm)
        Assert.Equal(ValueSome(DateOnly(2015, 3, 10)), company.IncorporatedOn)
        let address = (available company.Addresses).Head
        Assert.Equal(AddressKind.PrincipalOffice, address.Kind)
        Assert.Equal(ValueSome "SP", address.Address.Region)
        Assert.Equal(ValueSome "01001000", address.Address.PostalCode)
        Assert.Equal("RUA DAS FLORES 245", address.Address.Lines.Head)
        Assert.Contains("HOLDING EXEMPLO", evidence.Raw.Value)
        let! byBase, _ = companyIn registry "11222333" br
        Assert.Equal(company, byBase)
    }

[<Fact>]
let ``a Brazilian branch is an establishment of its head office, and a closed one has its date`` () : Task =
    task {
        let! registry = brazil ()
        let! company, _ = companyIn registry "11222333000262" br
        Assert.Equal(RecordRole.Establishment, company.Role)
        Assert.Equal(ValueSome "11222333000181", company.EstablishmentOf)
        Assert.Equal(CompanyStanding.Closed "Baixada", company.Standing)
        Assert.Equal(ValueSome(DateOnly(2023, 11, 30)), company.CeasedOn)
    }

[<Fact>]
let ``Brazil reads administrators and partners from the QSA`` () : Task =
    task {
        let! registry = brazil ()
        let! officers = officersOf registry "11222333000181" br
        let listed = available officers

        Assert.Equal<(string * OfficerRole) list>(
            [
                "PERSON ONE", OfficerRole.Other "Sócio-Administrador"
                "PERSON FOUR", OfficerRole.Other "Administrador"
            ],
            listed |> List.map (fun officer -> officer.Name, officer.Role)
        )

        Assert.Equal(ValueSome(DateOnly(2015, 3, 10)), listed.Head.AppointedOn)
        let! owners = ownersOf registry "11222333000181" br
        let listing = available owners

        Assert.Equal<string voption list>(
            [
                ValueSome "PERSON ONE"
                ValueSome "HOLDING EXEMPLO PARTICIPACOES S/A"
                ValueSome "PERSON THREE"
            ],
            listing.Owners |> List.map _.Name
        )

        Assert.Equal<PartyKind list>(
            [ PartyKind.Individual; PartyKind.LegalEntity; PartyKind.Individual ],
            listing.Owners |> List.map _.Kind
        )

        Assert.Equal(ValueSome "67279839000150", listing.Owners[1].Entity |> ValueOption.bind _.RegistrationNumber)
        Assert.Equal(ValueSome "BR", listing.Owners[1].Entity |> ValueOption.bind _.Country)
        // The QSA gives no shares, and one Socios part of ten was passed.
        Assert.Equal<OwnershipGap list>(
            [ OwnershipGap.OwnerDetailsNotConfirmed; OwnershipGap.OwnerNotIdentified ],
            listing.Gaps
        )
    }

[<Fact>]
let ``a Brazilian company domiciled abroad is a foreign registration, and its foreign partner can't be followed``
    ()
    : Task =
    task {
        let! registry = brazil ()
        let! company, _ = companyIn registry "07453393000138" br
        Assert.Equal(RecordRole.ForeignRegistration, company.Role)
        let address = (available company.Addresses).Head.Address
        Assert.Equal(ValueSome "ZURICH", address.Locality)
        Assert.Equal(ValueNone, address.Country)
        let! owners = ownersOf registry "07453393000138" br
        let listing = available owners
        Assert.Equal(PartyKind.LegalEntity, listing.Owners.Head.Kind)

        Assert.Equal<OwnershipGap list>(
            [
                OwnershipGap.OwnerDetailsNotConfirmed
                OwnershipGap.OwnerNotIdentified
                OwnershipGap.EntityNotIdentified
            ],
            listing.Gaps
        )
    }

[<Fact>]
let ``a bankrupt Brazilian company is in insolvency, and one unfit for missing declarations has them overdue``
    ()
    : Task =
    task {
        let! registry = brazil ()
        let! bankrupt, _ = companyIn registry "44555666000181" br
        Assert.Equal(CompanyStanding.InInsolvency "FALIDO", bankrupt.Standing)
        Assert.Equal<CompanyConcern list>([ CompanyConcern.InsolvencyHistory ], bankrupt.Concerns.Stated)
        let! unfit, _ = companyIn registry "77888999000181" br
        Assert.Equal(CompanyStanding.Registered(ValueSome "Inapta"), unfit.Standing)
        Assert.Equal(ValueSome "INAPTA; OMISSAO DE DECLARACOES", unfit.StatusAsPublished)
        Assert.Equal<CompanyConcern list>([ CompanyConcern.AnnualFilingOverdue ], unfit.Concerns.Stated)
        let! officers = officersOf registry "77888999000181" br
        isNotAvailable officers
        let! owners = ownersOf registry "77888999000181" br
        isNotAvailable owners
    }

[<Fact>]
let ``an establishment whose company wasn't passed is left out, and absence proves nothing`` () : Task =
    task {
        let! registry = brazil ()
        let! evidence = find registry "99000111000165" br
        isNotAvailable evidence.Value

        match! registry.SearchAsync("EXEMPLO", br, ct) with
        | Ok found -> Assert.Equal(2, (available found.Value).Matches.Length)
        | Error failure -> failwith (RegistryFailure.describe failure)
    }

[<Theory>]
[<InlineData("112223330001", "81")>]
[<InlineData("074533930001", "38")>]
[<InlineData("12ABC34501DE", "35")>]
let ``CNPJ check digits hold for numeric and alphanumeric CNPJs`` (first12: string, expected: string) =
    Assert.Equal(expected, Brazil.checkDigits first12)

[<Theory>]
[<InlineData("02", "", "Active")>]
[<InlineData("02", "RECUPERACAO JUDICIAL", "InInsolvency")>]
[<InlineData("02", "LIQUIDACAO EXTRA-JUDICIAL", "InInsolvency")>]
[<InlineData("02", "EM LIQUIDACAO", "WindingUp")>]
[<InlineData("02", "ESPOLIO EV 407", "ActiveWithConcern")>]
[<InlineData("01", "", "Closed")>]
[<InlineData("03", "", "Registered")>]
[<InlineData("04", "", "Registered")>]
[<InlineData("08", "", "Closed")>]
[<InlineData("09", "", "Unknown")>]
let ``every Brazilian cadastral situation has a standing, and a new one is unknown``
    (situation: string, special: string, expected: string)
    =
    Assert.StartsWith(expected, $"%A{Brazil.standing situation special}".Replace("CompanyStanding.", ""))

[<Theory; InlineData("11222333000182"); InlineData("1122233300018"); InlineData("ABC")>]
let ``a CNPJ of the wrong shape or check digits isn't looked up`` (number: string) : Task =
    task {
        let! registry = brazil ()
        do! notSent registry number br
    }

[<Fact>]
let ``Brazil answers outside its coverage with nothing read, and a row of the wrong width is unreadable`` () : Task =
    task {
        let! registry = brazil ()
        do! outside registry "11222333000181"
        do! unreadable (BulkSources.brazilAsync [ plain (utf8 "\"11222333\";\"X\"\n") ] [] [] date ct)
    }

// ASIC

let private asic () =
    built (BulkSources.asicCompaniesAsync (zipOf [ "COMPANY_202611.csv", fixtureBytes "asic-companies.csv" ]) date ct)

[<Fact>]
let ``ASIC reads a company by its current name, with former names as other names`` () : Task =
    task {
        let! registry = asic ()
        let! company, evidence = companyIn registry "000 000 019" au
        Assert.Equal("ASIC company dataset", evidence.Source)
        Assert.Equal(date.AsOf, evidence.AsOf)
        Assert.Equal("MONAKA PTY LTD", company.Name)
        Assert.Equal<string list>([ "LOVINI HOLDINGS PTY LTD" ], company.OtherNames)
        Assert.Equal("000000019", company.Number)
        Assert.Equal(CompanyStanding.Registered(ValueSome "Registered"), company.Standing)
        Assert.Equal(RecordRole.Home, company.Role)
        Assert.Equal(ValueSome "Australian proprietary company, limited by shares", company.LegalForm)
        Assert.Equal(ValueSome(DateOnly(1990, 1, 8)), company.IncorporatedOn)
        isNotAvailable company.Addresses
        Assert.Equal(2, evidence.Raw.Value.Split('\n').Length)
    }

[<Fact>]
let ``a deregistered Australian company is dissolved on its date, and a foreign one is a foreign registration``
    ()
    : Task =
    task {
        let! registry = asic ()
        let! gone, _ = companyIn registry "000000260" au
        Assert.Equal(CompanyStanding.Dissolved, gone.Standing)
        Assert.Equal(ValueSome(DateOnly(2026, 3, 13)), gone.CeasedOn)
        let! foreign, _ = companyIn registry "000000117" au
        Assert.Equal("BNP PARIBAS", foreign.Name)
        Assert.Equal(RecordRole.ForeignRegistration, foreign.Role)
        let! strikeOff, _ = companyIn registry "000005765" au
        Assert.Equal(CompanyStanding.WindingUp "Strike-off action in progress", strikeOff.Standing)
        let! administered, _ = companyIn registry "000014120" au

        Assert.Equal(
            CompanyStanding.InInsolvency "External administration (in receivership/liquidation)",
            administered.Standing
        )

        let! body, _ = companyIn registry "001273883" au
        Assert.Equal(RecordRole.Home, body.Role)
    }

[<Fact>]
let ``an ACN not in the ASIC dataset can still be held, and it lists no officers or owners`` () : Task =
    task {
        let! registry = asic ()
        let! evidence = find registry "000000028" au
        isNotAvailable evidence.Value
        let! officers = officersOf registry "000000019" au
        isNotAvailable officers
        let! owners = ownersOf registry "000000019" au
        isNotAvailable owners
        do! outside registry "000000019"
    }

[<Theory>]
[<InlineData("REGD", "Registered")>]
[<InlineData("SOFF", "WindingUp")>]
[<InlineData("EXAD", "InInsolvency")>]
[<InlineData("DRGD", "Dissolved")>]
[<InlineData("DISS", "Dissolved")>]
[<InlineData("NOAC", "Closed")>]
[<InlineData("NRGD", "Closed")>]
[<InlineData("CNCL", "Closed")>]
[<InlineData("DIV3", "Closed")>]
[<InlineData("PROV", "Unknown")>]
[<InlineData("PEND", "Unknown")>]
[<InlineData("RSVD", "Unknown")>]
let ``every ASIC status has a standing, and a new one is unknown`` (status: string, expected: string) =
    Assert.StartsWith(expected, $"%A{AsicCompanies.standing status}".Replace("CompanyStanding.", ""))

[<Theory; InlineData("000000018"); InlineData("00000019"); InlineData("ABN")>]
let ``a value that can't be an ACN isn't looked up`` (number: string) : Task =
    task {
        let! registry = asic ()
        do! notSent registry number au
    }

[<Fact>]
let ``an ASIC file without its columns is unreadable`` () : Task =
    task { do! unreadable (BulkSources.asicCompaniesAsync (plain (utf8 "Company Name\tACN\nX\t000000019\n")) date ct) }

// Alaska

let private alaska () =
    built (
        BulkSources.alaskaAsync
            (plain (fixtureBytes "alaska-corporations.csv"))
            (ValueSome(plain (fixtureBytes "alaska-officials.csv")))
            date
            ct
    )

[<Fact>]
let ``Alaska reads an entity with its addresses, officials and shareholders`` () : Task =
    task {
        let! registry = alaska ()
        let! company, evidence = companyIn registry "473d" ak
        Assert.Equal("Alaska Division of Corporations downloads", evidence.Source)
        Assert.Equal(date.AsOf, evidence.AsOf)
        Assert.Equal("GOLDSTEIN IMPROVEMENT COMPANY", company.Name)
        Assert.Equal(CompanyStanding.Active, company.Standing)
        Assert.Equal(RecordRole.Home, company.Role)
        Assert.Equal(ValueSome "Business Corporation", company.LegalForm)
        Assert.Equal(ValueSome(DateOnly(1914, 1, 8)), company.IncorporatedOn)

        Assert.Equal<AddressKind list>(
            [
                AddressKind.PrincipalOffice
                AddressKind.Postal
                AddressKind.RegisteredAgent
                AddressKind.Other "Registered agent's mailing address"
            ],
            available company.Addresses |> List.map _.Kind
        )

        let! officers = officersOf registry "473D" ak

        Assert.Equal<(string * OfficerRole) list>(
            [
                "PERSON ONE", OfficerRole.Director
                "PERSON ONE", OfficerRole.Other "President"
                "PERSON TWO", OfficerRole.Secretary
                "MB SERVICES, INC.", OfficerRole.Other "Registered Agent"
            ],
            available officers |> List.map (fun officer -> officer.Name, officer.Role)
        )

        let! owners = ownersOf registry "473D" ak
        let listing = available owners

        Assert.Equal<string voption list>(
            [ ValueSome "PERSON ONE"; ValueSome "PERSON TWO" ],
            listing.Owners |> List.map _.Name
        )

        Assert.Equal<OwnershipGap list>([ OwnershipGap.OwnerNotIdentified ], listing.Gaps)
    }

[<Fact>]
let ``an Alaska entity formed elsewhere is a foreign registration, and its members can be followed`` () : Task =
    task {
        let! registry = alaska ()
        let! company, _ = companyIn registry "2755PD" ak
        Assert.Equal(RecordRole.ForeignRegistration, company.Role)
        Assert.Equal(ValueSome "DELAWARE, UNITED STATES", company.HomeRegistration |> ValueOption.bind _.Country)
        let! owners = ownersOf registry "2755PD" ak
        let first = (available owners).Owners.Head
        Assert.Equal(PartyKind.LegalEntity, first.Kind)
        Assert.Equal(ValueSome "136289", first.Entity |> ValueOption.bind _.RegistrationNumber)
    }

[<Fact>]
let ``a non-compliant Alaska entity has a concern, and officials without owner titles leave owners unknown`` () : Task =
    task {
        let! registry = alaska ()
        let! company, _ = companyIn registry "1867D" ak
        Assert.Equal(CompanyStanding.ActiveWithConcern "Non-Compliant", company.Standing)
        let! owners = ownersOf registry "1867D" ak
        isNotAvailable owners
        let! name, _ = companyIn registry "32205" ak
        Assert.Equal(CompanyStanding.Registered(ValueSome "Active Name"), name.Standing)
        Assert.Equal(RecordRole.NotStated, name.Role)
    }

[<Fact>]
let ``a broken Alaska row is dropped, and absence proves nothing`` () : Task =
    task {
        let! registry = alaska ()
        let! evidence = find registry "99999999" ak
        isNotAvailable evidence.Value
    }

[<Fact>]
let ``without the officials download, Alaska names only the registered agent and no owners`` () : Task =
    task {
        let! registry =
            built (BulkSources.alaskaAsync (plain (fixtureBytes "alaska-corporations.csv")) ValueNone date ct)

        let! officers = officersOf registry "473D" ak
        Assert.Equal<string list>([ "MB SERVICES, INC." ], available officers |> List.map _.Name)
        let! owners = ownersOf registry "473D" ak
        isNotAvailable owners
    }

[<Theory>]
[<InlineData("Good Standing", "Active")>]
[<InlineData("Non-Compliant", "ActiveWithConcern")>]
[<InlineData("Active Name", "Registered")>]
[<InlineData("undefined", "Unknown")>]
[<InlineData("Involuntarily Dissolved", "Unknown")>]
let ``every Alaska status word has a standing, and a new one is unknown`` (status: string, expected: string) =
    Assert.StartsWith(expected, $"%A{Alaska.standing status}".Replace("CompanyStanding.", ""))

[<Theory; InlineData("473DDD"); InlineData("D473"); InlineData("")>]
let ``a value that can't be an Alaska entity number isn't looked up`` (number: string) : Task =
    task {
        let! registry = alaska ()
        do! notSent registry number ak
    }

[<Fact>]
let ``Alaska answers outside its coverage with nothing read, and a file without its columns is unreadable`` () : Task =
    task {
        let! registry = alaska ()
        do! outside registry "473D"
        do! unreadable (BulkSources.alaskaAsync (plain (utf8 "\"ENTITYNUMBER\"\n\"473D\"\n")) ValueNone date ct)
    }


// Fixes: fail-closed reading of the files

[<Fact>]
let ``a Brazilian CNPJ base on two company rows answers NotAvailable, with its establishments`` () : Task =
    task {
        let empresas =
            Array.append
                (fixtureBytes "brazil-empresas.csv")
                ("\"11222333\";\"OUTRA LTDA\";\"2062\";\"49\";\"1,00\";\"01\";\"\"\n"B)

        let! registry =
            built (
                BulkSources.brazilAsync
                    [ zipOf [ "K3241.K03200Y9.D61010.EMPRECSV", empresas ] ]
                    [
                        zipOf [ "K3241.K03200Y9.D61010.ESTABELE", fixtureBytes "brazil-estabelecimentos.csv" ]
                    ]
                    [ zipOf [ "K3241.K03200Y9.D61010.SOCIOCSV", fixtureBytes "brazil-socios.csv" ] ]
                    date
                    ct
            )

        let! establishment = find registry "11222333000181" br
        isNotAvailable establishment.Value
        let! basis = find registry "11222333" br
        isNotAvailable basis.Value
    }

[<Fact>]
let ``a Brazilian foreign partner of unknown kind leaves the owner unreadable`` () =
    let partner: Brazil.Partner =
        {
            Identifier = "3"
            Name = "SOCIO ESTRANGEIRO"
            Document = ""
            Qualification = "22"
            Since = ValueNone
            Line = ""
        }

    match Brazil.listing true [ partner ] with
    | ValueSome listing ->
        Assert.Equal<OwnershipGap list>(
            [ OwnershipGap.OwnerDetailsNotConfirmed; OwnershipGap.OwnerUnreadable ],
            listing.Gaps
        )
    | ValueNone -> failwith "no listing"

[<Fact>]
let ``a Ukrainian founder of unknown kind leaves the owner unreadable`` () : Task =
    task {
        let xml =
            "<DATA><SUBJECT><NAME>ТОВ ПРИКЛАД</NAME><EDRPOU>12345678</EDRPOU><STAN>зареєстровано</STAN><FOUNDERS><FOUNDER>PERSON ONE</FOUNDER></FOUNDERS></SUBJECT></DATA>"

        let! registry = built (BulkSources.ukraineAsync (plain (utf8 xml)) date ct)
        let! owners = ownersOf registry "12345678" ua
        let listing = available owners
        Assert.Contains(OwnershipGap.OwnerUnreadable, listing.Gaps)
    }

[<Fact>]
let ``a Florida number in two parts of the quarterly zip is repeated, not replaced`` () : Task =
    task {
        let quarterly =
            zipOf
                [
                    "cordata0.txt", fixtureBytes "florida-cordata.txt"
                    "cordata1.txt", fixtureBytes "florida-cordata.txt"
                ]

        let! registry = built (BulkSources.floridaAsync [ quarterly ] date ct)
        let! evidence = find registry "L20000012345" fl
        isNotAvailable evidence.Value
    }

[<Fact>]
let ``an Alaska officials row that can't be read withholds its company's officers and owners`` () : Task =
    task {
        let officials =
            Array.append
                (fixtureBytes "alaska-officials.csv")
                (utf8 "\"473D\",\"GOLDSTEIN IMPROVEMENT COMPANY\",\"Business Corporation\",,\"TWO\",\"PERSON\"\n")

        let! registry =
            built (
                BulkSources.alaskaAsync
                    (plain (fixtureBytes "alaska-corporations.csv"))
                    (ValueSome(plain officials))
                    date
                    ct
            )

        let! officers = officersOf registry "473D" ak
        isNotAvailable officers
        let! owners = ownersOf registry "473D" ak
        isNotAvailable owners
    }

[<Fact>]
let ``an Alaska officials row naming no company withholds every company's officials`` () : Task =
    task {
        let officials = Array.append (fixtureBytes "alaska-officials.csv") (utf8 "\"\n")

        let! registry =
            built (
                BulkSources.alaskaAsync
                    (plain (fixtureBytes "alaska-corporations.csv"))
                    (ValueSome(plain officials))
                    date
                    ct
            )

        match! officersOf registry "473D" ak with
        | NotAvailable reason -> Assert.Contains("couldn't be read or tied to a company", reason)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``bytes that aren't UTF-8 make a file unreadable, not other text`` () : Task =
    task {
        let ruc = "80000170"

        let row =
            Array.concat
                [
                    utf8 $"{ruc}|PARAPORT "
                    [| 0xFFuy |]
                    utf8 $" S.R.L.|{Paraguay.checkDigit ruc}||ACTIVO|\n"
                ]

        do! unreadable (BulkSources.paraguayAsync [ plain row ] date ct)
    }

[<Fact>]
let ``a zip from a stream that can't seek is read from a temporary copy`` () : Task =
    task {
        let gzipped = new MemoryStream()

        do
            use zipper = new GZipStream(gzipped, CompressionLevel.Fastest, true)
            (ruc0 ()).CopyTo zipper

        gzipped.Position <- 0L
        use unseekable = new GZipStream(gzipped, CompressionMode.Decompress)
        Assert.False unseekable.CanSeek
        let! registry = paraguay [ unseekable ]
        let! company, _ = companyIn registry "80000170-2" py
        Assert.Equal("PARAPORT S.R.L.", company.Name)
    }
