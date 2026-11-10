/// Bulk sources read from files recorded on 2026-11-08, trimmed and with people's names redacted. Nothing is called: each builds from the
/// streams a host would pass, as published (zip, XLSX, JSON or XML).
module BulkSourceTests3

open System
open System.IO
open System.IO.Compression
open System.Text
open System.Threading.Tasks
open Xunit
open CompanyDueDiligence
open Fake

let private bytes (name: string) =
    File.ReadAllBytes(Path.Combine(AppContext.BaseDirectory, "Fixtures", name))

let private downloadedAt = DateTimeOffset(2026, 11, 8, 9, 0, 0, TimeSpan.Zero)

let private date =
    {
        DownloadedAt = downloadedAt
        AsOf = ValueNone
    }

let private writeZip (stream: Stream) (entries: (string * byte array) list) =
    use archive = new ZipArchive(stream, ZipArchiveMode.Create, true)

    for name, content in entries do
        use entry = archive.CreateEntry(name).Open()
        entry.Write(content, 0, content.Length)

/// A zip of these entries, as a seekable stream at its start.
let private zipOf (entries: (string * byte array) list) : Stream =
    let stream = new MemoryStream()
    writeZip stream entries
    stream.Position <- 0L
    stream

let private xlsxOf (prefix: string) =
    zipOf
        [
            "xl/worksheets/sheet1.xml", bytes $"{prefix}-sheet1.xml"
            "xl/sharedStrings.xml", bytes $"{prefix}-sharedStrings.xml"
        ]

let private streamOf (name: string) : Stream = new MemoryStream(bytes name)

let private garbage () : Stream =
    new MemoryStream("this is not the file the source publishes"B)

let private md = Jurisdiction.ofCode "MD"
let private rs = Jurisdiction.ofCode "RS"
let private brc = Jurisdiction.ofCode "BA-BRC"
let private caba = Jurisdiction.ofCode "AR-C"
let private cl = Jurisdiction.ofCode "CL"
let private pe = Jurisdiction.ofCode "PE"
let private ec = Jurisdiction.ofCode "EC"
let private ru = Jurisdiction.ofCode "RU"

let private argentinaZip () =
    zipOf
        [
            "igj-entidades-202607.csv",
            "numero_correlativo,tipo_societario,descripcion_tipo_societario,razon_social,dada_de_baja,codigo_baja,detalle_baja,cuit\r\n365,40,SOCIEDAD DE RESPONSABILIDAD LIMITADA,CUFAROIL ANTERIOR,,,,30539374814\r\n"B
            "igj-domicilios-202607.csv",
            "numero_correlativo,tipo_societario,descripcion_tipo_societario,razon_social,tipo_domicilio,descripcion_tipo_domicilio,calle,numero,localidad,provincia,codigo_postal\r\n"B
            "igj-autoridades-202607.csv",
            "numero_correlativo,apellido_nombre,tipo_administrador,descripcion_tipo_administrador,tipo_documento,descripcion_tipo_documento,numero_documento,genero_autoridad\r\n"B
            "igj-entidades-202609.csv", bytes "argentina-entidades.csv"
            "igj-domicilios-202609.csv", bytes "argentina-domicilios.csv"
            "igj-autoridades-202609.csv", bytes "argentina-autoridades.csv"
            "igj-balances-202609.csv", "numero_correlativo\r\n"B
        ]

let private chileNames () =
    zipOf
        [
            "CODIGO_TIPO_SUBTIPO_.txt", bytes "chile-subtipos.txt"
            "PUB_NOMBRES_PJ.txt", bytes "chile-nombres.txt"
        ]

let private chileAddresses () =
    zipOf [ "PUB_NOM_DOMICILIO.txt", bytes "chile-domicilio.txt" ]

let private peruZip (extra: string) =
    let text = Array.append (bytes "peru-padron.txt") (Encoding.Latin1.GetBytes extra)
    zipOf [ "padron_reducido_ruc.txt", text ]

let private russiaZip () =
    zipOf
        [
            "VO_RRMSPSV_0000_9965_20261010_000c48be-5368-4e9c-a9d2-bb1e5ff316b4.xml", bytes "russia-rsmp.xml"
        ]

let private built (build: Task<Result<ICompanyRegistry, RegistryFailure>>) =
    task {
        match! build with
        | Ok registry -> return registry
        | Error failure -> return failwith (RegistryFailure.describe failure)
    }

let private moldova () =
    built (BulkSources.moldovaAsync (xlsxOf "moldova") date ct)

let private serbia () =
    built (BulkSources.serbiaAsync (streamOf "serbia-companies.json") date ct)

let private brcko () =
    built (BulkSources.bosniaBrckoAsync (streamOf "brcko-opendata.xml") date ct)

let private argentina () =
    built (BulkSources.argentinaCabaAsync (argentinaZip ()) date ct)

let private chile () =
    built (BulkSources.chileSiiAsync (chileNames ()) (chileAddresses ()) date ct)

let private peru () =
    built (BulkSources.peruAsync (peruZip "") date ct)

let private ecuador () =
    built (BulkSources.ecuadorAsync (xlsxOf "ecuador") date ct)

let private russia () =
    built (BulkSources.russiaSmeAsync (russiaZip ()) date ct)

let private find (registry: ICompanyRegistry) (number: string) (jurisdiction: Jurisdiction) =
    task {
        match! registry.TryFindAsync(number, jurisdiction, ct) with
        | Ok evidence -> return evidence
        | Error failure -> return failwith (RegistryFailure.describe failure)
    }

let private companyOf (evidence: Evidence<Availability<Company voption>>) =
    match evidence.Value with
    | Available(ValueSome company) -> company
    | other -> failwith $"%A{other}"

let private addressOf (company: Company) =
    match company.Addresses with
    | Available [ address ] -> address.Address
    | other -> failwith $"%A{other}"

let private officersOf (registry: ICompanyRegistry) number jurisdiction =
    task {
        match! registry.OfficersAsync(number, jurisdiction, ct) with
        | Ok evidence -> return evidence.Value
        | Error failure -> return failwith (RegistryFailure.describe failure)
    }

let private ownersOf (registry: ICompanyRegistry) number jurisdiction =
    task {
        match! registry.OwnersAsync(number, jurisdiction, OwnerPurpose.CustomerDueDiligence, ct) with
        | Ok evidence -> return evidence.Value
        | Error failure -> return failwith (RegistryFailure.describe failure)
    }

let private isNotAvailable (value: Availability<'T>) =
    match value with
    | NotAvailable _ -> true
    | Available _
    | NeedsEntitlement _ -> false

/// Each source built from its fixture, with a number it holds and one that can't be valid.
let private source (name: string) =
    match name with
    | "moldova" -> moldova (), "1005600009885", "1005600009886", md
    | "serbia" -> serbia (), "17003313", "1700331", rs
    | "brcko" -> brcko (), "1-369", "1/369", brc
    | "argentina" -> argentina (), "365", "123456789", caba
    | "chile" -> chile (), "76007222-2", "76007222-3", cl
    | "peru" -> peru (), "20330791412", "10452159428", pe
    | "ecuador" -> ecuador (), "1790008959001", "17900089590", ec
    | "russia" -> russia (), "1037200651290", "321723200039120", ru
    | other -> failwith other

/// Each source built from a file that isn't one.
let private unreadable (name: string) =
    match name with
    | "moldova" -> BulkSources.moldovaAsync (garbage ()) date ct
    | "serbia" -> BulkSources.serbiaAsync (garbage ()) date ct
    | "brcko" -> BulkSources.bosniaBrckoAsync (garbage ()) date ct
    | "argentina" -> BulkSources.argentinaCabaAsync (garbage ()) date ct
    | "chile" -> BulkSources.chileSiiAsync (garbage ()) (garbage ()) date ct
    | "peru" -> BulkSources.peruAsync (garbage ()) date ct
    | "ecuador" -> BulkSources.ecuadorAsync (garbage ()) date ct
    | "russia" -> BulkSources.russiaSmeAsync (garbage ()) date ct
    | other -> failwith other

[<Theory>]
[<InlineData("moldova")>]
[<InlineData("serbia")>]
[<InlineData("brcko")>]
[<InlineData("argentina")>]
[<InlineData("chile")>]
[<InlineData("peru")>]
[<InlineData("ecuador")>]
[<InlineData("russia")>]
let ``every bulk source answers OutsideCoverage outside its jurisdiction`` (name: string) : Task =
    task {
        let build, number, _, _ = source name
        let! registry = build

        match! registry.TryFindAsync(number, Jurisdiction.ofCode "GB", ct) with
        | Error(RegistryFailure.OutsideCoverage _) -> ()
        | other -> failwith $"%A{other}"

        match! registry.OfficersAsync(number, Jurisdiction.ofCode "GB", ct) with
        | Error(RegistryFailure.OutsideCoverage _) -> ()
        | other -> failwith $"%A{other}"
    }

[<Theory>]
[<InlineData("moldova")>]
[<InlineData("serbia")>]
[<InlineData("brcko")>]
[<InlineData("argentina")>]
[<InlineData("chile")>]
[<InlineData("peru")>]
[<InlineData("ecuador")>]
[<InlineData("russia")>]
let ``every bulk source refuses a number that can't be one`` (name: string) : Task =
    task {
        let build, _, bad, jurisdiction = source name
        let! registry = build

        match! registry.TryFindAsync(bad, jurisdiction, ct) with
        | Error(RegistryFailure.InvalidRequest _) -> ()
        | other -> failwith $"%A{other}"
    }

[<Theory>]
[<InlineData("moldova")>]
[<InlineData("serbia")>]
[<InlineData("brcko")>]
[<InlineData("argentina")>]
[<InlineData("chile")>]
[<InlineData("peru")>]
[<InlineData("ecuador")>]
[<InlineData("russia")>]
let ``every bulk source answers a file it can't read as unreadable, holding none of its text`` (name: string) : Task =
    task {
        match! unreadable name with
        | Error(RegistryFailure.UnreadableAnswer reason) -> Assert.DoesNotContain("not the file", reason)
        | other -> failwith $"%A{other}"
    }

[<Theory>]
[<InlineData("moldova")>]
[<InlineData("serbia")>]
[<InlineData("brcko")>]
[<InlineData("argentina")>]
[<InlineData("chile")>]
[<InlineData("peru")>]
[<InlineData("ecuador")>]
[<InlineData("russia")>]
let ``every bulk source answers as of its file, from its download`` (name: string) : Task =
    task {
        let build, number, _, jurisdiction = source name
        let! registry = build
        let! evidence = find registry number jurisdiction
        let company = companyOf evidence
        Assert.Equal(registry.Coverage.Registry, evidence.Source)
        Assert.Equal(downloadedAt, evidence.RetrievedAt)
        Assert.True evidence.Raw.IsSome
        Assert.Equal(jurisdiction, company.Jurisdiction)
    }

// Moldova

[<Fact>]
let ``Moldova reads a company with its leaders, founders and the file's date`` () : Task =
    task {
        let! registry = moldova ()
        let! evidence = find registry "1005600009885" md
        let company = companyOf evidence
        Assert.Equal("Societatea Comercială \"DAN SEBASTIAN\" S.R.L.", company.Name)
        Assert.Equal("1005600009885", company.Number)
        Assert.Equal(CompanyStanding.Registered ValueNone, company.Standing)
        Assert.Equal(ValueNone, company.StatusAsPublished)
        Assert.Equal(RecordRole.Home, company.Role)
        Assert.Equal(ValueSome "Societate cu răspundere limitată", company.LegalForm)
        Assert.Equal(ValueSome(DateOnly(1992, 3, 10)), company.IncorporatedOn)
        Assert.Equal(ValueSome(DateOnly(2026, 11, 2)), evidence.AsOf)
        let address = addressOf company
        Assert.Equal<string list>([ "MD-2003, mun. Chișinău, or. Durlești, Livezilor, 14/a" ], address.Lines)
        Assert.Equal(ValueSome "MD-2003", address.PostalCode)
        Assert.Contains("Fondatori: PERSON ONE (33,35%)", evidence.Raw.Value)

        match! officersOf registry "1005600009885" md with
        | Available [ officer ] ->
            Assert.Equal("PERSON ONE", officer.Name)
            Assert.Equal(OfficerRole.Director, officer.Role)
            Assert.True officer.Current
        | other -> failwith $"%A{other}"

        match! ownersOf registry "1005600009885" md with
        | Available listing ->
            Assert.Equal<string voption list>(
                [ ValueSome "PERSON ONE"; ValueSome "PERSON TWO"; ValueSome "PERSON THREE" ],
                listing.Owners |> List.map _.Name
            )

            let first = listing.Owners.Head.Control.Head
            Assert.Equal(ControlKind.Ownership, first.Kind)
            Assert.Equal(ValueSome { From = 33; To = 34 }, first.Band)
            Assert.Equal("33,35%", first.AsPublished)
            Assert.Equal<OwnershipGap list>([ OwnershipGap.OwnerUnreadable ], listing.Gaps)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``Moldova reads a whole share as an exact band and a company founder as published`` () : Task =
    task {
        let! registry = moldova ()

        match! ownersOf registry "1004600036086" md with
        | Available { Owners = [ owner ] } ->
            Assert.Equal(ValueSome "\"KAZAN WHEAT\" S.R.L.", owner.Name)
            Assert.Equal(ValueSome { From = 100; To = 100 }, owner.Control.Head.Band)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``Moldova reads a liquidation date as dissolved, and a liquidator or insolvency administrator from the leaders``
    ()
    : Task =
    task {
        let! registry = moldova ()
        let! liquidated = find registry "1004600031106" md
        let liquidated = companyOf liquidated
        Assert.Equal(CompanyStanding.Dissolved, liquidated.Standing)
        Assert.Equal(ValueSome(DateOnly(2025, 4, 11)), liquidated.CeasedOn)
        Assert.Equal(ValueSome "Data lichidării 11.04.2025", liquidated.StatusAsPublished)

        let! insolvent = find registry "1004600036086" md
        let insolvent = companyOf insolvent
        Assert.Equal(CompanyStanding.InInsolvency "Administrator al procesului de insolvabilitate", insolvent.Standing)
        Assert.Equal<CompanyConcern list>([ CompanyConcern.InsolvencyHistory ], insolvent.Concerns.Stated)

        let! winding = find registry "1003600086958" md
        Assert.Equal(CompanyStanding.WindingUp "Lichidator", (companyOf winding).Standing)

        let! branch = find registry "1007600021232" md
        Assert.Equal(RecordRole.Establishment, (companyOf branch).Role)
    }

[<Theory>]
[<InlineData("", "Administrator", "Registered")>]
[<InlineData("", "Administrator provizoriu", "Registered")>]
[<InlineData("", "Datele lipsesc", "Registered")>]
[<InlineData("", "Lichidator", "WindingUp")>]
[<InlineData("", "Administrator al procesului de insolvabilitate", "InInsolvency")>]
[<InlineData("", "Administrator al procedurii planului", "InInsolvency")>]
[<InlineData("2025-04-11", "Administrator", "Dissolved")>]
[<InlineData("2025-04-11", "Lichidator", "Dissolved")>]
let ``Moldova places a standing from the liquidation date and the leaders' roles``
    (liquidated: string, role: string, expected: string)
    =
    let liquidatedOn =
        if liquidated = "" then
            ValueNone
        else
            ValueSome(DateOnly.Parse liquidated)

    Assert.StartsWith(expected, $"%A{Moldova.standing liquidatedOn [ role ]}")

[<Fact>]
let ``Moldova can't tell about a number it doesn't hold, or holds twice`` () : Task =
    task {
        let! registry = moldova ()
        let! absent = find registry "1003600000006" md
        Assert.True(isNotAvailable absent.Value)
        let! twice = find registry "1002600001257" md

        match twice.Value with
        | NotAvailable reason -> Assert.StartsWith("Several records", reason)
        | other -> failwith $"%A{other}"

        let! officers = officersOf registry "1003600000006" md
        Assert.True(isNotAvailable officers)
    }

[<Theory>]
[<InlineData("1005600009885", true)>]
[<InlineData("1005600009886", false)>]
[<InlineData("100560000988", false)>]
[<InlineData("C 1005600009885", false)>]
let ``Moldova checks an IDNO's check digit`` (number: string, valid: bool) =
    Assert.Equal(valid, (Moldova.normalNumber number).IsSome)

// Serbia

[<Fact>]
let ``Serbia reads a company with its municipality and the file's date`` () : Task =
    task {
        let! registry = serbia ()
        let! evidence = find registry "17003313" rs
        let company = companyOf evidence
        Assert.Equal("PROIZVODNO TRGOVINSKO PREDUZEĆE JASENOVO AD  NOVA VAROŠ", company.Name)
        Assert.Equal(CompanyStanding.Active, company.Standing)
        Assert.Equal(ValueSome "Активан", company.StatusAsPublished)
        Assert.Equal(ValueSome "Акционарско друштво", company.LegalForm)
        Assert.Equal(RecordRole.Home, company.Role)
        Assert.Equal(ValueSome(DateOnly(1994, 8, 25)), company.IncorporatedOn)
        Assert.Equal(ValueSome "НОВА ВАРОШ", (addressOf company).Locality)
        Assert.Equal(ValueSome(DateOnly(2026, 10, 31)), evidence.AsOf)
        Assert.StartsWith("\"17003313\": {", evidence.Raw.Value)

        let! branch = find registry "20013010" rs
        Assert.Equal(RecordRole.Establishment, (companyOf branch).Role)

        let! bankrupt = find registry "17004999" rs
        let bankrupt = companyOf bankrupt
        Assert.Equal(CompanyStanding.InInsolvency "У стечају", bankrupt.Standing)
        Assert.Equal<CompanyConcern list>([ CompanyConcern.InsolvencyHistory ], bankrupt.Concerns.Stated)
    }

[<Theory>]
[<InlineData("Активан", "Active")>]
[<InlineData("У ликвидацији", "WindingUp")>]
[<InlineData("У принудној ликвидацији", "WindingUp")>]
[<InlineData("У стечају", "InInsolvency")>]
[<InlineData("Брисан", "Unknown (ValueSome")>]
[<InlineData("", "Unknown ValueNone")>]
let ``Serbia maps each status word it publishes`` (word: string, expected: string) =
    Assert.StartsWith(expected, $"%A{Serbia.standing (ValueSome word)}")

[<Fact>]
let ``Serbia can't tell about a number it doesn't hold, and lists no officers or owners without being asked``
    ()
    : Task =
    task {
        let! registry = serbia ()
        let! absent = find registry "12345678" rs
        Assert.True(isNotAvailable absent.Value)
        let! officers = officersOf registry "17003313" rs
        Assert.True(isNotAvailable officers)
        let! owners = ownersOf registry "17003313" rs
        Assert.True(isNotAvailable owners)
    }

[<Fact>]
let ``a bulk source searches names over its file, never completely when the file leaves companies out`` () : Task =
    task {
        let! registry = serbia ()

        match! registry.SearchAsync("simplon", rs, ct) with
        | Ok { Value = Available result } ->
            Assert.Equal<string list>([ "17004999" ], result.Matches |> List.map _.Number)
            Assert.False result.Complete
        | other -> failwith $"%A{other}"
    }

// Brčko District

[<Fact>]
let ``Brčko reads a subject by its registration number and by its tax number`` () : Task =
    task {
        let! registry = brcko ()
        let! evidence = find registry "1-369" brc
        let company = companyOf evidence
        Assert.StartsWith("\"DENI - COMPANI\" d.o.o.", company.Name)
        Assert.Equal("1-369", company.Number)
        Assert.Equal(CompanyStanding.Active, company.Standing)
        Assert.Equal(ValueSome "DOO", company.LegalForm)
        Assert.Equal(RecordRole.Home, company.Role)
        Assert.Equal(ValueSome(DateOnly(1996, 4, 18)), company.IncorporatedOn)
        let address = addressOf company
        Assert.Equal<string list>([ "Bijela broj 108 A" ], address.Lines)
        Assert.Equal(ValueSome "Bijela", address.Locality)
        Assert.Equal(ValueSome "Brčko distrikt", address.Region)
        Assert.StartsWith("<PoslovniSubjekat>", evidence.Raw.Value)

        let! byJib = find registry "4600039390002" brc
        Assert.Equal("1-369", (companyOf byJib).Number)

        let! padded = find registry "4600502870000" brc
        Assert.Equal("96-01-0078-22", (companyOf padded).Number)
    }

[<Theory>]
[<InlineData("Aktivan", "Active")>]
[<InlineData("U procesu izmjene", "Registered")>]
[<InlineData("U procesu žalbe", "Registered")>]
[<InlineData("U procesu prestanka", "WindingUp")>]
[<InlineData("Brisan", "Unknown (ValueSome")>]
let ``Brčko maps each status word it publishes`` (word: string, expected: string) =
    Assert.StartsWith(expected, $"%A{Brcko.standing (ValueSome word)}")

[<Fact>]
let ``Brčko skips a subject with no name or number and can't tell about a number it doesn't hold`` () : Task =
    task {
        let! registry = brcko ()
        let! changing = find registry "1-37" brc
        Assert.Equal(CompanyStanding.Registered(ValueSome "U procesu izmjene"), (companyOf changing).Standing)

        match! find registry "1-99999" brc with
        | { Value = NotAvailable reason } -> Assert.Contains("1 of 6", reason)
        | other -> failwith $"%A{other}"

        let! officers = officersOf registry "1-369" brc
        Assert.True(isNotAvailable officers)
        let! owners = ownersOf registry "1-369" brc
        Assert.True(isNotAvailable owners)
    }

// Argentina, Ciudad de Buenos Aires

[<Fact>]
let ``IGJ reads the latest month's entity with its address, authority and partner`` () : Task =
    task {
        let! registry = argentina ()
        let! evidence = find registry "365" caba
        let company = companyOf evidence
        Assert.Equal("CUFAROIL", company.Name)
        Assert.Equal("365", company.Number)
        Assert.Equal(CompanyStanding.Registered ValueNone, company.Standing)
        Assert.Equal(ValueSome "SOCIEDAD DE RESPONSABILIDAD LIMITADA", company.LegalForm)
        Assert.Equal(RecordRole.Home, company.Role)

        match company.Addresses with
        | Available [ address ] ->
            Assert.Equal(AddressKind.RegisteredOffice, address.Kind)
            Assert.Equal<string list>([ "ARANGUREN 2178" ], address.Address.Lines)
            Assert.Equal(ValueSome "CAPITAL FEDERAL", address.Address.Locality)
            Assert.Equal(ValueSome "1406", address.Address.PostalCode)
        | other -> failwith $"%A{other}"

        Assert.DoesNotContain("00000001", evidence.Raw.Value)

        let! byCuit = find registry "30-53937481-4" caba
        Assert.Equal("365", (companyOf byCuit).Number)

        match! officersOf registry "365" caba with
        | Available [ officer ] ->
            Assert.Equal("PERSON ONE", officer.Name)
            Assert.Equal(OfficerRole.Other "AUTORIDAD", officer.Role)
            Assert.Equal(PartyKind.Individual, officer.Kind)
        | other -> failwith $"%A{other}"

        match! ownersOf registry "365" caba with
        | Available listing ->
            Assert.Equal<string voption list>([ ValueSome "PERSON ONE" ], listing.Owners |> List.map _.Name)
            Assert.Equal(ValueNone, listing.Owners.Head.Control.Head.Band)
            Assert.Equal<OwnershipGap list>([ OwnershipGap.OwnerDetailsNotConfirmed ], listing.Gaps)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``IGJ reads a removal with its reason, a foreign company and a name broken over two lines`` () : Task =
    task {
        let! registry = argentina ()
        let! removed = find registry "2518" caba
        let removed = companyOf removed
        Assert.Equal(CompanyStanding.Closed "ESTATUTO POR TRANSFORMACION", removed.Standing)
        Assert.Equal(ValueSome "dada_de_baja S: ESTATUTO POR TRANSFORMACION", removed.StatusAsPublished)
        Assert.True(isNotAvailable removed.Addresses)

        let! owners = ownersOf registry "2518" caba
        Assert.True(isNotAvailable owners)

        match! officersOf registry "2518" caba with
        | Available officers -> Assert.Equal(2, officers.Length)
        | other -> failwith $"%A{other}"

        let! foreign = find registry "126983" caba
        Assert.Equal(RecordRole.ForeignRegistration, (companyOf foreign).Role)

        match! officersOf registry "126983" caba with
        | Available [ officer ] -> Assert.Equal(OfficerRole.Other "REPRESENTANTE", officer.Role)
        | other -> failwith $"%A{other}"

        let! broken = find registry "2013461" caba
        Assert.Equal("SIC-SERVICIOS INTEGRALES DE CONSTRUCCION Y DISEÑO", (companyOf broken).Name)
    }

[<Theory; InlineData("", "Registered"); InlineData("S", "Closed"); InlineData("N", "Unknown (ValueSome")>]
let ``IGJ maps dada_de_baja`` (removed: string, expected: string) =
    let removed = if removed = "" then ValueNone else ValueSome removed
    let standing = ArgentinaIgj.standing removed (ValueSome "CANCELACION")
    Assert.StartsWith(expected, $"%A{standing}")

[<Fact>]
let ``IGJ can't tell about a number it doesn't hold, and a zip without a whole month is unreadable`` () : Task =
    task {
        let! registry = argentina ()
        let! absent = find registry "999999" caba
        Assert.True(isNotAvailable absent.Value)

        let partial = zipOf [ "igj-entidades-202609.csv", bytes "argentina-entidades.csv" ]

        match! BulkSources.argentinaCabaAsync partial date ct with
        | Error(RegistryFailure.UnreadableAnswer _) -> ()
        | other -> failwith $"%A{other}"
    }

// Chile

[<Fact>]
let ``SII reads a legal person with its current domicile and end of activities`` () : Task =
    task {
        let! registry = chile ()
        let! evidence = find registry "76.007.222-2" cl
        let company = companyOf evidence
        Assert.Equal("CONTRERAS E HIJO LIMITADA", company.Name)
        Assert.Equal("76007222-2", company.Number)
        Assert.Equal(CompanyStanding.Closed "Término de giro 28-04-2022", company.Standing)
        Assert.Equal(ValueSome(DateOnly(2022, 4, 28)), company.CeasedOn)
        Assert.Equal(ValueSome "SOC. RESPONSABILIDAD LIMITADA", company.LegalForm)
        Assert.Equal(RecordRole.Home, company.Role)
        let address = addressOf company
        Assert.Equal<string list>([ "ESCANILLA 825"; "STGO" ], address.Lines)
        Assert.Equal(ValueSome "INDEPENDENCIA", address.Locality)
        Assert.Equal(ValueSome "XIII REGION METROPOLITANA", address.Region)
        Assert.Contains("2014-04-19", evidence.Raw.Value)
        Assert.DoesNotContain("LOS CANCILLERES", evidence.Raw.Value)

        let! open' = find registry "50092210-9" cl
        let open' = companyOf open'
        Assert.Equal(CompanyStanding.Registered ValueNone, open'.Standing)
        Assert.Equal(ValueSome(DateOnly(1993, 1, 1)), open'.IncorporatedOn)
        Assert.Equal<string list>([ "CONDELL 1176 74"; "VALPARAISO" ], (addressOf open').Lines)

        // A line with fewer fields than the header can't say whether its end date is blank or cut off.
        let! short = find registry "47000001-5" cl
        Assert.True(isNotAvailable short.Value)
    }

[<Theory; InlineData("28-04-2022", "Closed"); InlineData("", "Registered")>]
let ``SII reads an end of activities as closed and anything else as registered`` (ended: string, expected: string) =
    let standing = ChileSii.standing (if ended = "" then ValueNone else ValueSome ended)
    Assert.StartsWith(expected, $"%A{standing}")

[<Theory>]
[<InlineData("76007222-2", "76007222-2")>]
[<InlineData("76.007.222-2", "76007222-2")>]
[<InlineData("12.345.678-5", "12345678-5")>]
[<InlineData("76007222-3", "")>]
[<InlineData("76007222", "")>]
let ``SII checks a RUT's check digit`` (number: string, expected: string) =
    Assert.Equal(expected, ChileSii.normalNumber number |> ValueOption.defaultValue "")

[<Fact>]
let ``SII can't tell about a RUT it doesn't hold, and lists no officers or owners`` () : Task =
    task {
        let! registry = chile ()
        let! absent = find registry "12.345.678-5" cl
        Assert.True(isNotAvailable absent.Value)
        let! officers = officersOf registry "76007222-2" cl
        Assert.True(isNotAvailable officers)
        let! owners = ownersOf registry "76007222-2" cl
        Assert.True(isNotAvailable owners)
    }

// Peru

[<Fact>]
let ``SUNAT reads a company's RUC line with its address and domicile condition`` () : Task =
    task {
        let! registry = peru ()
        let! evidence = find registry "20330791412" pe
        let company = companyOf evidence
        Assert.Equal("ORYGEN PERU S.A.A.", company.Name)
        Assert.Equal(CompanyStanding.Registered(ValueSome "ACTIVO (tax registration)"), company.Standing)
        Assert.Equal(ValueSome "ACTIVO", company.StatusAsPublished)
        Assert.Equal(RecordRole.NotStated, company.Role)
        Assert.Empty company.Concerns.Stated
        Assert.DoesNotContain(CompanyConcern.RegisteredOfficeUndeliverable, company.Concerns.Unconfirmed)

        Assert.Equal<string list>(
            [ "PASEO DEL BOSQUE 500"; "URB. CHACARILLA DEL ESTANQUE"; "UBIGEO 150130" ],
            (addressOf company).Lines
        )

        Assert.StartsWith("20330791412|ORYGEN PERU S.A.A.|ACTIVO|HABIDO|", evidence.Raw.Value)

        let! notFound = find registry "20447543631" pe

        Assert.Equal<CompanyConcern list>(
            [ CompanyConcern.RegisteredOfficeUndeliverable ],
            (companyOf notFound).Concerns.Stated
        )

        let! piped = find registry "20614244730" pe
        Assert.Equal("EBEN EZER | INGENIERIA Y ARQUITECTURA S.A.C.", (companyOf piped).Name)
    }

[<Theory>]
[<InlineData("ACTIVO", "Registered")>]
[<InlineData("SUSPENSION TEMPORAL", "Unknown")>]
[<InlineData("PENDIENTE DE INI. DE", "Unknown")>]
[<InlineData("OTROS OBLIGADOS", "Registered")>]
[<InlineData("BAJA DEFINITIVA", "Closed")>]
[<InlineData("BAJA DE OFICIO", "Closed")>]
[<InlineData("BAJA PROV. POR OFICI", "Closed")>]
[<InlineData("BAJA PROVISIONAL", "Closed")>]
[<InlineData("BAJA MULT.INSCR. Y O", "Closed")>]
[<InlineData("BAJA NO ACTIV. SISEV", "Closed")>]
[<InlineData("INHABILITADO-VENT.UN", "Closed")>]
[<InlineData("ANULACION - ERROR SU", "Closed")>]
[<InlineData("ANUL.PROVI.-ACTO ILI", "Closed")>]
[<InlineData("NUM. INTERNO IDENTIF", "Unknown (ValueSome")>]
let ``SUNAT maps each taxpayer status word in its file`` (word: string, expected: string) =
    Assert.StartsWith(expected, $"%A{Peru.standing (ValueSome word)}")

[<Theory>]
[<InlineData("HABIDO", 0, 4)>]
[<InlineData("NO HABIDO", 1, 4)>]
[<InlineData("NO HALLADO SE MUDO D", 1, 4)>]
[<InlineData("NO HALLADO", 1, 4)>]
[<InlineData("PENDIENTE", 0, 5)>]
[<InlineData("-", 0, 5)>]
let ``SUNAT reads the domicile condition as a concern`` (word: string, stated: int, unconfirmed: int) =
    let concerns = Peru.concerns (ValueSome word)
    Assert.Equal(stated, concerns.Stated.Length)
    Assert.Equal(unconfirmed, concerns.Unconfirmed.Length)

[<Fact>]
let ``SUNAT's file holds every RUC, so a company RUC not in it isn't held, unless a line couldn't be read`` () : Task =
    task {
        let notHeld =
            List.init 10 (fun d -> $"2010004721{d}")
            |> List.find (fun n -> (Peru.normalNumber n).IsSome)

        let! registry = peru ()
        let! absent = find registry notHeld pe
        Assert.Equal(Available ValueNone, absent.Value)
        let! officers = officersOf registry "20330791412" pe
        Assert.True(isNotAvailable officers)

        let! damaged =
            built (BulkSources.peruAsync (peruZip "20999999999||ACTIVO|HABIDO|-|-|-|-|-|-|-|-|-|-|-|\n") date ct)

        let! unsure = find damaged notHeld pe
        Assert.True(isNotAvailable unsure.Value)
    }

[<Fact>]
let ``SUNAT answers a RUC on two lines as not available, not as either company`` () : Task =
    task {
        let! registry =
            built (
                BulkSources.peruAsync
                    (peruZip "20330791412|OTRA EMPRESA S.A.C.|ACTIVO|HABIDO|-|-|-|-|-|-|-|-|-|-|-|\n")
                    date
                    ct
            )

        let! twice = find registry "20330791412" pe

        match twice.Value with
        | NotAvailable reason -> Assert.StartsWith("Several records", reason)
        | other -> failwith $"%A{other}"
    }

[<Theory>]
[<InlineData("20330791412", true)>]
[<InlineData("20330791413", false)>]
[<InlineData("10452159428", false)>]
[<InlineData("2033079141", false)>]
let ``SUNAT checks a company RUC's prefix and check digit`` (number: string, valid: bool) =
    Assert.Equal(valid, (Peru.normalNumber number).IsSome)

// Ecuador

[<Fact>]
let ``Supercias reads a company with its legal representative and the file's date`` () : Task =
    task {
        let! registry = ecuador ()
        let! evidence = find registry "1790008959001" ec
        let company = companyOf evidence
        Assert.Equal("ACERO COMERCIAL ECUATORIANO S.A.", company.Name)
        Assert.Equal("1790008959001", company.Number)
        Assert.Equal(CompanyStanding.Active, company.Standing)
        Assert.Equal(ValueSome "ANÓNIMA", company.LegalForm)
        Assert.Equal(RecordRole.Home, company.Role)
        Assert.Equal(ValueSome(DateOnly(1957, 6, 27)), company.IncorporatedOn)
        Assert.Equal(ValueSome(DateOnly(2026, 11, 6)), evidence.AsOf)
        let address = addressOf company
        Assert.Equal<string list>([ "AV. DE LA PRENSA N45-14"; "Y EL TELEGRAFO 1"; "LA CONCEPCION" ], address.Lines)
        Assert.Equal(ValueSome "QUITO", address.Locality)
        Assert.Equal(ValueSome "PICHINCHA", address.Region)

        match! officersOf registry "1790008959001" ec with
        | Available [ officer ] ->
            Assert.Equal("PERSON ONE", officer.Name)
            Assert.Equal(OfficerRole.Other "GERENTE GENERAL", officer.Role)
        | other -> failwith $"%A{other}"

        let! byFile = find registry "3" ec
        Assert.Equal("1790008959001", (companyOf byFile).Number)

        let! owners = ownersOf registry "1790008959001" ec
        Assert.True(isNotAvailable owners)
    }

[<Fact>]
let ``Supercias reads a foreign branch, a company without a RUC and one with no representative`` () : Task =
    task {
        let! registry = ecuador ()
        let! branch = find registry "1790044149001" ec
        let branch = companyOf branch
        Assert.Equal(RecordRole.Establishment, branch.Role)
        Assert.Equal(ValueSome "COLOMBIA", branch.HomeRegistration.Value.Country)

        let! noRuc = find registry "1319" ec
        let noRuc = companyOf noRuc
        Assert.Equal("1319", noRuc.Number)
        Assert.StartsWith("WindingUp", $"%A{noRuc.Standing}")

        let! officers = officersOf registry "0990028249001" ec
        Assert.True(isNotAvailable officers)

        let! absent = find registry "1790000000001" ec
        Assert.True(isNotAvailable absent.Value)
    }

[<Theory>]
[<InlineData("ACTIVA", "Active")>]
[<InlineData("INACTIVA", "Registered")>]
[<InlineData("NO SUJETO CONTROL Y VIGILANCIA SCVS(ART. 432/OTRO)", "Registered")>]
[<InlineData("DISOLUCIÓN Y LIQUIDACIÓN OFICIO INSCRITA EN RM", "WindingUp")>]
[<InlineData("DISOLUCIÓN Y LIQUIDACIÓN DE PLENO DERECHO INSC. RM", "WindingUp")>]
[<InlineData("DISOLUCIÓN Y LIQUIDACIÓN OFICIO NO INSCRITA EN RM", "WindingUp")>]
[<InlineData("DISOLUCIÓN Y LIQUIDACIÓN ANTICIPADA INSCRITA RM", "WindingUp")>]
[<InlineData("DISOLUCIÓN Y LIQUIDACIÓ DE PLENO DERECHO NO INS RM", "WindingUp")>]
[<InlineData("DISOLUCIÓN Y LIQUIDACIÓN ANTIC. NO INSCRITA EN RM", "WindingUp")>]
[<InlineData("CANCELACIÓN PERMISO OPERACIÓN - OFICIO INSCRITA RM", "Closed")>]
[<InlineData("CANCELACIÓN PERMISO OPERACIÓN - OFICIO NO INSCRITA", "Closed")>]
[<InlineData("CANCELACIÓN PERMISO OPERACIÓN - VOLUNT INSCRITA RM", "Closed")>]
[<InlineData("CANCELACIÓN PERMISO OPERACIÓN - VOLUNT NO INSCRITA", "Closed")>]
[<InlineData("DEPURADO/ELIMINADO POR DUPLICIDAD", "Closed")>]
[<InlineData("MIGRADO A SEGUROS", "Closed")>]
[<InlineData("CANCELADA", "Unknown (ValueSome")>]
[<InlineData("", "Unknown ValueNone")>]
let ``Supercias maps each situación legal in its file`` (word: string, expected: string) =
    Assert.StartsWith(expected, $"%A{Ecuador.standing (ValueSome word)}")

// Russia

[<Fact>]
let ``FNS reads an SME by OGRN and INN with its category, location and the file's date`` () : Task =
    task {
        let! registry = russia ()
        let! evidence = find registry "1037200651290" ru
        let company = companyOf evidence
        Assert.Equal("ОБЩЕСТВО С ОГРАНИЧЕННОЙ ОТВЕТСТВЕННОСТЬЮ \"АЛЬФА-СТРОЙ\"", company.Name)
        Assert.Equal<string list>([ "ООО \"АЛЬФА-СТРОЙ\"" ], company.OtherNames)
        Assert.Equal(CompanyStanding.Registered(ValueSome "Малое предприятие"), company.Standing)
        Assert.Equal(ValueSome "КатСубМСП 2", company.StatusAsPublished)
        Assert.Equal(RecordRole.Home, company.Role)
        Assert.Equal(ValueSome(DateOnly(2026, 10, 10)), evidence.AsOf)
        let address = addressOf company
        Assert.Equal(ValueSome "Тюмень г.", address.Locality)
        Assert.Equal(ValueSome "Тюменская обл.", address.Region)
        Assert.Contains("ИННЮЛ=7204080198", evidence.Raw.Value)

        let! byInn = find registry "7204080198" ru
        Assert.Equal("1037200651290", (companyOf byInn).Number)

        let! rural = find registry "1047200559240" ru
        let rural = addressOf (companyOf rural)
        Assert.Equal<string list>([ "Тюменский р-н" ], rural.Lines)
        Assert.Equal(ValueSome "Боровский рп.", rural.Locality)

        let! medium = find registry "1067203134470" ru
        Assert.Equal(CompanyStanding.Registered(ValueSome "Среднее предприятие"), (companyOf medium).Standing)
    }

[<Theory>]
[<InlineData("1", "Registered (ValueSome \"Микропредприятие\")")>]
[<InlineData("2", "Registered (ValueSome \"Малое предприятие\")")>]
[<InlineData("3", "Registered (ValueSome \"Среднее предприятие\")")>]
[<InlineData("4", "Unknown (ValueSome")>]
[<InlineData("", "Unknown ValueNone")>]
let ``FNS maps each SME category the XSD lists`` (category: string, expected: string) =
    Assert.StartsWith(expected, $"%A{RussiaSme.standing (ValueSome category)}")

[<Fact>]
let ``FNS absence proves nothing, and an entrepreneur isn't read`` () : Task =
    task {
        let! registry = russia ()
        let! absent = find registry "1027700132195" ru

        match absent.Value with
        | NotAvailable reason -> Assert.Contains("proves nothing", reason)
        | other -> failwith $"%A{other}"

        match! registry.TryFindAsync("720407695380", ru, ct) with
        | Error(RegistryFailure.InvalidRequest _) -> ()
        | other -> failwith $"%A{other}"

        let! officers = officersOf registry "1037200651290" ru
        Assert.True(isNotAvailable officers)
        let! owners = ownersOf registry "1037200651290" ru
        Assert.True(isNotAvailable owners)

        match registry.Coverage.Standing with
        | Support.Partial caveat -> Assert.StartsWith("Covers small and medium businesses only", caveat)
        | other -> failwith $"%A{other}"
    }

[<Theory>]
[<InlineData("1037200651290", true)>]
[<InlineData("1037200651291", false)>]
[<InlineData("7204080198", true)>]
[<InlineData("7204080199", false)>]
let ``FNS checks an OGRN's and an INN's check digit`` (number: string, valid: bool) =
    Assert.Equal(valid, (RussiaSme.normalNumber number).IsSome)

// XLSX

[<Fact>]
let ``the XLSX reader places cells by their reference and reads shared, inline and number cells`` () =
    let sheet =
        """<?xml version="1.0" encoding="UTF-8"?><worksheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main"><sheetData><row r="1"><c r="B1" t="s"><v>1</v></c><c r="D1" t="inlineStr"><is><t>inline</t></is></c><c r="E1"><v>45758</v></c><c r="F1"/></row><row r="2"/></sheetData></worksheet>"""

    let strings =
        """<?xml version="1.0" encoding="UTF-8"?><sst xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main"><si><t/></si><si><r><t>two </t></r><r><t xml:space="preserve">runs</t></r><rPh><t>phonetic</t></rPh></si></sst>"""

    use zip =
        zipOf
            [
                "xl/worksheets/sheet1.xml", Encoding.UTF8.GetBytes sheet
                "xl/sharedStrings.xml", Encoding.UTF8.GetBytes strings
            ]

    use archive = new ZipArchive(zip, ZipArchiveMode.Read)
    let rows = Xlsx.rows archive "xl/worksheets/sheet1.xml" |> List.ofSeq

    match rows with
    | [ struct (1, first); struct (2, second) ] ->
        Assert.Equal<string array>([| ""; "two runs"; ""; "inline"; "45758"; "" |], first)
        Assert.Empty second
        Assert.Equal(ValueSome(DateOnly(2025, 4, 11)), Xlsx.serialDate (Xlsx.cell first 4))
    | other -> failwith $"%A{other}"


// Fixes: fail-closed reading of the files

/// A worksheet of inline strings, one row a list, in a workbook with no shared strings.
let private workbookOf (rows: string list list) =
    let cell (row: int) (column: int) (text: string) =
        let reference = $"{char (int 'A' + column)}{row}"
        let escaped = Security.SecurityElement.Escape text
        $"""<c r="{reference}" t="inlineStr"><is><t>{escaped}</t></is></c>"""

    let rowXml (index: int) (cells: string list) =
        let number = index + 1
        let inner = cells |> List.mapi (cell number) |> String.concat ""
        $"""<row r="{number}">{inner}</row>"""

    let body = rows |> List.mapi rowXml |> String.concat ""

    let sheet =
        $"""<?xml version="1.0" encoding="UTF-8"?><worksheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main"><sheetData>{body}</sheetData></worksheet>"""

    zipOf [ "xl/worksheets/sheet1.xml", Encoding.UTF8.GetBytes sheet ]

let private moldovaHeader =
    [
        "IDNO/ Cod fiscal"
        "Data înregistrării"
        "Denumirea completă"
        "Forma org./jurid."
        "Adresa"
        "Lista conducătorilor (cu indicarea rolurilor)"
        "Lista fondatorilor (cu indicarea cotei părţi în capitalul social %)"
        "Data lichidării"
    ]

[<Fact>]
let ``a Moldovan sheet without the liquidation date or leaders column is refused`` () : Task =
    task {
        for left in [ "Data lichidării"; "Lista conducătorilor (cu indicarea rolurilor)" ] do
            let header = moldovaHeader |> List.filter ((<>) left)

            let row =
                [
                    "1005600009885"
                    ""
                    "FIRMA SRL"
                    ""
                    ""
                    "PERSON ONE [Administrator]"
                    ""
                    ""
                ]
                |> List.truncate header.Length

            match! BulkSources.moldovaAsync (workbookOf [ header; row ]) date ct with
            | Error(RegistryFailure.UnreadableAnswer _) -> ()
            | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a Moldovan row with a blank leaders cell has officers not available, not none`` () : Task =
    task {
        let! registry =
            built (
                BulkSources.moldovaAsync
                    (workbookOf [ moldovaHeader; [ "1005600009885"; ""; "FIRMA SRL"; ""; ""; ""; ""; "" ] ])
                    date
                    ct
            )

        let! officers = officersOf registry "1005600009885" md
        Assert.True(isNotAvailable officers)
    }

let private siiNames (lines: string list) =
    zipOf [ "PUB_NOMBRES_PJ.txt", Encoding.UTF8.GetBytes(String.concat "\n" lines) ]

[<Fact>]
let ``an SII names file without the end of activities column is refused`` () : Task =
    task {
        let names =
            siiNames
                [
                    "RUT\tDV\tCOD_SUBTIPO\tRAZON_SOCIAL\tFECHA_INICIO_VIG"
                    "50092210\t9\t223\tOLAVE ASCENSORES SPA\t01-01-1993"
                ]

        match! BulkSources.chileSiiAsync names (chileAddresses ()) date ct with
        | Error(RegistryFailure.UnreadableAnswer _) -> ()
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``an SII line is split at tabs only, as the files quote nothing`` () : Task =
    task {
        let names =
            siiNames
                [
                    "RUT\tDV\tCOD_SUBTIPO\tRAZON_SOCIAL\tFECHA_INICIO_VIG\tFECHA_TG_VIG"
                    "50036540\t4\t213\t\"EL\" TALLER\t01-01-1993\t28-04-2022"
                ]

        let! registry = built (BulkSources.chileSiiAsync names (chileAddresses ()) date ct)
        let! quoted = find registry "50036540-4" cl
        Assert.Equal("\"EL\" TALLER", (companyOf quoted).Name)
        Assert.Equal(CompanyStanding.Closed "Término de giro 28-04-2022", (companyOf quoted).Standing)
    }

[<Fact>]
let ``an SII line with a tab inside its name is an unread line, never a record with its dates moved`` () : Task =
    task {
        let names =
            siiNames
                [
                    "RUT\tDV\tCOD_SUBTIPO\tRAZON_SOCIAL\tFECHA_INICIO_VIG\tFECHA_TG_VIG"
                    // Closed, and would read as registered with the end date pushed past the last column.
                    "76007222\t2\t211\tCONTRERAS\tE HIJO LIMITADA\t\t28-04-2022"
                    // Open, and would read as closed with the start date in the end date's column.
                    "50092210\t9\t223\tOLAVE\tASCENSORES SPA\t01-01-1993\t"
                    "50036540\t4\t213\tEL TALLER\t01-01-1993\t"
                ]

        let! registry = built (BulkSources.chileSiiAsync names (chileAddresses ()) date ct)

        for number in [ "76007222-2"; "50092210-9" ] do
            let! moved = find registry number cl

            match moved.Value with
            | NotAvailable reason -> Assert.Contains("2 lines couldn't be read", reason)
            | other -> failwith $"%s{number}: %A{other}"

        let! whole = find registry "50036540-4" cl
        Assert.Equal(CompanyStanding.Registered ValueNone, (companyOf whole).Standing)
    }

let private argentinaWith (entities: string) =
    zipOf
        [
            "igj-entidades-202609.csv",
            Encoding.UTF8.GetBytes(
                "numero_correlativo,tipo_societario,descripcion_tipo_societario,razon_social,dada_de_baja,codigo_baja,detalle_baja,cuit\r\n"
                + entities
            )
            "igj-domicilios-202609.csv", bytes "argentina-domicilios.csv"
            "igj-autoridades-202609.csv", bytes "argentina-autoridades.csv"
        ]

[<Fact>]
let ``an IGJ quote left open fails the file instead of swallowing the rows after it`` () : Task =
    task {
        let entities =
            "365,40,SRL,\"CUFAROIL,,,,30539374814\r\n366,40,SA,OTRA SA,,,,30539374822\r\n"

        match! BulkSources.argentinaCabaAsync (argentinaWith entities) date ct with
        | Error(RegistryFailure.UnreadableAnswer _) -> ()
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``an IGJ record running past 1 MB fails the file`` () : Task =
    task {
        let runOn = String.replicate 40000 "366,40,SA,OTRA SA,,,,30539374822\r\n"
        let entities = $"365,40,SRL,\"CUFAROIL,,,,30539374814\r\n{runOn}\"\r\n"

        match! BulkSources.argentinaCabaAsync (argentinaWith entities) date ct with
        | Error(RegistryFailure.UnreadableAnswer _) -> ()
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``an IGJ quote inside a name, not opening its field, doesn't join lines`` () : Task =
    task {
        let entities =
            "365,40,SRL,CUFAROIL \"EL,,,,30539374814\r\n366,40,SA,OTRA SA,,,,30539374822\r\n"

        let! registry =
            built (BulkSources.argentinaCabaAsync (argentinaWith entities) date ct)

        let! other = find registry "366" caba
        Assert.Equal("OTRA SA", (companyOf other).Name)
    }

[<Fact>]
let ``an IGJ número correlativo on two rows answers NotAvailable, with both CUITs`` () : Task =
    task {
        let entities =
            "365,40,SRL,FIRST SRL,,,,30539374814\r\n365,40,SRL,SECOND SRL,,,,30711111119\r\n366,40,SA,OTRA SA,,,,30539374822\r\n"

        let! registry =
            built (BulkSources.argentinaCabaAsync (argentinaWith entities) date ct)

        let! number = find registry "365" caba
        Assert.True(isNotAvailable number.Value)
        let! first = find registry "30539374814" caba
        Assert.True(isNotAvailable first.Value)
        let! second = find registry "30711111119" caba
        Assert.True(isNotAvailable second.Value)

        match! registry.SearchAsync("SRL", caba, ct) with
        | Ok { Value = Available result } ->
            Assert.Empty result.Matches
            Assert.False result.Complete
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``an Ecuadorian RUC stored as a number gets its leading zero back`` () =
    let header = Map.ofList [ "EXPEDIENTE", 0; "RUC", 1; "NOMBRE", 2 ]

    match Ecuador.record header [| "123"; "190012345001"; "AZUAY SA" |] with
    | ValueSome(struct (record, keys)) ->
        Assert.Equal("0190012345001", record.Company.Number)
        Assert.Contains("RUC 0190012345001", keys)
    | ValueNone -> failwith "no record"

[<Fact>]
let ``a cancelled build throws`` () : Task =
    task {
        let cancelled = Threading.CancellationToken(true)

        let! _ =
            Assert.ThrowsAnyAsync<OperationCanceledException>(fun () ->
                BulkSources.peruAsync (peruZip "") date cancelled :> Task)

        let! _ =
            Assert.ThrowsAnyAsync<OperationCanceledException>(fun () ->
                BulkSources.chileAsync [] date cancelled :> Task)

        ()
    }

[<Fact>]
let ``a cell reference past column XFD is refused, not overflowed`` () =
    Assert.Equal(16383, Xlsx.columnOf "XFD1")

    Assert.Throws<Xml.XmlException>(fun () -> Xlsx.columnOf "XFE1" |> ignore)
    |> ignore

    Assert.Throws<Xml.XmlException>(fun () -> Xlsx.columnOf "ZZZZZZZZZZZZZZ1" |> ignore)
    |> ignore

[<Fact>]
let ``a zip entry is read only up to its stated size`` () =
    use bounded = new BoundedStream(new MemoryStream(Array.zeroCreate<byte> 10), 5L)
    let buffer = Array.zeroCreate<byte> 4
    Assert.Equal(4, bounded.Read(buffer, 0, 4))

    Assert.Throws<InvalidDataException>(fun () -> bounded.Read(buffer, 0, 4) |> ignore)
    |> ignore

[<Fact>]
let ``a workbook from a stream that can't seek is read from a temporary copy`` () : Task =
    task {
        let gzipped = new MemoryStream()

        do
            use zipper = new GZipStream(gzipped, CompressionLevel.Fastest, true)
            (xlsxOf "moldova").CopyTo zipper

        gzipped.Position <- 0L
        use unseekable = new GZipStream(gzipped, CompressionMode.Decompress)
        Assert.False unseekable.CanSeek
        let! registry = built (BulkSources.moldovaAsync unseekable date ct)
        let! company = find registry "1005600009885" md
        Assert.Contains("DAN SEBASTIAN", (companyOf company).Name)
    }
