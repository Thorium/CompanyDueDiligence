/// Indexes on disk of bulk registers: every source's fixture built in memory, saved, opened and asked the same questions, with the answers
/// compared as values. Also the codec's round trip of every case, damaged indexes, and lookups in parallel.
module BulkIndexTests

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

let private plain (bytes: byte array) : Stream = new MemoryStream(bytes)
let private fixture name = plain (fixtureBytes name)
let private utf8 (text: string) = Encoding.UTF8.GetBytes text

let private zipOf (entries: (string * byte array) list) : Stream =
    let memory = new MemoryStream()

    do
        use archive = new ZipArchive(memory, ZipArchiveMode.Create, true)

        for name, bytes in entries do
            use content = archive.CreateEntry(name).Open()
            content.Write(bytes, 0, bytes.Length)

    memory.Position <- 0L
    memory

let private xlsxOf (prefix: string) =
    zipOf
        [
            "xl/worksheets/sheet1.xml", fixtureBytes $"{prefix}-sheet1.xml"
            "xl/sharedStrings.xml", fixtureBytes $"{prefix}-sharedStrings.xml"
        ]

let private date =
    {
        DownloadedAt = DateTimeOffset(2026, 11, 8, 9, 30, 0, TimeSpan.FromHours 2.)
        AsOf = ValueSome(DateOnly(2026, 11, 7))
    }

let private gb = Jurisdiction.ofCode "GB"

let private built (result: Task<Result<ICompanyRegistry, RegistryFailure>>) =
    task {
        match! result with
        | Ok registry -> return registry
        | Error failure -> return failwith (RegistryFailure.describe failure)
    }

let private argentinaZip () =
    zipOf
        [
            "igj-entidades-202607.csv",
            utf8
                "numero_correlativo,tipo_societario,descripcion_tipo_societario,razon_social,dada_de_baja,codigo_baja,detalle_baja,cuit\r\n365,40,SOCIEDAD DE RESPONSABILIDAD LIMITADA,CUFAROIL ANTERIOR,,,,30539374814\r\n"
            "igj-domicilios-202607.csv",
            utf8
                "numero_correlativo,tipo_societario,descripcion_tipo_societario,razon_social,tipo_domicilio,descripcion_tipo_domicilio,calle,numero,localidad,provincia,codigo_postal\r\n"
            "igj-autoridades-202607.csv",
            utf8
                "numero_correlativo,apellido_nombre,tipo_administrador,descripcion_tipo_administrador,tipo_documento,descripcion_tipo_documento,numero_documento,genero_autoridad\r\n"
            "igj-entidades-202609.csv", fixtureBytes "argentina-entidades.csv"
            "igj-domicilios-202609.csv", fixtureBytes "argentina-domicilios.csv"
            "igj-autoridades-202609.csv", fixtureBytes "argentina-autoridades.csv"
        ]

let private alaskaWith (officials: byte array) =
    BulkSources.alaskaAsync (fixture "alaska-corporations.csv") (ValueSome(plain officials)) date ct

/// Each source's register built from its fixture, by a name: the BulkCatalogue id, or the id and a variant.
let private source (name: string) : string * Task<Result<ICompanyRegistry, RegistryFailure>> =
    match name with
    | "EE" ->
        "EE",
        BulkSources.estoniaAsync
            (zipOf [ "ettevotja_rekvisiidid__yldandmed.json", fixtureBytes "estonia-general.json" ])
            (ValueSome(
                zipOf
                    [
                        "ettevotja_rekvisiidid__kaardile_kantud_isikud.json", fixtureBytes "estonia-persons.json"
                    ]
            ))
            (ValueSome(
                zipOf
                    [
                        "ettevotja_rekvisiidid__osanikud.json", fixtureBytes "estonia-shareholders.json"
                    ]
            ))
            (ValueSome(
                zipOf
                    [
                        "ettevotja_rekvisiidid__kasusaajad.json", fixtureBytes "estonia-beneficial-owners.json"
                    ]
            ))
            date
            ct
    | "CY" ->
        "CY",
        BulkSources.cyprusAsync
            (fixture "cyprus-organisations.csv")
            (ValueSome(fixture "cyprus-registered-office.csv"))
            (ValueSome(fixture "cyprus-officials.csv"))
            date
            ct
    | "RO" ->
        "RO",
        BulkSources.romaniaAsync
            (fixture "romania-firms.csv")
            (fixture "romania-states.csv")
            (ValueSome(fixture "romania-state-names.csv"))
            (ValueSome(fixture "romania-representatives.csv"))
            date
            ct
    | "SI" -> "SI", BulkSources.sloveniaAsync (fixture "slovenia-opsiprs.csv") date ct
    | "BE" ->
        "BE",
        BulkSources.belgiumAsync
            (zipOf
                [
                    "meta.csv", fixtureBytes "belgium-meta.csv"
                    "code.csv", fixtureBytes "belgium-code.csv"
                    "enterprise.csv", fixtureBytes "belgium-enterprise.csv"
                    "denomination.csv", fixtureBytes "belgium-denomination.csv"
                    "address.csv", fixtureBytes "belgium-address.csv"
                ])
            date
            ct
    | "UA" -> "UA", BulkSources.ukraineAsync (zipOf [ "UO.xml", fixtureBytes "ukraine-uo.xml" ]) date ct
    | "MD" -> "MD", BulkSources.moldovaAsync (xlsxOf "moldova") date ct
    | "RS" -> "RS", BulkSources.serbiaAsync (fixture "serbia-companies.json") date ct
    | "BA-BRC" -> "BA-BRC", BulkSources.bosniaBrckoAsync (fixture "brcko-opendata.xml") date ct
    | "RU-SME" ->
        "RU-SME",
        BulkSources.russiaSmeAsync
            (zipOf
                [
                    "VO_RRMSPSV_0000_9965_20261010_000c48be-5368-4e9c-a9d2-bb1e5ff316b4.xml",
                    fixtureBytes "russia-rsmp.xml"
                ])
            date
            ct
    | "US-FL" ->
        "US-FL",
        BulkSources.floridaAsync
            [
                zipOf [ "cordata0.txt", fixtureBytes "florida-cordata.txt" ]
                fixture "florida-daily.txt"
            ]
            date
            ct
    | "US-FL repeated" ->
        "US-FL",
        BulkSources.floridaAsync
            [
                zipOf
                    [
                        "cordata0.txt", fixtureBytes "florida-cordata.txt"
                        "cordata1.txt", fixtureBytes "florida-cordata.txt"
                    ]
            ]
            date
            ct
    | "US-AK" -> "US-AK", alaskaWith (fixtureBytes "alaska-officials.csv")
    | "US-AK broken row" ->
        "US-AK",
        alaskaWith (
            Array.append
                (fixtureBytes "alaska-officials.csv")
                (utf8 "\"473D\",\"GOLDSTEIN IMPROVEMENT COMPANY\",\"Business Corporation\",,\"TWO\",\"PERSON\"\n")
        )
    | "US-AK row without company" ->
        "US-AK", alaskaWith (Array.append (fixtureBytes "alaska-officials.csv") (utf8 "\"\n"))
    | "PY" -> "PY", BulkSources.paraguayAsync [ zipOf [ "ruc0.txt", fixtureBytes "paraguay-ruc0.txt" ] ] date ct
    | "PY malformed" ->
        "PY",
        BulkSources.paraguayAsync
            [
                zipOf
                    [
                        "ruc0.txt",
                        Array.append (fixtureBytes "paraguay-ruc0.txt") (utf8 "80000171|BROKEN S.A.|9||ACTIVO|\n")
                    ]
            ]
            date
            ct
    | "BR" ->
        "BR",
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
    | "AU-ASIC" ->
        "AU-ASIC",
        BulkSources.asicCompaniesAsync (zipOf [ "COMPANY_202611.csv", fixtureBytes "asic-companies.csv" ]) date ct
    | "CL-RES" -> "CL-RES", BulkSources.chileAsync [ fixture "chile-2013.csv" ] date ct
    | "CL-SII" ->
        "CL-SII",
        BulkSources.chileSiiAsync
            (zipOf
                [
                    "CODIGO_TIPO_SUBTIPO_.txt", fixtureBytes "chile-subtipos.txt"
                    "PUB_NOMBRES_PJ.txt", fixtureBytes "chile-nombres.txt"
                ])
            (zipOf [ "PUB_NOM_DOMICILIO.txt", fixtureBytes "chile-domicilio.txt" ])
            date
            ct
    | "AR-C" -> "AR-C", BulkSources.argentinaCabaAsync (argentinaZip ()) date ct
    | "PE" -> "PE", BulkSources.peruAsync (zipOf [ "padron_reducido_ruc.txt", fixtureBytes "peru-padron.txt" ]) date ct
    | "EC" -> "EC", BulkSources.ecuadorAsync (xlsxOf "ecuador") date ct
    | other -> failwith other

/// A fresh folder under the test run's temp directory, removed by the caller.
let private freshFolder () =
    Path.Combine(Path.GetTempPath(), "cdd-index-" + Guid.NewGuid().ToString "N")

let private removeFolder (folder: string) =
    if Directory.Exists folder then
        Directory.Delete(folder, true)

let private dispose (registry: ICompanyRegistry) =
    match registry with
    | :? IDisposable as held -> held.Dispose()
    | _ -> ()

let private saved (id: string) (registry: ICompanyRegistry) (folder: string) =
    task {
        match! BulkIndexes.saveAsync id registry folder ct with
        | Ok() -> ()
        | Error failure -> failwith (RegistryFailure.describe failure)

        match! BulkIndexes.openAsync folder ct with
        | Ok opened -> return opened
        | Error failure -> return failwith (RegistryFailure.describe failure)
    }

let private same (what: string) (expected: 'T) (actual: 'T) =
    if expected <> actual then
        failwith $"%s{what}: the index answered\n%A{actual}\nand the registry\n%A{expected}"

/// The numbers to ask a register for: every key it answers for differently from an absent one, each record's own number, each key's last word
/// (a CUI or an RUC behind its prefix), and numbers no register holds.
let private numbersOf (registry: ICompanyRegistry) =
    let contents = (registry :?> IBulkContents).Contents

    let keys =
        Seq.concat
            [
                contents.Records.Keys
                contents.Repeated :> string seq
                contents.Withheld.Keys
                contents.Aliases |> Seq.map (fun struct (alias, _) -> alias)
            ]
        |> List.ofSeq

    [
        yield! keys
        yield! keys |> List.map (fun key -> key.Substring(key.LastIndexOf ' ' + 1))
        yield! contents.Records.Values |> Seq.map _.Company.Number
        yield! [ "0"; "1"; "999999999"; "99999999999999"; "x"; "" ]
    ]
    |> List.distinct

/// The terms to search a register for: names it holds, parts of them, a letter, a term nothing holds and a blank one.
let private termsOf (registry: ICompanyRegistry) =
    let names =
        (registry :?> IBulkContents).Contents.Names
        |> Seq.map (fun struct (_, name) -> name)
        |> Seq.filter (String.IsNullOrWhiteSpace >> not)
        |> List.ofSeq

    [
        yield! names |> List.truncate 10
        yield!
            names
            |> List.truncate 10
            |> List.map (fun name ->
                let first = name.Split(' ')[0]
                first.ToLowerInvariant())
        yield! [ "A"; "E"; "S"; "ZZZ NOTHING HOLDS THIS"; "   "; "\uD800" ]
    ]
    |> List.distinct

/// Asks both registers every question for each number and term, and fails on the first answer that differs.
let private sameAnswers
    (memory: ICompanyRegistry)
    (disk: ICompanyRegistry)
    (numbers: string list)
    (terms: string list)
    =
    task {
        let jurisdiction = List.head memory.Coverage.Jurisdictions
        same "Coverage" memory.Coverage disk.Coverage

        for number in numbers do
            let! expected = memory.TryFindAsync(number, jurisdiction, ct)
            let! actual = disk.TryFindAsync(number, jurisdiction, ct)
            same $"TryFind {number}" expected actual
            let! expected = memory.OfficersAsync(number, jurisdiction, ct)
            let! actual = disk.OfficersAsync(number, jurisdiction, ct)
            same $"Officers {number}" expected actual

            for purpose in [ OwnerPurpose.CustomerDueDiligence; OwnerPurpose.PublicRead ] do
                let! expected = memory.OwnersAsync(number, jurisdiction, purpose, ct)
                let! actual = disk.OwnersAsync(number, jurisdiction, purpose, ct)
                same $"Owners {number} {purpose}" expected actual

        for term in terms do
            let! expected = memory.SearchAsync(term, jurisdiction, ct)
            let! actual = disk.SearchAsync(term, jurisdiction, ct)
            same $"Search {term}" expected actual

            // Contains answers as SearchAsync, and each mode answers alike in memory and from disk.
            let! contains =
                Registries.searchByNameAsync memory term NameMatch.Contains jurisdiction ct

            same $"Contains {term}" expected contains

            for matching in [ NameMatch.Exact; NameMatch.StartsWith; NameMatch.Contains ] do
                let! expected = Registries.searchByNameAsync memory term matching jurisdiction ct
                let! actual = Registries.searchByNameAsync disk term matching jurisdiction ct
                same $"Search {term} {matching}" expected actual

        let! expected = memory.TryFindAsync(List.head numbers, gb, ct)
        let! actual = disk.TryFindAsync(List.head numbers, gb, ct)
        same "outside coverage" expected actual
    }

[<Theory>]
[<InlineData("EE")>]
[<InlineData("CY")>]
[<InlineData("RO")>]
[<InlineData("SI")>]
[<InlineData("BE")>]
[<InlineData("UA")>]
[<InlineData("MD")>]
[<InlineData("RS")>]
[<InlineData("BA-BRC")>]
[<InlineData("RU-SME")>]
[<InlineData("US-FL")>]
[<InlineData("US-FL repeated")>]
[<InlineData("US-AK")>]
[<InlineData("US-AK broken row")>]
[<InlineData("US-AK row without company")>]
[<InlineData("PY")>]
[<InlineData("PY malformed")>]
[<InlineData("BR")>]
[<InlineData("AU-ASIC")>]
[<InlineData("CL-RES")>]
[<InlineData("CL-SII")>]
[<InlineData("AR-C")>]
[<InlineData("PE")>]
[<InlineData("EC")>]
let ``an index answers every question as the registry it was saved from`` (name: string) : Task =
    task {
        let pid, build = source name
        let! memory = built build
        let folder = freshFolder ()

        try
            let! disk = saved pid memory folder

            try
                do! sameAnswers memory disk (numbersOf memory) (termsOf memory)
            finally
                dispose disk
        finally
            removeFolder folder
    }

[<Fact>]
let ``a bulk search matches whole names, starts and contained terms, alike in memory and from its index`` () : Task =
    task {
        let pid, build = source "SI"
        let! memory = built build
        let folder = freshFolder ()
        let si = Jurisdiction.ofCode "SI"

        let namesOf (registry: ICompanyRegistry) (term: string) (matching: NameMatch) =
            task {
                match! Registries.searchByNameAsync registry term matching si ct with
                | Ok { Value = Available result } -> return result.Matches |> List.map _.Name, result.Complete
                | other -> return failwith $"%A{other}"
            }

        try
            let! disk = saved pid memory folder

            try
                Assert.True(memory :? INameMatchSearch)
                Assert.True(disk :? INameMatchSearch)

                let! _, complete = namesOf memory "PODJETJE" NameMatch.Contains

                for registry in [ memory; disk ] do
                    let expect (term: string) (matching: NameMatch) (names: string list) =
                        task {
                            let! found, answeredComplete = namesOf registry term matching
                            Assert.Equal<string list>(names, found)
                            Assert.Equal(complete, answeredComplete)
                        }

                    do! expect "primer podjetje doo" NameMatch.Exact [ "PRIMER PODJETJE d.o.o." ]
                    do! expect "Primer, drugo d.d." NameMatch.Exact [ "PRIMER; DRUGO d.d." ]
                    do! expect "PRIMER" NameMatch.Exact []
                    do! expect "primer" NameMatch.StartsWith [ "PRIMER PODJETJE d.o.o."; "PRIMER; DRUGO d.d." ]
                    do! expect "primer; dr" NameMatch.StartsWith [ "PRIMER; DRUGO d.d." ]
                    do! expect "podjetje" NameMatch.StartsWith []
                    do! expect "podjetje" NameMatch.Contains [ "PRIMER PODJETJE d.o.o." ]
                    // Contains is the term as given inside the upper-cased name, as SearchAsync has always read it.
                    do! expect "podjetje doo" NameMatch.Contains []

                    match! Registries.searchByNameAsync registry "&&" NameMatch.Exact si ct with
                    | Error(RegistryFailure.InvalidRequest _) -> ()
                    | other -> failwith $"%A{other}"
            finally
                dispose disk
        finally
            removeFolder folder
    }

[<Fact>]
let ``every bulk source in the catalogue has a normaliser for its index`` () =
    let catalogue = BulkCatalogue.all |> List.map _.Id |> List.sort
    Assert.Equal<string list>(catalogue, List.sort BulkSourceShape.ids)

[<Fact>]
let ``Ukraine's index written as the file streams answers as the built registry`` () : Task =
    task {
        let! memory =
            built (BulkSources.ukraineAsync (zipOf [ "UO.xml", fixtureBytes "ukraine-uo.xml" ]) date ct)

        let folder = freshFolder ()

        try
            match! BulkIndexes.saveUkraineAsync (zipOf [ "UO.xml", fixtureBytes "ukraine-uo.xml" ]) date folder ct with
            | Ok() -> ()
            | Error failure -> failwith (RegistryFailure.describe failure)

            match! BulkIndexes.openAsync folder ct with
            | Ok disk ->
                try
                    do! sameAnswers memory disk (numbersOf memory) (termsOf memory)
                finally
                    dispose disk
            | Error failure -> failwith (RegistryFailure.describe failure)
        finally
            removeFolder folder
    }

[<Fact>]
let ``Ukraine's streamed index of a file that isn't XML is UnreadableAnswer`` () : Task =
    task {
        let folder = freshFolder ()

        try
            match! BulkIndexes.saveUkraineAsync (zipOf [ "UO.xml", utf8 "not xml <<" ]) date folder ct with
            | Error(RegistryFailure.UnreadableAnswer _) -> ()
            | other -> failwith $"%A{other}"
        finally
            removeFolder folder
    }

[<Fact>]
let ``lookups in parallel answer as one at a time`` () : Task =
    task {
        let pid, build = source "RO"
        let! memory = built build
        let folder = freshFolder ()

        try
            let! disk = saved pid memory folder

            try
                let jurisdiction = List.head memory.Coverage.Jurisdictions
                let numbers = numbersOf memory |> Array.ofList

                let! expected =
                    numbers
                    |> Array.map (fun number -> memory.OfficersAsync(number, jurisdiction, ct))
                    |> Task.WhenAll

                let ask index =
                    Task.Run<Result<Evidence<Availability<Officer list>>, RegistryFailure>>(fun () ->
                        disk.OfficersAsync(numbers[index % numbers.Length], jurisdiction, ct))

                let! actual = Array.init (numbers.Length * 40) ask |> Task.WhenAll

                actual
                |> Array.iteri (fun index answer ->
                    let at = index % numbers.Length
                    same $"Officers {numbers[at]}" expected[at] answer)
            finally
                dispose disk
        finally
            removeFolder folder
    }

// Saving

type private NotBulk() =
    interface ICompanyRegistry with
        member _.Coverage = failwith "not called"
        member _.TryFindAsync(_, _, _) = failwith "not called"
        member _.SearchAsync(_, _, _) = failwith "not called"
        member _.OfficersAsync(_, _, _) = failwith "not called"
        member _.OwnersAsync(_, _, _, _) = failwith "not called"

[<Fact>]
let ``only a registry BulkSources built, by the source its id names, is saved`` () : Task =
    task {
        let! estonia = built (snd (source "EE"))
        let folder = freshFolder ()

        try
            for id, registry in [ "EE", NotBulk() :> ICompanyRegistry; "CY", estonia; "XX", estonia ] do
                match! BulkIndexes.saveAsync id registry folder ct with
                | Error(RegistryFailure.InvalidRequest _) -> ()
                | other -> failwith $"%s{id}: %A{other}"

            Assert.False(File.Exists(Path.Combine(folder, "manifest.json")))
        finally
            removeFolder folder
    }

[<Fact>]
let ``saving again replaces the index`` () : Task =
    task {
        let! serbia = built (snd (source "RS"))
        let! estonia = built (snd (source "EE"))
        let folder = freshFolder ()

        try
            let! first = saved "RS" serbia folder
            let! second = saved "EE" estonia folder

            try
                Assert.Equal("EE", (BulkStore.readManifest folder |> Result.map _.Source |> Result.defaultValue ""))
                do! sameAnswers estonia second (numbersOf estonia) (termsOf estonia)
            finally
                dispose first
                dispose second
        finally
            removeFolder folder
    }

// Damage

/// A folder holding Serbia's index, for a test to damage.
let private serbiaIndex () =
    task {
        let! serbia = built (snd (source "RS"))
        let folder = freshFolder ()
        let! disk = saved "RS" serbia folder
        dispose disk
        return serbia, folder
    }

let private openFailure (folder: string) =
    task {
        match! BulkIndexes.openAsync folder ct with
        | Ok registry ->
            dispose registry
            return failwith "A damaged index opened"
        | Error failure -> return failure
    }

let private rewriteManifest (folder: string) (change: string -> string) =
    let path = Path.Combine(folder, "manifest.json")
    File.WriteAllText(path, change (File.ReadAllText path))

[<Fact>]
let ``a truncated records file is UnreadableAnswer`` () : Task =
    task {
        let! _, folder = serbiaIndex ()

        try
            let path = Path.Combine(folder, "records.bin")
            let! bytes = File.ReadAllBytesAsync path
            do! File.WriteAllBytesAsync(path, Array.sub bytes 0 (bytes.Length / 2))

            match! openFailure folder with
            | RegistryFailure.UnreadableAnswer reason -> Assert.Contains("records.bin", reason)
            | other -> failwith $"%A{other}"
        finally
            removeFolder folder
    }

[<Fact>]
let ``a records file damaged in place answers UnreadableAnswer, not a wrong answer`` () : Task =
    task {
        let! serbia, folder = serbiaIndex ()

        try
            let path = Path.Combine(folder, "records.bin")
            let! bytes = File.ReadAllBytesAsync path
            do! File.WriteAllBytesAsync(path, Array.create bytes.Length 0xFFuy)

            match! BulkIndexes.openAsync folder ct with
            | Ok disk ->
                try
                    let number = (serbia :?> IBulkContents).Contents.Records.Keys |> Seq.head

                    match! disk.TryFindAsync(number, Jurisdiction.ofCode "RS", ct) with
                    | Error(RegistryFailure.UnreadableAnswer _) -> ()
                    | other -> failwith $"%A{other}"

                    match! disk.SearchAsync("A", Jurisdiction.ofCode "RS", ct) with
                    | Error(RegistryFailure.UnreadableAnswer _) -> ()
                    | other -> failwith $"%A{other}"
                finally
                    dispose disk
            | Error failure -> failwith (RegistryFailure.describe failure)
        finally
            removeFolder folder
    }

[<Fact>]
let ``a folder without a manifest refuses`` () : Task =
    task {
        let! _, folder = serbiaIndex ()

        try
            File.Delete(Path.Combine(folder, "manifest.json"))

            match! openFailure folder with
            | RegistryFailure.RequestRefused _ -> ()
            | other -> failwith $"%A{other}"

            match! openFailure (freshFolder ()) with
            | RegistryFailure.RequestRefused _ -> ()
            | other -> failwith $"%A{other}"
        finally
            removeFolder folder
    }

[<Fact>]
let ``a manifest of another format or source refuses`` () : Task =
    task {
        let! _, folder = serbiaIndex ()

        try
            rewriteManifest folder (fun text -> text.Replace($"\"format\": {BulkCodec.Format},", "\"format\": 999,"))

            match! openFailure folder with
            | RegistryFailure.RequestRefused reason -> Assert.Contains("999", reason)
            | other -> failwith $"%A{other}"

            rewriteManifest folder (fun text ->
                text.Replace("\"format\": 999,", $"\"format\": {BulkCodec.Format},").Replace("\"RS\"", "\"ZZ\""))

            match! openFailure folder with
            | RegistryFailure.RequestRefused reason -> Assert.Contains("ZZ", reason)
            | other -> failwith $"%A{other}"
        finally
            removeFolder folder
    }

[<Fact>]
let ``a missing file or a manifest that isn't JSON is UnreadableAnswer`` () : Task =
    task {
        let! _, folder = serbiaIndex ()

        try
            File.Delete(Path.Combine(folder, "keys.bin"))

            match! openFailure folder with
            | RegistryFailure.UnreadableAnswer reason -> Assert.Contains("keys.bin", reason)
            | other -> failwith $"%A{other}"

            rewriteManifest folder (fun _ -> "{ not json")

            match! openFailure folder with
            | RegistryFailure.UnreadableAnswer _ -> ()
            | other -> failwith $"%A{other}"
        finally
            removeFolder folder
    }

// The codec

let private address: Address =
    {
        Lines = [ "Narva mnt 5"; "" ]
        Locality = ValueSome "Tallinn"
        Region = ValueNone
        PostalCode = ValueSome "10117"
        Country = ValueSome "EESTI"
    }

let private registration: EntityRegistration =
    {
        LegalName = ValueSome "Example OÜ"
        LegalForm = ValueNone
        GoverningLaw = ValueSome "Estonian law"
        Country = ValueSome "EE"
        Register = ValueNone
        RegistrationNumber = ValueSome "10000000"
    }

let private verification: IdentityVerification =
    {
        VerifiedOn = ValueSome(DateOnly(2025, 11, 18))
        VerifiedBy = ValueNone
        VerifierSupervisors = [ "HMRC"; "FCA" ]
        StatementPossibleFrom = ValueSome DateOnly.MinValue
        StatementDueOn = ValueSome DateOnly.MaxValue
        StatementSuppliedOn = ValueNone
        StatementRemovedOn = ValueSome(DateOnly(2026, 1, 1))
        PreferredName = ValueSome "Ann"
    }

let private standings =
    [
        CompanyStanding.Active
        CompanyStanding.ActiveWithConcern "proposal to strike off"
        CompanyStanding.Registered(ValueSome "open")
        CompanyStanding.Registered ValueNone
        CompanyStanding.InInsolvency "liquidation"
        CompanyStanding.WindingUp "being wound up"
        CompanyStanding.Dissolved
        CompanyStanding.Closed "converted"
        CompanyStanding.Unknown ValueNone
        CompanyStanding.Unknown(ValueSome "?")
    ]

let private addressKinds =
    [
        AddressKind.RegisteredOffice
        AddressKind.Service
        AddressKind.PrincipalOffice
        AddressKind.RegisteredAgent
        AddressKind.Postal
        AddressKind.Other "branch"
    ]

let private roles =
    [
        RecordRole.Home
        RecordRole.ForeignRegistration
        RecordRole.Establishment
        RecordRole.NotStated
    ]

let private partyKinds =
    [
        PartyKind.Individual
        PartyKind.LegalEntity
        PartyKind.Withheld
        PartyKind.Unknown ValueNone
        PartyKind.Unknown(ValueSome "trust")
    ]

let private officerRoles =
    [ OfficerRole.Director; OfficerRole.Secretary; OfficerRole.Other "chair" ]

let private controlKinds =
    [
        ControlKind.Ownership
        ControlKind.VotingRights
        ControlKind.AppointsBoard
        ControlKind.SignificantInfluence
        ControlKind.Other
    ]

let private gaps =
    [
        OwnershipGap.OwnerNotIdentified
        OwnershipGap.OwnerDetailsNotConfirmed
        OwnershipGap.InquiriesIncomplete
        OwnershipGap.OwnerNotCooperating
        OwnershipGap.OwnerWithheld
        OwnershipGap.OwnerUnreadable
        OwnershipGap.EntityNotIdentified
        OwnershipGap.StatementContradictsOwners
        OwnershipGap.StatementNotUnderstood "code 9"
    ]

let private meanings =
    [
        OwnerStatementMeaning.NoOwnerToList
        OwnerStatementMeaning.AllOwnersIdentified
        OwnerStatementMeaning.ChangesOnly
    ]
    @ (gaps |> List.map OwnerStatementMeaning.LeavesGap)

/// Text the codec must keep exactly: empty, null, accented, other scripts, a lone surrogate and a long run that deflates.
let private texts =
    [
        ""
        null
        "Café Ελλάδα Київ 東京 😀"
        "lone \uD800 surrogate"
        "\uDC00"
        String('x', 5000)
    ]

let private owner index : Owner =
    {
        Name =
            if index % 3 = 0 then
                ValueNone
            else
                ValueSome texts[index % texts.Length]
        Kind = partyKinds[index % partyKinds.Length]
        Control =
            controlKinds
            |> List.mapi (fun at kind ->
                {
                    Kind = kind
                    Band =
                        if at % 2 = 0 then
                            ValueSome { From = 25; To = 50 }
                        else
                            ValueNone
                    AsPublished = $"control {at}"
                })
        Current = index % 2 = 0
        Since = ValueSome(DateOnly(2020, 1, 1 + index % 28))
        EndedOn = ValueNone
        BornIn =
            ValueSome
                {
                    Year = 1970 + index
                    Month = 1 + index % 12
                }
        Nationality = ValueSome "Estonian"
        CountryOfResidence = ValueNone
        Address = if index % 2 = 0 then ValueSome address else ValueNone
        Entity = if index % 2 = 1 then ValueSome registration else ValueNone
        IdentityVerification = if index % 2 = 0 then ValueSome verification else ValueNone
    }

let private officer index : Officer =
    {
        Name = texts[index % texts.Length]
        Kind = partyKinds[index % partyKinds.Length]
        Role = officerRoles[index % officerRoles.Length]
        Current = index % 2 = 1
        AppointedOn = ValueSome(DateOnly(1999, 12, 31))
        EndedOn =
            if index % 2 = 0 then
                ValueSome(DateOnly(2001, 2, 3))
            else
                ValueNone
        BornIn =
            if index % 2 = 0 then
                ValueSome { Year = 1960; Month = 7 }
            else
                ValueNone
        Nationality = ValueNone
        CountryOfResidence = ValueSome "Latvia"
        Address = ValueSome address
        Entity = if index % 3 = 0 then ValueSome registration else ValueNone
        IdentityVerification = if index % 3 = 1 then ValueSome verification else ValueNone
    }

let private record index (standing: CompanyStanding) : BulkRecord =
    {
        Company =
            {
                Name = texts[index % texts.Length]
                OtherNames = texts |> List.truncate (index % 4) |> List.filter (isNull >> not)
                Number = $"{index:D8}"
                Jurisdiction = Jurisdiction.ofCode (if index % 2 = 0 then "EE" else "US-DE")
                JurisdictionAsPublished = if index % 2 = 0 then ValueNone else ValueSome "Delaware"
                Standing = standing
                Concerns =
                    {
                        Stated = CompanyConcern.all |> List.truncate (index % 6)
                        Unconfirmed = CompanyConcern.all |> List.skip (index % 6)
                    }
                StatusAsPublished = ValueSome texts[(index + 1) % texts.Length]
                LegalForm = ValueNone
                IncorporatedOn = ValueSome(DateOnly(1900 + index, 1, 1))
                CeasedOn =
                    if index % 2 = 0 then
                        ValueNone
                    else
                        ValueSome(DateOnly(2026, 11, 8))
                Addresses =
                    match index % 3 with
                    | 0 -> Available(addressKinds |> List.map (fun kind -> { Kind = kind; Address = address }))
                    | 1 -> NotAvailable "The file holds no address"
                    | _ ->
                        NeedsEntitlement
                            {
                                Who = "Obliged entities"
                                Route = "Apply to the registrar"
                            }
                Role = roles[index % roles.Length]
                HomeRegistration = if index % 2 = 1 then ValueSome registration else ValueNone
                EstablishmentOf = if index % 4 = 2 then ValueSome "FC000001" else ValueNone
            }
        Officers =
            if index % 3 = 2 then
                ValueNone
            else
                ValueSome(List.init (5 + index % 3) (fun at -> officer (index + at)))
        Owners =
            if index % 4 = 3 then
                ValueNone
            else
                ValueSome
                    {
                        Owners = List.init (index % 5) (fun at -> owner (index + at))
                        Statements =
                            meanings
                            |> List.mapi (fun at meaning ->
                                {
                                    Meaning = meaning
                                    AsPublished = $"statement {at}"
                                })
                        Gaps = gaps |> List.truncate (index % (gaps.Length + 1))
                    }
        Raw = texts[(index + 2) % texts.Length]
    }

let private records = standings |> List.mapi record

[<Fact>]
let ``the codec reads back every case of every type a record holds`` () =
    for value in records do
        same "record" value (BulkCodec.decode BulkCodec.readRecord (BulkCodec.encode BulkCodec.record value))
        let framed = BulkCodec.frame value

        same
            "frame"
            value
            (BulkCodec.unframe
                framed[4]
                (Array.sub framed BulkCodec.FrameHeader (framed.Length - BulkCodec.FrameHeader)))

[<Fact>]
let ``a long record is deflated, a short one kept plain`` () =
    let long =
        { records[0] with
            Raw = String('x', 5000)
        }

    let short =
        { long with
            Company =
                { long.Company with
                    Addresses = NotAvailable ""
                }
            Officers = ValueNone
            Owners = ValueNone
            Raw = ""
        }

    Assert.Equal(1uy, (BulkCodec.frame long)[4])
    Assert.Equal(0uy, (BulkCodec.frame short)[4])

[<Fact>]
let ``the codec reads back every case of coverage, owners, dates and absence`` () =
    let supports =
        [ Support.Supported; Support.Partial "caveat"; Support.NotOffered "reason" ]

    let costs =
        [
            AccessCost.Free
            AccessCost.FreeWithKey "register"
            AccessCost.Paid "contract"
        ]

    let limits =
        [
            RateLimitPolicy.Published(600, TimeSpan.FromMinutes 5., "the source's page")
            RateLimitPolicy.NotPublished(1, TimeSpan.FromSeconds 0.5)
        ]

    for index in 0..8 do
        let coverage: RegistryCoverage =
            {
                Registry = texts[index % texts.Length]
                Jurisdictions =
                    [ Jurisdiction.ofCode "EE"; Jurisdiction.ofCode "US-DE" ]
                    |> List.truncate (index % 3)
                Standing = supports[index % 3]
                Concerns = supports[(index + 1) % 3]
                Addresses = supports[(index + 2) % 3]
                Search = supports[index % 3]
                Officers = supports[(index + 1) % 3]
                Owners = supports[(index + 2) % 3]
                Cost = costs[index % 3]
                RateLimit = limits[index % 2]
                Licence = "CC BY 4.0"
            }

        same
            "coverage"
            coverage
            (BulkCodec.decode BulkCodec.readCoverage (BulkCodec.encode BulkCodec.coverage coverage))

    for owners in
        [
            Available records[0].Owners.Value
            NotAvailable "The file lists no owners"
            NeedsEntitlement
                {
                    Who = "Obliged entities"
                    Route = "Apply"
                }
        ] do
        same "owners" owners (BulkCodec.decode BulkCodec.readOwners (BulkCodec.encode BulkCodec.owners owners))

    for value in
        [
            date
            { date with
                AsOf = ValueNone
                DownloadedAt = DateTimeOffset.MinValue
            }
        ] do
        let decoded =
            BulkCodec.decode BulkCodec.readDate (BulkCodec.encode BulkCodec.date value)

        same "date" value decoded
        Assert.Equal(value.DownloadedAt.Offset, decoded.DownloadedAt.Offset)

    for absence in [ BulkAbsence.NotHeld; BulkAbsence.CantTell "The file leaves some out" ] do
        same "absence" absence (BulkCodec.decode BulkCodec.readAbsence (BulkCodec.encode BulkCodec.absence absence))

[<Fact>]
let ``bytes the codec didn't write are refused`` () =
    let bytes = BulkCodec.encode BulkCodec.record records[1]

    Assert.ThrowsAny<IOException>(fun () ->
        BulkCodec.decode BulkCodec.readRecord (Array.sub bytes 0 (bytes.Length - 3))
        |> ignore)
    |> ignore

    Assert.Throws<InvalidDataException>(fun () ->
        BulkCodec.decode BulkCodec.readRecord (Array.append bytes [| 0uy |]) |> ignore)
    |> ignore

    Assert.Throws<InvalidDataException>(fun () -> BulkCodec.decode BulkCodec.readAbsence [| 9uy |] |> ignore)
    |> ignore

[<Fact>]
let ``a damaged length header is refused before its bytes are allocated`` () =
    let bytes =
        BulkCodec.encode (fun writer (length: int) -> writer.Write7BitEncodedInt(length * 2 + 1)) 1_000_000_000

    Assert.Throws<EndOfStreamException>(fun () -> BulkCodec.decode BulkCodec.readText bytes |> ignore)
    |> ignore

// Completeness, damaged keys and keys too long

[<Fact>]
let ``a search over a file that leaves companies out is never Complete, in memory or from its index`` () : Task =
    task {
        for id, term, complete in [ "RS", "simplon", false; "RO", "expert", true ] do
            let pid, build = source id
            let! memory = built build
            let folder = freshFolder ()

            try
                let! disk = saved pid memory folder

                try
                    for registry in [ memory; disk ] do
                        match! registry.SearchAsync(term, List.head memory.Coverage.Jurisdictions, ct) with
                        | Ok { Value = Available result } ->
                            Assert.NotEmpty result.Matches
                            Assert.Equal(complete, result.Complete)
                        | other -> failwith $"%A{other}"

                    let manifest = (BulkStore.readManifest folder |> Result.toOption).Value
                    Assert.Equal(not complete, manifest.Excluded)
                finally
                    dispose disk

                // An index whose manifest says nothing is left out still isn't Complete over a file that leaves companies out.
                rewriteManifest folder (fun text -> text.Replace("\"excluded\": true", "\"excluded\": false"))

                match! BulkIndexes.openAsync folder ct with
                | Ok disk ->
                    try
                        match! disk.SearchAsync(term, List.head memory.Coverage.Jurisdictions, ct) with
                        | Ok { Value = Available result } -> Assert.Equal(complete, result.Complete)
                        | other -> failwith $"%A{other}"
                    finally
                        dispose disk
                | Error failure -> failwith (RegistryFailure.describe failure)
            finally
                removeFolder folder
    }

[<Fact>]
let ``a key whose offset points at another record answers UnreadableAnswer, not that company`` () : Task =
    task {
        let! _, folder = serbiaIndex ()

        try
            let manifest = (BulkStore.readManifest folder |> Result.toOption).Value
            let width = 2 + manifest.KeyWidth + BulkKeyEntry.Tail
            let tail = 2 + manifest.KeyWidth
            let path = Path.Combine(folder, "keys.bin")
            let! bytes = File.ReadAllBytesAsync path

            let offsetOf entry =
                BitConverter.ToInt64(bytes, entry * width + tail)

            let held =
                [ 0 .. int manifest.Keys - 1 ]
                |> List.filter (fun entry -> offsetOf entry >= 0L)
                |> List.distinctBy offsetOf

            let first, second = held[0], held[1]

            let keyOf entry =
                Encoding.UTF8.GetString(bytes, entry * width + 2, int (BitConverter.ToUInt16(bytes, entry * width)))

            // The first key's offset is overwritten with the second's, keeping the first key's check.
            Array.blit bytes (second * width + tail) bytes (first * width + tail) 8
            do! File.WriteAllBytesAsync(path, bytes)

            match! BulkIndexes.openAsync folder ct with
            | Ok disk ->
                try
                    match! disk.TryFindAsync(keyOf first, Jurisdiction.ofCode "RS", ct) with
                    | Error(RegistryFailure.UnreadableAnswer _) -> ()
                    | other -> failwith $"%A{other}"

                    match! disk.TryFindAsync(keyOf second, Jurisdiction.ofCode "RS", ct) with
                    | Ok { Value = Available(ValueSome _) } -> ()
                    | other -> failwith $"%A{other}"
                finally
                    dispose disk
            | Error failure -> failwith (RegistryFailure.describe failure)
        finally
            removeFolder folder
    }

[<Fact>]
let ``a key longer than an index holds fails the save, not with an exception`` () : Task =
    task {
        let! serbia = built (snd (source "RS"))
        let contents = (serbia :?> IBulkContents).Contents
        let record = contents.Records.Values |> Seq.head
        let folder = freshFolder ()

        let long =
            new BulkRegistry(
                contents.Coverage,
                contents.Normal,
                contents.InvalidNumber,
                readOnlyDict [ String('1', 70_000), record ],
                contents.Date,
                contents.Absence,
                contents.OfficersNotOffered,
                contents.Owners
            )

        try
            match! BulkIndexes.saveAsync "RS" long folder ct with
            | Error(RegistryFailure.Unavailable reason) -> Assert.Contains("key", reason)
            | other -> failwith $"%A{other}"

            Assert.False(File.Exists(Path.Combine(folder, "manifest.json")))
            Assert.Empty(Directory.GetFiles(folder, "*.tmp"))
        finally
            removeFolder folder
    }

// Numbers a file writes in more than one form

[<Fact>]
let ``Romania's index can't tell a registration number absent, and holds no CUI it lacks, as the registry answers``
    ()
    : Task =
    task {
        let pid, build = source "RO"
        let! memory = built build
        let folder = freshFolder ()
        let ro = List.head memory.Coverage.Jurisdictions

        try
            let! disk = saved pid memory folder

            try
                for registry in [ memory; disk :> ICompanyRegistry ] do
                    for number in [ "J40/3266/2019"; "J1991001116400"; "J40/99999/2020" ] do
                        match! registry.TryFindAsync(number, ro, ct) with
                        | Ok { Value = NotAvailable reason } -> Assert.Contains("two forms", reason)
                        | other -> failwith $"%s{number}: %A{other}"

                    match! registry.TryFindAsync("99999999", ro, ct) with
                    | Ok { Value = Available ValueNone } -> ()
                    | other -> failwith $"%A{other}"

                    match! registry.TryFindAsync("J2019003266407", ro, ct) with
                    | Ok { Value = Available(ValueSome company) } -> Assert.Equal("J2019003266407", company.Number)
                    | other -> failwith $"%A{other}"
            finally
                dispose disk
        finally
            removeFolder folder
    }
