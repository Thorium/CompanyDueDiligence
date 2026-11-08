namespace CompanyDueDiligence

open System
open System.Collections.Generic
open System.IO
open System.Xml
open System.Xml.Linq

/// The Federal Tax Service's unified register of small and medium-sized businesses (единый реестр субъектов МСП), read from its monthly
/// open data zip (nalog.gov.ru/opendata/7707329152-rsmp, structure-12052026.xsd, format version 4.06). Its legal entities are read; individual
/// entrepreneurs are left out.
module internal RussiaSme =

    let jurisdiction = Jurisdiction.ofCode "RU"

    let coverage: RegistryCoverage =
        {
            Registry = "FNS register of small and medium-sized businesses (Russia)"
            Jurisdictions = [ jurisdiction ]
            Standing =
                Support.Partial
                    "Covers small and medium businesses only. Being listed means the FNS counted the company as an operating SME on the file's date; the category (micro, small, medium) is the status word"
            Concerns = Support.NotOffered "The register states no filing dates, disputes or insolvency"
            Addresses = Support.Partial "Region, district, city and settlement only, no street"
            Search = Support.Partial "Names containing the term, over the file, up to 50"
            Officers = Support.NotOffered "The SME register lists no officers"
            Owners = Support.NotOffered "The SME register lists no owners"
            Cost = AccessCost.Free
            RateLimit = RateLimitPolicy.NotPublished(1, TimeSpan.FromDays 1.)
            Licence =
                "FNS open data terms (Типовые условия использования общедоступной информации, размещаемой в форме открытых данных): free use, attribution to the FNS of Russia required"
        }

    let invalidNumber =
        "An OGRN has 13 digits and an INN 10, each ending in a check digit; an entrepreneur's 15-digit OGRNIP or 12-digit INN isn't read"

    /// An OGRN: 13 digits, the last being the first twelve as a number modulo 11, then modulo 10.
    let isOgrn (text: string) =
        text.Length = 13
        && text |> Seq.forall Char.IsAsciiDigit
        && int (Int64.Parse(text.AsSpan(0, 12)) % 11L % 10L) = int text[12] - int '0'

    /// An INN of a legal entity: 10 digits, the last being the first nine weighted 2, 4, 10, 3, 5, 9, 4, 6, 8, summed, modulo 11, then modulo 10.
    let isInn (text: string) =
        text.Length = 10
        && text |> Seq.forall Char.IsAsciiDigit
        && (Seq.zip [ 2; 4; 10; 3; 5; 9; 4; 6; 8 ] text
            |> Seq.sumBy (fun (w, c) -> w * (int c - int '0')))
           % 11
           % 10
            =
            int text[9] - int '0'

    /// The key of an OGRN or an INN.
    let normalNumber (number: string) =
        let text = if isNull number then "" else number.Trim()

        if isOgrn text then ValueSome("OGRN " + text)
        elif isInn text then ValueSome("INN " + text)
        else ValueNone

    /// The three КатСубМСП codes the XSD lists. Any other is Unknown.
    let standing (category: string voption) =
        match category |> ValueOption.map _.Trim() with
        | ValueSome "1" -> CompanyStanding.Registered(ValueSome "Микропредприятие")
        | ValueSome "2" -> CompanyStanding.Registered(ValueSome "Малое предприятие")
        | ValueSome "3" -> CompanyStanding.Registered(ValueSome "Среднее предприятие")
        | ValueNone
        | ValueSome "" -> CompanyStanding.Unknown ValueNone
        | ValueSome other -> CompanyStanding.Unknown(ValueSome other)

    let private attribute (e: XElement) (name: string) =
        match e with
        | null -> ValueNone
        | e ->
            match e.Attribute(XName.Get name) with
            | null -> ValueNone
            | a -> BulkFile.nonBlank a.Value

    let private child (e: XElement) (name: string) =
        match e with
        | null -> null
        | e -> e.Element(XName.Get name)

    /// A place element as "Наим Тип", such as "Тюменская обл.".
    let private place (e: XElement) =
        match attribute e "Наим", attribute e "Тип" with
        | ValueSome name, ValueSome kind -> ValueSome $"{name} {kind}"
        | name, _ -> name

    /// One Документ of a legal entity with its keys, OGRN and INN. An entrepreneur's is ValueNone, as is an entity's without a name or a valid OGRN.
    let record (document: XElement) : struct (BulkRecord * string list) voption =
        let entity = child document "ОргВклМСП"

        match attribute entity "ОГРН" |> ValueOption.filter isOgrn, attribute entity "НаимОрг" with
        | ValueSome ogrn, ValueSome name ->
            let inn = attribute entity "ИННЮЛ" |> ValueOption.filter isInn
            let category = attribute document "КатСубМСП"
            let location = child document "СведМН"

            let settlement =
                match child location "Город" |> place with
                | ValueSome city -> ValueSome city
                | ValueNone -> child location "НаселПункт" |> place

            let district = child location "Район" |> place

            let region =
                child location "Регион"
                |> place
                |> ValueOption.orElse (attribute location "КодРегион")

            let raw =
                [
                    "ДатаСост", attribute document "ДатаСост"
                    "ДатаВклМСП", attribute document "ДатаВклМСП"
                    "ВидСубМСП", attribute document "ВидСубМСП"
                    "КатСубМСП", category
                    "НаимОрг", ValueSome name
                    "НаимОргСокр", attribute entity "НаимОргСокр"
                    "ИННЮЛ", inn
                    "ОГРН", ValueSome ogrn
                    "КодРегион", attribute location "КодРегион"
                    "Регион", child location "Регион" |> place
                    "Район", district
                    "Город", child location "Город" |> place
                    "НаселПункт", child location "НаселПункт" |> place
                ]
                |> List.choose (fun (label, value) ->
                    value |> ValueOption.map (fun v -> $"{label}={v}") |> ValueOption.toOption)
                |> String.concat "\n"

            let record =
                {
                    Company =
                        {
                            Name = name
                            OtherNames = attribute entity "НаимОргСокр" |> ValueOption.toList
                            Number = ogrn
                            Jurisdiction = jurisdiction
                            JurisdictionAsPublished = ValueNone
                            Standing = standing category
                            Concerns = CompanyConcerns.notPublished
                            StatusAsPublished = category |> ValueOption.map (fun code -> $"КатСубМСП {code}")
                            LegalForm = ValueNone
                            IncorporatedOn = ValueNone
                            CeasedOn = ValueNone
                            Addresses =
                                match
                                    Bulk.companyAddress
                                        (AddressKind.Other "Место нахождения")
                                        [ district ]
                                        settlement
                                        region
                                        ValueNone
                                        ValueNone
                                with
                                | ValueSome address -> Available [ address ]
                                | ValueNone -> NotAvailable "The record holds no location"
                            Role = RecordRole.Home
                            HomeRegistration = ValueNone
                            EstablishmentOf = ValueNone
                        }
                    Officers = ValueNone
                    Owners = ValueNone
                    Raw = raw
                }

            ValueSome(struct (record, ("OGRN " + ogrn) :: (inn |> ValueOption.map ((+) "INN ") |> ValueOption.toList)))
        | _ -> ValueNone

    /// Builds the register from the monthly zip as published, holding many XML files of up to a thousand Документ each. ДатаСост is the AsOf.
    /// The zip is over 2 GB: pass a FileStream.
    let build (zip: Stream) (date: BulkFileDate) (ct: Threading.CancellationToken) : ICompanyRegistry =
        use archive = BulkFile.openZip zip

        let settings =
            XmlReaderSettings(DtdProcessing = DtdProcessing.Prohibit, IgnoreComments = true, CloseInput = true)

        let files =
            archive.Entries
            |> Seq.filter (fun entry -> entry.FullName.EndsWith(".xml", StringComparison.OrdinalIgnoreCase))
            |> List.ofSeq

        if files.IsEmpty then
            BulkFile.fail "The zip holds no XML files"

        let documents =
            files
            |> Seq.collect (fun entry ->
                seq {
                    use reader = XmlReader.Create(BulkFile.openEntry entry, settings)
                    reader.MoveToContent() |> ignore

                    if reader.LocalName <> "Файл" then
                        raise (InvalidDataException "A file in the zip isn't an SME register file")

                    yield! BulkFile.elements "Документ" reader
                })

        let index = BulkIndex<BulkRecord>()

        // The first ДатаСост read, and the entities that couldn't be read.
        let struct (stated, skipped) =
            documents
            |> Seq.fold
                (fun (struct (stated, skipped)) document ->
                    ct.ThrowIfCancellationRequested()

                    let stated =
                        stated
                        |> ValueOption.orElseWith (fun () ->
                            BulkFile.date "dd.MM.yyyy" (attribute document "ДатаСост"))

                    match record document with
                    | ValueSome(struct (record, keys)) ->
                        index.AddEach(keys, record)
                        struct (stated, skipped)
                    | ValueNone when not (isNull (child document "ОргВклМСП")) -> struct (stated, skipped + 1)
                    | ValueNone -> struct (stated, skipped))
                (struct (ValueNone, 0))

        BulkRegistry(
            coverage,
            normalNumber,
            invalidNumber,
            index.Held,
            Bulk.asOf date stated,
            BulkAbsence.CantTell(
                $"The register lists small and medium businesses only, so a company not in it may be large, no longer an SME, or not operating; it proves nothing. {skipped} entities couldn't be read"
            ),
            "The SME register lists no officers",
            (fun _ -> NotAvailable "The SME register lists no owners"),
            index.Repeated
        )
