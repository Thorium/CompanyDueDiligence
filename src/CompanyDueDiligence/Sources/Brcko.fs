namespace CompanyDueDiligence

open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions
open System.Xml
open System.Xml.Linq

/// The e-Registar of Brčko District of Bosnia and Herzegovina, read from its open data export
/// (bizreg.osbd.ba/Public/PublicPortal/OpenData?handler=ExportXML) as downloaded on 2026-11-08.
module internal Brcko =

    let jurisdiction = Jurisdiction.ofCode "BA-BRC"

    let coverage: RegistryCoverage =
        {
            Registry = "e-Registar, Brčko District (Bosnia and Herzegovina)"
            Jurisdictions = [ jurisdiction ]
            Standing = Support.Supported
            Concerns = Support.NotOffered "The export states no filing dates, disputes or insolvency"
            Addresses = Support.Supported
            Search = Support.Partial "Names containing the term, over the file, up to 50"
            Officers = Support.NotOffered "The export lists no representatives"
            Owners = Support.NotOffered "The export lists no founders or members"
            Cost = AccessCost.Free
            RateLimit = RateLimitPolicy.NotPublished(1, TimeSpan.FromDays 1.)
            Licence =
                "None stated. The portal's footer says \"Sva prava zadržana © Pravosudna komisija Brčko distrikta Bosne i Hercegovine\" (all rights reserved); its \"Uslovi i pravila\" weren't read"
        }

    let invalidNumber =
        "A Brčko registration number (MBS) reads like 1-369 or 96-05-0029-13; a tax number (JIB) has 13 digits"

    let private mbsRegex = Regex @"\A(\d{1,2}-\d{1,5}|\d{2}-\d{2}-\d{4}-\d{2})\z"

    /// The key of a registration number (MBS) or a 13-digit tax number (JIB).
    let normalNumber (number: string) =
        let text = if isNull number then "" else number.Trim()

        if text.Length = 13 && text |> Seq.forall Char.IsAsciiDigit then
            ValueSome("JIB " + text)
        elif mbsRegex.IsMatch text then
            ValueSome("MBS " + text)
        else
            ValueNone

    /// The four Status words in the export of 2026-11-08. Any other is Unknown.
    let standing (word: string voption) =
        match word |> ValueOption.map _.Trim() with
        | ValueNone
        | ValueSome "" -> CompanyStanding.Unknown ValueNone
        | ValueSome "Aktivan" -> CompanyStanding.Active
        | ValueSome("U procesu izmjene" as word)
        | ValueSome("U procesu žalbe" as word) -> CompanyStanding.Registered(ValueSome word)
        | ValueSome("U procesu prestanka" as word) -> CompanyStanding.WindingUp word
        | ValueSome other -> CompanyStanding.Unknown(ValueSome other)

    let private field (e: XElement) (name: string) =
        match e.Element(XName.Get name) with
        | null -> ValueNone
        | child -> BulkFile.nonBlank child.Value

    /// One PoslovniSubjekat with the keys it is found by, its registration number and any tax number, or ValueNone when it has no name or
    /// registration number.
    let record (e: XElement) : struct (BulkRecord * string list) voption =
        let mbs = field e "MBS"

        match mbs |> ValueOption.filter mbsRegex.IsMatch, field e "NazivFirme" with
        | ValueSome mbs, ValueSome name ->
            let status = field e "Status"
            let standing = standing status
            let street = field e "Ulica"

            let number = field e "Broj" |> ValueOption.filter (fun n -> n <> "-")

            let line =
                match street, number with
                | ValueSome street, ValueSome number -> ValueSome $"{street} {number}"
                | ValueSome street, ValueNone -> ValueSome street
                | ValueNone, number -> number

            let jib =
                match field e "PoreskiBroj" with
                | ValueSome jib when jib.Length = 13 && jib |> Seq.forall Char.IsAsciiDigit -> [ "JIB " + jib ]
                | _ -> []

            let record =
                {
                    Company =
                        {
                            Name = name
                            OtherNames = []
                            Number = mbs
                            Jurisdiction = jurisdiction
                            JurisdictionAsPublished = field e "Općina"
                            Standing = standing
                            Concerns = CompanyConcerns.notPublished
                            StatusAsPublished = status
                            LegalForm = field e "OblikOrganizovanja"
                            IncorporatedOn = BulkFile.date "dd.MM.yyyy." (field e "DatumRegistracije")
                            CeasedOn = ValueNone
                            Addresses =
                                match
                                    Bulk.companyAddress
                                        (AddressKind.Other "Sjedište")
                                        [ line ]
                                        (field e "NaseljenoMjesto")
                                        (field e "Općina")
                                        ValueNone
                                        ValueNone
                                with
                                | ValueSome address -> Available [ address ]
                                | ValueNone -> NotAvailable "The record holds no address"
                            Role = RecordRole.Home
                            HomeRegistration = ValueNone
                            EstablishmentOf = ValueNone
                        }
                    Officers = ValueNone
                    Owners = ValueNone
                    Raw = e.ToString SaveOptions.DisableFormatting
                }

            ValueSome(struct (record, ("MBS " + mbs) :: jib))
        | _ -> ValueNone

    /// Builds the register from OpenData.xml as exported: <OpenData> holding one <PoslovniSubjekat> per registered subject.
    let build (file: Stream) (date: BulkFileDate) (ct: Threading.CancellationToken) : ICompanyRegistry =
        let settings =
            XmlReaderSettings(DtdProcessing = DtdProcessing.Prohibit, IgnoreComments = true)

        use reader = XmlReader.Create(file, settings)
        reader.MoveToContent() |> ignore

        if reader.LocalName <> "OpenData" then
            BulkFile.fail "The file isn't an OpenData export"

        let index = BulkIndex<BulkRecord>()

        // Subjects read, and those without a name or number.
        let struct (subjects, skipped) =
            BulkFile.elements "PoslovniSubjekat" reader
            |> Seq.fold
                (fun (struct (subjects, skipped)) subject ->
                    ct.ThrowIfCancellationRequested()

                    match record subject with
                    | ValueSome(struct (record, keys)) ->
                        index.AddEach(keys, record)
                        struct (subjects + 1, skipped)
                    | ValueNone -> struct (subjects + 1, skipped + 1))
                (struct (0, 0))

        new BulkRegistry(
            coverage,
            normalNumber,
            invalidNumber,
            index.Held,
            date,
            BulkAbsence.CantTell(
                $"The export lists subjects on the register now, not deleted ones, and {skipped} of {subjects} had no name or number; a number it doesn't hold may still have been registered"
            ),
            "The export lists no representatives",
            (fun _ -> NotAvailable "The export lists no founders or members"),
            index.Repeated
        )
