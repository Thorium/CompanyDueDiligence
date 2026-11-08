/// The case file: what a compliance officer works from. It holds what sources said, how things connect, what stands out and what is missing. It has no
/// overall result: the officer decides. A case file is plain data that round-trips through CaseJson, and the host stores it.
namespace CompanyDueDiligence

open System
open System.Text.RegularExpressions

/// Who identifies an entity, and by what: a register and its identifier, never a bare number.
[<RequireQualifiedAccess>]
type EntityIdentity =
    /// A number on a register, with the register's name as first seen and its jurisdiction.
    | OnRegister of register: string * jurisdiction: Jurisdiction * number: string
    /// The German commercial register: a court, a section such as HRB, and a number that is only unique within both.
    | GermanCourtRegister of court: string * section: string * number: string
    | Lei of lei: string
    /// A name with nothing behind it yet, as with a person, or a company named in a filing. The context says where the name was seen.
    | NameOnly of name: string * context: string

module EntityIdentity =

    /// A jurisdiction as a register's scope: GB for any part of the UK, and a US state as itself because each state has its own register.
    let private scope (jurisdiction: Jurisdiction) =
        if jurisdiction.Country = "US" then
            jurisdiction.Code
        else
            jurisdiction.Country

    let private compactRegex = Regex @"\s+"

    let private compact (text: string) =
        compactRegex.Replace(text.Trim().ToUpperInvariant(), "")

    /// The key this identity gives an entity in a case. The same record reached two ways gets one key.
    let key identity =
        match identity with
        | EntityIdentity.OnRegister(_, jurisdiction, number) -> $"{scope jurisdiction}:{compact number}"
        | EntityIdentity.GermanCourtRegister(court, section, number) ->
            $"DE:{compact court}:{compact section}:{compact number}"
        | EntityIdentity.Lei lei -> $"LEI:{compact lei}"
        | EntityIdentity.NameOnly(name, context) ->
            $"NAME:{NameMatching.normalise name}:{NameMatching.normalise context}"

    /// One line for a person to read.
    let describe identity =
        match identity with
        | EntityIdentity.OnRegister(register, jurisdiction, number) -> $"{number} on {register} ({jurisdiction.Code})"
        | EntityIdentity.GermanCourtRegister(court, section, number) -> $"{section} {number} at the court of {court}"
        | EntityIdentity.Lei lei -> $"LEI {lei}"
        | EntityIdentity.NameOnly(name, context) -> $"{name}, named in {context}, with no identifier"

/// One party in a case. A UK record, a Delaware home record and a Texas registration of one business are three entities, joined by links, because each has
/// its own standing.
type CaseEntity =
    {
        Key: string
        Name: string
        Party: PartyKind
        Identities: EntityIdentity list
        Jurisdiction: Jurisdiction voption
        /// What this record is to the business it describes.
        Role: RecordRole
        /// The standing on this record's own register, as last read. ValueNone when no register has been read for it.
        Standing: CompanyStanding voption
        StatusAsPublished: string voption
    }

/// Who entered something by hand, when, and from what.
type Attribution =
    {
        By: string
        At: DateTimeOffset
        /// Where they got it: a document, a call, a web page.
        Source: string
    }

/// What a link is. HomeRecord points from a foreign registration or overseas record to the home record; ForeignRegistration from the home record to a
/// registration elsewhere; UkEstablishment from the overseas company to its establishment; Owner, Officer and Parent from a company to that party.
[<RequireQualifiedAccess>]
type LinkKind =
    | HomeRecord
    | ForeignRegistration
    | UkEstablishment
    | Owner
    | Officer
    | Parent
    | Creditor
    /// A relationship stated in those words.
    | StatedRelationship of words: string

/// Where a link comes from. Only OperatorDeclared comes with no evidence behind it, and it never reads as evidenced.
[<RequireQualifiedAccess>]
type LinkProvenance =
    /// A register's record says so.
    | Register of register: string
    /// A filing says so, as read into evidence.
    | FilingDocument of documentId: string * evidenceId: string
    | Vendor of vendor: string * reference: string
    /// GLEIF's reported data, which is a cross-check and not a register.
    | CrossCheck of source: string
    /// An officer says so.
    | OperatorDeclared of Attribution

type CaseLink =
    {
        Id: string
        From: string
        To: string
        Kind: LinkKind
        Provenance: LinkProvenance
        /// The evidence the link rests on, none for one declared.
        EvidenceIds: string list
        Note: string voption
    }

module CaseLink =

    /// False for a link an officer declared with nothing behind it.
    let isEvidenced (link: CaseLink) =
        match link.Provenance with
        | LinkProvenance.OperatorDeclared _ -> false
        | LinkProvenance.Register _
        | LinkProvenance.FilingDocument _
        | LinkProvenance.Vendor _
        | LinkProvenance.CrossCheck _ -> true

    let describeKind kind =
        match kind with
        | LinkKind.HomeRecord -> "home record"
        | LinkKind.ForeignRegistration -> "foreign registration"
        | LinkKind.UkEstablishment -> "UK establishment"
        | LinkKind.Owner -> "owner"
        | LinkKind.Officer -> "officer"
        | LinkKind.Parent -> "parent"
        | LinkKind.Creditor -> "creditor"
        | LinkKind.StatedRelationship words -> $"stated relationship: {words}"

    let describeProvenance provenance =
        match provenance with
        | LinkProvenance.Register register -> $"register: {register}"
        | LinkProvenance.FilingDocument(document, evidence) -> $"filing document {document}, read as {evidence}"
        | LinkProvenance.Vendor(vendor, reference) -> $"vendor: {vendor}, reference {reference}"
        | LinkProvenance.CrossCheck source -> $"cross-check: {source}"
        | LinkProvenance.OperatorDeclared who ->
            $"declared by {who.By} on {who.At:``yyyy-MM-dd``} from {who.Source}, not evidenced"

/// Who said a piece of evidence.
[<RequireQualifiedAccess>]
type EvidenceOrigin =
    | Register
    | Vendor
    /// GLEIF's reported data.
    | CrossCheck
    | SanctionsList
    /// What a filing said, read by a person or a reader.
    | FilingDocument
    /// An officer's own finding.
    | Operator

/// A broad reading of a status, so that sources with different words can be compared without treating every wording as a contradiction.
[<RequireQualifiedAccess>]
type StatusReading =
    | Live
    /// A proceeding such as administration, or winding up.
    | InProceeding
    | Ended
    | NotPlaced

/// One thing a source states about an entity, in a form sources can be compared by.
[<RequireQualifiedAccess>]
type Fact =
    | Name of string
    /// A name held besides the current one, with what kind of name it is, such as a former name. Sources aren't compared on it, and it is screened.
    | OtherName of kind: string * name: string
    | RegistrationNumber of string
    | Status of asPublished: string * reading: StatusReading
    | Address of kind: AddressKind * text: string * postalCode: string voption
    | LegalForm of string
    | Capital of CapitalStatement
    /// A current officer.
    | Officer of name: string
    /// Born in, for a person, as a source publishes it.
    | BornIn of YearMonth
    /// A country a source ties a person to.
    | Country of string
    /// Anything else, by topic.
    | Other of topic: string * text: string

type CaseEvidence =
    {
        Id: string
        /// The key of the entity it is about.
        About: string
        /// Who said it: a register's name, a vendor and product, a person.
        Source: string
        Origin: EvidenceOrigin
        /// What was read, such as "company record" or "officers".
        Topic: string
        RetrievedAt: DateTimeOffset
        /// The data's own date, where the source gives one.
        AsOf: DateOnly voption
        /// The answer as received. It can hold personal data.
        Raw: string voption
        Facts: Fact list
        /// Who entered it, for evidence entered by hand.
        Attribution: Attribution voption
        /// The case document it reads, if any.
        DocumentId: string voption
        Note: string voption
    }

/// What a flag is about. Each is something to show, and none of them is an allegation.
[<RequireQualifiedAccess>]
type FlagKind =
    | StatusConcern
    | OverdueFiling
    | Insolvency
    | RegisteredOfficeProblem
    | SharedAddress
    | VirtualAddress
    | NominalCapital
    | IdentityVerificationPastDue
    | Disagreement
    | NotOnRegister
    | PossibleSanctionsMatch
    /// A vendor's hit that isn't one of the kinds above.
    | VendorHit
    /// An officer's own flag.
    | OperatorFlag

/// Something that stands out about one entity, in neutral words, with where it came from.
type CaseFlag =
    {
        Id: string
        On: string
        Kind: FlagKind
        Statement: string
        Source: string
        EvidenceIds: string list
    }

/// What kind of hole a gap is.
[<RequireQualifiedAccess>]
type GapKind =
    /// Could be checked and wasn't, or hasn't been.
    | NotChecked
    /// No source here reads this, and no route is declared for it.
    | NoRoute
    | NeedsPaidContract
    | NeedsEntitlement
    /// The source was asked and failed, or answered something unreadable.
    | SourceFailed
    /// The source here for this country doesn't offer it.
    | NotAvailableForCountry
    /// The source says it doesn't publish this, or that its answer can't settle it.
    | NotPublished

/// How far a gap reaches. WholeCase means the picture of the business is incomplete because of it, as with an unchecked parent.
[<RequireQualifiedAccess>]
type GapReach =
    | WholeCase
    | ThisEntity

/// Something the case doesn't know, and what would close it.
type CaseGap =
    {
        Id: string
        /// The key of the entity it concerns.
        About: string voption
        Reach: GapReach
        Kind: GapKind
        /// What is missing, such as "owners" or "company record".
        Subject: string
        Detail: string
        WouldClose: string
        /// Set when an officer closes it, with the evidence that does.
        ClosedBy: (Attribution * string) voption
    }

/// What a filing is, for deciding what to read first.
[<RequireQualifiedAccess>]
type DocumentKind =
    | Insolvency
    /// Incorporation, constitution, resolutions, capital and overseas filings.
    | Constitutional
    | Other of category: string voption

/// A filing to read. The file can be scanned images, so the case holds its link and what it is, and an officer or a reader says what it contains.
type CaseDocument =
    {
        Id: string
        /// The key of the entity it was filed for.
        Entity: string
        Register: string
        /// The register's id to read the content by, if it has one.
        DocumentId: string voption
        TransactionId: string
        Kind: DocumentKind
        FormType: string voption
        Description: string voption
        FiledOn: DateOnly voption
        Pages: int voption
        Link: string voption
    }

module CaseDocument =

    /// Insolvency and constitutional filings are the ones to read first.
    let isHighlighted (document: CaseDocument) =
        match document.Kind with
        | DocumentKind.Insolvency
        | DocumentKind.Constitutional -> true
        | DocumentKind.Other _ -> false

type CaseNote =
    {
        Id: string
        /// The key of the entity it concerns.
        About: string voption
        Text: string
        Attribution: Attribution
    }

/// A vendor's hit, in the vendor's words.
type VendorHit =
    {
        /// Who it concerns, in the vendor's words.
        Subject: string
        Words: string
        Detail: string voption
        /// The kind of flag to show it as. ValueNone is FlagKind.VendorHit.
        ShowAs: FlagKind voption
    }

/// Something a vendor says it didn't check.
type VendorGap =
    {
        /// Who it concerns, in the vendor's words.
        Subject: string
        Words: string
        WouldClose: string voption
    }

/// A vendor's check, in terms the host fills in from its own vendor client. This package references no vendor client. OutcomeWords stay the vendor's: a
/// "Clear" here is that vendor's result on that date, and closes nothing else in the case.
type VendorCheck =
    {
        Vendor: string
        Product: string
        Reference: string
        CheckedAt: DateTimeOffset
        /// The vendor's own outcome word or words, unchanged.
        OutcomeWords: string
        Hits: VendorHit list
        Gaps: VendorGap list
        /// What the vendor states about the company, to compare with other sources.
        Facts: Fact list
        /// The vendor's answer as received.
        Raw: string voption
    }

/// One step of a path through the case's links.
type LinkStep =
    {
        LinkId: string
        Kind: LinkKind
        /// False when the step is a declared link.
        Evidenced: bool
    }

/// A flag as seen from an entity. Through is empty when the flag is the entity's own. Otherwise it is on a linked entity, and shows only that the
/// link exists: it says nothing about this entity.
type SeenFlag =
    {
        Flag: CaseFlag
        Through: LinkStep list
    }

/// What an officer needs to look at. An empty list wouldn't mean anything was cleared.
type ReviewItem =
    {
        /// The flag, gap or document it comes from.
        Reference: string
        /// The key of the entity it concerns.
        About: string voption
        Text: string
    }

type CaseFile =
    {
        /// The version of this shape, which CaseJson checks.
        SchemaVersion: int
        CaseId: string
        OpenedAt: DateTimeOffset
        /// The key of the company the case is about.
        Subject: string
        Entities: CaseEntity list
        Links: CaseLink list
        Evidence: CaseEvidence list
        Flags: CaseFlag list
        Gaps: CaseGap list
        Documents: CaseDocument list
        Notes: CaseNote list
        VendorChecks: VendorCheck list
        Screenings: Screening list
    }

/// A refused change to a case file: it references something the case doesn't hold.
[<RequireQualifiedAccess>]
type CaseEditRefused =
    | UnknownEntity of key: string
    | UnknownDocument of id: string
    | UnknownEvidence of id: string
    | UnknownGap of id: string
    /// A filing document link needs the document's reading in evidence first.
    | NotAReadingOfThatDocument of evidenceId: string * documentId: string
    /// Who entered it, or its source, is blank.
    | NotAttributed

/// The comparison of an entity's evidence across sources.
[<RequireQualifiedAccess>]
type DiscrepancyField =
    | Name
    | Number
    | Status
    | Address
    | LegalForm
    | Capital
    | Officers

/// One source's value in a comparison.
type StatedValue =
    {
        Source: string
        Origin: EvidenceOrigin
        EvidenceId: string
        AsOf: DateOnly voption
        Value: string
    }

/// What a difference is. Different kinds of thing aren't a contradiction, and are labelled so instead of being flagged.
[<RequireQualifiedAccess>]
type DiscrepancyLabel =
    | Disagreement
    | DifferentKinds of explanation: string

type Discrepancy =
    {
        About: string
        Field: DiscrepancyField
        Label: DiscrepancyLabel
        Statement: string
        Values: StatedValue list
    }

/// Finds where sources differ about one entity. It keeps every value and doesn't pick one.
module Discrepancies =

    let private spokenBy (evidence: CaseEvidence) value : StatedValue =
        {
            Source = evidence.Source
            Origin = evidence.Origin
            EvidenceId = evidence.Id
            AsOf = evidence.AsOf
            Value = value
        }

    /// Pairs of two different evidence items' values, each pair once.
    let private pairsAcrossSources (values: (CaseEvidence * 'T) list) =
        [
            for i, (first, a) in List.indexed values do
                for second, b in List.skip (i + 1) values do
                    if first.Source <> second.Source then
                        (first, a), (second, b)
        ]

    let private nameKey (name: string) =
        NameMatching.tokens name |> String.concat " "

    let private numberKeyRegex = Regex @"\s+"

    let private numberKey (number: string) =
        numberKeyRegex.Replace((NameMatching.normalise number), "").TrimStart '0'

    let private formsOfLtd =
        set
            [
                "LTD"
                "LIMITED"
                "PRIVATE LIMITED COMPANY"
                "PRIVATE LIMITED"
                "PRIVATE LIMITED BY SHARES"
            ]

    let private formsOfCorp =
        set
            [
                "CORP"
                "CORPORATION"
                "INC"
                "INCORPORATED"
                "STOCK CORPORATION"
                "PROFIT CORPORATION"
            ]

    let private formsOfLlc = set [ "LLC"; "LIMITED LIABILITY COMPANY" ]
    let private formsOfPlc = set [ "PLC"; "PUBLIC LIMITED COMPANY" ]

    /// The forms this comparison can place. Anything else is wording it can't compare, not a contradiction.
    let private formKey (form: string) =
        let normal = NameMatching.normalise form

        if formsOfLtd.Contains normal then
            ValueSome "private limited company"
        elif formsOfCorp.Contains normal then
            ValueSome "corporation"
        elif formsOfLlc.Contains normal then
            ValueSome "limited liability company"
        elif formsOfPlc.Contains normal then
            ValueSome "public limited company"
        else
            ValueNone

    let private addressWords (text: string) =
        (NameMatching.normalise text).Split(' ', StringSplitOptions.RemoveEmptyEntries)
        |> Set.ofArray

    /// A postal code compared without its spacing or case: "293 01" and "29301" are one code.
    let private postalKey (code: string) =
        (String.filter Char.IsLetterOrDigit code).ToUpperInvariant()

    let private postalWords (code: string voption) =
        code |> ValueOption.map addressWords |> ValueOption.defaultValue Set.empty

    /// False when both addresses hold a house or building number, besides the postal code's, and none is shared: one postal code can
    /// cover a whole district, as a US ZIP code does.
    let private sameNumbersIn (a: string) (b: string) (postal: Set<string>) =
        let numbers (text: string) =
            Set.difference (addressWords text) postal
            |> Set.filter (String.exists Char.IsDigit)

        let numbersA, numbersB = numbers a, numbers b

        numbersA.IsEmpty
        || numbersB.IsEmpty
        || not (Set.intersect numbersA numbersB).IsEmpty

    let private addressLabel (kind: AddressKind) =
        match kind with
        | AddressKind.RegisteredOffice -> "registered office"
        | AddressKind.Service -> "service address"
        | AddressKind.PrincipalOffice -> "principal office"
        | AddressKind.RegisteredAgent -> "registered agent"
        | AddressKind.Postal -> "postal address"
        | AddressKind.Other label -> label

    let private capitalLabel (kind: CapitalKind) =
        match kind with
        | CapitalKind.Authorised -> "authorised"
        | CapitalKind.Issued -> "issued"
        | CapitalKind.PaidUp -> "paid up"
        | CapitalKind.Unlabelled(ValueSome word) -> word
        | CapitalKind.Unlabelled ValueNone -> "unlabelled"

    let private capitalText (capital: CapitalStatement) =
        let shares =
            capital.Shares
            |> ValueOption.map (fun count -> $"{count:N0} shares")
            |> ValueOption.toList

        let money =
            capital.Amount
            |> ValueOption.map (fun amount ->
                let currency = capital.Currency |> ValueOption.defaultValue ""
                $"{amount:N2} {currency}".Trim())
            |> ValueOption.toList

        $"""{String.concat ", " (shares @ money)} {capitalLabel capital.Kind}"""

    /// Compares the capital figures two sources give. The same kind with another figure disagrees. The same figure under another word disagrees too,
    /// because authorised and issued mean different things. Another figure under another word is what a company normally has.
    let private capitalDifference (first: CapitalStatement) (second: CapitalStatement) =
        let sameFigure =
            first.Shares = second.Shares
            && first.Amount = second.Amount
            && first.Currency = second.Currency

        let bothNamed (kind: CapitalKind) =
            match kind with
            | CapitalKind.Unlabelled _ -> false
            | _ -> true

        if first.Kind = second.Kind then
            if sameFigure then
                ValueNone
            else
                ValueSome DiscrepancyLabel.Disagreement
        elif sameFigure && bothNamed first.Kind && bothNamed second.Kind then
            ValueSome DiscrepancyLabel.Disagreement
        elif bothNamed first.Kind && bothNamed second.Kind then
            ValueSome(
                DiscrepancyLabel.DifferentKinds
                    "Different kinds of capital: a company's authorised and issued figures normally differ"
            )
        elif sameFigure then
            ValueNone
        else
            ValueSome(DiscrepancyLabel.DifferentKinds "At least one figure isn't labelled, so they can't be compared")

    /// Values of one fact in each evidence item about the entity.
    let private factsOf (pick: Fact -> 'T voption) (evidence: CaseEvidence list) =
        evidence
        |> List.collect (fun item ->
            item.Facts
            |> List.choose (pick >> ValueOption.toOption)
            |> List.map (fun value -> item, value))

    let private oneDifference about field label statement (first, firstValue) (second, secondValue) =
        {
            About = about
            Field = field
            Label = label
            Statement = statement
            Values = [ spokenBy first firstValue; spokenBy second secondValue ]
        }

    let private names about evidence =
        factsOf
            (function
            | Fact.Name name -> ValueSome name
            | _ -> ValueNone)
            evidence
        |> pairsAcrossSources
        |> List.choose (fun ((first, a), (second, b)) ->
            if nameKey a = nameKey b then
                None
            else
                Some(
                    oneDifference
                        about
                        DiscrepancyField.Name
                        DiscrepancyLabel.Disagreement
                        $"{first.Source} gives the name {a}; {second.Source} gives {b}"
                        (first, a)
                        (second, b)
                ))

    let private numbers about evidence =
        factsOf
            (function
            | Fact.RegistrationNumber number -> ValueSome number
            | _ -> ValueNone)
            evidence
        |> pairsAcrossSources
        |> List.choose (fun ((first, a), (second, b)) ->
            if numberKey a = numberKey b then
                None
            else
                Some(
                    oneDifference
                        about
                        DiscrepancyField.Number
                        DiscrepancyLabel.Disagreement
                        $"{first.Source} gives the number {a}; {second.Source} gives {b}"
                        (first, a)
                        (second, b)
                ))

    /// Different words for a live company are no difference. A live against an ended reading disagrees, and a proceeding against a live reading is labelled.
    let private statuses about evidence =
        factsOf
            (function
            | Fact.Status(word, reading) -> ValueSome(word, reading)
            | _ -> ValueNone)
            evidence
        |> pairsAcrossSources
        |> List.choose (fun ((first, (a, readingA)), (second, (b, readingB))) ->
            let label =
                match readingA, readingB with
                | StatusReading.Live, StatusReading.Ended
                | StatusReading.Ended, StatusReading.Live
                | StatusReading.InProceeding, StatusReading.Ended
                | StatusReading.Ended, StatusReading.InProceeding -> ValueSome DiscrepancyLabel.Disagreement
                | StatusReading.Live, StatusReading.InProceeding
                | StatusReading.InProceeding, StatusReading.Live ->
                    ValueSome(
                        DiscrepancyLabel.DifferentKinds
                            "A company in a proceeding can still be live on a source that doesn't record proceedings"
                    )
                | _ -> ValueNone

            label
            |> ValueOption.map (fun label ->
                oneDifference
                    about
                    DiscrepancyField.Status
                    label
                    $"{first.Source} gives the status {a}; {second.Source} gives {b}"
                    (first, a)
                    (second, b))
            |> ValueOption.toOption)

    let private addresses about evidence =
        factsOf
            (function
            | Fact.Address(kind, text, postal) -> ValueSome(kind, text, postal)
            | _ -> ValueNone)
            evidence
        |> pairsAcrossSources
        |> List.choose (fun ((first, (kindA, a, postalA)), (second, (kindB, b, postalB))) ->
            let said = first.Source, second.Source

            if kindA <> kindB then
                let labelA, labelB = addressLabel kindA, addressLabel kindB

                if addressWords a = addressWords b then
                    None
                else
                    Some(
                        oneDifference
                            about
                            DiscrepancyField.Address
                            (DiscrepancyLabel.DifferentKinds $"A {labelA} and a {labelB} aren't expected to match")
                            $"{fst said} gives a {labelA}: {a}; {snd said} gives a {labelB}: {b}"
                            (first, a)
                            (second, b)
                    )
            else
                let wordsA, wordsB = addressWords a, addressWords b

                if wordsA.IsSubsetOf wordsB || wordsB.IsSubsetOf wordsA then
                    None
                elif
                    postalA.IsSome
                    && ValueOption.map postalKey postalA = ValueOption.map postalKey postalB
                    && ValueOption.map postalKey postalA <> ValueSome ""
                    && sameNumbersIn a b (Set.union (postalWords postalA) (postalWords postalB))
                then
                    Some(
                        oneDifference
                            about
                            DiscrepancyField.Address
                            (DiscrepancyLabel.DifferentKinds "The same postal code, worded differently")
                            $"{fst said} gives {a}; {snd said} gives {b}"
                            (first, a)
                            (second, b)
                    )
                else
                    Some(
                        oneDifference
                            about
                            DiscrepancyField.Address
                            DiscrepancyLabel.Disagreement
                            $"{fst said} gives the {addressLabel kindA} {a}; {snd said} gives {b}"
                            (first, a)
                            (second, b)
                    ))

    let private legalForms about evidence =
        factsOf
            (function
            | Fact.LegalForm form -> ValueSome form
            | _ -> ValueNone)
            evidence
        |> pairsAcrossSources
        |> List.choose (fun ((first, a), (second, b)) ->
            match formKey a, formKey b with
            | ValueSome x, ValueSome y when x = y -> None
            | ValueSome _, ValueSome _ ->
                Some(
                    oneDifference
                        about
                        DiscrepancyField.LegalForm
                        DiscrepancyLabel.Disagreement
                        $"{first.Source} gives the legal form {a}; {second.Source} gives {b}"
                        (first, a)
                        (second, b)
                )
            | _ when NameMatching.normalise a = NameMatching.normalise b -> None
            | _ ->
                Some(
                    oneDifference
                        about
                        DiscrepancyField.LegalForm
                        (DiscrepancyLabel.DifferentKinds "Wording this comparison can't place, so not compared")
                        $"{first.Source} gives the legal form {a}; {second.Source} gives {b}"
                        (first, a)
                        (second, b)
                ))

    let private capital about evidence =
        factsOf
            (function
            | Fact.Capital statement -> ValueSome statement
            | _ -> ValueNone)
            evidence
        |> pairsAcrossSources
        |> List.choose (fun ((first, a), (second, b)) ->
            capitalDifference a b
            |> ValueOption.map (fun label ->
                let textA, textB = capitalText a, capitalText b

                oneDifference
                    about
                    DiscrepancyField.Capital
                    label
                    $"{first.Source} gives {textA}; {second.Source} gives {textB}"
                    (first, textA)
                    (second, textB))
            |> ValueOption.toOption)

    /// An officer one source lists and another that lists officers doesn't. A source listing none isn't compared: most list none because they don't publish them.
    let private officers about (evidence: CaseEvidence list) =
        let bySource =
            evidence
            |> List.choose (fun item ->
                match
                    item.Facts
                    |> List.choose (function
                        | Fact.Officer name -> Some name
                        | _ -> None)
                with
                | [] -> None
                | names -> Some(item, names))

        let wordKey (name: string) =
            (NameMatching.normalise name).Split(' ', StringSplitOptions.RemoveEmptyEntries)
            |> Array.sort
            |> String.concat " "

        [
            for i, (first, a) in List.indexed bySource do
                for second, b in List.skip (i + 1) bySource do
                    if first.Source <> second.Source then
                        let keysA = a |> List.map wordKey |> Set.ofList
                        let keysB = b |> List.map wordKey |> Set.ofList

                        for name in a do
                            if not (keysB.Contains(wordKey name)) then
                                oneDifference
                                    about
                                    DiscrepancyField.Officers
                                    DiscrepancyLabel.Disagreement
                                    $"{first.Source} lists {name} as an officer; {second.Source}'s list doesn't"
                                    (first, name)
                                    (second, String.concat ", " b)

                        for name in b do
                            if not (keysA.Contains(wordKey name)) then
                                oneDifference
                                    about
                                    DiscrepancyField.Officers
                                    DiscrepancyLabel.Disagreement
                                    $"{second.Source} lists {name} as an officer; {first.Source}'s list doesn't"
                                    (second, name)
                                    (first, String.concat ", " a)
        ]

    /// One business's records: this entity and those joined to it as a home record, foreign registration or establishment. Status isn't compared across them,
    /// because a forfeited branch says nothing about the home record. Capital is, because a company has one capital wherever it is registered.
    let private recordsOfOneBusiness (case: CaseFile) (key: string) =
        let joins (link: CaseLink) =
            match link.Kind with
            | LinkKind.HomeRecord
            | LinkKind.ForeignRegistration
            | LinkKind.UkEstablishment -> true
            | _ -> false

        let rec spread (reached: Set<string>) =
            let wider =
                case.Links
                |> List.filter joins
                |> List.fold
                    (fun (set: Set<string>) link ->
                        if set.Contains link.From then Set.add link.To set
                        elif set.Contains link.To then Set.add link.From set
                        else set)
                    reached

            if wider.Count = reached.Count then
                reached
            else
                spread wider

        spread (Set.singleton key)

    /// The differences between sources in what they say of one entity, and of the capital of the business it is a record of.
    let forEntity (case: CaseFile) (key: string) =
        let about = case.Evidence |> List.filter (fun item -> item.About = key)
        let records = recordsOfOneBusiness case key

        let business =
            case.Evidence |> List.filter (fun item -> records.Contains item.About)

        names key about
        @ numbers key about
        @ statuses key about
        @ addresses key about
        @ legalForms key about
        @ capital key business
        @ officers key about

    /// The differences for every entity, a difference in one business's capital once.
    let all (case: CaseFile) =
        case.Entities
        |> List.collect (fun entity -> forEntity case entity.Key)
        |> List.fold
            (fun (kept: Discrepancy list) found ->
                if
                    found.Field = DiscrepancyField.Capital
                    && kept
                       |> List.exists (fun other ->
                           other.Field = DiscrepancyField.Capital && other.Statement = found.Statement)
                then
                    kept
                else
                    kept @ [ found ])
            []

/// What can't be decided from what the host said about an address. Used to label addresses from a list the host supplies.
type KnownAddress =
    {
        /// The address as the list gives it.
        Text: string
        /// What the list says it is, such as "virtual office provider".
        Label: string
    }

/// A list of addresses the host holds, such as known virtual office providers. No such list ships here.
type KnownAddressList =
    {
        Source: string
        Dated: DateOnly
        Addresses: KnownAddress list
    }

module CaseFile =

    [<Literal>]
    let CurrentSchemaVersion = 1

    let private nextId (prefix: string) (existing: string list) =
        let highest =
            existing
            |> List.choose (fun id ->
                if id.StartsWith prefix then
                    match Int32.TryParse(id.AsSpan prefix.Length) with
                    | true, number -> Some number
                    | _ -> None
                else
                    None)
            |> function
                | [] -> 0
                | numbers -> List.max numbers

        $"{prefix}{highest + 1}"

    [<Literal>]
    let private screeningSubject = "sanctions screening"

    [<Literal>]
    let private vendorSubject = "vendor check"

    let private hasEntity (case: CaseFile) key =
        case.Entities |> List.exists (fun entity -> entity.Key = key)

    let private attributed (who: Attribution) =
        not (String.IsNullOrWhiteSpace who.By || String.IsNullOrWhiteSpace who.Source)

    /// Finds a gap's id for the same hole, so that reading twice doesn't list it twice.
    let addGap (about: string voption) reach kind subject detail wouldClose (case: CaseFile) =
        let same (gap: CaseGap) =
            gap.About = about
            && gap.Subject = subject
            && gap.Kind = kind
            && gap.Detail = detail

        if case.Gaps |> List.exists same then
            case
        else
            { case with
                Gaps =
                    case.Gaps
                    @ [
                        {
                            Id = nextId "G" (case.Gaps |> List.map _.Id)
                            About = about
                            Reach = reach
                            Kind = kind
                            Subject = subject
                            Detail = detail
                            WouldClose = wouldClose
                            ClosedBy = ValueNone
                        }
                    ]
            }

    /// Adds a flag, unless the same statement already stands on that entity.
    let addFlag on kind statement source evidenceIds (case: CaseFile) =
        if
            case.Flags
            |> List.exists (fun flag -> flag.On = on && flag.Kind = kind && flag.Statement = statement)
        then
            case
        else
            { case with
                Flags =
                    case.Flags
                    @ [
                        {
                            Id = nextId "F" (case.Flags |> List.map _.Id)
                            On = on
                            Kind = kind
                            Statement = statement
                            Source = source
                            EvidenceIds = evidenceIds
                        }
                    ]
            }

    /// Every name held for an entity: its own and each one a source states for it. Words standing for a missing name don't count, and a party the register
    /// withholds has none.
    let internal namesHeldFor (case: CaseFile) (entity: CaseEntity) =
        if entity.Party = PartyKind.Withheld then
            []
        else
            let stated =
                case.Evidence
                |> List.filter (fun item -> item.About = entity.Key)
                |> List.collect _.Facts
                |> List.choose (function
                    | Fact.Name name
                    | Fact.OtherName(_, name) -> Some name
                    | _ -> None)

            entity.Name :: stated
            |> List.filter (PartyNames.isPlaceholder >> not)
            |> List.distinctBy NameMatching.normalise

    /// Officers a source names who have no entity of their own, with the entity the source spoke of.
    let internal officersWithoutEntity (case: CaseFile) =
        let known =
            case.Entities
            |> List.collect (namesHeldFor case)
            |> List.map NameMatching.normalise
            |> Set.ofList

        case.Evidence
        |> List.collect (fun item ->
            item.Facts
            |> List.choose (function
                | Fact.Officer name when
                    not (PartyNames.isPlaceholder name || known.Contains(NameMatching.normalise name))
                    ->
                    Some(item.About, name)
                | _ -> None))
        |> List.distinct

    /// A gap per name and entity the latest screening doesn't account for, as links and reads can add them after it. The next screening replaces these.
    let private noteUnscreened (case: CaseFile) =
        match List.tryLast case.Screenings with
        | None -> case
        | Some latest ->
            let accounted =
                latest.NamesScreened @ (latest.NamesNotScreened |> List.map _.Who)
                |> List.map NameMatching.normalise
                |> Set.ofList

            let isAccounted (name: string) =
                accounted.Contains(NameMatching.normalise name)

            let missing =
                [
                    for entity in case.Entities do
                        match namesHeldFor case entity with
                        | [] ->
                            if not (isAccounted entity.Key) then
                                entity.Key, entity.Name
                        | names ->
                            for name in names do
                                if not (isAccounted name) then
                                    entity.Key, name

                    for about, name in officersWithoutEntity case do
                        if not (isAccounted name) then
                            about, name
                ]

            missing
            |> List.fold
                (fun (state: CaseFile) (key, name) ->
                    addGap
                        (ValueSome key)
                        GapReach.WholeCase
                        GapKind.NotChecked
                        screeningSubject
                        $"Not screened: {name}. The latest screening, of {latest.At:``yyyy-MM-dd``}, didn't cover it"
                        "Run Investigation.screen again"
                        state)
                case

    /// Flags worked out from the evidence as a whole: disagreements and shared addresses. They follow the evidence, so they are worked out again as it
    /// changes, and one still standing keeps its id.
    let private refreshDerived (case: CaseFile) =
        let isDerived (flag: CaseFlag) =
            flag.Kind = FlagKind.Disagreement || flag.Kind = FlagKind.SharedAddress

        let disagreements =
            Discrepancies.all case
            |> List.filter (fun found -> found.Label = DiscrepancyLabel.Disagreement)
            |> List.map (fun found ->
                found.About,
                FlagKind.Disagreement,
                found.Statement,
                (found.Values |> List.map _.Source |> String.concat " and "),
                (found.Values |> List.map _.EvidenceId))

        let nameOf key =
            case.Entities
            |> List.tryFind (fun entity -> entity.Key = key)
            |> Option.map _.Name
            |> Option.defaultValue key

        let wordKey (text: string) =
            (NameMatching.normalise text).Split(' ', StringSplitOptions.RemoveEmptyEntries)
            |> Array.sort
            |> String.concat " "

        let companyAddresses =
            case.Evidence
            |> List.collect (fun item ->
                item.Facts
                |> List.choose (function
                    | Fact.Address(_, text, _) -> Some(item, text)
                    | _ -> None))
            |> List.filter (fun (item, _) ->
                case.Entities
                |> List.exists (fun entity -> entity.Key = item.About && entity.Party <> PartyKind.Individual))

        let shared =
            [
                for _, sharing in companyAddresses |> List.groupBy (snd >> wordKey) do
                    let entities = sharing |> List.map (fun (item, _) -> item.About) |> List.distinct

                    if entities.Length >= 2 then
                        for key in entities do
                            let others =
                                entities |> List.filter ((<>) key) |> List.map nameOf |> String.concat ", "

                            key,
                            FlagKind.SharedAddress,
                            $"The address {snd sharing.Head} is also given for {others}. This shows that the address is shared, and no more.",
                            (sharing
                             |> List.map (fun (item, _) -> item.Source)
                             |> List.distinct
                             |> String.concat " and "),
                            (sharing |> List.map (fun (item, _) -> item.Id))
            ]

        let wanted = disagreements @ shared

        let stillWanted (flag: CaseFlag) =
            wanted
            |> List.exists (fun (on, kind, statement, _, _) ->
                on = flag.On && kind = flag.Kind && statement = flag.Statement)

        wanted
        |> List.fold
            (fun (state: CaseFile) (on, kind, statement, source, evidenceIds) ->
                addFlag on kind statement source evidenceIds state)
            { case with
                Flags = case.Flags |> List.filter (fun flag -> not (isDerived flag) || stillWanted flag)
            }

    /// Opens a case on one company. It starts with the two things nothing has done yet: a vendor's check and a sanctions screening.
    let openCase
        (caseId: string)
        (identity: EntityIdentity)
        (name: string)
        (jurisdiction: Jurisdiction voption)
        (at: DateTimeOffset)
        =
        let key = EntityIdentity.key identity

        {
            SchemaVersion = CurrentSchemaVersion
            CaseId = caseId
            OpenedAt = at
            Subject = key
            Entities =
                [
                    {
                        Key = key
                        Name = name
                        Party = PartyKind.LegalEntity
                        Identities = [ identity ]
                        Jurisdiction = jurisdiction
                        Role = RecordRole.NotStated
                        Standing = ValueNone
                        StatusAsPublished = ValueNone
                    }
                ]
            Links = []
            Evidence = []
            Flags = []
            Gaps = []
            Documents = []
            Notes = []
            VendorChecks = []
            Screenings = []
        }
        |> addGap
            ValueNone
            GapReach.WholeCase
            GapKind.NotChecked
            vendorSubject
            "No vendor's check has been added"
            "Run the vendor checks and add them with CaseFile.addVendorCheck"
        |> addGap
            ValueNone
            GapReach.WholeCase
            GapKind.NotChecked
            screeningSubject
            "No sanctions screening has been run"
            "Run Investigation.screen with the lists' downloaded text"

    /// Adds an entity, or merges what is new into the one with the same key. The first name seen stays.
    let addEntity (entity: CaseEntity) (case: CaseFile) =
        match case.Entities |> List.tryFind (fun known -> known.Key = entity.Key) with
        | None ->
            noteUnscreened
                { case with
                    Entities = case.Entities @ [ entity ]
                }
        | Some known ->
            let merged =
                { known with
                    Identities = known.Identities @ entity.Identities |> List.distinct
                    Jurisdiction = known.Jurisdiction |> ValueOption.orElse entity.Jurisdiction
                    Role =
                        if known.Role = RecordRole.NotStated then
                            entity.Role
                        else
                            known.Role
                    Standing = entity.Standing |> ValueOption.orElse known.Standing
                    StatusAsPublished = entity.StatusAsPublished |> ValueOption.orElse known.StatusAsPublished
                    Party =
                        match known.Party with
                        | PartyKind.Unknown _ -> entity.Party
                        | party -> party
                }

            { case with
                Entities =
                    case.Entities
                    |> List.map (fun item -> if item.Key = known.Key then merged else item)
            }

    /// An entity with only an identity and a name, for something a link names.
    let entityOf (identity: EntityIdentity) (name: string) (party: PartyKind) (jurisdiction: Jurisdiction voption) =
        {
            Key = EntityIdentity.key identity
            Name = name
            Party = party
            Identities = [ identity ]
            Jurisdiction = jurisdiction
            Role = RecordRole.NotStated
            Standing = ValueNone
            StatusAsPublished = ValueNone
        }

    /// Adds evidence and works the derived flags out again. Returns the case and the evidence's id.
    let internal addEvidence (make: string -> CaseEvidence) (case: CaseFile) =
        let id = nextId "E" (case.Evidence |> List.map _.Id)

        let added =
            { case with
                Evidence = case.Evidence @ [ make id ]
            }

        noteUnscreened (refreshDerived added), id

    /// Adds a link, unless the same one stands already.
    let internal addLink from into kind provenance evidenceIds note (case: CaseFile) =
        if
            case.Links
            |> List.exists (fun link ->
                link.From = from
                && link.To = into
                && link.Kind = kind
                && link.Provenance = provenance)
        then
            case
        else
            { case with
                Links =
                    case.Links
                    @ [
                        {
                            Id = nextId "L" (case.Links |> List.map _.Id)
                            From = from
                            To = into
                            Kind = kind
                            Provenance = provenance
                            EvidenceIds = evidenceIds
                            Note = note
                        }
                    ]
            }

    /// An officer's own finding about an entity, kept as evidence with who, when and the source.
    let addFinding about (topic: string) (facts: Fact list) (note: string voption) (who: Attribution) (case: CaseFile) =
        if not (attributed who) then
            Error CaseEditRefused.NotAttributed
        elif not (hasEntity case about) then
            Error(CaseEditRefused.UnknownEntity about)
        else
            let updated, _ =
                addEvidence
                    (fun pid ->
                        {
                            Id = pid
                            About = about
                            Source = who.Source
                            Origin = EvidenceOrigin.Operator
                            Topic = topic
                            RetrievedAt = who.At
                            AsOf = ValueNone
                            Raw = ValueNone
                            Facts = facts
                            Attribution = ValueSome who
                            DocumentId = ValueNone
                            Note = note
                        })
                    case

            Ok updated

    let addNote (about: string voption) (text: string) (who: Attribution) (case: CaseFile) =
        match about with
        | _ when not (attributed who) -> Error CaseEditRefused.NotAttributed
        | ValueSome key when not (hasEntity case key) -> Error(CaseEditRefused.UnknownEntity key)
        | _ ->
            Ok
                { case with
                    Notes =
                        case.Notes
                        @ [
                            {
                                Id = nextId "N" (case.Notes |> List.map _.Id)
                                About = about
                                Text = text
                                Attribution = who
                            }
                        ]
                }

    /// An officer's own flag. The source says where it comes from, and the wording is theirs.
    let raiseFlag on (statement: string) (who: Attribution) (case: CaseFile) =
        if not (attributed who) then
            Error CaseEditRefused.NotAttributed
        elif not (hasEntity case on) then
            Error(CaseEditRefused.UnknownEntity on)
        else
            Ok(addFlag on FlagKind.OperatorFlag statement $"{who.By}, from {who.Source}" [] case)

    /// A link an officer declares, adding the far entity if the case doesn't hold it. It is shown as declared, never as evidenced.
    let declareLink from (into: CaseEntity) kind (note: string voption) (who: Attribution) (case: CaseFile) =
        if not (attributed who) then
            Error CaseEditRefused.NotAttributed
        elif not (hasEntity case from) then
            Error(CaseEditRefused.UnknownEntity from)
        else
            Ok(
                case
                |> addEntity into
                |> addLink from into.Key kind (LinkProvenance.OperatorDeclared who) [] note
            )

    let private gapForUnread (document: CaseDocument) =
        $"Document {document.Id} hasn't been read"

    /// What an officer or a reader says a document contains, as evidence on the document's entity. It closes the document's own not-read gap, and no other.
    let attachReading documentId (facts: Fact list) (text: string) (who: Attribution) (case: CaseFile) =
        match case.Documents |> List.tryFind (fun document -> document.Id = documentId) with
        | None -> Error(CaseEditRefused.UnknownDocument documentId)
        | Some _ when not (attributed who) -> Error CaseEditRefused.NotAttributed
        | Some document ->
            let updated, id =
                addEvidence
                    (fun pid ->
                        {
                            Id = pid
                            About = document.Entity
                            Source = who.Source
                            Origin = EvidenceOrigin.FilingDocument
                            Topic = $"reading of document {document.Id}"
                            RetrievedAt = who.At
                            AsOf = document.FiledOn
                            Raw = ValueSome text
                            Facts = facts
                            Attribution = ValueSome who
                            DocumentId = ValueSome document.Id
                            Note = ValueNone
                        })
                    case

            Ok(
                { updated with
                    Gaps =
                        updated.Gaps
                        |> List.map (fun gap ->
                            if gap.ClosedBy.IsNone && gap.Detail = gapForUnread document then
                                { gap with
                                    ClosedBy = ValueSome(who, id)
                                }
                            else
                                gap)
                },
                id
            )

    /// A link a filing states, resting on that filing's reading in evidence. The far entity is added if the case doesn't hold it.
    let linkFromDocument from (into: CaseEntity) kind documentId evidenceId (note: string voption) (case: CaseFile) =
        if not (hasEntity case from) then
            Error(CaseEditRefused.UnknownEntity from)
        elif not (case.Documents |> List.exists (fun document -> document.Id = documentId)) then
            Error(CaseEditRefused.UnknownDocument documentId)
        else
            match case.Evidence |> List.tryFind (fun item -> item.Id = evidenceId) with
            | None -> Error(CaseEditRefused.UnknownEvidence evidenceId)
            | Some item when item.DocumentId <> ValueSome documentId ->
                Error(CaseEditRefused.NotAReadingOfThatDocument(evidenceId, documentId))
            | Some _ ->
                Ok(
                    case
                    |> addEntity into
                    |> addLink
                        from
                        into.Key
                        kind
                        (LinkProvenance.FilingDocument(documentId, evidenceId))
                        [ evidenceId ]
                        note
                )

    /// Closes a gap with the evidence that answers it. A gap without evidence stays open.
    let closeGap gapId evidenceId (who: Attribution) (case: CaseFile) =
        if not (attributed who) then
            Error CaseEditRefused.NotAttributed
        elif not (case.Gaps |> List.exists (fun gap -> gap.Id = gapId)) then
            Error(CaseEditRefused.UnknownGap gapId)
        elif not (case.Evidence |> List.exists (fun item -> item.Id = evidenceId)) then
            Error(CaseEditRefused.UnknownEvidence evidenceId)
        else
            Ok
                { case with
                    Gaps =
                        case.Gaps
                        |> List.map (fun gap ->
                            if gap.Id = gapId then
                                { gap with
                                    ClosedBy = ValueSome(who, evidenceId)
                                }
                            else
                                gap)
                }

    /// Adds a vendor's check on an entity: its evidence, a flag per hit and a gap per thing it didn't check. It closes only the "no vendor's check yet"
    /// gap: a vendor's Clear is that vendor's result on that date, and leaves every other gap open.
    let addVendorCheck about (check: VendorCheck) (case: CaseFile) =
        if not (hasEntity case about) then
            Error(CaseEditRefused.UnknownEntity about)
        else
            let source = $"{check.Vendor} {check.Product}"

            let withEvidence, pid =
                addEvidence
                    (fun pid ->
                        {
                            Id = pid
                            About = about
                            Source = source
                            Origin = EvidenceOrigin.Vendor
                            Topic = $"vendor check {check.Reference}"
                            RetrievedAt = check.CheckedAt
                            AsOf = ValueSome(DateOnly.FromDateTime check.CheckedAt.UtcDateTime)
                            Raw = check.Raw
                            Facts = check.Facts
                            Attribution = ValueNone
                            DocumentId = ValueNone
                            Note =
                                ValueSome
                                    $"{check.Vendor}'s outcome on {check.CheckedAt:``yyyy-MM-dd``}: {check.OutcomeWords}"
                        })
                    case

            let withHits =
                check.Hits
                |> List.fold
                    (fun (state: CaseFile) hit ->
                        let detail =
                            hit.Detail
                            |> ValueOption.map (fun text -> $": {text}")
                            |> ValueOption.defaultValue ""

                        addFlag
                            about
                            (hit.ShowAs |> ValueOption.defaultValue FlagKind.VendorHit)
                            $"{check.Vendor} reports for {hit.Subject}: {hit.Words}{detail}"
                            source
                            [ pid ]
                            state)
                    withEvidence

            let withGaps =
                check.Gaps
                |> List.fold
                    (fun (state: CaseFile) gap ->
                        addGap
                            (ValueSome about)
                            GapReach.WholeCase
                            GapKind.NotChecked
                            $"{check.Vendor} check"
                            $"{check.Vendor} says of {gap.Subject}: {gap.Words}"
                            (gap.WouldClose
                             |> ValueOption.defaultValue $"Ask {check.Vendor} why, or run the check again")
                            state)
                    withHits

            Ok
                { withGaps with
                    VendorChecks = withGaps.VendorChecks @ [ check ]
                    Gaps =
                        withGaps.Gaps
                        |> List.filter (fun gap ->
                            about <> case.Subject
                            || not (gap.Subject = vendorSubject && gap.About.IsNone && gap.Kind = GapKind.NotChecked))
                }

    /// A link from a vendor's check. The far entity is added if the case doesn't hold it.
    let linkFromVendor from (into: CaseEntity) kind vendor reference (note: string voption) (case: CaseFile) =
        if not (hasEntity case from) then
            Error(CaseEditRefused.UnknownEntity from)
        else
            let evidenceIds =
                case.Evidence
                |> List.filter (fun item ->
                    item.Origin = EvidenceOrigin.Vendor && item.Topic = $"vendor check {reference}")
                |> List.map _.Id

            Ok(
                case
                |> addEntity into
                |> addLink from into.Key kind (LinkProvenance.Vendor(vendor, reference)) evidenceIds note
            )

    /// Adds a screening: a flag per possible match, a gap per list not used, per list whose file states another date than it was supplied under, and per name
    /// not screened or entity not covered. It replaces the gaps of the screening before it.
    let addScreening (screening: Screening) (case: CaseFile) =
        let withoutOld =
            { case with
                Gaps = case.Gaps |> List.filter (fun gap -> gap.Subject <> screeningSubject)
                Screenings = case.Screenings @ [ screening ]
            }

        let withLists =
            screening.ListsNotUsed
            |> List.fold
                (fun (state: CaseFile) list ->
                    let kind =
                        match list.List with
                        | SanctionsListName.EuConsolidated -> GapKind.NeedsEntitlement
                        | _ -> GapKind.NotChecked

                    addGap
                        ValueNone
                        GapReach.WholeCase
                        kind
                        screeningSubject
                        $"%A{list.List}: %s{list.Reason}"
                        list.WouldClose
                        state)
                withoutOld

        let withDates =
            screening.ListsRead
            |> List.fold
                (fun (state: CaseFile) list ->
                    match list.StatedInFile with
                    | ValueSome stated when stated <> list.Dated ->
                        addGap
                            ValueNone
                            GapReach.WholeCase
                            GapKind.NotChecked
                            screeningSubject
                            $"%A{list.List}: the file states {stated:``yyyy-MM-dd``}, and it was supplied as dated {list.Dated:``yyyy-MM-dd``}"
                            "Check which date is right, and download the list again if the file is old"
                            state
                    | _ -> state)
                withLists

        let withNames =
            screening.NamesNotScreened
            |> List.fold
                (fun (state: CaseFile) name ->
                    addGap
                        ValueNone
                        GapReach.WholeCase
                        GapKind.NotChecked
                        screeningSubject
                        $"Not screened: {name.Who}. {name.Reason}"
                        "Identify the party and screen the name"
                        state)
                withDates

        screening.PossibleMatches
        |> List.fold
            (fun (state: CaseFile) found ->
                let on = found.EntityKey |> ValueOption.defaultValue state.Subject

                addFlag
                    on
                    FlagKind.PossibleSanctionsMatch
                    $"Possible match for review: %s{found.ScreenedName} resembles %s{found.ListedName} on %A{found.List} dated {found.ListDated:``yyyy-MM-dd``}. %s{found.ScoreReason}."
                    $"%A{found.List}"
                    []
                    state)
            withNames
        |> noteUnscreened

    /// A flag's own entity and everything linked to it, shortest path first. Links are read both ways: a company's owner's flag shows on the company, and
    /// the company's on its owner.
    let flagsSeenFrom (key: string) (case: CaseFile) : SeenFlag list =
        let stepOf (link: CaseLink) =
            {
                LinkId = link.Id
                Kind = link.Kind
                Evidenced = CaseLink.isEvidenced link
            }

        let rec walk
            (frontier: (string * LinkStep list) list)
            (seen: Set<string>)
            (found: (string * LinkStep list) list)
            =
            match frontier with
            | [] -> List.rev found
            | (at, path) :: rest ->
                let next =
                    case.Links
                    |> List.choose (fun link ->
                        if link.From = at && not (seen.Contains link.To) then
                            Some(link.To, path @ [ stepOf link ])
                        elif link.To = at && not (seen.Contains link.From) then
                            Some(link.From, path @ [ stepOf link ])
                        else
                            None)
                    |> List.distinctBy fst

                walk
                    (rest @ next)
                    (next |> List.fold (fun set (entity, _) -> Set.add entity set) seen)
                    ((at, path) :: found)

        walk [ key, [] ] (Set.singleton key) []
        |> List.collect (fun (entity, path) ->
            case.Flags
            |> List.filter (fun flag -> flag.On = entity)
            |> List.map (fun flag -> { Flag = flag; Through = path }))

    let private nameOf (case: CaseFile) key =
        case.Entities
        |> List.tryFind (fun entity -> entity.Key = key)
        |> Option.map _.Name
        |> Option.defaultValue key

    /// One line for a flag as seen through links, which alleges nothing about the entity it is seen from.
    let describeSeen (case: CaseFile) (seen: SeenFlag) =
        match seen.Through with
        | [] -> seen.Flag.Statement
        | steps ->
            let path =
                steps
                |> List.map (fun step ->
                    let declared = if step.Evidenced then "" else ", declared, not evidenced"
                    $"{CaseLink.describeKind step.Kind} link {step.LinkId}{declared}")
                |> String.concat " then "

            $"On {nameOf case seen.Flag.On}, seen through {path}: {seen.Flag.Statement} This is about {nameOf case seen.Flag.On} and shows only that a link exists."

    /// What needs an officer's eye: every flag seen from the subject, every open gap and every declared link. An empty list wouldn't mean anything was cleared.
    let reviewNeeded (case: CaseFile) : ReviewItem list =
        let flags =
            flagsSeenFrom case.Subject case
            |> List.map (fun seen ->
                {
                    Reference = seen.Flag.Id
                    About = ValueSome seen.Flag.On
                    Text = describeSeen case seen
                })

        let elsewhere =
            let seen = flags |> List.map _.Reference |> Set.ofList

            case.Flags
            |> List.filter (fun flag -> not (seen.Contains flag.Id))
            |> List.map (fun flag ->
                {
                    Reference = flag.Id
                    About = ValueSome flag.On
                    Text = $"On {nameOf case flag.On}: {flag.Statement}"
                })

        let gaps =
            case.Gaps
            |> List.filter (fun gap -> gap.ClosedBy.IsNone)
            |> List.map (fun gap ->
                {
                    Reference = gap.Id
                    About = gap.About
                    Text = $"{gap.Subject}: {gap.Detail}. What would close it: {gap.WouldClose}"
                })

        let declared =
            case.Links
            |> List.filter (CaseLink.isEvidenced >> not)
            |> List.map (fun link ->
                {
                    Reference = link.Id
                    About = ValueSome link.From
                    Text =
                        $"Declared link, not evidenced: {nameOf case link.From} to {nameOf case link.To} as {CaseLink.describeKind link.Kind}"
                })

        flags @ elsewhere @ gaps @ declared

    /// Adds a filing, and a not-read gap if it is one to read first.
    let internal addDocument (make: string -> CaseDocument) (case: CaseFile) =
        let document = make (nextId "D" (case.Documents |> List.map _.Id))

        let known =
            case.Documents
            |> List.exists (fun other -> other.Entity = document.Entity && other.TransactionId = document.TransactionId)

        if known then
            case
        else
            let added =
                { case with
                    Documents = case.Documents @ [ document ]
                }

            if CaseDocument.isHighlighted document then
                addGap
                    (ValueSome document.Entity)
                    GapReach.ThisEntity
                    GapKind.NotChecked
                    "filing to read"
                    (gapForUnread document)
                    "An officer attaches what it says, or a reader reads it"
                    added
            else
                added

    /// Shows a flag where an address is on a list the host supplied.
    let checkKnownAddresses (lists: KnownAddressList list) (case: CaseFile) =
        let words (text: string) =
            (NameMatching.normalise text).Split(' ', StringSplitOptions.RemoveEmptyEntries)
            |> Set.ofArray

        [
            for list in lists do
                for known in list.Addresses do
                    for item in case.Evidence do
                        for fact in item.Facts do
                            match fact with
                            | Fact.Address(_, text, _) when (words known.Text).IsSubsetOf(words text) ->
                                list, known, item, text
                            | _ -> ()
        ]
        |> List.fold
            (fun (state: CaseFile) (list, known, item, text) ->
                addFlag
                    item.About
                    FlagKind.VirtualAddress
                    $"The address {text} is on {list.Source} dated {list.Dated:``yyyy-MM-dd``} as {known.Label}. It says what the address is, and nothing about who uses it."
                    list.Source
                    [ item.Id ]
                    state)
            case
