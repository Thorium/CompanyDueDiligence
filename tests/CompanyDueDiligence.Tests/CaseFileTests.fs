/// The case file's own rules: links and provenance, flags seen through links, gaps, manual evidence, vendor checks, the discrepancy finder and the JSON round trip.
module CaseFileTests

open System
open System.Text.Json
open Microsoft.FSharp.Reflection
open Xunit
open CompanyDueDiligence

let private at = DateTimeOffset(2026, 11, 6, 12, 0, 0, TimeSpan.Zero)
let private gb = Jurisdiction.ofCode "GB"
let private usDe = Jurisdiction.ofCode "US-DE"

let private officer =
    {
        By = "A. Officer"
        At = at
        Source = "Register web page, viewed by hand"
    }

let private subject = EntityIdentity.OnRegister("Companies House", gb, "00000001")

let private opened =
    CaseFile.openCase "CASE-1" subject "EXAMPLE TRADING LTD" (ValueSome gb) at

let private key = opened.Subject

let private expectOk result =
    result |> Result.defaultWith (fun refused -> failwith $"%A{refused}")

let private company identity name =
    CaseFile.entityOf identity name PartyKind.LegalEntity ValueNone

let private delaware =
    company (EntityIdentity.OnRegister("Delaware Division of Corporations", usDe, "1234567")) "EXAMPLE TRADING INC"

/// An evidence item with the given facts, added by a named source.
let private said (about: string) (source: string) (origin: EvidenceOrigin) (facts: Fact list) (case: CaseFile) =
    let updated, _ =
        CaseFile.addEvidence
            (fun pid ->
                {
                    Id = pid
                    About = about
                    Source = source
                    Origin = origin
                    Topic = "test"
                    RetrievedAt = at
                    AsOf = ValueNone
                    Raw = ValueNone
                    Facts = facts
                    Attribution = ValueNone
                    DocumentId = ValueNone
                    Note = ValueNone
                })
            case

    updated

[<Fact>]
let ``a German identity holds a court, a section and a number, and its key needs all three`` () =
    let identity = EntityIdentity.GermanCourtRegister("Düsseldorf", "HRB", "12345")
    let other = EntityIdentity.GermanCourtRegister("Köln", "HRB", "12345")
    Assert.NotEqual<string>(EntityIdentity.key identity, EntityIdentity.key other)
    Assert.Contains("Düsseldorf", EntityIdentity.describe identity)
    Assert.Contains("HRB 12345", EntityIdentity.describe identity)

[<Fact>]
let ``one number on two registers is two entities, and one record reached two ways is one`` () =
    let uk = EntityIdentity.OnRegister("Companies House", gb, "1234567")

    let texas =
        EntityIdentity.OnRegister("Texas", Jurisdiction.ofCode "US-TX", "1234567")

    let delawareToo = EntityIdentity.OnRegister("Delaware", usDe, "1234567")
    Assert.NotEqual<string>(EntityIdentity.key uk, EntityIdentity.key texas)
    Assert.NotEqual<string>(EntityIdentity.key texas, EntityIdentity.key delawareToo)

    Assert.Equal(
        EntityIdentity.key (EntityIdentity.OnRegister("Companies House", Jurisdiction.ofCode "GB-SCT", "SC 12")),
        EntityIdentity.key (EntityIdentity.OnRegister("The UK register", gb, "sc12"))
    )

[<Fact>]
let ``a declared link never reads as evidenced`` () =
    let case =
        CaseFile.declareLink key delaware (LinkKind.StatedRelationship "same name") ValueNone officer opened
        |> expectOk

    let link = case.Links.Head
    Assert.False(CaseLink.isEvidenced link)
    Assert.Contains("not evidenced", CaseLink.describeProvenance link.Provenance)
    Assert.Empty link.EvidenceIds

    Assert.Contains(
        CaseFile.reviewNeeded case,
        (fun item -> item.Reference = link.Id && item.Text.Contains "Declared link, not evidenced")
    )

    Assert.Contains("not evidenced", CaseExport.toText false case)

[<Fact>]
let ``links from a register, a vendor and a cross-check read as evidenced`` () =
    for provenance in
        [
            LinkProvenance.Register "Companies House"
            LinkProvenance.Vendor("Experian", "REF")
            LinkProvenance.CrossCheck "GLEIF"
            LinkProvenance.FilingDocument("D1", "E1")
        ] do
        let case =
            CaseFile.addLink key "other" LinkKind.Owner provenance [] ValueNone opened

        Assert.True(CaseLink.isEvidenced case.Links.Head)

[<Fact>]
let ``the same link isn't added twice`` () =
    let once =
        CaseFile.addLink key "other" LinkKind.Owner (LinkProvenance.Register "R") [] ValueNone opened

    let twice =
        CaseFile.addLink key "other" LinkKind.Owner (LinkProvenance.Register "R") [] ValueNone once

    Assert.Equal(1, twice.Links.Length)

[<Fact>]
let ``manual entries need who and the source, and an entity the case holds`` () =
    let blank = { officer with By = " " }
    let noSource = { officer with Source = "" }
    Assert.Equal(Error CaseEditRefused.NotAttributed, CaseFile.addFinding key "t" [] ValueNone blank opened)
    Assert.Equal(Error CaseEditRefused.NotAttributed, CaseFile.addNote ValueNone "n" noSource opened)
    Assert.Equal(Error CaseEditRefused.NotAttributed, CaseFile.raiseFlag key "n" blank opened)

    Assert.Equal(
        Error CaseEditRefused.NotAttributed,
        CaseFile.declareLink key delaware LinkKind.Parent ValueNone blank opened
    )

    Assert.Equal(Error(CaseEditRefused.UnknownEntity "no"), CaseFile.addFinding "no" "t" [] ValueNone officer opened)
    Assert.Equal(Error(CaseEditRefused.UnknownEntity "no"), CaseFile.addNote (ValueSome "no") "n" officer opened)
    Assert.Equal(Error(CaseEditRefused.UnknownEntity "no"), CaseFile.raiseFlag "no" "n" officer opened)

    Assert.Equal(
        Error(CaseEditRefused.UnknownEntity "no"),
        CaseFile.declareLink "no" delaware LinkKind.Parent ValueNone officer opened
    )

[<Fact>]
let ``a finding is kept with who, when and the source`` () =
    let case =
        CaseFile.addFinding
            key
            "call to the company"
            [ Fact.Other("capital", "as told") ]
            (ValueSome "A note")
            officer
            opened
        |> expectOk

    let item = case.Evidence.Head
    Assert.Equal(EvidenceOrigin.Operator, item.Origin)
    Assert.Equal(ValueSome officer, item.Attribution)
    Assert.Equal(officer.Source, item.Source)
    Assert.Equal(at, item.RetrievedAt)

[<Fact>]
let ``a note and an officer's flag stand with who entered them`` () =
    let case =
        opened
        |> CaseFile.addNote (ValueSome key) "Spoke to the company" officer
        |> expectOk
        |> CaseFile.raiseFlag key "Website doesn't load" officer
        |> expectOk

    Assert.Equal(officer, case.Notes.Head.Attribution)
    Assert.Equal(FlagKind.OperatorFlag, case.Flags.Head.Kind)
    Assert.Contains("A. Officer", case.Flags.Head.Source)

let private withOwner =
    let owner =
        company (EntityIdentity.OnRegister("Companies House", gb, "00000002")) "PARENT HOLDINGS LTD"

    opened
    |> CaseFile.addEntity owner
    |> CaseFile.addLink key owner.Key LinkKind.Owner (LinkProvenance.Register "Companies House") [] ValueNone
    |> CaseFile.addFlag owner.Key FlagKind.Insolvency "The register's status is administration" "Companies House" []

let private ownerKey =
    EntityIdentity.key (EntityIdentity.OnRegister("Companies House", gb, "00000002"))

[<Fact>]
let ``a flag on an owner is seen on the company, marked as through that link`` () =
    match CaseFile.flagsSeenFrom key withOwner with
    | [ seen ] ->
        Assert.Equal(ownerKey, seen.Flag.On)
        Assert.Equal(LinkKind.Owner, seen.Through.Head.Kind)
        Assert.True seen.Through.Head.Evidenced
        let text = CaseFile.describeSeen withOwner seen
        Assert.Contains("seen through owner link", text)
        Assert.Contains("shows only that a link exists", text)
    | other -> failwith $"%A{other}"

[<Fact>]
let ``a flag on the entity itself comes through nothing`` () =
    match CaseFile.flagsSeenFrom ownerKey withOwner with
    | [ seen ] ->
        Assert.Empty seen.Through
        Assert.Equal(seen.Flag.Statement, CaseFile.describeSeen withOwner seen)
    | other -> failwith $"%A{other}"

[<Fact>]
let ``links are read both ways, and a loop ends`` () =
    let looped =
        withOwner
        |> CaseFile.addFlag key FlagKind.OverdueFiling "Accounts" "CH" []
        |> CaseFile.addLink ownerKey key LinkKind.Owner (LinkProvenance.Register "R") [] ValueNone

    let fromOwner = CaseFile.flagsSeenFrom ownerKey looped
    Assert.Equal(2, fromOwner.Length)

    Assert.Single(fromOwner, (fun seen -> seen.Flag.Kind = FlagKind.OverdueFiling && seen.Through.Length = 1))
    |> ignore

[<Fact>]
let ``a flag seen through a declared link says it is declared`` () =
    let case =
        CaseFile.declareLink ownerKey delaware (LinkKind.StatedRelationship "same name") ValueNone officer withOwner
        |> expectOk

    let seen = CaseFile.flagsSeenFrom delaware.Key case |> List.head
    Assert.Equal(1, seen.Through.Length)
    Assert.False seen.Through.Head.Evidenced
    Assert.Contains("declared, not evidenced", CaseFile.describeSeen case seen)

[<Fact>]
let ``a new case holds gaps for the vendor check and the screening not yet done`` () =
    let subjects = opened.Gaps |> List.map _.Subject
    Assert.Contains("vendor check", subjects)
    Assert.Contains("sanctions screening", subjects)
    Assert.NotEmpty(CaseFile.reviewNeeded opened)

[<Fact>]
let ``a gap isn't listed twice, and closing one needs evidence`` () =
    let gapped =
        opened
        |> CaseFile.addGap (ValueSome key) GapReach.WholeCase GapKind.NoRoute "x" "d" "w"
        |> CaseFile.addGap (ValueSome key) GapReach.WholeCase GapKind.NoRoute "x" "d" "w"

    let gap = gapped.Gaps |> List.find (fun item -> item.Subject = "x")
    Assert.Equal(1, gapped.Gaps |> List.filter (fun item -> item.Subject = "x") |> List.length)
    Assert.Equal(Error(CaseEditRefused.UnknownEvidence "E9"), CaseFile.closeGap gap.Id "E9" officer gapped)
    Assert.Equal(Error(CaseEditRefused.UnknownGap "G99"), CaseFile.closeGap "G99" "E9" officer gapped)

    let withFinding =
        CaseFile.addFinding key "t" [] ValueNone officer gapped |> expectOk

    let closed =
        CaseFile.closeGap gap.Id withFinding.Evidence.Head.Id officer withFinding
        |> expectOk

    Assert.DoesNotContain(CaseFile.reviewNeeded closed, (fun item -> item.Reference = gap.Id))
    Assert.Contains("Closed by A. Officer", CaseExport.toText false closed)

let private clearCheck =
    {
        Vendor = "Experian"
        Product = "KYB"
        Reference = "REF-1"
        CheckedAt = at
        OutcomeWords = "Clear"
        Hits = []
        Gaps = []
        Facts = []
        Raw = ValueSome "{}"
    }

[<Fact>]
let ``a vendor's Clear stays that vendor's result on that date, and closes no other gap`` () =
    let gapped =
        opened
        |> CaseFile.addGap (ValueSome key) GapReach.WholeCase GapKind.NoRoute "home record" "d" "w"

    let case = CaseFile.addVendorCheck key clearCheck gapped |> expectOk
    let subjects = case.Gaps |> List.map _.Subject
    Assert.Contains("home record", subjects)
    Assert.Contains("sanctions screening", subjects)
    Assert.DoesNotContain("vendor check", subjects)
    Assert.Equal(EvidenceOrigin.Vendor, case.Evidence.Head.Origin)
    Assert.Equal(ValueSome(DateOnly(2026, 11, 6)), case.Evidence.Head.AsOf)
    Assert.Contains("Experian's outcome on 2026-11-06: Clear", case.Evidence.Head.Note.Value)
    Assert.Empty case.Flags

    let text = CaseExport.toText false case
    Assert.Contains("Clear. That is this vendor's result on that date, and closes nothing else here.", text)

[<Fact>]
let ``a vendor's hits and gaps become flags and gaps, in its own words`` () =
    let check =
        { clearCheck with
            OutcomeWords = "NeedsReview"
            Hits =
                [
                    {
                        Subject = "Company"
                        Words = "MailboxAddress"
                        Detail = ValueNone
                        ShowAs = ValueSome FlagKind.VirtualAddress
                    }
                    {
                        Subject = "A director"
                        Words = "Pep"
                        Detail = ValueSome "Standard"
                        ShowAs = ValueNone
                    }
                ]
            Gaps =
                [
                    {
                        Subject = "Company"
                        Words = "ORCHESTRATOR_API_TIMEOUT"
                        WouldClose = ValueSome "Run it again"
                    }
                ]
        }

    let case = CaseFile.addVendorCheck key check opened |> expectOk

    Assert.Contains(
        case.Flags,
        (fun flag -> flag.Kind = FlagKind.VirtualAddress && flag.Statement.Contains "MailboxAddress")
    )

    Assert.Contains(case.Flags, (fun flag -> flag.Kind = FlagKind.VendorHit && flag.Statement.Contains "Pep: Standard"))
    Assert.All(case.Flags, (fun flag -> Assert.Equal("Experian KYB", flag.Source)))

    Assert.Contains(
        case.Gaps,
        (fun gap ->
            gap.Detail.Contains "ORCHESTRATOR_API_TIMEOUT"
            && gap.WouldClose = "Run it again")
    )

[<Fact>]
let ``a vendor's link is a vendor's, with its evidence`` () =
    let case =
        opened
        |> CaseFile.addVendorCheck key clearCheck
        |> expectOk
        |> CaseFile.linkFromVendor key delaware LinkKind.Parent "Experian" "REF-1" ValueNone
        |> expectOk

    let link = case.Links.Head
    Assert.Equal(LinkProvenance.Vendor("Experian", "REF-1"), link.Provenance)
    Assert.Equal<string list>([ case.Evidence.Head.Id ], link.EvidenceIds)

[<Fact>]
let ``a vendor check on an unknown entity is refused`` () =
    Assert.Equal(Error(CaseEditRefused.UnknownEntity "no"), CaseFile.addVendorCheck "no" clearCheck opened)

// The discrepancy finder.

let private disagreements (case: CaseFile) =
    Discrepancies.all case
    |> List.filter (fun found -> found.Label = DiscrepancyLabel.Disagreement)

[<Fact>]
let ``sources agreeing, in their own wording, raise nothing`` () =
    let case =
        opened
        |> said
            key
            "A"
            EvidenceOrigin.Register
            [
                Fact.Name "EXAMPLE TRADING LIMITED"
                Fact.RegistrationNumber "00000001"
                Fact.Status("active", StatusReading.Live)
                Fact.LegalForm "ltd"
            ]
        |> said
            key
            "B"
            EvidenceOrigin.Vendor
            [
                Fact.Name "Example Trading Ltd."
                Fact.RegistrationNumber "1"
                Fact.Status("Registered", StatusReading.Live)
                Fact.LegalForm "Private Limited Company"
            ]

    Assert.Empty(Discrepancies.all case)
    Assert.Empty case.Flags

[<Fact>]
let ``a different name, number, status or form is a disagreement, flagged with both sources`` () =
    let case =
        opened
        |> said
            key
            "Register"
            EvidenceOrigin.Register
            [
                Fact.Name "EXAMPLE TRADING LTD"
                Fact.RegistrationNumber "00000001"
                Fact.Status("dissolved", StatusReading.Ended)
                Fact.LegalForm "ltd"
            ]
        |> said
            key
            "Vendor"
            EvidenceOrigin.Vendor
            [
                Fact.Name "OTHER NAME LTD"
                Fact.RegistrationNumber "00000002"
                Fact.Status("Active", StatusReading.Live)
                Fact.LegalForm "LLC"
            ]

    let found = disagreements case |> List.map _.Field

    Assert.Equal<DiscrepancyField list>(
        [
            DiscrepancyField.Name
            DiscrepancyField.Number
            DiscrepancyField.Status
            DiscrepancyField.LegalForm
        ],
        found
    )

    Assert.Equal(
        4,
        case.Flags
        |> List.filter (fun flag -> flag.Kind = FlagKind.Disagreement)
        |> List.length
    )

    Assert.All(case.Flags, (fun flag -> Assert.Equal("Register and Vendor", flag.Source)))

[<Fact>]
let ``both values stay: the finder picks neither`` () =
    let case =
        opened
        |> said key "Register" EvidenceOrigin.Register [ Fact.Name "A LTD" ]
        |> said key "Vendor" EvidenceOrigin.Vendor [ Fact.Name "B LTD" ]

    let values = (disagreements case).Head.Values
    Assert.Equal<string list>([ "A LTD"; "B LTD" ], values |> List.map _.Value)
    Assert.Equal<EvidenceOrigin list>([ EvidenceOrigin.Register; EvidenceOrigin.Vendor ], values |> List.map _.Origin)

[<Fact>]
let ``one source's own two facts aren't compared`` () =
    let case =
        opened
        |> said key "Register" EvidenceOrigin.Register [ Fact.Name "A LTD"; Fact.Name "B LTD" ]

    Assert.Empty(Discrepancies.all case)

[<Fact>]
let ``a registered agent's address and a principal office are labelled, not flagged`` () =
    let case =
        opened
        |> said
            key
            "Texas"
            EvidenceOrigin.Register
            [ Fact.Address(AddressKind.RegisteredAgent, "1 Agent Way, Dover", ValueNone) ]
        |> said
            key
            "Vendor"
            EvidenceOrigin.Vendor
            [ Fact.Address(AddressKind.PrincipalOffice, "9 Tower Road, Austin", ValueNone) ]

    match Discrepancies.all case with
    | [ found ] ->
        Assert.Equal(DiscrepancyField.Address, found.Field)

        match found.Label with
        | DiscrepancyLabel.DifferentKinds why -> Assert.Contains("aren't expected to match", why)
        | other -> failwith $"%A{other}"
    | other -> failwith $"%A{other}"

    Assert.Empty(case.Flags |> List.filter (fun flag -> flag.Kind = FlagKind.Disagreement))

[<Fact>]
let ``two registered offices that differ are a disagreement`` () =
    let case =
        opened
        |> said
            key
            "A"
            EvidenceOrigin.Register
            [
                Fact.Address(AddressKind.RegisteredOffice, "1 Road, London", ValueSome "AB1 1AA")
            ]
        |> said
            key
            "B"
            EvidenceOrigin.Vendor
            [
                Fact.Address(AddressKind.RegisteredOffice, "2 Street, Leeds", ValueSome "LS1 1AA")
            ]

    Assert.Equal(DiscrepancyField.Address, (disagreements case).Head.Field)

[<Fact>]
let ``two wordings of one address aren't a disagreement`` () =
    let contained =
        opened
        |> said
            key
            "A"
            EvidenceOrigin.Register
            [ Fact.Address(AddressKind.RegisteredOffice, "1 Road, London", ValueNone) ]
        |> said
            key
            "B"
            EvidenceOrigin.Vendor
            [
                Fact.Address(AddressKind.RegisteredOffice, "1 ROAD LONDON AB1 1AA", ValueNone)
            ]

    Assert.Empty(Discrepancies.all contained)

    let samePostcode =
        opened
        |> said
            key
            "A"
            EvidenceOrigin.Register
            [
                Fact.Address(AddressKind.RegisteredOffice, "1 Road, London", ValueSome "AB1 1AA")
            ]
        |> said
            key
            "B"
            EvidenceOrigin.Vendor
            [
                Fact.Address(AddressKind.RegisteredOffice, "Unit 1, The Road", ValueSome "AB1 1AA")
            ]

    Assert.Empty(disagreements samePostcode)
    Assert.Single(Discrepancies.all samePostcode) |> ignore

[<Fact>]
let ``one postal code spaced or cased differently is the same code`` () =
    let spaced =
        opened
        |> said
            key
            "ARES"
            EvidenceOrigin.Register
            [
                Fact.Address(
                    AddressKind.RegisteredOffice,
                    "tř. Václava Klementa 869, Mladá Boleslav II, 29301 Mladá Boleslav, Česká republika",
                    ValueSome "29301"
                )
            ]
        |> said
            key
            "GLEIF"
            EvidenceOrigin.CrossCheck
            [
                Fact.Address(
                    AddressKind.RegisteredOffice,
                    "Tř. Václava Klementa 869, Mladá Boleslav, CZ-20, 293 01, CZ",
                    ValueSome "293 01"
                )
            ]

    Assert.Empty(disagreements spaced)

    let cased =
        opened
        |> said
            key
            "A"
            EvidenceOrigin.Register
            [
                Fact.Address(AddressKind.RegisteredOffice, "1 Road, London", ValueSome "ab1 1aa")
            ]
        |> said
            key
            "B"
            EvidenceOrigin.Vendor
            [
                Fact.Address(AddressKind.RegisteredOffice, "Unit 1, The Road", ValueSome "AB11AA")
            ]

    Assert.Empty(disagreements cased)

    // A ZIP code covers a district: different street numbers under one code disagree, and an empty code is no code.
    let zip =
        opened
        |> said
            key
            "A"
            EvidenceOrigin.Register
            [
                Fact.Address(AddressKind.RegisteredOffice, "1209 Orange St, Wilmington DE", ValueSome "19801")
            ]
        |> said
            key
            "B"
            EvidenceOrigin.Vendor
            [
                Fact.Address(AddressKind.RegisteredOffice, "1000 N West St, Wilmington DE", ValueSome "19801")
            ]

    Assert.Single(disagreements zip) |> ignore

    let blank =
        opened
        |> said
            key
            "A"
            EvidenceOrigin.Register
            [ Fact.Address(AddressKind.RegisteredOffice, "1 Road, London", ValueSome "-") ]
        |> said
            key
            "B"
            EvidenceOrigin.Vendor
            [
                Fact.Address(AddressKind.RegisteredOffice, "Unit 1, The Road", ValueSome " ")
            ]

    Assert.Single(disagreements blank) |> ignore

[<Fact>]
let ``a status in a proceeding against a live one is labelled, and an unplaced one isn't compared`` () =
    let proceeding =
        opened
        |> said key "A" EvidenceOrigin.Register [ Fact.Status("administration", StatusReading.InProceeding) ]
        |> said key "B" EvidenceOrigin.CrossCheck [ Fact.Status("ACTIVE", StatusReading.Live) ]

    Assert.Empty(disagreements proceeding)
    Assert.Single(Discrepancies.all proceeding) |> ignore

    let unplaced =
        opened
        |> said key "A" EvidenceOrigin.Register [ Fact.Status("odd word", StatusReading.NotPlaced) ]
        |> said key "B" EvidenceOrigin.CrossCheck [ Fact.Status("ACTIVE", StatusReading.Live) ]

    Assert.Empty(Discrepancies.all unplaced)

[<Fact>]
let ``a legal form this comparison can't place is labelled, not flagged`` () =
    let case =
        opened
        |> said key "A" EvidenceOrigin.Register [ Fact.LegalForm "ltd" ]
        |> said key "B" EvidenceOrigin.Vendor [ Fact.LegalForm "Julkinen osakeyhtiö" ]

    Assert.Empty(disagreements case)
    Assert.Single(Discrepancies.all case) |> ignore

let private shares kind count =
    Fact.Capital
        {
            Kind = kind
            Shares = ValueSome count
            Amount = ValueNone
            Currency = ValueNone
        }

[<Fact>]
let ``capital: the same word and another figure disagree, and the same figure under another word too`` () =
    let case =
        opened
        |> said key "UK form" EvidenceOrigin.FilingDocument [ shares CapitalKind.Issued 10000000m ]
        |> said key "Home certificate" EvidenceOrigin.FilingDocument [ shares CapitalKind.Authorised 10000000m ]
        |> said key "Founders' resolution" EvidenceOrigin.FilingDocument [ shares CapitalKind.Issued 9000000m ]

    let found = Discrepancies.all case
    let statements = disagreements case |> List.map _.Statement

    Assert.Equal(2, statements.Length)

    Assert.Contains(
        statements,
        (fun text ->
            text.Contains "10,000,000 shares issued"
            && text.Contains "10,000,000 shares authorised")
    )

    Assert.Contains(
        statements,
        (fun text ->
            text.Contains "10,000,000 shares issued"
            && text.Contains "9,000,000 shares issued")
    )

    // The certificate's authorised figure against the resolution's issued one is what a company normally has.
    Assert.Contains(
        found,
        (fun item ->
            match item.Label with
            | DiscrepancyLabel.DifferentKinds why -> why.Contains "normally differ"
            | DiscrepancyLabel.Disagreement -> false)
    )

[<Fact>]
let ``capital in the same words raises nothing, and an unlabelled figure is labelled`` () =
    let same =
        opened
        |> said key "A" EvidenceOrigin.Register [ shares CapitalKind.Issued 100m ]
        |> said key "B" EvidenceOrigin.Vendor [ shares CapitalKind.Issued 100m ]

    Assert.Empty(Discrepancies.all same)

    let unlabelled =
        opened
        |> said key "A" EvidenceOrigin.Register [ shares CapitalKind.Issued 100m ]
        |> said key "B" EvidenceOrigin.Vendor [ shares (CapitalKind.Unlabelled(ValueSome "capital")) 200m ]

    Assert.Empty(disagreements unlabelled)
    Assert.Single(Discrepancies.all unlabelled) |> ignore

[<Fact>]
let ``capital is compared across a business's records, and status isn't`` () =
    let case =
        opened
        |> CaseFile.addEntity delaware
        |> CaseFile.addLink
            key
            delaware.Key
            LinkKind.HomeRecord
            (LinkProvenance.Register "Companies House")
            []
            ValueNone
        |> said
            key
            "UK form"
            EvidenceOrigin.FilingDocument
            [
                shares CapitalKind.Issued 10000000m
                Fact.Status("active", StatusReading.Live)
            ]
        |> said
            delaware.Key
            "Home certificate"
            EvidenceOrigin.FilingDocument
            [
                shares CapitalKind.Authorised 10000000m
                Fact.Status("Void", StatusReading.Ended)
            ]

    match disagreements case with
    | [ found ] -> Assert.Equal(DiscrepancyField.Capital, found.Field)
    | other -> failwith $"%A{other}"

[<Fact>]
let ``capital isn't compared across an owner's link, which is another company`` () =
    let owner =
        company (EntityIdentity.OnRegister("Companies House", gb, "00000002")) "OWNER LTD"

    let case =
        opened
        |> CaseFile.addEntity owner
        |> CaseFile.addLink key owner.Key LinkKind.Owner (LinkProvenance.Register "Companies House") [] ValueNone
        |> said key "A" EvidenceOrigin.Register [ shares CapitalKind.Issued 1m ]
        |> said owner.Key "B" EvidenceOrigin.Register [ shares CapitalKind.Issued 2m ]

    Assert.Empty(Discrepancies.all case)

[<Fact>]
let ``an officer one list has and another doesn't is a disagreement, in any name order`` () =
    let case =
        opened
        |> said key "Register" EvidenceOrigin.Register [ Fact.Officer "DOE, Jane"; Fact.Officer "ROE, Sam" ]
        |> said key "Vendor" EvidenceOrigin.Vendor [ Fact.Officer "Jane Doe"; Fact.Officer "Pat Lee" ]

    let statements = disagreements case |> List.map _.Statement
    Assert.Equal(2, statements.Length)
    Assert.Contains(statements, (fun text -> text.Contains "ROE, Sam"))
    Assert.Contains(statements, (fun text -> text.Contains "Pat Lee"))
    Assert.All(disagreements case, (fun found -> Assert.Equal(DiscrepancyField.Officers, found.Field)))

[<Fact>]
let ``a source listing no officers isn't compared`` () =
    let case =
        opened
        |> said key "Register" EvidenceOrigin.Register [ Fact.Officer "Jane Doe" ]
        |> said key "Vendor" EvidenceOrigin.Vendor [ Fact.Name "EXAMPLE TRADING LTD" ]

    Assert.Empty(disagreements case)

[<Fact>]
let ``a disagreement that goes away takes its flag, and one that stays keeps its id`` () =
    let first =
        opened
        |> said key "A" EvidenceOrigin.Register [ Fact.Name "A LTD" ]
        |> said key "B" EvidenceOrigin.Vendor [ Fact.Name "B LTD" ]

    let flagId = first.Flags.Head.Id
    let more = first |> said key "C" EvidenceOrigin.Operator [ Fact.Other("x", "y") ]
    Assert.Equal<string list>([ flagId ], more.Flags |> List.map _.Id)

    let without =
        { more with
            Evidence = more.Evidence |> List.filter (fun item -> item.Source <> "B")
        }
        |> said key "D" EvidenceOrigin.Operator [ Fact.Other("x", "z") ]

    Assert.Empty without.Flags

[<Fact>]
let ``two companies giving one address show a neutral flag on each`` () =
    let other =
        company (EntityIdentity.OnRegister("Companies House", gb, "00000009")) "OTHER LTD"

    let case =
        opened
        |> CaseFile.addEntity other
        |> said
            key
            "CH"
            EvidenceOrigin.Register
            [
                Fact.Address(AddressKind.RegisteredOffice, "1 Office Row, London", ValueNone)
            ]
        |> said
            other.Key
            "CH"
            EvidenceOrigin.Register
            [ Fact.Address(AddressKind.RegisteredOffice, "1 OFFICE ROW LONDON", ValueNone) ]

    let flags =
        case.Flags |> List.filter (fun flag -> flag.Kind = FlagKind.SharedAddress)

    Assert.Equal(2, flags.Length)
    Assert.All(flags, (fun flag -> Assert.Contains("shows that the address is shared, and no more", flag.Statement)))

[<Fact>]
let ``an address on a list the host holds shows what the list calls it`` () =
    let list =
        {
            Source = "The host's provider list"
            Dated = DateOnly(2026, 1, 1)
            Addresses =
                [
                    {
                        Text = "1 Office Row"
                        Label = "a virtual office provider"
                    }
                ]
        }

    let case =
        opened
        |> said
            key
            "CH"
            EvidenceOrigin.Register
            [
                Fact.Address(AddressKind.RegisteredOffice, "1 Office Row, London", ValueNone)
            ]
        |> CaseFile.checkKnownAddresses [ list ]

    let flag = case.Flags |> List.find (fun flag -> flag.Kind = FlagKind.VirtualAddress)
    Assert.Contains("a virtual office provider", flag.Statement)
    Assert.Contains("nothing about who uses it", flag.Statement)
    Assert.Equal("The host's provider list", flag.Source)

// JSON.

/// A case holding every shape the file can hold.
let private fullCase =
    let document (pid: string) : CaseDocument =
        {
            Id = pid
            Entity = key
            Register = "Companies House"
            DocumentId = ValueSome "doc-1"
            TransactionId = "tx-1"
            Kind = DocumentKind.Insolvency
            FormType = ValueSome "AM10"
            Description = ValueSome "Progress report"
            FiledOn = ValueSome(DateOnly(2020, 2, 3))
            Pages = ValueSome 4
            Link = ValueSome "/document/doc-1"
        }

    let screening =
        {
            At = at
            ListsRead =
                [
                    {
                        List = SanctionsListName.UkFcdo
                        Dated = DateOnly(2026, 11, 3)
                        StatedInFile = ValueSome(DateOnly(2026, 11, 3))
                        Entries = 2
                        Notes = [ "A note" ]
                    }
                ]
            ListsNotUsed = SanctionsScreening.knownGaps
            NamesScreened = [ "EXAMPLE TRADING LTD" ]
            NamesNotScreened =
                [
                    {
                        Who = "owners"
                        Reason = "Not identified"
                    }
                ]
            PossibleMatches =
                [
                    {
                        ScreenedName = "EXAMPLE TRADING LTD"
                        EntityKey = ValueSome key
                        List = SanctionsListName.UnConsolidated
                        ListDated = DateOnly(2026, 11, 6)
                        EntryId = "ZEe.001"
                        ListedName = "EXAMPLE TRADING"
                        Programme = ValueSome "ZED"
                        Score = 0.937
                        ScoreReason = "Similar spelling"
                        BirthYear = DetailComparison.BothSidesDiffer("1990", "1960")
                        Country = DetailComparison.BothSidesMatch "ZEDLAND"
                    }
                    {
                        ScreenedName = "X Y"
                        EntityKey = ValueNone
                        List = SanctionsListName.OfacSdn
                        ListDated = DateOnly(2026, 11, 6)
                        EntryId = "1"
                        ListedName = "X Y"
                        Programme = ValueNone
                        Score = 1.
                        ScoreReason = "Same"
                        BirthYear = DetailComparison.NotBothKnown
                        Country = DetailComparison.NotBothKnown
                    }
                ]
        }

    opened
    |> CaseFile.addEntity
        { delaware with
            Identities =
                [
                    EntityIdentity.GermanCourtRegister("Köln", "HRB", "1")
                    EntityIdentity.Lei "529900W18LQJJN6SJ336"
                    EntityIdentity.NameOnly("A name", "a filing")
                ]
            Standing = ValueSome(CompanyStanding.Registered ValueNone)
            Party = PartyKind.Unknown(ValueSome "odd")
            Role = RecordRole.ForeignRegistration
        }
    |> CaseFile.addLink
        key
        delaware.Key
        LinkKind.HomeRecord
        (LinkProvenance.Register "CH")
        [ "E1" ]
        (ValueSome "A note")
    |> CaseFile.addLink
        key
        delaware.Key
        (LinkKind.StatedRelationship "said so")
        (LinkProvenance.OperatorDeclared officer)
        []
        ValueNone
    |> CaseFile.addLink key delaware.Key LinkKind.Creditor (LinkProvenance.FilingDocument("D1", "E1")) [] ValueNone
    |> CaseFile.addLink key delaware.Key LinkKind.Parent (LinkProvenance.CrossCheck "GLEIF") [] ValueNone
    |> CaseFile.addLink key delaware.Key LinkKind.Owner (LinkProvenance.Vendor("V", "R")) [] ValueNone
    |> said
        key
        "A \"quoted\" source\nwith a line end"
        EvidenceOrigin.Register
        [
            Fact.Name "Ñame — with Ünicode"
            Fact.RegistrationNumber "1"
            Fact.Status("s", StatusReading.InProceeding)
            Fact.Address(AddressKind.Other "A label", "1 Road", ValueSome "AB1")
            Fact.Address(AddressKind.Service, "2 Road", ValueNone)
            Fact.LegalForm "ltd"
            Fact.Capital
                {
                    Kind = CapitalKind.Unlabelled(ValueSome "stated")
                    Shares = ValueSome 10000000.5m
                    Amount = ValueSome 0.01m
                    Currency = ValueSome "GBP"
                }
            Fact.Officer "Jane Doe"
            Fact.BornIn { Year = 1970; Month = 3 }
            Fact.Country "Zedland"
            Fact.Other("topic", "text")
        ]
    |> CaseFile.addDocument document
    |> CaseFile.addGap (ValueSome key) GapReach.ThisEntity GapKind.NeedsPaidContract "s" "d" "w"
    |> (fun case ->
        { case with
            Gaps =
                case.Gaps
                @ [
                    {
                        Id = "G90"
                        About = ValueNone
                        Reach = GapReach.WholeCase
                        Kind = GapKind.SourceFailed
                        Subject = "closed"
                        Detail = "d"
                        WouldClose = "w"
                        ClosedBy = ValueSome(officer, "E1")
                    }
                ]
            VendorChecks =
                [
                    { clearCheck with
                        Hits =
                            [
                                {
                                    Subject = "s"
                                    Words = "w"
                                    Detail = ValueSome "d"
                                    ShowAs = ValueSome FlagKind.NominalCapital
                                }
                            ]
                        Gaps =
                            [
                                {
                                    Subject = "s"
                                    Words = "w"
                                    WouldClose = ValueNone
                                }
                            ]
                        Facts = [ Fact.Name "v" ]
                    }
                ]
            Screenings = [ screening ]
        })
    |> CaseFile.addNote (ValueSome key) "A note" officer
    |> expectOk
    |> CaseFile.addFlag key FlagKind.Insolvency "s" "src" [ "E1" ]

[<Fact>]
let ``a case file round-trips through JSON, every shape in it`` () =
    let json = CaseJson.toJson fullCase

    match CaseJson.ofJson json with
    | Ok read ->
        Assert.Equal(fullCase, read)
        Assert.Equal(json, CaseJson.toJson read)
    | Error refused -> failwith $"%A{refused}"

[<Fact>]
let ``the new case round-trips too`` () =
    Assert.Equal(Ok opened, CaseJson.ofJson (CaseJson.toJson opened))

[<Fact>]
let ``JSON leaves a ValueNone out`` () =
    let json = CaseJson.toJson opened
    Assert.DoesNotContain("\"Standing\"", json)
    Assert.Contains("\"SchemaVersion\":1", json)

[<Theory; InlineData(""); InlineData("not json"); InlineData(null)>]
let ``text that isn't JSON is refused`` (text: string) =
    match CaseJson.ofJson text with
    | Error(CaseJsonRefused.NotJson _) -> ()
    | other -> failwith $"%A{other}"

[<Theory>]
[<InlineData("{}")>]
[<InlineData("[]")>]
[<InlineData("""{"SchemaVersion":99}""")>]
[<InlineData("""{"SchemaVersion":"1"}""")>]
let ``a file of no or another version is refused`` (text: string) =
    match CaseJson.ofJson text with
    | Error(CaseJsonRefused.WrongVersion _) -> ()
    | other -> failwith $"%A{other}"

[<Theory>]
[<InlineData("\"CaseId\"", "\"CaseIdentifier\"")>]
[<InlineData("\"Subject\":", "\"Unknown\":1,\"Subject\":")>]
[<InlineData("\"NotChecked\"", "\"NotACase\"")>]
[<InlineData("\"Reach\":\"WholeCase\"", "\"Reach\":3")>]
[<InlineData("\"OpenedAt\":\"2026-11-06T12:00:00+00:00\"", "\"OpenedAt\":\"yesterday\"")>]
[<InlineData("\"Entities\":[", "\"Entities\":{\"x\":[")>]
let ``a file damaged in one place is refused whole`` (from: string, into: string) =
    let json = CaseJson.toJson opened
    Assert.Contains(from, json)

    match CaseJson.ofJson (json.Replace(from, into)) with
    | Error _ -> ()
    | Ok _ -> failwith "Read a damaged file"

[<Fact>]
let ``a union with the wrong number of fields is refused`` () =
    let json = CaseJson.toJson opened
    let from = "\"OnRegister\",\"fields\":[\"Companies House\",\"GB\",\"00000001\"]"
    Assert.Contains(from, json)

    match CaseJson.ofJson (json.Replace(from, "\"OnRegister\",\"fields\":[\"Companies House\",\"GB\"]")) with
    | Error(CaseJsonRefused.WrongShape _) -> ()
    | other -> failwith $"%A{other}"

[<Fact>]
let ``a jurisdiction that isn't a code is refused`` () =
    let json = CaseJson.toJson opened
    let from = "\"Jurisdiction\":\"GB\""
    Assert.Contains(from, json)

    match CaseJson.ofJson (json.Replace(from, "\"Jurisdiction\":\"Great Britain\"")) with
    | Error(CaseJsonRefused.WrongShape _) -> ()
    | other -> failwith $"%A{other}"

// The file has no verdict in it.

/// Every field name and union case reachable from a type.
let rec private namesIn (seen: Set<string>) (kind: Type) : Set<string> =
    if seen.Contains kind.FullName || kind = typeof<string> then
        seen
    elif FSharpType.IsRecord kind then
        FSharpType.GetRecordFields kind
        |> Array.fold
            (fun set field -> namesIn (Set.add field.Name set) field.PropertyType)
            (Set.add kind.FullName seen)
    elif FSharpType.IsUnion kind && kind.FullName <> typeof<Jurisdiction>.FullName then
        FSharpType.GetUnionCases kind
        |> Array.fold
            (fun set case ->
                case.GetFields()
                |> Array.fold
                    (fun inner field -> namesIn (Set.add field.Name inner) field.PropertyType)
                    (Set.add case.Name set))
            (Set.add kind.FullName seen)
    elif FSharpType.IsTuple kind then
        FSharpType.GetTupleElements kind |> Array.fold namesIn seen
    else
        seen

[<Fact>]
let ``nothing in the case file's shape holds an overall result`` () =
    let names = namesIn Set.empty typeof<CaseFile>
    Assert.Contains("Flags", names)
    Assert.Contains("OperatorDeclared", names)

    for word in
        [
            "Pass"
            "Passed"
            "Fail"
            "Failed"
            "Clear"
            "Cleared"
            "Verdict"
            "Approved"
            "Rejected"
            "RiskScore"
            "Decision"
            "Result"
            "Outcome"
        ] do
        Assert.DoesNotContain(word, names)

[<Fact>]
let ``the export says the decision is the officer's, and shows no result of its own`` () =
    let text = CaseExport.toText false withOwner
    Assert.Contains("The decision is the officer's", text)
    let lower = text.ToLowerInvariant()

    for word in [ "not sanctioned"; "passed"; "verdict"; "approved"; "low risk" ] do
        Assert.DoesNotContain(word, lower)

[<Fact>]
let ``the export leaves raw answers out unless asked`` () =
    let case =
        { (opened |> said key "A" EvidenceOrigin.Register []) with
            Evidence =
                [
                    { (opened |> said key "A" EvidenceOrigin.Register []).Evidence.Head with
                        Raw = ValueSome "RAW-ANSWER"
                    }
                ]
        }

    Assert.DoesNotContain("RAW-ANSWER", CaseExport.toText false case)
    Assert.Contains("RAW-ANSWER", CaseExport.toText true case)

[<Fact>]
let ``an empty review list in the export says it clears nothing`` () =
    let empty = { opened with Gaps = [] }

    Assert.Empty(CaseFile.reviewNeeded empty)
    Assert.Contains("doesn't mean anything has been cleared", CaseExport.toText false empty)

[<Fact>]
let ``an older stored case is upgraded one version at a time, and a missing or broken step refuses it`` () =
    let current = CaseJson.toJson opened

    // A version 0 that had no Notes: its step adds them, empty, and says version 1.
    let older =
        let root = Nodes.JsonNode.Parse(current).AsObject()
        root.Remove "Notes" |> ignore
        root["SchemaVersion"] <- Nodes.JsonValue.Create 0
        root.ToJsonString()

    let addNotes (root: Nodes.JsonObject) =
        root["Notes"] <- Nodes.JsonArray()
        root["SchemaVersion"] <- Nodes.JsonValue.Create 1
        root

    Assert.Equal(Ok opened, CaseJson.ofJsonWith (Map [ 0, addNotes ]) 1 older)
    Assert.Equal(Error(CaseJsonRefused.WrongVersion(ValueSome 0)), CaseJson.ofJsonWith Map.empty 1 older)

    let forgetsVersion (root: Nodes.JsonObject) =
        root["Notes"] <- Nodes.JsonArray()
        root

    Assert.Equal(
        Error(CaseJsonRefused.WrongVersion(ValueSome 0)),
        CaseJson.ofJsonWith (Map [ 0, forgetsVersion ]) 1 older
    )
    // A file from a newer library is refused, never read as this version.
    Assert.Equal(
        Error(CaseJsonRefused.WrongVersion(ValueSome 2)),
        CaseJson.ofJsonWith Map.empty 1 (current.Replace("\"SchemaVersion\":1", "\"SchemaVersion\":2"))
    )

[<Fact>]
let ``a case holding a null text can't be written, and says so`` () =
    let error =
        Assert.Throws<ArgumentException>(fun () -> CaseJson.toJson { opened with CaseId = null } |> ignore)

    Assert.Contains("null", error.Message)

[<Fact>]
let ``a stored case holding a name twice in one object is refused`` () =
    let json = CaseJson.toJson opened
    let doubled = json.Replace("\"CaseId\":", "\"CaseId\":\"X\",\"CaseId\":")

    match CaseJson.ofJson doubled with
    | Error(CaseJsonRefused.WrongShape _) -> ()
    | other -> failwith $"%A{other}"
