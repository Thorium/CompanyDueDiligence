namespace CompanyDueDiligence

open System
open System.Text.RegularExpressions
open System.Threading
open System.Threading.Tasks

/// Reads a filing's content into text. Filings are often scanned images, and no OCR ships here: the host plugs in its own.
type IDocumentReader =
    /// The reader's name, shown as who read the document.
    abstract Name: string

    /// What the document says, with any facts picked out of it. An Error stays a gap.
    abstract ReadAsync:
        document: CaseDocument * file: DocumentFile * ct: CancellationToken -> Task<Result<string * Fact list, string>>

/// What an investigation reads from.
type CaseSources =
    {
        /// Registers. Every one covering a jurisdiction is asked, those naming its exact code first and then in this order; the first that holds the
        /// company is the record followed.
        Registries: ICompanyRegistry list
        /// Registers' filings, establishments and insolvency records.
        Filings: IFilingRegister list
        /// GLEIF, as a cross-check. ValueNone leaves it out and says so in a gap.
        CrossCheck: ILeiCrossCheck voption
        /// Routes for jurisdictions no register here reads. CoverageRoutes.all is the set this package declares.
        Routes: CoverageRoute list
        /// Address lists the host holds, such as virtual office providers.
        KnownAddresses: KnownAddressList list
    }

/// Building case sources.
module CaseSources =

    /// Sources of these registers: the filing registers among them as Filings, CoverageRoutes.all, and no cross-check or address lists. Add the
    /// rest with { CaseSources.ofRegistries ... with ... }, so that a field added later breaks no host.
    let ofRegistries (registries: ICompanyRegistry list) : CaseSources =
        {
            Registries = registries
            Filings =
                registries
                |> List.choose (fun registry ->
                    match registry with
                    | :? IFilingRegister as filings -> Some filings
                    | _ -> None)
            CrossCheck = ValueNone
            Routes = CoverageRoutes.all
            KnownAddresses = []
        }

/// How far an investigation reads and how it judges what it reads.
type InvestigationOptions =
    {
        /// How many links from the start to follow. A choice made without data.
        MaxDepth: int
        /// How many company records to read in one run. A choice made without data.
        MaxEntities: int
        Purpose: OwnerPurpose
        /// A filing's capital at or below this figure, in any currency, shows as a nominal capital flag. A choice made without data: set your own.
        NominalCapitalAtMost: decimal
        /// The fewest entries a list may hold when a case is screened. ListMinimums.defaults says where its figures come from.
        ListMinimums: ListMinimums
        Now: unit -> DateTimeOffset
    }

/// Options to start from.
module InvestigationOptions =

    /// Four links deep, at most 25 company records, owners read for customer due diligence, nominal capital at 100, ListMinimums.defaults and
    /// the system clock in UTC. Set your own with { InvestigationOptions.defaults with ... }.
    let defaults =
        {
            MaxDepth = 4
            MaxEntities = 25
            Purpose = OwnerPurpose.CustomerDueDiligence
            NominalCapitalAtMost = 100m
            ListMinimums = ListMinimums.defaults
            Now = fun () -> DateTimeOffset.UtcNow
        }

/// One company to start a case from.
type CaseStart =
    {
        CaseId: string
        Number: string
        Jurisdiction: Jurisdiction
        /// The name as the customer gave it, shown until a register answers.
        NameGiven: string
    }

/// Building case starts.
module CaseStart =

    /// A case start; add what a later version adds with { CaseStart.create ... with ... }.
    let create (caseId: string) (number: string) (jurisdiction: Jurisdiction) (nameGiven: string) : CaseStart =
        {
            CaseId = caseId
            Number = number
            Jurisdiction = jurisdiction
            NameGiven = nameGiven
        }

/// Registers' answers as facts and flags.
module internal Reading =

    /// What a party with no name in the answer is called in a case.
    [<Literal>]
    let NoName = PartyNames.NotGiven

    let addressText (address: Address) =
        address.Lines
        @ ([ address.Locality; address.Region; address.PostalCode; address.Country ]
           |> List.choose ValueOption.toOption)
        |> String.concat ", "

    let statusReading standing =
        match standing with
        | CompanyStanding.Active
        | CompanyStanding.ActiveWithConcern _
        | CompanyStanding.Registered _ -> StatusReading.Live
        | CompanyStanding.InInsolvency _
        | CompanyStanding.WindingUp _ -> StatusReading.InProceeding
        | CompanyStanding.Dissolved
        | CompanyStanding.Closed _ -> StatusReading.Ended
        | CompanyStanding.Unknown _ -> StatusReading.NotPlaced

    let standingWords standing =
        match standing with
        | CompanyStanding.Active -> "active"
        | CompanyStanding.ActiveWithConcern concern -> $"active, with {concern}"
        | CompanyStanding.Registered(ValueSome word) -> word
        | CompanyStanding.Registered ValueNone -> "registered, the register using no status word"
        | CompanyStanding.InInsolvency proceeding -> proceeding
        | CompanyStanding.WindingUp word -> word
        | CompanyStanding.Dissolved -> "dissolved"
        | CompanyStanding.Closed word -> word
        | CompanyStanding.Unknown(ValueSome word) -> word
        | CompanyStanding.Unknown ValueNone -> "no status given"

    /// True when two numbers are one after NameMatch.normalise, with spaces and leading zeros taken out, as Discrepancies compares them.
    /// A number holds a digit and isn't all zeros: "N/A", "NONE" or "0" in a number field is no number. SF000939 on Companies House states
    /// registration_number "N/A".
    let isNumber (number: string) =
        not (isNull number)
        && number |> Seq.exists Char.IsDigit
        && number |> Seq.exists (fun c -> Char.IsLetterOrDigit c && c <> '0')

    let private sameNumberRegex = Regex @"\s+"
    let sameNumber (a: string) (b: string) =
        let key (number: string) =
            sameNumberRegex.Replace(NameMatching.normalise number, "").TrimStart '0'

        key a = key b

    let companyFacts (company: Company) =
        [
            Fact.Name company.Name

            for name in company.OtherNames do
                Fact.OtherName("other name on the register", name)

            Fact.RegistrationNumber company.Number
            Fact.Status(
                company.StatusAsPublished
                |> ValueOption.defaultValue (standingWords company.Standing),
                statusReading company.Standing
            )

            match company.LegalForm with
            | ValueSome form -> Fact.LegalForm form
            | ValueNone -> ()

            match company.Addresses with
            | Available addresses ->
                for address in addresses do
                    Fact.Address(address.Kind, addressText address.Address, address.Address.PostalCode)
            | NotAvailable _
            | NeedsEntitlement _ -> ()
        ]

    /// Standing as a flag. Live standings raise none; every other is shown, in the register's words.
    let standingFlag (company: Company) =
        let word = standingWords company.Standing

        match company.Standing with
        | CompanyStanding.Active
        | CompanyStanding.Registered _ -> ValueNone
        | CompanyStanding.InInsolvency _ -> ValueSome(FlagKind.Insolvency, $"The register's status is {word}")
        | CompanyStanding.ActiveWithConcern _
        | CompanyStanding.WindingUp _
        | CompanyStanding.Dissolved
        | CompanyStanding.Closed _
        | CompanyStanding.Unknown _ -> ValueSome(FlagKind.StatusConcern, $"The register's status is {word}")

    /// A live standing that confirms nothing, as a gap's detail: a register with no status word.
    let standingUnconfirmed (company: Company) =
        match company.Standing with
        | CompanyStanding.Registered ValueNone ->
            ValueSome "The register gives no status word, so the company's standing is unconfirmed"
        | _ -> ValueNone

    let dateText (date: DateOnly) =
        date.ToString("yyyy-MM-dd", Globalization.CultureInfo.InvariantCulture)

    /// The date an answer's data is as of: the source's own date, else the day it was retrieved, which for a bulk copy is the download.
    let dataDate (asOf: DateOnly voption) (retrievedAt: DateTimeOffset) =
        asOf |> ValueOption.defaultValue (DateOnly.FromDateTime retrievedAt.UtcDateTime)

    let roleWords role =
        match role with
        | OfficerRole.Director -> "director"
        | OfficerRole.Secretary -> "secretary"
        | OfficerRole.Other asPublished -> asPublished

    /// One nature of control: its kind, its band if any, and the register's own words.
    let controlWords (control: NatureOfControl) =
        let kind =
            match control.Kind with
            | ControlKind.Ownership -> "ownership"
            | ControlKind.VotingRights -> "voting rights"
            | ControlKind.AppointsBoard -> "appoints the board"
            | ControlKind.SignificantInfluence -> "significant influence"
            | ControlKind.Other -> "other control"

        let band =
            match control.Band with
            | ValueSome band when band.From = band.To -> $" {band.From}%%"
            | ValueSome band -> $" {band.From} to {band.To}%%"
            | ValueNone -> ", no band given"

        if String.IsNullOrWhiteSpace control.AsPublished then
            kind + band
        else
            $"{kind}{band} ({control.AsPublished})"

    let concernFlag concern =
        match concern with
        | CompanyConcern.AccountsOverdue -> FlagKind.OverdueFiling, "The register shows accounts overdue"
        | CompanyConcern.AnnualFilingOverdue -> FlagKind.OverdueFiling, "The register shows an annual filing overdue"
        | CompanyConcern.RegisteredOfficeDisputed ->
            FlagKind.RegisteredOfficeProblem, "The register shows the registered office in dispute"
        | CompanyConcern.RegisteredOfficeUndeliverable ->
            FlagKind.RegisteredOfficeProblem, "The register shows the registered office as undeliverable"
        | CompanyConcern.InsolvencyHistory -> FlagKind.Insolvency, "The register shows insolvency history"

    let concernWord concern =
        match concern with
        | CompanyConcern.AccountsOverdue -> "accounts overdue"
        | CompanyConcern.AnnualFilingOverdue -> "annual filing overdue"
        | CompanyConcern.RegisteredOfficeDisputed -> "registered office disputed"
        | CompanyConcern.RegisteredOfficeUndeliverable -> "registered office undeliverable"
        | CompanyConcern.InsolvencyHistory -> "insolvency history"

    let ownershipGapWords gap =
        match gap with
        | OwnershipGap.OwnerNotIdentified -> "The register says an owner exists that it doesn't identify"
        | OwnershipGap.OwnerDetailsNotConfirmed -> "An owner's details aren't confirmed or provided"
        | OwnershipGap.InquiriesIncomplete -> "The company hasn't finished finding out who its owners are"
        | OwnershipGap.OwnerNotCooperating -> "An owner hasn't answered a formal notice, or has restrictions for that"
        | OwnershipGap.OwnerWithheld -> "The register holds a current owner but withholds who it is"
        | OwnershipGap.OwnerUnreadable -> "A current owner's kind or name couldn't be read"
        | OwnershipGap.EntityNotIdentified -> "A corporate owner has no registration number to follow"
        | OwnershipGap.StatementContradictsOwners ->
            "A statement says there is no one to list, yet a current owner is listed"
        | OwnershipGap.StatementNotUnderstood code -> $"A statement this library doesn't recognise: {code}"

    /// The category a filing goes under. Category words are the register's own, and forms not placed here stay Other.
    /// Capital filings aren't constitutional: a listed company files hundreds of allotments, buy-backs and cancellations (824 of Tesco PLC's
    /// newest 1,000 filings on 2026-11-08). Their figures are read as facts either way.
    let documentKind (category: string voption) =
        match category with
        | ValueSome("insolvency" | "liquidation" | "administration" | "receivership") -> DocumentKind.Insolvency
        | ValueSome("incorporation" | "resolution" | "change-of-constitution" | "constitution" | "overseas" | "reregistration") ->
            DocumentKind.Constitutional
        | other -> DocumentKind.Other other

    let failureGap (failure: RegistryFailure) =
        let wouldClose =
            match failure with
            | RegistryFailure.RateLimited _
            | RegistryFailure.Unavailable _ -> "Read it again later"
            | RegistryFailure.InvalidRequest _ -> "Correct the number, or have a person look it up"
            | RegistryFailure.AccessRefused _
            | RegistryFailure.RequestRefused _ -> "Fix the access or the request, then read it again"
            | RegistryFailure.UnreadableAnswer _ -> "Have a person read the source; the adapter may need updating"
            | RegistryFailure.OutsideCoverage _ -> "Use a source that covers this jurisdiction"

        RegistryFailure.describe failure, wouldClose

/// Starts from one company and follows corporate owners, UK establishments and home records into whichever source covers each country, within a depth limit
/// and never reading one entity twice. What it can't follow becomes a gap. It decides nothing.
module Investigation =

    /// One record still to read.
    type private Pending =
        {
            Key: string
            Number: string
            Jurisdiction: Jurisdiction
            Depth: int
            /// The link that led here, for saying what a gap holds up.
            Via: string
        }

    /// Calls a source so that a host's own adapter throwing is a failure like any other. Cancelling still throws.
    let private guarded (ct: CancellationToken) (call: unit -> Task<Result<'T, RegistryFailure>>) =
        task {
            try
                return! call ()
            with error when not (ct.IsCancellationRequested && (error :? OperationCanceledException)) ->
                return Error(RegistryFailure.Unavailable $"The source raised {error.GetType().Name}")
        }

    let private record (about: string) topic origin (answer: Evidence<'T>) facts note (case: CaseFile) =
        CaseFile.addEvidence
            (fun pid ->
                {
                    Id = pid
                    About = about
                    Source = answer.Source
                    Origin = origin
                    Topic = topic
                    RetrievedAt = answer.RetrievedAt
                    AsOf = answer.AsOf
                    Raw = answer.Raw
                    Facts = facts
                    Attribution = ValueNone
                    DocumentId = ValueNone
                    Note = note
                })
            case

    /// A gap for a source's own answer that it has nothing to give. A coverage that says it doesn't offer this is a gap for the country.
    let private unavailableGap
        about
        reach
        subject
        (support: Support)
        (availability: Availability<'T>)
        (case: CaseFile)
        =
        match availability with
        | Available _ -> case
        | NeedsEntitlement entitlement ->
            CaseFile.addGap
                about
                reach
                GapKind.NeedsEntitlement
                subject
                $"Held for those entitled: {entitlement.Who}"
                entitlement.Route
                case
        | NotAvailable reason ->
            let kind =
                match support with
                | Support.NotOffered _ -> GapKind.NotAvailableForCountry
                | Support.Supported
                | Support.Partial _ -> GapKind.NotPublished

            CaseFile.addGap
                about
                reach
                kind
                subject
                reason
                "A vendor's report, or a person reading the register or the filings"
                case

    /// A number as it can be followed. A UK record's owner often gives a number that has lost its leading zeros, so an all-digit one is padded back to eight
    /// for the UK; the evidence keeps what was published.
    let private followableNumber (jurisdiction: Jurisdiction) (number: string) =
        let trimmed = number.Trim()

        if
            jurisdiction.Country = "GB"
            && trimmed.Length < 8
            && trimmed |> Seq.forall Char.IsDigit
        then
            trimmed.PadLeft(8, '0')
        else
            trimmed

    /// A stated country that can't be placed stays unplaced, whatever the other wording says: following it elsewhere could reach an unrelated company.
    /// within is where the record stating it is held, which decides what a bare two-letter code means.
    let private registration (within: Jurisdiction) (entity: EntityRegistration) =
        match entity.Country with
        | ValueSome country when
            not (String.IsNullOrWhiteSpace country)
            && (JurisdictionWords.placeWithin within [ entity.Country ]).IsNone
            ->
            ValueNone
        | _ -> JurisdictionWords.placeWithin within [ entity.Country; entity.Register; entity.GoverningLaw ]

    let private routeGapKind (route: CoverageRoute) =
        match route.Kind with
        | RouteKind.PaidContract -> GapKind.NeedsPaidContract
        | RouteKind.Entitlement -> GapKind.NeedsEntitlement

    /// The gap for a placed registration with no number: the declared route where there is one, else whether a source here could find it by name.
    let private noNumberGap (sources: CaseSources) (jurisdiction: Jurisdiction) (wording: string) =
        let placed =
            $"The record gives no registration number to follow. It places the company in {jurisdiction.Code}, from: {wording}"

        match
            CoverageRoutes.forJurisdiction sources.Routes jurisdiction,
            Registries.forJurisdiction sources.Registries jurisdiction
        with
        | ValueSome route, _ -> routeGapKind route, $"{placed}. {route.Register}: {route.WhyNoAdapter}", route.Route
        | ValueNone, ValueSome registry ->
            GapKind.NotPublished,
            placed,
            $"A person finds the number on {registry.Coverage.Registry}, such as by a search by name, and follows it"
        | ValueNone, ValueNone ->
            GapKind.NoRoute,
            $"{placed}. No source here covers {jurisdiction.Code}",
            "A person reading that register, a vendor's report, or an adapter for it"

    let private germanNumber =
        Regex(@"\A(HRA|HRB|GNR|PR|VR)\s*([0-9]+[A-Z]?)\z", RegexOptions.IgnoreCase)

    let private germanCourtPrefix =
        Regex(@"\A(Amtsgericht|AG|Registergericht|District Court)\s+", RegexOptions.IgnoreCase)

    /// A German registration, whose number means nothing without its court: court and section when the wording gives both, else the words as given.
    let private identityOf (registerName: string) (jurisdiction: Jurisdiction) (number: string) =
        let found = germanNumber.Match(number.Trim())

        if
            jurisdiction.Country = "DE"
            && found.Success
            && germanCourtPrefix.IsMatch registerName
        then
            EntityIdentity.GermanCourtRegister(
                germanCourtPrefix.Replace(registerName, "").Trim(),
                found.Groups[1].Value.ToUpperInvariant(),
                found.Groups[2].Value
            )
        else
            EntityIdentity.OnRegister(registerName, jurisdiction, number)

    /// A linked company's entity, its link, and the read it needs next. A registration with no number or no placeable jurisdiction becomes a gap on the
    /// whole case. A home record with no number is keyed by name and the jurisdiction placed, so registrations of one company in several places reach one
    /// home entity. within is where the pointing record is held, and note goes on the link.
    let private linkRegistration
        (sources: CaseSources)
        (within: Jurisdiction)
        (from: string)
        kind
        (register: string)
        (name: string)
        (entity: EntityRegistration)
        evidenceId
        (at: DateTimeOffset)
        depth
        (note: string voption)
        (case: CaseFile)
        =
        let entity =
            { entity with
                RegistrationNumber = entity.RegistrationNumber |> ValueOption.filter Reading.isNumber
            }

        let holdsUp = $"{CaseLink.describeKind kind} {name}"

        // What the pointing record says of the far record, which can then be set against what the far record says of itself.
        let stated =
            [
                match entity.LegalName with
                | ValueSome legalName -> Fact.Name legalName
                | ValueNone -> Fact.OtherName($"name stated by {from}", name)

                match entity.RegistrationNumber with
                | ValueSome number when number.Trim() <> "" -> Fact.RegistrationNumber number
                | _ -> ()

                match entity.LegalForm with
                | ValueSome form -> Fact.LegalForm form
                | ValueNone -> ()
            ]

        let toEntity identity jurisdiction =
            let target = CaseFile.entityOf identity name PartyKind.LegalEntity jurisdiction

            let linked =
                case
                |> CaseFile.addEntity target
                |> CaseFile.addLink from target.Key kind (LinkProvenance.Register register) [ evidenceId ] note

            let withFacts, _ =
                CaseFile.addEvidence
                    (fun pid ->
                        {
                            Id = pid
                            About = target.Key
                            Source = $"{register}, on the record of {from}"
                            Origin = EvidenceOrigin.Register
                            Topic = $"as stated by {from}"
                            RetrievedAt = at
                            AsOf = ValueNone
                            Raw = ValueNone
                            Facts = stated
                            Attribution = ValueNone
                            DocumentId = ValueNone
                            Note = ValueSome "What another record says of this one"
                        })
                    linked

            withFacts, target.Key

        let wording =
            [ entity.Country; entity.Register; entity.GoverningLaw ]
            |> List.choose ValueOption.toOption
            |> List.filter (String.IsNullOrWhiteSpace >> not)
            |> String.concat ", "

        match entity.RegistrationNumber, registration within entity with
        | ValueSome number, ValueSome jurisdiction when number.Trim() <> "" ->
            let followed = followableNumber jurisdiction number

            let registerName =
                entity.Register
                |> ValueOption.defaultValue $"the register in {jurisdiction.Code}"

            let updated, key =
                toEntity (identityOf registerName jurisdiction followed) (ValueSome jurisdiction)

            updated,
            [
                {
                    Key = key
                    Number = followed
                    Jurisdiction = jurisdiction
                    Depth = depth
                    Via = holdsUp
                }
            ]
        | ValueSome number, _ when number.Trim() <> "" ->
            let numbered =
                match entity.Register with
                | ValueSome far when not (String.IsNullOrWhiteSpace far) -> $"{far}: number {number}"
                | _ -> $"number {number}, as stated by {register}"

            let updated, key = toEntity (EntityIdentity.NameOnly(name, numbered)) ValueNone

            updated
            |> CaseFile.addGap
                (ValueSome key)
                GapReach.WholeCase
                GapKind.NoRoute
                holdsUp
                $"Can't place the jurisdiction from: {wording}"
                "A person works out which register it is on and reads it",
            []
        | _, ValueSome jurisdiction ->
            let context =
                match kind with
                | LinkKind.HomeRecord -> jurisdiction.Code
                | _ -> register

            let updated, key =
                toEntity (EntityIdentity.NameOnly(name, context)) (ValueSome jurisdiction)

            let gapKind, detail, wouldClose = noNumberGap sources jurisdiction wording

            updated
            |> CaseFile.addGap (ValueSome key) GapReach.WholeCase gapKind holdsUp detail wouldClose,
            []
        | _ ->
            let updated, key = toEntity (EntityIdentity.NameOnly(name, register)) ValueNone

            let detail =
                if wording = "" then
                    "The record gives no registration number to follow"
                else
                    $"The record gives no registration number to follow, and its place can't be read from: {wording}"

            updated
            |> CaseFile.addGap
                (ValueSome key)
                GapReach.WholeCase
                GapKind.NoRoute
                holdsUp
                detail
                "A person finds the number and adds it, or a vendor's report",
            []

    let private readOfficers (registry: ICompanyRegistry) options (pending: Pending) (case: CaseFile) ct =
        let reach =
            if pending.Depth = 0 then
                GapReach.WholeCase
            else
                GapReach.ThisEntity

        let about = ValueSome pending.Key

        task {
            match! guarded ct (fun () -> registry.OfficersAsync(pending.Number, pending.Jurisdiction, ct)) with
            | Error failure ->
                let detail, wouldClose = Reading.failureGap failure

                return
                    case
                    |> CaseFile.addGap about reach GapKind.SourceFailed "officers" detail wouldClose
            | Ok answer ->
                match answer.Value with
                | NotAvailable _
                | NeedsEntitlement _ ->
                    return unavailableGap about reach "officers" registry.Coverage.Officers answer.Value case
                | Available officers ->
                    let current = officers |> List.filter _.Current

                    let facts = current |> List.map (fun officer -> Fact.Officer officer.Name)

                    let withEvidence, id =
                        record pending.Key "officers" EvidenceOrigin.Register answer facts ValueNone case

                    let today = DateOnly.FromDateTime((options.Now(): DateTimeOffset).UtcDateTime)

                    return
                        current
                        |> List.indexed
                        |> List.fold
                            (fun (state: CaseFile) (index, officer) ->
                                let unnamed =
                                    officer.Kind = PartyKind.Withheld || PartyNames.isPlaceholder officer.Name

                                let position =
                                    $"{Reading.roleWords officer.Role}, no. {index + 1} of {current.Length} current"

                                // An officer with no name is told apart from the others by role and place in the list, and is a gap of its own.
                                let identity =
                                    if unnamed then
                                        EntityIdentity.NameOnly(
                                            officer.Name,
                                            $"{answer.Source}, officer of {pending.Key}, {position}"
                                        )
                                    else
                                        EntityIdentity.NameOnly(
                                            officer.Name,
                                            $"{answer.Source}, officer of {pending.Key}"
                                        )

                                let target = CaseFile.entityOf identity officer.Name officer.Kind ValueNone

                                let withUnnamed =
                                    if not unnamed then
                                        state
                                    else
                                        let withholds =
                                            if officer.Kind = PartyKind.Withheld then
                                                "withholds the name of"
                                            else
                                                "gives no name for"

                                        state
                                        |> CaseFile.addGap
                                            about
                                            reach
                                            GapKind.NotPublished
                                            "officers"
                                            $"{answer.Source} {withholds} an officer: {position}"
                                            "The company's own register of officers, a vendor's report, or a person asking the company"

                                let withLink =
                                    withUnnamed
                                    |> CaseFile.addEntity target
                                    |> CaseFile.addLink
                                        pending.Key
                                        target.Key
                                        LinkKind.Officer
                                        (LinkProvenance.Register answer.Source)
                                        [ id ]
                                        ValueNone

                                let withFacts, _ =
                                    record
                                        target.Key
                                        "officer details"
                                        EvidenceOrigin.Register
                                        { answer with Raw = ValueNone }
                                        [
                                            match officer.BornIn with
                                            | ValueSome born -> Fact.BornIn born
                                            | ValueNone -> ()

                                            match officer.Nationality with
                                            | ValueSome country -> Fact.Country country
                                            | ValueNone -> ()

                                            match officer.CountryOfResidence with
                                            | ValueSome country -> Fact.Country country
                                            | ValueNone -> ()
                                        ]
                                        ValueNone
                                        withLink

                                // A verification statement still unsupplied past its due date is shown, with no conclusion about why.
                                match officer.IdentityVerification with
                                | ValueSome {
                                                StatementDueOn = ValueSome due
                                                StatementSuppliedOn = ValueNone
                                            } when due < today ->
                                    withFacts
                                    |> CaseFile.addFlag
                                        target.Key
                                        FlagKind.IdentityVerificationPastDue
                                        $"The register shows an identity verification statement for {officer.Name} due on {due:``yyyy-MM-dd``}, with none recorded as supplied"
                                        answer.Source
                                        [ id ]
                                | _ -> withFacts)
                            withEvidence
        }

    /// What an owner's link says of its control, kind and band as published.
    let private controlNote (owner: Owner) =
        match owner.Control with
        | [] -> ValueNone
        | controls ->
            ValueSome(
                "Control as published: "
                + (controls |> List.map Reading.controlWords |> String.concat "; ")
            )

    /// Owners, linked with their control. A partial coverage of owners, and current owners with no band at all, are gaps.
    let private readOwners
        (sources: CaseSources)
        (registry: ICompanyRegistry)
        options
        (pending: Pending)
        (case: CaseFile)
        ct
        =
        let about = ValueSome pending.Key

        task {
            match!
                guarded ct (fun () -> registry.OwnersAsync(pending.Number, pending.Jurisdiction, options.Purpose, ct))
            with
            | Error failure ->
                let detail, wouldClose = Reading.failureGap failure

                return
                    case
                    |> CaseFile.addGap about GapReach.WholeCase GapKind.SourceFailed "owners" detail wouldClose,
                    []
            | Ok answer ->
                match answer.Value with
                | NotAvailable _
                | NeedsEntitlement _ ->
                    return
                        unavailableGap about GapReach.WholeCase "owners" registry.Coverage.Owners answer.Value case, []
                | Available listing ->
                    let withEvidence, id =
                        record pending.Key "owners" EvidenceOrigin.Register answer [] ValueNone case

                    let withGaps =
                        listing.Gaps
                        |> List.fold
                            (fun (state: CaseFile) gap ->
                                state
                                |> CaseFile.addGap
                                    about
                                    GapReach.WholeCase
                                    GapKind.NotPublished
                                    "owners"
                                    (Reading.ownershipGapWords gap)
                                    "The company's own register of owners, a vendor's report, or a person asking the company")
                            withEvidence

                    let current = listing.Owners |> List.filter _.Current

                    let withPartial =
                        match registry.Coverage.Owners with
                        | Support.Partial caveat ->
                            withGaps
                            |> CaseFile.addGap
                                about
                                GapReach.WholeCase
                                GapKind.NotPublished
                                "owners"
                                $"{answer.Source}: {caveat}"
                                "The company's own register of owners, a vendor's report, or a person asking the company"
                        | Support.Supported
                        | Support.NotOffered _ -> withGaps

                    let withBands =
                        if
                            not current.IsEmpty
                            && current |> List.forall (fun owner -> owner.Control |> List.forall _.Band.IsNone)
                        then
                            withPartial
                            |> CaseFile.addGap
                                about
                                GapReach.WholeCase
                                GapKind.NotPublished
                                "shareholdings"
                                $"{answer.Source} gives no share or voting band for any current owner, so how much each holds isn't known"
                                "The company's register of members, its filings, or a vendor's report"
                        else
                            withPartial

                    return
                        current
                        |> List.indexed
                        |> List.fold
                            (fun ((state: CaseFile), more) (index, owner) ->
                                let name = owner.Name |> ValueOption.defaultValue Reading.NoName
                                let note = controlNote owner

                                match owner.Kind, owner.Entity with
                                | PartyKind.LegalEntity, ValueSome entity ->
                                    let updated, next =
                                        linkRegistration
                                            sources
                                            pending.Jurisdiction
                                            pending.Key
                                            LinkKind.Owner
                                            answer.Source
                                            (entity.LegalName |> ValueOption.defaultValue name)
                                            entity
                                            id
                                            answer.RetrievedAt
                                            (pending.Depth + 1)
                                            note
                                            state

                                    updated, more @ next
                                | _ ->
                                    // An owner with no name is told apart from the others by its place in the list.
                                    let identity =
                                        if owner.Name.IsNone || PartyNames.isPlaceholder name then
                                            EntityIdentity.NameOnly(
                                                name,
                                                $"{answer.Source}, owner of {pending.Key}, no. {index + 1} of {current.Length} current"
                                            )
                                        else
                                            EntityIdentity.NameOnly(name, $"{answer.Source}, owner of {pending.Key}")

                                    let target = CaseFile.entityOf identity name owner.Kind ValueNone

                                    let linked =
                                        state
                                        |> CaseFile.addEntity target
                                        |> CaseFile.addLink
                                            pending.Key
                                            target.Key
                                            LinkKind.Owner
                                            (LinkProvenance.Register answer.Source)
                                            [ id ]
                                            note

                                    let withFacts, _ =
                                        record
                                            target.Key
                                            "owner details"
                                            EvidenceOrigin.Register
                                            { answer with Raw = ValueNone }
                                            [
                                                match owner.BornIn with
                                                | ValueSome born -> Fact.BornIn born
                                                | ValueNone -> ()

                                                match owner.Nationality with
                                                | ValueSome country -> Fact.Country country
                                                | ValueNone -> ()

                                                match owner.CountryOfResidence with
                                                | ValueSome country -> Fact.Country country
                                                | ValueNone -> ()
                                            ]
                                            ValueNone
                                            linked

                                    withFacts, more)
                            (withBands, [])
        }

    let private filingRegisterFor (sources: CaseSources) (jurisdiction: Jurisdiction) =
        sources.Filings
        |> List.tryFind (fun register ->
            register.Coverage.Jurisdictions
            |> List.exists (fun scope -> Jurisdiction.covers scope jurisdiction))

    /// Filings surfaced as documents, with their capital figures as facts.
    let private readFilings (register: IFilingRegister) options (pending: Pending) (case: CaseFile) ct =
        task {
            match! guarded ct (fun () -> register.FilingsAsync(pending.Number, ct)) with
            | Error failure ->
                let detail, wouldClose = Reading.failureGap failure

                return
                    case
                    |> CaseFile.addGap
                        (ValueSome pending.Key)
                        GapReach.WholeCase
                        GapKind.SourceFailed
                        "filings"
                        detail
                        wouldClose
            | Ok answer ->
                match answer.Value with
                | NotAvailable reason ->
                    return
                        case
                        |> CaseFile.addGap
                            (ValueSome pending.Key)
                            GapReach.ThisEntity
                            GapKind.NotPublished
                            "filings"
                            reason
                            "A person reading the register"
                | NeedsEntitlement entitlement ->
                    return
                        case
                        |> CaseFile.addGap
                            (ValueSome pending.Key)
                            GapReach.ThisEntity
                            GapKind.NeedsEntitlement
                            "filings"
                            entitlement.Who
                            entitlement.Route
                | Available history ->
                    let filings = history.Filings

                    let case =
                        if history.Complete then
                            case
                        else
                            let total =
                                history.Total
                                |> ValueOption.map (fun total -> $" of {total}")
                                |> ValueOption.defaultValue ""

                            case
                            |> CaseFile.addGap
                                (ValueSome pending.Key)
                                GapReach.ThisEntity
                                GapKind.NotChecked
                                "older filings"
                                $"The newest {filings.Length}{total} filings were read"
                                "A person reading the rest of the register's filing history"

                    let capital =
                        filings
                        |> List.collect (fun filing -> filing.Capital |> List.map (fun figure -> filing, figure))

                    let withEvidence, pid =
                        record
                            pending.Key
                            "filing history"
                            EvidenceOrigin.Register
                            answer
                            // The latest statement of capital, by its filing date: an older one is history, not a figure to compare.
                            (capital
                             |> List.sortBy (fun (filing, _) ->
                                 filing.FiledOn |> ValueOption.defaultValue DateOnly.MinValue)
                             |> List.tryLast
                             |> Option.map (fun (_, figure) -> Fact.Capital figure)
                             |> Option.toList)
                            ValueNone
                            case

                    let withDocuments =
                        filings
                        |> List.fold
                            (fun (state: CaseFile) filing ->
                                state
                                |> CaseFile.addDocument (fun documentId ->
                                    {
                                        Id = documentId
                                        Entity = pending.Key
                                        Register = register.Coverage.Registry
                                        DocumentId = filing.DocumentId
                                        TransactionId = filing.TransactionId
                                        Kind = Reading.documentKind filing.Category
                                        FormType = filing.FormType
                                        Description = filing.Description
                                        FiledOn = filing.FiledOn
                                        Pages = filing.Pages
                                        Link = filing.Link
                                    }))
                            withEvidence

                    return
                        capital
                        |> List.filter (fun (_, figure) ->
                            match figure.Amount with
                            | ValueSome amount -> amount <= options.NominalCapitalAtMost
                            | ValueNone -> false)
                        |> List.fold
                            (fun (state: CaseFile) (filing, figure) ->
                                let currency = figure.Currency |> ValueOption.defaultValue ""
                                let form = filing.FormType |> ValueOption.defaultValue "a filing"

                                state
                                |> CaseFile.addFlag
                                    pending.Key
                                    FlagKind.NominalCapital
                                    $"{form} states capital of {figure.Amount.Value:N2} {currency}. Nominal capital is common; this only shows the figure."
                                    answer.Source
                                    [ pid ])
                            withDocuments
        }

    let private readInsolvency (register: IFilingRegister) (pending: Pending) (case: CaseFile) ct =
        task {
            match! guarded ct (fun () -> register.InsolvencyAsync(pending.Number, ct)) with
            | Error failure ->
                let detail, wouldClose = Reading.failureGap failure

                return
                    case
                    |> CaseFile.addGap
                        (ValueSome pending.Key)
                        GapReach.WholeCase
                        GapKind.SourceFailed
                        "insolvency record"
                        detail
                        wouldClose
            | Ok answer ->
                match answer.Value with
                | NotAvailable reason ->
                    return
                        case
                        |> CaseFile.addGap
                            (ValueSome pending.Key)
                            GapReach.WholeCase
                            GapKind.NotPublished
                            "insolvency record"
                            reason
                            "A person reading the filings"
                | NeedsEntitlement entitlement ->
                    return
                        case
                        |> CaseFile.addGap
                            (ValueSome pending.Key)
                            GapReach.WholeCase
                            GapKind.NeedsEntitlement
                            "insolvency record"
                            entitlement.Who
                            entitlement.Route
                | Available proceedings ->
                    let withEvidence, pid =
                        record pending.Key "insolvency record" EvidenceOrigin.Register answer [] ValueNone case

                    return
                        proceedings
                        |> List.fold
                            (fun (state: CaseFile) proceeding ->
                                let kind = proceeding.Kind |> ValueOption.defaultValue "a case of a kind not stated"

                                state
                                |> CaseFile.addFlag
                                    pending.Key
                                    FlagKind.Insolvency
                                    $"The register records an insolvency case: {kind}"
                                    answer.Source
                                    [ pid ])
                            withEvidence
        }

    /// A UK establishment or overseas parent on the same register, to follow.
    let private readEstablishments (register: IFilingRegister) (pending: Pending) (case: CaseFile) ct =
        task {
            match! guarded ct (fun () -> register.EstablishmentsAsync(pending.Number, ct)) with
            | Error failure ->
                let detail, wouldClose = Reading.failureGap failure

                return
                    case
                    |> CaseFile.addGap
                        (ValueSome pending.Key)
                        GapReach.WholeCase
                        GapKind.SourceFailed
                        "UK establishments"
                        detail
                        wouldClose,
                    []
            | Ok answer ->
                match answer.Value with
                | NotAvailable reason ->
                    return
                        case
                        |> CaseFile.addGap
                            (ValueSome pending.Key)
                            GapReach.ThisEntity
                            GapKind.NotPublished
                            "UK establishments"
                            reason
                            "A person reading the register",
                        []
                | NeedsEntitlement entitlement ->
                    return
                        case
                        |> CaseFile.addGap
                            (ValueSome pending.Key)
                            GapReach.ThisEntity
                            GapKind.NeedsEntitlement
                            "UK establishments"
                            entitlement.Who
                            entitlement.Route,
                        []
                | Available establishments ->
                    let withEvidence, pid =
                        record pending.Key "UK establishments" EvidenceOrigin.Register answer [] ValueNone case

                    return
                        establishments
                        |> List.fold
                            (fun ((state: CaseFile), more) establishment ->
                                let identity =
                                    EntityIdentity.OnRegister(
                                        register.Coverage.Registry,
                                        pending.Jurisdiction,
                                        establishment.Number
                                    )

                                let target =
                                    CaseFile.entityOf
                                        identity
                                        establishment.Name
                                        PartyKind.LegalEntity
                                        (ValueSome pending.Jurisdiction)

                                state
                                |> CaseFile.addEntity target
                                |> CaseFile.addLink
                                    pending.Key
                                    target.Key
                                    LinkKind.Establishment
                                    (LinkProvenance.Register answer.Source)
                                    [ pid ]
                                    ValueNone,
                                more
                                @ [
                                    {
                                        Key = target.Key
                                        Number = establishment.Number
                                        Jurisdiction = pending.Jurisdiction
                                        Depth = pending.Depth + 1
                                        Via = $"UK establishment {establishment.Name}"
                                    }
                                ])
                            (withEvidence, [])
        }

    let private leiFacts (record: LeiRecord) =
        [
            Fact.Name record.LegalName

            for name in record.OtherNames do
                Fact.OtherName("other name reported to GLEIF", name)

            match record.RegisteredAs with
            | ValueSome number -> Fact.RegistrationNumber number
            | ValueNone -> ()

            match record.EntityStatus with
            | ValueSome "ACTIVE" -> Fact.Status("ACTIVE", StatusReading.Live)
            | ValueSome "INACTIVE" -> Fact.Status("INACTIVE", StatusReading.Ended)
            | ValueSome word -> Fact.Status(word, StatusReading.NotPlaced)
            | ValueNone -> ()

            // The LEI's own status isn't the company's: a lapsed LEI says its data has gone stale.
            match record.LeiStatus with
            | ValueSome word -> Fact.Other("LEI status", word)
            | ValueNone -> ()

            match record.LastUpdatedOn with
            | ValueSome on ->
                Fact.Other("LEI last updated", on.ToString("yyyy-MM-dd", Globalization.CultureInfo.InvariantCulture))
            | ValueNone -> ()

            match record.LegalAddress with
            | ValueSome address ->
                Fact.Address(AddressKind.RegisteredOffice, Reading.addressText address, address.PostalCode)
            | ValueNone -> ()

            match record.HeadquartersAddress with
            | ValueSome address ->
                Fact.Address(AddressKind.PrincipalOffice, Reading.addressText address, address.PostalCode)
            | ValueNone -> ()
        ]

    /// The LEI record for a company, if it has one, with its parents: a parent is linked and followed, and a reporting exception shown as a gap.
    /// GLEIF is asked with the number as asked, then, when nothing reports it, as the register wrote it (alsoAs). An answer holding no record is
    /// kept as evidence.
    let private readCrossCheck (check: ILeiCrossCheck) (pending: Pending) (alsoAs: string voption) (case: CaseFile) ct =
        let about = ValueSome pending.Key
        let subject = "LEI cross-check"

        let nothingFound (answer: Evidence<LeiMatches>) =
            answer.Value.Matching.IsEmpty
            && answer.Value.Possible.IsEmpty
            && answer.Value.Unread = 0

        let ask () =
            task {
                match! guarded ct (fun () -> check.ByRegistrationAsync(pending.Number, pending.Jurisdiction, ct)) with
                | Ok first when nothingFound first ->
                    match alsoAs with
                    | ValueSome written ->
                        match! guarded ct (fun () -> check.ByRegistrationAsync(written, pending.Jurisdiction, ct)) with
                        | Ok second when not (nothingFound second) -> return Ok second
                        | _ -> return Ok first
                    | ValueNone -> return Ok first
                | other -> return other
            }

        task {
            match! ask () with
            | Error failure ->
                let detail, wouldClose = Reading.failureGap failure

                return
                    case
                    |> CaseFile.addGap about GapReach.ThisEntity GapKind.SourceFailed subject detail wouldClose,
                    []
            | Ok answer ->
                let case =
                    answer.Value.Possible
                    |> List.fold
                        (fun (state: CaseFile) possible ->
                            let reportedIn = possible.Jurisdiction |> ValueOption.defaultValue "no jurisdiction"

                            state
                            |> CaseFile.addGap
                                about
                                GapReach.ThisEntity
                                GapKind.NotChecked
                                subject
                                $"Possible LEI match, not attached: {possible.Lei}, {possible.LegalName}, reports this number in {reportedIn}, and {pending.Jurisdiction.Code} was asked"
                                "A person compares them, then reads it by LEI")
                        case

                let case =
                    if answer.Value.Unread > 0 then
                        case
                        |> CaseFile.addGap
                            about
                            GapReach.ThisEntity
                            GapKind.NotChecked
                            subject
                            $"GLEIF states {answer.Value.Unread} more LEI records reporting this number than were read, so one in {pending.Jurisdiction.Code} can be among them and none was attached"
                            "A person searches GLEIF by name, then reads the right LEI"
                    else
                        case

                match answer.Value.Matching with
                | [] when nothingFound answer ->
                    let withEvidence, _ =
                        record
                            pending.Key
                            "LEI record"
                            EvidenceOrigin.CrossCheck
                            answer
                            []
                            (ValueSome "GLEIF holds no LEI record reporting this number in this jurisdiction")
                            case

                    return withEvidence, []
                | [] -> return case, []
                | [ _ ] when answer.Value.Unread > 0 -> return case, []
                | _ :: _ :: _ ->
                    return
                        case
                        |> CaseFile.addGap
                            about
                            GapReach.ThisEntity
                            GapKind.NotChecked
                            subject
                            "More than one LEI record reports this number, so none was attached"
                            "A person picks the right LEI, then read it by LEI",
                        []
                | [ lei ] ->
                    let withIdentity =
                        case
                        |> CaseFile.addEntity
                            { (CaseFile.entityOf
                                  (EntityIdentity.Lei lei.Lei)
                                  lei.LegalName
                                  PartyKind.LegalEntity
                                  ValueNone) with
                                Key = pending.Key
                            }

                    let withEvidence, pid =
                        record
                            pending.Key
                            "LEI record"
                            EvidenceOrigin.CrossCheck
                            answer
                            (leiFacts lei)
                            (ValueSome "Reported to GLEIF, not a register's record")
                            withIdentity

                    let readLevel (state: CaseFile, more) level =
                        task {
                            let word =
                                match level with
                                | LeiParentLevel.Direct -> "direct"
                                | LeiParentLevel.Ultimate -> "ultimate"

                            match! guarded ct (fun () -> check.ParentAsync(lei.Lei, level, ct)) with
                            | Error failure ->
                                let detail, wouldClose = Reading.failureGap failure

                                return
                                    state
                                    |> CaseFile.addGap
                                        about
                                        GapReach.WholeCase
                                        GapKind.SourceFailed
                                        $"{word} parent"
                                        detail
                                        wouldClose,
                                    more
                            | Ok { Value = LeiParent.NothingReported } ->
                                return
                                    state
                                    |> CaseFile.addGap
                                        about
                                        GapReach.ThisEntity
                                        GapKind.NotPublished
                                        $"{word} parent"
                                        "GLEIF holds neither a parent nor a reporting exception"
                                        "A vendor's report, or the company's accounts",
                                    more
                            | Ok {
                                     Value = LeiParent.ReportingException reason
                                 } ->
                                return
                                    state
                                    |> CaseFile.addGap
                                        about
                                        GapReach.ThisEntity
                                        GapKind.NotPublished
                                        $"{word} parent"
                                        $"The firm reported an exception to GLEIF in place of a parent: {reason}"
                                        "The company's accounts, or asking the company",
                                    more
                            | Ok({ Value = LeiParent.Parent parent } as parentAnswer) ->
                                let parentJurisdiction =
                                    parent.Jurisdiction |> ValueOption.bind Jurisdiction.tryParse

                                let identities =
                                    [
                                        match parent.RegisteredAs, parentJurisdiction with
                                        | ValueSome number, ValueSome jurisdiction ->
                                            EntityIdentity.OnRegister(
                                                parent.RegisteredAt |> ValueOption.defaultValue jurisdiction.Code,
                                                jurisdiction,
                                                number
                                            )
                                        | _ -> ()

                                        EntityIdentity.Lei parent.Lei
                                    ]

                                let target =
                                    { CaseFile.entityOf
                                          identities.Head
                                          parent.LegalName
                                          PartyKind.LegalEntity
                                          parentJurisdiction with
                                        Identities = identities
                                    }

                                let linked =
                                    state
                                    |> CaseFile.addEntity target
                                    |> CaseFile.addLink
                                        pending.Key
                                        target.Key
                                        LinkKind.Parent
                                        (LinkProvenance.CrossCheck parentAnswer.Source)
                                        [ pid ]
                                        (ValueSome $"{word} parent, as reported to GLEIF")

                                let withFacts, _ =
                                    record
                                        target.Key
                                        "LEI record"
                                        EvidenceOrigin.CrossCheck
                                        parentAnswer
                                        (leiFacts parent)
                                        (ValueSome "Reported to GLEIF, not a register's record")
                                        linked

                                return
                                    withFacts,
                                    more
                                    @ (match parent.RegisteredAs, parentJurisdiction with
                                       | ValueSome number, ValueSome jurisdiction ->
                                           [
                                               {
                                                   Key = target.Key
                                                   Number = number
                                                   Jurisdiction = jurisdiction
                                                   Depth = pending.Depth + 1
                                                   Via = $"{word} parent {parent.LegalName}"
                                               }
                                           ]
                                       | _ -> [])
                        }

                    let! direct = readLevel (withEvidence, []) LeiParentLevel.Direct
                    return! readLevel direct LeiParentLevel.Ultimate
        }

    /// Registries covering a jurisdiction, in the order to read them: those whose coverage names the exact code first, then those covering its
    /// country, each in the order the sources give.
    let private coveringRegistries (registries: ICompanyRegistry list) (jurisdiction: Jurisdiction) =
        let exact, wider =
            registries
            |> List.filter (fun registry -> RegistryCoverage.covers registry.Coverage jurisdiction)
            |> List.partition (fun registry -> List.contains jurisdiction registry.Coverage.Jurisdictions)

        exact @ wider

    type private FindResult = Result<Evidence<Availability<Company voption>>, RegistryFailure>

    /// Each registry's answer for a number, in order.
    let rec private askEach (registries: ICompanyRegistry list) (pending: Pending) (ct: CancellationToken) =
        task {
            match registries with
            | [] -> return []
            | registry :: rest ->
                let! result =
                    guarded ct (fun () -> registry.TryFindAsync(pending.Number, pending.Jurisdiction, ct))

                let! others = askEach rest pending ct
                return (registry, result) :: others
        }

    let private today options =
        DateOnly.FromDateTime((options.Now(): DateTimeOffset).UtcDateTime)

    /// A gap for an answer whose data is older than today: what changed since isn't seen.
    /// True for an answer read from a downloaded file that states no date of its own: its RetrievedAt is when the file was downloaded, which
    /// says nothing of when its data was current.
    let private undatedFile (registry: ICompanyRegistry) (answer: Evidence<'T>) =
        answer.AsOf.IsNone && (registry :? IBulkContents || registry :? IndexedRegistry)

    let private dataDateGap options (registry: ICompanyRegistry) about (answer: Evidence<'T>) (case: CaseFile) =
        let asOf = Reading.dataDate answer.AsOf answer.RetrievedAt

        if undatedFile registry answer then
            case
            |> CaseFile.addGap
                about
                GapReach.ThisEntity
                GapKind.NotChecked
                "data date"
                $"{answer.Source}: the file states no date of its own, and was downloaded on {Reading.dateText asOf}; changes since the publisher made it aren't seen"
                "A later copy of the source, the register itself, or a vendor's report"
        elif asOf < today options then
            case
            |> CaseFile.addGap
                about
                GapReach.ThisEntity
                GapKind.NotChecked
                "data date"
                $"{answer.Source}: as of {Reading.dateText asOf}; later changes aren't seen"
                "A later copy of the source, the register itself, or a vendor's report"
        else
            case

    /// A register saying it holds no such number, as a flag dated by its data. Data older than today may predate the company, which is a gap.
    let private notHeld
        options
        (registry: ICompanyRegistry)
        (pending: Pending)
        (answer: Evidence<'T>)
        (case: CaseFile)
        =
        let asOf = Reading.dataDate answer.AsOf answer.RetrievedAt
        let undated = undatedFile registry answer

        let dated =
            if undated then
                $"in its file downloaded on {Reading.dateText asOf}, which states no date of its own"
            else
                $"in its data as of {Reading.dateText asOf}"

        let withEvidence, pid =
            record
                pending.Key
                "company record"
                EvidenceOrigin.Register
                answer
                []
                (ValueSome "The register says it holds no such number")
                case

        let flagged =
            withEvidence
            |> CaseFile.addFlag
                pending.Key
                FlagKind.NotOnRegister
                $"{answer.Source} says it holds no number {pending.Number}, {dated}"
                answer.Source
                [ pid ]

        if undated || asOf < today options then
            flagged
            |> CaseFile.addGap
                (ValueSome pending.Key)
                GapReach.WholeCase
                GapKind.NotChecked
                "data date"
                (if undated then
                     $"{answer.Source}'s file states no date of its own, so a company formed since it was made may not be in it"
                 else
                     $"{answer.Source}'s data is as of {Reading.dateText asOf}, so a company formed since isn't in it")
                "A later copy of the source, the register itself, or a vendor's report"
        else
            flagged

    /// A record no registry here could read: the declared route, or no route at all.
    let private unreadGap (sources: CaseSources) (pending: Pending) (refused: ICompanyRegistry list) (case: CaseFile) =
        let about = ValueSome pending.Key

        match CoverageRoutes.forJurisdiction sources.Routes pending.Jurisdiction with
        | ValueSome route ->
            case
            |> CaseFile.addGap
                about
                GapReach.WholeCase
                (routeGapKind route)
                $"{route.Register}: {pending.Via}"
                $"Number {pending.Number} wasn't read. {route.WhyNoAdapter}"
                route.Route
        | ValueNone ->
            let refusedBy =
                match refused with
                | [] -> ""
                | registries ->
                    let names = registries |> List.map _.Coverage.Registry |> String.concat ", "
                    $" ({names} answered that it doesn't cover it)"

            case
            |> CaseFile.addGap
                about
                GapReach.WholeCase
                GapKind.NoRoute
                $"The register in {pending.Jurisdiction.Code}: {pending.Via}"
                $"Number {pending.Number} wasn't read: no source here covers {pending.Jurisdiction.Code}{refusedBy}"
                "A person reading that register, a vendor's report, or an adapter for it"

    /// The LEI cross-check, or a gap saying it wasn't among the sources.
    let private crossCheck (sources: CaseSources) (pending: Pending) (alsoAs: string voption) (case: CaseFile) ct =
        match sources.CrossCheck with
        | ValueSome check -> readCrossCheck check pending alsoAs case ct
        | ValueNone when pending.Depth = 0 ->
            Task.FromResult(
                case
                |> CaseFile.addGap
                    (ValueSome pending.Key)
                    GapReach.ThisEntity
                    GapKind.NotChecked
                    "LEI cross-check"
                    "GLEIF wasn't among the sources"
                    "Add GLEIF to the sources",
                []
            )
        | ValueNone -> Task.FromResult(case, [])

    /// One registry's answer when no registry here holds the company: a failure, an answer that can't tell, or not held.
    let private unsettled options (pending: Pending) (case: CaseFile) (registry: ICompanyRegistry, result: FindResult) =
        let about = ValueSome pending.Key

        match result with
        | Error(RegistryFailure.OutsideCoverage _) -> case
        | Error failure ->
            let detail, wouldClose = Reading.failureGap failure

            case
            |> CaseFile.addGap
                about
                GapReach.WholeCase
                GapKind.SourceFailed
                $"company record: {pending.Via}"
                $"{registry.Coverage.Registry}: {detail}"
                wouldClose
        | Ok({ Evidence.Value = Available ValueNone } as answer) -> notHeld options registry pending answer case
        | Ok({
                 Evidence.Value = Available(ValueSome _)
             }) -> case
        | Ok answer ->
            let kept, _ =
                record
                    pending.Key
                    "company record"
                    EvidenceOrigin.Register
                    answer
                    []
                    (ValueSome "The source's answer can't settle this")
                    case

            unavailableGap
                about
                GapReach.WholeCase
                $"company record: {pending.Via}"
                registry.Coverage.Standing
                answer.Value
                kept

    /// What a register other than the one followed says of a company already read: evidence to compare, its standing and stated concerns as
    /// flags, and its own failure as a gap on this entity.
    let private another options (pending: Pending) (case: CaseFile) (registry: ICompanyRegistry, result: FindResult) =
        let about = ValueSome pending.Key

        match result with
        | Error(RegistryFailure.OutsideCoverage _) -> case
        | Error failure ->
            let detail, wouldClose = Reading.failureGap failure

            case
            |> CaseFile.addGap
                about
                GapReach.ThisEntity
                GapKind.SourceFailed
                $"record on {registry.Coverage.Registry}"
                detail
                wouldClose
        | Ok({ Evidence.Value = Available ValueNone } as answer) -> notHeld options registry pending answer case
        | Ok({
                 Evidence.Value = Available(ValueSome company)
             } as answer) ->
            // Both registers hold the number asked, so a number of this one's own scheme, such as an ABN for an ACN, is kept and not compared.
            let facts =
                Reading.companyFacts company
                |> List.map (function
                    | Fact.RegistrationNumber number -> Fact.Other("number on this register", number)
                    | fact -> fact)

            let withEvidence, id =
                record pending.Key "company record" EvidenceOrigin.Register answer facts ValueNone case

            let withStanding =
                match Reading.standingFlag company with
                | ValueSome(kind, statement) ->
                    withEvidence |> CaseFile.addFlag pending.Key kind statement answer.Source [ id ]
                | ValueNone -> withEvidence

            company.Concerns.Stated
            |> List.fold
                (fun (state: CaseFile) concern ->
                    let kind, statement = Reading.concernFlag concern
                    state |> CaseFile.addFlag pending.Key kind statement answer.Source [ id ])
                withStanding
            |> dataDateGap options registry about answer
        | Ok answer ->
            fst (
                record
                    pending.Key
                    "company record"
                    EvidenceOrigin.Register
                    answer
                    []
                    (ValueSome "The source's answer can't settle this")
                    case
            )

    /// A gap per part of the register's coverage that is only partial, with its caveat.
    let private partialGaps about (coverage: RegistryCoverage) (company: Company) (case: CaseFile) =
        [
            "standing", coverage.Standing
            // The concerns the answer leaves unconfirmed are already a gap that names them.
            if company.Concerns.Unconfirmed.IsEmpty then
                "filings and disputes", coverage.Concerns
            match company.Addresses with
            | Available _ -> "addresses", coverage.Addresses
            | NotAvailable _
            | NeedsEntitlement _ -> ()
        ]
        |> List.fold
            (fun (state: CaseFile) (subject, support) ->
                match support with
                | Support.Partial caveat ->
                    state
                    |> CaseFile.addGap
                        about
                        GapReach.ThisEntity
                        GapKind.NotPublished
                        subject
                        $"{coverage.Registry}: {caveat}"
                        "A vendor's report, or a person reading the register"
                | Support.Supported
                | Support.NotOffered _ -> state)
            case

    /// Reads one company's record and everything hanging off it. Every registry covering the jurisdiction is asked, the exact code's first; the first
    /// that holds the company gives the record followed, and the others' answers are kept as evidence beside it. GLEIF is read whatever the registers
    /// said. Answers the case and what to read next.
    let private readCompany (sources: CaseSources) options (pending: Pending) (case: CaseFile) (ct: CancellationToken) =
        let about = ValueSome pending.Key

        task {
            let registries = coveringRegistries sources.Registries pending.Jurisdiction
            let! answers = askEach registries pending ct

            let answered =
                answers
                |> List.filter (fun (_, result) ->
                    match result with
                    | Error(RegistryFailure.OutsideCoverage _) -> false
                    | _ -> true)

            let found =
                answers
                |> List.tryPick (fun (registry, result) ->
                    match result with
                    | Ok({
                             Evidence.Value = Available(ValueSome company)
                         } as answer) -> Some(registry, answer, company)
                    | _ -> None)

            match answered, found with
            | [], _ ->
                let unread = unreadGap sources pending registries case
                return! crossCheck sources pending ValueNone unread ct
            | _, None ->
                let unread = answered |> List.fold (unsettled options pending) case
                return! crossCheck sources pending ValueNone unread ct
            | _, Some(registry, answer, company) ->
                let registerName = answer.Source

                // The name held until now, which the register's answer replaces on the entity, stays in the case to be screened.
                let heldBefore =
                    case.Entities
                    |> List.tryFind (fun entity -> entity.Key = pending.Key)
                    |> Option.map _.Name
                    |> Option.filter (fun name ->
                        not (PartyNames.isPlaceholder name)
                        && NameMatching.normalise name <> NameMatching.normalise company.Name)
                    |> Option.map (fun name -> Fact.OtherName("name held before the register answered", name))
                    |> Option.toList

                let identity =
                    EntityIdentity.OnRegister(registerName, pending.Jurisdiction, company.Number)

                let withEntity =
                    case
                    |> CaseFile.addEntity
                        { (CaseFile.entityOf
                              identity
                              company.Name
                              PartyKind.LegalEntity
                              (ValueSome pending.Jurisdiction)) with
                            Key = pending.Key
                            Role = company.Role
                            Standing = ValueSome company.Standing
                            StatusAsPublished = company.StatusAsPublished
                        }

                // The entity's name is the one first seen, so a name a person typed gives way to the register's on the entity the read was for.
                let withName =
                    { withEntity with
                        Entities =
                            withEntity.Entities
                            |> List.map (fun entity ->
                                if entity.Key = pending.Key then
                                    { entity with
                                        Name = company.Name
                                        Role = company.Role
                                    }
                                else
                                    entity)
                    }

                // A number of another scheme than the one asked, such as an ABN for an ACN, is kept and not compared: GLEIF and stated numbers
                // are of the scheme asked.
                let facts =
                    if Reading.sameNumber company.Number pending.Number then
                        Reading.companyFacts company
                    else
                        Reading.companyFacts company
                        |> List.map (function
                            | Fact.RegistrationNumber number -> Fact.Other("number on this register", number)
                            | fact -> fact)

                let withEvidence, id =
                    record
                        pending.Key
                        "company record"
                        EvidenceOrigin.Register
                        answer
                        (facts @ heldBefore)
                        ValueNone
                        withName

                let withStanding =
                    match Reading.standingFlag company with
                    | ValueSome(kind, statement) ->
                        withEvidence |> CaseFile.addFlag pending.Key kind statement answer.Source [ id ]
                    | ValueNone -> withEvidence

                let withUnconfirmedStanding =
                    match Reading.standingUnconfirmed company with
                    | ValueSome detail ->
                        withStanding
                        |> CaseFile.addGap
                            about
                            GapReach.ThisEntity
                            GapKind.NotPublished
                            "standing"
                            $"{answer.Source}: {detail}"
                            "A vendor's report, or a person reading the register"
                    | ValueNone -> withStanding

                let withConcerns =
                    company.Concerns.Stated
                    |> List.fold
                        (fun (state: CaseFile) concern ->
                            let kind, statement = Reading.concernFlag concern
                            state |> CaseFile.addFlag pending.Key kind statement answer.Source [ id ])
                        withUnconfirmedStanding

                let withUnconfirmed =
                    match company.Concerns.Unconfirmed with
                    | [] -> withConcerns
                    // An establishment's flags are its overseas company's, which the case follows: read there, or a gap past the limits.
                    | _ when company.Role = RecordRole.Establishment && company.EstablishmentOf.IsSome -> withConcerns
                    | unconfirmed ->
                        let kind =
                            match registry.Coverage.Concerns with
                            | Support.NotOffered _ -> GapKind.NotAvailableForCountry
                            | Support.Supported
                            | Support.Partial _ -> GapKind.NotChecked

                        let words = unconfirmed |> List.map Reading.concernWord |> String.concat ", "

                        withConcerns
                        |> CaseFile.addGap
                            about
                            GapReach.ThisEntity
                            kind
                            "filings and disputes"
                            $"{answer.Source} doesn't settle: {words}"
                            "A vendor's report, or a person reading the register's filings"

                let withAddressGap =
                    match company.Addresses with
                    | Available _ -> withUnconfirmed
                    | other ->
                        unavailableGap
                            about
                            GapReach.ThisEntity
                            "addresses"
                            registry.Coverage.Addresses
                            other
                            withUnconfirmed

                let withOthers =
                    withAddressGap
                    |> partialGaps about registry.Coverage company
                    |> dataDateGap options registry about answer
                    |> fun state ->
                        answered
                        |> List.filter (fun (other, _) -> not (obj.ReferenceEquals(other, registry)))
                        |> List.fold (another options pending) state

                // A record that isn't the home record points at the home record, or the missing pointer is a gap.
                let withHome, homeNext =
                    match company.Role, company.HomeRegistration with
                    | _, ValueSome home ->
                        linkRegistration
                            sources
                            pending.Jurisdiction
                            pending.Key
                            LinkKind.HomeRecord
                            answer.Source
                            company.Name
                            home
                            id
                            answer.RetrievedAt
                            (pending.Depth + 1)
                            ValueNone
                            withOthers
                    | RecordRole.ForeignRegistration, ValueNone ->
                        withOthers
                        |> CaseFile.addGap
                            about
                            GapReach.WholeCase
                            GapKind.NotPublished
                            "home record"
                            $"{answer.Source} treats this as a registration of a company formed elsewhere, and doesn't say where"
                            "A person finds the home registration and adds it as a link",
                        []
                    | RecordRole.Establishment, ValueNone when company.EstablishmentOf.IsNone ->
                        withOthers
                        |> CaseFile.addGap
                            about
                            GapReach.WholeCase
                            GapKind.NotPublished
                            "home record"
                            $"{answer.Source} treats this as an establishment of a company, and doesn't say which"
                            "A person finds the company it belongs to and adds it as a link",
                        []
                    | _ -> withOthers, []

                let withParent, parentNext =
                    match company.EstablishmentOf with
                    | ValueSome parentNumber ->
                        let parentIdentity =
                            EntityIdentity.OnRegister(registerName, pending.Jurisdiction, parentNumber)

                        let parent =
                            CaseFile.entityOf
                                parentIdentity
                                (PartyNames.overseasCompany parentNumber)
                                PartyKind.LegalEntity
                                (ValueSome pending.Jurisdiction)

                        withHome
                        |> CaseFile.addEntity parent
                        |> CaseFile.addLink
                            parent.Key
                            pending.Key
                            LinkKind.Establishment
                            (LinkProvenance.Register answer.Source)
                            [ id ]
                            ValueNone,
                        [
                            {
                                Key = parent.Key
                                Number = parentNumber
                                Jurisdiction = pending.Jurisdiction
                                Depth = pending.Depth + 1
                                Via = $"overseas company of {company.Name}"
                            }
                        ]
                    | ValueNone -> withHome, []

                let! withOfficers = readOfficers registry options pending withParent ct

                let! withOwners, ownerNext =
                    readOwners sources registry options pending withOfficers ct

                let filingRegister = filingRegisterFor sources pending.Jurisdiction

                let! withFilings =
                    match filingRegister with
                    | Some register -> readFilings register options pending withOwners ct
                    | None ->
                        Task.FromResult(
                            withOwners
                            |> CaseFile.addGap
                                about
                                GapReach.ThisEntity
                                GapKind.NotAvailableForCountry
                                "filings"
                                $"No source here reads filings for {pending.Jurisdiction.Code}"
                                "A person reading the register's filings, or a vendor's report"
                        )

                let! withInsolvency =
                    match filingRegister with
                    | Some register when
                        List.contains CompanyConcern.InsolvencyHistory company.Concerns.Stated
                        || Reading.statusReading company.Standing = StatusReading.InProceeding
                        ->
                        readInsolvency register pending withFilings ct
                    | _ -> Task.FromResult withFilings

                let! withEstablishments, establishmentNext =
                    match filingRegister, company.Role with
                    | Some register, RecordRole.ForeignRegistration ->
                        readEstablishments register pending withInsolvency ct
                    | _ -> Task.FromResult(withInsolvency, [])

                let! withCrossCheck, crossCheckNext =
                    // The register's own writing of the same number, such as CHE-107.721.785 for CHE107721785.
                    let alsoAs =
                        if
                            company.Number <> pending.Number
                            && Reading.sameNumber company.Number pending.Number
                        then
                            ValueSome company.Number
                        else
                            ValueNone

                    crossCheck sources pending alsoAs withEstablishments ct

                return withCrossCheck, homeNext @ parentNext @ ownerNext @ establishmentNext @ crossCheckNext
        }

    /// Reads what is pending in the order found, never one key twice, so a loop ends. Past MaxDepth or MaxEntities, what is left is a gap on the whole case.
    let rec private visit
        sources
        options
        (case: CaseFile)
        (read: Set<string>)
        (queue: Pending list)
        ct
        : Task<CaseFile> =
        task {
            match queue with
            | [] -> return case
            | next :: rest when read.Contains next.Key -> return! visit sources options case read rest ct
            | next :: rest when next.Depth > options.MaxDepth ->
                let stopped =
                    case
                    |> CaseFile.addGap
                        (ValueSome next.Key)
                        GapReach.WholeCase
                        GapKind.NotChecked
                        $"company record: {next.Via}"
                        $"Not read: past the depth limit of {options.MaxDepth}"
                        "Follow from this entity with Investigation.followAsync"

                return! visit sources options stopped read rest ct
            | next :: rest when read.Count >= options.MaxEntities ->
                let stopped =
                    case
                    |> CaseFile.addGap
                        (ValueSome next.Key)
                        GapReach.WholeCase
                        GapKind.NotChecked
                        $"company record: {next.Via}"
                        $"Not read: past the limit of {options.MaxEntities} records in a run"
                        "Follow from this entity with Investigation.followAsync"

                return! visit sources options stopped read rest ct
            | next :: rest ->
                let! updated, more = readCompany sources options next case ct
                return! visit sources options updated (read.Add next.Key) (rest @ more) ct
        }

    /// Opens a case on one company and follows its links. A source that fails leaves a gap, and the case comes back with what the others gave.
    let openAsync (sources: CaseSources) (options: InvestigationOptions) (start: CaseStart) (ct: CancellationToken) =
        task {
            let identity =
                EntityIdentity.OnRegister(
                    (Registries.forJurisdiction sources.Registries start.Jurisdiction
                     |> ValueOption.map _.Coverage.Registry
                     |> ValueOption.defaultValue $"the register in {start.Jurisdiction.Code}"),
                    start.Jurisdiction,
                    start.Number
                )

            let case =
                CaseFile.openCase start.CaseId identity start.NameGiven (ValueSome start.Jurisdiction) (options.Now())

            let! read =
                visit
                    sources
                    options
                    case
                    Set.empty
                    [
                        {
                            Key = case.Subject
                            Number = start.Number
                            Jurisdiction = start.Jurisdiction
                            Depth = 0
                            Via = "the company the case is about"
                        }
                    ]
                    ct

            return CaseFile.checkKnownAddresses sources.KnownAddresses read
        }

    /// Reads a further record into a case and follows from it, such as an entity an officer declared. Fails only if the case doesn't hold the entity. Already
    /// read entities are read again.
    let followAsync
        (sources: CaseSources)
        (options: InvestigationOptions)
        (key: string)
        (number: string)
        (jurisdiction: Jurisdiction)
        (case: CaseFile)
        (ct: CancellationToken)
        =
        task {
            if not (case.Entities |> List.exists (fun entity -> entity.Key = key)) then
                return Error(CaseEditRefused.UnknownEntity key)
            else
                let! read =
                    visit
                        sources
                        options
                        case
                        Set.empty
                        [
                            {
                                Key = key
                                Number = number
                                Jurisdiction = jurisdiction
                                Depth = 0
                                Via = "an entity followed by hand"
                            }
                        ]
                        ct

                return Ok(CaseFile.checkKnownAddresses sources.KnownAddresses read)
        }

    /// Reads a document through its register and reader, into evidence. No reader, or a read that fails, leaves the not-read gap open and adds why.
    let readDocumentAsync
        (register: IFilingRegister)
        (reader: IDocumentReader)
        (documentId: string)
        (options: InvestigationOptions)
        (case: CaseFile)
        (ct: CancellationToken)
        =
        task {
            match case.Documents |> List.tryFind (fun document -> document.Id = documentId) with
            | None -> return Error(CaseEditRefused.UnknownDocument documentId)
            | Some document ->
                let failed detail wouldClose =
                    case
                    |> CaseFile.addGap
                        (ValueSome document.Entity)
                        GapReach.ThisEntity
                        GapKind.SourceFailed
                        "filing to read"
                        $"Document {document.Id}: {detail}"
                        wouldClose

                match document.DocumentId with
                | ValueNone ->
                    return Ok(failed "the register holds no image of it" "A person asking the register for a copy")
                | ValueSome registerId ->
                    match! guarded ct (fun () -> register.DocumentAsync(registerId, ct)) with
                    | Error failure ->
                        let detail, wouldClose = Reading.failureGap failure
                        return Ok(failed detail wouldClose)
                    | Ok ValueNone ->
                        return
                            Ok(
                                failed
                                    "the register answered that it holds no such document"
                                    "A person asking the register for a copy"
                            )
                    | Ok(ValueSome file) ->
                        let! answer =
                            task {
                                try
                                    return! reader.ReadAsync(document, file, ct)
                                with error when
                                    not (ct.IsCancellationRequested && (error :? OperationCanceledException)) ->
                                    return Error $"The reader raised {error.GetType().Name}"
                            }

                        match answer with
                        | Error reason ->
                            return
                                Ok(
                                    failed
                                        $"{reader.Name} couldn't read it: {reason}"
                                        "An officer reads it and attaches what it says"
                                )
                        | Ok(text, facts) ->
                            return
                                CaseFile.attachReading
                                    documentId
                                    facts
                                    text
                                    {
                                        By = reader.Name
                                        At = options.Now()
                                        Source = $"{document.Register} document {registerId}"
                                    }
                                    case
                                |> Result.map fst
        }

    /// Every name a case holds for an entity, and who it can't name. A party with no name in the answer can't be screened, nor can the owners and officers of a
    /// record that couldn't give them, and that is said beside the result.
    let private namesIn (case: CaseFile) =
        let factsAbout key =
            case.Evidence
            |> List.filter (fun item -> item.About = key)
            |> List.collect _.Facts

        let toScreen key party name =
            let facts = factsAbout key

            {
                Name = name
                EntityKey = ValueSome key
                Party = party
                BornIn =
                    facts
                    |> List.tryPick (function
                        | Fact.BornIn born -> Some born
                        | _ -> None)
                    |> ValueOption.ofOption
                Countries =
                    facts
                    |> List.choose (function
                        | Fact.Country country -> Some country
                        | _ -> None)
            }

        let names =
            [
                for entity in case.Entities do
                    for name in CaseFile.namesHeldFor case entity do
                        toScreen entity.Key entity.Party name

                for about, name in CaseFile.officersWithoutEntity case do
                    toScreen about (PartyKind.Unknown ValueNone) name
            ]

        let withheld =
            case.Entities
            |> List.filter (fun entity -> (CaseFile.namesHeldFor case entity).IsEmpty)
            |> List.map (fun entity ->
                {
                    Who = entity.Key
                    Reason = "The source withholds the name or doesn't give it"
                })

        let isUnknownParties (gap: CaseGap) =
            gap.Subject = "owners"
            || gap.Subject = "officers"
            || (gap.Kind = GapKind.SourceFailed
                && (gap.Subject = "LEI cross-check" || gap.Subject.EndsWith " parent"))

        let unknownParties =
            case.Gaps
            |> List.filter (fun gap -> gap.ClosedBy.IsNone && isUnknownParties gap)
            |> List.map (fun gap ->
                {
                    Who = $"{gap.Subject} of {gap.About |> ValueOption.defaultValue case.Subject}"
                    Reason = gap.Detail
                })

        let unreadCompanies =
            case.Gaps
            |> List.filter (fun gap -> gap.ClosedBy.IsNone && gap.Subject.StartsWith "company record")
            |> List.map (fun gap ->
                {
                    Who = gap.Subject
                    Reason = $"Only the name, if any, could be screened: {gap.Detail}"
                })

        names, withheld @ unknownParties @ unreadCompanies

    /// Screens the names in a case against the lists the host supplied, and adds the result. Every screening says which lists, their dates and who wasn't
    /// screened. A case with unknown owners stays open on that, whatever the names showed.
    let screen (lists: ListText list) (options: InvestigationOptions) (case: CaseFile) =
        let names, notScreened = namesIn case

        let screening =
            SanctionsScreening.screen
                (SanctionsScreening.load options.ListMinimums lists)
                names
                notScreened
                (options.Now())

        CaseFile.addScreening screening case
