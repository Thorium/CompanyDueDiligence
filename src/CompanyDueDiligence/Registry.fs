/// A country-neutral company register contract, which the case file builds on. Each register is one adapter behind ICompanyRegistry.
namespace CompanyDueDiligence

open System
open System.Text.RegularExpressions
open System.Threading
open System.Threading.Tasks

/// An ISO 3166 code: a country (GB, US) or a subdivision (GB-SCT, US-DE). Only the shape is checked, not that the code is assigned.
[<Struct; StructuredFormatDisplay("{Code}")>]
type Jurisdiction =
    private
    | Jurisdiction of code: string

    /// The code in upper case, such as GB or US-DE.
    member this.Code =
        let (Jurisdiction code) = this
        code

    /// The two-letter country part.
    member this.Country = this.Code.Substring(0, 2)
    override this.ToString() = this.Code

/// Parsing and comparing jurisdictions.
module Jurisdiction =

    let private tryParseRegex = Regex @"\A[A-Za-z]{2}(-[A-Za-z0-9]{1,3})?\z"

    /// The jurisdiction of a code in the form GB or GB-SCT, in any case, or ValueNone for null or any other shape.
    let tryParse (code: string) =
        if not (isNull code) && tryParseRegex.IsMatch(code) then
            ValueSome(Jurisdiction(code.ToUpperInvariant()))
        else
            ValueNone

    /// The jurisdiction of a code known when writing the program; raises ArgumentException on any other shape.
    let ofCode (code: string) =
        match tryParse code with
        | ValueSome jurisdiction -> jurisdiction
        | ValueNone -> raise (ArgumentException("Not an ISO 3166 code in the form GB or GB-SCT.", nameof code))

    /// True when a register for scope answers for asked: the same code, or a subdivision of the country scope names.
    let covers (scope: Jurisdiction) (asked: Jurisdiction) =
        scope = asked || scope.Code = asked.Country

/// What it takes to read data a register holds but doesn't give to everyone.
type Entitlement =
    {
        /// Who may read it, in the register's terms.
        Who: string
        /// How to apply, or who to contract with.
        Route: string
    }

/// Data a register may not give. Available [] means the register says there are none. NotAvailable means it doesn't publish this or its answer
/// can't settle it, and NeedsEntitlement that it holds it for those entitled. Neither says anything about the company.
type Availability<'T> =
    | Available of 'T
    | NotAvailable of reason: string
    | NeedsEntitlement of Entitlement

/// An answer as kept for the record, including "not found" and "not available", which are evidence too.
type Evidence<'T> =
    {
        /// The register that answered: its RegistryCoverage.Registry.
        Source: string
        RetrievedAt: DateTimeOffset
        /// The date the source says its data is as of, which a bulk copy can leave well behind RetrievedAt. ValueNone when it doesn't say.
        AsOf: DateOnly voption
        /// The answer as received, which can hold personal data. ValueNone for an answer with no body to keep, such as a 404.
        Raw: string voption
        Value: 'T
    }

/// Building evidence records.
module Evidence =

    /// An answer from source at retrievedAt, with no stated data date and no raw text: set those with { Evidence.create ... with ... }.
    let create (source: string) (retrievedAt: DateTimeOffset) (value: 'T) : Evidence<'T> =
        {
            Source = source
            RetrievedAt = retrievedAt
            AsOf = ValueNone
            Raw = ValueNone
            Value = value
        }

/// A company's legal standing as its register publishes it; a status an adapter can't place is Unknown. It says nothing of Company.Concerns.
[<RequireQualifiedAccess>]
type CompanyStanding =
    /// The register calls it active and its status holds nothing qualifying.
    | Active
    /// Active, with something in its status against it, such as a proposal to strike off.
    | ActiveWithConcern of concern: string
    /// On a register that doesn't describe it as active, with the word it does use, such as "open", or ValueNone for a register with no status word.
    | Registered of status: string voption
    /// In liquidation, administration or similar.
    | InInsolvency of proceeding: string
    /// Being wound up or dissolved, where the status doesn't say whether it is insolvent.
    | WindingUp of status: string
    | Dissolved
    /// Closed, removed or converted, other than by dissolution.
    | Closed of status: string
    /// A status the adapter doesn't recognise, or none.
    | Unknown of status: string voption

/// Reading a company's standing.
module CompanyStanding =

    /// True for Active only. This is legal standing alone, and only on the register that said it.
    let isActive standing =
        match standing with
        | CompanyStanding.Active -> true
        | CompanyStanding.ActiveWithConcern _
        | CompanyStanding.Registered _
        | CompanyStanding.InInsolvency _
        | CompanyStanding.WindingUp _
        | CompanyStanding.Dissolved
        | CompanyStanding.Closed _
        | CompanyStanding.Unknown _ -> false

/// Status words of registers no adapter reads yet, mapped ahead of a paid source.
module StandingWords =

    /// The Delaware Division of Corporations' words, for a host with a contract or a vendor that delivers them. Only the three listed are placed, and
    /// anything else is Unknown. Unverified: no answer from such a source has been seen.
    let delaware (word: string) =
        match (if isNull word then "" else word.Trim().ToUpperInvariant()) with
        | "GOOD STANDING" -> CompanyStanding.Active
        | "AR DELINQUENT" -> CompanyStanding.ActiveWithConcern "annual report delinquent"
        | "VOID" -> CompanyStanding.Closed "Void"
        | "" -> CompanyStanding.Unknown ValueNone
        | _ -> CompanyStanding.Unknown(ValueSome word)

/// A postal address as published. Country is the register's wording, not an ISO code.
type Address =
    {
        Lines: string list
        Locality: string voption
        Region: string voption
        PostalCode: string voption
        Country: string voption
    }

/// Which address it is. Addresses of different kinds aren't expected to match.
[<RequireQualifiedAccess>]
type AddressKind =
    | RegisteredOffice
    | Service
    /// Headquarters or principal place of business.
    | PrincipalOffice
    /// A commercial registered agent's address, usual for a US state registration.
    | RegisteredAgent
    | Postal
    /// A label of the source's own.
    | Other of string

/// An address with the kind the register gives it.
type CompanyAddress = { Kind: AddressKind; Address: Address }

/// Something a register can state against a company, apart from its legal standing.
[<RequireQualifiedAccess>]
type CompanyConcern =
    | AccountsOverdue
    /// A periodic statement of the company's details, such as an annual return, is overdue.
    | AnnualFilingOverdue
    | RegisteredOfficeDisputed
    | RegisteredOfficeUndeliverable
    /// Insolvency or liquidation, past or present.
    | InsolvencyHistory

/// The concerns as a set.
module CompanyConcern =

    /// Every concern, each once.
    let all =
        [
            CompanyConcern.AccountsOverdue
            CompanyConcern.AnnualFilingOverdue
            CompanyConcern.RegisteredOfficeDisputed
            CompanyConcern.RegisteredOfficeUndeliverable
            CompanyConcern.InsolvencyHistory
        ]

/// Each concern is in one of the lists, or the register's answer says it doesn't apply.
type CompanyConcerns =
    {
        /// What the register states against the company.
        Stated: CompanyConcern list
        /// What the register's answer doesn't settle, or the register doesn't publish. These aren't clear.
        Unconfirmed: CompanyConcern list
    }

/// Concern lists for registers to start from.
module CompanyConcerns =

    /// For a register that publishes none of them.
    let notPublished =
        {
            Stated = []
            Unconfirmed = CompanyConcern.all
        }

/// How a legal entity is identified on its own register, in the wording of the register publishing it. RegistrationNumber with Country or
/// Register is what a lookup of that entity starts from.
type EntityRegistration =
    {
        LegalName: string voption
        LegalForm: string voption
        /// The law or authority governing it.
        GoverningLaw: string voption
        /// The country or state it is registered in.
        Country: string voption
        /// The register holding it, or the place it is registered.
        Register: string voption
        RegistrationNumber: string voption
    }

/// What this record is to the company it describes. A foreign registration or establishment says how the company stands on this register, and
/// nothing about its home record.
[<RequireQualifiedAccess>]
type RecordRole =
    /// The record where the company was formed.
    | Home
    /// A registration to do business here of a company formed elsewhere.
    | ForeignRegistration
    /// A place of business here of a company formed elsewhere, on its own number, such as a UK establishment.
    | Establishment
    /// The source doesn't say.
    | NotStated

/// Words that stand where a source gave no name. They are no party's name, so they are never screened.
module internal PartyNames =

    [<Literal>]
    let NotGiven = "(name not given)"

    [<Literal>]
    let NotPublished = "(name not published)"

    [<Literal>]
    let private overseasPrefix = "The overseas company "

    let overseasCompany (number: string) = overseasPrefix + number

    let isPlaceholder (name: string) =
        String.IsNullOrWhiteSpace name
        || name = NotGiven
        || name = NotPublished
        || name.StartsWith(overseasPrefix, StringComparison.Ordinal)

/// A company as one register holds it. It says how the company stands on that register, and nothing of other registers.
type Company =
    {
        Name: string
        /// Names the register holds besides Name: former names and others. Empty for a register that publishes none, which doesn't say there are none.
        OtherNames: string list
        /// The number as the register holds it.
        Number: string
        /// Where the register holding this record sits, which for a foreign company isn't where it was formed.
        Jurisdiction: Jurisdiction
        /// The register's own wording for the governing jurisdiction.
        JurisdictionAsPublished: string voption
        Standing: CompanyStanding
        /// Red flags apart from Standing.
        Concerns: CompanyConcerns
        StatusAsPublished: string voption
        LegalForm: string voption
        IncorporatedOn: DateOnly voption
        CeasedOn: DateOnly voption
        Addresses: Availability<CompanyAddress list>
        Role: RecordRole
        /// What this record says of the company's home record, when it isn't itself the home record.
        HomeRegistration: EntityRegistration voption
        /// The number, on this register, of the overseas company this record is an establishment of.
        EstablishmentOf: string voption
    }

/// Building company records.
module Company =

    /// A company record with its name, number and register's jurisdiction, and nothing else stated: standing Unknown, concerns not published,
    /// addresses not read, role not stated. Set the rest with { Company.create ... with ... }, so that a field added later breaks no adapter.
    let create (name: string) (number: string) (jurisdiction: Jurisdiction) : Company =
        {
            Name = name
            OtherNames = []
            Number = number
            Jurisdiction = jurisdiction
            JurisdictionAsPublished = ValueNone
            Standing = CompanyStanding.Unknown ValueNone
            Concerns = CompanyConcerns.notPublished
            StatusAsPublished = ValueNone
            LegalForm = ValueNone
            IncorporatedOn = ValueNone
            CeasedOn = ValueNone
            Addresses = NotAvailable "Not read"
            Role = RecordRole.NotStated
            HomeRegistration = ValueNone
            EstablishmentOf = ValueNone
        }

/// One role a register publishes for a person whose name matched. It says the register holds the name, not that it is the person meant.
type PersonMatch =
    {
        /// The name as the register publishes it.
        Name: string
        /// The role as published, such as director, CEO, principal or beneficial owner. ValueNone when the register gives none.
        Role: string voption
        /// Whether the role is current, when the register says so. ValueNone says nothing either way.
        Current: bool voption
        /// The company's name on that register. ValueNone when the answer doesn't give it.
        CompanyName: string voption
        /// The company's number on that register, for TryFindAsync. ValueNone when the answer doesn't give it.
        CompanyNumber: string voption
        /// The company's status as published, when the answer gives it.
        CompanyStatus: string voption
        AppointedOn: DateOnly voption
        EndedOn: DateOnly voption
    }

/// The roles a person search found. Matches say the register holds the name, not that it is the person meant.
type PersonSearchResult =
    {
        Matches: PersonMatch list
        /// How many records the register says its own search found, before the name filter, in its own unit, such as companies or officers.
        /// ValueNone when it doesn't say.
        TotalMatches: int voption
        /// True only when the register says it returned every record its search found. Otherwise a role not in Matches can still be held, so an
        /// empty list never says the person holds no role.
        Complete: bool
    }

/// A search hit. It has no standing: read the company with TryFindAsync before relying on one.
type CompanyMatch =
    {
        Name: string
        Number: string
        StatusAsPublished: string voption
        Address: Address voption
    }

/// The companies a name search found. An empty list says no name is on the register only when Complete is true.
type SearchResult =
    {
        Matches: CompanyMatch list
        /// How many the register says match, which can be more than Matches holds.
        TotalMatches: int voption
        /// True only when Matches holds every match the register says there is. Otherwise a name not in Matches can still be on the register.
        Complete: bool
    }

/// How a company name searched for has to match a name on the register. NameMatch.matcher states the rule; a register matching in its own query
/// (INameMatchSearch) or a bulk source's Contains, which compares upper case only, can compare more strictly.
[<RequireQualifiedAccess>]
type NameMatch =
    /// The whole name, after NameMatch.normalise on both: case, accents and the spacing of punctuation are ignored.
    | Exact
    /// Names starting with the term.
    | StartsWith
    /// Names holding the term anywhere.
    | Contains

/// The name matching rule every adapter and bulk source keeps.
module NameMatch =

    /// A name in the form every mode compares: upper case, accents off, punctuation and spacing evened out.
    let normalise (name: string) = NameMatching.normalise name

    /// A test of published names against the name asked, after NameMatch.normalise on both: equal for Exact, starting with it for StartsWith,
    /// holding it for Contains. A part of a Greek name, asked in Greek, also matches letter by letter, as the pairs of its Latin spelling
    /// depend on the letters around them. A name asked with no letter or digit matches nothing.
    let matcher (matching: NameMatch) (asked: string) : string -> bool =
        match NameMatching.normalise asked with
        | "" -> fun _ -> false
        | normal ->
            // The part asked, letter by letter, against a published name spelled the same way.
            let greekPart (test: string -> string -> bool) : string -> bool =
                if NameMatching.holdsGreek asked then
                    let letters = NameMatching.normaliseByLetter asked

                    fun published ->
                        NameMatching.holdsGreek published
                        && test (NameMatching.normaliseByLetter published) letters
                else
                    fun _ -> false

            // An ASCII name, as most are, is compared as it is read: a bulk search tests every name in a file.
            let ascii (published: string) =
                not (isNull published) && Text.Ascii.IsValid published

            match matching with
            | NameMatch.Exact ->
                fun published ->
                    if ascii published then
                        NameMatching.asciiNormalMatches true normal published
                    else
                        NameMatching.normalise published = normal
            | NameMatch.StartsWith ->
                let part =
                    greekPart (fun published letters -> published.StartsWith(letters, StringComparison.Ordinal))

                fun published ->
                    if ascii published then
                        NameMatching.asciiNormalMatches false normal published
                    else
                        (NameMatching.normalise published).StartsWith(normal, StringComparison.Ordinal)
                        || part published
            | NameMatch.Contains ->
                let part =
                    greekPart (fun published letters -> published.Contains(letters, StringComparison.Ordinal))

                fun published ->
                    (NameMatching.normalise published).Contains(normal, StringComparison.Ordinal)
                    || part published

    /// The matches of a register's own search that match asked. The result is never Complete, and TotalMatches, which counts the register's own
    /// search, is dropped.
    let narrow (matching: NameMatch) (asked: string) (result: SearchResult) : SearchResult =
        let matches = matcher matching asked

        {
            Matches = result.Matches |> List.filter (fun found -> matches found.Name)
            TotalMatches = ValueNone
            Complete = false
        }

/// What sort of party an officer or owner is.
[<RequireQualifiedAccess>]
type PartyKind =
    | Individual
    | LegalEntity
    /// The register holds the party but withholds who it is.
    | Withheld
    | Unknown of asPublished: string voption

/// A year and month, as registers publish birth dates of people.
[<Struct>]
type YearMonth = { Year: int; Month: int }

/// What a register publishes about a person's identity verification, as published. A date it doesn't give is ValueNone, which doesn't say
/// the step hasn't happened or isn't due.
type IdentityVerification =
    {
        VerifiedOn: DateOnly voption
        /// The service provider that verified the identity.
        VerifiedBy: string voption
        /// The bodies supervising that provider.
        VerifierSupervisors: string list
        /// The first date a verification statement can be supplied.
        StatementPossibleFrom: DateOnly voption
        StatementDueOn: DateOnly voption
        StatementSuppliedOn: DateOnly voption
        StatementRemovedOn: DateOnly voption
        PreferredName: string voption
    }

/// An officer's role. Any role other than director or secretary keeps the register's wording.
[<RequireQualifiedAccess>]
type OfficerRole =
    | Director
    | Secretary
    | Other of asPublished: string

/// An appointment the register records, current or ended.
type Officer =
    {
        Name: string
        Kind: PartyKind
        Role: OfficerRole
        /// False once the register records the appointment as ended.
        Current: bool
        AppointedOn: DateOnly voption
        EndedOn: DateOnly voption
        BornIn: YearMonth voption
        Nationality: string voption
        CountryOfResidence: string voption
        Address: Address voption
        /// A legal entity's own registration, if the register gives one.
        Entity: EntityRegistration voption
        /// ValueNone when the answer holds none, which says nothing about whether the person is verified.
        IdentityVerification: IdentityVerification voption
    }

/// The kind of control an owner holds. A nature the adapter can't place is Other, with the wording in NatureOfControl.AsPublished.
[<RequireQualifiedAccess>]
type ControlKind =
    | Ownership
    | VotingRights
    | AppointsBoard
    | SignificantInfluence
    | Other

/// A percentage range, both ends as published.
[<Struct>]
type PercentBand = { From: int; To: int }

/// One nature of control as the register publishes it, with its kind and band read from the wording. AsPublished is the record.
type NatureOfControl =
    {
        Kind: ControlKind
        Band: PercentBand voption
        AsPublished: string
    }

/// A direct owner the register records, current or ended, with the control it holds.
type Owner =
    {
        /// ValueNone when the register withholds who it is.
        Name: string voption
        Kind: PartyKind
        Control: NatureOfControl list
        /// False once the register records the control as ended.
        Current: bool
        Since: DateOnly voption
        EndedOn: DateOnly voption
        BornIn: YearMonth voption
        Nationality: string voption
        CountryOfResidence: string voption
        Address: Address voption
        /// A legal entity's own registration, if the register gives one. Only this level is read: Investigation follows it.
        Entity: EntityRegistration voption
        /// ValueNone when the answer holds none, which says nothing about whether the person is verified.
        IdentityVerification: IdentityVerification voption
    }

/// Why a listing of owners doesn't show the whole of a company's direct ownership.
[<RequireQualifiedAccess>]
type OwnershipGap =
    /// An owner exists, or may exist, that the register doesn't identify.
    | OwnerNotIdentified
    /// An owner is identified but their details aren't confirmed or provided.
    | OwnerDetailsNotConfirmed
    /// The company hasn't finished finding out who its owners are.
    | InquiriesIncomplete
    /// An owner hasn't answered a formal notice, or has restrictions imposed for that.
    | OwnerNotCooperating
    /// The register holds a current owner but withholds who it is.
    | OwnerWithheld
    /// A current owner's kind or name couldn't be read.
    | OwnerUnreadable
    /// A current owner that is a legal entity has no registration number to follow it by.
    | EntityNotIdentified
    /// A statement says there is no one to list, yet a current owner is listed.
    | StatementContradictsOwners
    /// A current statement the adapter doesn't recognise.
    | StatementNotUnderstood of asPublished: string

/// What a statement on the register says about ownership.
[<RequireQualifiedAccess>]
type OwnerStatementMeaning =
    /// There is no one to list.
    | NoOwnerToList
    /// Every owner is identified and their details are given.
    | AllOwnersIdentified
    /// It reports changes over a period, and leaves who the owners are to the list.
    | ChangesOnly
    | LeavesGap of OwnershipGap

/// A statement the register holds about the company's ownership, apart from the owners listed.
type OwnerStatement =
    {
        Meaning: OwnerStatementMeaning
        /// The register's own code, for the record. Read Meaning instead of interpreting it.
        AsPublished: string
    }

/// A company's direct owners as one register lists them, with what keeps the list from being whole.
type OwnerListing =
    {
        /// Current and former owners. No current owner, inside Available, means the register states there is none.
        Owners: Owner list
        /// What the register currently states about ownership alongside the list.
        Statements: OwnerStatement list
        /// What keeps the listing from showing the whole of the direct ownership. Empty means nothing the register published does.
        Gaps: OwnershipGap list
    }

/// Reading an owner listing.
module OwnerListing =

    /// True only when nothing the register published leaves a gap. It covers direct owners: a legal entity's own owners are a further lookup.
    let isComplete (listing: OwnerListing) = List.isEmpty listing.Gaps

/// Why owners are being read. Beneficial owner registers in the EU give different access by purpose, so an adapter answers by it.
[<RequireQualifiedAccess>]
type OwnerPurpose =
    /// Customer due diligence by a firm under anti-money-laundering rules. Where the register restricts owners, the answer is NeedsEntitlement, naming the route.
    | CustomerDueDiligence
    /// What the register publishes to anyone. Where it restricts owners, the answer is NotAvailable.
    | PublicRead

/// How far a register provides one kind of data.
[<RequireQualifiedAccess>]
type Support =
    | Supported
    /// Provided with a gap the caller should know of.
    | Partial of caveat: string
    /// Calls answer NotAvailable, or Unknown for a standing, or Unconfirmed for concerns.
    | NotOffered of reason: string

/// What a source costs to use.
[<RequireQualifiedAccess>]
type AccessCost =
    | Free
    /// Free, once registered for a key.
    | FreeWithKey of how: string
    /// Needs a contract or a per-call fee.
    | Paid of route: string

/// What a source says about how fast it may be called.
[<RequireQualifiedAccess>]
type RateLimitPolicy =
    /// The source's own published figure, which an adapter keeps to.
    | Published of calls: int * per: TimeSpan * source: string
    /// No figure found. The adapter paces at this rate, a choice made without data.
    | NotPublished of pacedCalls: int * per: TimeSpan

/// What a register covers, to read before relying on it.
type RegistryCoverage =
    {
        /// The name each answer gives as its Source.
        Registry: string
        Jurisdictions: Jurisdiction list
        Standing: Support
        /// Red flags apart from standing. What isn't offered answers Unconfirmed.
        Concerns: Support
        Addresses: Support
        Search: Support
        Officers: Support
        Owners: Support
        Cost: AccessCost
        RateLimit: RateLimitPolicy
        /// The data's licence as the source states it, or that it states none.
        Licence: string
    }

/// Building and reading a register's coverage.
module RegistryCoverage =

    /// A register's coverage with every part NotOffered: set what it offers with { RegistryCoverage.create ... with ... }, so that a field added
    /// later breaks no adapter.
    let create
        (registry: string)
        (jurisdictions: Jurisdiction list)
        (cost: AccessCost)
        (rateLimit: RateLimitPolicy)
        (licence: string)
        : RegistryCoverage =
        let notOffered = Support.NotOffered "Not offered"

        {
            Registry = registry
            Jurisdictions = jurisdictions
            Standing = notOffered
            Concerns = notOffered
            Addresses = notOffered
            Search = notOffered
            Officers = notOffered
            Owners = notOffered
            Cost = cost
            RateLimit = rateLimit
            Licence = licence
        }

    /// True when one of the jurisdictions the coverage lists answers for jurisdiction, by Jurisdiction.covers.
    let covers (coverage: RegistryCoverage) (jurisdiction: Jurisdiction) =
        coverage.Jurisdictions
        |> List.exists (fun scope -> Jurisdiction.covers scope jurisdiction)

/// Why a register call gave no answer. None of them says anything about the company.
[<RequireQualifiedAccess>]
type RegistryFailure =
    /// The register doesn't answer for this jurisdiction, which says nothing about whether the company exists.
    | OutsideCoverage of Jurisdiction
    /// Not sent: the argument can't be valid for this register.
    | InvalidRequest of reason: string
    /// The register refused the adapter's credentials.
    | AccessRefused of reason: string
    /// The register refused this request or this access to it. Trying again fails the same way until the request or the access is fixed.
    | RequestRefused of reason: string
    | RateLimited of retryAfter: TimeSpan voption
    /// No answer, or a server failure. Try again later.
    | Unavailable of reason: string
    /// The register answered and the adapter could not read it. Holds no text from the answer.
    | UnreadableAnswer of reason: string

/// Reading a register failure.
module RegistryFailure =

    /// One line saying what went wrong, for logs and exception messages.
    let describe failure =
        match failure with
        | RegistryFailure.OutsideCoverage jurisdiction -> $"The register doesn't cover {jurisdiction.Code}"
        | RegistryFailure.InvalidRequest reason -> $"Not sent: {reason}"
        | RegistryFailure.AccessRefused reason -> $"The register refused access: {reason}"
        | RegistryFailure.RequestRefused reason -> $"The register refused the request, which needs fixing: {reason}"
        | RegistryFailure.RateLimited(ValueSome wait) -> $"Rate limited: try again in {wait}"
        | RegistryFailure.RateLimited ValueNone -> "Rate limited"
        | RegistryFailure.Unavailable reason -> $"The register is unavailable: {reason}"
        | RegistryFailure.UnreadableAnswer reason -> $"The register's answer could not be read: {reason}"

    /// True when the same call can succeed later without anything being fixed.
    let isTemporary failure =
        match failure with
        | RegistryFailure.RateLimited _
        | RegistryFailure.Unavailable _ -> true
        | RegistryFailure.OutsideCoverage _
        | RegistryFailure.InvalidRequest _
        | RegistryFailure.AccessRefused _
        | RegistryFailure.RequestRefused _
        | RegistryFailure.UnreadableAnswer _ -> false

/// One company register. An implementation answers OutsideCoverage for a jurisdiction its Coverage doesn't list, Available ValueNone only when the
/// register says the number isn't held, and NotAvailable when its answer can't tell. Cancelling ct throws.
type ICompanyRegistry =
    /// What the register covers. Calls for a jurisdiction it doesn't cover answer OutsideCoverage.
    abstract Coverage: RegistryCoverage

    /// The company with this registration number. Available ValueNone says the register holds no such number; NotAvailable says this source
    /// can't tell, as with a register that leaves some companies out of what it publishes.
    abstract TryFindAsync:
        number: string * jurisdiction: Jurisdiction * ct: CancellationToken ->
            Task<Result<Evidence<Availability<Company voption>>, RegistryFailure>>

    /// Companies by name.
    abstract SearchAsync:
        name: string * jurisdiction: Jurisdiction * ct: CancellationToken ->
            Task<Result<Evidence<Availability<SearchResult>>, RegistryFailure>>

    /// The company's officers, current and former.
    abstract OfficersAsync:
        number: string * jurisdiction: Jurisdiction * ct: CancellationToken ->
            Task<Result<Evidence<Availability<Officer list>>, RegistryFailure>>

    /// The company's owners, with their natures of control. purpose decides what a register that restricts owners answers.
    abstract OwnersAsync:
        number: string * jurisdiction: Jurisdiction * purpose: OwnerPurpose * ct: CancellationToken ->
            Task<Result<Evidence<Availability<OwnerListing>>, RegistryFailure>>

/// A register whose own query does the name matching. Registers that can't, don't implement it: Registries.searchByNameAsync narrows their
/// SearchAsync instead. An implementation answers as SearchAsync does, and InvalidRequest for an Exact or StartsWith name with no letter or digit.
/// Complete is true only when the register's query returned every row it matched. A query can compare less loosely than NameMatch.normalise,
/// such as accents as held: the README's Sources section says how each register compares.
type INameMatchSearch =
    /// Companies whose name matches as asked.
    abstract SearchByNameAsync:
        name: string * matching: NameMatch * jurisdiction: Jurisdiction * ct: CancellationToken ->
            Task<Result<Evidence<Availability<SearchResult>>, RegistryFailure>>

/// What the name match searches of the adapters share.
module internal NameSearch =

    /// The failure for a name the match can't use: an Exact or StartsWith name that isn't blank and holds no letter or digit.
    let refusal (name: string) (matching: NameMatch) =
        match matching with
        | NameMatch.Exact
        | NameMatch.StartsWith when not (String.IsNullOrWhiteSpace name) && NameMatching.normalise name = "" ->
            ValueSome(RegistryFailure.InvalidRequest "An exact or starts-with search needs a letter or digit")
        | _ -> ValueNone

    /// Runs send for a name that can be searched. A blank name, or one refusal refuses, is InvalidRequest and sends nothing.
    let withName (name: string) (matching: NameMatch) (send: unit -> Task<Result<'T, RegistryFailure>>) =
        if String.IsNullOrWhiteSpace name then
            Task.FromResult(Error(RegistryFailure.InvalidRequest "A search needs a term"))
        else
            match refusal name matching with
            | ValueSome failure -> Task.FromResult(Error failure)
            | ValueNone -> send ()

/// A person's name as asked: "Firstname Lastname", or "Lastname, Firstname" with the surname before the comma.
type PersonName =
    {
        /// The words before the surname, as given.
        Forenames: string list
        /// The surname as given: the last word, or everything before the comma.
        Surname: string
    }

    /// Every word as given, forenames first.
    member this.Words =
        this.Forenames
        @ (this.Surname.Split(' ', StringSplitOptions.RemoveEmptyEntries) |> List.ofArray)

/// Parsing a person's name and matching it to published names.
module PersonName =

    let private words (text: string) =
        text.Split([| ' '; '\t' |], StringSplitOptions.RemoveEmptyEntries)
        |> List.ofArray

    /// The name, or ValueNone when it holds no letter or digit. Only the first comma splits surname from forenames.
    let tryParse (name: string) =
        if String.IsNullOrWhiteSpace name || NameMatching.normalise name = "" then
            ValueNone
        else
            match name.IndexOf ',' with
            | comma when comma >= 0 ->
                let surname = String.Join(' ', words (name.Substring(0, comma)))
                let forenames = words (name.Substring(comma + 1).Replace(',', ' '))

                match surname, forenames with
                | "", [] -> ValueNone
                | "", only ->
                    ValueSome
                        {
                            Forenames = List.take (only.Length - 1) only
                            Surname = List.last only
                        }
                | surname, forenames ->
                    ValueSome
                        {
                            Forenames = forenames
                            Surname = surname
                        }
            | _ ->
                let all = words name

                ValueSome
                    {
                        Forenames = List.take (all.Length - 1) all
                        Surname = List.last all
                    }

    let private normalWords (text: string) =
        (NameMatching.normalise text).Split(' ', StringSplitOptions.RemoveEmptyEntries)
        |> Set.ofArray

    /// True when every word of the name asked, after NameMatch.normalise, is a whole word of the published name, in any order. A published name
    /// holding an asterisk is masked and never matches.
    let matches (asked: PersonName) (published: string) =
        if String.IsNullOrWhiteSpace published || published.Contains '*' then
            false
        else
            let wanted = asked.Words |> List.map normalWords |> Set.unionMany
            not wanted.IsEmpty && wanted.IsSubsetOf(normalWords published)

/// A register that publishes people and lets them be searched by name. Registers that don't, don't implement it. An implementation answers
/// OutsideCoverage for a jurisdiction its Coverage doesn't list, InvalidRequest for a name with no letter or digit, and keeps only roles whose
/// name PersonName.matches the name asked. Cancelling ct throws.
type IPersonSearch =
    /// How far the search reaches, and what it leaves out.
    abstract PersonSearch: Support

    /// Roles held by people of this name, as "Firstname Lastname" or "Lastname, Firstname".
    abstract SearchPeopleAsync:
        name: string * jurisdiction: Jurisdiction * ct: CancellationToken ->
            Task<Result<Evidence<Availability<PersonSearchResult>>, RegistryFailure>>

/// Choosing among registers and calling them through their optional interfaces.
module Registries =

    /// The registry to read first for the jurisdiction: the first whose coverage names the exact code, else the first covering its country.
    /// ValueNone when none covers it.
    let forJurisdiction (registries: ICompanyRegistry seq) (jurisdiction: Jurisdiction) =
        let covering =
            registries
            |> Seq.filter (fun registry -> RegistryCoverage.covers registry.Coverage jurisdiction)
            |> List.ofSeq

        covering
        |> List.tryFind (fun registry -> List.contains jurisdiction registry.Coverage.Jurisdictions)
        |> Option.orElse (List.tryHead covering)
        |> ValueOption.ofOption

    /// Companies whose name matches as asked: the register's own INameMatchSearch when it has one. Otherwise its SearchAsync, as answered for
    /// Contains, and narrowed by NameMatch.narrow for Exact and StartsWith, which is never Complete. An Exact or StartsWith name with no letter
    /// or digit is InvalidRequest, within the register's coverage, and nothing is sent.
    let searchByNameAsync
        (registry: ICompanyRegistry)
        (name: string)
        (matching: NameMatch)
        (jurisdiction: Jurisdiction)
        (ct: CancellationToken)
        : Task<Result<Evidence<Availability<SearchResult>>, RegistryFailure>> =
        match NameSearch.refusal name matching with
        | ValueSome failure when RegistryCoverage.covers registry.Coverage jurisdiction ->
            Task.FromResult(Error failure)
        | _ ->
            match registry, matching with
            | :? INameMatchSearch as search, _ -> search.SearchByNameAsync(name, matching, jurisdiction, ct)
            | _, NameMatch.Contains -> registry.SearchAsync(name, jurisdiction, ct)
            | _, NameMatch.Exact
            | _, NameMatch.StartsWith ->
                task {
                    let! answer = registry.SearchAsync(name, jurisdiction, ct)

                    return
                        answer
                        |> Result.map (fun evidence ->
                            match evidence.Value with
                            | Available result ->
                                { evidence with
                                    Value = Available(NameMatch.narrow matching name result)
                                }
                            | NotAvailable _
                            | NeedsEntitlement _ -> evidence)
                }

    /// The registries that also search people, in order.
    let personSearches (registries: ICompanyRegistry seq) =
        registries
        |> Seq.choose (fun registry ->
            match registry with
            | :? IPersonSearch as search -> Some(registry, search)
            | _ -> None)
        |> List.ofSeq
