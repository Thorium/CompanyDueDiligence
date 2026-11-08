/// Companies House records, as the Public Data and Document APIs publish them. Each holds the fields onboarding reads, and Raw
/// holds the rest as sent. A field the answer lacks is ValueNone, which for a flag says nothing either way.
namespace CompanyDueDiligence.CompaniesHouse

open System
open CompanyDueDiligence

/// What an overseas company's UK record says of its home registration, from foreign_company_details.
type ForeignCompanyDetails =
    {
        /// originating_registry.name.
        RegistryName: string voption
        /// originating_registry.country.
        RegistryCountry: string voption
        RegistrationNumber: string voption
        /// governed_by: the law governing it where it was incorporated.
        GovernedBy: string voption
        /// legal_form, or company_type as the spec names it.
        LegalForm: string voption
    }

/// A UK establishment's overseas parent, from branch_company_details.
type BranchParent =
    {
        ParentNumber: string voption
        ParentName: string voption
    }

type PreviousName =
    {
        Name: string
        From: DateOnly voption
        To: DateOnly voption
    }

type CompanyProfile =
    {
        CompanyNumber: string
        CompanyName: string
        /// Companies House's company type, such as "ltd" or "llp".
        Type: string voption
        /// Companies House's company_status, such as "active" or "liquidation".
        Status: string voption
        /// Companies House's company_status_detail, such as "active-proposal-to-strike-off".
        StatusDetail: string voption
        /// Companies House's jurisdiction, such as "england-wales".
        Jurisdiction: string voption
        CreatedOn: DateOnly voption
        CeasedOn: DateOnly voption
        HasInsolvencyHistory: bool voption
        HasBeenLiquidated: bool voption
        /// The profile's link to the company's insolvency list, which the spec names in place of the two deprecated flags above.
        InsolvencyLink: string voption
        HasCharges: bool voption
        RegisteredOfficeInDispute: bool voption
        UndeliverableRegisteredOffice: bool voption
        /// True if accounts.next_accounts.overdue or the deprecated accounts.overdue says so.
        AccountsOverdue: bool voption
        ConfirmationStatementOverdue: bool voption
        /// annual_return.overdue. The spec returns annual_return only where no confirmation statement has been filed.
        AnnualReturnOverdue: bool voption
        SicCodes: string list
        PreviousNames: PreviousName list
        RegisteredOffice: Address voption
        ServiceAddress: Address voption
        Foreign: ForeignCompanyDetails voption
        Branch: BranchParent voption
        /// external_registration_number.
        ExternalRegistrationNumber: string voption
        Raw: string
    }

module CompanyProfile =

    /// Legal standing: reads company_status and company_status_detail exactly as the enumerations spell them. A value not listed there is
    /// Unknown, as is one listed but contradicting "active". Red flags apart from the status are in concerns.
    let standing (profile: CompanyProfile) : CompanyStanding =
        match profile.Status, profile.StatusDetail with
        | ValueSome "active", (ValueNone | ValueSome "" | ValueSome "active") -> CompanyStanding.Active
        | ValueSome "active", ValueSome "active-proposal-to-strike-off" ->
            CompanyStanding.ActiveWithConcern "proposal to strike off"
        | ValueSome "active", ValueSome detail -> CompanyStanding.Unknown(ValueSome $"active, {detail}")
        | ValueSome "dissolved", _ -> CompanyStanding.Dissolved
        | ValueSome("liquidation" | "receivership" | "administration" | "voluntary-arrangement" | "insolvency-proceedings" as status),
          _ -> CompanyStanding.InInsolvency status
        | ValueSome("closed" | "converted-closed" | "removed" as status), _ -> CompanyStanding.Closed status
        | ValueSome("open" | "registered" as status), _ -> CompanyStanding.Registered(ValueSome status)
        | status, _ -> CompanyStanding.Unknown status

    /// True if either statement says so. False needs the confirmation statement to say so, or the annual return where there is no confirmation statement.
    let private annualFilingOverdue (profile: CompanyProfile) =
        match profile.ConfirmationStatementOverdue, profile.AnnualReturnOverdue with
        | ValueSome true, _
        | _, ValueSome true -> ValueSome true
        | ValueSome false, _
        | ValueNone, ValueSome false -> ValueSome false
        | ValueNone, ValueNone -> ValueNone

    /// True on the link or either flag. False needs both flags to say so.
    let private insolvencyHistory (profile: CompanyProfile) =
        match profile.InsolvencyLink, profile.HasInsolvencyHistory, profile.HasBeenLiquidated with
        | ValueSome _, _, _
        | _, ValueSome true, _
        | _, _, ValueSome true -> ValueSome true
        | ValueNone, ValueSome false, ValueSome false -> ValueSome false
        | _ -> ValueNone

    /// The flags in neutral terms. One the answer doesn't settle is Unconfirmed, never clear.
    let concerns (profile: CompanyProfile) : CompanyConcerns =
        let checks =
            [
                CompanyConcern.AccountsOverdue, profile.AccountsOverdue
                CompanyConcern.AnnualFilingOverdue, annualFilingOverdue profile
                CompanyConcern.RegisteredOfficeDisputed, profile.RegisteredOfficeInDispute
                CompanyConcern.RegisteredOfficeUndeliverable, profile.UndeliverableRegisteredOffice
                CompanyConcern.InsolvencyHistory, insolvencyHistory profile
            ]

        let where answer =
            checks |> List.filter (fun (_, state) -> state = answer) |> List.map fst

        {
            Stated = where (ValueSome true)
            Unconfirmed = where ValueNone
        }

/// A hit from the company search.
type CompanySearchItem =
    {
        CompanyNumber: string
        Title: string
        Status: string voption
        Type: string voption
        CreatedOn: DateOnly voption
        Address: Address voption
        Raw: string
    }

/// A hit from the officer search: one officer as Companies House groups appointments, not a company.
type OfficerSearchItem =
    {
        Title: string
        Description: string voption
        AppointmentCount: int voption
        DateOfBirth: YearMonth voption
        Address: Address voption
        /// For Api.Appointments, taken from the item's self link.
        OfficerId: string voption
        Raw: string
    }

type CompanyOfficer =
    {
        Name: string
        /// Companies House's officer_role, such as "director" or "corporate-secretary".
        Role: string voption
        AppointedOn: DateOnly voption
        ResignedOn: DateOnly voption
        DateOfBirth: YearMonth voption
        Nationality: string voption
        CountryOfResidence: string voption
        Occupation: string voption
        Address: Address voption
        /// The officer's appointments, as a path.
        AppointmentsLink: string voption
        /// For Api.Appointments, taken from AppointmentsLink.
        OfficerId: string voption
        /// A corporate officer's own registration.
        Identification: EntityRegistration voption
        IdentityVerification: IdentityVerification voption
        Raw: string
    }

/// One appointment of an officer, at any company.
type OfficerAppointment =
    {
        Name: string
        /// Companies House's officer_role, such as "director".
        Role: string voption
        AppointedOn: DateOnly voption
        ResignedOn: DateOnly voption
        /// The number of the company the appointment is at.
        CompanyNumber: string
        CompanyName: string voption
        CompanyStatus: string voption
        Nationality: string voption
        CountryOfResidence: string voption
        Occupation: string voption
        Address: Address voption
        Identification: EntityRegistration voption
        IdentityVerification: IdentityVerification voption
        Raw: string
    }

/// A UK establishment of a company registered overseas.
type UkEstablishment =
    {
        CompanyNumber: string
        CompanyName: string
        Status: string voption
        Locality: string voption
        Raw: string
    }

/// Which details call answers for a disqualified officer. These ids aren't known to link to officer ids.
[<RequireQualifiedAccess>]
type DisqualifiedOfficerId =
    | Natural of string
    | Corporate of string

/// A hit from the disqualified officer search: a name that matched, and no more than that.
type DisqualifiedOfficerSearchItem =
    {
        Title: string
        Description: string voption
        DateOfBirth: DateOnly voption
        Address: Address voption
        /// For Api.DisqualifiedOfficers.detailsAsync, taken from the item's self link.
        OfficerId: DisqualifiedOfficerId voption
        Raw: string
    }

/// One disqualification of an officer.
type Disqualification =
    {
        CaseIdentifier: string voption
        /// Companies House's disqualification_type, naming the authority that handled it.
        Type: string voption
        From: DateOnly voption
        Until: DateOnly voption
        HeardOn: DateOnly voption
        UndertakenOn: DateOnly voption
        CourtName: string voption
        /// The companies where the misconduct took place.
        CompanyNames: string list
        /// Companies House's reason.description_identifier.
        Reason: string voption
        /// Companies House's reason.act, the law it was made under.
        Act: string voption
        Address: Address voption
    }

/// Permission a disqualified officer has to act despite a disqualification.
type PermissionToAct =
    {
        CompanyNames: string list
        CourtName: string voption
        GrantedOn: DateOnly voption
        ExpiresOn: DateOnly voption
    }

/// A disqualified officer's record, natural or corporate. The fields of the other kind are ValueNone.
type DisqualifiedOfficer =
    {
        /// A natural officer's name parts joined, or a corporate officer's name.
        Name: string
        DateOfBirth: DateOnly voption
        Nationality: string voption
        /// A corporate officer's registration number.
        CompanyNumber: string voption
        CountryOfRegistration: string voption
        PersonNumber: string voption
        Disqualifications: Disqualification list
        PermissionsToAct: PermissionToAct list
        Raw: string
    }

/// What a search by name read, up to its cap.
type SearchRead<'T> =
    {
        Found: 'T list
        /// The total the search stated.
        StatedTotal: int
        /// True only when Found holds as many as StatedTotal.
        Complete: bool
    }

/// The answer to a disqualified officer search by name. It is never a definite hit or a clear: the register gives no link from a company's
/// officer to a disqualified officer, and a name can be spelled another way or shared. A search that failed is an Error, not one of these.
[<RequireQualifiedAccess>]
type DisqualificationSearch =
    /// Entries whose wording matched, to check by hand against each one's details. If not Complete, there are more unread.
    | PossibleMatches of SearchRead<DisqualifiedOfficerSearchItem>
    /// The search read everything and no entry matched this wording.
    | NoNameMatch
    /// The search answered without settling it.
    | Inconclusive of reason: string

/// A person with significant control, or a beneficial owner.
type Psc =
    {
        /// Companies House's kind, such as "individual-person-with-significant-control".
        Kind: string voption
        /// Absent for a super-secure person, whose details are withheld.
        Name: string voption
        NotifiedOn: DateOnly voption
        CeasedOn: DateOnly voption
        /// The answer's own ceased flag; a CeasedOn date counts as ceased without it.
        Ceased: bool
        NaturesOfControl: string list
        DateOfBirth: YearMonth voption
        Nationality: string voption
        CountryOfResidence: string voption
        Address: Address voption
        /// A corporate or legal person's own registration.
        Identification: EntityRegistration voption
        IdentityVerification: IdentityVerification voption
        Raw: string
    }

/// What a company states about its persons with significant control.
type PscStatement =
    {
        /// Companies House's code, such as "psc-exists-but-not-identified".
        Statement: string
        NotifiedOn: DateOnly voption
        CeasedOn: DateOnly voption
        LinkedPscName: string voption
        Raw: string
    }

type FilingHistoryItem =
    {
        TransactionId: string
        Category: string voption
        SubCategory: string voption
        /// The form type, such as "AD01".
        Type: string voption
        Date: DateOnly voption
        /// Companies House's description key, such as "incorporation-company".
        Description: string voption
        Pages: int voption
        /// For Api.Documents, taken from the item's document_metadata link. The link's host isn't used.
        DocumentId: string voption
        /// The item's document_metadata link as sent.
        DocumentLink: string voption
        /// description_values.capital: currency and figure as sent. Unverified: the bundled spec doesn't describe it, and an item without it is normal.
        Capital: (string * string) list
        Raw: string
    }

type Charge =
    {
        Id: string voption
        ChargeCode: string voption
        ChargeNumber: int voption
        /// Companies House's status, such as "outstanding" or "fully-satisfied".
        Status: string voption
        Classification: string voption
        CreatedOn: DateOnly voption
        DeliveredOn: DateOnly voption
        SatisfiedOn: DateOnly voption
        PersonsEntitled: string list
        Particulars: string list
        Raw: string
    }

type InsolvencyDate =
    {
        /// Companies House's date type, such as "wound-up-on".
        Type: string voption
        Date: DateOnly voption
    }

type InsolvencyPractitioner =
    {
        Name: string voption
        Role: string voption
        AppointedOn: DateOnly voption
        CeasedToActOn: DateOnly voption
        Address: Address voption
    }

type InsolvencyCase =
    {
        /// Companies House's case type, such as "compulsory-liquidation".
        Type: string voption
        Number: string voption
        Dates: InsolvencyDate list
        Practitioners: InsolvencyPractitioner list
        Notes: string list
    }

type Insolvency =
    {
        Status: string voption
        Cases: InsolvencyCase list
        Raw: string
    }

/// A document's metadata. ContentTypes are the values Api.Documents can ask for.
type DocumentMetadata =
    {
        Id: string voption
        Pages: int voption
        CreatedAt: DateTimeOffset voption
        ContentTypes: string list
        Raw: string
    }

/// Where the Document API redirected to. The URL's query is a signature that lets anyone read the document until it expires, so printing
/// one (%A, ToString) shows the host only: keep the Uri out of logs.
[<StructuredFormatDisplay("{Display}")>]
type DocumentLocation =
    {
        Uri: Uri
        ContentType: string
    }

    member this.Display =
        $"{{ Host = {this.Uri.Host}; ContentType = {this.ContentType}; signed URL hidden }}"

    override this.ToString() = this.Display

type DocumentContent =
    {
        /// The type the document host answered with, or else the type asked for.
        ContentType: string
        Content: byte[]
    }

/// The counts a list states beside its total.
[<Struct>]
type ListCounts =
    {
        Active: int
        /// resigned_count or ceased_count, whichever the list uses.
        Ended: int
        /// inactive_count, which the spec doesn't list; zero when absent.
        Inactive: int
    }

/// One page of a list.
type Page<'T> =
    {
        Items: 'T list
        /// total_results or total_count, whichever the list uses.
        Total: int
        /// For a list whose spec requires them.
        Counts: ListCounts voption
        /// Each item as sent, compared by Paging to notice a repeated page or an item that moved.
        ItemsRaw: string list
    }
