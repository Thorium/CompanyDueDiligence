namespace CompanyDueDiligence.CompaniesHouse

open System
open System.Text.RegularExpressions
open System.Threading
open CompanyDueDiligence

/// Companies House records in the contract's neutral terms.
module internal Neutral =

    [<Literal>]
    let source = "Companies House"

    /// Companies House answers for the whole United Kingdom register.
    let coverage: RegistryCoverage =
        {
            Registry = source
            Jurisdictions = [ Jurisdiction.ofCode "GB" ]
            Standing = Support.Supported
            Concerns = Support.Supported
            Addresses = Support.Supported
            Search =
                Support.Partial
                    "One page of matches, ranked by Companies House; Complete says whether it holds them all"
            Officers = Support.Supported
            Owners =
                Support.Partial
                    "The register of persons with significant control names those holding over 25% or control by other means, not every shareholder; exempt companies answer NotAvailable"
            Cost = AccessCost.FreeWithKey "An application of type \"API Key\" on the Companies House Developer Hub"
            RateLimit =
                RateLimitPolicy.Published(
                    600,
                    TimeSpan.FromMinutes 5.,
                    "The Companies House rate limiting guide; its X-Ratelimit headers were seen live on 2026-11-08"
                )
            Licence = "No rules on use, per Companies House; bans for exceeding 600 per 5 minutes"
        }

    /// england-wales has no ISO 3166 code of its own, so it and anything not listed is the register's country.
    let jurisdiction (published: string voption) =
        match published with
        | ValueSome "england" -> Jurisdiction.ofCode "GB-ENG"
        | ValueSome "wales" -> Jurisdiction.ofCode "GB-WLS"
        | ValueSome "scotland" -> Jurisdiction.ofCode "GB-SCT"
        | ValueSome "northern-ireland" -> Jurisdiction.ofCode "GB-NIR"
        | _ -> Jurisdiction.ofCode "GB"

    /// A registered-overseas-entity counts as a foreign registration: it was formed elsewhere and this record isn't its home. Unverified: the type
    /// values come from the enumerations, not from a live answer.
    let private role (profile: CompanyProfile) =
        match profile.Type with
        | ValueNone -> RecordRole.NotStated
        | ValueSome("oversea-company" | "registered-overseas-entity") -> RecordRole.ForeignRegistration
        | ValueSome "uk-establishment" -> RecordRole.Establishment
        | ValueSome _ -> RecordRole.Home

    /// What a foreign company's record says of its home registration. external_registration_number stands in for a number foreign_company_details lacks.
    let private home (profile: CompanyProfile) =
        match profile.Foreign with
        | ValueNone ->
            profile.ExternalRegistrationNumber
            |> ValueOption.map (fun number ->
                {
                    LegalName = ValueSome profile.CompanyName
                    LegalForm = ValueNone
                    GoverningLaw = ValueNone
                    Country = ValueNone
                    Register = ValueNone
                    RegistrationNumber = ValueSome number
                })
        | ValueSome foreign ->
            ValueSome
                {
                    LegalName = ValueSome profile.CompanyName
                    LegalForm = foreign.LegalForm
                    GoverningLaw = foreign.GovernedBy
                    Country = foreign.RegistryCountry
                    Register = foreign.RegistryName
                    RegistrationNumber =
                        foreign.RegistrationNumber
                        |> ValueOption.orElse profile.ExternalRegistrationNumber
                }

    let company (profile: CompanyProfile) : Company =
        let addresses =
            [
                profile.RegisteredOffice
                |> ValueOption.map (fun address ->
                    {
                        Kind = AddressKind.RegisteredOffice
                        Address = address
                    })
                profile.ServiceAddress
                |> ValueOption.map (fun address ->
                    {
                        Kind = AddressKind.Service
                        Address = address
                    })
            ]
            |> List.choose ValueOption.toOption

        {
            Name = profile.CompanyName
            OtherNames = profile.PreviousNames |> List.map _.Name
            Number = profile.CompanyNumber
            Jurisdiction = jurisdiction profile.Jurisdiction
            JurisdictionAsPublished = profile.Jurisdiction
            Standing = CompanyProfile.standing profile
            Concerns = CompanyProfile.concerns profile
            StatusAsPublished = profile.Status
            LegalForm = profile.Type
            IncorporatedOn = profile.CreatedOn
            CeasedOn = profile.CeasedOn
            Addresses =
                match addresses with
                | [] -> NotAvailable "The profile lists no address"
                | _ -> Available addresses
            Role = role profile
            HomeRegistration = home profile
            EstablishmentOf = profile.Branch |> ValueOption.bind _.ParentNumber
        }

    let companyMatch (item: CompanySearchItem) : CompanyMatch =
        {
            Name = item.Title
            Number = item.CompanyNumber
            StatusAsPublished = item.Status
            Address = item.Address
        }

    let officerRole (role: string voption) =
        match role with
        | ValueSome("director" | "corporate-director" | "nominee-director" | "corporate-nominee-director") ->
            OfficerRole.Director
        | ValueSome("secretary" | "corporate-secretary" | "nominee-secretary" | "corporate-nominee-secretary") ->
            OfficerRole.Secretary
        | ValueSome other -> OfficerRole.Other other
        | ValueNone -> OfficerRole.Other ""

    /// The roles of the specification's officer_role list that have a corporate- form beside them, so that without it they are a person's.
    let private personRoles =
        set
            [
                "director"
                "llp-designated-member"
                "llp-member"
                "manager-of-an-eeig"
                "managing-officer"
                "member-of-a-management-organ"
                "member-of-a-supervisory-organ"
                "member-of-an-administrative-organ"
                "nominee-director"
                "nominee-secretary"
                "secretary"
            ]

    /// A corporate- role or an identification block is a legal entity. A date of birth, or a role with a corporate- form beside it, is a
    /// person. Any other role, such as a partner in a limited partnership, is held by either, and doesn't say which.
    let officerKind (officer: CompanyOfficer) =
        match officer.Role with
        | ValueSome role when role.StartsWith("corporate-", StringComparison.Ordinal) -> PartyKind.LegalEntity
        | _ when officer.Identification.IsSome -> PartyKind.LegalEntity
        | _ when officer.DateOfBirth.IsSome -> PartyKind.Individual
        | ValueSome role when personRoles.Contains role -> PartyKind.Individual
        | role -> PartyKind.Unknown role

    let officer (officer: CompanyOfficer) : Officer =
        {
            Name = officer.Name
            Kind = officerKind officer
            Role = officerRole officer.Role
            Current = officer.ResignedOn.IsNone
            AppointedOn = officer.AppointedOn
            EndedOn = officer.ResignedOn
            BornIn = officer.DateOfBirth
            Nationality = officer.Nationality
            CountryOfResidence = officer.CountryOfResidence
            Address = officer.Address
            Entity = officer.Identification
            IdentityVerification = officer.IdentityVerification
        }

    let private controlKind (nature: string) =
        if nature.StartsWith "ownership-of-shares" then
            ControlKind.Ownership
        elif nature.StartsWith "voting-rights" then
            ControlKind.VotingRights
        elif nature.StartsWith "right-to-appoint-and-remove" then
            ControlKind.AppointsBoard
        elif nature.StartsWith "significant-influence-or-control" then
            ControlKind.SignificantInfluence
        else
            ControlKind.Other

    let private bandRegex = Regex @"(\d{1,3})-to-(\d{1,3})-percent"
    let private bandRegex2 = Regex @"more-than-(\d{1,3})-percent"

    /// The percentage in a nature such as ownership-of-shares-25-to-50-percent or voting-rights-more-than-25-percent-...
    let private band (nature: string) =
        let range = bandRegex.Match nature
        let moreThan = bandRegex2.Match nature

        if range.Success then
            ValueSome
                {
                    From = int range.Groups[1].Value
                    To = int range.Groups[2].Value
                }
        elif moreThan.Success then
            ValueSome
                {
                    From = int moreThan.Groups[1].Value
                    To = 100
                }
        else
            ValueNone

    let natureOfControl (nature: string) : NatureOfControl =
        {
            Kind = controlKind nature
            Band = band nature
            AsPublished = nature
        }

    let private ownerKind (kind: string voption) =
        match kind with
        | ValueSome kind when kind.StartsWith "individual-" -> PartyKind.Individual
        | ValueSome kind when kind.StartsWith "corporate-entity-" || kind.StartsWith "legal-person-" ->
            PartyKind.LegalEntity
        | ValueSome kind when kind.StartsWith "super-secure-" -> PartyKind.Withheld
        | other -> PartyKind.Unknown other

    let owner (psc: Psc) : Owner =
        {
            Name = psc.Name
            Kind = ownerKind psc.Kind
            Control = psc.NaturesOfControl |> List.map natureOfControl
            Current = Api.Psc.isActive psc
            Since = psc.NotifiedOn
            EndedOn = psc.CeasedOn
            BornIn = psc.DateOfBirth
            Nationality = psc.Nationality
            CountryOfResidence = psc.CountryOfResidence
            Address = psc.Address
            Entity = psc.Identification
            IdentityVerification = psc.IdentityVerification
        }

    /// A statement's meaning, from its code as psc_descriptions.yml in the enumerations spells it, misspelling included. A code not listed here
    /// leaves a gap until it is.
    let private statementMeaning (statement: string) =
        match statement with
        | "no-individual-or-entity-with-signficant-control"
        | "no-individual-or-entity-with-signficant-control-partnership" -> OwnerStatementMeaning.NoOwnerToList
        | "all-beneficial-owners-identified" -> OwnerStatementMeaning.AllOwnersIdentified
        | "nobody-has-become-or-ceased-to-be-a-beneficial-owner"
        | "somebody-has-become-or-ceased-to-be-a-beneficial-owner"
        | "no-change-beneficial-owner-relevant-period"
        | "change-beneficial-owner-relevant-period"
        | "no-trust-involved-relevant-period"
        | "trust-involved-relevant-period"
        | "no-change-beneficiary-relevant-period"
        | "change-beneficiary-relevant-period" -> OwnerStatementMeaning.ChangesOnly
        | "psc-exists-but-not-identified"
        | "psc-exists-but-not-identified-partnership"
        | "no-beneficial-owner-identified"
        | "at-least-one-beneficial-owner-unidentified"
        | "at-least-one-beneficial-owner-unidentified-and-information-not-provided-for-at-least-one-beneficial-owner" ->
            OwnerStatementMeaning.LeavesGap OwnershipGap.OwnerNotIdentified
        | "psc-details-not-confirmed"
        | "psc-details-not-confirmed-partnership"
        | "awaiting-confirmation-from-psc"
        | "information-not-provided-for-at-least-one-beneficial-owner" ->
            OwnerStatementMeaning.LeavesGap OwnershipGap.OwnerDetailsNotConfirmed
        | "steps-to-find-psc-not-yet-completed"
        | "steps-to-find-psc-not-yet-completed-partnership" ->
            OwnerStatementMeaning.LeavesGap OwnershipGap.InquiriesIncomplete
        | "psc-contacted-but-no-response"
        | "psc-contacted-but-no-response-partnership"
        | "psc-has-failed-to-confirm-changed-details"
        | "psc-has-failed-to-confirm-changed-details-partnership"
        | "restrictions-notice-issued-to-psc"
        | "restrictions-notice-issued-to-psc-partnership" ->
            OwnerStatementMeaning.LeavesGap OwnershipGap.OwnerNotCooperating
        | other -> OwnerStatementMeaning.LeavesGap(OwnershipGap.StatementNotUnderstood other)

    /// What keeps a current owner from counting as identified.
    let private ownerGap (owner: Owner) =
        match owner.Kind, owner.Name, owner.Entity with
        | PartyKind.Withheld, _, _ -> ValueSome OwnershipGap.OwnerWithheld
        | PartyKind.Unknown _, _, _
        | _, ValueNone, _ -> ValueSome OwnershipGap.OwnerUnreadable
        | PartyKind.LegalEntity, _, ValueSome { RegistrationNumber = ValueSome _ } -> ValueNone
        | PartyKind.LegalEntity, _, _ -> ValueSome OwnershipGap.EntityNotIdentified
        | PartyKind.Individual, _, _ -> ValueNone

    /// An empty list only reads as no owners when a current statement says so. Without a current owner, anything else is not available. With
    /// one, the listing's Gaps hold whatever a statement or a current owner leaves open.
    let owners (pscs: Psc list) (statements: PscStatement list) =
        let current =
            statements
            |> List.filter Api.PscStatements.isActive
            |> List.map (fun statement ->
                {
                    Meaning = statementMeaning statement.Statement
                    AsPublished = statement.Statement
                })

        let listed = pscs |> List.map owner
        let currentOwners = listed |> List.filter _.Current

        let statesNoOwner =
            current
            |> List.exists (fun statement -> statement.Meaning = OwnerStatementMeaning.NoOwnerToList)

        let gaps =
            [
                for statement in current do
                    match statement.Meaning with
                    | OwnerStatementMeaning.LeavesGap gap -> gap
                    | OwnerStatementMeaning.NoOwnerToList
                    | OwnerStatementMeaning.AllOwnersIdentified
                    | OwnerStatementMeaning.ChangesOnly -> ()

                if statesNoOwner && not currentOwners.IsEmpty then
                    OwnershipGap.StatementContradictsOwners

                for owner in currentOwners do
                    match ownerGap owner with
                    | ValueSome gap -> gap
                    | ValueNone -> ()
            ]
            |> List.distinct

        let codes = current |> List.map _.AsPublished

        if not currentOwners.IsEmpty || statesNoOwner then
            Available
                {
                    Owners = listed
                    Statements = current
                    Gaps = gaps
                }
        else
            match codes, pscs with
            | [], [] ->
                NotAvailable
                    "No person with significant control or statement is held for this number; an exempt company answers like this too"
            | [], _ -> NotAvailable "Only former persons with significant control are held, and no statement"
            | _ -> NotAvailable("The company's statements don't name its owners: " + String.concat ", " codes)

    /// What a profile says keeps the company's lists from being whole.
    [<Struct>]
    type ProfileMarkers =
        {
            /// partial_data_available.
            PartialData: string voption
            /// super_secure_managing_officer_count, zero when absent.
            SuperSecureOfficers: int
        }

    let markers (profile: CompanyProfile) =
        {
            PartialData = profile.PartialData
            SuperSecureOfficers = profile.SuperSecureManagingOfficers |> ValueOption.defaultValue 0 |> max 0
        }

    let private partialData (marker: string) =
        $"The profile states partial_data_available ({marker}): Companies House isn't the primary source of this company's data"

    /// An officer the lists don't show, as a placeholder that says why.
    let private unlisted (name: string) (kind: PartyKind) (role: string) : Officer =
        {
            Name = name
            Kind = kind
            Role = OfficerRole.Other role
            Current = true
            AppointedOn = ValueNone
            EndedOn = ValueNone
            BornIn = ValueNone
            Nationality = ValueNone
            CountryOfResidence = ValueNone
            Address = ValueNone
            Entity = ValueNone
            IdentityVerification = ValueNone
        }

    /// The officer list as the profile's markers leave it: one withheld officer for each super secure managing officer the profile counts,
    /// and, where Companies House isn't the primary source, an empty list as not available and any other with one officer not given.
    let officersWith (found: ProfileMarkers) (listed: Officer list) : Availability<Officer list> =
        let superSecure =
            List.replicate
                found.SuperSecureOfficers
                (unlisted
                    PartyNames.NotPublished
                    PartyKind.Withheld
                    "managing officer, super secure (super_secure_managing_officer_count)")

        match found.PartialData, listed @ superSecure with
        | ValueSome marker, [] ->
            NotAvailable(partialData marker + ", so an empty officer list doesn't say it has none")
        | ValueSome marker, officers ->
            Available(
                officers
                @ [
                    unlisted
                        PartyNames.NotGiven
                        (PartyKind.Unknown ValueNone)
                        $"any officer only the primary source holds (partial_data_available: {marker})"
                ]
            )
        | ValueNone, officers -> Available officers

    /// The owner listing with a gap where Companies House isn't the primary source.
    let ownersWith (found: ProfileMarkers) (listing: OwnerListing) : OwnerListing =
        match found.PartialData with
        | ValueSome marker ->
            { listing with
                Gaps =
                    listing.Gaps
                    @ [
                        OwnershipGap.StatementNotUnderstood
                            $"partial_data_available: {marker} (Companies House isn't the primary source)"
                    ]
            }
        | ValueNone -> listing

    /// Whether a filing history's own filing_history_status leaves it whole: absent, or available. Any other status says the list is limited.
    let filingsStatedWhole (status: string voption) =
        status.IsNone || status = ValueSome Api.FilingHistory.Available

    /// A filing in neutral terms. A capital figure that isn't a number is left out of Capital and stays in the raw item.
    let filing (item: FilingHistoryItem) : FiledDocument =
        {
            DocumentId = item.DocumentId
            TransactionId = item.TransactionId
            Category = item.Category
            FormType = item.Type
            Description = item.Description
            FiledOn = item.Date
            Pages = item.Pages
            Link = item.DocumentLink
            Capital =
                item.Capital
                |> List.choose (fun (currency, figure) ->
                    match
                        Decimal.TryParse(
                            figure,
                            Globalization.NumberStyles.Number,
                            Globalization.CultureInfo.InvariantCulture
                        )
                    with
                    | true, amount ->
                        Some
                            {
                                Kind = CapitalKind.Issued
                                Shares = ValueNone
                                Amount = ValueSome amount
                                Currency = ValueSome currency
                            }
                    | _ -> None)
        }

    let proceeding (case: InsolvencyCase) : InsolvencyProceeding =
        {
            Kind = case.Type
            CaseNumber = case.Number
            Dates =
                case.Dates
                |> List.choose (fun date ->
                    match date.Type, date.Date with
                    | ValueSome kind, ValueSome on -> Some(kind, on)
                    | _ -> None)
            Practitioners =
                case.Practitioners
                |> List.choose (fun practitioner -> practitioner.Name |> ValueOption.toOption)
        }

    let personSearch =
        Support.Partial
            "The 50 officers the officer search ranks first, whose name holds every word asked, with the first 50 appointments of up to 20 of them; disqualified directors are a separate search, Api.DisqualifiedOfficers.searchAsync. Run live on 2026-11-08"

    /// One appointment of an officer whose name matched.
    let personMatch (appointment: OfficerAppointment) : PersonMatch =
        {
            PersonMatch.Name = appointment.Name
            Role = appointment.Role
            Current = ValueSome appointment.ResignedOn.IsNone
            CompanyName = appointment.CompanyName
            CompanyNumber = ValueSome appointment.CompanyNumber
            CompanyStatus = appointment.CompanyStatus
            AppointedOn = appointment.AppointedOn
            EndedOn = appointment.ResignedOn
        }

    let failure (now: DateTimeOffset) (failure: CompaniesHouseFailure) : RegistryFailure =
        match failure with
        | InvalidRequest reason -> RegistryFailure.InvalidRequest reason
        | KeyRefused _ -> RegistryFailure.AccessRefused "Companies House refused the API key"
        | RateLimited limit -> RegistryFailure.RateLimited(ValueSome(RateLimit.wait now limit))
        | RedirectRefused(status, reason) ->
            RegistryFailure.RequestRefused $"HTTP {status} redirect not followed: {reason}"
        | ApiError(status, _) when status >= 500 -> RegistryFailure.Unavailable $"HTTP {status}"
        | ApiError(status, _) -> RegistryFailure.RequestRefused $"HTTP {status}"
        | IncompleteList(read, expected) ->
            RegistryFailure.Unavailable $"The list ended after {read} of {expected} items"
        | RepeatedPage start -> RegistryFailure.Unavailable $"The page at start_index {start} repeats an earlier page"
        | InconsistentList(start, reason) ->
            RegistryFailure.Unavailable $"The list can't be relied on at start_index {start}: {reason}"
        | ListTooLong(pages, read, expected) ->
            RegistryFailure.UnreadableAnswer $"Stopped after {pages} pages, at {read} of {expected} items"
        | TransportError e -> RegistryFailure.Unavailable e.Message
        | InvalidResponse answer -> RegistryFailure.UnreadableAnswer answer.Reason

namespace CompanyDueDiligence

open System
open System.Text.RegularExpressions
open System.Threading
open CompanyDueDiligence.CompaniesHouse

/// Companies House as an ICompanyRegistry, with its filings, insolvency and documents as an IFilingRegister and its officers as an
/// IPersonSearch. Make it with Sources.companiesHouse.
[<Sealed>]
type CompaniesHouseRegistry internal (client: CompaniesHouseClient, now: unit -> DateTimeOffset) =

    /// How many matches SearchAsync asks for.
    static let searchSize = 50

    /// How many officers SearchPeopleAsync asks for, how many of those named as asked it reads the appointments of, and how many appointments of each.
    static let officerPage = 50

    static let officersRead = 20
    static let appointmentPage = 50

    /// How many found numbers a registry remembers before it starts over.
    static let heldMost = 10000

    let answered raw value : Evidence<_> =
        {
            Source = Neutral.source
            RetrievedAt = now ()
            AsOf = ValueNone
            Raw = raw
            Value = value
        }

    /// Several records' raw text, as one JSON list.
    let rawList (items: string list) =
        ValueSome("[" + String.concat "," items + "]")

    /// Runs a call in a covered jurisdiction; any other answers OutsideCoverage and sends nothing.
    let inCoverage jurisdiction (call: unit -> Threading.Tasks.Task<Result<Evidence<'T>, RegistryFailure>>) =
        if RegistryCoverage.covers Neutral.coverage jurisdiction then
            call ()
        else
            Threading.Tasks.Task.FromResult(Error(RegistryFailure.OutsideCoverage jurisdiction))

    /// Numbers whose profile this registry has read, each with what the profile says keeps its lists from being whole, so that withProfile
    /// needn't ask again. Emptied past heldMost.
    let held =
        Collections.Concurrent.ConcurrentDictionary<string, Neutral.ProfileMarkers>()

    let normal (number: string) =
        if isNull number then
            ""
        else
            number.Trim().ToUpperInvariant()

    let remember (profile: CompanyProfile) =
        if held.Count >= heldMost then
            held.Clear()

        held[profile.CompanyNumber] <- Neutral.markers profile

    /// An Available list as the company's profile leaves it, which apply gives from the profile's markers. The profile is read unless this
    /// registry has read it. Companies House answers an officer list or filing history for a number it doesn't hold with an empty list, as
    /// it did on 2026-11-08, so an empty list for a number with no profile is not available. A failed read of the profile fails the answer.
    let withProfile
        (number: string)
        (isEmpty: 'T -> bool)
        (apply: Neutral.ProfileMarkers -> 'T -> Availability<'T>)
        (ct: CancellationToken)
        (answer: Result<Evidence<Availability<'T>>, RegistryFailure>)
        =
        let applied (evidence: Evidence<Availability<'T>>) (found: Neutral.ProfileMarkers) (value: 'T) =
            match apply found value with
            | Available _ as whole -> Ok { evidence with Value = whole }
            | other ->
                Ok
                    { evidence with
                        Raw = ValueNone
                        Value = other
                    }

        task {
            match answer with
            | Ok({ Value = Available value } as evidence) ->
                match held.TryGetValue(normal number) with
                | true, found -> return applied evidence found value
                | _ ->
                    match! Api.Companies.getAsync client number ct with
                    | Ok(ValueSome read) ->
                        remember read
                        return applied evidence (Neutral.markers read) value
                    | Ok ValueNone when isEmpty value ->
                        return
                            Ok
                                { evidence with
                                    Raw = ValueNone
                                    Value = NotAvailable "Companies House holds no company with this number"
                                }
                    | Ok ValueNone -> return answer
                    | Error failure -> return Error(Neutral.failure (now ()) failure)
            | _ -> return answer
        }

    internal new(client) = CompaniesHouseRegistry(client, (fun () -> DateTimeOffset.UtcNow))

    // The officer search, then the first page of appointments of each officer whose name matched, asked at once. A failed appointments call
    // fails the search; a 404 leaves that officer unread and the result incomplete.
    interface IPersonSearch with
        member _.PersonSearch = Neutral.personSearch

        member _.SearchPeopleAsync(name: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            inCoverage jurisdiction (fun () ->
                match PersonName.tryParse name with
                | ValueNone ->
                    Threading.Tasks.Task.FromResult(
                        Error(
                            RegistryFailure.InvalidRequest
                                "A person search needs a name, as Firstname Lastname or Lastname, Firstname"
                        )
                    )
                | ValueSome asked ->
                    task {
                        match! Api.Search.officersAsync client (String.concat " " asked.Words) 0 officerPage ct with
                        | Error failure -> return Error(Neutral.failure (now ()) failure)
                        | Ok ValueNone ->
                            return
                                Ok(
                                    answered
                                        ValueNone
                                        (NotAvailable
                                            "The search answered 404, which isn't a statement that nothing matches")
                                )
                        | Ok(ValueSome found) ->
                            let named =
                                found.Items |> List.filter (fun item -> PersonName.matches asked item.Title)

                            let ids =
                                named
                                |> List.choose (fun item -> item.OfficerId |> ValueOption.toOption)
                                |> List.truncate officersRead

                            let! pages =
                                ids
                                |> List.map (fun pid -> Api.Appointments.pageAsync client pid 0 appointmentPage ct)
                                |> Threading.Tasks.Task.WhenAll

                            match
                                pages
                                |> Array.tryPick (fun page ->
                                    match page with
                                    | Error failure -> Some failure
                                    | Ok _ -> None)
                            with
                            | Some failure -> return Error(Neutral.failure (now ()) failure)
                            | None ->
                                let read =
                                    pages
                                    |> Array.choose (fun page ->
                                        match page with
                                        | Ok(ValueSome page) -> Some page
                                        | _ -> None)
                                    |> List.ofArray

                                return
                                    Ok(
                                        answered
                                            (rawList (found.ItemsRaw @ (read |> List.collect _.ItemsRaw)))
                                            (Available
                                                {
                                                    PersonSearchResult.Matches =
                                                        read |> List.collect _.Items |> List.map Neutral.personMatch
                                                    TotalMatches = ValueSome found.Total
                                                    Complete =
                                                        found.Items.Length >= found.Total
                                                        && ids.Length = named.Length
                                                        && read.Length = ids.Length
                                                        && read
                                                           |> List.forall (fun page -> page.Items.Length >= page.Total)
                                                })
                                    )
                    })

    interface ICompanyRegistry with
        member _.Coverage = Neutral.coverage

        member _.TryFindAsync(number: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            inCoverage jurisdiction (fun () ->
                task {
                    let! profile = Api.Companies.getAsync client number ct

                    let asked = normal number

                    return
                        profile
                        |> Result.bind (fun answer ->
                            match answer with
                            | ValueSome read when read.CompanyNumber <> asked ->
                                Error(
                                    InvalidResponse
                                        {
                                            HttpStatus = 200
                                            Reason = "The answer holds another company"
                                            Body = read.Raw
                                        }
                                )
                            | ValueSome read ->
                                remember read
                                Ok answer
                            | ValueNone -> Ok answer)
                        |> Result.map (fun answer ->
                            answered
                                (answer |> ValueOption.map _.Raw)
                                (Available(answer |> ValueOption.map Neutral.company)))
                        |> Result.mapError (Neutral.failure (now ()))
                })

        member _.SearchAsync(name: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            inCoverage jurisdiction (fun () ->
                task {
                    let! page = Api.Search.companiesAsync client name 0 searchSize ct

                    return
                        page
                        |> Result.map (fun answer ->
                            match answer with
                            | ValueSome page ->
                                answered
                                    (rawList page.ItemsRaw)
                                    (Available
                                        {
                                            Matches = page.Items |> List.map Neutral.companyMatch
                                            TotalMatches = ValueSome page.Total
                                            Complete = page.Items.Length >= page.Total
                                        })
                            | ValueNone ->
                                answered
                                    ValueNone
                                    (NotAvailable
                                        "The search answered 404, which isn't a statement that nothing matches"))
                        |> Result.mapError (Neutral.failure (now ()))
                })

        member _.OfficersAsync(number: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            inCoverage jurisdiction (fun () ->
                task {
                    let! officers = Api.Officers.allAsync client number ct

                    return!
                        officers
                        |> Result.map (fun answer ->
                            match answer with
                            | ValueSome officers ->
                                answered
                                    (rawList (officers |> List.map _.Raw))
                                    (Available(officers |> List.map Neutral.officer))
                            | ValueNone -> answered ValueNone (NotAvailable "No officer list is held for this number"))
                        |> Result.mapError (Neutral.failure (now ()))
                        |> withProfile number List.isEmpty Neutral.officersWith ct
                })

        member _.OwnersAsync(number: string, jurisdiction: Jurisdiction, _: OwnerPurpose, ct: CancellationToken) =
            inCoverage jurisdiction (fun () ->
                task {
                    match! Api.Psc.allAsync client number ct with
                    | Error failure -> return Error(Neutral.failure (now ()) failure)
                    | Ok pscs ->
                        let! statements = Api.PscStatements.allAsync client number ct

                        return!
                            statements
                            |> Result.map (fun statements ->
                                let listedPscs = pscs |> ValueOption.defaultValue []
                                let listedStatements = statements |> ValueOption.defaultValue []

                                answered
                                    (rawList ((listedPscs |> List.map _.Raw) @ (listedStatements |> List.map _.Raw)))
                                    (Neutral.owners listedPscs listedStatements))
                            |> Result.mapError (Neutral.failure (now ()))
                            |> withProfile
                                number
                                (fun _ -> false)
                                (fun found listing -> Available(Neutral.ownersWith found listing))
                                ct
                })

    interface IFilingRegister with
        member _.Coverage = Neutral.coverage

        member _.FilingsAsync(number: string, ct: CancellationToken) =
            task {
                let! filings = Api.FilingHistory.recentAsync client number [] ct

                return!
                    filings
                    |> Result.map (fun answer ->
                        match answer with
                        | ValueSome(items, total, complete, status) ->
                            answered
                                (rawList (items |> List.map _.Raw))
                                (Available
                                    {
                                        Filings = items |> List.map Neutral.filing
                                        Total = ValueSome total
                                        Complete = complete && Neutral.filingsStatedWhole status
                                    })
                        | ValueNone -> answered ValueNone (NotAvailable "No filing history is held for this number"))
                    |> Result.mapError (Neutral.failure (now ()))
                    |> withProfile
                        number
                        (fun history -> history.Filings.IsEmpty)
                        (fun found history ->
                            Available
                                { history with
                                    Complete = history.Complete && found.PartialData.IsNone
                                })
                        ct
            }

        member _.InsolvencyAsync(number: string, ct: CancellationToken) =
            task {
                let! insolvency = Api.Insolvency.getAsync client number ct

                return
                    insolvency
                    |> Result.map (fun answer ->
                        match answer with
                        | ValueSome record ->
                            answered (ValueSome record.Raw) (Available(record.Cases |> List.map Neutral.proceeding))
                        | ValueNone -> answered ValueNone (NotAvailable "No insolvency record is held at that address"))
                    |> Result.mapError (Neutral.failure (now ()))
            }

        member _.EstablishmentsAsync(number: string, ct: CancellationToken) =
            task {
                let! establishments = Api.UkEstablishments.getAsync client number ct

                return
                    establishments
                    |> Result.map (fun answer ->
                        match answer with
                        | ValueSome items ->
                            answered
                                (rawList (items |> List.map _.Raw))
                                (Available(
                                    items
                                    |> List.map (fun item ->
                                        {
                                            Name = item.CompanyName
                                            Number = item.CompanyNumber
                                            StatusAsPublished = item.Status
                                            Address = ValueNone
                                        })
                                ))
                        | ValueNone ->
                            answered ValueNone (NotAvailable "No UK establishment list is held at that address"))
                    |> Result.mapError (Neutral.failure (now ()))
            }

        member _.DocumentAsync(documentId: string, ct: CancellationToken) =
            task {
                let! content = Api.Documents.contentAsync client documentId "application/pdf" ct

                return
                    content
                    |> Result.map (
                        ValueOption.map (fun file ->
                            {
                                DocumentFile.ContentType = file.ContentType
                                Content = file.Content
                            })
                    )
                    |> Result.mapError (Neutral.failure (now ()))
            }
