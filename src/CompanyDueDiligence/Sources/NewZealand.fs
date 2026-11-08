namespace CompanyDueDiligence

open System
open System.Text.Json
open System.Text.RegularExpressions
open System.Threading
open System.Threading.Tasks
open CompanyDueDiligence.Json

/// MBIE's NZBN API v5, written from its OpenAPI definition on portal.api.business.govt.nz and the "NZBN reference data - March 2023" code lists,
/// as read on 2026-11-08. Not run live: the API needs a subscription key.
module internal NewZealand =

    let coverage: RegistryCoverage =
        {
            Registry = "NZBN register (New Zealand)"
            Jurisdictions = [ Jurisdiction.ofCode "NZ" ]
            Standing = Support.Supported
            Concerns =
                Support.Partial
                    "Insolvencies listed on a company's record are stated; overdue filings and disputes aren't published"
            Addresses = Support.Supported
            Search = Support.Partial "One page of up to 50, across current and former names, trading names and numbers"
            Officers =
                Support.Partial
                    "Roles as published: directors of companies, partners and trustees; limited partners aren't public"
            Owners =
                Support.Partial
                    "Shareholders on the Companies Register, who are legal owners and not necessarily beneficial ones; a company with extensive shareholding lists only its largest parcels"
            Cost =
                AccessCost.FreeWithKey
                    "Subscribe to the NZBN API on portal.api.business.govt.nz, which charges no fee; the key goes in the Ocp-Apim-Subscription-Key header"
            RateLimit = RateLimitPolicy.NotPublished(2, TimeSpan.FromSeconds 1.)
            Licence =
                "New Zealand Crown copyright, reusable with credit to api.business.govt.nz, per the portal's copyright page. Not run live"
        }

    /// A number as the adapter reads it.
    [<RequireQualifiedAccess>]
    type Number =
        | Nzbn of string
        /// A Companies Office company number, found through the search.
        | CompanyNumber of string

    let private nzbnRegex = Regex @"\A[0-9]{13}\z"
    let private companyNumberRegex = Regex @"\A[0-9]{1,8}\z"

    /// GS1's check, which an NZBN's last digit is.
    let gs1 (digits: string) =
        let body = digits.Substring(0, digits.Length - 1)

        let total =
            body
            |> Seq.rev
            |> Seq.mapi (fun index digit -> (int digit - int '0') * (if index % 2 = 0 then 3 else 1))
            |> Seq.sum

        (10 - total % 10) % 10 = int digits[digits.Length - 1] - int '0'

    /// Thirteen digits with a valid GS1 check digit is an NZBN; up to eight digits is a company number.
    let numberOf (number: string) =
        let normal = if isNull number then "" else number.Trim().Replace(" ", "")

        if nzbnRegex.IsMatch normal then
            if gs1 normal then
                ValueSome(Number.Nzbn normal)
            else
                ValueNone
        elif companyNumberRegex.IsMatch normal then
            match normal.TrimStart '0' with
            | "" -> ValueNone
            | digits -> ValueSome(Number.CompanyNumber digits)
        else
            ValueNone

    /// The entityStatusCode list of the reference data, all 17 codes. removalCommenced on a registered company is a concern. Any other code is
    /// Unknown.
    let standing (code: string voption) (description: string voption) (removalCommenced: bool) =
        let word = description |> ValueOption.defaultValue ""

        match code |> ValueOption.map _.Trim() with
        | ValueSome "50" when removalCommenced -> CompanyStanding.ActiveWithConcern "Removal commenced"
        | ValueSome "50" -> CompanyStanding.Registered(ValueSome "Registered")
        | ValueSome("55" | "56" | "57" | "60" | "61" | "63" | "64" | "65" | "66" | "70" | "71" | "72" as held) ->
            CompanyStanding.InInsolvency(if word = "" then held else word)
        | ValueSome("62" | "80" | "91" as held) -> CompanyStanding.Closed(if word = "" then held else word)
        | ValueSome "90" -> CompanyStanding.Registered(ValueSome "Inactive")
        | ValueSome "" -> CompanyStanding.Unknown ValueNone
        | ValueSome other -> CompanyStanding.Unknown(ValueSome other)
        | ValueNone -> CompanyStanding.Unknown description

    /// Entity types on the Overseas Companies Register and overseas limited partnerships, per the reference data.
    let private foreignTypes = set [ "ASIC"; "NON_ASIC"; "Z" ]

    /// Entity types on the Companies Register, which a company number belongs to.
    let companyTypes = set [ "LTD"; "ULTD"; "COOP"; "ASIC"; "NON_ASIC" ]

    let private isCurrent (e: JsonElement) =
        (SourceAnswer.dateOfTimestamp "endDate" e).IsNone

    let private addressOf (e: JsonElement) : Address =
        {
            Lines =
                [ "careOf"; "address1"; "address2"; "address3"; "address4" ]
                |> List.choose (fun field -> SourceAnswer.trimmed field e |> ValueOption.toOption)
            Locality = ValueNone
            Region = ValueNone
            PostalCode = SourceAnswer.trimmed "postCode" e
            Country = SourceAnswer.trimmed "countryCode" e
        }

    /// The address mapping sheet: REGISTERED is the registered office, or an overseas company's principal place of business in New Zealand.
    let private addressKind (overseas: bool) (addressType: string voption) =
        match addressType with
        | ValueSome "REGISTERED" when overseas -> AddressKind.PrincipalOffice
        | ValueSome "REGISTERED" -> AddressKind.RegisteredOffice
        | ValueSome "SERVICE" -> AddressKind.Service
        | ValueSome "POSTAL" -> AddressKind.Postal
        | ValueSome "OFFICE" -> AddressKind.PrincipalOffice
        | ValueSome other -> AddressKind.Other other
        | ValueNone -> AddressKind.Other "Address on the NZBN register"

    let private companyDetails (e: JsonElement) = tryObject "company-details" e

    let company (e: JsonElement) : Company =
        let entityType = SourceAnswer.trimmed "entityTypeCode" e
        let overseas = entityType |> ValueOption.exists foreignTypes.Contains
        let details = companyDetails e
        let code = tryText "entityStatusCode" e
        let description = SourceAnswer.trimmed "entityStatusDescription" e

        let removal =
            details
            |> ValueOption.bind (tryFlag "removalCommenced")
            |> ValueOption.defaultValue false

        let insolvencies =
            details
            |> ValueOption.map (fun block -> array block "insolvencies")
            |> ValueOption.defaultValue []

        let name = required "entityName" tryString e

        let addresses =
            tryObject "addresses" e
            |> ValueOption.map (fun block -> array block "addressList")
            |> ValueOption.defaultValue []
            |> List.filter isCurrent
            |> List.map (fun held ->
                {
                    Kind = addressKind overseas (SourceAnswer.trimmed "addressType" held)
                    Address = addressOf held
                })

        {
            Name = name
            OtherNames =
                array e "tradingNames"
                |> List.choose (fun held -> SourceAnswer.trimmed "name" held |> ValueOption.toOption)
                |> List.filter ((<>) name)
                |> List.distinct
            Number = required "nzbn" tryText e
            Jurisdiction = Jurisdiction.ofCode "NZ"
            JurisdictionAsPublished = ValueNone
            Standing = standing code description removal
            Concerns =
                match insolvencies with
                | [] -> CompanyConcerns.notPublished
                | _ ->
                    {
                        Stated = [ CompanyConcern.InsolvencyHistory ]
                        Unconfirmed = CompanyConcern.all |> List.except [ CompanyConcern.InsolvencyHistory ]
                    }
            StatusAsPublished = description |> ValueOption.orElse code
            LegalForm = SourceAnswer.trimmed "entityTypeDescription" e
            IncorporatedOn = SourceAnswer.dateOfTimestamp "registrationDate" e
            CeasedOn = ValueNone
            Addresses =
                match addresses with
                | [] -> NotAvailable "The answer holds no current address"
                | _ -> Available addresses
            Role =
                match entityType with
                | ValueSome _ when overseas -> RecordRole.ForeignRegistration
                | ValueSome _ -> RecordRole.Home
                | ValueNone -> RecordRole.NotStated
            HomeRegistration =
                if overseas then
                    ValueSome
                        {
                            LegalName = ValueSome name
                            LegalForm = ValueNone
                            GoverningLaw = ValueNone
                            Country = details |> ValueOption.bind (SourceAnswer.trimmed "countryOfOrigin")
                            Register = ValueNone
                            RegistrationNumber =
                                details |> ValueOption.bind (SourceAnswer.trimmed "australianCompanyNumber")
                        }
                else
                    ValueNone
            EstablishmentOf = ValueNone
        }

    let private personName (e: JsonElement) =
        match SourceAnswer.trimmed "fullName" e with
        | ValueSome full -> full
        | ValueNone ->
            [ "firstName"; "middleNames"; "lastName" ]
            |> List.choose (fun field -> SourceAnswer.trimmed field e |> ValueOption.toOption)
            |> String.concat " "

    /// NZ for an entity the answer gives an NZBN or company number for, which are numbers on New Zealand's registers, so that it can be followed.
    let private onRegister (number: string voption) =
        number |> ValueOption.map (fun _ -> "NZ")

    let private role (e: JsonElement) : Officer =
        let person =
            tryObject "rolePerson" e
            |> ValueOption.map personName
            |> ValueOption.filter ((<>) "")

        let entity = tryObject "roleEntity" e
        let ended = SourceAnswer.dateOfTimestamp "endDate" e

        let status =
            SourceAnswer.trimmed "roleStatus" e |> ValueOption.map _.ToUpperInvariant()

        {
            Name =
                match person, entity |> ValueOption.bind (SourceAnswer.trimmed "entityName") with
                | ValueSome name, _ -> name
                | ValueNone, ValueSome name -> name
                | ValueNone, ValueNone -> PartyNames.NotGiven
            Kind =
                match person, entity with
                | ValueSome _, _ -> PartyKind.Individual
                | ValueNone, ValueSome _ -> PartyKind.LegalEntity
                | ValueNone, ValueNone -> PartyKind.Unknown ValueNone
            Role =
                match SourceAnswer.trimmed "roleType" e with
                | ValueSome "Director" -> OfficerRole.Director
                | ValueSome other -> OfficerRole.Other other
                | ValueNone -> OfficerRole.Other ""
            Current = ended.IsNone && status <> ValueSome "INACTIVE"
            AppointedOn = SourceAnswer.dateOfTimestamp "startDate" e
            EndedOn = ended
            BornIn = ValueNone
            Nationality = ValueNone
            CountryOfResidence = ValueNone
            Address = ValueNone
            Entity =
                match person, entity with
                | ValueNone, ValueSome held ->
                    let nzbn = SourceAnswer.trimmed "nzbn" held

                    ValueSome
                        {
                            LegalName = SourceAnswer.trimmed "entityName" held
                            LegalForm = ValueNone
                            GoverningLaw = ValueNone
                            Country = onRegister nzbn
                            Register = ValueNone
                            RegistrationNumber = nzbn
                        }
                | _ -> ValueNone
            IdentityVerification = ValueNone
        }

    let officers (e: JsonElement) =
        match array e "roles" with
        | [] -> NotAvailable "The register lists no roles for this entity, which doesn't say it has none"
        | items -> Available(items |> List.map role)

    /// One shareholder of a parcel. individual and other are the two kinds the definition gives.
    let private shareholder
        (control: NatureOfControl list)
        (current: bool)
        (individual: JsonElement voption)
        (other: JsonElement voption)
        (since: DateOnly voption)
        (ended: DateOnly voption)
        : Owner =
        let personal =
            individual |> ValueOption.map personName |> ValueOption.filter ((<>) "")

        let entityNumber =
            other
            |> ValueOption.bind (fun held ->
                SourceAnswer.trimmed "nzbn" held
                |> ValueOption.orElse (SourceAnswer.trimmed "companyNumber" held))

        {
            Name =
                match personal with
                | ValueSome name -> ValueSome name
                | ValueNone -> other |> ValueOption.bind (SourceAnswer.trimmed "currentEntityName")
            Kind =
                match personal, other with
                | ValueSome _, _ -> PartyKind.Individual
                | ValueNone, ValueSome _ -> PartyKind.LegalEntity
                | ValueNone, ValueNone -> PartyKind.Unknown ValueNone
            Control = control
            Current = current
            Since = since
            EndedOn = ended
            BornIn = ValueNone
            Nationality = ValueNone
            CountryOfResidence = ValueNone
            Address = ValueNone
            Entity =
                match personal, other with
                | ValueNone, ValueSome held ->
                    ValueSome
                        {
                            LegalName = SourceAnswer.trimmed "currentEntityName" held
                            LegalForm = SourceAnswer.trimmed "entityType" held
                            GoverningLaw = ValueNone
                            Country = onRegister entityNumber
                            Register = ValueNone
                            RegistrationNumber = entityNumber
                        }
                | _ -> ValueNone
            IdentityVerification = ValueNone
        }

    let private parcel (total: int64 voption) (allocation: int64 voption) holders : NatureOfControl list =
        let shared = if holders > 1 then $", held jointly by {holders}" else ""

        match allocation, total with
        | ValueSome shares, ValueSome all when all > 0L ->
            let low = int (shares * 100L / all)
            let high = int ((shares * 100L + all - 1L) / all)

            [
                {
                    Kind = ControlKind.Ownership
                    Band = ValueSome { From = low; To = high }
                    AsPublished = $"{shares} of {all} shares{shared}"
                }
            ]
        | ValueSome shares, _ ->
            [
                {
                    Kind = ControlKind.Ownership
                    Band = ValueNone
                    AsPublished = $"{shares} shares{shared}"
                }
            ]
        | ValueNone, _ -> []

    let private tryInt64 name e =
        tryText name e
        |> ValueOption.bind (fun text ->
            match
                Int64.TryParse(text, Globalization.NumberStyles.Integer, Globalization.CultureInfo.InvariantCulture)
            with
            | true, value -> ValueSome value
            | _ -> ValueNone)

    /// Current shareholders by parcel, and former ones. Extensive shareholding lists only the largest parcels, which is a gap. A shareholding with
    /// no current shareholder is NotAvailable, as the register states no company without one.
    let owners (e: JsonElement) =
        match companyDetails e |> ValueOption.bind (tryObject "shareholding") with
        | ValueNone ->
            NotAvailable "The record holds no shareholding: the register lists shareholders for companies only"
        | ValueSome shareholding ->
            let total = tryInt64 "numberOfShares" shareholding

            let current =
                array shareholding "shareAllocation"
                |> List.collect (fun allocation ->
                    let holders = array allocation "shareholder"
                    let control = parcel total (tryInt64 "allocation" allocation) holders.Length

                    holders
                    |> List.map (fun holder ->
                        let ended = SourceAnswer.dateOfTimestamp "vacationDate" holder

                        shareholder
                            control
                            ended.IsNone
                            (tryObject "individualShareholder" holder)
                            (tryObject "otherShareholder" holder)
                            (SourceAnswer.dateOfTimestamp "appointmentDate" holder)
                            ended))

            let former =
                array shareholding "historicShareholder"
                |> List.map (fun holder ->
                    shareholder
                        []
                        false
                        (tryObject "historicIndividualShareholder" holder)
                        (tryObject "historicOtherShareholder" holder)
                        (SourceAnswer.dateOfTimestamp "appointmentDate" holder)
                        (SourceAnswer.dateOfTimestamp "vacationDate" holder))

            let all = current @ former
            let live = all |> List.filter _.Current

            let extensive =
                companyDetails e |> ValueOption.bind (tryFlag "extensiveShareholding") = ValueSome true

            let gaps =
                [
                    if extensive then
                        OwnershipGap.OwnerNotIdentified
                    if
                        live
                        |> List.exists (fun owner -> owner.Kind = PartyKind.Unknown ValueNone || owner.Name.IsNone)
                    then
                        OwnershipGap.OwnerUnreadable
                    if
                        live
                        |> List.exists (fun owner ->
                            owner.Kind = PartyKind.LegalEntity
                            && (owner.Entity |> ValueOption.bind _.RegistrationNumber).IsNone)
                    then
                        OwnershipGap.EntityNotIdentified
                ]

            if List.isEmpty live then
                NotAvailable "The shareholding lists no current shareholder, though every company has one"
            else
                Available
                    {
                        Owners = all
                        Statements = []
                        Gaps = gaps
                    }

    let private searchItems (root: JsonElement) =
        match tryProp "items" root with
        | ValueSome list when list.ValueKind = JsonValueKind.Array -> list.EnumerateArray() |> List.ofSeq
        | _ -> raise (InvalidJson "The answer has no items list")

    /// The NZBN of the one company on the Companies Register that the search answer holds under this company number.
    let nzbnOfCompanyNumber (companyNumber: string) (root: JsonElement) =
        searchItems root
        |> List.filter (fun item ->
            (SourceAnswer.trimmed "sourceRegisterUniqueId" item
             |> ValueOption.map (fun held -> held.TrimStart '0'))
                =
                ValueSome companyNumber
            && (SourceAnswer.trimmed "entityTypeCode" item
                |> ValueOption.exists companyTypes.Contains))
        |> List.choose (fun item -> tryText "nzbn" item |> ValueOption.toOption)
        |> List.distinct

    /// True for the register's own not-found answer, errorCode NZBN-404. A 404 without it, such as a gateway's, says nothing of the entity.
    let isNotFound (body: string) =
        try
            use document = JsonDocument.Parse body
            SourceAnswer.trimmed "errorCode" document.RootElement = ValueSome "NZBN-404"
        with :? JsonException ->
            false

    let search (root: JsonElement) =
        let matches =
            searchItems root
            |> List.map (fun item ->
                {
                    Name = required "entityName" tryString item
                    Number = required "nzbn" tryText item
                    StatusAsPublished = SourceAnswer.trimmed "entityStatusDescription" item
                    Address = ValueNone
                })

        let total = tryInt "totalItems" root

        {
            Matches = matches
            TotalMatches = total
            Complete = (total = ValueSome matches.Length)
        }

/// The NZBN register as an ICompanyRegistry. A 404 for a well-formed NZBN, with the register's NZBN-404 code, reads as not held. A company
/// number is looked up through the search first, and only a single company on the Companies Register holding it is taken.
[<Sealed>]
type internal NewZealandRegistry(connection: SourceConnection, key: SourceKey) =
    static let searchSize = 50

    let client =
        SourceClient(connection, SourceAnswer.pacing 2 (TimeSpan.FromSeconds 1.))

    let coverage = NewZealand.coverage
    let baseUri = "https://api.business.govt.nz/gateway/nzbn/v5"

    let headers =
        [ "Ocp-Apim-Subscription-Key", key.Value; "Accept", "application/json" ]

    let searchUri (term: string) =
        Uri $"{baseUri}/entities?search-term={Uri.EscapeDataString term}&page-size={searchSize}"

    /// The NZBN to read: as given, or the one the search finds for a company number. Ok(Error) is the search's answer and why it settles nothing.
    let resolve (asked: NewZealand.Number) (ct: CancellationToken) =
        task {
            match asked with
            | NewZealand.Number.Nzbn nzbn -> return Ok(Ok nzbn)
            | NewZealand.Number.CompanyNumber companyNumber ->
                match! client.GetAsync(searchUri companyNumber, headers, ct) with
                | Error failure -> return Error failure
                | Ok reply when reply.Status = 404 -> return Error(RegistryFailure.RequestRefused "HTTP 404")
                | Ok reply ->
                    match SourceAnswer.json reply (NewZealand.nzbnOfCompanyNumber companyNumber) with
                    | Error failure -> return Error failure
                    | Ok [ nzbn ] -> return Ok(Ok nzbn)
                    | Ok [] ->
                        return
                            Ok(
                                Error(
                                    reply,
                                    "The search found no company on the Companies Register with this number, which proves nothing: look up the NZBN"
                                )
                            )
                    | Ok several ->
                        return
                            Ok(
                                Error(
                                    reply,
                                    $"{several.Length} companies carry this number in the search, so it can't tell which is meant"
                                )
                            )
        }

    /// The entity record for number, read by read. A 404 with the register's not-found code answers notHeld.
    let readEntity
        (number: string)
        (notHeld: Availability<'T>)
        (read: string -> JsonElement -> Availability<'T>)
        (ct: CancellationToken)
        : Task<Result<Evidence<Availability<'T>>, RegistryFailure>> =
        match NewZealand.numberOf number with
        | ValueNone ->
            Task.FromResult(
                Error(
                    RegistryFailure.InvalidRequest
                        "An NZBN has thirteen digits with a valid check digit, and a company number up to eight digits"
                )
            )
        | ValueSome asked ->
            task {
                match! resolve asked ct with
                | Error failure -> return Error failure
                | Ok(Error(reply, reason)) ->
                    return Ok(SourceAnswer.evidence coverage reply ValueNone (NotAvailable reason))
                | Ok(Ok nzbn) ->
                    match! client.GetAsync(Uri $"{baseUri}/entities/{nzbn}", headers, ct) with
                    | Error failure -> return Error failure
                    | Ok reply when reply.Status = 404 && NewZealand.isNotFound reply.Body ->
                        return
                            Ok
                                { SourceAnswer.evidence coverage reply ValueNone notHeld with
                                    Raw = ValueNone
                                }
                    | Ok reply when reply.Status = 404 ->
                        return Error(RegistryFailure.RequestRefused "HTTP 404 without the register's not-found code")
                    | Ok reply ->
                        return
                            SourceAnswer.json reply (fun root ->
                                read nzbn root, SourceAnswer.dateOfTimestamp "lastUpdatedDate" root)
                            |> Result.map (fun (value, asOf) -> SourceAnswer.evidence coverage reply asOf value)
            }

    let found (nzbn: string) (root: JsonElement) =
        let company = NewZealand.company root

        if company.Number = nzbn then
            Available(ValueSome company)
        else
            raise (InvalidJson "The answer holds another entity")

    /// read, once the answer is the entity asked for.
    let ofEntity (read: JsonElement -> Availability<'T>) (nzbn: string) (root: JsonElement) =
        if (NewZealand.company root).Number = nzbn then
            read root
        else
            raise (InvalidJson "The answer holds another entity")

    interface ICompanyRegistry with
        member _.Coverage = coverage

        member _.TryFindAsync(number: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () -> readEntity number (Available ValueNone) found ct)

        member _.SearchAsync(name: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                if String.IsNullOrWhiteSpace name then
                    Task.FromResult(Error(RegistryFailure.InvalidRequest "A search needs a term"))
                else
                    task {
                        match! client.GetAsync(searchUri (name.Trim()), headers, ct) with
                        | Error failure -> return Error failure
                        | Ok reply when reply.Status = 404 -> return Error(RegistryFailure.RequestRefused "HTTP 404")
                        | Ok reply ->
                            return
                                SourceAnswer.json reply NewZealand.search
                                |> Result.map (fun result ->
                                    SourceAnswer.evidence coverage reply ValueNone (Available result))
                    })

        member _.OfficersAsync(number: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                readEntity
                    number
                    (NotAvailable "The register holds no entity at this NZBN")
                    (ofEntity NewZealand.officers)
                    ct)

        member _.OwnersAsync(number: string, jurisdiction: Jurisdiction, _: OwnerPurpose, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                readEntity
                    number
                    (NotAvailable "The register holds no entity at this NZBN")
                    (ofEntity NewZealand.owners)
                    ct)
