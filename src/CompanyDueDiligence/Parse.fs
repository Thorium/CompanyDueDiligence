/// Companies House answers to records. A required field is one without which the record can't be identified; everything else may be absent.
/// A date that decides whether something has ended is refused if it can't be read, so that it can't read as no end.
module internal CompanyDueDiligence.CompaniesHouse.Parse

open System.Text.Json
open CompanyDueDiligence
open CompanyDueDiligence.Json

let private address (e: JsonElement) : Address =
    {
        Lines =
            [ "care_of"; "po_box"; "premises"; "address_line_1"; "address_line_2" ]
            |> List.choose (fun name -> tryString name e |> ValueOption.toOption)
        Locality = tryString "locality" e
        Region = tryString "region" e
        PostalCode = tryString "postal_code" e
        Country = tryString "country" e
    }

let private addressAt name e =
    tryObject name e |> ValueOption.map address

/// A month and year of birth.
let private birth (e: JsonElement) =
    match tryObject "date_of_birth" e with
    | ValueSome date ->
        match tryInt "year" date, tryInt "month" date with
        | ValueSome year, ValueSome month -> ValueSome { Year = year; Month = month }
        | _ -> ValueNone
    | ValueNone -> ValueNone

/// The registration block of a legal entity, which the entity's name belongs with.
let private identification (e: JsonElement) : EntityRegistration voption =
    tryObject "identification" e
    |> ValueOption.map (fun block ->
        {
            LegalName = tryString "name" e
            LegalForm = tryString "legal_form" block
            GoverningLaw = tryString "legal_authority" block
            Country = tryString "country_registered" block
            Register = tryString "place_registered" block
            RegistrationNumber = tryText "registration_number" block
        })

let private identityVerification (e: JsonElement) : IdentityVerification voption =
    tryObject "identity_verification_details" e
    |> ValueOption.map (fun block ->
        {
            VerifiedOn = dateIfAny "identity_verified_on" block
            VerifiedBy = tryString "authorised_corporate_service_provider_name" block
            VerifierSupervisors = strings block "anti_money_laundering_supervisory_bodies"
            StatementPossibleFrom = dateIfAny "appointment_verification_statement_date" block
            StatementDueOn = dateIfAny "appointment_verification_statement_due_on" block
            StatementSuppliedOn = dateIfAny "appointment_verification_start_on" block
            StatementRemovedOn = dateIfAny "appointment_verification_end_on" block
            PreferredName = tryString "preferred_name" block
        })

/// A list answer, with a total it can't do without. A missing list only reads as empty when the answer's own total is zero.
let page (item: JsonElement -> 'T) (e: JsonElement) : Page<'T> =
    let total =
        match tryInt "total_results" e |> ValueOption.orElse (tryInt "total_count" e) with
        | ValueSome total when total >= 0 -> total
        | _ -> raise (InvalidJson "The answer has no readable total")

    match tryProp "items" e with
    | ValueSome items when items.ValueKind = JsonValueKind.Array ->
        let elements = items.EnumerateArray() |> List.ofSeq

        {
            Items = elements |> List.map item
            Total = total
            Counts = ValueNone
            ItemsRaw = elements |> List.map _.GetRawText()
        }
    | ValueNone when total = 0 ->
        {
            Items = []
            Total = total
            Counts = ValueNone
            ItemsRaw = []
        }
    | _ -> raise (InvalidJson "The answer has no items list")

/// A list answer whose spec requires counts beside the total: officers, PSCs and PSC statements. inactive_count isn't in the spec, and is
/// added if present, so that officers counted that way still add up.
let countedPage (item: JsonElement -> 'T) (e: JsonElement) : Page<'T> =
    let answer = page item e

    let ended =
        match tryInt "resigned_count" e |> ValueOption.orElse (tryInt "ceased_count" e) with
        | ValueSome count -> count
        | ValueNone -> raise (InvalidJson "The answer has no readable count of ended items")

    { answer with
        Counts =
            ValueSome
                {
                    Active = required "active_count" tryInt e
                    Ended = ended
                    Inactive = tryInt "inactive_count" e |> ValueOption.defaultValue 0
                }
    }

let private previousName (e: JsonElement) : PreviousName =
    {
        Name = required "name" tryString e
        From = tryDate "effective_from" e
        To = tryDate "ceased_on" e
    }

/// True if either says so, false if those given say not, and ValueNone if neither is given.
let private eitherFlag (first: bool voption) (second: bool voption) =
    match first, second with
    | ValueSome true, _
    | _, ValueSome true -> ValueSome true
    | ValueNone, other
    | other, ValueNone -> other
    | _ -> ValueSome false

let companyProfile (e: JsonElement) : CompanyProfile =
    let accounts = tryObject "accounts" e

    let flagIn (name: string) block =
        block |> ValueOption.bind (tryFlag name)

    {
        CompanyNumber = required "company_number" tryString e
        CompanyName = required "company_name" tryString e
        Type = tryString "type" e
        Status = tryString "company_status" e
        StatusDetail = tryString "company_status_detail" e
        Jurisdiction = tryString "jurisdiction" e
        CreatedOn = tryDate "date_of_creation" e
        CeasedOn = dateIfAny "date_of_cessation" e
        HasInsolvencyHistory = tryFlag "has_insolvency_history" e
        HasBeenLiquidated = tryFlag "has_been_liquidated" e
        InsolvencyLink = tryObject "links" e |> ValueOption.bind (tryString "insolvency")
        HasCharges = tryFlag "has_charges" e
        RegisteredOfficeInDispute = tryFlag "registered_office_is_in_dispute" e
        UndeliverableRegisteredOffice = tryFlag "undeliverable_registered_office_address" e
        AccountsOverdue =
            eitherFlag
                (accounts |> ValueOption.bind (tryObject "next_accounts") |> flagIn "overdue")
                (accounts |> flagIn "overdue")
        ConfirmationStatementOverdue = tryObject "confirmation_statement" e |> flagIn "overdue"
        AnnualReturnOverdue = tryObject "annual_return" e |> flagIn "overdue"
        SicCodes = strings e "sic_codes"
        PreviousNames = array e "previous_company_names" |> List.map previousName
        RegisteredOffice = addressAt "registered_office_address" e
        ServiceAddress = addressAt "service_address" e
        Foreign =
            tryObject "foreign_company_details" e
            |> ValueOption.map (fun block ->
                let registry = tryObject "originating_registry" block

                {
                    RegistryName = registry |> ValueOption.bind (tryString "name")
                    RegistryCountry = registry |> ValueOption.bind (tryString "country")
                    RegistrationNumber = tryText "registration_number" block
                    GovernedBy = tryString "governed_by" block
                    LegalForm =
                        tryString "legal_form" block
                        |> ValueOption.orElse (tryString "company_type" block)
                })
        Branch =
            tryObject "branch_company_details" e
            |> ValueOption.map (fun block ->
                {
                    ParentNumber = tryText "parent_company_number" block
                    ParentName = tryString "parent_company_name" block
                })
        ExternalRegistrationNumber = tryText "external_registration_number" e
        Raw = e.GetRawText()
    }

let companySearchItem (e: JsonElement) : CompanySearchItem =
    {
        CompanyNumber = required "company_number" tryString e
        Title = required "title" tryString e
        Status = tryString "company_status" e
        Type = tryString "company_type" e
        CreatedOn = tryDate "date_of_creation" e
        Address = addressAt "address" e
        Raw = e.GetRawText()
    }

/// The id in an /officers/{id}/appointments link.
let private officerIdOf (link: string) =
    match link.Split('?', '#').[0].Trim('/').Split '/' with
    | [| "officers"; id; "appointments" |] when id <> "" -> ValueSome(System.Uri.UnescapeDataString id)
    | _ -> ValueNone

let officerSearchItem (e: JsonElement) : OfficerSearchItem =
    {
        Title = required "title" tryString e
        Description = tryString "description" e
        AppointmentCount = tryInt "appointment_count" e
        DateOfBirth = birth e
        Address = addressAt "address" e
        OfficerId =
            tryObject "links" e
            |> ValueOption.bind (tryString "self")
            |> ValueOption.bind officerIdOf
        Raw = e.GetRawText()
    }

let companyOfficer (e: JsonElement) : CompanyOfficer =
    let appointments =
        tryObject "links" e
        |> ValueOption.bind (tryObject "officer")
        |> ValueOption.bind (tryString "appointments")

    {
        Name = required "name" tryString e
        Role = tryString "officer_role" e
        AppointedOn = tryDate "appointed_on" e
        ResignedOn = dateIfAny "resigned_on" e
        DateOfBirth = birth e
        Nationality = tryString "nationality" e
        CountryOfResidence = tryString "country_of_residence" e
        Occupation = tryString "occupation" e
        Address = addressAt "address" e
        AppointmentsLink = appointments
        OfficerId = appointments |> ValueOption.bind officerIdOf
        Identification = identification e
        IdentityVerification = identityVerification e
        Raw = e.GetRawText()
    }

let officerAppointment (e: JsonElement) : OfficerAppointment =
    let company = required "appointed_to" tryObject e

    {
        Name = required "name" tryString e
        Role = tryString "officer_role" e
        AppointedOn = tryDate "appointed_on" e
        ResignedOn = dateIfAny "resigned_on" e
        CompanyNumber = required "company_number" tryString company
        CompanyName = tryString "company_name" company
        CompanyStatus = tryString "company_status" company
        Nationality = tryString "nationality" e
        CountryOfResidence = tryString "country_of_residence" e
        Occupation = tryString "occupation" e
        Address = addressAt "address" e
        Identification = identification e
        IdentityVerification = identityVerification e
        Raw = e.GetRawText()
    }

let private ukEstablishment (e: JsonElement) : UkEstablishment =
    {
        CompanyNumber = required "company_number" tryString e
        CompanyName = required "company_name" tryString e
        Status = tryString "company_status" e
        Locality = tryString "locality" e
        Raw = e.GetRawText()
    }

/// The spec gives this list no paging and no total. A total the answer holds all the same must match its items.
let ukEstablishments (e: JsonElement) : UkEstablishment list =
    match tryProp "items" e with
    | ValueSome items when items.ValueKind = JsonValueKind.Array ->
        let read = items.EnumerateArray() |> Seq.map ukEstablishment |> List.ofSeq

        match tryInt "total_results" e |> ValueOption.orElse (tryInt "total_count" e) with
        | ValueSome total when total <> read.Length -> raise (InvalidJson "The answer's total differs from its items")
        | _ -> read
    | _ -> raise (InvalidJson "The answer has no items list")

/// Which details call a /disqualified-officers/{kind}/{id} link is for.
let private disqualifiedIdOf (link: string) =
    match link.Split('?', '#').[0].Trim('/').Split '/' with
    | [| "disqualified-officers"; "natural"; id |] when id <> "" ->
        ValueSome(DisqualifiedOfficerId.Natural(System.Uri.UnescapeDataString id))
    | [| "disqualified-officers"; "corporate"; id |] when id <> "" ->
        ValueSome(DisqualifiedOfficerId.Corporate(System.Uri.UnescapeDataString id))
    | _ -> ValueNone

let disqualifiedOfficerSearchItem (e: JsonElement) : DisqualifiedOfficerSearchItem =
    {
        Title = required "title" tryString e
        Description = tryString "description" e
        DateOfBirth = tryDate "date_of_birth" e
        Address = addressAt "address" e
        OfficerId =
            tryObject "links" e
            |> ValueOption.bind (tryString "self")
            |> ValueOption.bind disqualifiedIdOf
        Raw = e.GetRawText()
    }

let private disqualification (e: JsonElement) : Disqualification =
    let reason = tryObject "reason" e

    {
        CaseIdentifier = tryString "case_identifier" e
        Type = tryString "disqualification_type" e
        From = dateIfAny "disqualified_from" e
        Until = dateIfAny "disqualified_until" e
        HeardOn = tryDate "heard_on" e
        UndertakenOn = tryDate "undertaken_on" e
        CourtName = tryString "court_name" e
        CompanyNames = strings e "company_names"
        Reason = reason |> ValueOption.bind (tryString "description_identifier")
        Act = reason |> ValueOption.bind (tryString "act")
        Address = addressAt "address" e
    }

let private permissionToAct (e: JsonElement) : PermissionToAct =
    {
        CompanyNames = strings e "company_names"
        CourtName = tryString "court_name" e
        GrantedOn = tryDate "granted_on" e
        ExpiresOn = dateIfAny "expires_on" e
    }

/// An answer without its disqualifications says nothing about the officer, so it isn't read as none.
let disqualifiedOfficer (e: JsonElement) : DisqualifiedOfficer =
    let natural =
        [ "title"; "forename"; "other_forenames"; "surname"; "honours" ]
        |> List.choose (fun name -> tryString name e |> ValueOption.toOption)

    let name =
        match tryString "name" e, natural with
        | ValueSome name, _ -> name
        | ValueNone, [] -> raise (InvalidJson "The answer has no name")
        | ValueNone, parts -> String.concat " " parts

    match tryProp "disqualifications" e with
    | ValueSome list when list.ValueKind = JsonValueKind.Array ->
        {
            Name = name
            DateOfBirth = tryDate "date_of_birth" e
            Nationality = tryString "nationality" e
            CompanyNumber = tryString "company_number" e
            CountryOfRegistration = tryString "country_of_registration" e
            PersonNumber = tryText "person_number" e
            Disqualifications = list.EnumerateArray() |> Seq.map disqualification |> List.ofSeq
            PermissionsToAct = array e "permissions_to_act" |> List.map permissionToAct
            Raw = e.GetRawText()
        }
    | _ -> raise (InvalidJson "The answer has no disqualifications list")

/// An item with neither a kind nor a name says nothing about who holds control, so it isn't read as an owner.
let psc (e: JsonElement) : Psc =
    let kind = tryString "kind" e
    let name = tryString "name" e

    if kind.IsNone && name.IsNone then
        raise (InvalidJson "A person with significant control has neither a kind nor a name")

    {
        Kind = kind
        Name = name
        NotifiedOn = tryDate "notified_on" e
        CeasedOn = dateIfAny "ceased_on" e
        Ceased = flag "ceased" e
        NaturesOfControl = strings e "natures_of_control"
        DateOfBirth = birth e
        Nationality = tryString "nationality" e
        CountryOfResidence = tryString "country_of_residence" e
        Address = addressAt "address" e
        Identification = identification e
        IdentityVerification = identityVerification e
        Raw = e.GetRawText()
    }

let pscStatement (e: JsonElement) : PscStatement =
    {
        Statement = required "statement" tryString e
        NotifiedOn = tryDate "notified_on" e
        CeasedOn = dateIfAny "ceased_on" e
        LinkedPscName = tryString "linked_psc_name" e
        Raw = e.GetRawText()
    }

/// The last path segment of a /document/{id} link, on whatever host the link names.
let private documentIdOf (link: string) =
    match link.Split('?', '#').[0].TrimEnd('/').Split('/') |> Array.rev |> List.ofArray with
    | id :: "document" :: _ when id <> "" -> ValueSome(System.Uri.UnescapeDataString id)
    | _ -> ValueNone

let filingHistoryItem (e: JsonElement) : FilingHistoryItem =
    {
        TransactionId = required "transaction_id" tryString e
        Category = tryString "category" e
        SubCategory = tryString "subcategory" e
        Type = tryString "type" e
        Date = tryDate "date" e
        Description = tryString "description" e
        Pages = tryInt "pages" e
        DocumentId =
            tryObject "links" e
            |> ValueOption.bind (tryString "document_metadata")
            |> ValueOption.bind documentIdOf
        DocumentLink = tryObject "links" e |> ValueOption.bind (tryString "document_metadata")
        Capital =
            match tryObject "description_values" e with
            | ValueNone -> []
            | ValueSome values ->
                array values "capital"
                |> List.choose (fun item ->
                    match tryString "currency" item, tryText "figure" item with
                    | ValueSome currency, ValueSome figure -> Some(currency, figure)
                    | _ -> None)
        Raw = e.GetRawText()
    }

let private description name e =
    oneOrMany name e
    |> List.choose (fun v -> tryString "description" v |> ValueOption.toOption)

let charge (e: JsonElement) : Charge =
    {
        Id = tryString "id" e
        ChargeCode = tryString "charge_code" e
        ChargeNumber = tryInt "charge_number" e
        Status = tryString "status" e
        Classification = description "classification" e |> List.tryHead |> ValueOption.ofOption
        CreatedOn = tryDate "created_on" e
        DeliveredOn = tryDate "delivered_on" e
        SatisfiedOn = tryDate "satisfied_on" e
        PersonsEntitled =
            array e "persons_entitled"
            |> List.choose (fun v -> tryString "name" v |> ValueOption.toOption)
        Particulars = description "particulars" e
        Raw = e.GetRawText()
    }

let private insolvencyDate (e: JsonElement) : InsolvencyDate =
    {
        Type = tryString "type" e
        Date = tryDate "date" e
    }

let private practitioner (e: JsonElement) : InsolvencyPractitioner =
    {
        Name = tryString "name" e
        Role = tryString "role" e
        AppointedOn = tryDate "appointed_on" e
        CeasedToActOn = tryDate "ceased_to_act_on" e
        Address =
            oneOrMany "address" e
            |> List.tryHead
            |> ValueOption.ofOption
            |> ValueOption.map address
    }

let private insolvencyCase (e: JsonElement) : InsolvencyCase =
    {
        Type = tryString "type" e
        Number = tryText "number" e
        Dates = array e "dates" |> List.map insolvencyDate
        Practitioners = array e "practitioners" |> List.map practitioner
        Notes = strings e "notes"
    }

/// An answer without its cases says nothing about the company, so it isn't read as none.
let insolvency (e: JsonElement) : Insolvency =
    match tryProp "cases" e with
    | ValueSome cases when cases.ValueKind = JsonValueKind.Array ->
        {
            Status = tryString "status" e
            Cases = cases.EnumerateArray() |> Seq.map insolvencyCase |> List.ofSeq
            Raw = e.GetRawText()
        }
    | _ -> raise (InvalidJson "The answer has no cases list")

let documentMetadata (e: JsonElement) : DocumentMetadata =
    {
        Id = tryString "id" e
        Pages = tryInt "pages" e
        CreatedAt = tryTimestamp "created_at" e
        ContentTypes =
            match tryObject "resources" e with
            | ValueSome resources -> resources.EnumerateObject() |> Seq.map _.Name |> List.ofSeq
            | ValueNone -> []
        Raw = e.GetRawText()
    }
