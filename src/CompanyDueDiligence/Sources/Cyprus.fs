namespace CompanyDueDiligence

open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions
open System.Threading
open System.Threading.Tasks

/// The Registrar of Companies' register on data.gov.cy (dataset 1026): organisations, registered offices and officials, monthly CSVs, read as
/// published on 2026-11-02.
module internal Cyprus =

    [<Literal>]
    let Registry = "Registrar of Companies open data (Cyprus)"

    let coverage (offices: bool) (officials: bool) : RegistryCoverage =
        {
            Registry = Registry
            Jurisdictions = [ Jurisdiction.ofCode "CY" ]
            Standing = Support.Supported
            Concerns = Support.Partial "Liquidation and administration are stated; overdue filings aren't in the file"
            Addresses =
                if offices then
                    Support.Supported
                else
                    Support.NotOffered "The host passed no registered office file"
            Search = Support.Partial "By name within the file, up to 50 matches"
            Officers =
                if officials then
                    Support.Partial
                        "Officials as the file lists them, with no dates, so whether they still serve isn't stated"
                else
                    Support.NotOffered "The host passed no officials file"
            Owners = Support.NotOffered "The open data holds no shareholders or beneficial owners"
            Cost = AccessCost.Free
            RateLimit = RateLimitPolicy.NotPublished(1, TimeSpan.FromDays 30.)
            Licence = "CC BY 4.0, as the data.gov.cy dataset page states"
        }

    /// The file's organisation type codes and the prefix a number is cited with. HE and AE are the prefixes companies and overseas companies are
    /// cited with; business names (B), partnerships (P) and old partnerships (N) keep the file's letter.
    let private prefixOf (code: string) =
        match code with
        | "C" -> ValueSome "HE"
        | "O" -> ValueSome "AE"
        | "B" -> ValueSome "B"
        | "P" -> ValueSome "P"
        | "N" -> ValueSome "N"
        | _ -> ValueNone

    let private numberRegex = Regex @"\A(HE|ΗΕ|C|AE|ΑΕ|O|B|P|N)\s*([0-9]{1,7})\z"

    /// A number with its type: HE 12345 (or ΗΕ, or the file's C), AE for overseas companies (or ΑΕ, O), B, P or N. A bare number can belong to
    /// several types, so it isn't one.
    let normalNumber (number: string) =
        let normal =
            if isNull number then
                ""
            else
                number.Trim().ToUpperInvariant()

        let found = numberRegex.Match normal

        if found.Success then
            let prefix =
                match found.Groups[1].Value with
                | "HE"
                | "ΗΕ"
                | "C" -> "HE"
                | "AE"
                | "ΑΕ"
                | "O" -> "AE"
                | other -> other

            let digits = found.Groups[2].Value.TrimStart '0'

            if digits = "" then
                ValueNone
            else
                ValueSome $"{prefix} {digits}"
        else
            ValueNone

    /// Every ORGANISATION_STATUS word in the file of 2026-11-02. A word not listed is Unknown.
    let standing (word: string voption) =
        match word |> ValueOption.map _.Trim() with
        | ValueSome "Εγγεγραμμένη" -> CompanyStanding.Registered(ValueSome "Εγγεγραμμένη")
        | ValueSome "Στάληκε επιστολή Υπενθύμισης" ->
            CompanyStanding.ActiveWithConcern "Reminder letter sent before striking off (Στάληκε επιστολή Υπενθύμισης)"
        | ValueSome "Δημοσιεύτηκε η Τρίμηνη" ->
            CompanyStanding.ActiveWithConcern "Three-month notice of striking off published (Δημοσιεύτηκε η Τρίμηνη)"
        | ValueSome "Διαγραμμένη" -> CompanyStanding.Closed "Struck off (Διαγραμμένη)"
        | ValueSome "Διάλυση λόγω Ολοκλήρωσης Εκούσιας Εκκαθάρισης"
        | ValueSome "Διάλυση λόγω Ολοκλήρωσης Εκκαθάρισης" -> CompanyStanding.Dissolved
        | ValueSome "Διαλύθηκε λόγω συγχώνευσης" ->
            CompanyStanding.Closed "Dissolved following a merger (Διαλύθηκε λόγω συγχώνευσης)"
        | ValueSome "Αφαιρέθηκε από το Μητρώο ΕΕ με απόφαση Ανωτάτου Δικαστηρίου" ->
            CompanyStanding.Closed "Removed from the register by Supreme Court decision"
        | ValueSome "Εκούσια Εκκαθάριση από Μετόχους" ->
            CompanyStanding.WindingUp "Members' voluntary liquidation (Εκούσια Εκκαθάριση από Μετόχους)"
        | ValueSome "Υπο Εκκαθάριση" -> CompanyStanding.WindingUp "In liquidation (Υπο Εκκαθάριση)"
        | ValueSome "Εκούσια Εκκαθάριση από Πιστωτές" ->
            CompanyStanding.InInsolvency "Creditors' voluntary liquidation (Εκούσια Εκκαθάριση από Πιστωτές)"
        | ValueSome "Εκκαθάριση με Διάταγμα Δικαστηρίου" ->
            CompanyStanding.InInsolvency "Liquidation by court order (Εκκαθάριση με Διάταγμα Δικαστηρίου)"
        | ValueSome "Εκκαθάριση με Διάταγμα Δικαστηρίου& Διορ.Ειδικού Διαχειριστή" ->
            CompanyStanding.InInsolvency "Liquidation by court order with a special manager appointed"
        | ValueSome "Διορισμός Προσωρινού Εκκαθαριστή" ->
            CompanyStanding.InInsolvency "Provisional liquidator appointed (Διορισμός Προσωρινού Εκκαθαριστή)"
        | ValueSome "Υπο Διαχείριση" -> CompanyStanding.InInsolvency "Under examinership (Υπο Διαχείριση)"
        | ValueSome "Υπο Διαχείριση & Υπο Εκκαθάριση" ->
            CompanyStanding.InInsolvency "Under examinership and in liquidation (Υπο Διαχείριση & Υπο Εκκαθάριση)"
        | ValueSome "Ευρωπαϊκή Εταιρεία" -> CompanyStanding.Unknown(ValueSome "Ευρωπαϊκή Εταιρεία")
        | ValueSome "" -> CompanyStanding.Unknown ValueNone
        | ValueSome other -> CompanyStanding.Unknown(ValueSome other)
        | ValueNone -> CompanyStanding.Unknown ValueNone

    /// 01/01/1900 stands where the file has no date.
    let private dateOf (text: string voption) =
        BulkFile.date "dd/MM/yyyy" text
        |> ValueOption.filter (fun date -> date <> DateOnly(1900, 1, 1))

    let private insolvent standing =
        match standing with
        | CompanyStanding.InInsolvency _ -> true
        | _ -> false

    /// The street and building fields end with ", ", which is trimmed.
    let private part (text: string voption) =
        text
        |> ValueOption.map (fun text -> text.Trim().TrimEnd(',').Trim())
        |> ValueOption.filter (fun text -> text <> "")

    let private officeAddress (columns: IReadOnlyDictionary<string, int>) (record: DelimitedRecord) : Address =
        let cell name = BulkFile.cell columns name record

        {
            Lines =
                [ part (cell "STREET"); part (cell "BUILDING") ]
                |> List.choose ValueOption.toOption
            Locality = part (cell "TERRITORY")
            Region = ValueNone
            PostalCode = ValueNone
            Country = ValueNone
        }

    /// The positions the file lists, Director and Secretary placed, every other kept in its wording.
    let officerRole (position: string voption) =
        match position with
        | ValueSome "Διευθυντής" -> OfficerRole.Director
        | ValueSome "Γραμματέας" -> OfficerRole.Secretary
        | ValueSome other -> OfficerRole.Other other
        | ValueNone -> OfficerRole.Other "Not stated"

    let private officer (columns: IReadOnlyDictionary<string, int>) (record: DelimitedRecord) : Officer =
        let cell name = BulkFile.cell columns name record

        {
            Name = defaultValueArg (cell "PERSON_OR_ORGANISATION_NAME") PartyNames.NotGiven
            Kind = PartyKind.Unknown ValueNone
            Role = officerRole (cell "OFFICIAL_POSITION")
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

    /// The key of a row by its type code and number columns; ValueNone for a row whose type or number can't be one.
    let private keyOf (columns: IReadOnlyDictionary<string, int>) (record: DelimitedRecord) =
        match BulkFile.cell columns "ORGANISATION_TYPE_CODE" record, BulkFile.cell columns "REGISTRATION_NO" record with
        | ValueSome code, ValueSome number ->
            prefixOf code
            |> ValueOption.bind (fun prefix -> normalNumber $"{prefix} {number}")
        | _ -> ValueNone

    let private organisationColumns =
        [
            "ORGANISATION_NAME"
            "REGISTRATION_NO"
            "ORGANISATION_TYPE_CODE"
            "ORGANISATION_TYPE"
            "REGISTRATION_DATE"
            "ORGANISATION_STATUS"
        ]

    /// Builds the register from the organisations file and, as far as the host passes them, the registered office and officials files. The
    /// file names carry a release number that changes, so the host picks the files.
    let build
        (organisations: Stream)
        (registeredOffices: Stream voption)
        (officials: Stream voption)
        (date: BulkFileDate)
        (ct: CancellationToken)
        : ICompanyRegistry =
        let offices =
            match registeredOffices with
            | ValueSome stream ->
                let columns, rows = BulkFile.delimited "The registered office file" ',' true stream
                BulkFile.requireColumns "The registered office file" columns [ "ADDRESS_SEQ_NO" ]

                rows
                |> Seq.choose (fun row ->
                    BulkFile.cell columns "ADDRESS_SEQ_NO" row
                    |> ValueOption.map (fun sequence -> sequence, officeAddress columns row)
                    |> ValueOption.toOption)
                |> readOnlyDict
            | ValueNone -> readOnlyDict []

        let people =
            match officials with
            | ValueSome stream ->
                let columns, rows = BulkFile.delimited "The officials file" ',' true stream

                BulkFile.requireColumns
                    "The officials file"
                    columns
                    [
                        "REGISTRATION_NO"
                        "ORGANISATION_TYPE_CODE"
                        "PERSON_OR_ORGANISATION_NAME"
                        "OFFICIAL_POSITION"
                    ]

                rows
                |> Seq.choose (fun row ->
                    ct.ThrowIfCancellationRequested()

                    keyOf columns row
                    |> ValueOption.map (fun key -> struct (key, officer columns row))
                    |> ValueOption.toOption)
                |> Bulk.grouped
            | ValueNone -> readOnlyDict []

        let columns, rows =
            BulkFile.delimited "The organisations file" ',' true organisations

        BulkFile.requireColumns "The organisations file" columns organisationColumns

        let row (row: DelimitedRecord) =
            ct.ThrowIfCancellationRequested()
            let cell name = BulkFile.cell columns name row

            match keyOf columns row, cell "ORGANISATION_NAME" with
            | ValueNone, _ -> BulkRow.WithoutNumber
            | ValueSome number, ValueNone -> BulkRow.Unreadable(number, "The file's record for this number has no name")
            | ValueSome number, ValueSome name ->
                let status = cell "ORGANISATION_STATUS"
                let standing = standing status

                let address =
                    cell "ADDRESS_SEQ_NO"
                    |> ValueOption.bind (fun sequence ->
                        match offices.TryGetValue sequence with
                        | true, address -> ValueSome address
                        | _ -> ValueNone)

                let company =
                    {
                        Name = name
                        OtherNames = []
                        Number = number
                        Jurisdiction = Jurisdiction.ofCode "CY"
                        JurisdictionAsPublished = ValueNone
                        Standing = standing
                        Concerns = Bulk.insolvencyConcerns (insolvent standing)
                        StatusAsPublished = status
                        LegalForm =
                            match cell "ORGANISATION_TYPE", cell "ORGANISATION_SUB_TYPE" with
                            | ValueSome kind, ValueSome sub -> ValueSome $"{kind}, {sub}"
                            | kind, ValueNone -> kind
                            | ValueNone, sub -> sub
                        IncorporatedOn = dateOf (cell "REGISTRATION_DATE")
                        CeasedOn =
                            match standing with
                            | CompanyStanding.Dissolved
                            | CompanyStanding.Closed _ -> dateOf (cell "ORGANISATION_STATUS_DATE")
                            | _ -> ValueNone
                        Addresses =
                            match registeredOffices, address with
                            | ValueNone, _ -> NotAvailable "The host passed no registered office file"
                            | ValueSome _, ValueSome address ->
                                Available
                                    [
                                        {
                                            Kind = AddressKind.RegisteredOffice
                                            Address = address
                                        }
                                    ]
                            | ValueSome _, ValueNone ->
                                NotAvailable "The file gives no registered office for this record"
                        Role =
                            match cell "ORGANISATION_TYPE_CODE" with
                            | ValueSome "O" -> RecordRole.ForeignRegistration
                            | _ -> RecordRole.Home
                        HomeRegistration = ValueNone
                        EstablishmentOf = ValueNone
                    }

                BulkRow.Record
                    {
                        Company = company
                        Officers =
                            match Bulk.listed people number with
                            | [] -> ValueNone
                            | listed -> ValueSome listed
                        Owners = ValueNone
                        Raw = row.Raw
                    }

        let tally = rows |> Seq.map row |> Bulk.tally normalNumber

        if tally.Records = 0 then
            BulkFile.fail "The organisations file holds no organisation"

        BulkAnswers.Over(
            coverage registeredOffices.IsSome officials.IsSome,
            normalNumber,
            "A Cyprus number has its type, such as HE 12345 or AE 123",
            tally,
            date,
            BulkAbsence.NotHeld,
            (if officials.IsSome then
                 "The officials file lists no one for this organisation"
             else
                 "The host passed no officials file"),
            (fun purpose ->
                SourceAnswer.restrictedOwners
                    purpose
                    "Unverified: those the Cyprus beneficial owner register rules allow, such as obliged entities under anti-money-laundering rules"
                    "Apply to the Registrar of Companies for beneficial owner register access, or use a vendor")
        )
