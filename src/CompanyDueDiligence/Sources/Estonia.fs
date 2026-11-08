namespace CompanyDueDiligence

open System
open System.Collections.Generic
open System.IO
open System.Text
open System.Text.Json
open System.Text.RegularExpressions
open System.Threading
open System.Threading.Tasks
open CompanyDueDiligence.Json

/// The e-Business Register's open data files (avaandmed.ariregister.rik.ee), JSON in zips, as published on 2026-11-07: general data
/// (yldandmed), persons on the registry card (kaardile_kantud_isikud), shareholders (osanikud) and beneficial owners (kasusaajad).
module internal Estonia =

    [<Literal>]
    let Registry = "e-Business Register open data (Estonia)"

    let private absentFile what =
        Support.NotOffered $"The host passed no {what} file"

    let coverage (persons: bool) (owners: bool) : RegistryCoverage =
        {
            Registry = Registry
            Jurisdictions = [ Jurisdiction.ofCode "EE" ]
            Standing = Support.Partial "Entered, in liquidation or bankrupt: the files hold current entities only"
            Concerns = Support.Partial "Bankruptcy is stated; overdue filings aren't in the files"
            Addresses = Support.Supported
            Search = Support.Partial "By name within the file, up to 50 matches"
            Officers =
                if persons then
                    Support.Partial "Persons on the registry card now; former ones aren't in the file"
                else
                    absentFile "persons on the registry card"
            Owners =
                if owners then
                    Support.Partial "Current shareholders and beneficial owners, as far as the host passed their files"
                else
                    absentFile "shareholder or beneficial owner"
            Cost = AccessCost.Free
            RateLimit = RateLimitPolicy.NotPublished(1, TimeSpan.FromDays 1.)
            Licence = "CC BY 4.0, as the open data page states; downloading accepts its licence terms"
        }

    let private codeRegex = Regex @"\A[0-9]{8}\z"

    /// A registry code: eight digits.
    let normalCode (number: string) =
        let normal = if isNull number then "" else number.Trim()

        if codeRegex.IsMatch normal then
            ValueSome normal
        else
            ValueNone

    let private dateOf name e =
        BulkFile.date "dd.MM.yyyy" (tryText name e)

    /// The status codes the files use: R entered, L in liquidation, N bankrupt, K deleted, which the status history holds.
    let standing (code: string voption) (text: string voption) =
        let word = text |> ValueOption.orElse code

        match code with
        | ValueSome "R" -> CompanyStanding.Registered(ValueSome(defaultValueArg word "Registrisse kantud"))
        | ValueSome "L" -> CompanyStanding.WindingUp "Likvideerimisel (in liquidation)"
        | ValueSome "N" -> CompanyStanding.InInsolvency "Pankrotis (bankrupt)"
        | ValueSome "K" -> CompanyStanding.Closed "Kustutatud (deleted from the register)"
        | _ -> CompanyStanding.Unknown word

    let private nonBlank (text: string voption) =
        text
        |> ValueOption.map _.Trim()
        |> ValueOption.bind (fun text -> if text = "" then ValueNone else ValueSome text)

    let private trimmedText name e = nonBlank (tryText name e)

    let private isCurrent (e: JsonElement) = (trimmedText "lopp_kpv" e).IsNone

    let private joined (parts: string voption list) =
        match parts |> List.choose ValueOption.toOption with
        | [] -> ValueNone
        | parts -> ValueSome(String.Join(" ", parts))

    /// The record trimmed to the fields read, as compact JSON.
    let private trimmedRaw (e: JsonElement) (general: JsonElement) =
        use buffer = new MemoryStream()
        use writer = new Utf8JsonWriter(buffer)
        writer.WriteStartObject()

        for name in [ "ariregistri_kood"; "nimi" ] do
            match tryProp name e with
            | ValueSome value ->
                writer.WritePropertyName name
                value.WriteTo writer
            | ValueNone -> ()

        writer.WritePropertyName "yldandmed"
        writer.WriteStartObject()

        for name in
            [
                "esmaregistreerimise_kpv"
                "kustutamise_kpv"
                "staatus"
                "staatus_tekstina"
                "oiguslik_vorm"
                "oiguslik_vorm_tekstina"
                "oigusliku_vormi_alaliik_tekstina"
                "staatused"
                "arinimed"
                "aadressid"
            ] do
            match general.TryGetProperty name with
            | true, value ->
                writer.WritePropertyName name
                value.WriteTo writer
            | _ -> ()

        writer.WriteEndObject()
        writer.WriteEndObject()
        writer.Flush()
        Encoding.UTF8.GetString(buffer.ToArray())

    let private address (e: JsonElement) : CompanyAddress =
        {
            Kind = AddressKind.RegisteredOffice
            Address =
                {
                    Lines = trimmedText "tanav_maja_korter" e |> ValueOption.toList
                    Locality = trimmedText "ehak_nimetus" e
                    Region = ValueNone
                    PostalCode = trimmedText "postiindeks" e
                    Country = trimmedText "riik_tekstina" e
                }
        }

    /// A company from the general data file's record. One without a registry code or name is unreadable.
    let company (e: JsonElement) : Company * string =
        let number =
            match trimmedText "ariregistri_kood" e with
            | ValueSome number -> number
            | ValueNone -> raise (InvalidJson "A record has no registry code")

        let name =
            match trimmedText "nimi" e with
            | ValueSome name -> name
            | ValueNone -> raise (InvalidJson "A record has no name")

        let general =
            match tryObject "yldandmed" e with
            | ValueSome general -> general
            | ValueNone -> raise (InvalidJson "A record has no general data")

        let code = trimmedText "staatus" general
        let standing = standing code (trimmedText "staatus_tekstina" general)

        let addresses =
            array general "aadressid" |> List.filter isCurrent |> List.map address

        let form = trimmedText "oiguslik_vorm" general

        let company =
            {
                Name = name
                OtherNames =
                    array general "arinimed"
                    |> List.filter (isCurrent >> not)
                    |> List.choose (trimmedText "sisu" >> ValueOption.toOption)
                    |> List.filter ((<>) name)
                    |> List.distinct
                Number = number
                Jurisdiction = Jurisdiction.ofCode "EE"
                JurisdictionAsPublished = ValueNone
                Standing = standing
                Concerns = Bulk.insolvencyConcerns (code = ValueSome "N")
                StatusAsPublished = trimmedText "staatus_tekstina" general |> ValueOption.orElse code
                LegalForm =
                    joined
                        [
                            trimmedText "oiguslik_vorm_tekstina" general
                            trimmedText "oigusliku_vormi_alaliik_tekstina" general
                        ]
                IncorporatedOn = dateOf "esmaregistreerimise_kpv" general
                CeasedOn = dateOf "kustutamise_kpv" general
                Addresses =
                    match addresses with
                    | [] -> NotAvailable "The record holds no current address"
                    | addresses -> Available addresses
                Role =
                    match form with
                    | ValueSome "FIL" -> RecordRole.ForeignRegistration
                    | ValueSome _ -> RecordRole.Home
                    | ValueNone -> RecordRole.NotStated
                HomeRegistration = ValueNone
                EstablishmentOf = ValueNone
            }

        company, trimmedRaw e general

    let private kind (e: JsonElement) =
        match trimmedText "isiku_tyyp" e with
        | ValueSome "F" -> PartyKind.Individual
        | ValueSome "J" -> PartyKind.LegalEntity
        | other -> PartyKind.Unknown other

    let private personName (surname: string) (e: JsonElement) =
        joined [ trimmedText "eesnimi" e; trimmedText surname e ]

    /// A legal entity's registration as a person record gives it: an Estonian registry code, or a foreign code and country.
    let private entity (name: string voption) (e: JsonElement) =
        let estonian = trimmedText "isikukood_registrikood" e
        let foreign = trimmedText "valis_kood" e

        match estonian, foreign with
        | ValueNone, ValueNone -> ValueNone
        | _ ->
            ValueSome
                {
                    LegalName = name
                    LegalForm = ValueNone
                    GoverningLaw = ValueNone
                    Country =
                        if estonian.IsSome then
                            ValueSome "EE"
                        else
                            trimmedText "valis_kood_riik_tekstina" e
                    Register = if estonian.IsSome then ValueSome Registry else ValueNone
                    RegistrationNumber = estonian |> ValueOption.orElse foreign
                }

    /// Board member roles are directors; every other role on the card keeps its own wording.
    let officerRole (code: string voption) (text: string voption) =
        match code with
        | ValueSome("JUHL" | "JUHJ" | "JUHE" | "JUHA") -> OfficerRole.Director
        | _ -> OfficerRole.Other(text |> ValueOption.orElse code |> ValueOption.defaultValue "Not stated")

    let officer (e: JsonElement) : Officer =
        let kind = kind e
        let name = personName "nimi_arinimi" e

        {
            Name = defaultValueArg name PartyNames.NotGiven
            Kind = kind
            Role = officerRole (trimmedText "isiku_roll" e) (trimmedText "isiku_roll_tekstina" e)
            Current = isCurrent e
            AppointedOn = dateOf "algus_kpv" e
            EndedOn = dateOf "lopp_kpv" e
            BornIn = ValueNone
            Nationality = ValueNone
            CountryOfResidence = trimmedText "aadress_riik_tekstina" e
            Address = ValueNone
            Entity =
                if kind = PartyKind.LegalEntity then
                    entity name e
                else
                    ValueNone
            IdentityVerification = ValueNone
        }

    /// A percentage as the file writes it, such as "23.75", as a band between the whole numbers either side; absent or unreadable is ValueNone.
    let percentBand (percent: string voption) =
        percent
        |> ValueOption.bind (fun text ->
            match
                Decimal.TryParse(text, Globalization.NumberStyles.Number, Globalization.CultureInfo.InvariantCulture)
            with
            | true, value when value >= 0m && value <= 100m ->
                ValueSome
                    {
                        From = int (Math.Floor value)
                        To = int (Math.Ceiling value)
                    }
            | _ -> ValueNone)

    let shareholder (e: JsonElement) : Owner * OwnershipGap list =
        let kind = kind e
        let name = personName "nimi_arinimi" e

        let entity =
            if kind = PartyKind.LegalEntity then
                entity name e
            else
                ValueNone

        let share =
            [
                trimmedText "osaluse_protsent" e
                |> ValueOption.map (fun percent -> percent + "%")
                trimmedText "osaluse_omandiliik_tekstina" e
            ]
            |> List.choose ValueOption.toOption

        let role = trimmedText "isiku_roll_tekstina" e |> ValueOption.defaultValue "Osanik"

        let owner =
            {
                Name = name
                Kind = kind
                Control =
                    [
                        {
                            Kind = ControlKind.Ownership
                            Band = percentBand (trimmedText "osaluse_protsent" e)
                            AsPublished =
                                match share with
                                | [] -> role
                                | share -> $"""{role}: {String.Join(", ", share)}"""
                        }
                    ]
                Current = isCurrent e
                Since = dateOf "algus_kpv" e
                EndedOn = dateOf "lopp_kpv" e
                BornIn = ValueNone
                Nationality = ValueNone
                CountryOfResidence = trimmedText "aadress_riik_tekstina" e
                Address = ValueNone
                Entity = entity
                IdentityVerification = ValueNone
            }

        let gaps =
            [
                if name.IsNone then
                    OwnershipGap.OwnerUnreadable
                if kind = PartyKind.LegalEntity && entity.IsNone then
                    OwnershipGap.EntityNotIdentified
            ]

        owner, gaps

    /// How a beneficial owner controls, by the file's code. J and C name a board member because no owner was identified.
    let control (code: string voption) (text: string voption) =
        let asPublished =
            text |> ValueOption.orElse code |> ValueOption.defaultValue "Not stated"

        let kind, gaps =
            match code with
            | ValueSome("O" | "K") -> ControlKind.Ownership, []
            | ValueSome "X" -> ControlKind.VotingRights, []
            | ValueSome "Y" -> ControlKind.AppointsBoard, []
            | ValueSome "F" -> ControlKind.SignificantInfluence, []
            | ValueSome("J" | "C") -> ControlKind.Other, [ OwnershipGap.OwnerNotIdentified ]
            | ValueSome("Z" | "S") -> ControlKind.Other, []
            | _ -> ControlKind.Other, [ OwnershipGap.StatementNotUnderstood asPublished ]

        {
            Kind = kind
            Band = ValueNone
            AsPublished = $"Kasusaaja (beneficial owner): {asPublished}"
        },
        gaps

    let beneficialOwner (e: JsonElement) : Owner * OwnershipGap list =
        let name = personName "nimi" e

        let control, gaps =
            control (trimmedText "kontrolli_teostamise_viis" e) (trimmedText "kontrolli_teostamise_viis_tekstina" e)

        let disputed =
            match tryProp "lahknevusteade_esitatud" e with
            | ValueSome value when value.ValueKind = JsonValueKind.True -> [ OwnershipGap.OwnerDetailsNotConfirmed ]
            | _ -> []

        let owner =
            {
                Name = name
                Kind = PartyKind.Individual
                Control = [ control ]
                Current = isCurrent e
                Since = dateOf "algus_kpv" e
                EndedOn = dateOf "lopp_kpv" e
                BornIn = ValueNone
                Nationality = ValueNone
                CountryOfResidence = trimmedText "aadress_riik_tekstina" e
                Address = ValueNone
                Entity = ValueNone
                IdentityVerification = ValueNone
            }

        owner, gaps @ disputed @ (if name.IsNone then [ OwnershipGap.OwnerUnreadable ] else [])


    /// Folds each element of a top-level JSON array into state, one at a time, as the stream arrives.
    let private foldElementsAsync
        (stream: Stream)
        (ct: CancellationToken)
        (folder: 'S -> JsonElement -> 'S)
        (initial: 'S)
        : Task<'S> =
        let enumerator =
            JsonSerializer
                .DeserializeAsyncEnumerable<JsonElement>(stream, JsonSerializerOptions(), ct)
                .GetAsyncEnumerator
                ct

        // Folds every element whose move completed at once, without awaiting, so a long run of buffered elements adds no stack. Gives
        // the state and the move still pending, or ValueNone at the array's end.
        let rec drain (state: 'S) (move: ValueTask<bool>) =
            if not move.IsCompletedSuccessfully then
                struct (state, ValueSome move)
            elif move.Result then
                ct.ThrowIfCancellationRequested()
                drain (folder state enumerator.Current) (enumerator.MoveNextAsync())
            else
                struct (state, ValueNone)

        // Each pending move is awaited in its own task, chained by recursion: an await inside a loop throws in WebAssembly.
        let rec fold (state: 'S) (move: ValueTask<bool>) : Task<'S> =
            match drain state move with
            | struct (state, ValueNone) -> Task.FromResult state
            | struct (state, ValueSome pending) ->
                task {
                    let! more = pending

                    if more then
                        ct.ThrowIfCancellationRequested()
                        return! fold (folder state enumerator.Current) (enumerator.MoveNextAsync())
                    else
                        return state
                }

        task {
            // The enumerator is disposed by awaiting, which a finally block can't do, so the outcome is kept and given after it.
            let! outcome =
                task {
                    try
                        let! state = fold initial (enumerator.MoveNextAsync())
                        return Ok state
                    with error ->
                        return Error error
                }

            do! enumerator.DisposeAsync()

            return
                match outcome with
                | Ok state -> state
                | Error error ->
                    Runtime.ExceptionServices.ExceptionDispatchInfo.Capture(error).Throw()
                    initial
        }

    /// The items each record of a per-company file lists under property, by registry code.
    let private perCompanyAsync (stream: Stream) (file: string) (property: string) (read: JsonElement -> 'T) ct =
        task {
            let! found =
                BulkFile.publishedAsync stream file (fun opened ->
                    foldElementsAsync
                        opened
                        ct
                        (fun found e ->
                            match trimmedText "ariregistri_kood" e |> ValueOption.bind normalCode with
                            | ValueNone -> found
                            | ValueSome code -> struct (code, array e property |> List.map read) :: found)
                        [])

            return
                found
                |> List.rev
                |> Seq.collect (fun struct (code, items) -> items |> Seq.map (fun item -> struct (code, item)))
                |> Bulk.grouped
        }

    /// A company's owners from its shareholder and beneficial owner rows. Without shareholder rows, as when the host passed no shareholder file
    /// or the file lists none for the company, the direct owners aren't identified.
    let private listing (withShareholders: bool) (owners: (Owner * OwnershipGap list) list) =
        {
            Owners = owners |> List.map fst
            Statements = []
            Gaps =
                (if withShareholders then
                     []
                 else
                     [ OwnershipGap.OwnerNotIdentified ])
                @ (owners |> List.collect snd)
                |> List.distinct
        }

    /// Builds the register from the general data file and, as far as the host passes them, the persons, shareholder and beneficial owner files:
    /// each the zip as published, or its JSON. The files are read as they stream, so this stays asynchronous.
    let buildAsync
        (general: Stream)
        (persons: Stream voption)
        (shareholders: Stream voption)
        (beneficialOwners: Stream voption)
        (date: BulkFileDate)
        (ct: CancellationToken)
        : Task<ICompanyRegistry> =
        task {
            let perCompany (file: Stream voption) property read =
                match file with
                | ValueSome stream -> perCompanyAsync stream ".json" property read ct
                | ValueNone -> Task.FromResult(readOnlyDict [])

            let! officers = perCompany persons "kaardile_kantud_isikud" officer
            let! shares = perCompany shareholders "osanikud" shareholder
            let! owners = perCompany beneficialOwners "kasusaajad" beneficialOwner

            let row (e: JsonElement) =
                try
                    let company, raw = company e
                    let shareRows = Bulk.listed shares company.Number
                    let ownerRows = shareRows @ Bulk.listed owners company.Number

                    BulkRow.Record
                        {
                            Company = company
                            Officers =
                                match Bulk.listed officers company.Number with
                                | [] -> ValueNone
                                | listed -> ValueSome listed
                            Owners =
                                match ownerRows with
                                | [] -> ValueNone
                                | rows -> ValueSome(listing (not shareRows.IsEmpty) rows)
                            Raw = raw
                        }
                with InvalidJson reason ->
                    match trimmedText "ariregistri_kood" e with
                    | ValueSome number ->
                        BulkRow.Unreadable(number, $"The file's record for this number couldn't be read: {reason}")
                    | ValueNone -> BulkRow.WithoutNumber

            let! rows =
                BulkFile.publishedAsync general ".json" (fun opened ->
                    foldElementsAsync opened ct (fun rows e -> row e :: rows) [])

            let tally = rows |> List.rev |> Bulk.tally normalCode

            if tally.Records = 0 then
                BulkFile.fail "The general data file holds no company"

            return
                BulkAnswers.Over(
                    coverage persons.IsSome (shareholders.IsSome || beneficialOwners.IsSome),
                    normalCode,
                    "A registry code has eight digits",
                    tally,
                    date,
                    BulkAbsence.CantTell
                        "The open data files hold only entities now on the register (entered, in liquidation or bankrupt); deleted ones are left out",
                    (if persons.IsSome then
                         "The persons file lists no one on this company's registry card"
                     else
                         "The host passed no persons on the registry card file"),
                    (fun _ ->
                        if shareholders.IsSome || beneficialOwners.IsSome then
                            NotAvailable "The files passed list no shareholder or beneficial owner for this company"
                        else
                            NotAvailable "The host passed no shareholder or beneficial owner file")
                )
        }
