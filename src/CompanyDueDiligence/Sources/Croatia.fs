namespace CompanyDueDiligence

open System
open System.Text.Json
open System.Text.RegularExpressions
open System.Threading
open System.Threading.Tasks
open CompanyDueDiligence.Json

/// The Sudski registar open data API, written from its OpenAPI document (v3.0.4) at sudreg-data.gov.hr/api/javni/dokumentacija/open_api as read
/// on 2026-11-08. Not run live: every call needs OAuth2 client credentials. The status codes 1, 0 and 5 and the 6-a-minute figure are a
/// published client library's, unverified.
module internal Croatia =

    let coverage: RegistryCoverage =
        {
            Registry = "Sudski registar open data (Croatia)"
            Jurisdictions = [ Jurisdiction.ofCode "HR" ]
            Standing =
                Support.Partial
                    "Deletion, bankruptcy and liquidation as the record flags them; a proceeding by the codebook's own word, and any other is Unknown"
            Concerns =
                Support.Partial
                    "A bankruptcy or liquidation the record shows now; past ones, overdue filings and disputes aren't read"
            Addresses = Support.Supported
            Search = Support.NotOffered "Not read: the API's name filter is on a bulk table paged at 1,000 rows"
            Officers = Support.NotOffered "The open data API lists no persons"
            Owners = Support.NotOffered "The court register lists no beneficial owners"
            Cost =
                AccessCost.FreeWithKey
                    "OAuth2 client credentials from sudreg-data.gov.hr, after registering and confirming the e-mail"
            RateLimit = RateLimitPolicy.NotPublished(6, TimeSpan.FromMinutes 1.)
            Licence = "No licence found: the portal's pages loop on redirects and couldn't be read. Not run live"
        }

    /// What a number is looked up as: an OIB, the tax number, or an MBS, the register's own.
    [<RequireQualifiedAccess>]
    type Lookup =
        | Oib of oib: string
        | Mbs of mbs: string

    let private oibRegex = Regex @"\A[0-9]{11}\z"
    let private mbsRegex = Regex @"\A[0-9]{1,9}\z"

    /// ISO 7064 MOD 11,10, as the OIB's check digit is defined.
    let isOib (text: string) =
        oibRegex.IsMatch text
        && (let remainder =
                text
                |> Seq.take 10
                |> Seq.fold
                    (fun carried c ->
                        let sum = (carried + int c - int '0') % 10
                        (if sum = 0 then 10 else sum) * 2 % 11)
                    10

            let check = 11 - remainder
            (if check = 10 then 0 else check) = int text[10] - int '0')

    /// Eleven digits passing the OIB check, or an MBS of up to nine digits, sent with its leading zeros. Spaces are taken out.
    let lookupOf (number: string) =
        let digits = if isNull number then "" else number.Replace(" ", "")

        if isOib digits then
            ValueSome(Lookup.Oib digits)
        elif mbsRegex.IsMatch digits && digits.TrimStart('0') <> "" then
            ValueSome(Lookup.Mbs(digits.PadLeft(9, '0')))
        else
            ValueNone

    /// The current entry of a block, whether the answer gives it alone or with its history, in which status 1 is current.
    let private current (name: string) (e: JsonElement) =
        match tryObject name e with
        | ValueNone -> ValueNone
        | ValueSome block ->
            match
                tryProp (name + "_povijest_ex") block
                |> ValueOption.orElse (tryProp (name + "_povijest") block)
            with
            | ValueSome history when history.ValueKind = JsonValueKind.Array ->
                history.EnumerateArray()
                |> Seq.tryFind (fun item -> tryInt "status" item = ValueSome 1)
                |> ValueOption.ofOption
            | _ -> ValueSome block

    /// A flag the API sends as 1 or 0.
    let private flagged name e = tryInt name e = ValueSome 1

    /// Deletion decides, then the subject's status, then a bankruptcy or liquidation estate, then a running proceeding by its word.
    let standing
        (deleted: bool)
        (status: int voption)
        (bankruptcyEstate: bool)
        (liquidationEstate: bool)
        (proceeding: string voption)
        =
        match deleted, status with
        | true, _ -> CompanyStanding.Closed "Deleted from the register (brisan)"
        | _, ValueSome 5 -> CompanyStanding.Closed "Status 5: deleted"
        | _, ValueSome 0 -> CompanyStanding.Unknown(ValueSome "Status 0: historical")
        | _, ValueSome 1 when bankruptcyEstate -> CompanyStanding.InInsolvency "Bankruptcy estate (stečajna masa)"
        | _, ValueSome 1 when liquidationEstate -> CompanyStanding.WindingUp "Liquidation estate (likvidacijska masa)"
        | _, ValueSome 1 ->
            match proceeding with
            | ValueNone -> CompanyStanding.Active
            | ValueSome word when word.Contains("stečaj", StringComparison.OrdinalIgnoreCase) ->
                CompanyStanding.InInsolvency word
            | ValueSome word when word.Contains("likvidacij", StringComparison.OrdinalIgnoreCase) ->
                CompanyStanding.WindingUp word
            | ValueSome word -> CompanyStanding.Unknown(ValueSome word)
        | _, ValueSome other -> CompanyStanding.Unknown(ValueSome $"Status {other}")
        | _, ValueNone -> CompanyStanding.Unknown ValueNone

    /// The running proceeding's name, with expand_relations, or its code when the answer gives only that. A proceeding whose kind can't be read is
    /// still one.
    let private proceedingOf (e: JsonElement) =
        current "postupak" e
        |> ValueOption.map (fun block ->
            match tryObject "postupak" block with
            | ValueSome kind ->
                SourceAnswer.trimmed "znacenje" kind
                |> ValueOption.defaultValue "Proceeding of a kind not given"
            | ValueNone ->
                tryText "postupak" block
                |> ValueOption.map (fun code -> $"Proceeding code {code}")
                |> ValueOption.defaultValue "Proceeding of a kind not given")

    let private address (e: JsonElement) =
        match current "sjediste" e with
        | ValueNone -> NotAvailable "The answer holds no seat"
        | ValueSome seat ->
            let street =
                [
                    OpenData.text "ulica" seat
                    OpenData.text "kucni_broj" seat
                    OpenData.text "kucni_podbroj" seat
                ]
                |> List.choose ValueOption.toOption
                |> String.concat " "

            Available
                [
                    {
                        Kind = AddressKind.RegisteredOffice
                        Address =
                            {
                                Lines = if street = "" then [] else [ street ]
                                Locality =
                                    SourceAnswer.trimmed "naziv_naselja" seat
                                    |> ValueOption.orElse (SourceAnswer.trimmed "naselje_van_sifrarnika" seat)
                                Region = SourceAnswer.trimmed "naziv_zupanije" seat
                                PostalCode = OpenData.text "postanski_broj" seat
                                Country = tryObject "drzava" seat |> ValueOption.bind (SourceAnswer.trimmed "naziv")
                            }
                    }
                ]

    let private homeOf (e: JsonElement) =
        current "inozemni_registar" e
        |> ValueOption.map (fun foreign ->
            {
                LegalName = ValueNone
                LegalForm =
                    SourceAnswer.trimmed "pravni_oblik" foreign
                    |> ValueOption.orElse (
                        tryObject "bris_pravni_oblik_kod" foreign
                        |> ValueOption.bind (SourceAnswer.trimmed "naziv")
                    )
                GoverningLaw = ValueNone
                Country =
                    tryObject "drzava" foreign
                    |> ValueOption.bind (fun country ->
                        SourceAnswer.trimmed "oznaka_2" country
                        |> ValueOption.orElse (SourceAnswer.trimmed "naziv" country))
                Register =
                    SourceAnswer.trimmed "naziv_registra" foreign
                    |> ValueOption.orElse (SourceAnswer.trimmed "registarsko_tijelo" foreign)
                RegistrationNumber = SourceAnswer.trimmed "broj_iz_registra" foreign
            })

    let company (e: JsonElement) : Company =
        let name =
            current "tvrtka" e
            |> ValueOption.bind (SourceAnswer.trimmed "ime")
            |> ValueOption.defaultWith (fun () -> raise (InvalidJson "The answer has no current name"))

        let mbs =
            SourceAnswer.trimmed "potpuni_mbs" e
            |> ValueOption.orElse (tryText "mbs" e |> ValueOption.map (fun mbs -> mbs.PadLeft(9, '0')))
            |> ValueOption.defaultWith (fun () -> raise (InvalidJson "Missing or invalid field 'mbs'"))

        let bankruptcyEstate = flagged "stecajna_masa" e
        let liquidationEstate = flagged "likvidacijska_masa" e
        let proceeding = proceedingOf e
        let deleted = (tryProp "datum_brisanja" e).IsSome

        let standing =
            standing deleted (tryInt "status" e) bankruptcyEstate liquidationEstate proceeding

        let insolvent =
            match standing with
            | CompanyStanding.InInsolvency _
            | CompanyStanding.WindingUp _ -> true
            | _ -> false

        {
            Name = name
            OtherNames =
                [
                    current "skracena_tvrtka" e |> ValueOption.bind (SourceAnswer.trimmed "ime")
                    SourceAnswer.trimmed "tvrtka_kod_brisanja" e
                ]
                |> List.choose ValueOption.toOption
                |> List.filter ((<>) name)
                |> List.distinct
            Number = mbs
            Jurisdiction = Jurisdiction.ofCode "HR"
            JurisdictionAsPublished = ValueNone
            Standing = standing
            Concerns =
                if insolvent then
                    {
                        Stated = [ CompanyConcern.InsolvencyHistory ]
                        Unconfirmed = CompanyConcern.all |> List.except [ CompanyConcern.InsolvencyHistory ]
                    }
                else
                    CompanyConcerns.notPublished
            StatusAsPublished =
                match deleted, proceeding, tryInt "status" e with
                | true, _, _ -> ValueSome "brisan"
                | _, ValueSome word, _ -> ValueSome word
                | _, _, ValueSome status -> ValueSome $"status {status}"
                | _ -> ValueNone
            LegalForm =
                current "pravni_oblik" e
                |> ValueOption.bind (tryObject "vrsta_pravnog_oblika")
                |> ValueOption.bind (SourceAnswer.trimmed "naziv")
            IncorporatedOn = SourceAnswer.dateOfTimestamp "datum_osnivanja" e
            CeasedOn = SourceAnswer.dateOfTimestamp "datum_brisanja" e
            Addresses = address e
            Role =
                match tryInt "ino_podruznica" e with
                | ValueSome 1 -> RecordRole.ForeignRegistration
                | ValueSome 0 -> RecordRole.Home
                | _ -> RecordRole.NotStated
            HomeRegistration = if flagged "ino_podruznica" e then homeOf e else ValueNone
            EstablishmentOf = ValueNone
        }

    /// The subject for this lookup: ValueNone for the empty object no_data_error=0 asks for. A subject with another number fails the read.
    let lookup (asked: Lookup) (root: JsonElement) =
        if root.ValueKind = JsonValueKind.Object && Seq.isEmpty (root.EnumerateObject()) then
            ValueNone
        elif root.ValueKind <> JsonValueKind.Object then
            raise (InvalidJson "The answer isn't a subject")
        else
            let found = company root

            let same =
                match asked with
                | Lookup.Mbs mbs -> found.Number = mbs
                | Lookup.Oib oib ->
                    (SourceAnswer.trimmed "potpuni_oib" root
                     |> ValueOption.orElse (tryText "oib" root |> ValueOption.map (fun oib -> oib.PadLeft(11, '0'))))
                        =
                        ValueSome oib

            if same then
                ValueSome(found, root.GetRawText())
            else
                raise (InvalidJson "The answer holds another subject")

/// Croatia as an ICompanyRegistry, with a bearer token from the client-credentials grant.
[<Sealed>]
type internal CroatiaRegistry(connection: SourceConnection, credentials: ClientCredentials) =
    let client =
        SourceClient(connection, SourceAnswer.pacing 6 (TimeSpan.FromMinutes 1.))

    let token =
        new ClientCredentialsToken(
            SourceClient(connection, SourceAnswer.pacing 2 (TimeSpan.FromSeconds 1.)),
            Uri "https://sudreg-data.gov.hr/api/oauth/token",
            credentials,
            ValueNone
        )
        :> IBearerToken

    let coverage = Croatia.coverage
    let hide = SourceAnswer.hiding [ credentials.Id; credentials.Secret ]

    interface ICompanyRegistry with
        member _.Coverage = coverage

        member _.TryFindAsync(number: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                match Croatia.lookupOf number with
                | ValueNone ->
                    Task.FromResult(
                        Error(
                            RegistryFailure.InvalidRequest
                                "An OIB has eleven digits passing its check digit, and an MBS up to nine digits"
                        )
                    )
                | ValueSome lookup ->
                    task {
                        let kind, value =
                            match lookup with
                            | Croatia.Lookup.Oib oib -> "oib", oib
                            | Croatia.Lookup.Mbs mbs -> "mbs", mbs

                        let uri =
                            Uri
                                $"https://sudreg-data.gov.hr/api/javni/detalji_subjekta?tip_identifikatora={kind}&identifikator={value}&expand_relations=1&no_data_error=0"

                        let send header =
                            client.GetAsync(uri, [ header; "Accept", "application/json" ], ct)

                        match! Bearer.sendAsync token send ct with
                        | Error failure -> return Error(hide failure)
                        | Ok reply when reply.Status = 404 -> return Error(RegistryFailure.RequestRefused "HTTP 404")
                        | Ok reply ->
                            return
                                SourceAnswer.json reply (Croatia.lookup lookup)
                                |> Result.map (fun found ->
                                    match found with
                                    | ValueNone ->
                                        SourceAnswer.evidence coverage reply reply.LastModified (Available ValueNone)
                                    | ValueSome(company, raw) ->
                                        { SourceAnswer.evidence
                                              coverage
                                              reply
                                              reply.LastModified
                                              (Available(ValueSome company)) with
                                            Raw = ValueSome raw
                                        })
                    })

        member _.SearchAsync(_: string, jurisdiction: Jurisdiction, _: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                Task.FromResult(
                    Ok(
                        SourceAnswer.unasked
                            coverage
                            client.Now
                            (NotAvailable "Not read: the API's name filter is on a bulk table paged at 1,000 rows")
                    )
                ))

        member _.OfficersAsync(_: string, jurisdiction: Jurisdiction, _: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                Task.FromResult(
                    Ok(SourceAnswer.unasked coverage client.Now (NotAvailable "The open data API lists no persons"))
                ))

        member _.OwnersAsync(_: string, jurisdiction: Jurisdiction, purpose: OwnerPurpose, _: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                Task.FromResult(
                    Ok(
                        SourceAnswer.unasked
                            coverage
                            client.Now
                            (SourceAnswer.restrictedOwners
                                purpose
                                "Unverified: those the Croatian beneficial ownership register (FINA) admits, such as obliged entities under anti-money-laundering rules"
                                "Apply to FINA for the register of beneficial owners, or use a vendor")
                    )
                ))
