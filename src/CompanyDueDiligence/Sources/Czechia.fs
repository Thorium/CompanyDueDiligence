namespace CompanyDueDiligence

open System
open System.Net.Http
open System.Text
open System.Text.Json
open System.Text.RegularExpressions
open System.Threading
open CompanyDueDiligence.Json

/// The Ministry of Finance's ARES REST API 1.4, read as it answered on 2026-11-08, with its OpenAPI document and the StavZdroje and
/// PravniForma code lists from its own ciselniky-nazevniky service.
module internal Czechia =

    let coverage: RegistryCoverage =
        {
            Registry = "ARES (Czechia)"
            Jurisdictions = [ Jurisdiction.ofCode "CZ" ]
            Standing =
                Support.Partial
                    "The public register's state as ARES copies it, or else the economic subjects register's; liquidation is read from the name's suffix"
            Concerns =
                Support.Partial
                    "A record in the insolvency register or the central register of bankrupts is stated; overdue filings aren't published"
            Addresses = Support.Supported
            Search =
                Support.Partial
                    "Up to 100 matches by business name, never complete: ARES leaves out entities deleted from the register, and a number it doesn't find is NotAvailable"
            Officers =
                Support.Partial
                    "Statutory bodies, other bodies and prokura from the public register; an entity outside it, such as a sole trader, has none there"
            Owners =
                Support.Partial
                    "Partners of an s.r.o. and a sole shareholder of an a.s., from the public register; legal owners, not beneficial owners"
            Cost = AccessCost.Free
            RateLimit =
                RateLimitPolicy.Published(
                    500,
                    TimeSpan.FromMinutes 1.,
                    "Ministry of Finance ARES terms: access may be limited above 500 queries a minute"
                )
            Licence = "No licence stated: neither the API's OpenAPI document nor its answers name one"
        }

    let private icoRegex = Regex @"\A[0-9]{8}\z"

    /// Eight digits, as the API's path pattern requires. Spaces are taken out. No check digit is tested: ARES answers 404 for a number that fails it.
    let normalIco (number: string) =
        let normal = if isNull number then "" else number.Replace(" ", "")

        if icoRegex.IsMatch normal then
            ValueSome normal
        else
            ValueNone

    /// The StavZdroje code list. The public register's state decides; an entity not in it takes the economic subjects register's.
    /// A live entity whose name carries "v likvidaci" is being wound up, and one in the insolvency register has that against it.
    let standing
        (publicRegister: string voption)
        (economicRegister: string voption)
        (insolvencyRegister: string voption)
        (name: string)
        =
        let state =
            match publicRegister with
            | ValueSome code when code <> "NEEXISTUJICI" -> ValueSome code
            | _ ->
                match economicRegister with
                | ValueSome code when code <> "NEEXISTUJICI" -> ValueSome code
                | other -> other |> ValueOption.orElse publicRegister

        match state with
        | ValueSome "AKTIVNI" when name.Contains(" v likvidaci", StringComparison.OrdinalIgnoreCase) ->
            CompanyStanding.WindingUp "v likvidaci (in liquidation)"
        | ValueSome "AKTIVNI" when insolvencyRegister = ValueSome "AKTIVNI" ->
            CompanyStanding.ActiveWithConcern "Listed in the insolvency register (ISIR)"
        | ValueSome "AKTIVNI" -> CompanyStanding.Active
        | ValueSome "POZASTAVENY" -> CompanyStanding.ActiveWithConcern "Pozastavený (suspended)"
        | ValueSome "ZANIKLY" -> CompanyStanding.Closed "Zaniklý (ceased)"
        | ValueSome "HISTORICKY" -> CompanyStanding.Closed "Historický (historical)"
        | ValueSome "LOGICKY_SMAZANY" -> CompanyStanding.Closed "Logicky smazaný (logically deleted)"
        | ValueSome "BUDOUCI" -> CompanyStanding.Unknown(ValueSome "BUDOUCI")
        | ValueSome "NEEXISTUJICI" -> CompanyStanding.Unknown(ValueSome "NEEXISTUJICI")
        | other -> CompanyStanding.Unknown other

    /// Names of the forms met most, from the PravniForma list. Any other stays a code.
    let private formWord (code: string) =
        match code with
        | "101" -> "Fyzická osoba podnikající dle živnostenského zákona"
        | "111" -> "Veřejná obchodní společnost"
        | "112" -> "Společnost s ručením omezeným"
        | "113" -> "Společnost komanditní"
        | "121" -> "Akciová společnost"
        | "205" -> "Družstvo"
        | "421" -> "Odštěpný závod zahraniční právnické osoby"
        | "932" -> "Evropská společnost"
        | other -> other

    /// Forms 421 to 426 are a foreign person's branch, organisational unit or representation, per the PravniForma list.
    let role (form: string voption) =
        match form with
        | ValueSome("421" | "422" | "423" | "424" | "425" | "426") -> RecordRole.ForeignRegistration
        | ValueSome _ -> RecordRole.Home
        | ValueNone -> RecordRole.NotStated

    let private addressOf (e: JsonElement) : Address =
        {
            Lines = SourceAnswer.trimmed "textovaAdresa" e |> ValueOption.toList
            Locality = SourceAnswer.trimmed "nazevObce" e
            Region = SourceAnswer.trimmed "nazevKraje" e
            PostalCode = OpenData.text "psc" e |> ValueOption.orElse (SourceAnswer.trimmed "pscTxt" e)
            Country = SourceAnswer.trimmed "nazevStatu" e
        }

    let private registrations (e: JsonElement) name =
        tryObject "seznamRegistraci" e |> ValueOption.bind (tryString name)

    let company (e: JsonElement) : Company =
        let name = required "obchodniJmeno" tryString e
        let form = tryString "pravniForma" e
        let insolvency = registrations e "stavZdrojeIr"
        let bankrupts = registrations e "stavZdrojeCeu"
        let publicRegister = registrations e "stavZdrojeVr"
        let economicRegister = registrations e "stavZdrojeRes"

        let recorded (state: string voption) =
            match state with
            | ValueSome code when code <> "NEEXISTUJICI" -> true
            | _ -> false

        let addresses =
            [
                tryObject "sidlo" e
                |> ValueOption.map (fun seat ->
                    {
                        Kind = AddressKind.RegisteredOffice
                        Address = addressOf seat
                    })
                tryObject "adresaDorucovaci" e
                |> ValueOption.map (fun postal ->
                    {
                        Kind = AddressKind.Postal
                        Address =
                            {
                                Lines =
                                    [ "radekAdresy1"; "radekAdresy2"; "radekAdresy3" ]
                                    |> List.choose (fun line ->
                                        SourceAnswer.trimmed line postal |> ValueOption.toOption)
                                Locality = ValueNone
                                Region = ValueNone
                                PostalCode = ValueNone
                                Country = ValueNone
                            }
                    })
            ]
            |> List.choose ValueOption.toOption
            |> List.filter (fun address -> not address.Address.Lines.IsEmpty)

        {
            Name = name
            OtherNames = []
            Number = required "ico" tryString e
            Jurisdiction = Jurisdiction.ofCode "CZ"
            JurisdictionAsPublished = ValueNone
            Standing = standing publicRegister economicRegister insolvency name
            Concerns =
                if recorded insolvency || recorded bankrupts then
                    {
                        Stated = [ CompanyConcern.InsolvencyHistory ]
                        Unconfirmed = CompanyConcern.all |> List.except [ CompanyConcern.InsolvencyHistory ]
                    }
                else
                    CompanyConcerns.notPublished
            StatusAsPublished =
                match publicRegister, economicRegister with
                | ValueNone, ValueNone -> ValueNone
                | _ ->
                    ValueSome
                        $"VR {publicRegister |> ValueOption.defaultValue String.Empty}; RES {economicRegister |> ValueOption.defaultValue String.Empty}"
            LegalForm = form |> ValueOption.map formWord
            IncorporatedOn = tryDate "datumVzniku" e
            CeasedOn = tryDate "datumZaniku" e
            Addresses =
                match addresses with
                | [] -> NotAvailable "The answer holds no address"
                | _ -> Available addresses
            Role = role form
            HomeRegistration = ValueNone
            EstablishmentOf = ValueNone
        }

    /// A 404 with code NENALEZENO: ARES holds no economic subject under the number. Any other 404 is no such statement.
    let isNotFound (root: JsonElement) =
        tryString "kod" root = ValueSome "NENALEZENO"

    /// What NENALEZENO leaves open: an entity deleted from the register answers it too, so it never says the number isn't held.
    let notFound =
        NotAvailable
            "ARES leaves out entities deleted from the register, so NENALEZENO doesn't say the number was never held"

    let lookup (ico: string) (root: JsonElement) =
        let found = company root

        if found.Number = ico then
            found
        else
            raise (InvalidJson "The answer holds another entity")

    /// Matches with ARES's total. Never complete, as ARES leaves out entities deleted from the register.
    let search (limit: int) (root: JsonElement) =
        let found =
            match tryProp "ekonomickeSubjekty" root, tryInt "pocetCelkem" root with
            | ValueSome list, total when list.ValueKind = JsonValueKind.Array ->
                OpenData.matches limit total (list.EnumerateArray() |> Seq.map company |> List.ofSeq)
            | _, ValueSome 0 -> OpenData.matches limit (ValueSome 0) []
            | _ -> raise (InvalidJson "The answer has no list of economic subjects")

        { found with Complete = false }

    /// The primary record of the public register's answer.
    let primaryRecord (root: JsonElement) =
        match tryProp "zaznamy" root with
        | ValueSome records when records.ValueKind = JsonValueKind.Array ->
            let all = records.EnumerateArray() |> List.ofSeq

            match
                all
                |> List.tryFind (fun record -> tryFlag "primarniZaznam" record = ValueSome true),
                all
            with
            | Some primary, _ -> primary
            | None, [ only ] -> only
            | None, _ -> raise (InvalidJson "The answer has no primary record")
        | _ -> raise (InvalidJson "The answer has no records")

    let private bornIn (person: JsonElement) =
        tryDate "datumNarozeni" person
        |> ValueOption.map (fun date -> { Year = date.Year; Month = date.Month })

    let private personName (person: JsonElement) =
        [
            tryString "titulPredJmenem" person
            tryString "jmeno" person
            tryString "prijmeni" person
            tryString "titulZaJmenem" person
        ]
        |> List.choose ValueOption.toOption
        |> List.map _.Trim()
        |> List.filter ((<>) "")
        |> String.concat " "

    /// A legal person's own registration. Its country is the seat's ISO code (kodStatu), else the country's name as ARES writes it.
    let private registrationOf (entity: JsonElement) =
        let seat = tryObject "adresa" entity

        {
            LegalName = tryString "obchodniJmeno" entity
            LegalForm = tryString "pravniForma" entity |> ValueOption.map formWord
            GoverningLaw = ValueNone
            Country =
                seat
                |> ValueOption.bind (SourceAnswer.trimmed "kodStatu")
                |> ValueOption.filter (fun code -> code.Length = 2 && code |> Seq.forall Char.IsAsciiLetter)
                |> ValueOption.map _.ToUpperInvariant()
                |> ValueOption.orElse (seat |> ValueOption.bind (tryString "nazevStatu"))
            Register = ValueNone
            RegistrationNumber = tryString "ico" entity
        }

    /// Who an engagement names: a person, a legal person, or a hidden entry.
    let private party (entry: JsonElement) =
        match tryObject "fyzickaOsoba" entry, tryObject "pravnickaOsoba" entry with
        | ValueSome person, _ ->
            match personName person with
            | "" -> PartyNames.NotGiven, PartyKind.Unknown ValueNone, ValueSome person, ValueNone
            | name -> name, PartyKind.Individual, ValueSome person, ValueNone
        | _, ValueSome entity ->
            tryString "obchodniJmeno" entity |> ValueOption.defaultValue PartyNames.NotGiven,
            PartyKind.LegalEntity,
            ValueSome entity,
            ValueSome(registrationOf entity)
        | _ when (tryObject "skrytyUdaj" entry).IsSome ->
            PartyNames.NotPublished, PartyKind.Withheld, ValueNone, ValueNone
        | _ -> PartyNames.NotGiven, PartyKind.Unknown ValueNone, ValueNone, ValueNone

    let private membership (entry: JsonElement) = tryObject "clenstvi" entry

    /// A member of a body, current until the register records its deletion or the end of the membership or function.
    let private officer (bodyType: string voption) (bodyName: string voption) (entry: JsonElement) : Officer =
        let name, kind, held, entity = party entry
        let held' = membership entry
        let tenure = held' |> ValueOption.bind (tryObject "clenstvi")
        let duty = held' |> ValueOption.bind (tryObject "funkce")

        let ended =
            tryDate "datumVymazu" entry
            |> ValueOption.orElse (tenure |> ValueOption.bind (tryDate "zanikClenstvi"))
            |> ValueOption.orElse (duty |> ValueOption.bind (tryDate "zanikFunkce"))

        let title =
            duty
            |> ValueOption.bind (tryString "nazev")
            |> ValueOption.orElse (tryString "nazevAngazma" entry)
            |> ValueOption.orElse bodyName
            |> ValueOption.defaultValue ""

        {
            Name = name
            Kind = kind
            Role =
                match bodyType with
                | ValueSome "STATUTARNI_ORGAN" when tryString "typAngazma" entry = ValueSome "STATUTARNI_ORGAN_CLEN" ->
                    OfficerRole.Director
                | _ -> OfficerRole.Other title
            Current = ended.IsNone
            AppointedOn =
                tenure
                |> ValueOption.bind (tryDate "vznikClenstvi")
                |> ValueOption.orElse (duty |> ValueOption.bind (tryDate "vznikFunkce"))
                |> ValueOption.orElse (tryDate "datumZapisu" entry)
            EndedOn = ended
            BornIn =
                held
                |> ValueOption.bind (fun person ->
                    if kind = PartyKind.Individual then
                        bornIn person
                    else
                        ValueNone)
            Nationality = ValueNone
            CountryOfResidence = ValueNone
            Address = held |> ValueOption.bind (tryObject "adresa") |> ValueOption.map addressOf
            Entity = entity
            IdentityVerification = ValueNone
        }

    let private bodyMembers (body: JsonElement) =
        let bodyType = tryString "typOrganu" body
        let bodyName = tryString "nazevOrganu" body
        array body "clenoveOrganu" |> List.map (officer bodyType bodyName)

    /// Statutory bodies, prokura among them, and other bodies such as a supervisory board.
    let officers (root: JsonElement) =
        let record = primaryRecord root

        match
            (array record "statutarniOrgany" @ array record "ostatniOrgany")
            |> List.collect bodyMembers
        with
        | [] -> NotAvailable "The public register lists no bodies for this entity, which doesn't say it has none"
        | found -> Available found

    let private shareOf (share: JsonElement) =
        match tryObject "velikostPodilu" share with
        | ValueSome size when tryString "typObnos" size = ValueSome "PROCENTA" ->
            match
                tryString "hodnota" size
                |> ValueOption.map (fun text ->
                    Decimal.TryParse(
                        text.Replace(',', '.'),
                        Globalization.NumberStyles.Number,
                        Globalization.CultureInfo.InvariantCulture
                    ))
            with
            | ValueSome(true, percent) when percent >= 0m && percent <= 100m && percent = Decimal.Truncate percent ->
                ValueSome { From = int percent; To = int percent }
            | _ -> ValueNone
        | _ -> ValueNone

    let private owner
        (asPublished: string)
        (since: DateOnly voption)
        (ended: DateOnly voption)
        (band: PercentBand voption)
        (entry: JsonElement)
        : Owner =
        let name, kind, held, entity = party entry

        {
            Name =
                if PartyNames.isPlaceholder name then
                    ValueNone
                else
                    ValueSome name
            Kind = kind
            Control =
                [
                    {
                        Kind = ControlKind.Ownership
                        Band = band
                        AsPublished = asPublished
                    }
                ]
            Current = ended.IsNone
            Since = since
            EndedOn = ended
            BornIn =
                held
                |> ValueOption.bind (fun person ->
                    if kind = PartyKind.Individual then
                        bornIn person
                    else
                        ValueNone)
            Nationality = ValueNone
            CountryOfResidence = ValueNone
            Address = held |> ValueOption.bind (tryObject "adresa") |> ValueOption.map addressOf
            Entity = entity
            IdentityVerification = ValueNone
        }

    /// Partners with their current share, and a sole shareholder.
    let owners (root: JsonElement) =
        let record = primaryRecord root

        let partners =
            array record "spolecnici"
            |> List.collect (fun body -> array body "spolecnik")
            |> List.choose (fun partner ->
                tryObject "osoba" partner
                |> ValueOption.map (fun person ->
                    let current =
                        array partner "podil"
                        |> List.tryFind (fun share -> (tryDate "datumVymazu" share).IsNone)

                    let ended =
                        tryDate "datumVymazu" partner
                        |> ValueOption.orElse (tryDate "datumVymazu" person)

                    owner
                        "Společník (partner)"
                        (tryDate "datumZapisu" partner)
                        ended
                        (current |> Option.map shareOf |> Option.defaultValue ValueNone)
                        person)
                |> ValueOption.toOption)

        let shareholders =
            array record "akcionari"
            |> List.collect (fun body ->
                array body "clenoveOrganu"
                |> List.map (fun holder ->
                    let ended =
                        tryDate "datumVymazu" holder |> ValueOption.orElse (tryDate "datumVymazu" body)

                    owner "Akcionář (sole shareholder)" (tryDate "datumZapisu" holder) ended ValueNone holder))

        match partners @ shareholders with
        | all when all |> List.exists _.Current ->
            let current = all |> List.filter _.Current

            Available
                {
                    Owners = all
                    Statements = []
                    Gaps =
                        [
                            if current |> List.exists (fun o -> o.Kind = PartyKind.Withheld) then
                                OwnershipGap.OwnerWithheld
                            if current |> List.exists (fun o -> o.Name.IsNone && o.Kind <> PartyKind.Withheld) then
                                OwnershipGap.OwnerUnreadable
                            if
                                current
                                |> List.exists (fun o ->
                                    o.Kind = PartyKind.LegalEntity
                                    && (o.Entity |> ValueOption.bind _.RegistrationNumber).IsNone)
                            then
                                OwnershipGap.EntityNotIdentified
                        ]
                }
        | _ ->
            NotAvailable
                "The public register lists no current partner or sole shareholder; an a.s. with several shareholders has none listed, so this doesn't say there are none"

/// Czechia as an ICompanyRegistry. The economic subject is read from ARES's own record, and officers and owners from its public register copy.
[<Sealed>]
type internal CzechiaRegistry(connection: SourceConnection) =
    static let searchLimit = 100

    let client =
        SourceClient(connection, SourceAnswer.pacing 500 (TimeSpan.FromMinutes 1.))

    let coverage = Czechia.coverage
    let baseUri = "https://ares.gov.cz/ekonomicke-subjekty-v-be/rest"

    let offIco number send =
        match Czechia.normalIco number with
        | ValueSome ico -> send ico
        | ValueNone -> Threading.Tasks.Task.FromResult(Error(RegistryFailure.InvalidRequest "An IČO has eight digits"))

    /// The public register's record. A 404 that says NENALEZENO means the entity isn't in the public register.
    let readPublicRegister (ico: string) (read: JsonElement -> Availability<'T>) (ct: CancellationToken) =
        task {
            match! client.GetAsync(Uri $"{baseUri}/ekonomicke-subjekty-vr/{ico}", ct) with
            | Error failure -> return Error failure
            | Ok reply when reply.Status = 404 ->
                match SourceAnswer.json reply Czechia.isNotFound with
                | Ok true ->
                    return
                        Ok(
                            SourceAnswer.evidence
                                coverage
                                reply
                                ValueNone
                                (NotAvailable
                                    "The entity isn't in the public register, which lists bodies and owners only for those in it")
                        )
                | Ok false -> return Error(RegistryFailure.RequestRefused "HTTP 404")
                | Error failure -> return Error failure
            | Ok reply ->
                return
                    SourceAnswer.json reply (fun root ->
                        read root, SourceAnswer.dateOfTimestamp "datumAktualizace" (Czechia.primaryRecord root))
                    |> Result.map (fun (value, asOf) -> SourceAnswer.evidence coverage reply asOf value)
        }

    interface ICompanyRegistry with
        member _.Coverage = coverage

        member _.TryFindAsync(number: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                offIco number (fun ico ->
                    task {
                        match! client.GetAsync(Uri $"{baseUri}/ekonomicke-subjekty/{ico}", ct) with
                        | Error failure -> return Error failure
                        | Ok reply when reply.Status = 404 ->
                            match SourceAnswer.json reply Czechia.isNotFound with
                            | Ok true -> return Ok(SourceAnswer.evidence coverage reply ValueNone Czechia.notFound)
                            | Ok false -> return Error(RegistryFailure.RequestRefused "HTTP 404")
                            | Error failure -> return Error failure
                        | Ok reply ->
                            return
                                SourceAnswer.json reply (fun root ->
                                    Czechia.lookup ico root, SourceAnswer.dateOfTimestamp "datumAktualizace" root)
                                |> Result.map (fun (company, asOf) ->
                                    SourceAnswer.evidence coverage reply asOf (Available(ValueSome company)))
                    }))

        member _.SearchAsync(name: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                if String.IsNullOrWhiteSpace name then
                    Threading.Tasks.Task.FromResult(Error(RegistryFailure.InvalidRequest "A search needs a term"))
                else
                    task {
                        let body =
                            JsonSerializer.Serialize(
                                dict [ "obchodniJmeno", box (name.Trim()); "start", box 0; "pocet", box searchLimit ]
                            )

                        let content () : HttpContent =
                            new StringContent(body, Encoding.UTF8, "application/json")

                        match! client.PostAsync(Uri $"{baseUri}/ekonomicke-subjekty/vyhledat", content, [], ct) with
                        | Error failure -> return Error failure
                        | Ok reply when reply.Status = 404 ->
                            match SourceAnswer.json reply Czechia.isNotFound with
                            | Ok true ->
                                return
                                    Ok(
                                        SourceAnswer.evidence
                                            coverage
                                            reply
                                            ValueNone
                                            (Available
                                                {
                                                    Matches = []
                                                    TotalMatches = ValueSome 0
                                                    Complete = false
                                                })
                                    )
                            | Ok false -> return Error(RegistryFailure.RequestRefused "HTTP 404")
                            | Error failure -> return Error failure
                        | Ok reply ->
                            return
                                SourceAnswer.json reply (Czechia.search searchLimit)
                                |> Result.map (fun found ->
                                    SourceAnswer.evidence coverage reply ValueNone (Available found))
                    })

        member _.OfficersAsync(number: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                offIco number (fun ico -> readPublicRegister ico Czechia.officers ct))

        member _.OwnersAsync(number: string, jurisdiction: Jurisdiction, _: OwnerPurpose, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                offIco number (fun ico -> readPublicRegister ico Czechia.owners ct))
