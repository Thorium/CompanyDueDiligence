namespace CompanyDueDiligence

open System
open System.Text.Json
open System.Text.RegularExpressions
open System.Threading
open CompanyDueDiligence.Json

/// Brønnøysund's Enhetsregisteret API, read as it answered on 2026-11-08, with its OpenAPI document and role type lists.
module internal Norway =

    let coverage: RegistryCoverage =
        {
            Registry = "Brønnøysund Enhetsregisteret (Norway)"
            Jurisdictions = [ Jurisdiction.ofCode "NO" ]
            Standing =
                Support.Partial
                    "The bankruptcy, liquidation, compulsory dissolution and restructuring markers, and deletion; the register has no status word for a live entity"
            Concerns =
                Support.Partial
                    "Bankruptcy, liquidation and restructuring are stated while running; overdue filings aren't published"
            Addresses = Support.Supported
            Search = Support.Partial "The first page of up to 100 entities, with the total the register states"
            Officers = Support.Partial "Roles as the register lists them, a resigned one marked as such without a date"
            Owners =
                Support.NotOffered
                    "The register lists no shareholders; partners and a sole proprietor appear among the roles"
            Cost = AccessCost.Free
            RateLimit = RateLimitPolicy.NotPublished(2, TimeSpan.FromSeconds 1.)
            Licence = "NLOD 2.0 (Norwegian Licence for Open Government Data), as the API documentation states"
        }

    let private organisationNumberRegex = Regex @"\A[0-9]{9}\z"

    /// Nine digits whose last is the modulus 11 check digit of the other eight. Spaces, as the number is often written, are taken out.
    let normalOrganisationNumber (number: string) =
        let normal = if isNull number then "" else number.Replace(" ", "")

        if organisationNumberRegex.IsMatch normal then
            let digits = normal |> Seq.map (fun c -> int c - int '0') |> Array.ofSeq
            let weights = [| 3; 2; 7; 6; 5; 4; 3; 2 |]

            let sum =
                Array.fold2 (fun total digit weight -> total + digit * weight) 0 digits[0..7] weights

            match 11 - sum % 11 with
            | 11 when digits[8] = 0 -> ValueSome normal
            | 10 -> ValueNone
            | check when check = digits[8] -> ValueSome normal
            | _ -> ValueNone
        else
            ValueNone

    let private dateOf name e = SourceAnswer.dateOfTimestamp name e

    /// A running proceeding decides, then the liquidation markers, then deletion. A live entity has no status word, so it is Registered with none;
    /// one missing the three markers the API always sends is Unknown.
    let standing (e: JsonElement) =
        let marked name = tryFlag name e = ValueSome true

        match tryString "respons_klasse" e, dateOf "slettedato" e with
        | ValueSome "SlettetEnhet", _
        | _, ValueSome _ -> CompanyStanding.Closed "Slettet (struck off)"
        | _ when marked "konkurs" -> CompanyStanding.InInsolvency "Konkurs (bankrupt)"
        | _ when (dateOf "underRekonstruksjonsforhandlingDato" e).IsSome ->
            CompanyStanding.InInsolvency "Under rekonstruksjonsforhandling (restructuring negotiation)"
        | _ when (dateOf "underUtenlandskInsolvensbehandlingDato" e).IsSome ->
            CompanyStanding.InInsolvency "Under utenlandsk insolvensbehandling (foreign insolvency proceeding)"
        | _ when marked "underTvangsavviklingEllerTvangsopplosning" ->
            CompanyStanding.WindingUp
                "Under tvangsavvikling eller tvangsoppløsning (compulsory liquidation or dissolution)"
        | _ when marked "underAvvikling" -> CompanyStanding.WindingUp "Under avvikling (liquidation)"
        | _ ->
            match
                tryFlag "konkurs" e, tryFlag "underAvvikling" e, tryFlag "underTvangsavviklingEllerTvangsopplosning" e
            with
            | ValueSome false, ValueSome false, ValueSome false -> CompanyStanding.Registered ValueNone
            | _ -> CompanyStanding.Unknown ValueNone

    /// The markers that are set, as the API names them, or the deletion.
    let private statusAsPublished (e: JsonElement) =
        match dateOf "slettedato" e with
        | ValueSome deleted ->
            ValueSome(
                "slettet "
                + deleted.ToString("yyyy-MM-dd", Globalization.CultureInfo.InvariantCulture)
            )
        | ValueNone ->
            [ "konkurs"; "underAvvikling"; "underTvangsavviklingEllerTvangsopplosning" ]
            |> List.filter (fun name -> tryFlag name e = ValueSome true)
            |> function
                | [] -> ValueNone
                | set -> ValueSome(String.concat ", " set)

    let private addressOf kind (e: JsonElement) : CompanyAddress voption =
        let lines = strings e "adresse" |> List.map _.Trim() |> List.filter ((<>) "")

        match lines, SourceAnswer.trimmed "poststed" e with
        | [], ValueNone -> ValueNone
        | _ ->
            ValueSome
                {
                    Kind = kind
                    Address =
                        {
                            Lines = lines
                            Locality = SourceAnswer.trimmed "poststed" e
                            Region = SourceAnswer.trimmed "kommune" e
                            PostalCode = SourceAnswer.trimmed "postnummer" e
                            Country = SourceAnswer.trimmed "land" e
                        }
                }

    /// NUF is a Norwegian-registered foreign enterprise.
    let private role (form: string voption) =
        match form with
        | ValueSome "NUF" -> RecordRole.ForeignRegistration
        | ValueSome _ -> RecordRole.Home
        | ValueNone -> RecordRole.NotStated

    let private concerns (standing: CompanyStanding) =
        match standing with
        | CompanyStanding.InInsolvency _
        | CompanyStanding.WindingUp _ ->
            {
                Stated = [ CompanyConcern.InsolvencyHistory ]
                Unconfirmed = CompanyConcern.all |> List.except [ CompanyConcern.InsolvencyHistory ]
            }
        | _ -> CompanyConcerns.notPublished

    /// An entity, live or struck off. A struck-off one keeps its name, form and deletion date only.
    let company (e: JsonElement) : Company =
        let name = required "navn" tryString e
        let form = tryObject "organisasjonsform" e
        let formCode = form |> ValueOption.bind (tryString "kode")
        let standing = standing e

        let addresses =
            [
                tryObject "forretningsadresse" e
                |> ValueOption.bind (addressOf (AddressKind.Other "Forretningsadresse (business address)"))
                tryObject "postadresse" e |> ValueOption.bind (addressOf AddressKind.Postal)
            ]
            |> List.choose ValueOption.toOption

        {
            Name = name
            OtherNames =
                array e "historiskeNavn"
                |> List.choose (fun held -> tryString "navn" held |> ValueOption.toOption)
                |> List.filter ((<>) name)
                |> List.distinct
            Number = required "organisasjonsnummer" tryString e
            Jurisdiction = Jurisdiction.ofCode "NO"
            JurisdictionAsPublished = ValueNone
            Standing = standing
            Concerns = concerns standing
            StatusAsPublished = statusAsPublished e
            LegalForm =
                form
                |> ValueOption.bind (tryString "beskrivelse")
                |> ValueOption.orElse formCode
            IncorporatedOn =
                tryDate "stiftelsesdato" e
                |> ValueOption.orElse (tryDate "registreringsdatoEnhetsregisteret" e)
            CeasedOn = dateOf "slettedato" e
            Addresses =
                match addresses with
                | [] -> NotAvailable "The answer holds no address"
                | _ -> Available addresses
            Role = role formCode
            HomeRegistration = ValueNone
            EstablishmentOf = ValueNone
        }

    /// The entity with this number. Another entity in the answer fails the read rather than being taken for it.
    let lookup (number: string) (root: JsonElement) =
        let found = company root

        if found.Number = number then
            found
        else
            raise (InvalidJson "The answer holds another entity")

    /// A 410 says the entity was removed for legal reasons. It carries the number and deletion date, and no name.
    let removed (number: string) (root: JsonElement) : Company =
        let held = required "organisasjonsnummer" tryString root

        if held <> number then
            raise (InvalidJson "The answer holds another entity")

        {
            Name = PartyNames.NotGiven
            OtherNames = []
            Number = held
            Jurisdiction = Jurisdiction.ofCode "NO"
            JurisdictionAsPublished = ValueNone
            Standing = CompanyStanding.Closed "Removed for legal reasons (HTTP 410)"
            Concerns = CompanyConcerns.notPublished
            StatusAsPublished = ValueSome "410 Gone"
            LegalForm = ValueNone
            IncorporatedOn = ValueNone
            CeasedOn = dateOf "slettedato" root
            Addresses = NotAvailable "The register removed this entity for legal reasons and gives nothing more"
            Role = RecordRole.NotStated
            HomeRegistration = ValueNone
            EstablishmentOf = ValueNone
        }

    /// What a 404's body is.
    [<RequireQualifiedAccess>]
    type NotFoundReply =
        /// An empty body. Brønnøysund answers it for a number it never held and for an entity removed for legal reasons alike.
        | Empty
        /// Brønnøysund's error object with status 404, a message, and the path asked as its sti: the number isn't held there.
        | Own
        /// Any other body, which isn't Brønnøysund's answer.
        | Other

    /// Reads a 404's body for the path asked.
    let notFoundReply (path: string) (body: string) =
        if String.IsNullOrWhiteSpace body then
            NotFoundReply.Empty
        else
            try
                use document = JsonDocument.Parse body
                let root = document.RootElement

                if
                    root.ValueKind = JsonValueKind.Object
                    && tryInt "status" root = ValueSome 404
                    && (tryString "feilmelding" root).IsSome
                    && tryString "sti" root = ValueSome path
                then
                    NotFoundReply.Own
                else
                    NotFoundReply.Other
            with :? JsonException ->
                NotFoundReply.Other

    /// Both 404s empty: the register answers nothing, as it does for an entity removed for legal reasons.
    let answersNothing =
        NotAvailable "Brønnøysund answers nothing for this number, as it does for an entity removed for legal reasons"

    /// A sub-unit (underenhet) is a place of business of an entity, not an entity, so its number says nothing of a company: the reason names its
    /// parent. Another number in the answer fails the read.
    let subUnit (number: string) (root: JsonElement) =
        let held = required "organisasjonsnummer" tryString root

        if held <> number then
            raise (InvalidJson "The answer holds another sub-unit")

        match tryString "overordnetEnhet" root with
        | ValueSome parent ->
            $"{number} is a sub-unit (underenhet) of {parent}: a place of business, not an entity, so look up {parent} instead"
        | ValueNone ->
            $"{number} is a sub-unit (underenhet): a place of business, not an entity, and the answer names no parent"

    /// The daily manager and the board's chair, members and deputy chair. Any other role keeps its own description.
    let private officerRole (kind: JsonElement voption) =
        match kind |> ValueOption.bind (tryString "kode") with
        | ValueSome("DAGL" | "LEDE" | "MEDL" | "NEST") -> OfficerRole.Director
        | code ->
            OfficerRole.Other(
                kind
                |> ValueOption.bind (tryString "beskrivelse")
                |> ValueOption.orElse code
                |> ValueOption.defaultValue ""
            )

    let private bornIn (person: JsonElement) =
        tryDate "fodselsdato" person
        |> ValueOption.map (fun date -> { Year = date.Year; Month = date.Month })

    let private officer (e: JsonElement) : Officer =
        let person = tryObject "person" e
        let entity = tryObject "enhet" e

        let personName =
            person
            |> ValueOption.bind (tryObject "navn")
            |> ValueOption.map (fun name ->
                [
                    tryString "fornavn" name
                    tryString "mellomnavn" name
                    tryString "etternavn" name
                ]
                |> List.choose ValueOption.toOption
                |> String.concat " ")
            |> ValueOption.filter ((<>) "")

        let entityName =
            entity
            |> ValueOption.bind (fun held ->
                match strings held "navn" with
                | [] -> tryString "navn" held
                | parts -> ValueSome(String.concat " " parts))

        {
            Name =
                personName
                |> ValueOption.orElse entityName
                |> ValueOption.defaultValue PartyNames.NotGiven
            Kind =
                match person, entity with
                | ValueSome _, _ -> PartyKind.Individual
                | _, ValueSome _ -> PartyKind.LegalEntity
                | _ -> PartyKind.Unknown ValueNone
            Role = officerRole (tryObject "type" e)
            // A role deregistered or resigned from, or held by a person the register records as dead, is no longer held.
            Current =
                not (
                    flag "avregistrert" e
                    || tryFlag "fratraadt" e = ValueSome true
                    || (person |> ValueOption.bind (tryFlag "erDoed")) = ValueSome true
                )
            AppointedOn = ValueNone
            EndedOn = ValueNone
            BornIn = person |> ValueOption.bind bornIn
            Nationality = ValueNone
            CountryOfResidence = ValueNone
            Address = ValueNone
            Entity =
                entity
                |> ValueOption.map (fun held ->
                    {
                        LegalName = entityName
                        LegalForm = tryObject "organisasjonsform" held |> ValueOption.bind (tryString "beskrivelse")
                        GoverningLaw = ValueNone
                        Country = ValueSome "Norge"
                        Register = ValueSome coverage.Registry
                        RegistrationNumber = tryString "organisasjonsnummer" held
                    })
            IdentityVerification = ValueNone
        }

    /// Every role in every group. An answer without the groups list is unreadable; an empty one says nothing.
    let officers (root: JsonElement) =
        match tryProp "rollegrupper" root with
        | ValueSome groups when groups.ValueKind = JsonValueKind.Array ->
            match
                groups.EnumerateArray()
                |> Seq.collect (fun group -> array group "roller")
                |> List.ofSeq
            with
            | [] -> NotAvailable "The register lists no roles for this entity, which doesn't say it has none"
            | roles -> Available(roles |> List.map officer)
        | _ -> raise (InvalidJson "The answer has no role groups")

    /// A page of entities. With no match the API leaves the list out, and the page still states its total. Never complete, as the search
    /// leaves out deleted entities.
    let search (limit: int) (root: JsonElement) =
        let page =
            match tryObject "page" root with
            | ValueSome page -> page
            | ValueNone -> raise (InvalidJson "The answer has no page")

        let entities =
            tryObject "_embedded" root
            |> ValueOption.map (fun embedded -> array embedded "enheter")
            |> ValueOption.defaultValue []

        let found =
            OpenData.matches limit (tryInt "totalElements" page) (entities |> List.map company)

        { found with Complete = false }

/// Norway as an ICompanyRegistry. Brønnøysund's error object for both the entity and the sub-unit says the number isn't held; an empty 404,
/// which it also answers for an entity removed for legal reasons, says nothing. A sub-unit is NotAvailable, naming its parent; a 410 says the
/// entity was removed. Search is never complete, as it leaves out deleted entities.
[<Sealed>]
type internal NorwayRegistry(connection: SourceConnection) =
    static let searchLimit = 100

    let client =
        SourceClient(connection, SourceAnswer.pacing 2 (TimeSpan.FromSeconds 1.), set [ 410 ])

    let coverage = Norway.coverage
    let baseUri = "https://data.brreg.no/enhetsregisteret/api/enheter"
    let subUnitsUri = "https://data.brreg.no/enhetsregisteret/api/underenheter"

    let offNumber number send =
        match Norway.normalOrganisationNumber number with
        | ValueSome normal -> send normal
        | ValueNone ->
            Threading.Tasks.Task.FromResult(
                Error(
                    RegistryFailure.InvalidRequest
                        "An organisation number has nine digits, the last a modulus 11 check digit"
                )
            )

    interface ICompanyRegistry with
        member _.Coverage = coverage

        member _.TryFindAsync(number: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                offNumber number (fun normal ->
                    task {
                        let entityUri = Uri $"{baseUri}/{normal}"
                        let subUnitUri = Uri $"{subUnitsUri}/{normal}"

                        match! client.GetAsync(entityUri, ct) with
                        | Error failure -> return Error failure
                        | Ok reply when
                            reply.Status = 404
                            && Norway.notFoundReply entityUri.AbsolutePath reply.Body = Norway.NotFoundReply.Other
                            ->
                            return Error(RegistryFailure.RequestRefused "HTTP 404 without Brønnøysund's own answer")
                        | Ok reply when reply.Status = 404 ->
                            match! client.GetAsync(subUnitUri, ct) with
                            | Error failure -> return Error failure
                            | Ok sub when sub.Status = 404 ->
                                match
                                    Norway.notFoundReply entityUri.AbsolutePath reply.Body,
                                    Norway.notFoundReply subUnitUri.AbsolutePath sub.Body
                                with
                                | Norway.NotFoundReply.Own, Norway.NotFoundReply.Own ->
                                    return
                                        Ok
                                            { SourceAnswer.evidence coverage reply ValueNone (Available ValueNone) with
                                                Raw = ValueNone
                                            }
                                | _, Norway.NotFoundReply.Other ->
                                    return
                                        Error(
                                            RegistryFailure.RequestRefused
                                                "HTTP 404 without Brønnøysund's own answer for the sub-unit lookup"
                                        )
                                | _ ->
                                    return
                                        Ok
                                            { SourceAnswer.evidence coverage reply ValueNone Norway.answersNothing with
                                                Raw = ValueNone
                                            }
                            | Ok sub when sub.Status = 200 ->
                                return
                                    SourceAnswer.json sub (Norway.subUnit normal)
                                    |> Result.map (fun reason ->
                                        SourceAnswer.evidence coverage sub ValueNone (NotAvailable reason))
                            | Ok sub when sub.Status = 410 ->
                                return
                                    Ok(
                                        SourceAnswer.evidence
                                            coverage
                                            sub
                                            ValueNone
                                            (NotAvailable
                                                $"{normal} is a sub-unit (underenhet) the register removed for legal reasons, not an entity")
                                    )
                            | Ok sub ->
                                return
                                    Error(RegistryFailure.RequestRefused $"HTTP {sub.Status} for the sub-unit lookup")
                        | Ok reply when reply.Status = 410 ->
                            return
                                SourceAnswer.json reply (Norway.removed normal)
                                |> Result.map (fun company ->
                                    SourceAnswer.evidence coverage reply ValueNone (Available(ValueSome company)))
                        | Ok reply ->
                            return
                                SourceAnswer.json reply (Norway.lookup normal)
                                |> Result.map (fun company ->
                                    SourceAnswer.evidence coverage reply ValueNone (Available(ValueSome company)))
                    }))

        member _.SearchAsync(name: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                if String.IsNullOrWhiteSpace name then
                    Threading.Tasks.Task.FromResult(Error(RegistryFailure.InvalidRequest "A search needs a term"))
                elif name.Trim().Length > 180 then
                    Threading.Tasks.Task.FromResult(
                        Error(RegistryFailure.InvalidRequest "A name search takes up to 180 characters")
                    )
                else
                    task {
                        let uri =
                            Uri $"{baseUri}?navn={Uri.EscapeDataString(name.Trim())}&size={searchLimit}"

                        match! client.GetAsync(uri, ct) with
                        | Error failure -> return Error failure
                        | Ok reply when reply.Status <> 200 ->
                            return Error(RegistryFailure.RequestRefused $"HTTP {reply.Status}")
                        | Ok reply ->
                            return
                                SourceAnswer.json reply (Norway.search searchLimit)
                                |> Result.map (fun found ->
                                    SourceAnswer.evidence coverage reply ValueNone (Available found))
                    })

        member _.OfficersAsync(number: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                offNumber number (fun normal ->
                    task {
                        match! client.GetAsync(Uri $"{baseUri}/{normal}/roller", ct) with
                        | Error failure -> return Error failure
                        | Ok reply when reply.Status = 404 || reply.Status = 410 ->
                            return
                                Ok
                                    { SourceAnswer.evidence
                                          coverage
                                          reply
                                          ValueNone
                                          (NotAvailable
                                              "The register gives no roles for this number: it isn't held, has been deleted, or has no roles") with
                                        Raw = ValueNone
                                    }
                        | Ok reply ->
                            return
                                SourceAnswer.json reply Norway.officers
                                |> Result.map (SourceAnswer.evidence coverage reply ValueNone)
                    }))

        member _.OwnersAsync(_: string, jurisdiction: Jurisdiction, _: OwnerPurpose, _: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                Threading.Tasks.Task.FromResult(
                    Ok(
                        SourceAnswer.unasked
                            coverage
                            client.Now
                            (NotAvailable
                                "The register lists no shareholders; partners and a sole proprietor appear among the roles")
                    )
                ))
