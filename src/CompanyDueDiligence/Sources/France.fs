namespace CompanyDueDiligence

open System
open System.Text.Json
open System.Text.RegularExpressions
open System.Threading
open CompanyDueDiligence.Json

/// France's Recherche d'entreprises API, read as it answered on 2026-11-06.
module internal France =

    let coverage: RegistryCoverage =
        {
            Registry = "Recherche d'entreprises (France)"
            Jurisdictions = [ Jurisdiction.ofCode "FR" ]
            Standing =
                Support.Partial
                    "A and C, SIRENE's administrative state: A says the unit isn't ceased, not that it is free of collective proceedings (safeguard, receivership, judicial liquidation), which the API doesn't carry. A company left out of the API has none"
            Concerns = Support.NotOffered "The API states no overdue filings, disputes or insolvency proceedings"
            Addresses = Support.Partial "The head office; a company under partial diffusion hides it"
            Search = Support.Partial "25 matches, ranked by the API; non-diffusible companies are left out"
            Officers =
                Support.Partial
                    "Current officers as the API lists them, auditors among them; an empty list says nothing"
            Owners = Support.NotOffered "The API lists no beneficial owners"
            Cost = AccessCost.Free
            RateLimit =
                RateLimitPolicy.Published(
                    7,
                    TimeSpan.FromSeconds 1.,
                    "openapi.json: 7 per second per address, and 30 per second per ASN"
                )
            Licence = "Licence Ouverte 2.0, per data.gouv.fr; the API's own record states none"
        }

    /// Non-diffusible companies and those refused registration are left out of the API, so its silence says nothing about a number.
    [<Literal>]
    let leftOut =
        "The API leaves out non-diffusible companies and those refused registration, so not finding this number proves nothing"

    let private sirenRegex = Regex @"\A[0-9]{9}\z"

    /// Nine digits. Spaces, as a SIREN is often written, are taken out.
    let normalSiren (number: string) =
        let normal = if isNull number then "" else number.Replace(" ", "")

        if sirenRegex.IsMatch normal then
            ValueSome normal
        else
            ValueNone

    /// The API answers any nine digits with a record holding the number and no name. Such a stub, or a name given as [ND], is not a company.
    let isCompany (e: JsonElement) =
        match
            tryString "nom_complet" e
            |> ValueOption.orElse (tryString "nom_raison_sociale" e)
        with
        | ValueSome name -> name.Trim() <> "" && not (name.Contains "[ND]")
        | ValueNone -> false

    let private standing (status: string voption) =
        match status with
        | ValueSome "A" -> CompanyStanding.Active
        | ValueSome "C" -> CompanyStanding.Closed "C: cessée"
        | other -> CompanyStanding.Unknown other

    let private addresses (e: JsonElement) =
        match tryString "statut_diffusion" e, tryObject "siege" e with
        | ValueSome "P", _ -> NotAvailable "The company is under partial diffusion, which hides its address"
        | _, ValueNone -> NotAvailable "The answer holds no head office"
        | _, ValueSome head ->
            match SourceAnswer.trimmed "adresse" head with
            | ValueNone -> NotAvailable "The head office holds no address"
            | ValueSome text when text.Contains "[ND]" -> NotAvailable "The head office address isn't published"
            | ValueSome text ->
                Available
                    [
                        {
                            Kind = AddressKind.RegisteredOffice
                            Address =
                                {
                                    Lines = [ text ]
                                    Locality = SourceAnswer.trimmed "libelle_commune" head
                                    Region = ValueNone
                                    PostalCode = SourceAnswer.trimmed "code_postal" head
                                    Country = SourceAnswer.trimmed "libelle_pays_etranger" head
                                }
                        }
                    ]

    /// Category 31xx is a foreign company on the RCS. Unverified beyond the INSEE category table: no such company has been read live.
    let private role (form: string voption) =
        match form with
        | ValueSome code when code.StartsWith "31" -> RecordRole.ForeignRegistration
        | ValueSome _ -> RecordRole.Home
        | ValueNone -> RecordRole.NotStated

    let company (e: JsonElement) : Company =
        let form = tryString "nature_juridique" e
        let status = tryString "etat_administratif" e

        {
            Name =
                tryString "nom_complet" e
                |> ValueOption.orElse (tryString "nom_raison_sociale" e)
                |> ValueOption.defaultValue ""
            Number = required "siren" tryString e
            Jurisdiction = Jurisdiction.ofCode "FR"
            JurisdictionAsPublished = ValueNone
            Standing = standing status
            Concerns = CompanyConcerns.notPublished
            StatusAsPublished = status
            LegalForm = form
            IncorporatedOn = tryDate "date_creation" e
            CeasedOn = dateIfAny "date_fermeture" e
            Addresses = addresses e
            Role = role form
            HomeRegistration = ValueNone
            OtherNames = []
            EstablishmentOf = ValueNone
        }

    let private results (root: JsonElement) =
        match tryProp "results" root with
        | ValueSome list when list.ValueKind = JsonValueKind.Array -> list.EnumerateArray() |> List.ofSeq
        | _ -> raise (InvalidJson "The answer has no results list")

    /// The record for this number, if the answer holds one that is a company.
    let recordOf (siren: string) (root: JsonElement) =
        results root
        |> List.tryFind (fun e -> tryString "siren" e = ValueSome siren)
        |> Option.filter isCompany
        |> ValueOption.ofOption

    let private officerRole (quality: string voption) =
        match quality with
        | ValueNone -> OfficerRole.Other ""
        | ValueSome word ->
            let lower = word.ToLowerInvariant()

            if
                lower.Contains "gérant"
                || lower.Contains "administrateur"
                || lower.Contains "président"
                || lower.Contains "directeur"
            then
                OfficerRole.Director
            else
                OfficerRole.Other word

    let private birth (e: JsonElement) =
        match tryString "date_de_naissance" e with
        | ValueSome text when text.Length >= 7 ->
            match Int32.TryParse(text.AsSpan(0, 4)), Int32.TryParse(text.AsSpan(5, 2)) with
            | (true, year), (true, month) when month >= 1 && month <= 12 -> ValueSome { Year = year; Month = month }
            | _ -> ValueNone
        | _ -> ValueNone

    /// A person with no name published comes through as withheld, not dropped.
    let private officer (e: JsonElement) : Officer =
        let person =
            [ tryString "prenoms" e; tryString "nom" e ]
            |> List.choose ValueOption.toOption
            |> String.concat " "

        let isEntity = tryString "type_dirigeant" e = ValueSome "personne morale"

        {
            Name =
                if isEntity then
                    tryString "denomination" e |> ValueOption.defaultValue PartyNames.NotPublished
                elif person = "" then
                    PartyNames.NotPublished
                else
                    person
            Kind =
                if isEntity then PartyKind.LegalEntity
                elif person = "" then PartyKind.Withheld
                else PartyKind.Individual
            Role = officerRole (tryString "qualite" e)
            Current = true
            AppointedOn = ValueNone
            EndedOn = ValueNone
            BornIn = birth e
            Nationality = tryString "nationalite" e
            CountryOfResidence = ValueNone
            Address = ValueNone
            Entity =
                if isEntity then
                    ValueSome
                        {
                            LegalName = tryString "denomination" e
                            LegalForm = ValueNone
                            GoverningLaw = ValueNone
                            Country = ValueSome "France"
                            Register = ValueSome coverage.Registry
                            RegistrationNumber = tryString "siren" e
                        }
                else
                    ValueNone
            IdentityVerification = ValueNone
        }

    let officers (record: JsonElement) =
        match array record "dirigeants" with
        | [] -> NotAvailable "The API lists no officers for this company, which doesn't say it has none"
        | items -> Available(items |> List.map officer)

    let matchOf (e: JsonElement) : CompanyMatch =
        {
            Name = tryString "nom_complet" e |> ValueOption.defaultValue ""
            Number = required "siren" tryString e
            StatusAsPublished = tryString "etat_administratif" e
            Address =
                tryObject "siege" e
                |> ValueOption.bind (SourceAnswer.trimmed "adresse")
                |> ValueOption.map (fun text ->
                    {
                        Lines = [ text ]
                        Locality = ValueNone
                        Region = ValueNone
                        PostalCode = ValueNone
                        Country = ValueNone
                    })
        }

    let search (root: JsonElement) =
        let records = results root
        let matches = records |> List.filter isCompany |> List.map matchOf

        {
            Matches = matches
            TotalMatches = tryInt "total_results" root
            Complete = false
        }

    let personSearch =
        Support.Partial
            "Officers who are people, among the 25 companies the API ranks first for the surname and forenames; it gives no dates, and leaves out non-diffusible companies, so Complete is never true"

    /// The officers of the companies found who are people named as asked. The API finds companies by any officer's name, so the rest are dropped.
    let people (asked: PersonName) (root: JsonElement) : PersonSearchResult =
        let matches =
            results root
            |> List.filter isCompany
            |> List.collect (fun company ->
                array company "dirigeants"
                |> List.filter (fun e -> tryString "type_dirigeant" e = ValueSome "personne physique")
                |> List.choose (fun e ->
                    match PeopleSearch.joined [ "prenoms"; "nom" ] e with
                    | ValueSome name when PersonName.matches asked name ->
                        Some
                            {
                                PersonMatch.Name = name
                                Role = SourceAnswer.trimmed "qualite" e
                                Current = ValueNone
                                CompanyName =
                                    tryString "nom_complet" company
                                    |> ValueOption.orElse (tryString "nom_raison_sociale" company)
                                CompanyNumber = tryString "siren" company
                                CompanyStatus = tryString "etat_administratif" company
                                AppointedOn = ValueNone
                                EndedOn = ValueNone
                            }
                    | _ -> None))

        {
            PersonSearchResult.Matches = matches
            TotalMatches = tryInt "total_results" root
            Complete = false
        }

/// France as an ICompanyRegistry. Not finding a number is NotAvailable, never "not held": the API leaves some companies out.
[<Sealed>]
type internal FranceRegistry(connection: SourceConnection) =
    let client =
        SourceClient(connection, SourceAnswer.pacing 7 (TimeSpan.FromSeconds 1.))

    let coverage = France.coverage

    let searchUri (query: string) size =
        Uri $"https://recherche-entreprises.api.gouv.fr/search?q={Uri.EscapeDataString query}&page=1&per_page={size}"

    let offSiren number send =
        match France.normalSiren number with
        | ValueSome siren -> send siren
        | ValueNone -> Threading.Tasks.Task.FromResult(Error(RegistryFailure.InvalidRequest "A SIREN has nine digits"))

    /// The record's own JSON text is what is kept as raw.
    let readRecord (siren: string) (ct: CancellationToken) =
        task {
            match! client.GetAsync(searchUri siren 25, ct) with
            | Error failure -> return Error failure
            | Ok reply ->
                return
                    SourceAnswer.json reply (fun root ->
                        France.recordOf siren root
                        |> ValueOption.map (fun record ->
                            France.company record,
                            France.officers record,
                            record.GetRawText(),
                            SourceAnswer.dateOfTimestamp "date_mise_a_jour" record))
                    |> Result.map (fun record -> reply, record)
        }

    /// The surname as nom_personne and the forenames, if any, as prenoms_personne.
    let peopleUri (name: PersonName) =
        let forenames =
            match name.Forenames with
            | [] -> ""
            | words -> "&prenoms_personne=" + Uri.EscapeDataString(String.concat " " words)

        Uri
            $"https://recherche-entreprises.api.gouv.fr/search?nom_personne={Uri.EscapeDataString name.Surname}{forenames}&page=1&per_page=25"

    interface IPersonSearch with
        member _.PersonSearch = France.personSearch

        member _.SearchPeopleAsync(name: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                PeopleSearch.withName name (fun asked ->
                    task {
                        match! client.GetAsync(peopleUri asked, ct) with
                        | Error failure -> return Error failure
                        | Ok reply when reply.Status = 404 -> return Error(RegistryFailure.RequestRefused "HTTP 404")
                        | Ok reply ->
                            return
                                SourceAnswer.json reply (France.people asked)
                                |> Result.map (fun found ->
                                    SourceAnswer.evidence coverage reply ValueNone (Available found))
                    }))

    interface ICompanyRegistry with
        member _.Coverage = coverage

        member _.TryFindAsync(number: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                offSiren number (fun siren ->
                    task {
                        match! readRecord siren ct with
                        | Error failure -> return Error failure
                        | Ok(reply, ValueNone) ->
                            return Ok(SourceAnswer.evidence coverage reply ValueNone (NotAvailable France.leftOut))
                        | Ok(reply, ValueSome(company, _, raw, asOf)) ->
                            return
                                Ok
                                    { SourceAnswer.evidence coverage reply asOf (Available(ValueSome company)) with
                                        Raw = ValueSome raw
                                    }
                    }))

        member _.SearchAsync(name: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                if String.IsNullOrWhiteSpace name then
                    Threading.Tasks.Task.FromResult(Error(RegistryFailure.InvalidRequest "A search needs a term"))
                else
                    task {
                        match! client.GetAsync(searchUri name 25, ct) with
                        | Error failure -> return Error failure
                        | Ok reply ->
                            return
                                SourceAnswer.json reply France.search
                                |> Result.map (fun found ->
                                    SourceAnswer.evidence coverage reply ValueNone (Available found))
                    })

        member _.OfficersAsync(number: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                offSiren number (fun siren ->
                    task {
                        match! readRecord siren ct with
                        | Error failure -> return Error failure
                        | Ok(reply, ValueNone) ->
                            return Ok(SourceAnswer.evidence coverage reply ValueNone (NotAvailable France.leftOut))
                        | Ok(reply, ValueSome(_, officers, raw, asOf)) ->
                            return
                                Ok
                                    { SourceAnswer.evidence coverage reply asOf officers with
                                        Raw = ValueSome raw
                                    }
                    }))

        member _.OwnersAsync(_: string, jurisdiction: Jurisdiction, purpose: OwnerPurpose, _: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                Threading.Tasks.Task.FromResult(
                    Ok(
                        SourceAnswer.unasked
                            coverage
                            client.Now
                            (SourceAnswer.restrictedOwners
                                purpose
                                "Unverified: those with a legitimate interest, as INPI's register defines them"
                                "Apply to INPI for access to the register of beneficial owners, or use a vendor")
                    )
                ))
