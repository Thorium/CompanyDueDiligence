namespace CompanyDueDiligence

open System
open System.Text.Json
open System.Text.RegularExpressions
open System.Threading
open CompanyDueDiligence.Json

/// The Register of Legal Persons (RPO) REST API, read as it answered on 2026-11-08, with its documentation at rpo.minv.sk.
module internal Slovakia =

    let coverage: RegistryCoverage =
        {
            Registry = "RPO (Slovakia)"
            Jurisdictions = [ Jurisdiction.ofCode "SK" ]
            Standing =
                Support.Partial
                    "The termination date and legal statuses such as liquidation and bankruptcy; a live entity has no status word"
            Concerns =
                Support.Partial
                    "Bankruptcy, restructuring and liquidation, past or present, are stated; overdue filings aren't published"
            Addresses =
                Support.Partial "The seat's address; the API is updated nightly, so a day's changes can be missing"
            Search = Support.Partial "Up to 500 entities by full-text name, terminated ones included"
            Officers =
                Support.Partial
                    "Statutory bodies and stakeholders other than partners, current and former, with their dates"
            Owners =
                Support.Partial
                    "Partners and shareholders listed as stakeholders: legal owners. Beneficial owners aren't published without a legitimate interest"
            Cost = AccessCost.Free
            RateLimit = RateLimitPolicy.NotPublished(2, TimeSpan.FromSeconds 1.)
            Licence = "CC BY 4.0, as each answer's license field states"
        }

    /// RPO's API is a replica refreshed nightly, so an entity registered since the last refresh isn't in it yet.
    [<Literal>]
    let notFound =
        "RPO's API is a nightly replica, so a search finding no record for this IČO doesn't say the register holds none"

    let private icoRegex = Regex @"\A[0-9]{8}\z"

    /// Eight digits. Spaces are taken out. No check digit is tested: the register holds numbers, such as 00000001, that would fail one.
    let normalIco (number: string) =
        let normal = if isNull number then "" else number.Replace(" ", "")

        if icoRegex.IsMatch normal then
            ValueSome normal
        else
            ValueNone

    let private isCurrent (e: JsonElement) = (tryDate "validTo" e).IsNone

    /// The entry still valid, or else the last one.
    let private currentOf (entries: JsonElement list) =
        entries
        |> List.tryFind isCurrent
        |> Option.orElse (List.tryLast entries)
        |> ValueOption.ofOption

    let private codeValue (e: JsonElement) =
        tryObject "value" e
        |> ValueOption.map (fun value -> tryString "code" value, tryString "value" value)

    /// One legal status from CL010108, by code then by word. Codes 2 and 3 were seen live; a word naming bankruptcy, restructuring or liquidation
    /// is placed by that word, and anything else is Unknown.
    let statusStanding (code: string voption) (word: string voption) =
        let text = word |> ValueOption.defaultValue ""

        let says (part: string) =
            text.Contains(part, StringComparison.OrdinalIgnoreCase)

        match code with
        | ValueSome "3" -> CompanyStanding.InInsolvency text
        | ValueSome "2" -> CompanyStanding.WindingUp text
        | _ when says "konkurz" -> CompanyStanding.InInsolvency text
        | _ when says "reštrukturaliz" -> CompanyStanding.InInsolvency text
        | _ when says "likvid" -> CompanyStanding.WindingUp text
        | _ -> CompanyStanding.Unknown(word |> ValueOption.orElse code)

    /// A termination date closes the entity. Otherwise a current legal status decides, insolvency before liquidation; with none the entity is on
    /// the register without a status word.
    let standing (terminated: DateOnly voption) (statuses: (string voption * string voption) list) =
        match terminated with
        | ValueSome _ -> CompanyStanding.Closed "Zánik (terminated)"
        | ValueNone ->
            let placed = statuses |> List.map (fun (code, word) -> statusStanding code word)

            placed
            |> List.tryFind (function
                | CompanyStanding.InInsolvency _ -> true
                | _ -> false)
            |> Option.orElse (
                placed
                |> List.tryFind (function
                    | CompanyStanding.WindingUp _ -> true
                    | _ -> false)
            )
            |> Option.orElse (List.tryHead placed)
            |> Option.defaultValue (CompanyStanding.Registered ValueNone)

    let private addressOf (e: JsonElement) : Address =
        let number =
            match OpenData.text "regNumber" e, SourceAnswer.trimmed "buildingNumber" e with
            | ValueSome "0", building -> building
            | ValueSome registered, ValueSome building -> ValueSome $"{registered}/{building}"
            | registered, building -> building |> ValueOption.orElse registered

        let street =
            [ SourceAnswer.trimmed "street" e; number ]
            |> List.choose ValueOption.toOption
            |> String.concat " "

        {
            Lines =
                if street <> "" then
                    [ street ]
                else
                    SourceAnswer.trimmed "formatedAddress" e |> ValueOption.toList
            Locality = tryObject "municipality" e |> ValueOption.bind (SourceAnswer.trimmed "value")
            Region = ValueNone
            PostalCode = strings e "postalCodes" |> List.tryHead |> ValueOption.ofOption
            Country = tryObject "country" e |> ValueOption.bind (SourceAnswer.trimmed "value")
        }

    /// A legal form naming a foreign person is a Slovak branch of one.
    let private role (form: string voption) =
        match form with
        | ValueSome word when word.Contains("zahraničn", StringComparison.OrdinalIgnoreCase) ->
            RecordRole.ForeignRegistration
        | ValueSome _ -> RecordRole.Home
        | ValueNone -> RecordRole.NotStated

    let private statuses (e: JsonElement) =
        array e "legalStatuses"
        |> List.choose (fun status ->
            codeValue status
            |> ValueOption.map (fun (code, word) -> isCurrent status, code, word)
            |> ValueOption.toOption)

    let company (e: JsonElement) : Company =
        let name =
            currentOf (array e "fullNames")
            |> ValueOption.bind (SourceAnswer.trimmed "value")
            |> ValueOption.defaultWith (fun () -> raise (InvalidJson "The answer has no name"))

        let number =
            currentOf (array e "identifiers")
            |> ValueOption.bind (OpenData.text "value")
            |> ValueOption.defaultWith (fun () -> raise (InvalidJson "The answer has no IČO"))

        let form =
            currentOf (array e "legalForms")
            |> ValueOption.bind codeValue
            |> ValueOption.bind snd

        let all = statuses e

        let current =
            all
            |> List.filter (fun (now, _, _) -> now)
            |> List.map (fun (_, code, word) -> code, word)

        let terminated = tryDate "termination" e

        let insolvent =
            all
            |> List.exists (fun (_, code, word) ->
                match statusStanding code word with
                | CompanyStanding.InInsolvency _
                | CompanyStanding.WindingUp _ -> true
                | _ -> false)

        let seat =
            array e "addresses"
            |> List.filter isCurrent
            |> List.map (fun address ->
                {
                    Kind = AddressKind.RegisteredOffice
                    Address = addressOf address
                })

        {
            Name = name
            OtherNames =
                array e "fullNames" @ array e "alternativeNames"
                |> List.choose (fun held -> SourceAnswer.trimmed "value" held |> ValueOption.toOption)
                |> List.filter ((<>) name)
                |> List.distinct
            Number = number
            Jurisdiction = Jurisdiction.ofCode "SK"
            JurisdictionAsPublished = ValueNone
            Standing = standing terminated current
            Concerns =
                if insolvent then
                    {
                        Stated = [ CompanyConcern.InsolvencyHistory ]
                        Unconfirmed = CompanyConcern.all |> List.except [ CompanyConcern.InsolvencyHistory ]
                    }
                else
                    CompanyConcerns.notPublished
            StatusAsPublished =
                match current |> List.choose (snd >> ValueOption.toOption), terminated with
                | [], ValueSome _ -> ValueSome "termination"
                | [], ValueNone -> ValueNone
                | words, _ -> ValueSome(String.concat ", " words)
            LegalForm = form
            IncorporatedOn = tryDate "establishment" e
            CeasedOn = terminated
            Addresses =
                match seat with
                | [] -> NotAvailable "The answer holds no current address"
                | _ -> Available seat
            Role = role form
            HomeRegistration = ValueNone
            EstablishmentOf = ValueNone
        }

    /// The record ids the search gives for one IČO.
    let resultsOf (root: JsonElement) =
        match tryProp "results" root with
        | ValueSome results when results.ValueKind = JsonValueKind.Array -> results.EnumerateArray() |> List.ofSeq
        | _ -> raise (InvalidJson "The answer has no results list")

    let idOf (result: JsonElement) =
        match tryProp "id" result with
        | ValueSome id when id.ValueKind = JsonValueKind.Number -> id.GetRawText()
        | _ -> raise (InvalidJson "A result has no id")

    let lookup (ico: string) (root: JsonElement) =
        let found = company root

        if found.Number = ico then
            found
        else
            raise (InvalidJson "The answer holds another entity")

    let search (limit: int) (root: JsonElement) =
        let found = resultsOf root |> List.map company

        { OpenData.matches limit ValueNone found with
            Complete = found.Length < limit
        }

    let private isOwnerType (word: string voption) =
        match word with
        | ValueSome text ->
            text.Contains("Spoločník", StringComparison.OrdinalIgnoreCase)
            || text.Contains("akcionár", StringComparison.OrdinalIgnoreCase)
        | ValueNone -> false

    let private typeWord (e: JsonElement) =
        tryObject "stakeholderType" e |> ValueOption.bind (tryString "value")

    /// ISO 3166-1 numeric codes and their alpha-2 codes, as RPO's country code list (CL000086) uses the numeric ones.
    let private isoAlpha2 =
        "004AF 248AX 008AL 012DZ 016AS 020AD 024AO 660AI 010AQ 028AG 032AR 051AM 533AW 036AU 040AT 031AZ 044BS 048BH 050BD 052BB 112BY 056BE 084BZ
         204BJ 060BM 064BT 068BO 535BQ 070BA 072BW 074BV 076BR 086IO 096BN 100BG 854BF 108BI 132CV 116KH 120CM 124CA 136KY 140CF 148TD 152CL 156CN
         162CX 166CC 170CO 174KM 178CG 180CD 184CK 188CR 384CI 191HR 192CU 531CW 196CY 203CZ 208DK 262DJ 212DM 214DO 218EC 818EG 222SV 226GQ 232ER
         233EE 748SZ 231ET 238FK 234FO 242FJ 246FI 250FR 254GF 258PF 260TF 266GA 270GM 268GE 276DE 288GH 292GI 300GR 304GL 308GD 312GP 316GU 320GT
         831GG 324GN 624GW 328GY 332HT 334HM 336VA 340HN 344HK 348HU 352IS 356IN 360ID 364IR 368IQ 372IE 833IM 376IL 380IT 388JM 392JP 832JE 400JO
         398KZ 404KE 296KI 408KP 410KR 414KW 417KG 418LA 428LV 422LB 426LS 430LR 434LY 438LI 440LT 442LU 446MO 450MG 454MW 458MY 462MV 466ML 470MT
         584MH 474MQ 478MR 480MU 175YT 484MX 583FM 498MD 492MC 496MN 499ME 500MS 504MA 508MZ 104MM 516NA 520NR 524NP 528NL 540NC 554NZ 558NI 562NE
         566NG 570NU 574NF 807MK 580MP 578NO 512OM 586PK 585PW 275PS 591PA 598PG 600PY 604PE 608PH 612PN 616PL 620PT 630PR 634QA 638RE 642RO 643RU
         646RW 652BL 654SH 659KN 662LC 663MF 666PM 670VC 882WS 674SM 678ST 682SA 686SN 688RS 690SC 694SL 702SG 534SX 703SK 705SI 090SB 706SO 710ZA
         239GS 728SS 724ES 144LK 729SD 740SR 744SJ 752SE 756CH 760SY 158TW 762TJ 834TZ 764TH 626TL 768TG 772TK 776TO 780TT 788TN 792TR 795TM 796TC
         798TV 800UG 804UA 784AE 826GB 840US 581UM 858UY 860UZ 548VU 862VE 704VN 092VG 850VI 876WF 732EH 887YE 894ZM 716ZW"
            .Split([| ' '; '\r'; '\n' |], StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun pair -> pair.Substring(0, 3), pair.Substring 3)
        |> Map.ofArray

    /// The alpha-2 code for an address's numeric country code. A code ISO doesn't assign, such as 898 (unspecified), is ValueNone.
    let countryCode (address: JsonElement) =
        tryObject "country" address
        |> ValueOption.bind (tryString "code")
        |> ValueOption.bind (fun code -> isoAlpha2 |> Map.tryFind (code.Trim().PadLeft(3, '0')) |> ValueOption.ofOption)

    let private party (e: JsonElement) =
        let person =
            tryObject "personName" e
            |> ValueOption.bind (fun name ->
                SourceAnswer.trimmed "formatedName" name
                |> ValueOption.orElse (
                    match strings name "givenNames" @ strings name "familyNames" with
                    | [] -> ValueNone
                    | parts -> ValueSome(String.concat " " parts)
                ))

        match person, SourceAnswer.trimmed "fullName" e with
        | ValueSome name, _ -> name, PartyKind.Individual, ValueNone
        | ValueNone, ValueSome name ->
            name,
            PartyKind.LegalEntity,
            ValueSome
                {
                    LegalName = ValueSome name
                    LegalForm = ValueNone
                    GoverningLaw = ValueNone
                    Country = tryObject "address" e |> ValueOption.bind countryCode
                    Register = ValueNone
                    RegistrationNumber = SourceAnswer.trimmed "identifier" e
                }
        | ValueNone, ValueNone -> PartyNames.NotGiven, PartyKind.Unknown ValueNone, ValueNone

    let private officer (role: OfficerRole) (e: JsonElement) : Officer =
        let name, kind, entity = party e

        {
            Name = name
            Kind = kind
            Role = role
            Current = isCurrent e
            AppointedOn = tryDate "validFrom" e
            EndedOn = tryDate "validTo" e
            BornIn = ValueNone
            Nationality = ValueNone
            CountryOfResidence = ValueNone
            Address = tryObject "address" e |> ValueOption.map addressOf
            Entity = entity
            IdentityVerification = ValueNone
        }

    /// A statutory body is a director, unless it is a liquidator. A stakeholder that isn't an owner keeps its type, such as a prokurist.
    let officers (root: JsonElement) =
        let bodies =
            array root "statutoryBodies"
            |> List.map (fun body ->
                let kind = typeWord body |> ValueOption.defaultValue ""

                let role =
                    if kind.Contains("Likvidátor", StringComparison.OrdinalIgnoreCase) then
                        OfficerRole.Other kind
                    else
                        OfficerRole.Director

                officer role body)

        let others =
            array root "stakeholders"
            |> List.filter (typeWord >> isOwnerType >> not)
            |> List.map (fun stakeholder ->
                officer (OfficerRole.Other(typeWord stakeholder |> ValueOption.defaultValue "")) stakeholder)

        match bodies @ others with
        | [] -> NotAvailable "The register lists no statutory body for this entity, which doesn't say it has none"
        | found -> Available found

    let private owner (e: JsonElement) : Owner =
        let name, kind, entity = party e

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
                        Band = ValueNone
                        AsPublished = typeWord e |> ValueOption.defaultValue ""
                    }
                ]
            Current = isCurrent e
            Since = tryDate "validFrom" e
            EndedOn = tryDate "validTo" e
            BornIn = ValueNone
            Nationality = ValueNone
            CountryOfResidence = ValueNone
            Address = tryObject "address" e |> ValueOption.map addressOf
            Entity = entity
            IdentityVerification = ValueNone
        }

    /// Stakeholders whose type is a partner or shareholder.
    let owners (root: JsonElement) =
        let all =
            array root "stakeholders"
            |> List.filter (typeWord >> isOwnerType)
            |> List.map owner

        let current = all |> List.filter _.Current

        match current with
        | [] ->
            NotAvailable
                "The register lists no current partner or shareholder; a joint-stock company's shareholders aren't listed, so this doesn't say there are none"
        | _ ->
            Available
                {
                    Owners = all
                    Statements = []
                    Gaps =
                        [
                            if current |> List.exists (fun o -> o.Name.IsNone) then
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

/// Slovakia as an ICompanyRegistry. A lookup is a search by IČO, then the one record it names. No result is NotAvailable, as the API is a
/// nightly replica; several, as an IČO given twice, can't be told apart.
[<Sealed>]
type internal SlovakiaRegistry(connection: SourceConnection) =
    static let searchLimit = 500

    let client =
        SourceClient(connection, SourceAnswer.pacing 2 (TimeSpan.FromSeconds 1.))

    let coverage = Slovakia.coverage
    let baseUri = "https://api.statistics.sk/rpo/v1"

    let offIco number send =
        match Slovakia.normalIco number with
        | ValueSome ico -> send ico
        | ValueNone -> Threading.Tasks.Task.FromResult(Error(RegistryFailure.InvalidRequest "An IČO has eight digits"))

    /// The one record for an IČO, read by read, or the answer that settles there isn't one to read.
    let readEntity
        (ico: string)
        (absent: Availability<'T>)
        (read: string -> JsonElement -> Availability<'T>)
        (ct: CancellationToken)
        =
        task {
            match! client.GetAsync(Uri $"{baseUri}/search?identifier={ico}", ct) with
            | Error failure -> return Error failure
            | Ok reply when reply.Status = 404 -> return Error(RegistryFailure.RequestRefused "HTTP 404")
            | Ok reply ->
                match SourceAnswer.json reply (Slovakia.resultsOf >> List.map Slovakia.idOf) with
                | Error failure -> return Error failure
                | Ok [] -> return Ok(SourceAnswer.evidence coverage reply ValueNone absent)
                | Ok(_ :: _ :: _ as several) ->
                    return
                        Ok(
                            SourceAnswer.evidence
                                coverage
                                reply
                                ValueNone
                                (NotAvailable
                                    $"{several.Length} records carry this IČO, so the register can't tell which is meant")
                        )
                | Ok [ pid ] ->
                    match! client.GetAsync(Uri $"{baseUri}/entity/{pid}?showHistoricalData=true", ct) with
                    | Error failure -> return Error failure
                    | Ok entity when entity.Status = 404 ->
                        return Error(RegistryFailure.RequestRefused "HTTP 404 for a record the search named")
                    | Ok entity ->
                        return
                            SourceAnswer.json entity (fun root ->
                                read ico root, SourceAnswer.dateOfTimestamp "dbModificationDate" root)
                            |> Result.map (fun (value, asOf) -> SourceAnswer.evidence coverage entity asOf value)
        }

    interface ICompanyRegistry with
        member _.Coverage = coverage

        member _.TryFindAsync(number: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                offIco number (fun ico ->
                    readEntity
                        ico
                        (NotAvailable Slovakia.notFound)
                        (fun ico root -> Available(ValueSome(Slovakia.lookup ico root)))
                        ct))

        member _.SearchAsync(name: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                if String.IsNullOrWhiteSpace name || name.Trim().Length < 3 then
                    Threading.Tasks.Task.FromResult(
                        Error(
                            RegistryFailure.InvalidRequest
                                "A name search needs at least three characters, below which the register matches exactly"
                        )
                    )
                else
                    task {
                        match!
                            client.GetAsync(Uri $"{baseUri}/search?fullName={Uri.EscapeDataString(name.Trim())}", ct)
                        with
                        | Error failure -> return Error failure
                        | Ok reply when reply.Status = 404 -> return Error(RegistryFailure.RequestRefused "HTTP 404")
                        | Ok reply ->
                            return
                                SourceAnswer.json reply (Slovakia.search searchLimit)
                                |> Result.map (fun found ->
                                    SourceAnswer.evidence coverage reply ValueNone (Available found))
                    })

        member _.OfficersAsync(number: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                offIco number (fun ico ->
                    readEntity
                        ico
                        (NotAvailable Slovakia.notFound)
                        (fun ico root ->
                            Slovakia.lookup ico root |> ignore
                            Slovakia.officers root)
                        ct))

        member _.OwnersAsync(number: string, jurisdiction: Jurisdiction, _: OwnerPurpose, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                offIco number (fun ico ->
                    readEntity
                        ico
                        (NotAvailable Slovakia.notFound)
                        (fun ico root ->
                            Slovakia.lookup ico root |> ignore
                            Slovakia.owners root)
                        ct))
