namespace CompanyDueDiligence

open System
open System.Text.Json
open System.Text.RegularExpressions
open System.Threading
open CompanyDueDiligence.Json

/// PRH's YTJ open data API v3, read as it answered on 2026-11-06, with its code lists from /description.
module internal Finland =

    let coverage: RegistryCoverage =
        {
            Registry = "PRH/YTJ open data (Finland)"
            Jurisdictions = [ Jurisdiction.ofCode "FI" ]
            Standing = Support.Supported
            Concerns =
                Support.Partial
                    "Bankruptcy, liquidation and re-organisation are stated when recorded; overdue filings and disputes aren't published"
            Addresses = Support.Supported
            Search =
                Support.Partial
                    "The first page, up to 100 matches, never complete: the open data holds Trade Register companies, ended ones among them, and leaves out sole traders and entities not on the Trade Register"
            Officers = Support.NotOffered "The API lists no officers; a trade register extract from PRH does"
            Owners = Support.NotOffered "The API lists no owners"
            Cost = AccessCost.Free
            RateLimit = RateLimitPolicy.NotPublished(2, TimeSpan.FromSeconds 1.)
            Licence = "CC BY 4.0, as the API's own schema states"
        }

    /// The open data holds Trade Register companies, ended ones among them, and leaves out sole traders and entities not on the Trade Register,
    /// so a Business ID it doesn't hold can still be on YTJ.
    [<Literal>]
    let notHeld =
        "PRH's open data holds no company with this Business ID; it leaves out sole traders and entities not on the Trade Register, so YTJ itself can still hold it"

    let private businessIdRegex = Regex @"\A[0-9]{7}-[0-9]\z"

    /// A Business ID: seven digits, a hyphen and a check digit. The API answers anything else with an empty list, which would read as not held.
    let normalBusinessId (number: string) =
        let normal = if isNull number then "" else number.Trim()

        if businessIdRegex.IsMatch normal then
            ValueSome normal
        else
            ValueNone

    let private isCurrent (e: JsonElement) = (tryDate "endDate" e).IsNone

    /// The current item, else the one with the latest endDate, then the latest registrationDate, as an ended company has every item ended.
    /// None only for no items.
    let private latest (items: JsonElement list) =
        match items |> List.tryFind isCurrent, items with
        | Some current, _ -> Some current
        | None, [] -> None
        | None, ended ->
            ended
            |> List.maxBy (fun item -> tryDate "endDate" item, tryDate "registrationDate" item)
            |> Some

    /// The description in English, language code 3, or else the code.
    let private described (e: JsonElement) =
        array e "descriptions"
        |> List.tryFind (fun item -> tryText "languageCode" item = ValueSome "3")
        |> Option.bind (fun item -> tryString "description" item |> ValueOption.toOption)
        |> ValueOption.ofOption
        |> ValueOption.orElse (tryString "type" e)

    /// A situation still running decides the standing, then the Business ID, then the trade register's status. REK_KDI is the code list for that
    /// status. A code not on it is Unknown.
    let standing (situations: string list) (idStatus: string voption) (tradeRegister: string voption) =
        match situations, idStatus, tradeRegister with
        | _ when List.contains "KONK" situations -> CompanyStanding.InInsolvency "Bankrupt"
        | _ when List.contains "SANE" situations -> CompanyStanding.InInsolvency "Company re-organisation"
        | _ when List.contains "SELTILA" situations -> CompanyStanding.WindingUp "Liquidation"
        | _, ValueSome "5", _ -> CompanyStanding.Closed "Business ID invalidated"
        | _, _, ValueSome "1" -> CompanyStanding.Registered(ValueSome "Registered")
        | _, _, ValueSome "2" -> CompanyStanding.Closed "Removed from register"
        | _, _, ValueSome "4" -> CompanyStanding.Closed "Ceased"
        | _, _, ValueSome "0" -> CompanyStanding.Unknown(ValueSome "Unregistered")
        | _, _, ValueSome "3" -> CompanyStanding.Unknown(ValueSome "Start-up not registered")
        | _, _, other -> CompanyStanding.Unknown other

    /// The company name, type 1: the current one, else the latest ended one. None only when the answer has no type-1 name.
    let private currentName (e: JsonElement) =
        array e "names"
        |> List.filter (fun name -> tryText "type" name = ValueSome "1")
        |> latest
        |> Option.bind (fun name -> tryString "name" name |> ValueOption.toOption)

    let private addressOf (e: JsonElement) : CompanyAddress voption =
        let cityOf language =
            array e "postOffices"
            |> List.tryFind (fun office -> tryText "languageCode" office = ValueSome language)
            |> Option.bind (fun office -> tryString "city" office |> ValueOption.toOption)

        let street =
            [
                SourceAnswer.trimmed "co" e
                SourceAnswer.trimmed "street" e
                SourceAnswer.trimmed "buildingNumber" e
                SourceAnswer.trimmed "entrance" e
                SourceAnswer.trimmed "apartmentNumber" e
                SourceAnswer.trimmed "postOfficeBox" e
            ]
            |> List.choose ValueOption.toOption
            |> String.concat " "

        let kind =
            match tryInt "type" e with
            | ValueSome 1 -> ValueSome(AddressKind.Other "Street address on YTJ")
            | ValueSome 2 -> ValueSome AddressKind.Postal
            | _ -> ValueNone

        kind
        |> ValueOption.map (fun kind ->
            {
                Kind = kind
                Address =
                    {
                        Lines = if street = "" then [] else [ street ]
                        Locality = cityOf "1" |> ValueOption.ofOption
                        Region = ValueNone
                        PostalCode = SourceAnswer.trimmed "postCode" e
                        Country = SourceAnswer.trimmed "country" e
                    }
            })

    /// Forms 19 and 7 are a foreign trader's branch and an EEIG's Finnish branch.
    let private role (form: JsonElement voption) =
        match form |> ValueOption.bind (tryText "type") with
        | ValueSome("19" | "7") -> RecordRole.ForeignRegistration
        | ValueSome _ -> RecordRole.Home
        | ValueNone -> RecordRole.NotStated

    let company (e: JsonElement) : Company =
        let idBlock =
            match tryObject "businessId" e with
            | ValueSome block -> block
            | ValueNone -> raise (InvalidJson "Missing or invalid field 'businessId'")

        let name =
            match currentName e with
            | Some name -> name
            | None -> raise (InvalidJson "The answer has no company name")

        let running = array e "companySituations" |> List.filter isCurrent

        let situations =
            running
            |> List.choose (fun situation -> tryString "type" situation |> ValueOption.toOption)

        let everSituated = not (List.isEmpty (array e "companySituations"))
        let idStatus = tryText "status" e
        let tradeRegister = tryText "tradeRegisterStatus" e
        let form = array e "companyForms" |> latest |> ValueOption.ofOption

        let addresses =
            array e "addresses" |> List.choose (addressOf >> ValueOption.toOption)

        {
            Name = name
            OtherNames =
                array e "names"
                |> List.choose (fun held -> tryString "name" held |> ValueOption.toOption)
                |> List.filter ((<>) name)
                |> List.distinct
            Number = required "value" tryString idBlock
            Jurisdiction = Jurisdiction.ofCode "FI"
            JurisdictionAsPublished = ValueNone
            Standing = standing situations idStatus tradeRegister
            Concerns =
                if everSituated then
                    {
                        Stated = [ CompanyConcern.InsolvencyHistory ]
                        Unconfirmed = CompanyConcern.all |> List.except [ CompanyConcern.InsolvencyHistory ]
                    }
                else
                    CompanyConcerns.notPublished
            StatusAsPublished = tradeRegister
            LegalForm = form |> ValueOption.bind described
            IncorporatedOn = tryDate "registrationDate" e
            CeasedOn = tryDate "endDate" e
            Addresses =
                match addresses with
                | [] -> NotAvailable "The answer holds no address"
                | _ -> Available addresses
            Role = role form
            HomeRegistration = ValueNone
            EstablishmentOf = ValueNone
        }

    let private companies (root: JsonElement) =
        match tryProp "companies" root with
        | ValueSome list when list.ValueKind = JsonValueKind.Array -> list.EnumerateArray() |> List.ofSeq
        | _ -> raise (InvalidJson "The answer has no companies list")

    /// The company with this Business ID. Another company in the answer fails the read rather than being taken for it.
    let lookup (businessId: string) (root: JsonElement) =
        match companies root with
        | [] -> ValueNone
        | [ only ] ->
            let found = company only

            if found.Number = businessId then
                ValueSome(found, SourceAnswer.dateOfTimestamp "lastModified" only, only.GetRawText())
            else
                raise (InvalidJson "The answer holds another company")
        | _ -> raise (InvalidJson "The answer holds several companies for one Business ID")

    /// A company of a search answer, or None when it can't be read.
    let private readable (e: JsonElement) =
        try
            Some(company e)
        with
        | InvalidJson _
        | :? ArgumentOutOfRangeException
        | :? FormatException
        | :? OverflowException -> None

    /// The companies of a search answer. One that can't be read is left out rather than failing the rest, and the search is never complete.
    let search (root: JsonElement) =
        let matches =
            companies root
            |> List.choose readable
            |> List.map (fun found ->
                {
                    Name = found.Name
                    Number = found.Number
                    StatusAsPublished = found.StatusAsPublished
                    Address =
                        match found.Addresses with
                        | Available(first :: _) -> ValueSome first.Address
                        | _ -> ValueNone
                })

        let total = tryInt "totalResults" root

        {
            Matches = matches
            TotalMatches = total
            // The open data leaves out sole traders and entities not on the Trade Register, so a search is never the whole register.
            Complete = false
        }

/// Finland as an ICompanyRegistry.
[<Sealed>]
type internal FinlandRegistry(connection: SourceConnection) =
    let client =
        SourceClient(connection, SourceAnswer.pacing 2 (TimeSpan.FromSeconds 1.))

    let coverage = Finland.coverage

    interface ICompanyRegistry with
        member _.Coverage = coverage

        member _.TryFindAsync(number: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                match Finland.normalBusinessId number with
                | ValueNone ->
                    Threading.Tasks.Task.FromResult(
                        Error(
                            RegistryFailure.InvalidRequest "A Business ID has seven digits, a hyphen and a check digit"
                        )
                    )
                | ValueSome businessId ->
                    task {
                        let uri =
                            Uri
                                $"https://avoindata.prh.fi/opendata-ytj-api/v3/companies?businessId={Uri.EscapeDataString businessId}"

                        match! client.GetAsync(uri, ct) with
                        | Error failure -> return Error failure
                        | Ok reply when reply.Status = 404 -> return Error(RegistryFailure.RequestRefused "HTTP 404")
                        | Ok reply ->
                            return
                                SourceAnswer.json reply (Finland.lookup businessId)
                                |> Result.map (fun found ->
                                    match found with
                                    | ValueNone ->
                                        SourceAnswer.evidence coverage reply ValueNone (NotAvailable Finland.notHeld)
                                    | ValueSome(company, asOf, raw) ->
                                        { SourceAnswer.evidence coverage reply asOf (Available(ValueSome company)) with
                                            Raw = ValueSome raw
                                        })
                    })

        member _.SearchAsync(name: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                if String.IsNullOrWhiteSpace name then
                    Threading.Tasks.Task.FromResult(Error(RegistryFailure.InvalidRequest "A search needs a term"))
                else
                    task {
                        let uri =
                            Uri
                                $"https://avoindata.prh.fi/opendata-ytj-api/v3/companies?name={Uri.EscapeDataString name}"

                        match! client.GetAsync(uri, ct) with
                        | Error failure -> return Error failure
                        | Ok reply when reply.Status = 404 -> return Error(RegistryFailure.RequestRefused "HTTP 404")
                        | Ok reply ->
                            return
                                SourceAnswer.json reply Finland.search
                                |> Result.map (fun found ->
                                    SourceAnswer.evidence coverage reply ValueNone (Available found))
                    })

        member _.OfficersAsync(_: string, jurisdiction: Jurisdiction, _: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                Threading.Tasks.Task.FromResult(
                    Ok(
                        SourceAnswer.unasked
                            coverage
                            client.Now
                            (NotAvailable "The API lists no officers; a trade register extract from PRH does")
                    )
                ))

        member _.OwnersAsync(_: string, jurisdiction: Jurisdiction, purpose: OwnerPurpose, _: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                Threading.Tasks.Task.FromResult(
                    Ok(
                        SourceAnswer.unasked
                            coverage
                            client.Now
                            (SourceAnswer.restrictedOwners
                                purpose
                                "Unverified: those PRH's beneficial owner rules allow, such as firms under anti-money-laundering rules"
                                "Apply to PRH for beneficial owner data, or use a vendor")
                    )
                ))
