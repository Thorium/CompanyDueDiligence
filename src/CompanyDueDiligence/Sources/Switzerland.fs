namespace CompanyDueDiligence

open System
open System.Globalization
open System.Net.Http
open System.Text
open System.Text.RegularExpressions
open System.Threading
open System.Threading.Tasks
open System.Xml
open System.Xml.Linq

/// The BFS UID register's public SOAP service, version 5.0, read as it answered on 2026-11-08, with its WSDL, the BFS interface
/// specification 5.0 and the eCH-0108 5.1 code lists.
module internal Switzerland =

    let coverage: RegistryCoverage =
        {
            Registry = "UID register (Switzerland)"
            Jurisdictions = [ Jurisdiction.ofCode "CH" ]
            Standing =
                Support.Partial
                    "The UID status and the commercial register entry's status; liquidation is read from the name's suffix"
            Concerns = Support.Partial "An undeliverable legal address is stated; other concerns aren't published"
            Addresses = Support.Supported
            Search = Support.Partial "Up to 30 entries, ranked by the register; non-public entries are left out"
            Officers = Support.NotOffered "The UID register lists no officers; the commercial register does"
            Owners = Support.NotOffered "The UID register lists no owners"
            Cost = AccessCost.Free
            RateLimit =
                RateLimitPolicy.Published(
                    20,
                    TimeSpan.FromMinutes 1.,
                    "BFS: the public services are limited to 20 requests a minute, and a caller over it is blocked for a while"
                )
            Licence = "None stated; Public Services are for unregistered users. OPEN-BY covers the BFS spec only"
        }

    [<Literal>]
    let endpoint = "https://www.uid-wse.admin.ch/V5.0/PublicServices.svc"

    [<Literal>]
    let private actionBase = "http://www.uid.admin.ch/xmlns/uid-wse/IPublicServices/"

    let private uidRegex = Regex @"\A(?:CHE)?([0-9]{9})\z"

    /// A UID's nine digits, from CHE-123.456.789 or the same without separators or prefix. The last digit is the modulus 11 check digit of
    /// the other eight, weighted 5 4 3 2 7 6 5 4; a check of 10 is never issued.
    let uidDigits (number: string) =
        let normal =
            if isNull number then
                ""
            else
                number.Trim().ToUpperInvariant().Replace("-", "").Replace(".", "").Replace(" ", "")

        let found = uidRegex.Match normal

        if found.Success then
            let digits =
                found.Groups[1].Value |> Seq.map (fun c -> int c - int '0') |> Array.ofSeq

            let weights = [| 5; 4; 3; 2; 7; 6; 5; 4 |]

            let sum =
                Array.fold2 (fun total digit weight -> total + digit * weight) 0 digits[0..7] weights

            match 11 - sum % 11 with
            | 11 when digits[8] = 0 -> ValueSome found.Groups[1].Value
            | check when check < 10 && check = digits[8] -> ValueSome found.Groups[1].Value
            | _ -> ValueNone
        else
            ValueNone

    /// A UID's nine digits written CHE-123.456.789. Anything but nine digits is unreadable.
    let format (digits: string) =
        if
            isNull digits
            || digits.Length <> 9
            || not (digits |> Seq.forall Char.IsAsciiDigit)
        then
            raise (Json.InvalidJson "A UID is nine digits")

        $"CHE-{digits.Substring(0, 3)}.{digits.Substring(3, 3)}.{digits.Substring(6, 3)}"

    let private soap = XNamespace.Get "http://schemas.xmlsoap.org/soap/envelope/"
    let private wse = XNamespace.Get "http://www.uid.admin.ch/xmlns/uid-wse"
    let private wse5 = XNamespace.Get "http://www.uid.admin.ch/xmlns/uid-wse/5"
    let private ech97 = XNamespace.Get "http://www.ech.ch/xmlns/eCH-0097/5"

    let private envelope (body: XElement) =
        XDocument(XElement(soap + "Envelope", XElement(soap + "Body", body))).ToString SaveOptions.DisableFormatting

    let getByUidEnvelope (digits: string) =
        envelope (
            XElement(
                wse + "GetByUID",
                XElement(
                    wse + "uid",
                    XElement(ech97 + "uidOrganisationIdCategorie", "CHE"),
                    XElement(ech97 + "uidOrganisationId", digits)
                )
            )
        )

    /// True when every character of the text is one XML 1.0 can carry: no control character but tab and line ends, and no half surrogate pair.
    let isXmlText (text: string) =
        try
            XmlConvert.VerifyXmlChars text |> ignore
            true
        with :? XmlException ->
            false

    let searchEnvelope (name: string) =
        envelope (
            XElement(
                wse + "Search",
                XElement(
                    wse + "searchParameters",
                    XElement(wse5 + "uidEntitySearchParameters", XElement(wse5 + "organisationName", name.Trim()))
                )
            )
        )

    let request (operation: string) (body: string) : (unit -> HttpContent) * (string * string) list =
        (fun () -> new StringContent(body, Encoding.UTF8, "text/xml")), [ "SOAPAction", $"\"{actionBase}{operation}\"" ]

    let private children (name: string) (e: XElement) =
        e.Elements()
        |> Seq.filter (fun child -> child.Name.LocalName = name)
        |> List.ofSeq

    let private child (name: string) (e: XElement) =
        children name e |> List.tryHead |> ValueOption.ofOption

    let private descendant (name: string) (e: XElement) =
        e.Descendants()
        |> Seq.tryFind (fun d -> d.Name.LocalName = name)
        |> ValueOption.ofOption

    let private text (e: XElement voption) =
        e |> ValueOption.map _.Value.Trim() |> ValueOption.filter ((<>) "")

    let private dateOf (e: XElement voption) =
        text e
        |> ValueOption.bind (fun value ->
            match
                DateOnly.TryParseExact(
                    value.Substring(0, min 10 value.Length),
                    "yyyy-MM-dd",
                    CultureInfo.InvariantCulture,
                    DateTimeStyles.None
                )
            with
            | true, date -> ValueSome date
            | _ -> ValueNone)

    /// The answer's XML. One that isn't XML is unreadable, quoting nothing from it.
    let parse (body: string) (read: XDocument -> 'T) =
        try
            Ok(read (XDocument.Parse body))
        with
        | :? XmlException as error -> Error(RegistryFailure.UnreadableAnswer $"Not XML, at line {error.LineNumber}")
        | Json.InvalidJson reason -> Error(RegistryFailure.UnreadableAnswer reason)

    /// A SOAP fault, which the service sends with HTTP 500. Request_limit_exceeded is rate limiting and Data_validation_failed a refused request;
    /// any other fault, or a 500 that isn't a fault, is the service failing.
    let fault (body: string) =
        let code =
            try
                let document = XDocument.Parse body

                document.Descendants()
                |> Seq.tryFind (fun e -> e.Name.LocalName = "error")
                |> Option.orElse (
                    document.Descendants()
                    |> Seq.tryFind (fun e -> e.Name.LocalName = "faultstring")
                )
                |> Option.map _.Value.Trim()
            with :? XmlException ->
                None

        match code with
        | Some "Request_limit_exceeded" -> RegistryFailure.RateLimited ValueNone
        | Some "Data_validation_failed" -> RegistryFailure.RequestRefused "SOAP fault Data_validation_failed"
        | Some "Permission_denied" -> RegistryFailure.RequestRefused "SOAP fault Permission_denied"
        | Some _ -> RegistryFailure.Unavailable "SOAP fault"
        | None -> RegistryFailure.Unavailable "HTTP 500"

    /// The wording a name in liquidation carries, as words of their own in any case: "in Liquidation", "in Nachlassliquidation",
    /// "en liquidation" (so "en liquidation concordataire" too), "in liquidazione" (so "in liquidazione concordataria" too),
    /// "en liquidaziun", and the short forms "in Liq." and "en liq.". A name that only holds the word, such as "Liquidation Services AG",
    /// doesn't carry it.
    let private liquidationWording =
        Regex(
            @"(?<![\p{L}\p{N}])(?:in\s+(?:Nachlass)?Liquidation|in\s+liquidazione|en\s+liquidation|en\s+liquidaziun|(?:in|en)\s+liq\.)(?![\p{L}\p{N}])",
            RegexOptions.IgnoreCase ||| RegexOptions.CultureInvariant
        )

    /// The eCH-0108 UID status decides a deleted or annulled entry. An active one (definitive, or being amended) takes the commercial register
    /// entry's status where it has one; a name carrying liquidation wording is being wound up.
    let standing
        (uidStatus: string voption)
        (registerStatus: string voption)
        (entryStatus: string voption)
        (name: string)
        =
        let inLiquidation = not (isNull name) && liquidationWording.IsMatch name

        match uidStatus, registerStatus, entryStatus with
        | ValueSome "5", _, _ -> CompanyStanding.Closed "Gelöscht (deleted from the UID register)"
        | ValueSome "6", _, _ -> CompanyStanding.Closed "Definitiv gelöscht (deleted, retention period over)"
        | ValueSome "7", _, _ -> CompanyStanding.Closed "Annulliert (annulled as a duplicate)"
        | ValueSome("3" | "4"), ValueSome "2", ValueSome "2" ->
            CompanyStanding.Closed "Gelöscht im Handelsregister (deleted from the commercial register)"
        | ValueSome("3" | "4"), ValueSome "2", ValueSome "3" ->
            CompanyStanding.Registered(ValueSome "Provisorisch im Handelsregister")
        | ValueSome("3" | "4"), ValueSome "2", ValueSome "1" when inLiquidation ->
            CompanyStanding.WindingUp "in Liquidation"
        | ValueSome("3" | "4"), ValueSome "2", ValueSome "1" -> CompanyStanding.Active
        | ValueSome("3" | "4"), ValueSome "3", _ when inLiquidation -> CompanyStanding.WindingUp "in Liquidation"
        | ValueSome("3" | "4"), ValueSome "3", _ ->
            CompanyStanding.Registered(ValueSome "Aktiv im UID-Register, nicht im Handelsregister")
        | ValueSome "1", _, _ -> CompanyStanding.Registered(ValueSome "Provisorisch (UID not yet checked)")
        | ValueSome "2", _, _ -> CompanyStanding.Unknown(ValueSome "In Reaktivierung")
        | ValueSome code, register, entry ->
            CompanyStanding.Unknown(
                ValueSome
                    $"UID {code}; HR {register |> ValueOption.defaultValue String.Empty}/{entry |> ValueOption.defaultValue String.Empty}"
            )
        | ValueNone, _, _ -> CompanyStanding.Unknown ValueNone

    /// eCH-0097 forms named in the BFS and eCH documents read here. Any other stays a code.
    let private formWord (code: string) =
        match code with
        | "0101" -> "Einzelunternehmen"
        | "0103" -> "Kollektivgesellschaft"
        | "0104" -> "Kommanditgesellschaft"
        | "0106" -> "Aktiengesellschaft"
        | "0107" -> "Gesellschaft mit beschränkter Haftung"
        | "0108" -> "Genossenschaft"
        | "0109" -> "Verein"
        | "0110" -> "Stiftung"
        | "0111" -> "Zweigniederlassung einer ausländischen Gesellschaft, im Handelsregister"
        | "0312" -> "Zweigniederlassung einer ausländischen Gesellschaft, nicht im Handelsregister"
        | "0441" -> "Ausländisches Unternehmen"
        | other -> other

    /// Per eCH-0108 5.1 section 3.2.7: forms 0111 and 0312 are Swiss branches of foreign companies, and 0441 a foreign enterprise.
    let role (form: string voption) =
        match form with
        | ValueSome("0111" | "0312" | "0441") -> RecordRole.ForeignRegistration
        | ValueSome _ -> RecordRole.Home
        | ValueNone -> RecordRole.NotStated

    let private addressOf (e: XElement) : CompanyAddress =
        let category = text (child "addressCategory" e)

        let street =
            [ text (child "street" e); text (child "houseNumber" e) ]
            |> List.choose ValueOption.toOption
            |> String.concat " "

        let lines =
            [
                (if street = "" then ValueNone else ValueSome street)
                text (child "addressLine1" e)
                text (child "addressLine2" e)
                text (child "postOfficeBoxText" e)
            ]
            |> List.choose ValueOption.toOption

        {
            Kind =
                match category with
                | ValueSome "LEGAL" -> AddressKind.RegisteredOffice
                | ValueSome "POBOX" -> AddressKind.Postal
                | ValueSome other -> AddressKind.Other other
                | ValueNone -> AddressKind.Other "Address in the UID register"
            Address =
                {
                    Lines = lines
                    Locality = text (child "town" e)
                    Region = text (child "cantonAbbreviation" e)
                    PostalCode =
                        text (child "swissZipCode" e)
                        |> ValueOption.orElse (text (child "foreignZipCode" e))
                    Country = text (child "countryIdISO2" e)
                }
        }

    /// An eCH-0108 organisationType: the organisation and its UID, commercial register and VAT information.
    let company (record: XElement) : Company =
        let organisation =
            child "organisation" record
            |> ValueOption.defaultWith (fun () -> raise (Json.InvalidJson "The record has no organisation"))

        let identification =
            child "organisationIdentification" organisation
            |> ValueOption.defaultWith (fun () -> raise (Json.InvalidJson "The record has no identification"))

        let digits =
            child "uid" identification
            |> ValueOption.bind (child "uidOrganisationId")
            |> text
            |> ValueOption.filter (fun uid -> uid.Length = 9 && uid |> Seq.forall Char.IsAsciiDigit)
            |> ValueOption.defaultWith (fun () -> raise (Json.InvalidJson "The record has no UID of nine digits"))

        let name =
            text (child "organisationName" identification)
            |> ValueOption.defaultWith (fun () -> raise (Json.InvalidJson "The record has no name"))

        let form = text (child "legalForm" identification)
        let uidInformation = child "uidregInformation" record
        let register = child "commercialRegisterInformation" record
        let vat = child "vatRegisterInformation" record

        let uidStatus =
            uidInformation
            |> ValueOption.bind (child "uidregStatusEnterpriseDetail")
            |> text

        let registerStatus =
            register |> ValueOption.bind (child "commercialRegisterStatus") |> text

        let entryStatus =
            register |> ValueOption.bind (child "commercialRegisterEntryStatus") |> text

        let vatStatus = vat |> ValueOption.bind (child "vatStatus") |> text
        let addresses = children "address" organisation

        let legalDeliverable =
            addresses
            |> List.tryFind (fun address -> text (child "addressCategory" address) = ValueSome "LEGAL")
            |> Option.bind (fun address -> text (child "deliverableYesNo" address) |> ValueOption.toOption)

        {
            Name = name
            OtherNames =
                [
                    text (child "organisationLegalName" identification)
                    text (child "organisationAdditionalName" identification)
                ]
                |> List.choose ValueOption.toOption
                |> List.filter ((<>) name)
                |> List.distinct
            Number = format digits
            Jurisdiction = Jurisdiction.ofCode "CH"
            JurisdictionAsPublished = ValueNone
            Standing = standing uidStatus registerStatus entryStatus name
            Concerns =
                match legalDeliverable with
                | Some "false" ->
                    {
                        Stated = [ CompanyConcern.RegisteredOfficeUndeliverable ]
                        Unconfirmed =
                            CompanyConcern.all
                            |> List.except [ CompanyConcern.RegisteredOfficeUndeliverable ]
                    }
                | Some "true" ->
                    {
                        Stated = []
                        Unconfirmed =
                            CompanyConcern.all
                            |> List.except [ CompanyConcern.RegisteredOfficeUndeliverable ]
                    }
                | _ -> CompanyConcerns.notPublished
            StatusAsPublished =
                [
                    uidStatus |> ValueOption.map (fun code -> $"UID {code}")
                    registerStatus
                    |> ValueOption.map (fun code ->
                        $"""HR {code}{entryStatus
                                      |> ValueOption.map (fun entry -> "/" + entry)
                                      |> ValueOption.defaultValue ""}""")
                    vatStatus |> ValueOption.map (fun code -> $"MWST {code}")
                ]
                |> List.choose ValueOption.toOption
                |> function
                    | [] -> ValueNone
                    | parts -> ValueSome(String.concat "; " parts)
            LegalForm = form |> ValueOption.map formWord
            IncorporatedOn = register |> ValueOption.bind (child "commercialRegisterEntryDate") |> dateOf
            CeasedOn =
                register
                |> ValueOption.bind (child "commercialRegisterLiquidationDate")
                |> dateOf
                |> ValueOption.orElse (
                    child "liquidation" organisation
                    |> ValueOption.bind (child "uidregLiquidationDate")
                    |> dateOf
                )
            Addresses =
                match addresses |> List.map addressOf with
                | [] -> NotAvailable "The record holds no address"
                | found -> Available found
            Role = role form
            HomeRegistration = ValueNone
            EstablishmentOf = ValueNone
        }

    /// The records GetByUID answered, each with its own XML.
    let records (document: XDocument) =
        match
            document.Descendants()
            |> Seq.tryFind (fun e -> e.Name.LocalName = "GetByUIDResponse")
        with
        | Some response ->
            response.Descendants()
            |> Seq.filter (fun e -> e.Name.LocalName = "organisationType")
            |> Seq.map (fun record -> company record, record.ToString(SaveOptions.DisableFormatting))
            |> List.ofSeq
        | None -> raise (Json.InvalidJson "The answer has no GetByUID response")

    /// What the records for one UID show. None is no public entry; one is the company; several, as a company entered at two seats, can't be
    /// told apart.
    let found (digits: string) (document: XDocument) =
        match records document with
        | [] ->
            NotAvailable
                "The public service holds no public entry for this UID; non-public entries are withheld, so this doesn't say it isn't held",
            ValueNone
        | [ company, raw ] when company.Number = format digits -> Available(ValueSome company), ValueSome raw
        | [ _ ] -> raise (Json.InvalidJson "The answer holds another UID")
        | several ->
            NotAvailable
                $"{several.Length} records carry this UID, as for a company entered at two seats, so the service can't tell which is meant",
            ValueNone

    [<Literal>]
    let searchLimit = 30

    let search (document: XDocument) =
        match
            document.Descendants()
            |> Seq.tryFind (fun e -> e.Name.LocalName = "SearchResponse")
        with
        | Some response ->
            let companies =
                response.Descendants()
                |> Seq.filter (fun e -> e.Name.LocalName = "uidEntitySearchResultItem")
                |> Seq.map (fun item ->
                    child "organisation" item
                    |> ValueOption.map company
                    |> ValueOption.defaultWith (fun () ->
                        raise (Json.InvalidJson "A search result has no organisation")))
                |> List.ofSeq

            OpenData.matches searchLimit ValueNone companies
        | None -> raise (Json.InvalidJson "The answer has no search response")

/// Switzerland as an ICompanyRegistry, over the UID register's public SOAP service.
[<Sealed>]
type internal SwitzerlandRegistry(connection: SourceConnection) =
    let client =
        SourceClient(
            connection,
            { SourceAnswer.pacing 20 (TimeSpan.FromMinutes 1.) with
                LongestWait = TimeSpan.FromMinutes 1.
            },
            set [ 500 ]
        )

    let coverage = Switzerland.coverage

    /// How long a Request_limit_exceeded fault holds every call: the published limit's window, as BFS gives no figure for its block.
    let blockedFor = TimeSpan.FromMinutes 1.

    /// UTC ticks before which no call is sent, after a Request_limit_exceeded fault.
    let mutable blockedUntil = 0L

    let call (operation: string) (body: string) (read: Xml.Linq.XDocument -> 'T) (ct: CancellationToken) =
        task {
            let wait = TimeSpan(Interlocked.Read(&blockedUntil) - client.Now.UtcTicks)

            if wait > TimeSpan.Zero then
                return Error(RegistryFailure.RateLimited(ValueSome wait))
            else
                let content, headers = Switzerland.request operation body

                match! client.PostAsync(Uri Switzerland.endpoint, content, headers, ct) with
                | Error failure -> return Error failure
                | Ok reply when reply.Status = 500 ->
                    return
                        match Switzerland.fault reply.Body with
                        | RegistryFailure.RateLimited _ ->
                            Interlocked.Exchange(&blockedUntil, (client.Now + blockedFor).UtcTicks)
                            |> ignore

                            Error(RegistryFailure.RateLimited(ValueSome blockedFor))
                        | failure -> Error failure
                | Ok reply when reply.Status <> 200 ->
                    return Error(RegistryFailure.RequestRefused $"HTTP {reply.Status}")
                | Ok reply -> return Switzerland.parse reply.Body read |> Result.map (fun value -> reply, value)
        }

    interface ICompanyRegistry with
        member _.Coverage = coverage

        member _.TryFindAsync(number: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                match Switzerland.uidDigits number with
                | ValueNone ->
                    Task.FromResult(
                        Error(
                            RegistryFailure.InvalidRequest
                                "A UID is CHE and nine digits, the last a modulus 11 check digit"
                        )
                    )
                | ValueSome digits ->
                    task {
                        match! call "GetByUID" (Switzerland.getByUidEnvelope digits) (Switzerland.found digits) ct with
                        | Error failure -> return Error failure
                        | Ok(reply, (value, raw)) ->
                            let evidence = SourceAnswer.evidence coverage reply ValueNone value

                            return
                                Ok(
                                    match raw with
                                    | ValueSome record -> { evidence with Raw = ValueSome record }
                                    | ValueNone -> evidence
                                )
                    })

        member _.SearchAsync(name: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                if String.IsNullOrWhiteSpace name then
                    Task.FromResult(Error(RegistryFailure.InvalidRequest "A search needs a term"))
                elif not (Switzerland.isXmlText name) then
                    Task.FromResult(
                        Error(
                            RegistryFailure.InvalidRequest
                                "A search term can't hold a control character or half a surrogate pair, which XML can't carry"
                        )
                    )
                else
                    task {
                        match! call "Search" (Switzerland.searchEnvelope name) Switzerland.search ct with
                        | Error failure -> return Error failure
                        | Ok(reply, found) ->
                            return Ok(SourceAnswer.evidence coverage reply ValueNone (Available found))
                    })

        member _.OfficersAsync(_: string, jurisdiction: Jurisdiction, _: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                Task.FromResult(
                    Ok(
                        SourceAnswer.unasked
                            coverage
                            client.Now
                            (NotAvailable "The UID register lists no officers; the commercial register does")
                    )
                ))

        member _.OwnersAsync(_: string, jurisdiction: Jurisdiction, _: OwnerPurpose, _: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                Task.FromResult(
                    Ok(SourceAnswer.unasked coverage client.Now (NotAvailable "The UID register lists no owners"))
                ))
