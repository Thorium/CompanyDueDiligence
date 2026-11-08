namespace CompanyDueDiligence

open System
open System.Globalization
open System.Net.Http
open System.Security
open System.Text
open System.Text.RegularExpressions
open System.Threading
open System.Threading.Tasks
open System.Xml
open System.Xml.Linq
open CompanyDueDiligence.Json

/// The Firmenbuch HVD web service, written from WebService-HVD.zip as read on 2026-11-08: the Schnittstellenbeschreibung v1.3 and the
/// AuszugRequest_v2, AuszugResponse_v2 and SucheFirma schemas. Not run live: every call needs an X-API-KEY. The HVD extract is the
/// "Kurzinformation" only.
module internal Austria =

    let coverage: RegistryCoverage =
        {
            Registry = "Firmenbuch HVD web service (Austria)"
            Jurisdictions = [ Jurisdiction.ofCode "AT" ]
            Standing =
                Support.Partial
                    "Registered or deleted, from the deletion marks on the record's data groups. Legal facts such as insolvency are coded in RECHTSTATSACHE, whose codes the HVD spec doesn't list, and aren't read"
            Concerns =
                Support.Partial
                    "An address flagged undeliverable; legal facts such as insolvency aren't read, and overdue filings aren't published"
            Addresses = Support.Supported
            Search = Support.Partial "Registered and deleted companies matching the name, with no total"
            Officers = Support.NotOffered "The HVD spec doesn't say the Kurzinformation extract holds officers"
            Owners = Support.NotOffered "The Firmenbuch lists no beneficial owners; the WiEReG register does"
            Cost =
                AccessCost.FreeWithKey
                    "An X-API-KEY from JustizOnline, once the BMJ approves an application under the IWG"
            RateLimit = RateLimitPolicy.NotPublished(2, TimeSpan.FromSeconds 1.)
            Licence = "CC BY 4.0, as reported for the BMJ's HVD release; the terms weren't read here. Not run live"
        }

    let private soap = XNamespace.Get "http://www.w3.org/2003/05/soap-envelope"

    let private auszug =
        XNamespace.Get "ns://firmenbuch.justiz.gv.at/Abfrage/v2/AuszugResponse"

    let private suche =
        XNamespace.Get "ns://firmenbuch.justiz.gv.at/Abfrage/SucheFirmaResponse"

    let private fnrRegex = Regex @"\A0*([1-9][0-9]{0,5})\s*([A-Za-z])\z"

    /// A Firmenbuchnummer: up to six digits and a check letter, as in "123456a". Leading zeros and a space before the letter are taken out.
    let normalNumber (number: string) =
        let found = fnrRegex.Match(if isNull number then "" else number.Trim())

        if found.Success then
            ValueSome(found.Groups[1].Value + found.Groups[2].Value.ToLowerInvariant())
        else
            ValueNone

    /// The extract of the company as of the date, as the spec's example asks for it.
    let auszugRequest (fnr: string) (asOf: DateOnly) =
        let date = asOf.ToString("yyyy-MM-dd", CultureInfo.InvariantCulture)

        $"""<soap:Envelope xmlns:soap="http://www.w3.org/2003/05/soap-envelope" xmlns:aus="ns://firmenbuch.justiz.gv.at/Abfrage/v2/AuszugRequest"><soap:Header/><soap:Body><aus:AUSZUG_V2_REQUEST><aus:FNR>{SecurityElement.Escape fnr}</aus:FNR><aus:STICHTAG>{date}</aus:STICHTAG><aus:UMFANG>Kurzinformation</aus:UMFANG></aus:AUSZUG_V2_REQUEST></soap:Body></soap:Envelope>"""

    /// A search of registered and deleted companies, branches left out: SUCHBEREICH 1, phonetic.
    let searchRequest (term: string) =
        $"""<soap:Envelope xmlns:soap="http://www.w3.org/2003/05/soap-envelope" xmlns:suc="ns://firmenbuch.justiz.gv.at/Abfrage/SucheFirmaRequest"><soap:Header/><soap:Body><suc:SUCHEFIRMAREQUEST><suc:FIRMENWORTLAUT>{SecurityElement.Escape term}</suc:FIRMENWORTLAUT><suc:EXAKTESUCHE>false</suc:EXAKTESUCHE><suc:SUCHBEREICH>1</suc:SUCHBEREICH><suc:GERICHT></suc:GERICHT><suc:RECHTSFORM></suc:RECHTSFORM><suc:RECHTSEIGENSCHAFT></suc:RECHTSEIGENSCHAFT><suc:ORTNR></suc:ORTNR></suc:SUCHEFIRMAREQUEST></soap:Body></soap:Envelope>"""

    /// An attribute the schemas qualify, read with or without its namespace.
    let private attribute (ns: XNamespace) (name: string) (e: XElement) =
        match
            e.Attribute(ns + name)
            |> Option.ofObj
            |> Option.orElse (e.Attribute(XName.Get name) |> Option.ofObj)
        with
        | Some found when found.Value.Trim() <> "" -> ValueSome(found.Value.Trim())
        | _ -> ValueNone

    let private texts (ns: XNamespace) (name: string) (e: XElement) =
        e.Elements(ns + name)
        |> Seq.map _.Value.Trim()
        |> Seq.filter ((<>) "")
        |> List.ofSeq

    let private text (ns: XNamespace) (name: string) (e: XElement) =
        match e.Element(ns + name) with
        | null -> ValueNone
        | found when found.Value.Trim() = "" -> ValueNone
        | found -> ValueSome(found.Value.Trim())

    /// The data groups of one kind still in force, AUFRECHT true; the latest ones when none says so, as on a deleted company.
    let private inForce (name: string) (firma: XElement) =
        let groups = firma.Elements(auszug + name) |> List.ofSeq

        match
            groups
            |> List.filter (fun group -> attribute auszug "AUFRECHT" group = ValueSome "true")
        with
        | [] -> groups |> List.tryLast |> Option.toList
        | current -> current

    /// The Firmenbuch has no status word in the extract. A data group deleted with the company carries MIT_FIRMA_GELOESCHT_DURCH_VNR.
    let standing (deleted: bool) =
        if deleted then
            CompanyStanding.Closed "gelöscht"
        else
            CompanyStanding.Registered ValueNone

    /// A search hit's STATUS, whose words the schema gives as "historisch", "gelöscht" and empty.
    let searchStanding (status: string voption) =
        match status with
        | ValueNone -> CompanyStanding.Registered ValueNone
        | ValueSome "gelöscht" -> CompanyStanding.Closed "gelöscht"
        | ValueSome "historisch" -> CompanyStanding.Unknown(ValueSome "historisch")
        | ValueSome other -> CompanyStanding.Unknown(ValueSome other)

    let private address (group: XElement) =
        let street =
            match texts auszug "STELLE" group with
            | [] ->
                [ text auszug "STRASSE" group; text auszug "HAUSNUMMER" group ]
                |> List.choose ValueOption.toOption
                |> String.concat " "
                |> fun line ->
                    [
                        match text auszug "ZUSTELLANWEISUNG" group with
                        | ValueSome note -> note
                        | ValueNone -> ()
                        if line <> "" then
                            line
                        match text auszug "STIEGE" group, text auszug "TUERNUMMER" group with
                        | ValueNone, ValueNone -> ()
                        | stair, door ->
                            [
                                stair |> ValueOption.map (sprintf "Stiege %s")
                                door |> ValueOption.map (sprintf "Tür %s")
                            ]
                            |> List.choose ValueOption.toOption
                            |> String.concat " "
                    ]
            | lines -> lines

        {
            Kind = AddressKind.Other "Geschäftsanschrift in the Firmenbuch"
            Address =
                {
                    Lines = street
                    Locality = text auszug "ORT" group
                    Region = ValueNone
                    PostalCode = text auszug "PLZ" group
                    Country = text auszug "STAAT" group
                }
        }

    /// A foreign company's Austrian branch carries its foreign seat and number in FI_DKZ17.
    let private homeOf (firma: XElement) =
        match inForce "FI_DKZ17" firma with
        | [] -> ValueNone
        | group :: _ ->
            ValueSome
                {
                    LegalName = ValueNone
                    LegalForm = ValueNone
                    GoverningLaw = ValueNone
                    Country = ValueNone
                    Register = texts auszug "AUSL_SITZ" group |> List.tryHead |> ValueOption.ofOption
                    RegistrationNumber = texts auszug "AUSL_FN" group |> List.tryHead |> ValueOption.ofOption
                }

    /// The Body of a SOAP 1.2 answer, or what stands in for it: Error true for a fault.
    let private body (document: XDocument) =
        match document.Root with
        | null -> raise (InvalidJson "The answer is empty")
        | root ->
            match root.Element(soap + "Body") with
            | null -> raise (InvalidJson "The answer has no SOAP body")
            | found when not (isNull (found.Element(soap + "Fault"))) -> Error()
            | found -> Ok found

    /// Parses the answer's XML. An answer that isn't XML, or lacks what parse requires, is UnreadableAnswer, quoting nothing from it.
    let xml (reply: SourceReply) (parse: XDocument -> 'T) =
        try
            Ok(parse (XDocument.Parse(reply.Body)))
        with
        | :? XmlException as error -> Error(RegistryFailure.UnreadableAnswer $"Not XML, at line {error.LineNumber}")
        | InvalidJson reason -> Error(RegistryFailure.UnreadableAnswer reason)

    let company (response: XElement) : Company * DateOnly voption =
        let firma =
            match response.Element(auszug + "FIRMA") with
            | null -> raise (InvalidJson "The extract has no FIRMA")
            | found -> found

        let name =
            inForce "FI_DKZ02" firma
            |> List.collect (texts auszug "BEZEICHNUNG")
            |> String.concat " "
            |> fun name ->
                if name = "" then
                    raise (InvalidJson "The extract has no name")
                else
                    name

        let number =
            attribute auszug "FNR" response
            |> ValueOption.bind normalNumber
            |> ValueOption.defaultWith (fun () -> raise (InvalidJson "The extract has no FNR"))

        let deleted =
            firma.Elements()
            |> Seq.exists (fun group -> (attribute auszug "MIT_FIRMA_GELOESCHT_DURCH_VNR" group).IsSome)

        let addressGroups = inForce "FI_DKZ03" firma

        let undeliverable =
            addressGroups
            |> List.exists (fun group -> text auszug "ZUSTELLBAR" group = ValueSome "false")

        let deliverable =
            not addressGroups.IsEmpty
            && addressGroups
               |> List.forall (fun group -> text auszug "ZUSTELLBAR" group = ValueSome "true")

        let home = homeOf firma

        let asOf =
            attribute auszug "STICHTAG" response
            |> ValueOption.bind (fun text ->
                match
                    DateOnly.TryParseExact(text, "yyyy-MM-dd", CultureInfo.InvariantCulture, DateTimeStyles.None)
                with
                | true, date -> ValueSome date
                | _ -> ValueNone)

        {
            Name = name
            OtherNames = []
            Number = number
            Jurisdiction = Jurisdiction.ofCode "AT"
            JurisdictionAsPublished = ValueNone
            Standing = standing deleted
            Concerns =
                {
                    Stated =
                        if undeliverable then
                            [ CompanyConcern.RegisteredOfficeUndeliverable ]
                        else
                            []
                    Unconfirmed =
                        if undeliverable || deliverable then
                            CompanyConcern.all
                            |> List.except [ CompanyConcern.RegisteredOfficeUndeliverable ]
                        else
                            CompanyConcern.all
                }
            StatusAsPublished = if deleted then ValueSome "gelöscht" else ValueNone
            LegalForm =
                inForce "FI_DKZ07" firma
                |> List.tryHead
                |> Option.bind (fun group -> group.Element(auszug + "RECHTSFORM") |> Option.ofObj)
                |> Option.bind (fun form -> text auszug "TEXT" form |> ValueOption.toOption)
                |> ValueOption.ofOption
            IncorporatedOn = ValueNone
            CeasedOn = ValueNone
            Addresses =
                match addressGroups with
                | [] -> NotAvailable "The extract holds no business address"
                | groups -> Available(groups |> List.map address)
            Role =
                if home.IsSome then
                    RecordRole.ForeignRegistration
                else
                    RecordRole.Home
            HomeRegistration = home
            EstablishmentOf = ValueNone
        },
        asOf

    /// The company in an extract for this number. A fault is Error; an extract of another number fails the read.
    let lookup (fnr: string) (document: XDocument) =
        match body document with
        | Error() -> Error()
        | Ok found ->
            match found.Element(auszug + "AUSZUG_V2_RESPONSE") with
            | null -> raise (InvalidJson "The answer holds no extract")
            | response ->
                let company, asOf = company response

                if company.Number = fnr then
                    Ok(company, asOf, response.ToString(SaveOptions.DisableFormatting))
                else
                    raise (InvalidJson "The extract is of another company")

    let search (document: XDocument) =
        match body document with
        | Error() -> Error()
        | Ok found ->
            match found.Element(suche + "SUCHEFIRMARESPONSE") with
            | null -> raise (InvalidJson "The answer holds no search result")
            | response ->
                let matches =
                    response.Elements(suche + "ERGEBNIS")
                    |> Seq.map (fun hit ->
                        let names = texts suche "NAME" hit

                        {
                            Name =
                                if names.IsEmpty then
                                    raise (InvalidJson "A search hit has no name")
                                else
                                    String.concat " " names
                            Number =
                                text suche "FNR" hit
                                |> ValueOption.defaultWith (fun () -> raise (InvalidJson "A search hit has no FNR"))
                            StatusAsPublished = text suche "STATUS" hit
                            Address =
                                text suche "SITZ" hit
                                |> ValueOption.map (fun seat ->
                                    {
                                        Lines = []
                                        Locality = ValueSome seat
                                        Region = ValueNone
                                        PostalCode = ValueNone
                                        Country = ValueNone
                                    })
                        })
                    |> List.ofSeq

                Ok
                    {
                        Matches = matches
                        TotalMatches = ValueNone
                        Complete = false
                    }

/// Austria as an ICompanyRegistry, over SOAP 1.2 with the key in X-API-KEY.
[<Sealed>]
type internal AustriaRegistry(connection: SourceConnection, key: SourceKey) =
    let client =
        SourceClient(connection, SourceAnswer.pacing 2 (TimeSpan.FromSeconds 1.))

    let coverage = Austria.coverage
    let endpoint = Uri "https://justizonline.gv.at/jop/api/at.gv.justiz.fbw/ws"
    let hide = SourceAnswer.hiding [ key.Value ]

    let post (envelope: string) (ct: CancellationToken) =
        task {
            let content () : HttpContent =
                new StringContent(envelope, Encoding.UTF8, "application/soap+xml")

            match! client.PostAsync(endpoint, content, [ "X-API-KEY", key.Value ], ct) with
            | Error failure -> return Error(hide failure)
            | Ok reply when reply.Status = 404 -> return Error(RegistryFailure.RequestRefused "HTTP 404")
            | Ok reply -> return Ok reply
        }

    /// The spec ties no fault to a number not held, so a fault is refused rather than read as absence.
    let fault = RegistryFailure.RequestRefused "The service answered with a SOAP fault"

    interface ICompanyRegistry with
        member _.Coverage = coverage

        member _.TryFindAsync(number: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                match Austria.normalNumber number with
                | ValueNone ->
                    Task.FromResult(
                        Error(
                            RegistryFailure.InvalidRequest
                                "A Firmenbuchnummer has up to six digits and a check letter, as in 123456a"
                        )
                    )
                | ValueSome fnr ->
                    task {
                        // Today in UTC is never after today in Vienna, and the service refuses a STICHTAG in the future.
                        let today = DateOnly.FromDateTime(client.Now.UtcDateTime)

                        match! post (Austria.auszugRequest fnr today) ct with
                        | Error failure -> return Error failure
                        | Ok reply ->
                            return
                                Austria.xml reply (Austria.lookup fnr)
                                |> Result.bind (fun found ->
                                    match found with
                                    | Error() -> Error fault
                                    | Ok(company, asOf, raw) ->
                                        Ok
                                            { SourceAnswer.evidence coverage reply asOf (Available(ValueSome company)) with
                                                Raw = ValueSome raw
                                            })
                    })

        member _.SearchAsync(name: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                if String.IsNullOrWhiteSpace name then
                    Task.FromResult(Error(RegistryFailure.InvalidRequest "A search needs a term"))
                else
                    task {
                        match! post (Austria.searchRequest (name.Trim())) ct with
                        | Error failure -> return Error failure
                        | Ok reply ->
                            return
                                Austria.xml reply Austria.search
                                |> Result.bind (fun found ->
                                    match found with
                                    | Error() -> Error fault
                                    | Ok result ->
                                        Ok(SourceAnswer.evidence coverage reply ValueNone (Available result)))
                    })

        member _.OfficersAsync(_: string, jurisdiction: Jurisdiction, _: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                Task.FromResult(
                    Ok(
                        SourceAnswer.unasked
                            coverage
                            client.Now
                            (NotAvailable "The HVD spec doesn't say the Kurzinformation extract holds officers")
                    )
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
                                "Unverified: those the WiEReG register of beneficial owners admits, such as obliged entities under anti-money-laundering rules"
                                "Apply to the BMF for WiEReG register extracts, or use a vendor")
                    )
                ))
