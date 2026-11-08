namespace CompanyDueDiligence

open System
open System.Text.Json
open System.Text.RegularExpressions
open System.Threading
open System.Threading.Tasks
open CompanyDueDiligence.Json

/// The ABR's ABN Lookup JSON services, written from abr.business.gov.au/json/ and its sample scripts as read on 2026-11-08. Not run live: every
/// call needs a GUID. Field names are the sample scripts'; the message texts are as a published client library quotes them, unverified.
module internal Australia =

    let coverage: RegistryCoverage =
        {
            Registry = "ABN Lookup (Australian Business Register)"
            Jurisdictions = [ Jurisdiction.ofCode "AU" ]
            Standing = Support.Partial "The ABN's status, Active or Cancelled, which isn't ASIC's company registration"
            Concerns = Support.NotOffered "ABN Lookup states no overdue filings, disputes or insolvency"
            Addresses = Support.Partial "The main business location's state and postcode only"
            Search = Support.Partial "Up to 20 names, ranked by the service, with no status"
            Officers = Support.NotOffered "ABN Lookup lists no officers; ASIC's register does"
            Owners = Support.NotOffered "ABN Lookup lists no owners"
            Cost =
                AccessCost.FreeWithKey
                    "A GUID the ABR sends by email once its Web Services Agreement is accepted, at abr.business.gov.au/Tools/WebServices"
            RateLimit = RateLimitPolicy.NotPublished(2, TimeSpan.FromSeconds 1.)
            Licence =
                "The ABR Web Services Agreement: extracts may be passed on at the user's own risk, and copies of data the ABR says is withdrawn must be deleted. Not run live"
        }

    /// What a number is looked up as.
    [<RequireQualifiedAccess>]
    type Lookup =
        | Abn of abn: string
        | Acn of acn: string

    let private abnRegex = Regex @"\A[0-9]{11}\z"
    let private acnRegex = Regex @"\A[0-9]{9}\z"

    let private digitsOf (text: string) =
        text |> Seq.map (fun c -> int c - int '0') |> List.ofSeq

    /// The ABR's ABN check: one off the first digit, then weights 10, 1, 3, ..., 19, and the sum divides by 89.
    let isAbn (text: string) =
        abnRegex.IsMatch text
        && (match digitsOf text with
            | first :: rest ->
                List.map2 (*) ((first - 1) :: rest) [ 10; 1; 3; 5; 7; 9; 11; 13; 15; 17; 19 ]
                |> List.sum
                |> fun sum -> sum % 89 = 0
            | [] -> false)

    /// ASIC's ACN check: weights 8 down to 1 on the first eight digits, and the ninth is the remainder's complement to ten.
    let isAcn (text: string) =
        acnRegex.IsMatch text
        && (let digits = digitsOf text
            let sum = List.map2 (*) (List.take 8 digits) [ 8; 7; 6; 5; 4; 3; 2; 1 ] |> List.sum
            (10 - sum % 10) % 10 = List.item 8 digits)

    /// Eleven digits that pass the ABN check, or nine that pass the ACN check. Spaces, as both are often written, are taken out.
    let lookupOf (number: string) =
        let digits = if isNull number then "" else number.Replace(" ", "")

        if isAbn digits then ValueSome(Lookup.Abn digits)
        elif isAcn digits then ValueSome(Lookup.Acn digits)
        else ValueNone

    /// The JSON inside the callback the request names. Anything else is not the answer the service documents.
    let unwrapped (body: string) =
        let text = if isNull body then "" else body.Trim()

        let text =
            if text.EndsWith ';' then
                text.Substring(0, text.Length - 1).TrimEnd()
            else
                text

        if text.StartsWith("callback(", StringComparison.Ordinal) && text.EndsWith(')') then
            ValueSome(text.Substring(9, text.Length - 10))
        else
            ValueNone

    /// The only two statuses ABN Lookup describes. An active ABN is a tax registration in use, which isn't ASIC's word on the company, so it is
    /// Registered.
    let standing (status: string voption) =
        match status |> ValueOption.map _.Trim() with
        | ValueSome "Active" -> CompanyStanding.Registered(ValueSome "ABN Active")
        | ValueSome "Cancelled" -> CompanyStanding.Closed "ABN cancelled"
        | ValueNone
        | ValueSome "" -> CompanyStanding.Unknown ValueNone
        | ValueSome other -> CompanyStanding.Unknown(ValueSome other)

    /// What an answer says, after its Message.
    [<RequireQualifiedAccess>]
    type Answer =
        | Found of Company
        /// The ABR blanks every detail but the ABN, its status and GST of an entity it suppresses.
        | Suppressed
        | NotHeld
        /// No ABN is linked to the ACN, which many companies lack; ASIC's register decides whether the company exists.
        | NoAbnForAcn
        | GuidRefused
        | Refused

    let private addresses (e: JsonElement) =
        match SourceAnswer.trimmed "AddressState" e, SourceAnswer.trimmed "AddressPostcode" e with
        | ValueNone, ValueNone -> NotAvailable "The answer holds no state or postcode"
        | state, postcode ->
            Available
                [
                    {
                        Kind = AddressKind.Other "Main business location on the ABR"
                        Address =
                            {
                                Lines = []
                                Locality = ValueNone
                                Region = state
                                PostalCode = postcode
                                Country = ValueSome "Australia"
                            }
                    }
                ]

    /// The ABR's entity types don't say where an entity was formed, so the role is NotStated.
    let company (e: JsonElement) : Company voption =
        let abn =
            SourceAnswer.trimmed "Abn" e
            |> ValueOption.defaultWith (fun () -> raise (InvalidJson "Missing or invalid field 'Abn'"))

        match SourceAnswer.trimmed "EntityName" e, SourceAnswer.trimmed "EntityTypeCode" e with
        | ValueNone, ValueNone -> ValueNone
        | ValueNone, ValueSome _ -> raise (InvalidJson "Missing or invalid field 'EntityName'")
        | ValueSome name, _ ->
            let status = SourceAnswer.trimmed "AbnStatus" e
            let standing = standing status

            ValueSome
                {
                    Name = name
                    OtherNames =
                        strings e "BusinessName"
                        |> List.map _.Trim()
                        |> List.filter (fun other -> other <> "" && other <> name)
                        |> List.distinct
                    Number = abn
                    Jurisdiction = Jurisdiction.ofCode "AU"
                    JurisdictionAsPublished = ValueNone
                    Standing = standing
                    Concerns = CompanyConcerns.notPublished
                    StatusAsPublished = status
                    LegalForm = SourceAnswer.trimmed "EntityTypeName" e
                    IncorporatedOn = ValueNone
                    CeasedOn =
                        match standing with
                        | CompanyStanding.Closed _ -> tryDate "AbnStatusEffectiveFrom" e
                        | _ -> ValueNone
                    Addresses = addresses e
                    Role = RecordRole.NotStated
                    HomeRegistration = ValueNone
                    EstablishmentOf = ValueNone
                }

    /// The answer for this lookup. A record for another number fails the read rather than being taken for it. No record for an ACN says only
    /// that no ABN is linked to it.
    let answer (lookup: Lookup) (e: JsonElement) =
        match (required "Message" tryString e).Trim(), lookup with
        | "No record found", Lookup.Abn _ -> Answer.NotHeld
        | "No record found", Lookup.Acn _ -> Answer.NoAbnForAcn
        | "The GUID entered is not recognised as a Registered Party", _ -> Answer.GuidRefused
        | "", _ ->
            match company e with
            | ValueNone -> Answer.Suppressed
            | ValueSome found ->
                let same =
                    match lookup with
                    | Lookup.Abn abn -> found.Number = abn
                    | Lookup.Acn acn -> SourceAnswer.trimmed "Acn" e = ValueSome acn

                if same then
                    Answer.Found found
                else
                    raise (InvalidJson "The answer holds another entity")
        | _ -> Answer.Refused

    let search (e: JsonElement) =
        match (required "Message" tryString e).Trim() with
        | "The GUID entered is not recognised as a Registered Party" ->
            Error(RegistryFailure.AccessRefused "The ABR doesn't recognise the GUID")
        | _ ->
            let matches =
                array e "Names"
                |> List.map (fun item ->
                    {
                        Name = required "Name" tryString item
                        Number = required "Abn" tryString item
                        StatusAsPublished = ValueNone
                        Address = ValueNone
                    })

            Ok
                {
                    Matches = matches
                    TotalMatches = ValueNone
                    Complete = false
                }

/// Australia as an ICompanyRegistry. The GUID travels in the query, so it is taken out of every failure and raw answer.
[<Sealed>]
type internal AustraliaRegistry(connection: SourceConnection, guid: SourceKey) =
    static let searchLimit = 20

    let client =
        SourceClient(connection, SourceAnswer.pacing 2 (TimeSpan.FromSeconds 1.))

    let coverage = Australia.coverage
    let baseUri = "https://abr.business.gov.au/json/"
    let hide = SourceAnswer.hiding [ guid.Value ]
    let hideText = SourceAnswer.hidingIn [ guid.Value ]

    let call (path: string) (ct: CancellationToken) =
        task {
            let uri =
                Uri $"{baseUri}{path}&callback=callback&guid={Uri.EscapeDataString guid.Value}"

            match! client.GetAsync(uri, ct) with
            | Error failure -> return Error(hide failure)
            | Ok reply when reply.Status = 404 -> return Error(RegistryFailure.RequestRefused "HTTP 404")
            | Ok reply ->
                return
                    match Australia.unwrapped reply.Body with
                    | ValueNone ->
                        Error(RegistryFailure.UnreadableAnswer "Not the callback answer the service documents")
                    | ValueSome inner -> Ok { reply with Body = hideText inner }
        }

    interface ICompanyRegistry with
        member _.Coverage = coverage

        member _.TryFindAsync(number: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                match Australia.lookupOf number with
                | ValueNone ->
                    Task.FromResult(
                        Error(
                            RegistryFailure.InvalidRequest
                                "An ABN has eleven digits and an ACN nine, each passing its check digit"
                        )
                    )
                | ValueSome lookup ->
                    task {
                        let path =
                            match lookup with
                            | Australia.Lookup.Abn abn -> $"AbnDetails.aspx?abn={abn}"
                            | Australia.Lookup.Acn acn -> $"AcnDetails.aspx?acn={acn}"

                        match! call path ct with
                        | Error failure -> return Error failure
                        | Ok reply ->
                            return
                                SourceAnswer.json reply (Australia.answer lookup)
                                |> Result.bind (fun answer ->
                                    let evidence = SourceAnswer.evidence coverage reply ValueNone

                                    match answer with
                                    | Australia.Answer.Found company -> Ok(evidence (Available(ValueSome company)))
                                    | Australia.Answer.Suppressed ->
                                        Ok(evidence (NotAvailable "The ABR suppresses this entity's details"))
                                    | Australia.Answer.NotHeld -> Ok(evidence (Available ValueNone))
                                    | Australia.Answer.NoAbnForAcn ->
                                        Ok(
                                            evidence (
                                                NotAvailable
                                                    "No ABN is linked to this ACN; ASIC's register decides whether the company exists"
                                            )
                                        )
                                    | Australia.Answer.GuidRefused ->
                                        Error(RegistryFailure.AccessRefused "The ABR doesn't recognise the GUID")
                                    | Australia.Answer.Refused ->
                                        Error(
                                            RegistryFailure.RequestRefused
                                                "The ABR answered with a message in place of a record"
                                        ))
                    })

        member _.SearchAsync(name: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                if String.IsNullOrWhiteSpace name then
                    Task.FromResult(Error(RegistryFailure.InvalidRequest "A search needs a term"))
                else
                    task {
                        match!
                            call
                                $"MatchingNames.aspx?name={Uri.EscapeDataString(name.Trim())}&maxResults={searchLimit}"
                                ct
                        with
                        | Error failure -> return Error failure
                        | Ok reply ->
                            return
                                SourceAnswer.json reply Australia.search
                                |> Result.bind id
                                |> Result.map (fun found ->
                                    SourceAnswer.evidence coverage reply ValueNone (Available found))
                    })

        member _.OfficersAsync(_: string, jurisdiction: Jurisdiction, _: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                Task.FromResult(
                    Ok(
                        SourceAnswer.unasked
                            coverage
                            client.Now
                            (NotAvailable "ABN Lookup lists no officers; ASIC's register does")
                    )
                ))

        member _.OwnersAsync(_: string, jurisdiction: Jurisdiction, _: OwnerPurpose, _: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                Task.FromResult(
                    Ok(SourceAnswer.unasked coverage client.Now (NotAvailable "ABN Lookup lists no owners"))
                ))
