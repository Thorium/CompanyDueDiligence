namespace CompanyDueDiligence

open System
open System.Text.Json
open System.Text.RegularExpressions
open System.Threading
open System.Threading.Tasks
open CompanyDueDiligence.Json

/// GEMI's open data API, written from its swagger document at opendata-api.businessportal.gr/api-docs (1.0.0) as read on 2026-11-08. Not
/// run live: every call needs an api_key. The 8-a-minute pace is a third party's figure, unverified.
module internal Greece =

    let coverage: RegistryCoverage =
        {
            Registry = "GEMI open data (Greece)"
            Jurisdictions = [ Jurisdiction.ofCode "GR" ]
            Standing =
                Support.Partial
                    "Whether GEMI's status codebook flags the company's status active; a status it doesn't flag active is Unknown, with its word, and a word of liquidation or bankruptcy decides whatever the flag says"
            Concerns = Support.NotOffered "The API states no overdue filings, disputes or insolvency as such"
            Addresses = Support.Supported
            Search = Support.Partial "The first 20 matches on name or distinctive title, with the total GEMI states"
            Officers =
                Support.Partial
                    "The persons GEMI lists, in its own role words; partners and shareholders are among them"
            Owners = Support.NotOffered "The API lists no beneficial owners"
            Cost =
                AccessCost.FreeWithKey
                    "An api_key GEMI issues once it approves the application form at opendata.businessportal.gr"
            RateLimit = RateLimitPolicy.NotPublished(8, TimeSpan.FromMinutes 1.)
            Licence = "ODC-BY-1.0, as the API's swagger document states. Not run live"
        }

    /// What a number is looked up as: a GEMI number, or an AFM, the tax number, written with EL or AFM in front.
    [<RequireQualifiedAccess>]
    type Lookup =
        | Gemi of number: string
        | Afm of afm: string

    let private gemiRegex = Regex @"\A0*[1-9][0-9]{0,11}\z"
    let private afmRegex = Regex @"\A(?:EL|AFM|ΑΦΜ)[:\s]*([0-9]{9})\z"

    /// The AFM's check: the first eight digits weighted by 256 down to 2, and the sum's remainder by 11, taken modulo 10, is the ninth digit.
    let isAfm (digits: string) =
        digits.Length = 9
        && digits |> Seq.forall Char.IsAsciiDigit
        && (let values = digits |> Seq.map (fun c -> int c - int '0') |> List.ofSeq

            let sum =
                List.map2 (*) (List.take 8 values) [ 256; 128; 64; 32; 16; 8; 4; 2 ] |> List.sum

            sum % 11 % 10 = values[8])

    /// Bare digits are a GEMI number, sent without its leading zeros, since the API types it as an integer. A nine-digit AFM needs its prefix,
    /// because a GEMI number can have nine digits too, and nine bare digits passing the AFM check are refused as ambiguous.
    let lookupOf (number: string) : Result<Lookup, string> =
        let normal =
            if isNull number then
                ""
            else
                number.Trim().ToUpperInvariant()

        let afm = afmRegex.Match normal

        if afm.Success then
            Ok(Lookup.Afm afm.Groups[1].Value)
        elif isAfm normal then
            Error
                "Nine bare digits passing the AFM check could be an AFM or a GEMI number: write an AFM as EL followed by it, or give the GEMI number"
        elif gemiRegex.IsMatch normal then
            Ok(Lookup.Gemi(normal.TrimStart '0'))
        else
            Error "A GEMI number has up to twelve digits; an AFM is nine digits after EL or AFM"

    /// True for an answer in the error shape the swagger documents: a list of entries, each with a code or a message.
    let isErrorList (body: string) =
        try
            use document = JsonDocument.Parse body
            let root = document.RootElement

            root.ValueKind = JsonValueKind.Array
            && root.GetArrayLength() > 0
            && root.EnumerateArray()
               |> Seq.forall (fun entry -> (tryString "code" entry).IsSome || (tryString "message" entry).IsSome)
        with :? JsonException ->
            false

    /// Status words of liquidation and bankruptcy, which decide whatever the codebook's flag says.
    let private proceedingOf (word: string) =
        let bare =
            word.Normalize Text.NormalizationForm.FormD
            |> String.filter (fun c ->
                Globalization.CharUnicodeInfo.GetUnicodeCategory c
                <> Globalization.UnicodeCategory.NonSpacingMark)
            |> _.ToUpperInvariant()

        if bare.Contains "ΠΤΩΧΕΥ" || bare.Contains "BANKRUPT" then
            ValueSome(CompanyStanding.InInsolvency word)
        elif bare.Contains "ΕΚΚΑΘΑΡΙΣ" || bare.Contains "LIQUIDATION" then
            ValueSome(CompanyStanding.WindingUp word)
        else
            ValueNone

    /// The status codebook: each id with whether GEMI calls it active.
    let statuses (root: JsonElement) =
        if root.ValueKind <> JsonValueKind.Array then
            raise (InvalidJson "The status codebook isn't a list")

        root.EnumerateArray()
        |> Seq.map (fun item ->
            match tryInt "id" item, tryFlag "isActive" item with
            | ValueSome pid, ValueSome active -> pid, active
            | _ -> raise (InvalidJson "A status in the codebook has no id or isActive"))
        |> Map.ofSeq

    /// The swagger advises isActive from the codebook over reading the words, which it doesn't list. A word of liquidation or bankruptcy decides
    /// first, whatever the flag says.
    let standing (codebook: Map<int, bool>) (statusId: int voption) (word: string voption) =
        match word |> ValueOption.bind proceedingOf, statusId with
        | ValueSome proceeding, _ -> proceeding
        | ValueNone, ValueNone -> CompanyStanding.Unknown word
        | ValueNone, ValueSome id ->
            match Map.tryFind id codebook with
            | Some true -> CompanyStanding.Active
            | Some false
            | None -> CompanyStanding.Unknown(word |> ValueOption.orElse (ValueSome $"Status {id}"))

    let private described name e =
        tryObject name e |> ValueOption.bind (SourceAnswer.trimmed "descr")

    let private addresses (e: JsonElement) =
        let street =
            [ SourceAnswer.trimmed "street" e; SourceAnswer.trimmed "streetNumber" e ]
            |> List.choose ValueOption.toOption
            |> String.concat " "

        let lines =
            [
                if street <> "" then
                    street
                match SourceAnswer.trimmed "poBox" e with
                | ValueSome box -> $"PO Box {box}"
                | ValueNone -> ()
            ]

        match lines, SourceAnswer.trimmed "city" e, SourceAnswer.trimmed "zipCode" e with
        | [], ValueNone, ValueNone -> NotAvailable "The answer holds no address"
        | _, city, zip ->
            Available
                [
                    {
                        Kind = AddressKind.Other "Address on GEMI"
                        Address =
                            {
                                Lines = lines
                                Locality = city |> ValueOption.orElse (described "municipality" e)
                                Region = described "prefecture" e
                                PostalCode = zip
                                Country = ValueNone
                            }
                    }
                ]

    let company (codebook: Map<int, bool>) (e: JsonElement) : Company =
        let name = required "coNameEl" SourceAnswer.trimmed e
        let status = tryObject "status" e
        let word = status |> ValueOption.bind (SourceAnswer.trimmed "descr")

        {
            Name = name
            OtherNames =
                [ "coNamesEn"; "coTitlesEl"; "coTitlesEn" ]
                |> List.collect (strings e)
                |> List.map _.Trim()
                |> List.filter (fun other -> other <> "" && other <> name)
                |> List.distinct
            Number = required "arGemi" OpenData.text e
            Jurisdiction = Jurisdiction.ofCode "GR"
            JurisdictionAsPublished = ValueNone
            Standing = standing codebook (status |> ValueOption.bind (tryInt "id")) word
            Concerns = CompanyConcerns.notPublished
            StatusAsPublished = word
            LegalForm = described "legalType" e
            IncorporatedOn = SourceAnswer.dateOfTimestamp "incorporationDate" e
            CeasedOn = ValueNone
            Addresses = addresses e
            Role =
                match tryFlag "isBranch" e with
                | ValueSome false -> RecordRole.Home
                | ValueSome true
                | ValueNone -> RecordRole.NotStated
            HomeRegistration = ValueNone
            EstablishmentOf = ValueNone
        }

    /// A date that decides whether a role has ended: missing or null is none, and one that can't be read fails the answer.
    let private endDate name e =
        match tryProp name e with
        | ValueNone -> ValueNone
        | ValueSome _ ->
            match SourceAnswer.dateOfTimestamp name e with
            | ValueSome date -> ValueSome date
            | ValueNone -> raise (InvalidJson $"Field '{name}' is not a date")

    let private officer (today: DateOnly) (e: JsonElement) : Officer =
        let person = SourceAnswer.trimmed "personName" e
        let entity = SourceAnswer.trimmed "businessName" e
        let ended = endDate "dtTo" e

        {
            Name =
                person
                |> ValueOption.orElse entity
                |> ValueOption.defaultValue PartyNames.NotPublished
            Kind =
                match person, entity with
                | ValueSome _, _ -> PartyKind.Individual
                | ValueNone, ValueSome _ -> PartyKind.LegalEntity
                | ValueNone, ValueNone -> PartyKind.Withheld
            Role = OfficerRole.Other(SourceAnswer.trimmed "role" e |> ValueOption.defaultValue "")
            Current =
                match ended with
                | ValueSome date -> date > today
                | ValueNone -> true
            AppointedOn = SourceAnswer.dateOfTimestamp "dtFrom" e
            EndedOn = ended
            BornIn = ValueNone
            Nationality = ValueNone
            CountryOfResidence = ValueNone
            Address = ValueNone
            Entity =
                match person, entity with
                | ValueNone, ValueSome name ->
                    ValueSome
                        {
                            LegalName = ValueSome name
                            LegalForm = ValueNone
                            GoverningLaw = ValueNone
                            Country = ValueNone
                            Register = ValueNone
                            RegistrationNumber = ValueNone
                        }
                | _ -> ValueNone
            IdentityVerification = ValueNone
        }

    let officers (today: DateOnly) (e: JsonElement) =
        match array e "persons" with
        | [] -> NotAvailable "GEMI lists no persons for this company, which doesn't say it has none"
        | items -> Available(items |> List.map (officer today))

    /// The record for this lookup, read by read. A GEMI number is one record. An AFM is a search, which can come back empty, since not every
    /// holder of an AFM is on GEMI, or hold several.
    let recordOf (lookup: Lookup) (read: JsonElement -> 'T) (root: JsonElement) : Availability<('T * string) voption> =
        let checkedOne (record: JsonElement) =
            let same =
                match lookup with
                | Lookup.Gemi number ->
                    OpenData.text "arGemi" record |> ValueOption.map _.TrimStart('0') = ValueSome number
                | Lookup.Afm afm -> SourceAnswer.trimmed "afm" record = ValueSome afm

            if same then
                Available(ValueSome(read record, record.GetRawText()))
            else
                raise (InvalidJson "The answer holds another company")

        match lookup with
        | Lookup.Gemi _ ->
            if root.ValueKind <> JsonValueKind.Object then
                raise (InvalidJson "The answer isn't a company")

            checkedOne root
        | Lookup.Afm _ ->
            match tryProp "searchResults" root with
            | ValueSome list when list.ValueKind = JsonValueKind.Array ->
                match list.EnumerateArray() |> List.ofSeq with
                | [] -> NotAvailable "GEMI holds no company with this AFM, and not every holder of an AFM is on GEMI"
                | [ only ] -> checkedOne only
                | several ->
                    NotAvailable $"{several.Length} companies carry this AFM, so GEMI can't tell which is meant"
            | _ -> raise (InvalidJson "The answer has no searchResults list")

    let search (codebook: Map<int, bool>) (limit: int) (root: JsonElement) =
        let companies =
            match tryProp "searchResults" root with
            | ValueSome list when list.ValueKind = JsonValueKind.Array ->
                list.EnumerateArray() |> Seq.map (company codebook) |> List.ofSeq
            | _ -> raise (InvalidJson "The answer has no searchResults list")

        let total =
            tryObject "searchMetadata" root |> ValueOption.bind (tryInt "totalCount")

        OpenData.matches limit total companies

/// Greece as an ICompanyRegistry. The status codebook is read once and kept.
[<Sealed>]
type internal GreeceRegistry(connection: SourceConnection, key: SourceKey) =
    static let searchLimit = 20

    let client =
        SourceClient(connection, SourceAnswer.pacing 8 (TimeSpan.FromMinutes 1.))

    let coverage = Greece.coverage
    let baseUri = "https://opendata-api.businessportal.gr/api/opendata/v1"
    let headers = [ "api_key", key.Value; "Accept", "application/json" ]
    let hide = SourceAnswer.hiding [ key.Value ]

    /// Kept once read, as the codebook changes rarely; a failed read isn't kept.
    let mutable codebook: Map<int, bool> voption = ValueNone

    let get (path: string) (ct: CancellationToken) =
        task {
            match! client.GetAsync(Uri(baseUri + path), headers, ct) with
            | Error failure -> return Error(hide failure)
            | Ok reply -> return Ok reply
        }

    let statusesAsync (ct: CancellationToken) =
        task {
            match codebook with
            | ValueSome held -> return Ok held
            | ValueNone ->
                match! get "/metadata/companyStatuses" ct with
                | Error failure -> return Error failure
                | Ok reply when reply.Status = 404 ->
                    return Error(RegistryFailure.RequestRefused "HTTP 404 from the status codebook")
                | Ok reply ->
                    return
                        match SourceAnswer.json reply Greece.statuses with
                        | Error failure -> Error failure
                        | Ok read ->
                            codebook <- ValueSome read
                            Ok read
        }

    let offNumber number (send: Greece.Lookup -> Task<Result<Evidence<'T>, RegistryFailure>>) =
        match Greece.lookupOf number with
        | Ok lookup -> send lookup
        | Error reason -> Task.FromResult(Error(RegistryFailure.InvalidRequest reason))

    /// A 404 without the documented error list, such as a gateway's, says nothing of the company.
    let unmarked =
        RegistryFailure.RequestRefused "HTTP 404 without the error list GEMI documents"

    /// The record answer for a lookup. A 404 in GEMI's error shape on a GEMI number says no company has it; on an AFM search it says none was
    /// found.
    let readAsync (lookup: Greece.Lookup) (read: JsonElement -> 'T) (ct: CancellationToken) =
        task {
            let path =
                match lookup with
                | Greece.Lookup.Gemi number -> $"/companies/{number}"
                | Greece.Lookup.Afm afm -> $"/companies?afm={afm}&resultsSize=2"

            match! get path ct with
            | Error failure -> return Error failure
            | Ok reply when reply.Status = 404 && not (Greece.isErrorList reply.Body) -> return Error unmarked
            | Ok reply when reply.Status = 404 ->
                return
                    match lookup with
                    | Greece.Lookup.Gemi _ -> Ok(reply, Available ValueNone)
                    | Greece.Lookup.Afm _ ->
                        Ok(
                            reply,
                            NotAvailable
                                "GEMI holds no company with this AFM, and not every holder of an AFM is on GEMI"
                        )
            | Ok reply ->
                return
                    SourceAnswer.json reply (Greece.recordOf lookup read)
                    |> Result.map (fun found -> reply, found)
        }

    /// Evidence of a record answer, with the record's own JSON as raw.
    let evidenceOf
        (reply: SourceReply)
        (found: Availability<('T * string) voption>)
        : Evidence<Availability<'T voption>> =
        match found with
        | Available(ValueSome(value, raw)) ->
            { SourceAnswer.evidence coverage reply ValueNone (Available(ValueSome value)) with
                Raw = ValueSome raw
            }
        | Available ValueNone ->
            { SourceAnswer.evidence coverage reply ValueNone (Available ValueNone) with
                Raw = ValueNone
            }
        | NotAvailable reason -> SourceAnswer.evidence coverage reply ValueNone (NotAvailable reason)
        | NeedsEntitlement entitlement -> SourceAnswer.evidence coverage reply ValueNone (NeedsEntitlement entitlement)

    interface ICompanyRegistry with
        member _.Coverage = coverage

        member _.TryFindAsync(number: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                offNumber number (fun lookup ->
                    task {
                        match! statusesAsync ct with
                        | Error failure -> return Error failure
                        | Ok statuses ->
                            match! readAsync lookup (Greece.company statuses) ct with
                            | Error failure -> return Error failure
                            | Ok(reply, found) -> return Ok(evidenceOf reply found)
                    }))

        member _.SearchAsync(name: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                if String.IsNullOrWhiteSpace name then
                    Task.FromResult(Error(RegistryFailure.InvalidRequest "A search needs a term"))
                else
                    task {
                        match! statusesAsync ct with
                        | Error failure -> return Error failure
                        | Ok statuses ->
                            match!
                                get $"/companies?name={Uri.EscapeDataString(name.Trim())}&resultsSize={searchLimit}" ct
                            with
                            | Error failure -> return Error failure
                            | Ok reply when reply.Status = 404 && not (Greece.isErrorList reply.Body) ->
                                return Error unmarked
                            | Ok reply when reply.Status = 404 ->
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
                            | Ok reply ->
                                return
                                    SourceAnswer.json reply (Greece.search statuses searchLimit)
                                    |> Result.map (fun found ->
                                        SourceAnswer.evidence coverage reply ValueNone (Available found))
                    })

        member _.OfficersAsync(number: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                offNumber number (fun lookup ->
                    task {
                        let today = DateOnly.FromDateTime(client.Now.UtcDateTime)

                        match! readAsync lookup (Greece.officers today) ct with
                        | Error failure -> return Error failure
                        | Ok(reply, found) ->
                            let evidence = evidenceOf reply found

                            let officers =
                                match evidence.Value with
                                | Available(ValueSome officers) -> officers
                                | Available ValueNone -> NotAvailable "GEMI holds no company with this number"
                                | NotAvailable reason -> NotAvailable reason
                                | NeedsEntitlement entitlement -> NeedsEntitlement entitlement

                            return
                                Ok
                                    {
                                        Source = evidence.Source
                                        RetrievedAt = evidence.RetrievedAt
                                        AsOf = evidence.AsOf
                                        Raw = evidence.Raw
                                        Value = officers
                                    }
                    }))

        member _.OwnersAsync(_: string, jurisdiction: Jurisdiction, purpose: OwnerPurpose, _: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                Task.FromResult(
                    Ok(
                        SourceAnswer.unasked
                            coverage
                            client.Now
                            (SourceAnswer.restrictedOwners
                                purpose
                                "Unverified: those the Greek register of beneficial owners admits, such as obliged entities under anti-money-laundering rules"
                                "Apply to the Greek register of beneficial owners, or use a vendor")
                    )
                ))
