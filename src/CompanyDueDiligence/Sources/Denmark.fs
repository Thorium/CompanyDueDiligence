namespace CompanyDueDiligence

open System
open System.Net.Http
open System.Text
open System.Text.Json
open System.Text.RegularExpressions
open System.Threading
open System.Threading.Tasks
open CompanyDueDiligence.Json

/// CVR's system-to-system Elasticsearch, written from Erhvervsstyrelsen's guide "Kom godt igang med Elasticsearch" (version 1, 3 November
/// 2025) as read on 2026-11-08. Not run live: every call needs a user ID and password. Only the fields the guide shows are read.
module internal Denmark =

    let coverage: RegistryCoverage =
        {
            Registry = "CVR system-to-system (Denmark)"
            Jurisdictions = [ Jurisdiction.ofCode "DK" ]
            Standing =
                Support.Partial
                    "sammensatStatus, of whose words the guide names only normal and under konkurs, read with whether the life cycle is still open; any other word is Unknown"
            Concerns = Support.Partial "Bankruptcy when sammensatStatus says under konkurs; nothing else is read"
            Addresses = Support.Partial "The newest location address (beliggenhedsadresse)"
            Search = Support.Partial "20 names holding every word of the term, ranked by Elasticsearch"
            Officers = Support.NotOffered "The guide doesn't describe the participant relations, so they aren't read"
            Owners = Support.NotOffered "The guide doesn't describe the participant relations, so owners aren't read"
            Cost =
                AccessCost.FreeWithKey
                    "A user ID and password from Erhvervsstyrelsen (cvrselvbetjening@erst.dk), passed as ClientCredentials"
            RateLimit = RateLimitPolicy.NotPublished(2, TimeSpan.FromSeconds 1.)
            Licence =
                "Terms not read: the guide states no licence and cites the CVR Act. Its endpoint is plain http, so the credentials travel unencrypted. Not run live"
        }

    let private cvrRegex = Regex @"\A[0-9]{8}\z"

    /// Eight digits. Spaces, and a DK in front as on an invoice, are taken out.
    let normalNumber (number: string) =
        let normal =
            if isNull number then
                ""
            else
                number.Trim().ToUpperInvariant().Replace(" ", "")

        let normal =
            if normal.StartsWith "DK" then
                normal.Substring 2
            else
                normal

        if cvrRegex.IsMatch normal then
            ValueSome normal
        else
            ValueNone

    let private fields =
        [|
            "Vrvirksomhed.cvrNummer"
            "Vrvirksomhed.virksomhedMetadata.nyesteNavn.navn"
            "Vrvirksomhed.virksomhedMetadata.nyesteBeliggenhedsadresse"
            "Vrvirksomhed.virksomhedMetadata.sammensatStatus"
            "Vrvirksomhed.livsforloeb"
        |]

    /// The guide's term query on the CVR number, asking for two hits so that a second one shows.
    let lookupQuery (cvr: string) =
        JsonSerializer.Serialize
            {|
                _source = fields
                query =
                    {|
                        term = dict [ "Vrvirksomhed.cvrNummer", cvr ]
                    |}
                size = 2
            |}

    /// A match on the newest name holding every word, the term passed as a JSON string so that nothing in it is query syntax.
    let searchQuery (term: string) (size: int) =
        JsonSerializer.Serialize
            {|
                _source = fields
                query =
                    {|
                        ``match`` =
                            dict
                                [
                                    "Vrvirksomhed.virksomhedMetadata.nyesteNavn.navn",
                                    {| query = term; operator = "and" |}
                                ]
                    |}
                size = size
            |}

    /// Under konkurs and normal are the two words the guide names. An ended life cycle is the guide's sign of a company no longer active.
    let standing (status: string voption) (lifeOpen: bool voption) =
        let word = status |> ValueOption.map _.Trim() |> ValueOption.filter ((<>) "")

        match word |> ValueOption.map _.ToUpperInvariant(), lifeOpen with
        | ValueSome "UNDER KONKURS", _ -> CompanyStanding.InInsolvency word.Value
        | ValueSome "NORMAL", ValueSome true -> CompanyStanding.Active
        | ValueSome _, ValueSome false -> CompanyStanding.Closed $"{word.Value}, life cycle ended"
        | ValueNone, ValueSome false -> CompanyStanding.Closed "Life cycle ended"
        | ValueSome _, _ -> CompanyStanding.Unknown word
        | ValueNone, _ -> CompanyStanding.Unknown ValueNone

    let private address (e: JsonElement) =
        let number =
            [ OpenData.text "husnummerFra" e; SourceAnswer.trimmed "bogstavFra" e ]
            |> List.choose ValueOption.toOption
            |> String.concat ""

        let street =
            [
                SourceAnswer.trimmed "vejnavn" e
                (if number = "" then ValueNone else ValueSome number)
                OpenData.text "etage" e |> ValueOption.map (fun floor -> $"{floor}.")
                SourceAnswer.trimmed "sidedoer" e
            ]
            |> List.choose ValueOption.toOption
            |> String.concat " "

        {
            Kind = AddressKind.Other "Beliggenhedsadresse on CVR"
            Address =
                {
                    Lines =
                        [
                            match SourceAnswer.trimmed "conavn" e with
                            | ValueSome care -> $"c/o {care}"
                            | ValueNone -> ()
                            if street <> "" then
                                street
                            match OpenData.text "postboks" e with
                            | ValueSome box -> $"Postboks {box}"
                            | ValueNone -> ()
                            match SourceAnswer.trimmed "fritekst" e with
                            | ValueSome free -> free
                            | ValueNone -> ()
                        ]
                    Locality =
                        SourceAnswer.trimmed "postdistrikt" e
                        |> ValueOption.orElse (SourceAnswer.trimmed "bynavn" e)
                    Region = tryObject "kommune" e |> ValueOption.bind (SourceAnswer.trimmed "kommuneNavn")
                    PostalCode = OpenData.text "postnummer" e
                    Country = SourceAnswer.trimmed "landekode" e
                }
        }

    let company (vr: JsonElement) : Company =
        let metadata = tryObject "virksomhedMetadata" vr

        let name =
            metadata
            |> ValueOption.bind (tryObject "nyesteNavn")
            |> ValueOption.bind (SourceAnswer.trimmed "navn")
            |> ValueOption.defaultWith (fun () -> raise (InvalidJson "The record has no newest name"))

        let periods =
            array vr "livsforloeb"
            |> List.choose (fun life -> tryObject "periode" life |> ValueOption.toOption)

        let lifeOpen =
            match periods with
            | [] -> ValueNone
            | _ -> ValueSome(periods |> List.exists (fun period -> (tryProp "gyldigTil" period).IsNone))

        let status = metadata |> ValueOption.bind (SourceAnswer.trimmed "sammensatStatus")
        let standing = standing status lifeOpen

        {
            Name = name
            OtherNames = []
            Number = required "cvrNummer" OpenData.text vr
            Jurisdiction = Jurisdiction.ofCode "DK"
            JurisdictionAsPublished = ValueNone
            Standing = standing
            Concerns =
                match standing with
                | CompanyStanding.InInsolvency _ ->
                    {
                        Stated = [ CompanyConcern.InsolvencyHistory ]
                        Unconfirmed = CompanyConcern.all |> List.except [ CompanyConcern.InsolvencyHistory ]
                    }
                | _ -> CompanyConcerns.notPublished
            StatusAsPublished = status
            LegalForm = ValueNone
            IncorporatedOn =
                periods
                |> List.choose (tryDate "gyldigFra" >> ValueOption.toOption)
                |> List.sort
                |> List.tryHead
                |> ValueOption.ofOption
            CeasedOn =
                if lifeOpen = ValueSome false then
                    periods
                    |> List.choose (tryDate "gyldigTil" >> ValueOption.toOption)
                    |> List.sort
                    |> List.tryLast
                    |> ValueOption.ofOption
                else
                    ValueNone
            Addresses =
                match metadata |> ValueOption.bind (tryObject "nyesteBeliggenhedsadresse") with
                | ValueSome found -> Available [ address found ]
                | ValueNone -> NotAvailable "The record holds no location address"
            Role = RecordRole.NotStated
            HomeRegistration = ValueNone
            EstablishmentOf = ValueNone
        }

    /// The Vrvirksomhed of each hit, with the total Elasticsearch states.
    let hits (root: JsonElement) =
        match tryObject "hits" root with
        | ValueSome block ->
            match tryProp "hits" block with
            | ValueSome list when list.ValueKind = JsonValueKind.Array ->
                let records =
                    list.EnumerateArray()
                    |> Seq.map (fun hit ->
                        match tryObject "_source" hit |> ValueOption.bind (tryObject "Vrvirksomhed") with
                        | ValueSome vr -> vr
                        | ValueNone -> raise (InvalidJson "A hit has no Vrvirksomhed"))
                    |> List.ofSeq

                records,
                tryInt "total" block
                |> ValueOption.orElse (tryObject "total" block |> ValueOption.bind (tryInt "value"))
            | _ -> raise (InvalidJson "The answer has no hits list")
        | ValueNone -> raise (InvalidJson "The answer has no hits")

    /// Why the answer may hold only part of the hits: Elasticsearch timed out, or a shard failed or didn't answer.
    let incomplete (root: JsonElement) =
        let shards = tryObject "_shards" root

        let count name =
            shards |> ValueOption.bind (tryInt name)

        if tryFlag "timed_out" root = ValueSome true then
            ValueSome "Elasticsearch timed out, so the hits may be partial"
        elif count "failed" |> ValueOption.exists (fun failed -> failed > 0) then
            ValueSome "A shard failed, so the hits may be partial"
        else
            match count "successful", count "total" with
            | ValueSome successful, ValueSome total when successful < total ->
                ValueSome "Not every shard answered, so the hits may be partial"
            | _ -> ValueNone

    /// The company with this number: no hit says CVR holds none, as the guide's example of a search without results shows. An answer that may
    /// be partial is Unavailable.
    let lookup (cvr: string) (root: JsonElement) =
        match incomplete root with
        | ValueSome reason -> Error(RegistryFailure.Unavailable reason)
        | ValueNone ->
            match hits root with
            | [], _ -> Ok(Available ValueNone)
            | [ only ], _ ->
                let found = company only

                if found.Number = cvr then
                    Ok(Available(ValueSome(found, only.GetRawText())))
                else
                    raise (InvalidJson "The answer holds another company")
            | several, _ ->
                Ok(
                    NotAvailable
                        $"{several.Length} records carry this CVR number, so the source can't tell which is meant"
                )

    /// The matches, or Unavailable for an answer that may be partial.
    let search (limit: int) (root: JsonElement) =
        match incomplete root with
        | ValueSome reason -> Error(RegistryFailure.Unavailable reason)
        | ValueNone ->
            let records, total = hits root
            Ok(OpenData.matches limit total (records |> List.map company))

/// Denmark as an ICompanyRegistry, with the user ID and password as HTTP Basic credentials, as the guide's curl example sends them. Partial
/// results are refused with allow_partial_search_results=false, a parameter Elasticsearch takes in the URL and not in the query body.
[<Sealed>]
type internal DenmarkRegistry(connection: SourceConnection, credentials: ClientCredentials) =
    static let searchLimit = 20

    let client =
        SourceClient(connection, SourceAnswer.pacing 2 (TimeSpan.FromSeconds 1.))

    let coverage = Denmark.coverage

    let endpoint =
        Uri "http://distribution.virk.dk/cvr-permanent/virksomhed/_search?allow_partial_search_results=false"

    let basic =
        Convert.ToBase64String(Encoding.UTF8.GetBytes($"{credentials.Id}:{credentials.Secret}"))

    let hide = SourceAnswer.hiding [ credentials.Id; credentials.Secret; basic ]

    let post (query: string) (ct: CancellationToken) =
        task {
            let content () : HttpContent =
                new StringContent(query, Encoding.UTF8, "application/json")

            match! client.PostAsync(endpoint, content, [ "Authorization", "Basic " + basic ], ct) with
            | Error failure -> return Error(hide failure)
            | Ok reply when reply.Status = 404 -> return Error(RegistryFailure.RequestRefused "HTTP 404")
            | Ok reply -> return Ok reply
        }

    interface ICompanyRegistry with
        member _.Coverage = coverage

        member _.TryFindAsync(number: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                match Denmark.normalNumber number with
                | ValueNone -> Task.FromResult(Error(RegistryFailure.InvalidRequest "A CVR number has eight digits"))
                | ValueSome cvr ->
                    task {
                        match! post (Denmark.lookupQuery cvr) ct with
                        | Error failure -> return Error failure
                        | Ok reply ->
                            return
                                SourceAnswer.json reply (Denmark.lookup cvr)
                                |> Result.bind id
                                |> Result.map (fun found ->
                                    match found with
                                    | Available(ValueSome(company, raw)) ->
                                        { SourceAnswer.evidence
                                              coverage
                                              reply
                                              ValueNone
                                              (Available(ValueSome company)) with
                                            Raw = ValueSome raw
                                        }
                                    | Available ValueNone ->
                                        SourceAnswer.evidence coverage reply ValueNone (Available ValueNone)
                                    | NotAvailable reason ->
                                        SourceAnswer.evidence coverage reply ValueNone (NotAvailable reason)
                                    | NeedsEntitlement entitlement ->
                                        SourceAnswer.evidence coverage reply ValueNone (NeedsEntitlement entitlement))
                    })

        member _.SearchAsync(name: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                if String.IsNullOrWhiteSpace name then
                    Task.FromResult(Error(RegistryFailure.InvalidRequest "A search needs a term"))
                else
                    task {
                        match! post (Denmark.searchQuery (name.Trim()) searchLimit) ct with
                        | Error failure -> return Error failure
                        | Ok reply ->
                            return
                                SourceAnswer.json reply (Denmark.search searchLimit)
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
                            (NotAvailable "The guide doesn't describe the participant relations, so they aren't read")
                    )
                ))

        member _.OwnersAsync(_: string, jurisdiction: Jurisdiction, _: OwnerPurpose, _: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                Task.FromResult(
                    Ok(
                        SourceAnswer.unasked
                            coverage
                            client.Now
                            (NotAvailable "The guide doesn't describe the participant relations, so owners aren't read")
                    )
                ))
