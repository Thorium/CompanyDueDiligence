namespace CompanyDueDiligence

open System
open System.Text.Json
open System.Text.RegularExpressions
open System.Threading
open System.Threading.Tasks
open CompanyDueDiligence.Json

/// An LEI record: what the holder reported to GLEIF's data, as a cross-check. It is no register's answer, and only a firm with an LEI has one.
type LeiRecord =
    {
        Lei: string
        LegalName: string
        /// entity.otherNames and entity.transliteratedOtherNames.
        OtherNames: string list
        /// entity.status, such as ACTIVE.
        EntityStatus: string voption
        /// registration.status of the LEI itself, such as ISSUED or LAPSED.
        LeiStatus: string voption
        /// entity.jurisdiction, such as GB or US-DE.
        Jurisdiction: string voption
        /// entity.registeredAs: the number on its home register, as reported.
        RegisteredAs: string voption
        /// entity.registeredAt.id: GLEIF's code for that register.
        RegisteredAt: string voption
        LegalAddress: Address voption
        HeadquartersAddress: Address voption
        CreatedOn: DateOnly voption
        LastUpdatedOn: DateOnly voption
        /// registration.corroborationLevel.
        Corroboration: string voption
    }

/// The records reporting a number, by whether they can be taken for the company asked about.
type LeiMatches =
    {
        /// Reported in the jurisdiction asked.
        Matching: LeiRecord list
        /// Reported in the same country with another subdivision or none, or with no jurisdiction: only a person can say whether it is this company.
        Possible: LeiRecord list
        /// Records GLEIF states for the number past the most read, GleifCrossCheck.MostRead, so not read: any of them can be in the jurisdiction asked.
        Unread: int
    }

[<RequireQualifiedAccess>]
type LeiParentLevel =
    | Direct
    | Ultimate

/// What GLEIF holds of a firm's accounting parent at one level.
[<RequireQualifiedAccess>]
type LeiParent =
    | Parent of LeiRecord
    /// The firm reported an exception in place of a parent, with GLEIF's reason word, such as NO_KNOWN_PERSON.
    | ReportingException of reason: string
    /// Neither a parent nor an exception is on record.
    | NothingReported

/// GLEIF as it answered on 2026-11-06.
module internal Gleif =

    [<Literal>]
    let Source = "GLEIF (cross-check)"

    let private leiRegex = Regex @"\A[A-Z0-9]{20}\z"

    let isLei (text: string) =
        not (isNull text) && leiRegex.IsMatch text

    let private address (name: string) (e: JsonElement) : Address voption =
        tryObject name e
        |> ValueOption.map (fun block ->
            {
                Lines = strings block "addressLines"
                Locality = tryString "city" block
                Region = tryString "region" block
                PostalCode = tryString "postalCode" block
                Country = tryString "country" block
            })

    let record (data: JsonElement) : LeiRecord =
        let attributes =
            match tryObject "attributes" data with
            | ValueSome block -> block
            | ValueNone -> raise (InvalidJson "A record has no attributes")

        let entity =
            match tryObject "entity" attributes with
            | ValueSome block -> block
            | ValueNone -> raise (InvalidJson "A record has no entity")

        let registration = tryObject "registration" attributes

        let legalName =
            match tryObject "legalName" entity |> ValueOption.bind (tryString "name") with
            | ValueSome name -> name
            | ValueNone -> raise (InvalidJson "A record has no legal name")

        {
            Lei = required "lei" tryString attributes
            LegalName = legalName
            OtherNames =
                [ "otherNames"; "transliteratedOtherNames" ]
                |> List.collect (array entity)
                |> List.choose (fun name -> tryString "name" name |> ValueOption.toOption)
                |> List.distinct
            EntityStatus = tryString "status" entity
            LeiStatus = registration |> ValueOption.bind (tryString "status")
            Jurisdiction = tryString "jurisdiction" entity
            RegisteredAs = tryString "registeredAs" entity
            RegisteredAt = tryObject "registeredAt" entity |> ValueOption.bind (tryString "id")
            LegalAddress = address "legalAddress" entity
            HeadquartersAddress = address "headquartersAddress" entity
            CreatedOn = SourceAnswer.dateOfTimestamp "creationDate" entity
            LastUpdatedOn = registration |> ValueOption.bind (SourceAnswer.dateOfTimestamp "lastUpdateDate")
            Corroboration = registration |> ValueOption.bind (tryString "corroborationLevel")
        }

    let single (root: JsonElement) =
        match tryObject "data" root with
        | ValueSome data -> record data
        | ValueNone -> raise (InvalidJson "The answer has no data object")

    /// One page of a list: its records and the total it states.
    let page (root: JsonElement) =
        match tryProp "data" root with
        | ValueSome data when data.ValueKind = JsonValueKind.Array ->
            let records = data.EnumerateArray() |> List.ofSeq |> List.map record

            let total =
                tryObject "meta" root
                |> ValueOption.bind (tryObject "pagination")
                |> ValueOption.bind (tryInt "total")

            records, total
        | _ -> raise (InvalidJson "The answer has no data list")

    /// What reading a list does after a page, given the records read so far, that page's size and the total it states: ValueSome the records
    /// stated past mostRead when it is done, or ValueNone for the next page. A page cut short before the total is unreadable, as the list can't
    /// be read as all of them.
    let afterPage (pageSize: int) (mostRead: int) (read: int) (pageRecords: int) (total: int voption) =
        match total with
        | ValueNone -> ValueSome 0
        | ValueSome stated when read >= stated -> ValueSome 0
        | ValueSome _ when pageRecords < pageSize -> raise (InvalidJson "The answer holds fewer records than it states")
        | ValueSome stated when read >= mostRead -> ValueSome(stated - read)
        | ValueSome _ -> ValueNone

    /// Numbers are per register, and a subdivision can keep a register of its own, as a Canadian province or a US state does. So only the
    /// code asked is the same register; a subdivision's record for a country, or the reverse, is only possible.
    let private sameRegister (asked: Jurisdiction) (stated: Jurisdiction) = stated = asked

    /// A record stating another country is about something else, and is left out.
    let matchesFor (asked: Jurisdiction) (records: LeiRecord list) =
        let stated (record: LeiRecord) =
            record.Jurisdiction |> ValueOption.bind Jurisdiction.tryParse

        let matching, others =
            records
            |> List.partition (fun record ->
                match stated record with
                | ValueSome jurisdiction -> sameRegister asked jurisdiction
                | ValueNone -> false)

        {
            Matching = matching
            Possible =
                others
                |> List.filter (fun record ->
                    match stated record with
                    | ValueSome jurisdiction -> jurisdiction.Country = asked.Country
                    | ValueNone -> true)
            Unread = 0
        }

    let exceptionReason (root: JsonElement) =
        match tryObject "data" root |> ValueOption.bind (tryObject "attributes") with
        | ValueSome attributes -> required "reason" tryString attributes
        | ValueNone -> raise (InvalidJson "The answer has no data object")

    /// The data's date is the golden copy's publish date.
    let publishedOn (root: JsonElement) =
        tryObject "meta" root
        |> ValueOption.bind (tryObject "goldenCopy")
        |> ValueOption.bind (SourceAnswer.dateOfTimestamp "publishDate")

/// GLEIF's API. A cross-check, labelled as such in the case file: reported by the firm or its issuer and not a register's record. CC0, as GLEIF states.
[<Sealed>]
type GleifCrossCheck internal (client: SourceClient) =
    static let pageSize = 100

    /// 60 calls a minute: a figure that couldn't be confirmed in GLEIF's docs on 2026-11-06. Answers carry no rate limit header.
    new(connection: SourceConnection) =
        GleifCrossCheck(SourceClient(connection, SourceAnswer.pacing 60 (TimeSpan.FromMinutes 1.)))

    member private _.Read(reply: SourceReply, parse: JsonElement -> 'T) =
        SourceAnswer.json reply (fun root -> parse root, Gleif.publishedOn root)
        |> Result.map (fun (value, asOf) ->
            {
                Source = Gleif.Source
                RetrievedAt = reply.At
                AsOf = asOf
                Raw = ValueSome reply.Body
                Value = value
            })

    /// The record for an LEI. ValueNone when GLEIF holds none.
    member this.ByLeiAsync
        (lei: string, ct: CancellationToken)
        : Task<Result<Evidence<LeiRecord voption>, RegistryFailure>> =
        if not (Gleif.isLei lei) then
            Task.FromResult(Error(RegistryFailure.InvalidRequest "An LEI has twenty letters and digits"))
        else
            task {
                match! client.GetAsync(Uri $"https://api.gleif.org/api/v1/lei-records/{lei}", ct) with
                | Error failure -> return Error failure
                | Ok reply when reply.Status = 404 ->
                    return
                        Ok
                            {
                                Source = Gleif.Source
                                RetrievedAt = reply.At
                                AsOf = ValueNone
                                Raw = ValueNone
                                Value = ValueNone
                            }
                | Ok reply -> return this.Read(reply, (Gleif.single >> ValueSome))
            }

    /// The most records a number's list is read to, a page of 100 at a time: a number such as 1 is reported by thousands. A choice made
    /// without data.
    static member MostRead = 200

    /// Records whose reported number is this one, in the jurisdiction asked or possibly so, read up to MostRead; those past it are counted in
    /// Unread. None found says only that no LEI reports this number.
    member _.ByRegistrationAsync(number: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
        if String.IsNullOrWhiteSpace number then
            Task.FromResult(Error(RegistryFailure.InvalidRequest "A number is needed"))
        else
            let filter = Uri.EscapeDataString "filter[entity.registeredAs]"
            let size = Uri.EscapeDataString "page[size]"
            let numbered = Uri.EscapeDataString "page[number]"
            let value = Uri.EscapeDataString(number.Trim())

            let uri (page: int) =
                Uri $"https://api.gleif.org/api/v1/lei-records?{filter}={value}&{size}={pageSize}&{numbered}={page}"

            let finish (replies: SourceReply list) (records: LeiRecord list) (unread: int) =
                let first = List.head replies

                {
                    Source = Gleif.Source
                    RetrievedAt = (List.last replies).At
                    AsOf = SourceAnswer.json first Gleif.publishedOn |> Result.defaultValue ValueNone
                    Raw =
                        match replies with
                        | [ only ] -> ValueSome only.Body
                        | _ -> ValueSome("[" + (replies |> List.map _.Body |> String.concat ",") + "]")
                    Value =
                        { Gleif.matchesFor jurisdiction records with
                            Unread = unread
                        }
                }

            // Each page is asked after the last is read, by recursion: an await inside a loop throws in WebAssembly.
            let rec read
                (page: int)
                (replies: SourceReply list)
                (records: LeiRecord list)
                : Task<Result<Evidence<LeiMatches>, RegistryFailure>> =
                task {
                    match! client.GetAsync(uri page, ct) with
                    | Error failure -> return Error failure
                    | Ok reply when reply.Status = 404 -> return Error(RegistryFailure.RequestRefused "HTTP 404")
                    | Ok reply ->
                        let replies = replies @ [ reply ]

                        let next =
                            SourceAnswer.json reply (fun root ->
                                let found, total = Gleif.page root
                                let all = records @ found
                                all, Gleif.afterPage pageSize GleifCrossCheck.MostRead all.Length found.Length total)

                        match next with
                        | Error failure -> return Error failure
                        | Ok(all, ValueSome unread) -> return Ok(finish replies all unread)
                        | Ok(all, ValueNone) -> return! read (page + 1) replies all
                }

            read 1 [] []

    /// The firm's parent at this level, or the exception it reported. A failure on either call is an Error, never NothingReported.
    member this.ParentAsync(lei: string, level: LeiParentLevel, ct: CancellationToken) =
        if not (Gleif.isLei lei) then
            Task.FromResult(Error(RegistryFailure.InvalidRequest "An LEI has twenty letters and digits"))
        else
            task {
                let name =
                    match level with
                    | LeiParentLevel.Direct -> "direct"
                    | LeiParentLevel.Ultimate -> "ultimate"

                match! client.GetAsync(Uri $"https://api.gleif.org/api/v1/lei-records/{lei}/{name}-parent", ct) with
                | Error failure -> return Error failure
                | Ok reply when reply.Status <> 404 -> return this.Read(reply, (Gleif.single >> LeiParent.Parent))
                | Ok _ ->
                    match!
                        client.GetAsync(
                            Uri $"https://api.gleif.org/api/v1/lei-records/{lei}/{name}-parent-reporting-exception",
                            ct
                        )
                    with
                    | Error failure -> return Error failure
                    | Ok reply when reply.Status = 404 ->
                        return
                            Ok
                                {
                                    Source = Gleif.Source
                                    RetrievedAt = reply.At
                                    AsOf = ValueNone
                                    Raw = ValueNone
                                    Value = LeiParent.NothingReported
                                }
                    | Ok reply -> return this.Read(reply, (Gleif.exceptionReason >> LeiParent.ReportingException))
            }
