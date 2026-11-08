namespace CompanyDueDiligence

open System
open System.Globalization
open System.Text.RegularExpressions
open System.Threading
open System.Threading.Tasks
open System.Xml
open System.Xml.Linq
open CompanyDueDiligence.Json

/// The National Tax Agency's Corporate Number Web-API, version 4, written from its specification (第六編, version 1.2, October 2021) and the resource
/// definitions of the overview (第二編, 別紙１ version 4.1), as read on 2026-11-08. It answers in XML, type 12. Not run live: the API needs an
/// application ID.
module internal Japan =

    /// The sentence the NTA's terms of use require a service built on the Web-API to show.
    [<Literal>]
    let attribution =
        "このサービスは、国税庁法人番号システムのWeb-API機能を利用して取得した情報をもとに作成しているが、サービスの内容は国税庁によって保証されたものではない"

    let coverage: RegistryCoverage =
        {
            Registry = "NTA Corporate Number Web-API (Japan)"
            Jurisdictions = [ Jurisdiction.ofCode "JP" ]
            Standing =
                Support.Partial
                    "Closure of the registration record and its cause; the NTA states nothing else of a company's standing"
            Concerns = Support.NotOffered "The NTA states no overdue filings, disputes or insolvency"
            Addresses = Support.Partial "The head office or principal office as the NTA publishes it"
            Search = Support.Partial "Names holding the term (mode 2, partial match), up to 2,000 in one answer"
            Officers = Support.NotOffered "The NTA publishes no officers"
            Owners = Support.NotOffered "The NTA publishes no owners"
            Cost =
                AccessCost.FreeWithKey
                    "Apply to the NTA for a Web-API application ID, which is free and takes two weeks to a month to issue"
            RateLimit = RateLimitPolicy.NotPublished(2, TimeSpan.FromSeconds 1.)
            Licence =
                $"The NTA's Web-API terms of use, which require showing this sentence: 「{attribution}」. Not run live"
        }

    let private numberRegex = Regex @"\A[0-9]{13}\z"

    /// The check digit, the first of the thirteen: 9 minus the sum of the other twelve, the n-th from the right weighted 1 when n is odd and 2
    /// when it is even, modulo 9.
    let checkDigitHolds (number: string) =
        let total =
            number.Substring 1
            |> Seq.rev
            |> Seq.mapi (fun index digit -> (int digit - int '0') * (if index % 2 = 0 then 1 else 2))
            |> Seq.sum

        9 - total % 9 = int number[0] - int '0'

    /// Thirteen digits with a valid check digit. Spaces and hyphens are taken out.
    let normalNumber (number: string) =
        let normal =
            if isNull number then
                ""
            else
                number.Trim().Replace(" ", "").Replace("-", "")

        if numberRegex.IsMatch normal && checkDigitHolds normal then
            ValueSome normal
        else
            ValueNone

    let private text (name: string) (e: XElement) =
        match e.Element(XName.Get name) with
        | null -> ValueNone
        | child ->
            match child.Value.Trim() with
            | "" -> ValueNone
            | value -> ValueSome value

    let private date name e =
        text name e
        |> ValueOption.bind (fun value ->
            match DateOnly.TryParseExact(value, "yyyy-MM-dd", CultureInfo.InvariantCulture, DateTimeStyles.None) with
            | true, parsed -> ValueSome parsed
            | _ -> ValueNone)

    /// The closeCause code list: 01 liquidation completed, 11 dissolved by merger, 21 closed by the registrar, 31 liquidation or the like completed
    /// for a body not formed by registration.
    let closed (cause: string voption) =
        match cause with
        | ValueSome "01" -> CompanyStanding.Dissolved
        | ValueSome "11" -> CompanyStanding.Closed "Dissolved by merger"
        | ValueSome "21" -> CompanyStanding.Closed "Record closed by the registrar"
        | ValueSome "31" -> CompanyStanding.Closed "Liquidation or the like completed"
        | ValueSome other -> CompanyStanding.Unknown(ValueSome $"closeCause {other}")
        | ValueNone -> CompanyStanding.Closed "Registration record closed"

    /// The process code list, all ten codes. A closure cause, or process 21, decides; a trade name struck off is closed; the rest leave the number
    /// designated with no status word. Process 99 withdraws the number and blanks the record, which the lookup answers before this.
    let standing (processCode: string voption) (closeCause: string voption) =
        match processCode, closeCause with
        | _, ValueSome _
        | ValueSome "21", _ -> closed closeCause
        | ValueSome "81", _ -> CompanyStanding.Closed "Trade name registration struck off"
        | ValueSome("01" | "11" | "12" | "13" | "22" | "71" | "72"), _ -> CompanyStanding.Registered ValueNone
        | ValueSome "99", _ -> CompanyStanding.Unknown(ValueSome "Number withdrawn")
        | ValueSome other, _ -> CompanyStanding.Unknown(ValueSome $"process {other}")
        | ValueNone, _ -> CompanyStanding.Unknown ValueNone

    /// The kind code list.
    let private kindWord (kind: string) =
        match kind with
        | "101" -> "National government body"
        | "201" -> "Local public body"
        | "301" -> "Kabushiki kaisha"
        | "302" -> "Yugen kaisha"
        | "303" -> "Gomei kaisha"
        | "304" -> "Goshi kaisha"
        | "305" -> "Godo kaisha"
        | "399" -> "Other body formed by registration"
        | "401" -> "Foreign company or the like"
        | "499" -> "Other"
        | other -> other

    let private role (kind: string voption) =
        match kind with
        | ValueSome "401" -> RecordRole.ForeignRegistration
        | ValueSome("301" | "302" | "303" | "304" | "305" | "399") -> RecordRole.Home
        | _ -> RecordRole.NotStated

    let company (e: XElement) : Company =
        let kind = text "kind" e
        let processCode = text "process" e
        let closeCause = text "closeCause" e

        let domestic =
            [ text "prefectureName" e; text "cityName" e; text "streetNumber" e ]
            |> List.choose ValueOption.toOption
            |> String.concat ""

        let lines =
            match domestic, text "addressOutside" e with
            | "", ValueSome outside -> [ outside ]
            | "", ValueNone -> []
            | inside, _ -> [ inside ]

        let formed = kind |> ValueOption.exists (fun code -> code.StartsWith "3")

        {
            Name =
                text "name" e
                |> ValueOption.defaultWith (fun () -> raise (InvalidJson "The record has no name"))
            OtherNames = [ text "enName" e; text "furigana" e ] |> List.choose ValueOption.toOption
            Number =
                text "corporateNumber" e
                |> ValueOption.defaultWith (fun () -> raise (InvalidJson "The record has no corporate number"))
            Jurisdiction = Jurisdiction.ofCode "JP"
            JurisdictionAsPublished = ValueNone
            Standing = standing processCode closeCause
            Concerns = CompanyConcerns.notPublished
            StatusAsPublished =
                match processCode, closeCause with
                | ValueSome code, ValueSome cause -> ValueSome $"process {code}; closeCause {cause}"
                | ValueSome code, ValueNone -> ValueSome $"process {code}"
                | ValueNone, ValueSome cause -> ValueSome $"closeCause {cause}"
                | ValueNone, ValueNone -> ValueNone
            LegalForm = kind |> ValueOption.map kindWord
            IncorporatedOn = ValueNone
            CeasedOn = date "closeDate" e
            Addresses =
                match lines with
                | [] -> NotAvailable "The record holds no address"
                | _ ->
                    Available
                        [
                            {
                                Kind =
                                    if formed then
                                        AddressKind.RegisteredOffice
                                    else
                                        AddressKind.PrincipalOffice
                                Address =
                                    {
                                        Lines = lines
                                        Locality = ValueNone
                                        Region = ValueNone
                                        PostalCode = text "postCode" e
                                        Country = if domestic = "" then ValueNone else ValueSome "Japan"
                                    }
                            }
                        ]
            Role = role kind
            HomeRegistration = ValueNone
            EstablishmentOf = ValueNone
        }

    /// Runs parse over the reply's XML. An answer that isn't XML or lacks what parse requires is UnreadableAnswer, quoting nothing from it.
    let xml (reply: SourceReply) (parse: XElement -> 'T) =
        try
            let document = XDocument.Parse(reply.Body)

            match document.Root with
            | null -> Error(RegistryFailure.UnreadableAnswer "The answer has no root element")
            | root when root.Name.LocalName <> "corporations" ->
                Error(RegistryFailure.UnreadableAnswer "The answer isn't a corporations document")
            | root -> Ok(parse root)
        with
        | :? XmlException as error -> Error(RegistryFailure.UnreadableAnswer $"Not XML, at line {error.LineNumber}")
        | InvalidJson reason -> Error(RegistryFailure.UnreadableAnswer reason)

    /// The data's date, from the header.
    let asOf (root: XElement) = date "lastUpdateDate" root

    let private corporations (root: XElement) =
        root.Elements(XName.Get "corporation") |> List.ofSeq

    /// The record for this number, with the data's date. An empty answer isn't a statement that the number isn't designated: an association
    /// that hasn't consented isn't published.
    let lookup (number: string) (root: XElement) =
        let asOf = asOf root

        match corporations root |> List.filter (fun e -> text "latest" e <> ValueSome "0") with
        | [] -> NotAvailable "The NTA published no record for this number, which proves nothing", asOf
        | [ only ] when text "process" only = ValueSome "99" ->
            NotAvailable "The NTA has withdrawn this number's designation or its publication", asOf
        | [ only ] ->
            let found = company only

            if found.Number = number then
                Available(ValueSome(found, only.ToString(SaveOptions.DisableFormatting))), asOf
            else
                raise (InvalidJson "The answer holds another corporation")
        | several -> NotAvailable $"The answer holds {several.Length} current records for this number", asOf

    let search (root: XElement) =
        let matches =
            corporations root
            |> List.filter (fun e -> text "process" e <> ValueSome "99")
            |> List.map (fun e ->
                let found = company e

                {
                    Name = found.Name
                    Number = found.Number
                    StatusAsPublished = found.StatusAsPublished
                    Address =
                        match found.Addresses with
                        | Available(first :: _) -> ValueSome first.Address
                        | _ -> ValueNone
                })

        let total =
            text "count" root
            |> ValueOption.bind (fun value ->
                match Int32.TryParse value with
                | true, n -> ValueSome n
                | _ -> ValueNone)

        {
            Matches = matches
            TotalMatches = total
            Complete = total = ValueSome matches.Length && text "divideSize" root = ValueSome "1"
        }

/// The Corporate Number Web-API as an ICompanyRegistry. The application ID goes in the query string, as the API requires; the answer never holds
/// it, and failures hold no text from the answer or the request. A 404 means the ID isn't registered or is invalid.
[<Sealed>]
type internal JapanRegistry(connection: SourceConnection, applicationId: SourceKey) =
    let client =
        SourceClient(connection, SourceAnswer.pacing 2 (TimeSpan.FromSeconds 1.))

    let coverage = Japan.coverage
    let baseUri = "https://api.houjin-bangou.nta.go.jp/4"

    let id () =
        Uri.EscapeDataString applicationId.Value

    let refused =
        RegistryFailure.AccessRefused
            "HTTP 404: the NTA answers so for an application ID that isn't registered or is invalid"

    interface ICompanyRegistry with
        member _.Coverage = coverage

        member _.TryFindAsync(number: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                match Japan.normalNumber number with
                | ValueNone ->
                    Task.FromResult(
                        Error(
                            RegistryFailure.InvalidRequest
                                "A corporate number has thirteen digits with a valid check digit"
                        )
                    )
                | ValueSome corporateNumber ->
                    task {
                        let uri = Uri $"{baseUri}/num?id={id ()}&number={corporateNumber}&type=12&history=0"

                        match! client.GetAsync(uri, ct) with
                        | Error failure -> return Error(SourceAnswer.hiding [ applicationId.Value ] failure)
                        | Ok reply when reply.Status = 404 -> return Error refused
                        | Ok reply ->
                            return
                                Japan.xml reply (Japan.lookup corporateNumber)
                                |> Result.map (fun (found, asOf) ->
                                    match found with
                                    | Available(ValueSome(company, raw)) ->
                                        { SourceAnswer.evidence coverage reply asOf (Available(ValueSome company)) with
                                            Raw = ValueSome raw
                                        }
                                    | Available ValueNone ->
                                        SourceAnswer.evidence coverage reply asOf (Available ValueNone)
                                    | NotAvailable reason ->
                                        SourceAnswer.evidence coverage reply asOf (NotAvailable reason)
                                    | NeedsEntitlement entitlement ->
                                        SourceAnswer.evidence coverage reply asOf (NeedsEntitlement entitlement))
                    })

        member _.SearchAsync(name: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                if String.IsNullOrWhiteSpace name then
                    Task.FromResult(Error(RegistryFailure.InvalidRequest "A search needs a term"))
                else
                    task {
                        let uri =
                            Uri $"{baseUri}/name?id={id ()}&name={Uri.EscapeDataString(name.Trim())}&type=12&mode=2"

                        match! client.GetAsync(uri, ct) with
                        | Error failure -> return Error(SourceAnswer.hiding [ applicationId.Value ] failure)
                        | Ok reply when reply.Status = 404 -> return Error refused
                        | Ok reply ->
                            return
                                Japan.xml reply (fun root -> Japan.search root, Japan.asOf root)
                                |> Result.map (fun (found, asOf) ->
                                    SourceAnswer.evidence coverage reply asOf (Available found))
                    })

        member _.OfficersAsync(_: string, jurisdiction: Jurisdiction, _: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                Task.FromResult(
                    Ok(SourceAnswer.unasked coverage client.Now (NotAvailable "The NTA publishes no officers"))
                ))

        member _.OwnersAsync(_: string, jurisdiction: Jurisdiction, _: OwnerPurpose, _: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                Task.FromResult(
                    Ok(SourceAnswer.unasked coverage client.Now (NotAvailable "The NTA publishes no owners"))
                ))
