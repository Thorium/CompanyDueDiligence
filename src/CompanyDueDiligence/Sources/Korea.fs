namespace CompanyDueDiligence

open System
open System.Globalization
open System.Net.Http
open System.Text
open System.Text.Json
open System.Text.RegularExpressions
open System.Threading
open System.Threading.Tasks
open CompanyDueDiligence.Json

/// The National Tax Service's business status service on data.go.kr (dataset 15081808), written from its swagger document at
/// infuser.odcloud.kr/api/stages/28493/api-docs as read on 2026-11-08. Not run live: every call needs a service key. It holds tax status, not
/// a company register: it returns no name or address.
module internal Korea =

    let coverage: RegistryCoverage =
        {
            Registry = "NTS business status (data.go.kr, Korea)"
            Jurisdictions = [ Jurisdiction.ofCode "KR" ]
            Standing =
                Support.Partial
                    "The tax status, continuing, suspended or closed, which isn't a company register's standing"
            Concerns = Support.NotOffered "The service states no overdue filings, disputes or insolvency"
            Addresses = Support.NotOffered "The service returns no address"
            Search = Support.NotOffered "The service looks up business registration numbers only"
            Officers = Support.NotOffered "The service lists no officers"
            Owners = Support.NotOffered "The service lists no owners"
            Cost = AccessCost.FreeWithKey "A service key from data.go.kr, after applying for dataset 15081808"
            RateLimit =
                RateLimitPolicy.Published(
                    1_000_000,
                    TimeSpan.FromDays 1.,
                    "data.go.kr dataset 15081808: 1,000,000 a day and 100 numbers a call; paced at 10 a second, under it"
                )
            Licence = "No restriction on use (이용허락범위 제한 없음), as data.go.kr states. Not run live"
        }

    /// The tax_type the service gives a number that isn't registered with the NTS, or whose registration was deleted.
    [<Literal>]
    let NotRegistered = "국세청에 등록되지 않은 사업자등록번호입니다"

    let private numberRegex = Regex @"\A[0-9]{10}\z"

    /// The NTS check digit: weights 1, 3, 7, 1, 3, 7, 1, 3, 5 on the first nine digits, plus the tens of the ninth digit times 5, and the tenth
    /// digit is the sum's complement to ten.
    let hasCheckDigit (digits: string) =
        let values = digits |> Seq.map (fun c -> int c - int '0') |> List.ofSeq
        let ninth = values[8]

        let weighted =
            List.map2 (*) (List.take 9 values) [ 1; 3; 7; 1; 3; 7; 1; 3; 5 ] |> List.sum

        let sum = weighted + ninth * 5 / 10
        (10 - sum % 10) % 10 = values[9]

    /// Ten digits with a valid check digit. Hyphens and spaces, as the number is usually written, are taken out, as the service requires.
    let normalNumber (number: string) =
        let normal =
            if isNull number then
                ""
            else
                number.Replace("-", "").Replace(" ", "")

        if numberRegex.IsMatch normal && hasCheckDigit normal then
            ValueSome normal
        else
            ValueNone

    /// b_stt_cd as the swagger lists it: 01 continuing (계속사업자), 02 suspended (휴업자), 03 closed (폐업자). These are tax registration, not legal
    /// standing: continuing is Registered, and suspended, which doesn't say what became of the company, is Unknown. The word stays in
    /// StatusAsPublished.
    let standing (code: string voption) (word: string voption) =
        match code |> ValueOption.map _.Trim() with
        | ValueSome "01" -> CompanyStanding.Registered(ValueSome "계속사업자 (tax registration)")
        | ValueSome "02" -> CompanyStanding.Unknown(ValueSome "휴업자: business suspended (tax registration)")
        | ValueSome "03" -> CompanyStanding.Closed "폐업자: business closed"
        | ValueNone
        | ValueSome "" -> CompanyStanding.Unknown word
        | ValueSome other -> CompanyStanding.Unknown(ValueSome(word |> ValueOption.defaultValue other))

    let private compactDate name e =
        SourceAnswer.trimmed name e
        |> ValueOption.bind (fun text ->
            match DateOnly.TryParseExact(text, "yyyyMMdd", CultureInfo.InvariantCulture, DateTimeStyles.None) with
            | true, date -> ValueSome date
            | _ -> ValueNone)

    let company (e: JsonElement) : Company =
        let code = SourceAnswer.trimmed "b_stt_cd" e
        let word = SourceAnswer.trimmed "b_stt" e

        {
            Name = PartyNames.NotPublished
            OtherNames = []
            Number =
                SourceAnswer.trimmed "b_no" e
                |> ValueOption.defaultWith (fun () -> raise (InvalidJson "Missing or invalid field 'b_no'"))
            Jurisdiction = Jurisdiction.ofCode "KR"
            JurisdictionAsPublished = ValueNone
            Standing = standing code word
            Concerns = CompanyConcerns.notPublished
            StatusAsPublished = word
            LegalForm = ValueNone
            IncorporatedOn = ValueNone
            CeasedOn =
                if code = ValueSome "03" then
                    compactDate "end_dt" e
                else
                    ValueNone
            Addresses = NotAvailable "The service returns no address"
            Role = RecordRole.NotStated
            HomeRegistration = ValueNone
            EstablishmentOf = ValueNone
        }

    /// The result for this number, with its own JSON: ValueNone when the service says it isn't registered. status_code other than OK, or no
    /// result for the number, is unreadable.
    let lookup (number: string) (root: JsonElement) =
        match tryString "status_code" root with
        | ValueSome "OK" ->
            match
                array root "data"
                |> List.filter (fun item -> SourceAnswer.trimmed "b_no" item = ValueSome number)
            with
            | [ item ] ->
                if
                    SourceAnswer.trimmed "tax_type" item = ValueSome NotRegistered
                    && (SourceAnswer.trimmed "b_stt_cd" item).IsNone
                then
                    ValueNone
                else
                    ValueSome(company item, item.GetRawText())
            | [] -> raise (InvalidJson "The answer holds no result for the number")
            | _ -> raise (InvalidJson "The answer holds several results for one number")
        | _ -> raise (InvalidJson "The answer doesn't say it succeeded")

/// Korea as an ICompanyRegistry. The service key travels in the query, as the service requires, so it is taken out of every failure.
[<Sealed>]
type internal KoreaRegistry(connection: SourceConnection, key: SourceKey) =
    let client =
        SourceClient(connection, SourceAnswer.pacing 10 (TimeSpan.FromSeconds 1.))

    let coverage = Korea.coverage
    let hide = SourceAnswer.hiding [ key.Value ]

    interface ICompanyRegistry with
        member _.Coverage = coverage

        member _.TryFindAsync(number: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                match Korea.normalNumber number with
                | ValueNone ->
                    Task.FromResult(
                        Error(
                            RegistryFailure.InvalidRequest
                                "A business registration number has ten digits with a valid check digit"
                        )
                    )
                | ValueSome normal ->
                    task {
                        // The key is the decoded one data.go.kr issues, escaped here once.
                        let uri =
                            Uri
                                $"https://api.odcloud.kr/api/nts-businessman/v1/status?serviceKey={Uri.EscapeDataString key.Value}&returnType=JSON"

                        let body () : HttpContent =
                            new StringContent(
                                JsonSerializer.Serialize {| b_no = [| normal |] |},
                                Encoding.UTF8,
                                "application/json"
                            )

                        match! client.PostAsync(uri, body, [ "Accept", "application/json" ], ct) with
                        | Error failure -> return Error(hide failure)
                        | Ok reply when reply.Status = 404 -> return Error(RegistryFailure.RequestRefused "HTTP 404")
                        | Ok reply ->
                            return
                                SourceAnswer.json reply (Korea.lookup normal)
                                |> Result.map (fun found ->
                                    match found with
                                    | ValueNone ->
                                        SourceAnswer.evidence coverage reply ValueNone (Available ValueNone)
                                    | ValueSome(company, raw) ->
                                        { SourceAnswer.evidence
                                              coverage
                                              reply
                                              ValueNone
                                              (Available(ValueSome company)) with
                                            Raw = ValueSome raw
                                        })
                    })

        member _.SearchAsync(_: string, jurisdiction: Jurisdiction, _: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                Task.FromResult(
                    Ok(
                        SourceAnswer.unasked
                            coverage
                            client.Now
                            (NotAvailable "The service looks up business registration numbers only")
                    )
                ))

        member _.OfficersAsync(_: string, jurisdiction: Jurisdiction, _: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                Task.FromResult(
                    Ok(SourceAnswer.unasked coverage client.Now (NotAvailable "The service lists no officers"))
                ))

        member _.OwnersAsync(_: string, jurisdiction: Jurisdiction, _: OwnerPurpose, _: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                Task.FromResult(
                    Ok(SourceAnswer.unasked coverage client.Now (NotAvailable "The service lists no owners"))
                ))
