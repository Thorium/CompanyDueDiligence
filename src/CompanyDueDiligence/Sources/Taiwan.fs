namespace CompanyDueDiligence

open System
open System.Text.Json
open System.Text.RegularExpressions
open System.Threading
open CompanyDueDiligence.Json

/// The MOEA commerce open data platform (data.gcis.nat.gov.tw), read as it answered on 2026-11-08, with its swagger.json and code list
/// datacode.odt. Company basics come from 公司登記基本資料-應用一, directors and supervisors from 公司登記董監事資料集, and search from
/// 公司登記關鍵字查詢. The platform's rules ask API users to email a usage notice and have their IP registered; it answered without.
module internal Taiwan =

    let coverage: RegistryCoverage =
        {
            Registry = "MOEA company registration open data (Taiwan)"
            Jurisdictions = [ Jurisdiction.ofCode "TW" ]
            Standing = Support.Supported
            Concerns =
                Support.Partial
                    "Reorganisation, bankruptcy and receivership are stated by the status; overdue filings and disputes aren't published"
            Addresses = Support.Partial "The company location (公司所在地) only"
            Search =
                Support.Partial
                    "Names containing the term, up to 50 for each of status 01 核准設立 and the ended statuses 04, 05, 07, 08, 09, 10, 11 and 13; other statuses aren't asked, so it is never complete"
            Officers =
                Support.Partial
                    "Current directors and supervisors with juristic persons represented; no appointment dates. A full page of 50 rows is NotAvailable"
            Owners = Support.NotOffered "The API gives directors' shareholdings, not owners"
            Cost = AccessCost.Free
            RateLimit = RateLimitPolicy.NotPublished(2, TimeSpan.FromSeconds 1.)
            Licence =
                "Open Government Data License v1, per the platform's open data declaration; not re-read. Free; the platform's rules ask for a usage notice and IP registration"
        }

    /// The API holds companies only, so a UBN it doesn't hold can be a business (商業), a branch or another body.
    [<Literal>]
    let notListed =
        "The API holds companies only; a UBN it doesn't hold can be a business registration, a branch or another body, so not finding it proves nothing"

    let private ubnRegex = Regex @"\A[0-9]{8}\z"

    /// The Ministry of Finance's check: weights 1,2,1,2,1,2,4,1, each product's digits summed, the total divisible by 5 (by 10 before 2023,
    /// which 5 includes). A seventh digit of 7 gives 28, whose digit sum may be read as 1 or 0.
    let isUbn (ubn: string) =
        ubnRegex.IsMatch ubn
        && (let weights = [| 1; 2; 1; 2; 1; 2; 4; 1 |]

            let total =
                Seq.zip ubn weights
                |> Seq.sumBy (fun (digit, weight) ->
                    let product = (int digit - int '0') * weight
                    product / 10 + product % 10)

            total % 5 = 0 || (ubn[6] = '7' && (total + 1) % 5 = 0))

    /// Eight digits passing the check. Spaces are taken out.
    let normalUbn (number: string) =
        let normal = if isNull number then "" else number.Replace(" ", "")
        if isUbn normal then ValueSome normal else ValueNone

    /// A Republic of China date, yyyMMdd with the year counted from 1912. Blank is absent; anything else that isn't a date is refused.
    let rocDate name (e: JsonElement) =
        match SourceAnswer.trimmed name e with
        | ValueNone -> ValueNone
        | ValueSome text ->
            let parsed =
                if text.Length = 7 then
                    match
                        Int32.TryParse(text.AsSpan(0, 3)),
                        Int32.TryParse(text.AsSpan(3, 2)),
                        Int32.TryParse(text.AsSpan(5, 2))
                    with
                    | (true, year), (true, month), (true, day) when
                        month >= 1
                        && month <= 12
                        && day >= 1
                        && day <= DateTime.DaysInMonth(year + 1911, month)
                        ->
                        ValueSome(DateOnly(year + 1911, month, day))
                    | _ -> ValueNone
                else
                    ValueNone

            match parsed with
            | ValueSome date -> ValueSome date
            | ValueNone -> raise (InvalidJson $"Field '{name}' is not a date")

    /// Table 7 of datacode.odt, 現況代碼, by the word the API gives, and 廢止登記, which the API gives for code 10. A word not in it is Unknown.
    let statusStanding (word: string voption) =
        match word with
        | ValueSome "核准設立" -> CompanyStanding.Active
        | ValueSome "核准設立，但已命令解散" -> CompanyStanding.WindingUp "核准設立，但已命令解散: ordered to dissolve"
        | ValueSome "重整" -> CompanyStanding.InInsolvency "重整: reorganisation"
        | ValueSome "解散" -> CompanyStanding.WindingUp "解散: dissolved, liquidation not recorded as complete"
        | ValueSome "撤銷" -> CompanyStanding.Closed "撤銷: registration revoked"
        | ValueSome "破產" -> CompanyStanding.InInsolvency "破產: bankrupt"
        | ValueSome "合併解散" -> CompanyStanding.Closed "合併解散: dissolved by merger"
        | ValueSome "撤回登記" -> CompanyStanding.Closed "撤回登記: registration withdrawn"
        | ValueSome "廢止" -> CompanyStanding.Closed "廢止: registration annulled"
        | ValueSome "廢止登記" -> CompanyStanding.Closed "廢止登記: registration annulled"
        | ValueSome "廢止認許" -> CompanyStanding.Closed "廢止認許: recognition annulled"
        | ValueSome "解散已清算完結" -> CompanyStanding.Dissolved
        | ValueSome "撤銷已清算完結" -> CompanyStanding.Closed "撤銷已清算完結: revoked, liquidation complete"
        | ValueSome "廢止已清算完結" -> CompanyStanding.Closed "廢止已清算完結: annulled, liquidation complete"
        | ValueSome "撤回登記已清算完結" -> CompanyStanding.Closed "撤回登記已清算完結: withdrawn, liquidation complete"
        | ValueSome "撤銷登記已清算完結" -> CompanyStanding.Closed "撤銷登記已清算完結: registration revoked, liquidation complete"
        | ValueSome "廢止登記已清算完結" -> CompanyStanding.Closed "廢止登記已清算完結: registration annulled, liquidation complete"
        | ValueSome "撤銷登記" -> CompanyStanding.Closed "撤銷登記: registration revoked"
        | ValueSome "分割解散" -> CompanyStanding.Closed "分割解散: dissolved by division"
        | ValueSome "終止破產" -> CompanyStanding.InInsolvency "終止破產: bankruptcy terminated"
        | ValueSome "中止破產" -> CompanyStanding.InInsolvency "中止破產: bankruptcy suspended"
        | ValueSome "塗銷破產" ->
            CompanyStanding.Unknown(ValueSome "塗銷破產: bankruptcy record cancelled, standing not stated")
        | ValueSome "破產程序終結(終止)" -> CompanyStanding.InInsolvency "破產程序終結(終止): bankruptcy proceedings ended"
        | ValueSome "破產程序終結(終止)清算中" -> CompanyStanding.InInsolvency "破產程序終結(終止)清算中: liquidating after bankruptcy"
        | ValueSome "破產已清算完結" -> CompanyStanding.Closed "破產已清算完結: bankrupt, liquidation complete"
        | ValueSome "接管" -> CompanyStanding.InInsolvency "接管: receivership"
        | ValueSome "撤銷無需清算" -> CompanyStanding.Closed "撤銷無需清算: revoked, no liquidation needed"
        | ValueSome "撤銷許可" -> CompanyStanding.Closed "撤銷許可: permission revoked"
        | ValueSome "廢止許可" -> CompanyStanding.Closed "廢止許可: permission annulled"
        | ValueSome "撤銷許可已清算完結" -> CompanyStanding.Closed "撤銷許可已清算完結: permission revoked, liquidation complete"
        | ValueSome "廢止許可已清算完結" -> CompanyStanding.Closed "廢止許可已清算完結: permission annulled, liquidation complete"
        | ValueSome "清理" -> CompanyStanding.WindingUp "清理: winding up"
        | ValueSome "撤銷公司設立" -> CompanyStanding.Closed "撤銷公司設立: incorporation revoked"
        | ValueSome "清理完結" -> CompanyStanding.Closed "清理完結: winding up complete"
        | other -> CompanyStanding.Unknown other

    /// Statuses that state an insolvency proceeding, past or present.
    let private insolvencyWords =
        set
            [
                "重整"
                "破產"
                "終止破產"
                "中止破產"
                "塗銷破產"
                "破產程序終結(終止)"
                "破產程序終結(終止)清算中"
                "破產已清算完結"
                "接管"
            ]

    /// Table 8 of datacode.odt, 停業與延展狀態, read from Case_Status_Desc or its code. Unverified: no suspended company has been read live.
    let suspensionWord (description: string voption) (code: string voption) =
        match description, code with
        | ValueSome word, _ -> ValueSome word
        | ValueNone, ValueSome "1" -> ValueSome "停業"
        | ValueNone, ValueSome "2" -> ValueSome "延展開業"
        | ValueNone, ValueSome "3" -> ValueSome "復業"
        | ValueNone, ValueSome other -> ValueSome other
        | ValueNone, ValueNone -> ValueNone

    /// The status decides; a live company's suspension or postponed opening qualifies it, and resumption leaves it active.
    let standing
        (status: string voption)
        (suspension: string voption)
        (from: DateOnly voption)
        (until: DateOnly voption)
        =
        match statusStanding status, suspension with
        | CompanyStanding.Active, ValueNone
        | CompanyStanding.Active, ValueSome "復業" -> CompanyStanding.Active
        | CompanyStanding.Active, ValueSome "停業" ->
            let day (date: DateOnly) =
                date.ToString("yyyy-MM-dd", Globalization.CultureInfo.InvariantCulture)

            let period =
                match from, until with
                | ValueSome start, ValueSome finish -> $" from {day start} to {day finish}"
                | ValueSome start, ValueNone -> $" from {day start}"
                | _ -> ""

            CompanyStanding.ActiveWithConcern $"停業: business suspended{period}"
        | CompanyStanding.Active, ValueSome "延展開業" ->
            CompanyStanding.ActiveWithConcern "延展開業: start of business postponed"
        | CompanyStanding.Active, ValueSome other -> CompanyStanding.Unknown(ValueSome $"核准設立; {other}")
        | placed, _ -> placed

    let company (e: JsonElement) : Company =
        let status = SourceAnswer.trimmed "Company_Status_Desc" e

        let suspension =
            suspensionWord (SourceAnswer.trimmed "Case_Status_Desc" e) (OpenData.text "Case_Status" e)

        {
            Name = required "Company_Name" SourceAnswer.trimmed e
            OtherNames = []
            Number = required "Business_Accounting_NO" OpenData.text e
            Jurisdiction = Jurisdiction.ofCode "TW"
            JurisdictionAsPublished = ValueNone
            Standing = standing status suspension (rocDate "Sus_Beg_Date" e) (rocDate "Sus_End_Date" e)
            Concerns =
                match status with
                | ValueSome word when insolvencyWords.Contains word ->
                    {
                        Stated = [ CompanyConcern.InsolvencyHistory ]
                        Unconfirmed = CompanyConcern.all |> List.except [ CompanyConcern.InsolvencyHistory ]
                    }
                | _ -> CompanyConcerns.notPublished
            StatusAsPublished = status
            LegalForm = ValueNone
            IncorporatedOn = rocDate "Company_Setup_Date" e
            CeasedOn = rocDate "Revoke_App_Date" e
            Addresses =
                match SourceAnswer.trimmed "Company_Location" e with
                | ValueNone -> NotAvailable "The record holds no company location"
                | ValueSome location ->
                    Available
                        [
                            {
                                Kind = AddressKind.RegisteredOffice
                                Address =
                                    {
                                        Lines = [ location ]
                                        Locality = ValueNone
                                        Region = ValueNone
                                        PostalCode = ValueNone
                                        Country = ValueNone
                                    }
                            }
                        ]
            // 應用一 carries no organisation type, so a foreign company's registration can't be told from a home one.
            Role = RecordRole.NotStated
            HomeRegistration = ValueNone
            EstablishmentOf = ValueNone
        }

    /// The record for this UBN. Another company's record fails the read rather than being taken for it.
    let lookup (ubn: string) (records: JsonElement list) =
        match records with
        | [] -> NotAvailable notListed, ValueNone
        | [ only ] ->
            let found = company only

            if found.Number = ubn then
                Available(ValueSome found), ValueSome(only.GetRawText())
            else
                raise (InvalidJson "The answer holds another company")
        | several ->
            NotAvailable $"{several.Length} records carry this number, so the API can't tell which is meant", ValueNone

    let private directorPositions = set [ "董事長"; "副董事長"; "常務董事"; "董事"; "獨立董事" ]

    let private role (position: string voption) =
        match position with
        | ValueSome word when directorPositions.Contains word -> OfficerRole.Director
        | ValueSome word -> OfficerRole.Other word
        | ValueNone -> OfficerRole.Other ""

    let private officerOf name kind officerRole entity : Officer =
        {
            Name = name
            Kind = kind
            Role = officerRole
            Current = true
            AppointedOn = ValueNone
            EndedOn = ValueNone
            BornIn = ValueNone
            Nationality = ValueNone
            CountryOfResidence = ValueNone
            Address = ValueNone
            Entity = entity
            IdentityVerification = ValueNone
        }

    /// A juristic person holding a seat is the officer, a legal entity, and the person representing it is listed too, under a role naming it.
    let officers (e: JsonElement) : Officer list =
        let position = SourceAnswer.trimmed "Person_Position_Name" e
        let person = SourceAnswer.trimmed "Person_Name" e

        match SourceAnswer.trimmed "Juristic_Person_Name" e with
        | ValueSome juristic ->
            let entity =
                officerOf
                    juristic
                    PartyKind.LegalEntity
                    (role position)
                    (ValueSome
                        {
                            LegalName = ValueSome juristic
                            LegalForm = ValueNone
                            GoverningLaw = ValueNone
                            Country = ValueNone
                            Register = ValueNone
                            RegistrationNumber = ValueNone
                        })

            match person with
            | ValueSome name ->
                let seat = position |> ValueOption.defaultValue "seat"

                [
                    entity
                    officerOf
                        name
                        PartyKind.Individual
                        (OfficerRole.Other $"Representative of {juristic} as {seat}")
                        ValueNone
                ]
            | ValueNone -> [ entity ]
        | ValueNone ->
            match person with
            | ValueSome name -> [ officerOf name PartyKind.Individual (role position) ValueNone ]
            | ValueNone ->
                [
                    officerOf PartyNames.NotGiven (PartyKind.Unknown ValueNone) (role position) ValueNone
                ]

    /// The most rows one directors' request asks for.
    [<Literal>]
    let OfficerPage = 50

    /// A full page of rows may have more behind it, so it is NotAvailable rather than a listing that could be cut short.
    let officerList (records: JsonElement list) =
        match records with
        | [] ->
            NotAvailable "The API lists no directors or supervisors for this number, which doesn't say there are none"
        | records when records.Length >= OfficerPage ->
            NotAvailable
                $"The API answered a full page of {OfficerPage} directors and supervisors, so the list may be cut short"
        | records -> Available(records |> List.collect officers)

    let matchOf (e: JsonElement) : CompanyMatch =
        {
            Name = required "Company_Name" SourceAnswer.trimmed e
            Number = required "Business_Accounting_NO" OpenData.text e
            StatusAsPublished = SourceAnswer.trimmed "Company_Status_Desc" e
            Address =
                SourceAnswer.trimmed "Company_Location" e
                |> ValueOption.map (fun location ->
                    {
                        Lines = [ location ]
                        Locality = ValueNone
                        Region = ValueNone
                        PostalCode = ValueNone
                        Country = ValueNone
                    })
        }

    /// Status codes a search asks for, one call each, as the filter takes one Company_Status: 01 核准設立, then the ended 04 解散, 05 撤銷,
    /// 07 合併解散, 08 撤回登記, 09 廢止, 10 廢止登記, 11 解散已清算完結 and 13 廢止已清算完結.
    let searchStatuses = [ "01"; "04"; "05"; "07"; "08"; "09"; "10"; "11"; "13" ]

    /// Each status's matches in the order asked, a UBN met twice taken once. Never complete, as other statuses aren't asked.
    let searchResult (lists: CompanyMatch list list) : SearchResult =
        {
            Matches = lists |> List.concat |> List.distinctBy _.Number
            TotalMatches = ValueNone
            Complete = false
        }

    /// The letters and digits of a search term; anything else would change the filter's meaning.
    let searchTerm (name: string) =
        if isNull name then
            ""
        else
            String(name.Trim().ToCharArray() |> Array.filter Char.IsLetterOrDigit)

/// Taiwan as an ICompanyRegistry. A UBN not found is NotAvailable: the API holds companies only. A search asks once per status in
/// Taiwan.searchStatuses, the calls together under the source's pacing.
[<Sealed>]
type internal TaiwanRegistry(connection: SourceConnection) =
    static let searchLimit = 50

    let client =
        SourceClient(connection, SourceAnswer.pacing 2 (TimeSpan.FromSeconds 1.))

    let coverage = Taiwan.coverage
    let site = "https://data.gcis.nat.gov.tw/od/data/api"

    let byUbn (dataset: string) (ubn: string) =
        let filter = Uri.EscapeDataString $"Business_Accounting_NO eq {ubn}"
        Uri $"{site}/{dataset}?$format=json&$filter={filter}&$skip=0&$top={Taiwan.OfficerPage}"

    /// The rows of an answer. The API answers no rows with an empty body, or an empty list.
    let read (reply: SourceReply) (parse: JsonElement list -> 'T) =
        if String.IsNullOrWhiteSpace reply.Body then
            Ok(parse [])
        else
            SourceAnswer.json reply (OpenData.socrataRows >> parse)

    let offUbn number send =
        match Taiwan.normalUbn number with
        | ValueSome ubn -> send ubn
        | ValueNone ->
            Threading.Tasks.Task.FromResult(
                Error(RegistryFailure.InvalidRequest "A UBN has eight digits passing the Ministry of Finance's check")
            )

    interface ICompanyRegistry with
        member _.Coverage = coverage

        member _.TryFindAsync(number: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                offUbn number (fun ubn ->
                    task {
                        match! client.GetAsync(byUbn "5F64D864-61CB-4D0D-8AD9-492047CC1EA6" ubn, ct) with
                        | Error failure -> return Error failure
                        | Ok reply when reply.Status = 404 -> return Error(RegistryFailure.RequestRefused "HTTP 404")
                        | Ok reply ->
                            return
                                read reply (Taiwan.lookup ubn)
                                |> Result.map (fun (found, raw) ->
                                    { SourceAnswer.evidence coverage reply ValueNone found with
                                        Raw = raw |> ValueOption.orElse (ValueSome reply.Body)
                                    })
                    }))

        member _.SearchAsync(name: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                match Taiwan.searchTerm name with
                | "" ->
                    Threading.Tasks.Task.FromResult(
                        Error(RegistryFailure.InvalidRequest "A search needs a term of letters or digits")
                    )
                | term ->
                    task {
                        let uri status =
                            let filter =
                                Uri.EscapeDataString $"Company_Name like {term} and Company_Status eq {status}"

                            Uri
                                $"{site}/6BBA2268-1367-4B42-9CCA-BC17499EBE8C?$format=json&$filter={filter}&$skip=0&$top={searchLimit}"

                        // Started together and awaited once: an await inside a loop throws in WebAssembly.
                        let! answers =
                            Taiwan.searchStatuses
                            |> List.map (fun status -> client.GetAsync(uri status, ct))
                            |> Threading.Tasks.Task.WhenAll

                        let replies =
                            answers
                            |> List.ofArray
                            |> List.fold
                                (fun earlier answer ->
                                    match earlier, answer with
                                    | Error failure, _ -> Error failure
                                    | Ok _, Error failure -> Error failure
                                    | Ok _, Ok reply when reply.Status = 404 ->
                                        Error(RegistryFailure.RequestRefused "HTTP 404")
                                    | Ok replies, Ok reply -> Ok(replies @ [ reply ]))
                                (Ok [])

                        let lists (replies: SourceReply list) =
                            List.foldBack
                                (fun reply later ->
                                    match read reply (List.map Taiwan.matchOf), later with
                                    | Ok matches, Ok all -> Ok(matches :: all)
                                    | Error failure, _
                                    | _, Error failure -> Error failure)
                                replies
                                (Ok [])

                        return
                            replies
                            |> Result.bind (fun replies ->
                                lists replies
                                |> Result.map (fun found ->
                                    let bodies =
                                        replies
                                        |> List.map (fun reply ->
                                            if String.IsNullOrWhiteSpace reply.Body then
                                                "[]"
                                            else
                                                reply.Body)

                                    { SourceAnswer.evidence
                                          coverage
                                          (List.last replies)
                                          ValueNone
                                          (Available(Taiwan.searchResult found)) with
                                        Raw = ValueSome("[" + String.concat "," bodies + "]")
                                    }))
                    })

        member _.OfficersAsync(number: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                offUbn number (fun ubn ->
                    task {
                        match! client.GetAsync(byUbn "4E5F7653-1B91-4DDC-99D5-468530FAE396" ubn, ct) with
                        | Error failure -> return Error failure
                        | Ok reply when reply.Status = 404 -> return Error(RegistryFailure.RequestRefused "HTTP 404")
                        | Ok reply ->
                            return
                                read reply Taiwan.officerList
                                |> Result.map (SourceAnswer.evidence coverage reply ValueNone)
                    }))

        member _.OwnersAsync(_: string, jurisdiction: Jurisdiction, _: OwnerPurpose, _: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                Threading.Tasks.Task.FromResult(
                    Ok(
                        SourceAnswer.unasked
                            coverage
                            client.Now
                            (NotAvailable "The API gives directors' shareholdings, not owners")
                    )
                ))
