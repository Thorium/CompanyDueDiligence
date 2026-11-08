namespace CompanyDueDiligence

open System
open System.Globalization
open System.Text.Json
open System.Text.RegularExpressions
open System.Threading
open CompanyDueDiligence.Json

/// The Israeli Companies Registrar's list of companies on data.gov.il (package ica_companies), read through CKAN's datastore as it answered on
/// 2026-11-08. Field names and values are Hebrew; the answers write a quotation mark inside a word as "~".
module internal Israel =

    [<Literal>]
    let ResourceId = "f004176c-b85f-4542-8901-7b3176f9a054"

    let coverage: RegistryCoverage =
        {
            Registry = "Israeli Companies Registrar on data.gov.il"
            Jurisdictions = [ Jurisdiction.ofCode "IL" ]
            Standing = Support.Supported
            Concerns =
                Support.Partial
                    "Court liquidation and receivership are stated by the status; overdue filings and disputes aren't published"
            Addresses = Support.Partial "The address the list holds, unlabelled"
            Search = Support.Partial "50 records matching CKAN's full-text search"
            Officers = Support.NotOffered "The list holds no officers"
            Owners = Support.NotOffered "The list holds no shareholders"
            Cost = AccessCost.Free
            RateLimit = RateLimitPolicy.NotPublished(2, TimeSpan.FromSeconds 1.)
            Licence = "Other (Open), as the package metadata states"
        }

    /// The list is a daily copy of companies, so a number missing from it may be a newer company or another kind of body.
    [<Literal>]
    let notListed =
        "The list is a daily copy holding Israeli companies only, so not finding this number proves nothing"

    [<Literal>]
    let NumberField = "מספר חברה"

    let private numberRegex = Regex @"\A[0-9]{9}\z"

    /// A company number: nine digits.
    let normalNumber (number: string) =
        let normal =
            if isNull number then
                ""
            else
                number.Trim().Replace("-", "").Replace(" ", "")

        if numberRegex.IsMatch normal then
            ValueSome normal
        else
            ValueNone

    /// The answer's "~" put back as the quotation mark it stands for.
    let private normalWord (word: string voption) =
        word
        |> ValueOption.map (fun word -> word.Trim().Replace('~', '"'))
        |> ValueOption.defaultValue ""

    [<Literal>]
    let private Violating =
        "Violating company: an annual fee or annual report is outstanding"

    /// Every status and sub-status seen in the list on 2026-11-08, and the statuses the package notes list. A pair not listed is Unknown.
    let standing (status: string voption) (subStatus: string voption) (violating: bool) =
        match normalWord status, normalWord subStatus with
        | "פעילה", "" when violating -> CompanyStanding.ActiveWithConcern Violating
        | "פעילה", "" -> CompanyStanding.Active
        | "פעילה", "בפרוק מרצון בהליך מזורז" -> CompanyStanding.WindingUp "Voluntary liquidation, expedited procedure"
        | "פעילה זמנית", "" -> CompanyStanding.ActiveWithConcern "Temporarily active"
        | "פעילה זמנית", "בקשת חברה בהחייאה" ->
            CompanyStanding.ActiveWithConcern "Temporarily active: the company has applied for revival"
        | "פעילה זמנית", "בקשת נושים בהחייאה" ->
            CompanyStanding.ActiveWithConcern "Temporarily active: creditors have applied for revival"
        | "פעילה/בפירוק - בכינוס נכסים", _ -> CompanyStanding.InInsolvency "Receivership"
        | "פעילה/בפירוק - הליך פשרה או הסדר/פירוק זמני", _ ->
            CompanyStanding.InInsolvency "Compromise or arrangement, or provisional liquidation"
        | "בפרוק מרצון", "" -> CompanyStanding.WindingUp "Voluntary liquidation"
        | "בפרוק ע\"י בימ\"ש", "" -> CompanyStanding.InInsolvency "Liquidation by the court"
        | "מחוקה", "" -> CompanyStanding.Closed "Struck off"
        | "מחוקה", "מחוסלת עקב מחיקה"
        | "מחוסלת מרצון", ""
        | "חיסול ע\"י בימ\"ש", "" -> CompanyStanding.Dissolved
        | "מחוסלת עקב מיזוג", "" -> CompanyStanding.Closed "Dissolved following a merger"
        | "נגרעה מהמרשם", "" -> CompanyStanding.Closed "Removed from the register"
        | "", "" -> CompanyStanding.Unknown ValueNone
        | word, "" -> CompanyStanding.Unknown(ValueSome word)
        | word, sub -> CompanyStanding.Unknown(ValueSome $"{word} / {sub}")

    /// Liquidation by the court, now or ended, and receivership.
    let private courtInsolvency (status: string voption) =
        match normalWord status with
        | "בפרוק ע\"י בימ\"ש"
        | "חיסול ע\"י בימ\"ש"
        | "פעילה/בפירוק - בכינוס נכסים"
        | "פעילה/בפירוק - הליך פשרה או הסדר/פירוק זמני" -> true
        | _ -> false

    let private dateOf name e =
        OpenData.text name e
        |> ValueOption.bind (fun text ->
            match DateOnly.TryParseExact(text, "dd/MM/yyyy", CultureInfo.InvariantCulture, DateTimeStyles.None) with
            | true, date -> ValueSome date
            | _ -> ValueNone)

    let private addressOf (e: JsonElement) =
        let street =
            [ OpenData.text "שם רחוב" e; OpenData.text "מספר בית" e ]
            |> List.choose ValueOption.toOption
            |> String.concat " "

        let lines =
            [
                OpenData.text "אצל" e |> ValueOption.map (fun careOf -> "אצל " + careOf)
                (if street = "" then ValueNone else ValueSome street)
                OpenData.text "ת.ד." e |> ValueOption.map (fun box -> "ת.ד. " + box)
            ]
            |> List.choose ValueOption.toOption

        let address =
            {
                Lines = lines
                Locality = OpenData.text "שם עיר" e
                Region = ValueNone
                PostalCode = OpenData.text "מיקוד" e
                Country = OpenData.text "מדינה" e
            }

        if List.isEmpty lines && address.Locality.IsNone && address.PostalCode.IsNone then
            NotAvailable "The record holds no address"
        else
            Available
                [
                    {
                        Kind = AddressKind.Other "Address on the Companies Registrar's list"
                        Address = address
                    }
                ]

    /// סוג תאגיד names an Israeli company (ישראלית) or a foreign one registered here (זרה); any other type doesn't say.
    let role (corporationType: string voption) =
        match corporationType with
        | ValueSome kind when kind.Contains "ישראלית" -> RecordRole.Home
        | ValueSome kind when kind.Contains "זרה" -> RecordRole.ForeignRegistration
        | _ -> RecordRole.NotStated

    let company (e: JsonElement) : Company =
        let status = OpenData.text "סטטוס חברה" e
        let subStatus = OpenData.text "תת סטטוס" e
        let violating = OpenData.text "מפרה" e |> ValueOption.isSome

        {
            Name =
                OpenData.text "שם חברה" e
                |> ValueOption.defaultWith (fun () -> raise (InvalidJson "The record has no company name"))
            OtherNames = OpenData.text "שם באנגלית" e |> ValueOption.toList
            Number =
                OpenData.text NumberField e
                |> ValueOption.defaultWith (fun () -> raise (InvalidJson "The record has no company number"))
            Jurisdiction = Jurisdiction.ofCode "IL"
            JurisdictionAsPublished = ValueNone
            Standing = standing status subStatus violating
            Concerns =
                if courtInsolvency status then
                    {
                        Stated = [ CompanyConcern.InsolvencyHistory ]
                        Unconfirmed = CompanyConcern.all |> List.except [ CompanyConcern.InsolvencyHistory ]
                    }
                else
                    CompanyConcerns.notPublished
            StatusAsPublished =
                match
                    [ status; subStatus; OpenData.text "מפרה" e ]
                    |> List.choose ValueOption.toOption
                with
                | [] -> ValueNone
                | words -> ValueSome(String.concat " / " words)
            LegalForm = OpenData.text "סוג תאגיד" e
            IncorporatedOn = dateOf "תאריך התאגדות" e
            CeasedOn = ValueNone
            Addresses = addressOf e
            Role = role (OpenData.text "סוג תאגיד" e)
            HomeRegistration = ValueNone
            EstablishmentOf = ValueNone
        }

    /// The record carrying this number. The filter is exact, so a record with another number fails the read.
    let lookup (number: string) (root: JsonElement) =
        let records, _ = OpenData.ckanRecords root

        match records with
        | [] -> NotAvailable notListed, ValueNone
        | [ only ] ->
            let found = company only

            if found.Number = number then
                Available(ValueSome found), ValueSome(only.GetRawText())
            else
                raise (InvalidJson "The answer holds another company")
        | several ->
            NotAvailable $"{several.Length} records carry this number, so the source can't tell which is meant",
            ValueNone

    let search (limit: int) (root: JsonElement) =
        let records, total = OpenData.ckanRecords root
        OpenData.matches limit total (records |> List.map company)

/// Israel as an ICompanyRegistry. A number not found is NotAvailable.
[<Sealed>]
type internal IsraelRegistry(connection: SourceConnection) =
    static let searchLimit = 50

    let client =
        SourceClient(connection, SourceAnswer.pacing 2 (TimeSpan.FromSeconds 1.))

    let coverage = Israel.coverage
    let site = "https://data.gov.il"

    let unasked reason =
        Threading.Tasks.Task.FromResult(Ok(SourceAnswer.unasked coverage client.Now (NotAvailable reason)))

    interface ICompanyRegistry with
        member _.Coverage = coverage

        member _.TryFindAsync(number: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                match Israel.normalNumber number with
                | ValueNone ->
                    Threading.Tasks.Task.FromResult(
                        Error(RegistryFailure.InvalidRequest "An Israeli company number has nine digits")
                    )
                | ValueSome number ->
                    task {
                        // Two records are enough to show the number isn't unique.
                        let uri = OpenData.ckanWhere site Israel.ResourceId [ Israel.NumberField, number ] 2

                        match! client.GetAsync(uri, ct) with
                        | Error failure -> return Error failure
                        | Ok reply when reply.Status = 404 -> return Error(RegistryFailure.RequestRefused "HTTP 404")
                        | Ok reply ->
                            return
                                SourceAnswer.json reply (Israel.lookup number)
                                |> Result.map (fun (found, raw) ->
                                    { SourceAnswer.evidence coverage reply ValueNone found with
                                        Raw = raw |> ValueOption.orElse (ValueSome reply.Body)
                                    })
                    })

        member _.SearchAsync(name: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                if String.IsNullOrWhiteSpace name then
                    Threading.Tasks.Task.FromResult(Error(RegistryFailure.InvalidRequest "A search needs a term"))
                else
                    task {
                        let uri = OpenData.ckanSearch site Israel.ResourceId name searchLimit

                        match! client.GetAsync(uri, ct) with
                        | Error failure -> return Error failure
                        | Ok reply when reply.Status = 404 -> return Error(RegistryFailure.RequestRefused "HTTP 404")
                        | Ok reply ->
                            return
                                SourceAnswer.json reply (Israel.search searchLimit)
                                |> Result.map (fun found ->
                                    SourceAnswer.evidence coverage reply ValueNone (Available found))
                    })

        member _.OfficersAsync(_: string, jurisdiction: Jurisdiction, _: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () -> unasked "The list holds no officers")

        member _.OwnersAsync(_: string, jurisdiction: Jurisdiction, _: OwnerPurpose, _: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () -> unasked "The list holds no shareholders")
