namespace CompanyDueDiligence

open System
open System.Text.Json
open System.Text.RegularExpressions
open System.Threading
open CompanyDueDiligence.Json

/// The Companies Registration Office's "Company Records" resource on its CKAN open data portal, read as it answered on 2026-11-08, with the
/// distinct status and type values it held that day.
module internal Ireland =

    let coverage: RegistryCoverage =
        {
            Registry = "CRO open data (Ireland)"
            Jurisdictions = [ Jurisdiction.ofCode "IE" ]
            Standing = Support.Partial "The status the daily snapshot carries"
            Concerns =
                Support.NotOffered
                    "The dataset states no overdue filings or disputes; a strike-off listing is in the standing"
            Addresses = Support.Partial "The registered address as four lines and an Eircode, where given"
            Search = Support.Partial "Up to 50 records whose name matches the words"
            Officers = Support.NotOffered "The dataset lists no officers"
            Owners = Support.NotOffered "The dataset lists no owners"
            Cost = AccessCost.Free
            RateLimit = RateLimitPolicy.NotPublished(2, TimeSpan.FromSeconds 1.)
            Licence = "CC BY 4.0, as the dataset's metadata states"
        }

    let site = "https://opendata.cro.ie"
    let resourceId = "3fef41bc-b8f4-4b10-8434-ce51c29b1bba"

    /// The snapshot holds dissolved companies as well as live ones, yet it is a daily copy of one resource, so silence isn't the register's word.
    [<Literal>]
    let notListed =
        "The daily snapshot holds no record with this number; it holds dissolved companies too, but a copy's silence isn't the register saying it holds none"

    let private numberRegex = Regex @"\A[0-9]{1,7}\z"

    /// Up to seven digits, without leading zeros, as the dataset holds the number.
    let normalNumber (number: string) =
        let normal = if isNull number then "" else number.Trim()

        if numberRegex.IsMatch normal then
            match normal.TrimStart '0' with
            | "" -> ValueNone
            | digits -> ValueSome digits
        else
            ValueNone

    /// Statuses that end a record other than by dissolution: striking off, a branch ceasing, conversion, deletion after a cross-border
    /// operation, and de-registration of a fund vehicle.
    let private closedWords =
        set
            [
                "Struck Off"
                "Ceased"
                "Ceased IRL"
                "Conv. to IndProvSoc"
                "Deleted CB Merger"
                "Deleted per SE merge"
                "Deleted as per SE Tr"
                "Deleted CB Conversion"
                "De-Registered ICAV"
                "De-Registered Investment Company"
            ]

    /// Every status the resource held on 2026-11-08, by its word with blanks trimmed. Anything else is Unknown. "Liquidation stayed" is a winding
    /// up the court has stayed (Companies Act 2014, s. 678): halted, not ended, so it is WindingUp and not active.
    let standing (status: string voption) =
        match status |> ValueOption.map _.Trim() with
        | ValueSome "Normal" -> CompanyStanding.Active
        | ValueSome "Strike Off Listed" -> CompanyStanding.ActiveWithConcern "Strike Off Listed"
        | ValueSome("Liquidation" | "Liquidation (UK)" | "Liquidation stayed" as word) -> CompanyStanding.WindingUp word
        | ValueSome("Examinership" | "Administration" | "Administration (UK)" as word) ->
            CompanyStanding.InInsolvency word
        | ValueSome("Dissolved" | "Dissolved-20 years" | "Dissolved PostMerger" | "Dissolved by Divis") ->
            CompanyStanding.Dissolved
        | ValueSome word when closedWords.Contains word -> CompanyStanding.Closed word
        | ValueSome "" -> CompanyStanding.Unknown ValueNone
        | other -> CompanyStanding.Unknown other

    /// Type 1128 is an external company and 1139 a foreign body corporate making a local offer; 1100 is the dataset's "Unknown".
    let role (typeCode: int voption) =
        match typeCode with
        | ValueSome(1128 | 1139) -> RecordRole.ForeignRegistration
        | ValueSome 1100
        | ValueNone -> RecordRole.NotStated
        | ValueSome _ -> RecordRole.Home

    let company (e: JsonElement) : Company =
        let status = SourceAnswer.trimmed "company_status" e

        let lines =
            [
                "company_address_1"
                "company_address_2"
                "company_address_3"
                "company_address_4"
            ]
            |> List.choose (fun field -> SourceAnswer.trimmed field e |> ValueOption.toOption)

        {
            Name = required "company_name" (fun name e -> SourceAnswer.trimmed name e) e
            OtherNames = []
            Number = required "company_num" OpenData.text e
            Jurisdiction = Jurisdiction.ofCode "IE"
            JurisdictionAsPublished = ValueNone
            Standing = standing status
            Concerns = CompanyConcerns.notPublished
            StatusAsPublished = status
            LegalForm = SourceAnswer.trimmed "company_type" e
            IncorporatedOn = SourceAnswer.dateOfTimestamp "company_reg_date" e
            CeasedOn = SourceAnswer.dateOfTimestamp "comp_dissolved_date" e
            Addresses =
                match lines with
                | [] -> NotAvailable "The record holds no address"
                | _ ->
                    Available
                        [
                            {
                                Kind = AddressKind.RegisteredOffice
                                Address =
                                    {
                                        Lines = lines
                                        Locality = ValueNone
                                        Region = ValueNone
                                        PostalCode = SourceAnswer.trimmed "eircode" e
                                        Country = ValueNone
                                    }
                            }
                        ]
            Role = role (tryInt "company_type_code" e)
            HomeRegistration = ValueNone
            EstablishmentOf = ValueNone
        }

    /// What the records for one number show, with the record's own JSON when there is one.
    let found (root: JsonElement) =
        let records, _ = OpenData.ckanRecords root

        let raw =
            match records with
            | [ only ] -> ValueSome(only.GetRawText())
            | _ -> ValueNone

        OpenData.foundIn (NotAvailable notListed) company records, raw

    /// CKAN's full-text query on the name field alone.
    let searchUri (name: string) (limit: int) =
        let query = JsonSerializer.Serialize(dict [ "company_name", name.Trim() ])

        Uri
            $"{site}/api/3/action/datastore_search?resource_id={Uri.EscapeDataString resourceId}&q={Uri.EscapeDataString query}&limit={limit}"

    let search (limit: int) (root: JsonElement) =
        let records, total = OpenData.ckanRecords root
        OpenData.matches limit total (records |> List.map company)

/// Ireland as an ICompanyRegistry. A number not in the snapshot is NotAvailable.
[<Sealed>]
type internal IrelandRegistry(connection: SourceConnection) =
    static let searchLimit = 50

    let client =
        SourceClient(connection, SourceAnswer.pacing 2 (TimeSpan.FromSeconds 1.))

    let coverage = Ireland.coverage

    interface ICompanyRegistry with
        member _.Coverage = coverage

        member _.TryFindAsync(number: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                match Ireland.normalNumber number with
                | ValueNone ->
                    Threading.Tasks.Task.FromResult(
                        Error(RegistryFailure.InvalidRequest "A CRO company number has up to seven digits")
                    )
                | ValueSome digits ->
                    task {
                        // Two records are enough to show the number isn't unique.
                        let uri =
                            OpenData.ckanWhere Ireland.site Ireland.resourceId [ "company_num", digits ] 2

                        match! client.GetAsync(uri, ct) with
                        | Error failure -> return Error failure
                        | Ok reply when reply.Status = 404 -> return Error(RegistryFailure.RequestRefused "HTTP 404")
                        | Ok reply ->
                            return
                                SourceAnswer.json reply Ireland.found
                                |> Result.map (fun (value, raw) ->
                                    let evidence = SourceAnswer.evidence coverage reply reply.LastModified value

                                    match raw with
                                    | ValueSome record -> { evidence with Raw = ValueSome record }
                                    | ValueNone -> evidence)
                    })

        member _.SearchAsync(name: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                if String.IsNullOrWhiteSpace name then
                    Threading.Tasks.Task.FromResult(Error(RegistryFailure.InvalidRequest "A search needs a term"))
                else
                    task {
                        match! client.GetAsync(Ireland.searchUri name searchLimit, ct) with
                        | Error failure -> return Error failure
                        | Ok reply when reply.Status = 404 -> return Error(RegistryFailure.RequestRefused "HTTP 404")
                        | Ok reply ->
                            return
                                SourceAnswer.json reply (Ireland.search searchLimit)
                                |> Result.map (fun found ->
                                    SourceAnswer.evidence coverage reply reply.LastModified (Available found))
                    })

        member _.OfficersAsync(_: string, jurisdiction: Jurisdiction, _: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                Threading.Tasks.Task.FromResult(
                    Ok(SourceAnswer.unasked coverage client.Now (NotAvailable "The dataset lists no officers"))
                ))

        member _.OwnersAsync(_: string, jurisdiction: Jurisdiction, _: OwnerPurpose, _: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                Threading.Tasks.Task.FromResult(
                    Ok(SourceAnswer.unasked coverage client.Now (NotAvailable "The dataset lists no owners"))
                ))
