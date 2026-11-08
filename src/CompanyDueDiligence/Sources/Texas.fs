namespace CompanyDueDiligence

open System
open System.Text.Json
open System.Text.RegularExpressions
open System.Threading
open CompanyDueDiligence.Json

/// The Texas Comptroller's "Active Franchise Taxpayers" dataset, read with its own layout attachment on 2026-11-06. Despite the name it holds
/// forfeited and ended records, so a row is no statement of good standing, and it isn't the Secretary of State's register.
module internal Texas =

    let coverage: RegistryCoverage =
        {
            Registry = "Texas Comptroller franchise taxpayers"
            Jurisdictions = [ Jurisdiction.ofCode "US-TX" ]
            Standing =
                Support.Partial "The Secretary of State status and right to transact as this tax file copies them"
            Concerns = Support.NotOffered "The dataset states no overdue filings, disputes or insolvency"
            Addresses = Support.Partial "The taxpayer's address, not a registered office"
            Search =
                Support.Partial
                    "50 names starting with the term, never complete: the dataset holds almost no dissolved companies"
            Officers = Support.NotOffered "The dataset lists no officers"
            Owners = Support.NotOffered "The dataset lists no owners"
            Cost = AccessCost.Free
            RateLimit = RateLimitPolicy.NotPublished(2, TimeSpan.FromSeconds 1.)
            Licence = "No licence stated in the dataset's metadata"
        }

    /// The dataset holds taxpayers the Comptroller has set up, so a number missing from it can be any of many things.
    [<Literal>]
    let notListed =
        "The dataset lists franchise taxpayers set up with the Comptroller, so not finding this number proves nothing"

    let private fileNumberRegex = Regex @"\A[0-9]{1,10}\z"
    let private taxpayerRegex = Regex @"\A[0-9]{11}\z"

    /// The field to look in and the value: eleven digits is a taxpayer number, and up to ten a Secretary of State file number, with its leading zeros back.
    let lookupOf (number: string) =
        let normal = if isNull number then "" else number.Trim()

        if taxpayerRegex.IsMatch normal then
            ValueSome("taxpayer_number", normal)
        elif fileNumberRegex.IsMatch normal then
            ValueSome("secretary_of_state_sos_or_coa_file_number", normal.PadLeft(10, '0'))
        else
            ValueNone

    /// Codes as the layout spells them. Answers hold them with stray spaces and line ends, which are trimmed first.
    let sosStatusWord (code: string) =
        match code with
        | "A" -> ValueSome "Active"
        | "B" -> ValueSome "Consolidated"
        | "C" -> ValueSome "Converted"
        | "D" -> ValueSome "Dissolved"
        | "E" -> ValueSome "Expired"
        | "F" -> ValueSome "Forfeited Franchise Tax"
        | "G" -> ValueSome "Miscellaneous"
        | "I" -> ValueSome "Closed by FDIC"
        | "J" -> ValueSome "State Charter Pulled"
        | "K" -> ValueSome "Forfeited Registered Agent"
        | "L" -> ValueSome "Forfeited Registered Office"
        | "M" -> ValueSome "Merger"
        | "N" -> ValueSome "Forfeited Hot Check"
        | "P" -> ValueSome "Forfeited Court Order"
        | "R" -> ValueSome "Reinstated"
        | "T" -> ValueSome "Terminated"
        | "W" -> ValueSome "Withdrawn"
        | "Y" -> ValueSome "Dead at Conversion 69"
        | "Z" -> ValueSome "Dead at Conversion 83"
        | _ -> ValueNone

    let private forfeitureOfRight (right: string) =
        match right with
        | "N" -> ValueSome "right to transact forfeited"
        | "I" -> ValueSome "franchise tax involuntarily ended"
        | "U" -> ValueSome "franchise tax not established"
        | "D" -> ValueSome "eligible for termination or withdrawal"
        | "" -> ValueSome "franchise tax ended"
        | _ -> ValueNone

    /// The Secretary of State's code decides, and the right to transact qualifies a live one. A taxpayer with no charter, record type X, has no
    /// status word.
    let standing (sos: string) (right: string) =
        match sos with
        | ""
        | "G" ->
            match right with
            | "A" -> CompanyStanding.Registered ValueNone
            | other -> CompanyStanding.Unknown(ValueSome $"right to transact {other}")
        | "A"
        | "R" ->
            match right, forfeitureOfRight right with
            | "A", _ -> CompanyStanding.Active
            | _, ValueSome concern -> CompanyStanding.ActiveWithConcern concern
            | other, ValueNone -> CompanyStanding.Unknown(ValueSome $"status {sos}, right to transact {other}")
        | "D" -> CompanyStanding.Dissolved
        | code ->
            match sosStatusWord code with
            | ValueSome word -> CompanyStanding.Closed word
            | ValueNone -> CompanyStanding.Unknown(ValueSome code)

    /// Types the layout names as foreign, as a Certificate of Authority's record type V also says.
    let private foreignTypes =
        set
            [
                "AC"
                "AF"
                "CF"
                "CI"
                "CM"
                "CS"
                "CU"
                "CX"
                "HF"
                "PF"
                "PW"
                "PY"
                "SF"
                "TF"
                "TI"
            ]

    let role (organisation: string) (recordType: string) =
        match organisation, recordType with
        | code, _ when foreignTypes.Contains code -> RecordRole.ForeignRegistration
        | _, "V" -> RecordRole.ForeignRegistration
        | _, "U" -> RecordRole.Home
        | _ -> RecordRole.NotStated

    let private organisationWord (code: string) =
        match code with
        | "AB" -> "Texas business association"
        | "AC" -> "Foreign business association"
        | "AF" -> "Foreign professional association"
        | "AP" -> "Texas professional association"
        | "AR" -> "Other association"
        | "CF" -> "Foreign profit corporation"
        | "CI" -> "Foreign limited liability company"
        | "CL" -> "Texas limited liability company"
        | "CM" -> "Foreign non-profit corporation"
        | "CN" -> "Texas non-profit corporation"
        | "CP" -> "Texas professional corporation"
        | "CT" -> "Texas profit corporation"
        | "CU" -> "Foreign professional corporation"
        | "PF" -> "Foreign limited partnership"
        | "PL" -> "Texas limited partnership"
        | other -> other

    let private code name e =
        tryString name e |> ValueOption.map _.Trim() |> ValueOption.defaultValue ""

    let company (e: JsonElement) : Company =
        let organisation = code "taxpayer_organizational_type" e
        let sos = code "sos_status_code" e
        let right = code "right_to_transact_business_code" e
        let recordType = code "record_type_code" e

        {
            Name = required "taxpayer_name" tryString e
            Number =
                SourceAnswer.trimmed "secretary_of_state_sos_or_coa_file_number" e
                |> ValueOption.orElse (tryString "taxpayer_number" e)
                |> ValueOption.defaultWith (fun () -> raise (InvalidJson "The row has no number"))
            Jurisdiction = Jurisdiction.ofCode "US-TX"
            JurisdictionAsPublished = ValueNone
            Standing = standing sos right
            Concerns = CompanyConcerns.notPublished
            StatusAsPublished =
                match sos, right with
                | "", "" -> ValueNone
                | _ -> ValueSome $"SOS status {sos}; right to transact {right}"
            LegalForm =
                if organisation = "" then
                    ValueNone
                else
                    ValueSome(organisationWord organisation)
            IncorporatedOn = SourceAnswer.dateOfTimestamp "sos_charter_date" e
            CeasedOn = ValueNone
            Addresses =
                match SourceAnswer.trimmed "taxpayer_address" e with
                | ValueNone -> NotAvailable "The row holds no address"
                | ValueSome line ->
                    Available
                        [
                            {
                                Kind = AddressKind.Other "Taxpayer address at the Texas Comptroller"
                                Address =
                                    {
                                        Lines = [ line ]
                                        Locality = SourceAnswer.trimmed "taxpayer_city" e
                                        Region = SourceAnswer.trimmed "taxpayer_state" e
                                        PostalCode = SourceAnswer.trimmed "taxpayer_zip" e
                                        Country = ValueNone
                                    }
                            }
                        ]
            Role = role organisation recordType
            HomeRegistration = ValueNone
            OtherNames = []
            EstablishmentOf = ValueNone
        }

    let rows (root: JsonElement) =
        if root.ValueKind = JsonValueKind.Array then
            root.EnumerateArray() |> List.ofSeq
        else
            raise (InvalidJson "The answer isn't a list of rows")

    /// What the rows for one number show. Some file numbers are held by two taxpayers, which can't be told apart here.
    let found (root: JsonElement) =
        match rows root with
        | [] -> NotAvailable notListed
        | [ only ] -> Available(ValueSome(company only))
        | several -> NotAvailable $"{several.Length} rows carry this number, so the dataset can't tell which is meant"

    /// The rows found. Never complete: the dataset holds almost no dissolved companies, so a short list doesn't hold every match.
    let search (root: JsonElement) =
        let matches =
            rows root
            |> List.map (fun row ->
                let found = company row

                {
                    Name = found.Name
                    Number = found.Number
                    StatusAsPublished = found.StatusAsPublished
                    Address =
                        match found.Addresses with
                        | Available(first :: _) -> ValueSome first.Address
                        | _ -> ValueNone
                })

        {
            Matches = matches
            TotalMatches = ValueNone
            Complete = false
        }

/// Texas as an ICompanyRegistry. A number not found is NotAvailable: the dataset's silence says nothing.
[<Sealed>]
type internal TexasRegistry(connection: SourceConnection) =
    static let searchLimit = 50

    let client =
        SourceClient(connection, SourceAnswer.pacing 2 (TimeSpan.FromSeconds 1.))

    let coverage = Texas.coverage
    let baseUri = "https://data.texas.gov/resource/9cir-efmm.json"

    let search (name: string) (matching: NameMatch) (jurisdiction: Jurisdiction) (ct: CancellationToken) =
        SourceAnswer.inCoverage coverage jurisdiction (fun () ->
            NameSearch.withName name matching (fun () ->
                task {
                    // The dataset holds names in upper case, so the column is compared as it is.
                    let where =
                        Uri.EscapeDataString(OpenData.nameCondition "taxpayer_name" "like" name matching)

                    let uri = Uri $"{baseUri}?$where={where}&$limit={searchLimit}"

                    match! client.GetAsync(uri, ct) with
                    | Error failure -> return Error failure
                    | Ok reply when reply.Status = 404 -> return Error(RegistryFailure.RequestRefused "HTTP 404")
                    | Ok reply ->
                        return
                            SourceAnswer.json reply (Texas.search >> OpenData.keepExact name matching)
                            |> Result.map (fun found ->
                                SourceAnswer.evidence coverage reply reply.LastModified (Available found))
                }))

    interface INameMatchSearch with
        member _.SearchByNameAsync
            (name: string, matching: NameMatch, jurisdiction: Jurisdiction, ct: CancellationToken)
            =
            search name matching jurisdiction ct

    interface ICompanyRegistry with
        member _.Coverage = coverage

        member _.TryFindAsync(number: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                match Texas.lookupOf number with
                | ValueNone ->
                    Threading.Tasks.Task.FromResult(
                        Error(
                            RegistryFailure.InvalidRequest
                                "A Texas number is a file number of up to ten digits, or a taxpayer number of eleven"
                        )
                    )
                | ValueSome(field, value) ->
                    task {
                        // Two rows are enough to show the number isn't unique.
                        let uri = Uri $"{baseUri}?{field}={Uri.EscapeDataString value}&$limit=2"

                        match! client.GetAsync(uri, ct) with
                        | Error failure -> return Error failure
                        | Ok reply when reply.Status = 404 -> return Error(RegistryFailure.RequestRefused "HTTP 404")
                        | Ok reply ->
                            return
                                SourceAnswer.json reply Texas.found
                                |> Result.map (SourceAnswer.evidence coverage reply reply.LastModified)
                    })

        member _.SearchAsync(name: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            search name NameMatch.StartsWith jurisdiction ct

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
