namespace CompanyDueDiligence

open System
open System.Text.Json
open System.Text.RegularExpressions
open System.Threading
open CompanyDueDiligence.Json
open CompanyDueDiligence.CompaniesHouse

/// KVK's open dataset "Basis Bedrijfsgegevens" (the high-value dataset), read from its OpenAPI document api_opendata_basis.yml, version 1.1.0. It
/// holds BVs and NVs only, and publishes neither the name nor the number, so a record is the KVK number asked with the name not published.
module internal Netherlands =

    let coverage: RegistryCoverage =
        {
            Registry = "KVK open dataset Basis Bedrijfsgegevens (Netherlands)"
            Jurisdictions = [ Jurisdiction.ofCode "NL" ]
            Standing =
                Support.Partial
                    "Active or inactive, and bankruptcy, debt restructuring or suspension of payments; BVs and NVs only"
            Concerns =
                Support.Partial
                    "Insolvency proceedings are stated when recorded; overdue filings and disputes aren't published"
            Addresses = Support.NotOffered "Only the first two digits of the visiting address's postcode are published"
            Search = Support.NotOffered "The dataset publishes no names to search"
            Officers = Support.NotOffered "The dataset lists no officers"
            Owners = Support.NotOffered "The dataset lists no owners"
            Cost = AccessCost.Free
            RateLimit =
                RateLimitPolicy.Published(
                    1,
                    TimeSpan.FromMinutes 1.,
                    "KVK developer portal: one request a minute per IP address, and 200 per five minutes for all users together"
                )
            Licence = "CC BY 4.0, as KVK states; enriching the data so that it traces back to a person isn't permitted"
        }

    /// The dataset holds BVs and NVs, so a KVK number it doesn't hold can be any other legal form.
    [<Literal>]
    let notListed =
        "The dataset holds only BVs and NVs, so not finding this KVK number proves nothing"

    let private kvkRegex = Regex @"\A[0-9]{8}\z"

    /// A KVK number: eight digits.
    let normalKvk (number: string) =
        let normal = if isNull number then "" else number.Trim().Replace(" ", "")

        if kvkRegex.IsMatch normal then
            ValueSome normal
        else
            ValueNone

    /// An insolvency code decides the standing, then the active indicator, as the OpenAPI document lists them. A code not listed is Unknown.
    let standing (active: string voption) (insolvency: string voption) =
        match insolvency, active with
        | ValueSome "FAIL", _ -> CompanyStanding.InInsolvency "Faillissement (bankruptcy)"
        | ValueSome "SSAN", _ -> CompanyStanding.InInsolvency "Schuldsanering (debt restructuring)"
        | ValueSome "SURS", _ -> CompanyStanding.InInsolvency "Surseance van betaling (suspension of payments)"
        | ValueSome code, _ -> CompanyStanding.Unknown(ValueSome code)
        | ValueNone, ValueSome "J" -> CompanyStanding.Active
        | ValueNone, ValueSome "N" -> CompanyStanding.Closed "Inactief (inactive)"
        | ValueNone, ValueSome other -> CompanyStanding.Unknown(ValueSome other)
        | ValueNone, ValueNone -> CompanyStanding.Unknown ValueNone

    let private legalForm (code: string) =
        match code with
        | "BV" -> "Besloten vennootschap (BV)"
        | "NV" -> "Naamloze vennootschap (NV)"
        | other -> other

    /// The record for the number asked. One without the active indicator isn't a record of this dataset.
    let company (kvk: string) (e: JsonElement) : Company =
        let active =
            match SourceAnswer.trimmed "actief" e with
            | ValueSome flag -> flag
            | ValueNone -> raise (InvalidJson "Missing or invalid field 'actief'")

        let insolvency = SourceAnswer.trimmed "insolventieCode" e
        let memberState = SourceAnswer.trimmed "lidstaat" e

        {
            Name = PartyNames.NotPublished
            OtherNames = []
            Number = kvk
            Jurisdiction = Jurisdiction.ofCode "NL"
            JurisdictionAsPublished = memberState
            Standing = standing (ValueSome active) insolvency
            Concerns =
                match insolvency with
                | ValueSome _ ->
                    {
                        Stated = [ CompanyConcern.InsolvencyHistory ]
                        Unconfirmed = CompanyConcern.all |> List.except [ CompanyConcern.InsolvencyHistory ]
                    }
                | ValueNone -> CompanyConcerns.notPublished
            StatusAsPublished =
                match insolvency with
                | ValueSome code -> ValueSome $"actief {active}; insolventieCode {code}"
                | ValueNone -> ValueSome $"actief {active}"
            LegalForm = SourceAnswer.trimmed "rechtsvormCode" e |> ValueOption.map legalForm
            IncorporatedOn = ValueNone
            CeasedOn = ValueNone
            Addresses = NotAvailable "Only the first two digits of the visiting address's postcode are published"
            Role =
                match memberState with
                | ValueSome "NL" -> RecordRole.Home
                | _ -> RecordRole.NotStated
            HomeRegistration = ValueNone
            EstablishmentOf = ValueNone
        }

/// The Netherlands as an ICompanyRegistry over KVK's open dataset. It waits up to a minute for its turn, a 429 is RateLimited without asking
/// again, and a 404 is NotAvailable.
[<Sealed>]
type internal NetherlandsRegistry(connection: SourceConnection) =
    static let pacing: Pacing =
        { SourceAnswer.pacing 1 (TimeSpan.FromMinutes 1.) with
            LongestWait = TimeSpan.FromMinutes 1.
            Retries = 0
        }

    let client = SourceClient(connection, pacing)
    let coverage = Netherlands.coverage

    let unasked reason =
        Threading.Tasks.Task.FromResult(Ok(SourceAnswer.unasked coverage client.Now (NotAvailable reason)))

    interface ICompanyRegistry with
        member _.Coverage = coverage

        member _.TryFindAsync(number: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                match Netherlands.normalKvk number with
                | ValueNone ->
                    Threading.Tasks.Task.FromResult(
                        Error(RegistryFailure.InvalidRequest "A KVK number has eight digits")
                    )
                | ValueSome kvk ->
                    task {
                        let uri =
                            Uri $"https://opendata.kvk.nl/api/v1/hvds/basisbedrijfsgegevens/kvknummer/{kvk}"

                        match! client.GetAsync(uri, ct) with
                        | Error failure -> return Error failure
                        | Ok reply when reply.Status = 404 ->
                            return
                                Ok(
                                    { SourceAnswer.evidence
                                          coverage
                                          reply
                                          ValueNone
                                          (NotAvailable Netherlands.notListed) with
                                        Raw = ValueNone
                                    }
                                )
                        | Ok reply ->
                            return
                                SourceAnswer.json reply (Netherlands.company kvk)
                                |> Result.map (fun found ->
                                    SourceAnswer.evidence coverage reply ValueNone (Available(ValueSome found)))
                    })

        member _.SearchAsync(_: string, jurisdiction: Jurisdiction, _: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () -> unasked "The dataset publishes no names to search")

        member _.OfficersAsync(_: string, jurisdiction: Jurisdiction, _: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () -> unasked "The dataset lists no officers")

        member _.OwnersAsync(_: string, jurisdiction: Jurisdiction, _: OwnerPurpose, _: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () -> unasked "The dataset lists no owners")
