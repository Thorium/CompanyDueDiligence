namespace CompanyDueDiligence

open System
open System.Text.Json
open System.Text.RegularExpressions
open System.Threading
open CompanyDueDiligence.Json

/// data.gov.sg's "Entities Registered with ACRA" (d_3f960c10fed6145404ca7b821f263b87), one dataset keyed by UEN, read through its datastore_search
/// as it answered on 2026-11-08. ACRA's more detailed "Information on Corporate Entities" is split into 27 datasets by the name's first letter and
/// isn't read here. The answers write "na" for a value not given.
module internal Singapore =

    [<Literal>]
    let ResourceId = "d_3f960c10fed6145404ca7b821f263b87"

    let coverage: RegistryCoverage =
        {
            Registry = "ACRA entities on data.gov.sg (Singapore)"
            Jurisdictions = [ Jurisdiction.ofCode "SG" ]
            Standing = Support.Partial "Registered or Deregistered only; a registered entity isn't stated to be live"
            Concerns = Support.NotOffered "The dataset states no overdue filings, disputes or insolvency"
            Addresses = Support.Partial "The registered street name and postal code only"
            Search = Support.Partial "50 records matching the datastore's full-text search"
            Officers = Support.NotOffered "The dataset lists no officers"
            Owners = Support.NotOffered "The dataset lists no owners"
            Cost = AccessCost.Free
            RateLimit =
                RateLimitPolicy.Published(
                    4,
                    TimeSpan.FromSeconds 10.,
                    "data.gov.sg API rate limits: Datastore Search without an API key"
                )
            Licence = "Singapore Open Data Licence version 1.0, which asks for a notice acknowledging the source"
        }

    /// The dataset is a monthly copy, so a number missing from it may be newer than the copy.
    [<Literal>]
    let notListed =
        "The dataset is a monthly copy of entities registered with ACRA, so not finding this UEN proves nothing"

    let private uenRegex =
        Regex @"\A([0-9]{8}[A-Z]|[0-9]{9}[A-Z]|[TSR][0-9]{2}[A-Z]{2}[0-9]{4}[A-Z])\z"

    /// A UEN: eight digits and a letter for a business, nine digits and a letter for a local company, or T, S or R, two digits, two letters, four
    /// digits and a letter for other entities.
    let normalUen (number: string) =
        let normal =
            if isNull number then
                ""
            else
                number.Trim().ToUpperInvariant()

        if uenRegex.IsMatch normal then
            ValueSome normal
        else
            ValueNone

    /// A value with "na" read as absent.
    let private field name e =
        OpenData.text name e
        |> ValueOption.bind (fun value ->
            if value.Equals("na", StringComparison.OrdinalIgnoreCase) then
                ValueNone
            else
                ValueSome value)

    /// The two words the dataset uses. Anything else is Unknown.
    let standing (status: string voption) =
        match status with
        | ValueSome "Registered" -> CompanyStanding.Registered(ValueSome "Registered")
        | ValueSome "Deregistered" -> CompanyStanding.Closed "Deregistered"
        | ValueSome other -> CompanyStanding.Unknown(ValueSome other)
        | ValueNone -> CompanyStanding.Unknown ValueNone

    /// Entity types seen in the answers. Any other is NotStated.
    let role (entityType: string voption) =
        match entityType with
        | ValueSome "Foreign Company Branch" -> RecordRole.ForeignRegistration
        | ValueSome("Local Company" | "Sole Proprietorship/ Partnership") -> RecordRole.Home
        | _ -> RecordRole.NotStated

    let company (e: JsonElement) : Company =
        let status = field "uen_status_desc" e
        let entityType = field "entity_type_desc" e

        {
            Name =
                field "entity_name" e
                |> ValueOption.defaultWith (fun () -> raise (InvalidJson "The record has no entity name"))
            OtherNames = []
            Number =
                field "uen" e
                |> ValueOption.defaultWith (fun () -> raise (InvalidJson "The record has no UEN"))
            Jurisdiction = Jurisdiction.ofCode "SG"
            JurisdictionAsPublished = ValueNone
            Standing = standing status
            Concerns = CompanyConcerns.notPublished
            StatusAsPublished = status
            LegalForm = entityType
            IncorporatedOn = ValueNone
            CeasedOn = ValueNone
            Addresses =
                match field "reg_street_name" e, field "reg_postal_code" e with
                | ValueNone, ValueNone -> NotAvailable "The record holds no registered address"
                | street, postalCode ->
                    Available
                        [
                            {
                                Kind = AddressKind.RegisteredOffice
                                Address =
                                    {
                                        Lines = street |> ValueOption.toList
                                        Locality = ValueNone
                                        Region = ValueNone
                                        PostalCode = postalCode
                                        Country = ValueNone
                                    }
                            }
                        ]
            Role = role entityType
            HomeRegistration = ValueNone
            EstablishmentOf = ValueNone
        }

    /// The record carrying this UEN. The filter is exact, so a record with another UEN fails the read.
    let lookup (uen: string) (root: JsonElement) =
        let records, _ = OpenData.ckanRecords root

        match records with
        | [] -> NotAvailable notListed, ValueNone
        | [ only ] ->
            let found = company only

            if found.Number = uen then
                Available(ValueSome found), ValueSome(only.GetRawText())
            else
                raise (InvalidJson "The answer holds another entity")
        | several ->
            NotAvailable $"{several.Length} records carry this UEN, so the source can't tell which is meant", ValueNone

    /// The term without accents, as data.gov.sg refuses any other letter with a 409; none when it still holds one or is blank.
    let searchTerm (name: string) =
        let plain =
            (if isNull name then "" else name.Trim()).Normalize Text.NormalizationForm.FormD
            |> String.filter (fun c ->
                Globalization.CharUnicodeInfo.GetUnicodeCategory c
                <> Globalization.UnicodeCategory.NonSpacingMark)

        if plain = "" || plain |> Seq.exists (fun c -> c > '') then
            ValueNone
        else
            ValueSome plain

    let search (limit: int) (root: JsonElement) =
        let records, total = OpenData.ckanRecords root
        OpenData.matches limit total (records |> List.map company)

/// Singapore as an ICompanyRegistry, paced at data.gov.sg's published four datastore calls in ten seconds. A UEN not found is NotAvailable.
[<Sealed>]
type internal SingaporeRegistry(connection: SourceConnection) =
    static let searchLimit = 50

    let client =
        SourceClient(connection, SourceAnswer.pacing 4 (TimeSpan.FromSeconds 10.))

    let coverage = Singapore.coverage
    let baseUri = "https://data.gov.sg/api/action/datastore_search"

    let unasked reason =
        Threading.Tasks.Task.FromResult(Ok(SourceAnswer.unasked coverage client.Now (NotAvailable reason)))

    interface ICompanyRegistry with
        member _.Coverage = coverage

        member _.TryFindAsync(number: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                match Singapore.normalUen number with
                | ValueNone ->
                    Threading.Tasks.Task.FromResult(
                        Error(
                            RegistryFailure.InvalidRequest
                                "A UEN is eight or nine digits and a letter, or a T, S or R form such as T08LL0001A"
                        )
                    )
                | ValueSome uen ->
                    task {
                        let filter = JsonSerializer.Serialize(dict [ "uen", uen ])

                        let uri =
                            Uri
                                $"{baseUri}?resource_id={Singapore.ResourceId}&filters={Uri.EscapeDataString filter}&limit=2"

                        match! client.GetAsync(uri, ct) with
                        | Error failure -> return Error failure
                        | Ok reply when reply.Status = 404 -> return Error(RegistryFailure.RequestRefused "HTTP 404")
                        | Ok reply ->
                            return
                                SourceAnswer.json reply (Singapore.lookup uen)
                                |> Result.map (fun (found, raw) ->
                                    { SourceAnswer.evidence coverage reply ValueNone found with
                                        Raw = raw |> ValueOption.orElse (ValueSome reply.Body)
                                    })
                    })

        member _.SearchAsync(name: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                match Singapore.searchTerm name with
                | ValueNone when String.IsNullOrWhiteSpace name ->
                    Threading.Tasks.Task.FromResult(Error(RegistryFailure.InvalidRequest "A search needs a term"))
                | ValueNone ->
                    Threading.Tasks.Task.FromResult(
                        Error(
                            RegistryFailure.InvalidRequest
                                "data.gov.sg searches Latin letters, digits and punctuation only"
                        )
                    )
                | ValueSome term ->
                    task {
                        let uri =
                            Uri
                                $"{baseUri}?resource_id={Singapore.ResourceId}&q={Uri.EscapeDataString term}&limit={searchLimit}"

                        match! client.GetAsync(uri, ct) with
                        | Error failure -> return Error failure
                        | Ok reply when reply.Status = 404 -> return Error(RegistryFailure.RequestRefused "HTTP 404")
                        | Ok reply ->
                            return
                                SourceAnswer.json reply (Singapore.search searchLimit)
                                |> Result.map (fun found ->
                                    SourceAnswer.evidence coverage reply ValueNone (Available found))
                    })

        member _.OfficersAsync(_: string, jurisdiction: Jurisdiction, _: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () -> unasked "The dataset lists no officers")

        member _.OwnersAsync(_: string, jurisdiction: Jurisdiction, _: OwnerPurpose, _: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () -> unasked "The dataset lists no owners")
