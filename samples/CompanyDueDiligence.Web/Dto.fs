/// The local server's API answers, as plain records the page and the server share, with the conversions from the library's values. A string
/// that may be absent is null; nothing here is a discriminated union, so System.Text.Json reads and writes each as it is.
module CompanyDueDiligence.Web.Dto

open System
open CompanyDueDiligence

/// GET api/mode. Only the local server answers it.
type ModeDto = { Server: bool }

/// Any API failure.
type ErrorDto = { Error: string }

/// The kinds a register is listed as.
module Kind =

    [<Literal>]
    let Keyless = "keyless"

    [<Literal>]
    let Keyed = "keyed"

    [<Literal>]
    let Bulk = "bulk"

/// A register available now.
type RegisterDto =
    {
        /// Unique among the registers listed: a jurisdiction code, or a BulkCatalogue id such as AU-ASIC.
        Code: string
        Label: string
        Region: string
        /// The flag-icons file name.
        Flag: string
        /// Keyless, keyed or bulk.
        Kind: string
        /// The jurisdiction a case asks it for.
        Jurisdiction: string
        /// A number to try, or empty.
        Example: string
        SearchesCompanies: bool
        SearchesPeople: bool
    }

type CompanyHitDto =
    {
        Name: string
        Number: string
        /// The status as published, or empty.
        Status: string
    }

/// One register's answer to a name search: its matches, or the reason it gave none.
type SearchRowDto =
    {
        Code: string
        Matches: CompanyHitDto array
        TotalMatches: Nullable<int>
        Complete: bool
        /// Null when the register answered.
        Reason: string
    }

type PersonHitDto =
    {
        Name: string
        Role: string
        Current: Nullable<bool>
        CompanyName: string
        CompanyNumber: string
        CompanyStatus: string
    }

/// One register's answer to a person search: its matches, or the reason it gave none.
type PersonRowDto =
    {
        Code: string
        Matches: PersonHitDto array
        Complete: bool
        /// Null when the register answered.
        Reason: string
    }

/// A row per register asked that searches people, and the codes of those asked that don't.
type PeopleDto =
    {
        Rows: PersonRowDto array
        CantSearch: string array
    }

type ReviewItemDto =
    {
        Reference: string
        About: string
        Text: string
    }

/// A case opened: what to review and the case file as text, without raw answers.
type CaseDto =
    {
        Review: ReviewItemDto array
        Export: string
    }

/// One setting of a keyed register. Its value is never sent.
type KeySettingDto =
    {
        Name: string
        /// Saved in keys.json.
        Saved: bool
        /// Given by an environment variable of the same name, which a saved value overrides.
        FromEnvironment: bool
    }

type KeyedRegisterDto =
    {
        Code: string
        Label: string
        Flag: string
        Settings: KeySettingDto array
        /// True when every setting is given, so the register is listed.
        Active: bool
        /// A setting given without its pair, or null.
        Problem: string
    }

/// PUT api/keys/{settingName}.
type KeyValueDto = { Value: string }

/// A file a by-hand bulk source is built from.
type BulkSlotDto =
    {
        Name: string
        Label: string
        Required: bool
        /// Takes several paths, one per line.
        Many: bool
    }

/// The states a bulk source is reported in.
module BulkState =

    [<Literal>]
    let NotDownloaded = "NotDownloaded"

    [<Literal>]
    let Downloading = "Downloading"

    [<Literal>]
    let Indexing = "Indexing"

    [<Literal>]
    let Downloaded = "Downloaded"

    [<Literal>]
    let IndexBuilt = "IndexBuilt"

/// A file a resolver's listing names.
type BulkFileDto =
    {
        /// The name it is saved under.
        Name: string
        /// Its name on the portal.
        PortalName: string
        Url: string
        /// ISO 8601, or null when the listing gives no date.
        Modified: string
        /// As the listing states it, or the source's stated size when it states none.
        Bytes: int64
    }

/// A BulkCatalogue entry with what the server holds of it.
type BulkDto =
    {
        Id: string
        Title: string
        Flag: string
        Page: string
        /// "links", "resolved" or "by-hand".
        Delivery: string
        /// The catalogue's size of all its files, for links and resolved sources.
        Bytes: Nullable<int64>
        RefreshDays: float
        Licence: string
        /// Where to get the files by hand, for a by-hand or resolved source.
        How: string
        /// The listing a resolved source's files are found through: its portal and dataset. Null for other sources.
        Portal: string
        /// The files a by-hand or resolved source is built from on this machine; empty when the server can't build it.
        Slots: BulkSlotDto array
        /// The files the resolver named for the download saved now; empty for other sources, or when nothing is downloaded.
        Files: BulkFileDto array
        /// One of BulkState.
        State: string
        /// The file being fetched.
        File: string
        /// Bytes fetched so far, across its files.
        Done: Nullable<int64>
        Expected: Nullable<int64>
        /// The saved files' size.
        SavedBytes: Nullable<int64>
        IndexBytes: Nullable<int64>
        /// ISO 8601, or null.
        DownloadedAt: string
        /// Older than the publisher's cadence.
        Stale: bool
        /// The last download's or build's failure, or null.
        Error: string
    }

/// The DTO of a resolved file.
let bulkFileDto (file: BulkResolvedFile) =
    {
        Name = file.Link.Name
        PortalName = file.PortalName
        Url = string file.Link.Url
        Modified =
            file.Modified
            |> ValueOption.map _.ToString("o", Globalization.CultureInfo.InvariantCulture)
            |> ValueOption.toObj
        Bytes = file.Link.ApproximateBytes
    }

/// POST api/bulk/{id}/files: paths on the server's machine, by slot name.
type BulkFilesDto =
    {
        Paths: Collections.Generic.Dictionary<string, string array>
    }

let private nullable (value: 'T voption) =
    match value with
    | ValueSome value -> Nullable value
    | ValueNone -> Nullable()

let private orNull (value: string voption) = ValueOption.toObj value

/// A failure in words for a register's row or a case; a refused Companies House key is named without the key.
let describeFailure (code: string) (failure: RegistryFailure) =
    match failure with
    | RegistryFailure.AccessRefused _ when code = "GB" -> "Companies House refused the key"
    | other -> RegistryFailure.describe other

/// A register as listed, with what its registry says it searches.
let registerOf
    (code: string)
    (label: string)
    (region: string)
    (flag: string)
    (kind: string)
    (jurisdiction: string)
    (example: string)
    (registry: ICompanyRegistry)
    : RegisterDto =
    {
        Code = code
        Label = label
        Region = region
        Flag = flag
        Kind = kind
        Jurisdiction = jurisdiction
        Example = example
        SearchesCompanies =
            match registry.Coverage.Search with
            | Support.NotOffered _ -> false
            | Support.Supported
            | Support.Partial _ -> true
        SearchesPeople = registry :? IPersonSearch
    }

let private availableOr
    (describe: RegistryFailure -> string)
    (answer: Result<Evidence<Availability<'T>>, RegistryFailure>)
    =
    match answer with
    | Ok { Value = Available value } -> Ok value
    | Ok { Value = NotAvailable reason } -> Error reason
    | Ok { Value = NeedsEntitlement entitlement } -> Error entitlement.Route
    | Error failure -> Error(describe failure)

/// A row with no matches and the reason.
let searchRowFailed (code: string) (reason: string) =
    {
        SearchRowDto.Code = code
        Matches = [||]
        TotalMatches = Nullable()
        Complete = false
        Reason = reason
    }

let searchRow (code: string) (answer: Result<Evidence<Availability<SearchResult>>, RegistryFailure>) =
    match availableOr (describeFailure code) answer with
    | Ok result ->
        {
            SearchRowDto.Code = code
            Matches =
                result.Matches
                |> List.map (fun found ->
                    {
                        CompanyHitDto.Name = found.Name
                        Number = found.Number
                        Status = found.StatusAsPublished |> ValueOption.defaultValue ""
                    })
                |> Array.ofList
            TotalMatches = nullable result.TotalMatches
            Complete = result.Complete
            Reason = null
        }
    | Error reason -> searchRowFailed code reason

let personRowFailed (code: string) (reason: string) =
    {
        PersonRowDto.Code = code
        Matches = [||]
        Complete = false
        Reason = reason
    }

let personRow (code: string) (answer: Result<Evidence<Availability<PersonSearchResult>>, RegistryFailure>) =
    match availableOr (describeFailure code) answer with
    | Ok result ->
        {
            PersonRowDto.Code = code
            Matches =
                result.Matches
                |> List.map (fun found ->
                    {
                        PersonHitDto.Name = found.Name
                        Role = orNull found.Role
                        Current = nullable found.Current
                        CompanyName = orNull found.CompanyName
                        CompanyNumber = orNull found.CompanyNumber
                        CompanyStatus = orNull found.CompanyStatus
                    })
                |> Array.ofList
            Complete = result.Complete
            Reason = null
        }
    | Error reason -> personRowFailed code reason

let caseOf (case: CaseFile) =
    {
        Review =
            CaseFile.reviewNeeded case
            |> List.map (fun item ->
                {
                    Reference = item.Reference
                    About = orNull item.About
                    Text = item.Text
                })
            |> Array.ofList
        Export = CaseExport.toText false case
    }
