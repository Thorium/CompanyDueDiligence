namespace CompanyDueDiligence

open System
open System.Collections.Generic
open System.Text.Json
open CompanyDueDiligence.Json

/// Queries and answers of the open data platforms several registers publish on: Socrata (SODA 2.1), CKAN's datastore and ArcGIS feature
/// services. Values go into a query only as quoted literals.
module internal OpenData =

    /// A SoQL or SQL string literal, with a quote doubled.
    let literal (value: string) = "'" + value.Replace("'", "''") + "'"

    /// A search term with the characters that would widen a LIKE pattern taken out, in upper case.
    let likeTerm (name: string) =
        name.Trim().ToUpperInvariant().Replace("%", "").Replace("_", " ")

    /// Rows of a Socrata resource where field equals value. dataset is the resource URL ending in .json.
    let socrataWhere (dataset: string) (field: string) (value: string) (limit: int) =
        let where = Uri.EscapeDataString $"{field} = {literal value}"
        Uri $"{dataset}?$where={where}&$limit={limit}"

    /// A normalised word as a LIKE pattern that also takes the full stops and apostrophes normalising drops in the usual places: after the first
    /// letter (O'NEIL), before a final S (MCDONALD'S), and between the letters of a word of up to three (L.L.C.).
    let private looseWord (word: string) =
        if word.Length <= 3 then
            word |> Seq.map string |> String.concat "%"
        elif word.EndsWith 'S' then
            $"{word[0]}%%{word.Substring(1, word.Length - 2)}%%S"
        else
            $"{word[0]}%%{word.Substring 1}"

    /// A SoQL or ArcGIS SQL condition that column, an expression holding the name in upper case, can match the name as asked; like is the
    /// dialect's spelling of LIKE. Every mode asks for NameMatch.normalise's words in order, each as looseWord, with anything before, between
    /// and after them; Exact also asks for the whole name in upper case. The rows, which keepExact narrows by NameMatch.matcher, hold every name
    /// matching in that mode that differs only in case, separators, and full stops or apostrophes where looseWord takes them; accents are
    /// compared as held. A name with no letter or digit asks for the term as likeTerm leaves it.
    let nameCondition (column: string) (like: string) (name: string) (matching: NameMatch) =
        let likes (pattern: string) = $"{column} {like} {literal pattern}"
        let whole = $"{column} = {literal (name.Trim().ToUpperInvariant())}"

        match NameMatching.normalise name, matching with
        | "", NameMatch.Exact -> whole
        | "", _ -> likes ("%" + likeTerm name + "%")
        | normal, _ ->
            let words =
                "%" + (normal.Split ' ' |> Array.map looseWord |> String.concat "%") + "%"

            match matching with
            | NameMatch.Exact -> $"({whole} OR {likes words})"
            | NameMatch.StartsWith
            | NameMatch.Contains -> likes words

    /// Rows of a Socrata resource whose field matches the name as asked, compared in upper case, as nameCondition builds it.
    let socrataName (dataset: string) (field: string) (name: string) (matching: NameMatch) (limit: int) =
        let where =
            Uri.EscapeDataString(nameCondition $"upper({field})" "like" name matching)

        Uri $"{dataset}?$where={where}&$limit={limit}"

    /// Rows of a Socrata resource whose field starts with the term, compared in upper case.
    let socrataPrefix (dataset: string) (field: string) (name: string) (limit: int) =
        socrataName dataset field name NameMatch.StartsWith limit

    /// Rows of a Socrata resource that can be the whole name, compared in upper case; keepExact narrows them.
    let socrataExact (dataset: string) (field: string) (name: string) (limit: int) =
        socrataName dataset field name NameMatch.Exact limit

    /// Rows of a Socrata resource whose field contains the term, compared in upper case.
    let socrataContains (dataset: string) (field: string) (name: string) (limit: int) =
        socrataName dataset field name NameMatch.Contains limit

    /// A condition that each word is contained in one of the fields, compared in upper case. Words emptied by likeTerm are left out.
    let private containsEach (upper: string) (like: string) (fields: string list) (words: string list) =
        words
        |> List.map likeTerm
        |> List.filter (String.IsNullOrWhiteSpace >> not)
        |> List.map (fun word ->
            let pattern = literal ("%" + word + "%")

            let either =
                fields
                |> List.map (fun field -> $"{upper}({field}) {like} {pattern}")
                |> String.concat " OR "

            $"({either})")
        |> String.concat " AND "

    /// Rows of a Socrata resource where each word is contained in one of the fields, compared in upper case.
    let socrataWords (dataset: string) (fields: string list) (words: string list) (limit: int) =
        let where = Uri.EscapeDataString(containsEach "upper" "like" fields words)
        Uri $"{dataset}?$where={where}&$limit={limit}"

    /// Rows of a Socrata resource whose field is one of the values.
    let socrataIn (dataset: string) (field: string) (values: string list) (limit: int) =
        let listed = values |> List.map literal |> String.concat ","
        let where = Uri.EscapeDataString $"{field} in ({listed})"
        Uri $"{dataset}?$where={where}&$limit={limit}"

    /// A Socrata answer: a list of rows. Anything else, such as an error object, is unreadable.
    let socrataRows (root: JsonElement) =
        if root.ValueKind = JsonValueKind.Array then
            root.EnumerateArray() |> List.ofSeq
        else
            raise (InvalidJson "The answer isn't a list of rows")

    /// Records of a CKAN datastore resource where each field equals its value exactly.
    let ckanWhere (site: string) (resourceId: string) (filters: (string * string) list) (limit: int) =
        let filter = JsonSerializer.Serialize(dict filters)

        Uri
            $"{site}/api/3/action/datastore_search?resource_id={Uri.EscapeDataString resourceId}&filters={Uri.EscapeDataString filter}&limit={limit}"

    /// Records of a CKAN datastore resource whose field is any of the values.
    let ckanAny (site: string) (resourceId: string) (field: string) (values: string list) (limit: int) =
        let filter = JsonSerializer.Serialize(dict [ field, Array.ofList values ])

        Uri
            $"{site}/api/3/action/datastore_search?resource_id={Uri.EscapeDataString resourceId}&filters={Uri.EscapeDataString filter}&limit={limit}"

    /// Records of a CKAN datastore resource matching a full-text query.
    let ckanSearch (site: string) (resourceId: string) (query: string) (limit: int) =
        Uri
            $"{site}/api/3/action/datastore_search?resource_id={Uri.EscapeDataString resourceId}&q={Uri.EscapeDataString(query.Trim())}&limit={limit}"

    /// A CKAN answer's records, with the total it states. success false, or no records list, is unreadable.
    let ckanRecords (root: JsonElement) =
        match tryProp "success" root with
        | ValueSome flag when flag.ValueKind = JsonValueKind.True ->
            match tryObject "result" root with
            | ValueSome result ->
                match tryProp "records" result with
                | ValueSome records when records.ValueKind = JsonValueKind.Array ->
                    records.EnumerateArray() |> List.ofSeq, tryInt "total" result
                | _ -> raise (InvalidJson "The answer has no records list")
            | ValueNone -> raise (InvalidJson "The answer has no result")
        | _ -> raise (InvalidJson "The answer doesn't say it succeeded")

    /// Features of an ArcGIS layer where field equals value, attributes only. layer is the layer URL, ending in its number.
    let arcgisWhere (layer: string) (field: string) (value: string) (limit: int) =
        let where = Uri.EscapeDataString $"{field} = {literal value}"
        Uri $"{layer}/query?where={where}&outFields=*&returnGeometry=false&resultRecordCount={limit}&f=json"

    /// Features of an ArcGIS layer whose field matches the name as asked, compared in upper case, as nameCondition builds it.
    let arcgisName (layer: string) (field: string) (name: string) (matching: NameMatch) (limit: int) =
        let where =
            Uri.EscapeDataString(nameCondition $"UPPER({field})" "LIKE" name matching)

        Uri $"{layer}/query?where={where}&outFields=*&returnGeometry=false&resultRecordCount={limit}&f=json"

    /// Features of an ArcGIS layer whose field starts with the term, compared in upper case.
    let arcgisPrefix (layer: string) (field: string) (name: string) (limit: int) =
        arcgisName layer field name NameMatch.StartsWith limit

    /// Features of an ArcGIS layer that can be the whole name, compared in upper case; keepExact narrows them.
    let arcgisExact (layer: string) (field: string) (name: string) (limit: int) =
        arcgisName layer field name NameMatch.Exact limit

    /// Features of an ArcGIS layer whose field contains the term, compared in upper case.
    let arcgisContains (layer: string) (field: string) (name: string) (limit: int) =
        arcgisName layer field name NameMatch.Contains limit

    /// Features of an ArcGIS layer where each word is contained in the field, compared in upper case.
    let arcgisWords (layer: string) (field: string) (words: string list) (limit: int) =
        let where = Uri.EscapeDataString(containsEach "UPPER" "LIKE" [ field ] words)
        Uri $"{layer}/query?where={where}&outFields=*&returnGeometry=false&resultRecordCount={limit}&f=json"

    /// One ArcGIS answer: the attributes of each feature, and whether more features matched than it holds.
    type ArcgisPage =
        {
            Rows: JsonElement list
            /// exceededTransferLimit: the service stopped at its own or the asked record count, so more features match than Rows holds.
            Truncated: bool
        }

    /// The features of an answer. ArcGIS answers a failure with 200 and an error object, which is unreadable.
    let arcgisPage (root: JsonElement) =
        match tryObject "error" root with
        | ValueSome _ -> raise (InvalidJson "The service answered with an error")
        | ValueNone ->
            match tryProp "features" root with
            | ValueSome features when features.ValueKind = JsonValueKind.Array ->
                {
                    Rows =
                        features.EnumerateArray()
                        |> Seq.map (fun feature ->
                            match tryObject "attributes" feature with
                            | ValueSome attributes -> attributes
                            | ValueNone -> raise (InvalidJson "A feature has no attributes"))
                        |> List.ofSeq
                    Truncated = tryFlag "exceededTransferLimit" root = ValueSome true
                }
            | _ -> raise (InvalidJson "The answer has no features list")

    /// Every feature of an answer that holds them all. A truncated answer is unreadable, since what it leaves out can't be told.
    let arcgisAttributes (root: JsonElement) =
        match arcgisPage root with
        | { Truncated = true } ->
            raise (InvalidJson "The service returned fewer features than matched (exceededTransferLimit)")
        | page -> page.Rows

    let private unixMillisecondsRange =
        DateTimeOffset.MinValue.ToUnixTimeMilliseconds(), DateTimeOffset.MaxValue.ToUnixTimeMilliseconds()

    /// ArcGIS dates are milliseconds since 1970, in UTC. A value outside the dates .NET can hold is none.
    let arcgisDate name (e: JsonElement) =
        let lowest, highest = unixMillisecondsRange

        tryProp name e
        |> ValueOption.bind (fun value ->
            match value.ValueKind with
            | JsonValueKind.Number ->
                match value.TryGetInt64() with
                | true, ms when ms >= lowest && ms <= highest ->
                    ValueSome(DateOnly.FromDateTime(DateTimeOffset.FromUnixTimeMilliseconds(ms).UtcDateTime))
                | _ -> ValueNone
            | _ -> ValueNone)

    /// What the rows for one number show: none is absent, one is the company, and several can't be told apart.
    let foundIn (absent: Availability<Company voption>) (read: JsonElement -> Company) (rows: JsonElement list) =
        match rows with
        | [] -> absent
        | [ only ] -> Available(ValueSome(read only))
        | several -> NotAvailable $"{several.Length} records carry this number, so the source can't tell which is meant"

    /// A search result from rows read as companies, complete when fewer came back than the limit asked.
    let matches (limit: int) (total: int voption) (companies: Company list) : SearchResult =
        let found =
            companies
            |> List.map (fun company ->
                {
                    Name = company.Name
                    Number = company.Number
                    StatusAsPublished = company.StatusAsPublished
                    Address =
                        match company.Addresses with
                        | Available(first :: _) -> ValueSome first.Address
                        | _ -> ValueNone
                })

        {
            Matches = found
            TotalMatches = total
            Complete =
                match total with
                | ValueSome total -> total = found.Length
                | ValueNone -> found.Length < limit
        }

    /// The matches whose name matches the name asked in that mode, as NameMatch.matcher tests: nameCondition's rows can hold more.
    let exactOnly (name: string) (matching: NameMatch) (found: CompanyMatch list) =
        let matches = NameMatch.matcher matching name
        found |> List.filter (fun one -> matches one.Name)

    /// The result with its matches narrowed by exactOnly. Complete is kept: a query that returned every row it matched has shown every
    /// match. TotalMatches, which counts the query's rows, is dropped.
    let keepExact (name: string) (matching: NameMatch) (result: SearchResult) =
        { result with
            Matches = exactOnly name matching result.Matches
            TotalMatches = ValueNone
        }

    /// matches over the rows of a nameCondition query, narrowed by keepExact; complete when fewer rows came back than the limit asked.
    let nameMatches (name: string) (matching: NameMatch) (limit: int) (companies: Company list) =
        matches limit ValueNone companies |> keepExact name matching

    /// A text value with its blanks trimmed, read whether the source types it as a string or a number; empty is absent.
    let text name e =
        tryText name e
        |> ValueOption.map _.Trim()
        |> ValueOption.bind (fun value -> if value = "" then ValueNone else ValueSome value)

    /// Distinct values in order of first appearance.
    let distinctInOrder (values: string seq) =
        let seen = HashSet<string>()
        values |> Seq.filter seen.Add |> List.ofSeq

/// What the person searches of the adapters share.
module internal PeopleSearch =

    /// Runs send with the parsed name; a name with no letter or digit is InvalidRequest and sends nothing.
    let withName (name: string) (send: PersonName -> Threading.Tasks.Task<Result<Evidence<'T>, RegistryFailure>>) =
        match PersonName.tryParse name with
        | ValueSome parsed -> send parsed
        | ValueNone ->
            Threading.Tasks.Task.FromResult(
                Error(
                    RegistryFailure.InvalidRequest
                        "A person search needs a name, as Firstname Lastname or Lastname, Firstname"
                )
            )

    /// The evidence of several calls: the last one's time, and every answer as one JSON list in the order asked.
    let evidence (coverage: RegistryCoverage) (replies: SourceReply list) asOf value : Evidence<_> =
        {
            Source = coverage.Registry
            RetrievedAt = (List.last replies).At
            AsOf = asOf
            Raw =
                match replies with
                | [ only ] -> ValueSome only.Body
                | several -> ValueSome("[" + (several |> List.map _.Body |> String.concat ",") + "]")
            Value = value
        }

    /// Words as given in each field, joined by spaces; ValueNone when none has any.
    let joined (fields: string list) (e: JsonElement) =
        match
            fields
            |> List.choose (fun field -> OpenData.text field e |> ValueOption.toOption)
        with
        | [] -> ValueNone
        | parts -> ValueSome(String.concat " " parts)
