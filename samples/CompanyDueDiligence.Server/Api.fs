/// The JSON API under /api. Every answer is JSON, a failure as { "error": … }.
module CompanyDueDiligence.Server.Api

open System
open System.Threading.Tasks
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Routing
open CompanyDueDiligence
open CompanyDueDiligence.Web.Dto

/// An answer: its status and the value written as JSON.
type Reply = { Status: int; Body: obj }

let ok (body: 'T) = { Status = 200; Body = box body }

let accepted (body: 'T) = { Status = 202; Body = box body }

let failure (status: int) (message: string) =
    {
        Status = status
        Body = box { ErrorDto.Error = message }
    }

/// Writes work's reply; an exception is a 500 holding its message.
let private reply (work: HttpContext -> Task<Reply>) =
    RequestDelegate(fun context ->
        task {
            let! answer =
                task {
                    try
                        return! work context
                    with
                    | :? OperationCanceledException when context.RequestAborted.IsCancellationRequested ->
                        return failure 499 "The request was cancelled"
                    | error -> return failure 500 error.Message
                }

            context.Response.StatusCode <- answer.Status
            do! context.Response.WriteAsJsonAsync(answer.Body, answer.Body.GetType())
        }
        :> Task)

let private queryOf (context: HttpContext) (name: string) =
    match context.Request.Query.TryGetValue name with
    | true, values -> ValueSome(values.ToString())
    | _ -> ValueNone

/// The codes a request names, separated by commas, in order. ValueNone when the parameter is absent, which means every register.
let private codesOf (context: HttpContext) =
    queryOf context "codes"
    |> ValueOption.map (fun text ->
        text.Split(',', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
        |> Array.distinct
        |> List.ofArray)

let private routeOf (context: HttpContext) (name: string) =
    string context.Request.RouteValues[name]

/// A route value as sent: Kestrel leaves an encoded slash encoded, so a number holding one is decoded here.
let private numberOf (context: HttpContext) =
    (routeOf context "number").Replace("%2F", "/").Replace("%2f", "/").Trim()

/// Maps the API's endpoints, and a JSON 404 for any other path under /api.
let map
    (app: WebApplication)
    (connection: SourceConnection)
    (keys: KeyStore)
    (bulk: BulkService)
    (available: ServerRegisters)
    =
    let gleif = Sources.gleif connection

    let find (all: AvailableRegister list) (code: string) =
        all |> List.tryFind (fun register -> register.Listed.Code = code)

    let jurisdictionOf (register: AvailableRegister) =
        Jurisdiction.ofCode register.Listed.Jurisdiction

    let lookup (context: HttpContext) =
        task {
            let code = (routeOf context "code").ToUpperInvariant()
            let number = numberOf context
            let! all = available.AllAsync()

            match find all code with
            | None -> return failure 404 $"No register {code} is available now"
            | Some _ when number = "" -> return failure 400 "A registration number is needed"
            | Some picked ->
                let order =
                    match codesOf context with
                    | ValueSome codes -> codes |> List.choose (find all)
                    | ValueNone -> all

                let registries =
                    picked :: (order |> List.filter (fun register -> register.Listed.Code <> code))
                    |> List.map _.Registry

                let sources: CaseSources =
                    {
                        Registries = registries
                        Filings =
                            registries
                            |> List.choose (fun registry ->
                                match registry with
                                | :? IFilingRegister as filings -> Some filings
                                | _ -> None)
                        CrossCheck =
                            if queryOf context "gleif" = ValueSome "false" then
                                ValueNone
                            else
                                ValueSome gleif
                        Routes = CoverageRoutes.all
                        KnownAddresses = []
                    }

                let start: CaseStart =
                    {
                        CaseId = "LOCAL-" + DateTime.UtcNow.ToString "yyyyMMddHHmmss"
                        Number = number
                        Jurisdiction = jurisdictionOf picked
                        NameGiven = "(as entered in the demo)"
                    }

                let! case =
                    Investigation.openAsync sources InvestigationOptions.defaults start context.RequestAborted

                return ok (caseOf case)
        }

    /// The codes asked, or every register's when none are named.
    let wanted (context: HttpContext) (all: AvailableRegister list) =
        codesOf context |> ValueOption.defaultValue (all |> List.map _.Listed.Code)

    let search (context: HttpContext) =
        task {
            match queryOf context "term" |> ValueOption.map _.Trim() with
            | ValueNone
            | ValueSome "" -> return failure 400 "A name to search for is needed"
            | ValueSome term ->
                let! all = available.AllAsync()

                // ?match=exact or contains; anything else is starts-with, as the page sends by default.
                let matching =
                    match queryOf context "match" |> ValueOption.map _.ToLowerInvariant() with
                    | ValueSome "exact" -> NameMatch.Exact
                    | ValueSome "contains" -> NameMatch.Contains
                    | _ -> NameMatch.StartsWith

                let ask (code: string) =
                    task {
                        match find all code with
                        | None -> return searchRowFailed code "Not available now"
                        | Some register ->
                            try
                                let! answer =
                                    Registries.searchByNameAsync
                                        register.Registry
                                        term
                                        matching
                                        (jurisdictionOf register)
                                        context.RequestAborted

                                return searchRow code answer
                            with error when not context.RequestAborted.IsCancellationRequested ->
                                return searchRowFailed code error.Message
                    }

                let! rows = wanted context all |> List.map ask |> Task.WhenAll
                return ok rows
        }

    let people (context: HttpContext) =
        task {
            match queryOf context "name" |> ValueOption.map _.Trim() with
            | ValueSome name when (PersonName.tryParse name).IsSome ->
                let! all = available.AllAsync()
                let codes = wanted context all

                let searching, cantSearch =
                    codes
                    |> List.partition (fun code ->
                        match find all code with
                        | Some { Registry = :? IPersonSearch } -> true
                        | Some _ -> false
                        // Kept as a row, which says it isn't available.
                        | None -> true)

                let ask (code: string) =
                    task {
                        match find all code with
                        | Some({
                                   Registry = :? IPersonSearch as search
                               } as register) ->
                            try
                                let! answer =
                                    search.SearchPeopleAsync(name, jurisdictionOf register, context.RequestAborted)

                                return personRow code answer
                            with error when not context.RequestAborted.IsCancellationRequested ->
                                return personRowFailed code error.Message
                        | _ -> return personRowFailed code "Not available now"
                    }

                let! rows = searching |> List.map ask |> Task.WhenAll

                return
                    ok
                        {
                            Rows = rows
                            CantSearch = Array.ofList cantSearch
                        }
            | _ -> return failure 400 "Type a person's name as Firstname Lastname, or Lastname, Firstname"
        }

    let settingOf (context: HttpContext) =
        let name = routeOf context "settingName"

        if List.contains name KeyedSources.settingNames then
            ValueSome name
        else
            ValueNone

    let saveKey (context: HttpContext) =
        task {
            match settingOf context with
            | ValueNone -> return failure 404 "No keyed register reads that setting"
            | ValueSome name ->
                let! body =
                    task {
                        try
                            let! read = context.Request.ReadFromJsonAsync<KeyValueDto>(context.RequestAborted)
                            return ValueOption.ofObj (box read) |> ValueOption.map (fun _ -> read.Value)
                        with
                        | :? InvalidOperationException
                        | :? Text.Json.JsonException -> return ValueNone
                    }

                return
                    match body |> ValueOption.bind ValueOption.ofObj |> ValueOption.map _.Trim() with
                    | ValueSome value when value <> "" ->
                        keys.Save(name, value)
                        ok (available.Keys() |> Array.ofList)
                    | _ -> failure 400 "Send the key as JSON: { \"value\": \"…\" }"
        }

    let removeKey (context: HttpContext) =
        task {
            return
                match settingOf context with
                | ValueNone -> failure 404 "No keyed register reads that setting"
                | ValueSome name ->
                    keys.Remove name
                    ok (available.Keys() |> Array.ofList)
        }

    let bulkStates () = bulk.States() |> Array.ofList

    let started (result: Result<unit, string>) =
        match result with
        | Ok() -> accepted (bulkStates ())
        | Error message -> failure 409 message

    let known (context: HttpContext) (act: string -> Task<Reply>) =
        let id = routeOf context "id"

        match BulkCatalogue.tryFind id with
        | ValueSome _ -> act id
        | ValueNone -> Task.FromResult(failure 404 $"No bulk source has the id {id}")

    let addFiles (context: HttpContext) =
        known context (fun pid ->
            task {
                let! body =
                    task {
                        try
                            let! read = context.Request.ReadFromJsonAsync<BulkFilesDto>(context.RequestAborted)

                            return
                                ValueOption.ofObj (box read)
                                |> ValueOption.bind (fun _ -> ValueOption.ofObj read.Paths)
                        with
                        | :? InvalidOperationException
                        | :? Text.Json.JsonException -> return ValueNone
                    }

                return
                    match body with
                    | ValueNone -> failure 400 "Send the files as JSON: { \"paths\": { \"<slot>\": [\"<path>\"] } }"
                    | ValueSome paths ->
                        let bySlot =
                            paths
                            |> Seq.map (fun pair -> pair.Key, (if isNull pair.Value then [] else List.ofArray pair.Value))
                            |> Map.ofSeq

                        match bulk.StartBuild(pid, bySlot) with
                        | Ok() -> accepted (bulkStates ())
                        | Error message -> failure 400 message
            })

    let get (pattern: string) work =
        app.MapGet(pattern, reply work) |> ignore

    get "/api/mode" (fun _ -> Task.FromResult(ok { Server = true }))

    get "/api/registers" (fun _ ->
        task {
            let! all = available.AllAsync()
            return ok (all |> List.map _.Listed |> Array.ofList)
        })

    get "/api/lookup/{code}/{number}" lookup
    get "/api/search" search
    get "/api/people" people
    get "/api/keys" (fun _ -> Task.FromResult(ok (available.Keys() |> Array.ofList)))
    app.MapPut("/api/keys/{settingName}", reply saveKey) |> ignore
    app.MapDelete("/api/keys/{settingName}", reply removeKey) |> ignore
    get "/api/bulk" (fun _ -> Task.FromResult(ok (bulkStates ())))

    app.MapPost(
        "/api/bulk/{id}/download",
        reply (fun context -> known context (bulk.StartDownload >> started >> Task.FromResult))
    )
    |> ignore

    app.MapDelete(
        "/api/bulk/{id}",
        reply (fun context ->
            known context (fun pid ->
                task {
                    match! bulk.Remove pid with
                    | Ok() -> return ok (bulkStates ())
                    | Error message -> return failure 409 message
                }))
    )
    |> ignore

    app.MapPost("/api/bulk/{id}/files", reply addFiles) |> ignore

    get "/api/bulk/{id}/current" (fun context ->
        known context (fun id ->
            task {
                match BulkCatalogue.tryFind id with
                | ValueSome { Delivery = BulkDelivery.ByHand how } ->
                    return failure 409 $"It is fetched by hand: {how}"
                | _ ->
                    match! bulk.CurrentFiles id with
                    | Ok files -> return ok (files |> List.map bulkFileDto |> Array.ofList)
                    | Error message -> return failure 502 message
            }))

    app.MapFallback("/api/{**rest}", reply (fun _ -> Task.FromResult(failure 404 "No such API path")))
    |> ignore
