module CompanyDueDiligence.Web.Main

open System
open System.Net.Http
open System.Net.Http.Json
open System.Threading
open System.Threading.Tasks
open Elmish
open Microsoft.JSInterop
open Bolero
open Bolero.Html
open CompanyDueDiligence
open CompanyDueDiligence.Web.Registers
open CompanyDueDiligence.Web.Dto

/// The keyless sources whose servers allow calls from a web page.
let browserSources = keyless |> List.filter _.InBrowser

/// The browser storage key for the Companies House JavaScript key.
[<Literal>]
let CompaniesHouseKeyStorage = "cdd-demo-companies-house-key"

/// Companies House's code, which the browser page lists only while a key is set.
[<Literal>]
let CompaniesHouseCode = "GB"

/// The browser storage key for the registers switched on in the browser.
[<Literal>]
let EnabledKey = "cdd-demo-registers"

/// The browser storage key for the registers switched off in server mode. It holds those off, so that a register listed later, such as a bulk
/// source just downloaded, starts on.
[<Literal>]
let ServerOffKey = "cdd-server-registers-off"

/// GLEIF's cross-check, switched on and off like a register.
[<Literal>]
let Gleif = "GLEIF"

/// One connection for the page. The browser follows redirects itself, which a desktop host's connection refuses.
let connection = lazy (SourceConnection(new HttpClientHandler()))

type Page =
    | CasePage
    | SettingsPage
    | AboutPage

/// Where cases and searches run.
type Mode =
    /// Until api/mode answers; the page lists the browser's registers meanwhile.
    | Detecting
    /// In this browser, against the keyless registers that allow it.
    | InBrowser
    /// Through the local server's API, which lists the registers.
    | OnServer

/// One register's answer to a name search, with the register as listed when it was asked.
type SearchRow =
    {
        Source: RegisterDto
        Row: SearchRowDto
    }

/// One register's answer to a person search, with the register as listed when it was asked.
type PersonRow =
    {
        Source: RegisterDto
        Row: PersonRowDto
    }

/// The person search's answer: a row per register that searches people, and the codes of those that don't.
type PeopleFound =
    {
        Rows: PersonRow list
        CantSearch: string list
    }

/// A link the page can be opened at: a hash route, so that it needs no server rewrite under GitHub Pages' sub-path.
type Route =
    /// #/company/<code>/<number>
    | CompanyRoute of code: string * number: string
    /// #/search/<term>, with ?match=exact or ?match=contains when not the default starts-with, and ?register=<code> for one register.
    | SearchRoute of term: string * matching: NameMatch * register: string voption
    /// #/person/<name>, with ?register=<code> for one register.
    | PersonRoute of name: string * register: string voption
    /// No hash.
    | NoRoute
    /// A hash of no shape above.
    | UnknownRoute of hash: string

module Route =

    /// The match a link's ?match= names; any other word is the default, starts-with.
    let matchOf (word: string) =
        match word.ToLowerInvariant() with
        | "exact" -> NameMatch.Exact
        | "contains" -> NameMatch.Contains
        | _ -> NameMatch.StartsWith

    /// The word a link carries for a match; none for the default.
    let matchWord (matching: NameMatch) =
        match matching with
        | NameMatch.Exact -> ValueSome "exact"
        | NameMatch.Contains -> ValueSome "contains"
        | NameMatch.StartsWith -> ValueNone

    /// The route a hash names. Each part is URL-decoded after splitting, so a number holding a slash travels encoded.
    let parse (hash: string) =
        let whole = (if isNull hash then "" else hash).TrimStart('#').TrimStart '/'

        let path, query =
            match whole.IndexOf '?' with
            | -1 -> whole, ""
            | at -> whole.Substring(0, at), whole.Substring(at + 1)

        let param (name: string) =
            query.Split('&', StringSplitOptions.RemoveEmptyEntries)
            |> Array.tryPick (fun pair ->
                match pair.Split('=', 2) with
                | [| key; value |] when key = name -> Some(Uri.UnescapeDataString value)
                | _ -> None)

        let matching =
            param "match" |> Option.map matchOf |> Option.defaultValue NameMatch.StartsWith

        let register =
            match param "register" with
            | Some code when code.Trim() <> "" -> ValueSome(code.Trim().ToUpperInvariant())
            | _ -> ValueNone

        match path.Split('/') |> Array.map Uri.UnescapeDataString with
        | [| "" |] -> NoRoute
        | [| "company"; code; number |] when code <> "" && number <> "" -> CompanyRoute(code.ToUpperInvariant(), number)
        | [| "search"; term |] when term <> "" -> SearchRoute(term, matching, register)
        | [| "person"; name |] when name <> "" -> PersonRoute(name, register)
        | _ -> UnknownRoute hash

    /// The query a link carries, from its pairs; none when there are none.
    let private queryOf (pairs: (string * string) list) =
        match pairs with
        | [] -> ""
        | _ ->
            "?"
            + (pairs
               |> List.map (fun (key, value) -> $"{key}={Uri.EscapeDataString value}")
               |> String.concat "&")

    let private registerPair (register: string voption) =
        register |> ValueOption.map (fun code -> "register", code) |> ValueOption.toList

    let hash route =
        match route with
        | CompanyRoute(code, number) -> $"#/company/{Uri.EscapeDataString code}/{Uri.EscapeDataString number}"
        | SearchRoute(term, matching, register) ->
            let pairs =
                (matchWord matching
                 |> ValueOption.map (fun word -> "match", word)
                 |> ValueOption.toList)
                @ registerPair register

            $"#/search/{Uri.EscapeDataString term}" + queryOf pairs
        | PersonRoute(name, register) -> $"#/person/{Uri.EscapeDataString name}" + queryOf (registerPair register)
        | NoRoute -> ""
        | UnknownRoute hash -> hash

/// What a resolved source's listing named when asked, without downloading.
type CurrentFiles =
    | Reading
    | Listed of BulkFileDto array
    | NotListed of reason: string

type Model =
    {
        Page: Page
        Mode: Mode
        Code: string
        Number: string
        /// The register picker open now: "lookup", "companies" or "people".
        OpenPicker: string voption
        Busy: bool
        Review: ReviewItemDto list
        Export: string voption
        Error: string voption
        Term: string
        Searching: bool
        Found: SearchRow list voption
        PersonTerm: string
        SearchingPeople: bool
        People: PeopleFound voption
        /// The result blocks showing every match, keyed by card and register code, such as "people:FR".
        ShowingAll: Set<string>
        /// The registers each search card leaves out, keyed by card, "companies" or "people". Every switched-on register is asked unless left out here.
        /// The one register a search card asks, by card; a card not here asks all.
        Chosen: Map<string, string>
        /// The Companies House JavaScript key kept in this browser, which is never shown. Browser mode only.
        CompaniesHouseKey: string voption
        /// What the key field holds before Save.
        KeyDraft: string
        /// The shareable hash of the case on screen, the last name search and the last person search.
        CaseLink: string voption
        SearchLink: string voption
        /// How a name search matches names: exact, starts with (the default) or contains.
        Matching: NameMatch
        PersonLink: string voption
        /// The "Copy link" button showing what its copy did, by its key: true when copied, false when the link went to the address bar.
        Copied: (string * bool) voption
        /// Set when a link opened a case in a register switched off in Settings.
        OffNote: string voption
        /// True once the hash the page opened at has been followed.
        OpeningLinkRead: bool
        /// The registers, and GLEIF, switched off on the settings screen. Cases and searches read every other one.
        Off: Set<string>
        /// The registers the server lists now. Server mode only.
        ServerRegisters: RegisterDto list
        /// The keyed registers and their settings, without values. Server mode only.
        Keys: KeyedRegisterDto list
        /// What each setting's field holds before Save, by setting name.
        KeyDrafts: Map<string, string>
        /// The bulk catalogue with what the server holds. Server mode only.
        Bulk: BulkDto list
        /// True while a bulk poll waits, so that only one runs.
        Polling: bool
        /// What each by-hand file field holds, by "<id>:<slot>".
        PathDrafts: Map<string, string>
        /// The resolved sources whose form for files already on the server is open.
        OwnFiles: Set<string>
        /// What each resolved source's listing named when asked, by id.
        Current: Map<string, CurrentFiles>
    }

type Message =
    | Go of Page
    | ModeRead of server: bool
    | TogglePicker of picker: string
    | Pick of code: string
    | Choose of card: string * code: string voption
    | SetNumber of string
    | OpenCase
    | CaseOpened of CaseDto
    | Failed of exn
    | SetTerm of string
    | SetMatching of NameMatch
    | Search
    | Searched of SearchRow list
    | SetPersonTerm of string
    | SearchPeople
    | PeopleSearched of PeopleFound
    | ToggleShowAll of key: string
    | FollowLink of hash: string
    | CopyLink of key: string * hash: string
    | LinkCopied of key: string * copied: bool
    | ClearCopied of key: string
    | LookUp of code: string * number: string
    | Switch of code: string * on: bool
    | EnabledRead of stored: string * companiesHouseKey: string
    | SetKeyDraft of string
    | SaveKey
    | RemoveKey
    | ServerRead of registers: RegisterDto list * storedOff: string
    | RegistersRead of RegisterDto list
    | KeysRead of KeyedRegisterDto list
    | SetSettingDraft of setting: string * value: string
    | SaveSetting of setting: string
    | RemoveSetting of setting: string
    | SettingsChanged
    | BulkRead of BulkDto list
    | Poll
    | Download of id: string
    | RemoveBulk of id: string
    | SetPathDraft of key: string * value: string
    | AddFiles of id: string
    | BulkChanged
    | ToggleOwnFiles of id: string
    | ShowCurrent of id: string
    | CurrentRead of id: string * CurrentFiles

/// Passes the browser's hashchange events to the program.
type HashListener(dispatch: Message -> unit) =
    [<JSInvokable>]
    member _.HashChanged(hash: string) = dispatch (FollowLink hash)

let listenForLinks (js: IJSRuntime) : Cmd<Message> =
    Cmd.ofEffect (fun dispatch ->
        js.InvokeVoidAsync("cddLinks.listen", DotNetObjectReference.Create(HashListener dispatch))
        |> ignore)

let readOpeningLink (js: IJSRuntime) =
    Cmd.OfTask.perform (fun () -> js.InvokeAsync<string>("cddLinks.hash").AsTask()) () FollowLink

/// Puts the route in the address bar without adding a history entry or firing hashchange.
let showLink (js: IJSRuntime) (route: Route) =
    Cmd.OfTask.attempt
        (fun () -> task { do! js.InvokeVoidAsync("cddLinks.replace", Route.hash route).AsTask() })
        ()
        Failed

/// Every code the browser page can switch.
let browserCodes =
    Set.ofList (Gleif :: CompaniesHouseCode :: (browserSources |> List.map _.Code))

/// The stored set: codes separated by commas. Nothing stored, or nothing readable, is every register.
let enabledFrom (stored: string) =
    match stored with
    | null
    | "" -> browserCodes
    | text ->
        text.Split(',', StringSplitOptions.RemoveEmptyEntries)
        |> Set.ofArray
        |> Set.intersect browserCodes

/// The registers switched on and the Companies House key, read together so that a link is followed knowing both.
let readEnabled (js: IJSRuntime) =
    Cmd.OfTask.perform
        (fun () ->
            task {
                let! enabled = js.InvokeAsync<string>("localStorage.getItem", EnabledKey).AsTask()

                let! key =
                    js.InvokeAsync<string>("localStorage.getItem", CompaniesHouseKeyStorage).AsTask()

                return enabled, key
            })
        ()
        EnabledRead

/// Companies House's registry for a key, made once per key, since it holds the client's pacing.
let companiesHouseFor =
    let made = Collections.Generic.Dictionary<string, ICompanyRegistry>()

    fun (key: string) ->
        match made.TryGetValue key with
        | true, registry -> registry
        | _ ->
            let registry =
                Sources.companiesHouse connection.Value (SourceKey key) :> ICompanyRegistry
            made[key] <- registry
            registry

/// The browser sources as the page lists them.
let browserRegisters =
    lazy
        (browserSources
         |> List.map (fun source ->
             registerOf
                 source.Code
                 source.Label
                 source.Region
                 source.Flag
                 Kind.Keyless
                 source.Code
                 source.Example
                 (source.Make connection.Value)))

let companiesHouseRegister (key: string) =
    let source = keyed |> List.find (fun source -> source.Code = CompaniesHouseCode)

    registerOf
        source.Code
        source.Label
        source.Region
        source.Flag
        Kind.Keyed
        source.Jurisdiction
        source.Example
        (companiesHouseFor key)

/// Every register the page can read: the server's list in server mode; otherwise Companies House first while a key is set, then the keyless ones.
let registersOf (model: Model) =
    match model.Mode, model.CompaniesHouseKey with
    | OnServer, _ -> model.ServerRegisters
    | _, ValueSome key -> companiesHouseRegister key :: browserRegisters.Value
    | _, ValueNone -> browserRegisters.Value

let isOn (model: Model) (code: string) = not (model.Off.Contains code)

let enabledSources (model: Model) =
    registersOf model |> List.filter (fun source -> isOn model source.Code)

/// Keeps the switches: in the browser as the set switched on, on the server as the set switched off.
let saveSwitches (js: IJSRuntime) (model: Model) =
    let key, value =
        match model.Mode with
        | OnServer -> ServerOffKey, String.Join(",", model.Off)
        | Detecting
        | InBrowser -> EnabledKey, String.Join(",", browserCodes - model.Off)

    Cmd.OfTask.attempt
        (fun () -> task { do! js.InvokeVoidAsync("localStorage.setItem", key, value).AsTask() })
        ()
        Failed

/// The one register a search card asks, if one is picked.
let chosenFor (model: Model) (card: string) = model.Chosen |> Map.tryFind card

/// The switched-on registers a search card offers in its picker: those that search people for the person search; for the name search every one in
/// the browser, and those that search names on the server.
let chipSources (model: Model) (card: string) =
    match card, model.Mode with
    | "people", _ -> enabledSources model |> List.filter _.SearchesPeople
    | _, OnServer -> enabledSources model |> List.filter _.SearchesCompanies
    | _ -> enabledSources model

/// The switched-on registers a search card asks: the one picked while it is offered, else all, and for the person search with all also those
/// that can't search people, which its answer lists by code.
let selectedSources (model: Model) (card: string) =
    let offered = chipSources model card

    match
        chosenFor model card
        |> Option.bind (fun code -> offered |> List.tryFind (fun source -> source.Code = code))
    with
    | Some one -> [ one ]
    | None when card = "people" -> enabledSources model
    | None -> offered

/// The register a search card shows picked: the one chosen while it is offered, else none for all.
let pickedFor (model: Model) (card: string) =
    chosenFor model card
    |> Option.bind (fun code -> chipSources model card |> List.tryFind (fun source -> source.Code = code))

let sourceOf (model: Model) (code: string) =
    registersOf model |> List.tryFind (fun source -> source.Code = code)

/// The register to show selected: the current one while it is switched on, else the first switched on.
let selecting (model: Model) =
    match enabledSources model with
    | sources when sources |> List.exists (fun source -> source.Code = model.Code) -> model
    | first :: _ ->
        { model with
            Code = first.Code
            Number = first.Example
        }
    | [] -> model

/// Where lookups and searches run: in this browser or on the local server. The page is the same for both.
type Backend =
    {
        /// The case for a register code and number, reading the registers of the codes in order, and GLEIF when the flag is true.
        Lookup: string -> string -> string list -> bool -> Task<CaseDto>
        /// A row per code for a name, matched as asked.
        Search: string -> NameMatch -> string list -> Task<SearchRowDto list>
        /// A row per code whose register searches people for a person's name; the other codes are listed as unable to.
        People: string -> string list -> Task<PeopleDto>
    }

/// The registry for a code in this browser: Companies House while a key is set, else a browser source.
let private browserRegistry (key: string voption) (code: string) =
    match key with
    | ValueSome key when code = CompaniesHouseCode -> ValueSome(companiesHouseFor key)
    | _ ->
        browserSources
        |> List.tryFind (fun source -> source.Code = code)
        |> Option.map (fun source -> source.Make connection.Value)
        |> ValueOption.ofOption

/// Runs the library here, each call going from the browser to the register.
let browserBackend (key: string voption) : Backend =
    let registries codes =
        codes
        |> List.choose (fun code ->
            browserRegistry key code
            |> ValueOption.map (fun registry -> code, registry)
            |> ValueOption.toOption)

    {
        Lookup =
            fun code number codes gleif ->
                task {
                    let sources: CaseSources =
                        {
                            Registries = registries codes |> List.map snd
                            Filings = []
                            CrossCheck =
                                if gleif then
                                    ValueSome(Sources.gleif connection.Value)
                                else
                                    ValueNone
                            Routes = CoverageRoutes.all
                            KnownAddresses = []
                        }

                    let start: CaseStart =
                        {
                            CaseId = "DEMO-" + DateTime.UtcNow.ToString "yyyyMMddHHmmss"
                            Number = number
                            Jurisdiction = Jurisdiction.ofCode code
                            NameGiven = "(as entered in the demo)"
                        }

                    let! case =
                        Investigation.openAsync sources InvestigationOptions.defaults start CancellationToken.None

                    return caseOf case
                }
        Search =
            fun term matching codes ->
                let ask (code: string, registry: ICompanyRegistry) =
                    task {
                        let! answer =
                            Registries.searchByNameAsync
                                registry
                                term
                                matching
                                (Jurisdiction.ofCode code)
                                CancellationToken.None

                        return searchRow code answer
                    }

                task {
                    let! rows = registries codes |> List.map ask |> Task.WhenAll
                    return List.ofArray rows
                }
        People =
            fun name codes ->
                let made = registries codes

                let searching =
                    made
                    |> List.choose (fun (code, registry) ->
                        match registry with
                        | :? IPersonSearch as search -> Some(code, search)
                        | _ -> None)

                let ask (code: string, search: IPersonSearch) =
                    task {
                        let! answer =
                            search.SearchPeopleAsync(name, Jurisdiction.ofCode code, CancellationToken.None)

                        return personRow code answer
                    }

                task {
                    let! rows = searching |> List.map ask |> Task.WhenAll

                    return
                        {
                            PeopleDto.Rows = rows
                            CantSearch =
                                made
                                |> List.filter (fun (_, registry) -> not (registry :? IPersonSearch))
                                |> List.map fst
                                |> Array.ofList
                        }
                }
    }

let private webJson =
    Text.Json.JsonSerializerOptions(Text.Json.JsonSerializerDefaults.Web)

/// The error a failed API answer states, or its status.
let private failureOf (response: HttpResponseMessage) =
    task {
        let! body = response.Content.ReadAsStringAsync()

        let stated =
            try
                Text.Json.JsonSerializer.Deserialize<ErrorDto>(body, webJson)
                |> ValueOption.ofObj
                |> ValueOption.bind (_.Error >> ValueOption.ofObj)
            with _ ->
                ValueNone

        return
            stated
            |> ValueOption.defaultValue $"The server answered {int response.StatusCode}"
    }

/// GETs an API answer; a failure raises its stated error.
let getJson<'T> (http: HttpClient) (path: string) : Task<'T> =
    task {
        use! response = http.GetAsync path

        if response.IsSuccessStatusCode then
            return! response.Content.ReadFromJsonAsync<'T>(webJson)
        else
            let! failure = failureOf response
            return failwith failure
    }

/// Sends a request whose answer isn't read; a failure raises its stated error.
let send (http: HttpClient) (request: HttpRequestMessage) =
    task {
        use request = request
        use! response = http.SendAsync request

        if not response.IsSuccessStatusCode then
            let! failure = failureOf response
            failwith failure
    }

let private escaped (text: string) = Uri.EscapeDataString text

let private codesQuery (codes: string list) =
    codes |> List.map escaped |> String.concat ","

/// Runs everything on the local server through its API.
let serverBackend (http: HttpClient) : Backend =
    {
        Lookup =
            fun code number codes gleif ->
                let withGleif = if gleif then "true" else "false"

                getJson<CaseDto>
                    http
                    $"api/lookup/{escaped code}/{escaped number}?codes={codesQuery codes}&gleif={withGleif}"
        Search =
            fun term matching codes ->
                let word = Route.matchWord matching |> ValueOption.defaultValue "startswith"

                task {
                    let! rows =
                        getJson<SearchRowDto array>
                            http
                            $"api/search?term={escaped term}&match={word}&codes={codesQuery codes}"

                    return List.ofArray rows
                }
        People = fun name codes -> getJson<PeopleDto> http $"api/people?name={escaped name}&codes={codesQuery codes}"
    }

let backendOf (http: HttpClient) (model: Model) =
    match model.Mode with
    | OnServer -> serverBackend http
    | Detecting
    | InBrowser -> browserBackend model.CompaniesHouseKey

/// True when api/mode answers as the local server. On GitHub Pages it is a 404, and a development server answers the page instead.
let detectMode (http: HttpClient) =
    let ask () =
        task {
            try
                use! response = http.GetAsync "api/mode"

                let json =
                    not (isNull response.Content.Headers.ContentType)
                    && response.Content.Headers.ContentType.MediaType = "application/json"

                if response.IsSuccessStatusCode && json then
                    let! mode = response.Content.ReadFromJsonAsync<ModeDto>(webJson)
                    return not (isNull (box mode)) && mode.Server
                else
                    return false
            with _ ->
                return false
        }

    Cmd.OfTask.perform ask () ModeRead

let readServer (js: IJSRuntime) (http: HttpClient) =
    Cmd.OfTask.either
        (fun () ->
            task {
                let! registers = getJson<RegisterDto array> http "api/registers"
                let! off = js.InvokeAsync<string>("localStorage.getItem", ServerOffKey).AsTask()
                return List.ofArray registers, off
            })
        ()
        ServerRead
        Failed

let loadRegisters (http: HttpClient) =
    Cmd.OfTask.either
        (fun () -> getJson<RegisterDto array> http "api/registers")
        ()
        (List.ofArray >> RegistersRead)
        Failed

let loadKeys (http: HttpClient) =
    Cmd.OfTask.either (fun () -> getJson<KeyedRegisterDto array> http "api/keys") () (List.ofArray >> KeysRead) Failed

let loadBulk (http: HttpClient) =
    Cmd.OfTask.either (fun () -> getJson<BulkDto array> http "api/bulk") () (List.ofArray >> BulkRead) Failed

let pollLater =
    Cmd.OfTask.perform (fun () -> task { do! Task.Delay(TimeSpan.FromSeconds 1.) }) () (fun () -> Poll)

let sendThen (http: HttpClient) (request: unit -> HttpRequestMessage) (next: Message) =
    Cmd.OfTask.either (fun () -> send http (request ())) () (fun () -> next) Failed

/// The ids of the bulk sources downloading or building.
let running (items: BulkDto list) =
    items
    |> List.filter (fun item -> item.State = BulkState.Downloading || item.State = BulkState.Indexing)
    |> List.map _.Id
    |> Set.ofList

/// The paths typed for a slot: one per line for a slot taking several, each without the quotes a copied path carries.
let pathsOf (text: string) =
    (if isNull text then "" else text).Split([| '\r'; '\n' |], StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun path -> path.Trim().Trim '"')
    |> Array.filter (fun path -> path <> "")

/// A row's register as listed now, or one named by its code alone.
let registerFor (registers: RegisterDto list) (code: string) =
    registers
    |> List.tryFind (fun register -> register.Code = code)
    |> Option.defaultValue
        {
            RegisterDto.Code = code
            Label = code
            Region = ""
            Flag = ""
            Kind = ""
            Jurisdiction = code
            Example = ""
            SearchesCompanies = false
            SearchesPeople = false
        }

/// The exception a task's failure wraps, whose message is the one to show.
[<TailCall>]
let rec innermost (error: exn) =
    match error with
    | :? AggregateException as wrapped when wrapped.InnerExceptions.Count = 1 -> innermost wrapped.InnerExceptions[0]
    | error -> error

/// The cards' choices with a link's register, or none for all, set for the card.
let choosing (model: Model) (card: string) (register: string voption) =
    match register with
    | ValueSome code -> model.Chosen.Add(card, code)
    | ValueNone -> model.Chosen.Remove card

let init (js: IJSRuntime) (http: HttpClient) =
    {
        Page = CasePage
        Mode = Detecting
        Code = "NO"
        Number = ""
        OpenPicker = ValueNone
        Busy = false
        Review = []
        Export = ValueNone
        Error = ValueNone
        Term = ""
        Matching = NameMatch.StartsWith
        Searching = false
        Found = ValueNone
        PersonTerm = ""
        SearchingPeople = false
        People = ValueNone
        ShowingAll = Set.empty
        Chosen = Map.empty
        CompaniesHouseKey = ValueNone
        KeyDraft = ""
        CaseLink = ValueNone
        SearchLink = ValueNone
        PersonLink = ValueNone
        Copied = ValueNone
        OffNote = ValueNone
        OpeningLinkRead = false
        Off = Set.empty
        ServerRegisters = []
        Keys = []
        KeyDrafts = Map.empty
        Bulk = []
        Polling = false
        PathDrafts = Map.empty
        OwnFiles = Set.empty
        Current = Map.empty
    },
    Cmd.batch [ detectMode http; listenForLinks js ]

/// The registers a case reads: the one picked first, then every other one switched on.
let lookupCodes (model: Model) =
    model.Code
    :: (enabledSources model
        |> List.map _.Code
        |> List.filter (fun code -> code <> model.Code))

let update (js: IJSRuntime) (http: HttpClient) message (model: Model) =
    match message with
    | Go page ->
        { model with
            Page = page
            OpenPicker = ValueNone
        },
        Cmd.none
    | ModeRead true -> { model with Mode = OnServer }, Cmd.batch [ readServer js http; loadKeys http; loadBulk http ]
    | ModeRead false -> { model with Mode = InBrowser }, readEnabled js
    | TogglePicker picker ->
        { model with
            OpenPicker =
                if model.OpenPicker = ValueSome picker then
                    ValueNone
                else
                    ValueSome picker
        },
        Cmd.none
    | Pick code ->
        { model with
            Code = code
            OpenPicker = ValueNone
        },
        Cmd.none
    | Choose(card, code) ->
        { model with
            Chosen =
                match code with
                | ValueSome code -> model.Chosen.Add(card, code)
                | ValueNone -> model.Chosen.Remove card
            OpenPicker = ValueNone
        },
        Cmd.none
    | SetNumber number -> { model with Number = number }, Cmd.none
    | OpenCase ->
        let route = CompanyRoute(model.Code, model.Number.Trim())
        let backend = backendOf http model

        { model with
            Busy = true
            Error = ValueNone
            Export = ValueNone
            Review = []
            OpenPicker = ValueNone
            CaseLink = ValueSome(Route.hash route)
            OffNote =
                if isOn model model.Code then
                    ValueNone
                else
                    ValueSome $"{model.Code} is switched off in your settings; this link looks it up all the same."
        },
        Cmd.batch
            [
                Cmd.OfTask.either
                    (fun () -> backend.Lookup model.Code (model.Number.Trim()) (lookupCodes model) (isOn model Gleif))
                    ()
                    CaseOpened
                    Failed
                showLink js route
            ]
    | CaseOpened case ->
        { model with
            Busy = false
            Review = List.ofArray case.Review
            Export = ValueSome case.Export
        },
        Cmd.none
    | Failed error ->
        { model with
            Busy = false
            Searching = false
            SearchingPeople = false
            Error = ValueSome (innermost error).Message
        },
        Cmd.none
    | SetTerm term -> { model with Term = term }, Cmd.none
    | SetMatching matching -> { model with Matching = matching }, Cmd.none
    | Search when String.IsNullOrWhiteSpace model.Term -> model, Cmd.none
    | Search ->
        let route =
            SearchRoute(
                model.Term.Trim(),
                model.Matching,
                pickedFor model "companies" |> Option.map _.Code |> ValueOption.ofOption
            )

        let backend = backendOf http model
        let registers = registersOf model
        let codes = selectedSources model "companies" |> List.map _.Code

        let ask () =
            task {
                let! rows = backend.Search (model.Term.Trim()) model.Matching codes

                return
                    rows
                    |> List.map (fun row ->
                        {
                            SearchRow.Source = registerFor registers row.Code
                            Row = row
                        })
            }

        { model with
            Searching = true
            Found = ValueNone
            Error = ValueNone
            SearchLink = ValueSome(Route.hash route)
        },
        Cmd.batch [ Cmd.OfTask.either ask () Searched Failed; showLink js route ]
    | Searched rows ->
        { model with
            Searching = false
            Found = ValueSome rows
            ShowingAll = Set.empty
        },
        Cmd.none
    | SetPersonTerm term -> { model with PersonTerm = term }, Cmd.none
    | SearchPeople when PersonName.tryParse model.PersonTerm |> ValueOption.isNone -> model, Cmd.none
    | SearchPeople ->
        let route =
            PersonRoute(model.PersonTerm.Trim(), pickedFor model "people" |> Option.map _.Code |> ValueOption.ofOption)

        let backend = backendOf http model
        let registers = registersOf model
        let codes = selectedSources model "people" |> List.map _.Code

        let ask () =
            task {
                let! found = backend.People (model.PersonTerm.Trim()) codes

                return
                    {
                        Rows =
                            found.Rows
                            |> Array.map (fun row ->
                                {
                                    PersonRow.Source = registerFor registers row.Code
                                    Row = row
                                })
                            |> List.ofArray
                        CantSearch = List.ofArray found.CantSearch
                    }
            }

        { model with
            SearchingPeople = true
            People = ValueNone
            Error = ValueNone
            PersonLink = ValueSome(Route.hash route)
        },
        Cmd.batch [ Cmd.OfTask.either ask () PeopleSearched Failed; showLink js route ]
    | PeopleSearched found ->
        { model with
            SearchingPeople = false
            People = ValueSome found
            ShowingAll = Set.empty
        },
        Cmd.none
    | ToggleShowAll key ->
        let showing =
            if model.ShowingAll.Contains key then
                model.ShowingAll.Remove key
            else
                model.ShowingAll.Add key

        { model with ShowingAll = showing }, Cmd.none
    | LookUp(code, number) ->
        { model with
            Code = code
            Number = number
        },
        Cmd.ofMsg OpenCase
    | Switch(code, on) ->
        let switched =
            selecting
                { model with
                    Off = (if on then model.Off.Remove code else model.Off.Add code)
                }

        switched, saveSwitches js switched
    | EnabledRead(stored, key) ->
        // The opening link is followed once the settings are read, so that a register switched off is known as such.
        selecting
            { model with
                Off = browserCodes - enabledFrom stored
                CompaniesHouseKey =
                    if String.IsNullOrWhiteSpace key then
                        ValueNone
                    else
                        ValueSome(key.Trim())
                OpeningLinkRead = true
            },
        (if model.OpeningLinkRead then
             Cmd.none
         else
             readOpeningLink js)
    | ServerRead(registers, storedOff) ->
        let off =
            match storedOff with
            | null -> Set.empty
            | text -> text.Split(',', StringSplitOptions.RemoveEmptyEntries) |> Set.ofArray

        selecting
            { model with
                ServerRegisters = registers
                Off = off
                OpeningLinkRead = true
            },
        (if model.OpeningLinkRead then
             Cmd.none
         else
             readOpeningLink js)
    | RegistersRead registers ->
        selecting
            { model with
                ServerRegisters = registers
            },
        Cmd.none
    | KeysRead keys -> { model with Keys = keys }, Cmd.none
    | SetSettingDraft(setting, value) ->
        { model with
            KeyDrafts = model.KeyDrafts.Add(setting, value)
        },
        Cmd.none
    | SaveSetting setting ->
        match model.KeyDrafts.TryFind setting |> Option.map _.Trim() with
        | Some value when value <> "" ->
            let request () =
                new HttpRequestMessage(
                    HttpMethod.Put,
                    $"api/keys/{escaped setting}",
                    Content = JsonContent.Create { Value = value }
                )

            { model with
                KeyDrafts = model.KeyDrafts.Remove setting
            },
            sendThen http request SettingsChanged
        | _ -> model, Cmd.none
    | RemoveSetting setting ->
        model,
        sendThen
            http
            (fun () -> new HttpRequestMessage(HttpMethod.Delete, $"api/keys/{escaped setting}"))
            SettingsChanged
    | SettingsChanged -> model, Cmd.batch [ loadKeys http; loadRegisters http ]
    | BulkRead items ->
        let isRunning = running items
        let finished = not (Set.isEmpty (running model.Bulk - isRunning))
        let poll = not (isRunning.IsEmpty || model.Polling)

        { model with
            Bulk = items
            Polling = model.Polling || poll
        },
        Cmd.batch
            [
                (if poll then pollLater else Cmd.none)
                (if finished then loadRegisters http else Cmd.none)
            ]
    | Poll -> { model with Polling = false }, loadBulk http
    | Download pid ->
        model,
        sendThen http (fun () -> new HttpRequestMessage(HttpMethod.Post, $"api/bulk/{escaped pid}/download")) BulkChanged
    | RemoveBulk pid ->
        model, sendThen http (fun () -> new HttpRequestMessage(HttpMethod.Delete, $"api/bulk/{escaped pid}")) BulkChanged
    | SetPathDraft(key, value) ->
        { model with
            PathDrafts = model.PathDrafts.Add(key, value)
        },
        Cmd.none
    | AddFiles id ->
        let slots =
            model.Bulk
            |> List.tryFind (fun item -> item.Id = id)
            |> Option.map _.Slots
            |> Option.defaultValue [||]

        let paths =
            slots
            |> Array.choose (fun slot ->
                match model.PathDrafts.TryFind $"{id}:{slot.Name}" |> Option.map pathsOf with
                | Some given when given.Length > 0 -> Some(Collections.Generic.KeyValuePair(slot.Name, given))
                | _ -> None)
            |> Collections.Generic.Dictionary

        let request () =
            new HttpRequestMessage(
                HttpMethod.Post,
                $"api/bulk/{escaped id}/files",
                Content = JsonContent.Create { Paths = paths }
            )

        model, sendThen http request BulkChanged
    | BulkChanged -> model, Cmd.batch [ loadBulk http; loadRegisters http ]
    | ToggleOwnFiles id ->
        { model with
            OwnFiles =
                if model.OwnFiles.Contains id then
                    model.OwnFiles.Remove id
                else
                    model.OwnFiles.Add id
        },
        Cmd.none
    | ShowCurrent id ->
        { model with
            Current = model.Current.Add(id, Reading)
        },
        Cmd.OfTask.either
            (fun () -> getJson<BulkFileDto array> http $"api/bulk/{escaped id}/current")
            ()
            (fun files -> CurrentRead(id, Listed files))
            (fun error -> CurrentRead(id, NotListed error.Message))
    | CurrentRead(pid, files) ->
        { model with
            Current = model.Current.Add(pid, files)
        },
        Cmd.none
    | FollowLink hash ->
        match Route.parse hash with
        | NoRoute -> model, Cmd.none
        | UnknownRoute _ ->
            { model with
                Page = CasePage
                Error = ValueSome $"The link {hash} isn't one this demo reads."
            },
            Cmd.none
        | CompanyRoute(code, number) ->
            match sourceOf model code with
            | Some _ ->
                { model with
                    Page = CasePage
                    Code = code
                    Number = number
                },
                Cmd.ofMsg OpenCase
            | None when model.Mode = OnServer ->
                { model with
                    Page = CasePage
                    Error =
                        ValueSome
                            $"The link names the register {code}, which the server doesn't list now: download it or set its key in Settings."
                },
                Cmd.none
            | None when code = CompaniesHouseCode ->
                { model with
                    Page = CasePage
                    Error = ValueSome "The link is for Companies House, which needs a JavaScript key set in Settings."
                },
                Cmd.none
            | None ->
                { model with
                    Page = CasePage
                    Error = ValueSome $"The link names the register {code}, which this demo doesn't read."
                },
                Cmd.none
        | SearchRoute(term, matching, register) ->
            { model with
                Page = CasePage
                Term = term
                Matching = matching
                Chosen = choosing model "companies" register
            },
            Cmd.ofMsg Search
        | PersonRoute(name, register) ->
            { model with
                Page = CasePage
                PersonTerm = name
                Chosen = choosing model "people" register
            },
            Cmd.ofMsg SearchPeople
    | SetKeyDraft draft -> { model with KeyDraft = draft }, Cmd.none
    | SaveKey when String.IsNullOrWhiteSpace model.KeyDraft -> model, Cmd.none
    | SaveKey ->
        let key = model.KeyDraft.Trim()

        { model with
            CompaniesHouseKey = ValueSome key
            KeyDraft = ""
        },
        Cmd.OfTask.attempt
            (fun () -> task { do! js.InvokeVoidAsync("localStorage.setItem", CompaniesHouseKeyStorage, key).AsTask() })
            ()
            Failed
    | RemoveKey ->
        selecting
            { model with
                CompaniesHouseKey = ValueNone
                KeyDraft = ""
            },
        Cmd.OfTask.attempt
            (fun () -> task { do! js.InvokeVoidAsync("localStorage.removeItem", CompaniesHouseKeyStorage).AsTask() })
            ()
            Failed
    | CopyLink(key, hash) ->
        model,
        Cmd.OfTask.perform (fun () -> js.InvokeAsync<bool>("cddLinks.copy", hash).AsTask()) () (fun copied ->
            LinkCopied(key, copied))
    | LinkCopied(key, copied) ->
        { model with
            Copied = ValueSome(key, copied)
        },
        Cmd.OfTask.perform (fun () -> task { do! Task.Delay(TimeSpan.FromSeconds 2.) }) () (fun () -> ClearCopied key)
    | ClearCopied key when model.Copied |> ValueOption.exists (fun (copied, _) -> copied = key) ->
        { model with Copied = ValueNone }, Cmd.none
    | ClearCopied _ -> model, Cmd.none

let flagOf (flag: string) =
    match flag with
    | "" -> empty ()
    | flag ->
        img {
            attr.src $"flags/1x1/{flag}.svg"
            attr.alt ""
            attr.``class`` "flag"
        }

/// A button to the issuer's page for a key, opened in a new tab, with what getting one takes.
let keyLink (page: string) (how: string) =
    span {
        attr.``class`` "key-link"

        a {
            attr.``class`` "small button"
            attr.href page
            attr.target "_blank"
            attr.rel "noopener noreferrer"
            "Get a key"
        }

        span {
            attr.``class`` "muted"
            $" {how}"
        }
    }

let navButton (model: Model) dispatch page (label: string) =
    button {
        attr.``class`` (if model.Page = page then "active" else "")
        on.click (fun _ -> dispatch (Go page))
        label
    }

/// A register picker: the sources under their regions, each with its flag, and with all set, "All registers" first. A native select can't
/// show images, so this is a button and a list. current is the source picked, none for all.
let registerPicker
    (model: Model)
    dispatch
    (key: string)
    (sources: RegisterDto list)
    (current: RegisterDto option)
    (all: bool)
    (choose: RegisterDto voption -> Message)
    =
    div {
        attr.``class`` "picker"

        button {
            attr.``class`` "picker-button"
            attr.``type`` "button"
            on.click (fun _ -> dispatch (TogglePicker key))

            match sources, current with
            | [], _ -> span { "Switch a register on in Settings" }
            | _, Some current ->
                concat {
                    flagOf current.Flag
                    span { current.Label }
                }
            | _, None -> span { $"All registers ({sources.Length})" }

            span {
                attr.``class`` "caret"
                "▾"
            }
        }

        if model.OpenPicker = ValueSome key && not sources.IsEmpty then
            div {
                attr.``class`` "picker-list"

                if all then
                    button {
                        attr.``type`` "button"

                        attr.``class`` (
                            if current.IsNone then
                                "picker-item chosen"
                            else
                                "picker-item"
                        )

                        on.click (fun _ -> dispatch (choose ValueNone))
                        span { $"All registers ({sources.Length})" }
                    }
                else
                    empty ()

                forEach regions (fun region ->
                    match sources |> List.filter (fun source -> source.Region = region) with
                    | [] -> empty ()
                    | inRegion ->
                        concat {
                            div {
                                attr.``class`` "picker-group"
                                region
                            }

                            forEach inRegion (fun source ->
                                button {
                                    attr.``type`` "button"

                                    attr.``class`` (
                                        if current |> Option.exists (fun picked -> picked.Code = source.Code) then
                                            "picker-item chosen"
                                        else
                                            "picker-item"
                                    )

                                    on.click (fun _ -> dispatch (choose (ValueSome source)))
                                    flagOf source.Flag
                                    span { source.Label }
                                })
                        })
            }
        else
            empty ()
    }

/// The lookup's picker: one register, the switched-on ones.
let picker (model: Model) dispatch =
    registerPicker
        model
        dispatch
        "lookup"
        (enabledSources model)
        (sourceOf model model.Code
         |> Option.filter (fun source -> isOn model source.Code))
        false
        (fun source ->
            match source with
            | ValueSome source -> Pick source.Code
            | ValueNone -> Pick model.Code)

/// A search card's picker: all the registers it offers, or one.
let searchPicker (model: Model) dispatch (card: string) =
    registerPicker model dispatch card (chipSources model card) (pickedFor model card) true (fun source ->
        Choose(card, source |> ValueOption.map _.Code))

/// The words of Finland's trade register status codes (REK_KDI), as Finland.fs maps them: YTJ's answer holds the code alone.
let finnishStatus (code: string) =
    match code with
    | "0" -> "Unregistered"
    | "1" -> "Registered"
    | "2" -> "Removed from register"
    | "3" -> "Start-up not registered"
    | "4" -> "Ceased"
    | other -> other

/// Rows shown per register until its "Show all" is clicked.
[<Literal>]
let RowsShown = 5

/// One register's block in a results table: its header summary and items, or why it gave none.
type ResultBlock<'T> =
    {
        Source: RegisterDto
        Outcome: Result<string * 'T list, string>
    }

/// Results grouped by register: those with matches first, each under a header and cut to RowsShown with its own toggle; then those that gave a
/// reason, a header each; then one muted line of the codes with no match.
let groupedResults
    (model: Model)
    dispatch
    (card: string)
    (columns: int)
    (cells: RegisterDto -> 'T -> Node)
    (blocks: ResultBlock<'T> list)
    =
    let header (source: RegisterDto) (summary: string) =
        tr {
            attr.``class`` "register"

            td {
                attr.colspan columns
                flagOf source.Flag
                strong { source.Label }

                span {
                    attr.``class`` "muted"
                    " · " + summary
                }
            }
        }

    let withMatches =
        blocks
        |> List.choose (fun block ->
            match block.Outcome with
            | Ok(summary, items) when not items.IsEmpty -> Some(block.Source, summary, items)
            | _ -> None)

    let reasons =
        blocks
        |> List.choose (fun block ->
            match block.Outcome with
            | Error reason -> Some(block.Source, reason)
            | Ok _ -> None)

    let noMatch =
        blocks
        |> List.choose (fun block ->
            match block.Outcome with
            | Ok(_, []) -> Some block.Source.Code
            | _ -> None)

    concat {
        table {
            attr.``class`` "results"

            tbody {
                forEach withMatches (fun (source, summary, items) ->
                    let key = $"{card}:{source.Code}"
                    let showingAll = model.ShowingAll.Contains key

                    concat {
                        header source summary

                        forEach (if showingAll then items else List.truncate RowsShown items) (fun item ->
                            tr { cells source item })

                        if items.Length > RowsShown then
                            tr {
                                td {
                                    attr.colspan columns

                                    button {
                                        attr.``class`` "small"
                                        on.click (fun _ -> dispatch (ToggleShowAll key))

                                        text (
                                            if showingAll then
                                                $"Show the first {RowsShown}"
                                            else
                                                $"Show all {items.Length}"
                                        )
                                    }
                                }
                            }
                        else
                            empty ()
                    })

                forEach reasons (fun (source, reason) -> header source reason)
            }
        }

        match noMatch with
        | [] -> empty ()
        | codes ->
            p {
                attr.``class`` "muted compact"
                "No match: " + String.Join(", ", codes)
            }
    }

let private matchCount (count: int) =
    if count = 1 then "1 match" else $"{count} matches"

/// A row's matches with its summary, or the reason it gave none.
let private outcomeOf (reason: string) (summary: string) (items: 'T array) =
    match reason with
    | null -> Ok(summary, List.ofArray items)
    | reason -> Error reason

/// True when the card has a register to ask.
let anySelected (model: Model) (card: string) = not (chipSources model card).IsEmpty

/// Copies the hash's link; "Copied" for a moment after.
let copyButton (model: Model) dispatch (key: string) (hash: string voption) =
    match hash with
    | ValueSome hash ->
        button {
            attr.``type`` "button"
            attr.``class`` "small copy"
            on.click (fun _ -> dispatch (CopyLink(key, hash)))

            text (
                match model.Copied with
                | ValueSome(copied, true) when copied = key -> "Copied"
                | ValueSome(copied, false) when copied = key -> "The link is in the address bar"
                | _ -> "Copy link"
            )
        }
    | ValueNone -> empty ()

let lookUpCell dispatch (source: RegisterDto) (number: string voption) =
    td {
        match number with
        | ValueSome number ->
            button {
                attr.``class`` "small"
                on.click (fun _ -> dispatch (LookUp(source.Code, number)))
                "Look up"
            }
        | ValueNone -> empty ()
    }

let searchCard (model: Model) dispatch =
    div {
        attr.``class`` "card"
        h2 { "Find a company by name" }

        p {
            attr.``class`` "muted"
            "Asks every register switched on at once, or the one picked; each answers from its own search, live. A register shows at most its first page, and Poland and Costa Rica search by number only."
        }

        div {
            attr.``class`` "row"

            label {
                "Name"

                input {
                    bind.input.string model.Term (SetTerm >> dispatch)

                    on.keydown (fun event ->
                        if event.Key = "Enter" then
                            dispatch Search)
                }
            }

            label {
                "Match"

                select {
                    attr.``class`` "matching"

                    bind.change.string
                        (Route.matchWord model.Matching |> ValueOption.defaultValue "startswith")
                        (Route.matchOf >> SetMatching >> dispatch)

                    option {
                        attr.value "exact"
                        "Exact name"
                    }

                    option {
                        attr.value "startswith"
                        "Starts with"
                    }

                    option {
                        attr.value "contains"
                        "Contains"
                    }
                }
            }

            label {
                "Registers"
                searchPicker model dispatch "companies"
            }

            button {
                attr.``class`` "primary"
                attr.disabled (model.Searching || not (anySelected model "companies"))
                on.click (fun _ -> dispatch Search)
                text (if model.Searching then "Searching…" else "Search")
            }
        }


        match model.Found with
        | ValueNone -> empty ()
        | ValueSome rows ->
            concat {
                div {
                    attr.``class`` "links"
                    copyButton model dispatch "search" model.SearchLink
                }

                rows
                |> List.map (fun found ->
                    let row = found.Row
                    let shown = matchCount row.Matches.Length

                    let summary =
                        if row.TotalMatches.HasValue && row.TotalMatches.Value > row.Matches.Length then
                            $"{shown} of {row.TotalMatches.Value}"
                        else
                            shown

                    {
                        Source = found.Source
                        Outcome = outcomeOf row.Reason summary row.Matches
                    })
                |> groupedResults model dispatch "companies" 4 (fun source (hit: CompanyHitDto) ->
                    concat {
                        td { hit.Name }
                        td { hit.Number }

                        td {
                            if source.Code = "FI" then
                                finnishStatus hit.Status
                            else
                                hit.Status
                        }

                        lookUpCell dispatch source (ValueSome hit.Number)
                    })
            }
    }

let personCard (model: Model) dispatch =
    div {
        attr.``class`` "card"
        h2 { "Find a person" }

        p {
            attr.``class`` "muted"
            "Is this person a director, officer or owner of a company? Asks the registers that publish people and let them be searched, all at once or the one picked. A register holding the name doesn't say it is the person meant, and no match never says the person holds no role. Type Firstname Lastname, or Lastname, Firstname."
        }

        div {
            attr.``class`` "row"

            label {
                "Name"

                input {
                    bind.input.string model.PersonTerm (SetPersonTerm >> dispatch)

                    on.keydown (fun event ->
                        if event.Key = "Enter" then
                            dispatch SearchPeople)
                }
            }

            label {
                "Registers"
                searchPicker model dispatch "people"
            }

            button {
                attr.``class`` "primary"
                attr.disabled (model.SearchingPeople || not (anySelected model "people"))
                on.click (fun _ -> dispatch SearchPeople)
                text (if model.SearchingPeople then "Searching…" else "Search")
            }
        }


        match model.People with
        | ValueNone -> empty ()
        | ValueSome found ->
            concat {
                div {
                    attr.``class`` "links"
                    copyButton model dispatch "person" model.PersonLink
                }

                found.Rows
                |> List.map (fun found ->
                    let row = found.Row
                    let shown = matchCount row.Matches.Length

                    {
                        Source = found.Source
                        Outcome =
                            outcomeOf
                                row.Reason
                                (if row.Complete then shown else shown + "; more may be held")
                                row.Matches
                    })
                |> groupedResults model dispatch "people" 6 (fun source (hit: PersonHitDto) ->
                    let companyName = ValueOption.ofObj hit.CompanyName
                    let companyNumber = ValueOption.ofObj hit.CompanyNumber

                    concat {
                        td { hit.Name }
                        td { hit.Role |> ValueOption.ofObj |> ValueOption.defaultValue "" }

                        td {
                            match ValueOption.ofNullable hit.Current with
                            | ValueSome true -> text "current"
                            | ValueSome false -> text "former"
                            | ValueNone ->
                                span {
                                    attr.``class`` "muted"
                                    "not stated"
                                }
                        }

                        td {
                            match companyName, companyNumber with
                            | ValueSome name, _ -> text name
                            | ValueNone, ValueSome _ -> empty ()
                            | ValueNone, ValueNone ->
                                span {
                                    attr.``class`` "muted"
                                    "Company not in the register's company list yet"
                                }

                            match ValueOption.ofObj hit.CompanyStatus with
                            | ValueSome status ->
                                span {
                                    attr.``class`` "muted"
                                    $" ({status})"
                                }
                            | ValueNone -> empty ()
                        }

                        td { companyNumber |> ValueOption.defaultValue "" }
                        lookUpCell dispatch source companyNumber
                    })

                match found.CantSearch with
                | [] -> empty ()
                | codes ->
                    p {
                        attr.``class`` "muted compact"
                        "Can't search people: " + String.Join(", ", codes)
                    }
            }

        p {
            attr.``class`` "muted"
            "PEP status isn't in any free official register; it comes from a vendor's screening, such as Plaid Monitor or Experian, added to a case as a VendorCheck."
        }
    }

let lookupCard (model: Model) dispatch =
    div {
        attr.``class`` "card"
        h2 { "Look up a company by its number" }

        p {
            attr.``class`` "muted"
            "Reads the company's record from its register: name, status, addresses, and officers and owners where the register publishes them. Then it follows owners and home records into the other registers switched on, cross-checks GLEIF, and lists what an officer needs to review. It never says clear."
        }

        div {
            attr.``class`` "row"

            label {
                "Register"
                picker model dispatch
            }

            label {
                "Registration number"

                input {
                    attr.placeholder (
                        match sourceOf model model.Code with
                        | Some source when source.Example <> "" -> $"such as {source.Example}"
                        | _ -> ""
                    )

                    bind.input.string model.Number (SetNumber >> dispatch)
                }
            }

            button {
                attr.``class`` "primary"
                attr.disabled (model.Busy || List.isEmpty (enabledSources model))
                on.click (fun _ -> dispatch OpenCase)
                text (if model.Busy then "Reading…" else "Look up")
            }
        }
    }

let errorCard (model: Model) =
    match model.Error with
    | ValueSome error ->
        div {
            attr.``class`` "card bad"
            error
        }
    | ValueNone -> empty ()

let casePage (model: Model) dispatch =
    concat {
        searchCard model dispatch
        personCard model dispatch
        lookupCard model dispatch

        match model.OffNote with
        | ValueSome note ->
            p {
                attr.``class`` "muted compact"
                note
            }
        | ValueNone -> empty ()

        errorCard model

        match model.Export with
        | ValueSome export ->
            concat {
                div {
                    attr.``class`` "card"

                    h3 {
                        $"To review ({model.Review.Length}) "
                        copyButton model dispatch "case" model.CaseLink
                    }

                    ul {
                        attr.``class`` "review"
                        forEach model.Review (fun item -> li { item.Text })
                    }
                }

                div {
                    attr.``class`` "card"
                    h3 { "Case file" }
                    pre { export }
                }
            }
        | ValueNone -> empty ()
    }

let megabytes (bytes: int64) = $"{float bytes / 1_000_000.:F0} MB"

let switchRow (model: Model) dispatch (code: string) (flag: string voption) (label': string) =
    li {
        label {
            attr.``class`` "switch"

            input {
                attr.``type`` "checkbox"
                bind.``checked`` (isOn model code) (fun on -> dispatch (Switch(code, on)))
            }

            match flag with
            | ValueSome flag -> flagOf flag
            | ValueNone ->
                span {
                    attr.``class`` "flag globe"
                    "🌐"
                }

            $"{label'} ({code})"
        }
    }

let keysCard (model: Model) dispatch =
    div {
        attr.``class`` "card"
        h2 { "Keys" }

        p {
            attr.``class`` "muted"
            "Get a Companies House key from the Developer Hub at https://developer.company-information.service.gov.uk/: create an application in the live environment, then a key of type JavaScript, with this site's address (http://localhost:5180, and the GitHub Pages address when published) as a JavaScript domain. The key stays in this browser and goes only to Companies House."
        }

        div {
            attr.``class`` "row"

            label {
                span {
                    flagOf "gb"
                    "United Kingdom: Companies House JavaScript key"
                }

                keyLink (fst keyPages["GB"]) "Free; create a key of type JavaScript for this site's address"

                input {
                    attr.``type`` "password"
                    attr.autocomplete "off"
                    bind.input.string model.KeyDraft (SetKeyDraft >> dispatch)
                }
            }

            button {
                attr.``class`` "primary"
                attr.disabled (String.IsNullOrWhiteSpace model.KeyDraft)
                on.click (fun _ -> dispatch SaveKey)
                "Save"
            }

            button {
                attr.``class`` "small"
                attr.disabled model.CompaniesHouseKey.IsNone
                on.click (fun _ -> dispatch RemoveKey)
                "Remove"
            }
        }

        p {
            attr.``class`` "muted compact"

            match model.CompaniesHouseKey with
            | ValueSome _ ->
                text
                    "A key is set: United Kingdom: Companies House is listed with the registers, in both searches and in cases."
            | ValueNone -> text "No key is set, so Companies House isn't listed."
        }
    }

/// Server mode: a field per setting of each keyed register, showing whether it is set, never its value.
let serverKeysCard (model: Model) dispatch =
    div {
        attr.``class`` "card"
        h2 { "Keys" }

        p {
            attr.``class`` "muted"
            "The local server keeps these in keys.json in its data folder and sends each only to its own register; on Windows they are encrypted for your Windows account. An environment variable of the same name works too, and a saved key overrides it. A register is listed once all its settings are given. docs/keys.md says where to get each key. Companies House takes a key of type API Key here, not JavaScript."
        }

        table {
            attr.``class`` "results"

            tbody {
                forEach model.Keys (fun register ->
                    concat {
                        tr {
                            attr.``class`` "register"

                            td {
                                attr.colspan 3
                                flagOf register.Flag
                                strong { register.Label }

                                span {
                                    attr.``class`` "muted"

                                    match register.Problem with
                                    | null -> text (if register.Active then " · listed" else " · not listed")
                                    | problem -> text (" · " + problem)
                                }

                                match keyPages.TryFind register.Code with
                                | Some(page, how) -> keyLink page how
                                | None -> empty ()
                            }
                        }

                        forEach register.Settings (fun setting ->
                            let draft = model.KeyDrafts.TryFind setting.Name |> Option.defaultValue ""

                            tr {
                                td {
                                    label {
                                        setting.Name

                                        input {
                                            attr.``type`` "password"
                                            attr.autocomplete "off"

                                            bind.input.string draft (fun value ->
                                                dispatch (SetSettingDraft(setting.Name, value)))
                                        }
                                    }
                                }

                                td {
                                    button {
                                        attr.``class`` "primary"
                                        attr.disabled (String.IsNullOrWhiteSpace draft)
                                        on.click (fun _ -> dispatch (SaveSetting setting.Name))
                                        "Save"
                                    }

                                    " "

                                    button {
                                        attr.``class`` "small"
                                        attr.disabled (not setting.Saved)
                                        on.click (fun _ -> dispatch (RemoveSetting setting.Name))
                                        "Remove"
                                    }
                                }

                                td {
                                    attr.``class`` "muted"

                                    if setting.Saved then
                                        "set"
                                    elif setting.FromEnvironment then
                                        "set by an environment variable"
                                    else
                                        "not set"
                                }
                            })
                    })
            }
        }
    }

let registersCard (model: Model) dispatch =
    div {
        attr.``class`` "card"
        h2 { "Registers" }

        p {
            attr.``class`` "muted"
            "Cases and searches read only the registers switched on here, and follow owners only into those. A company in a country switched off becomes a gap: not checked, never clear. The choice is kept in this browser."
        }

        ul {
            attr.``class`` "switches"
            switchRow model dispatch Gleif ValueNone "GLEIF cross-check, any country"
        }

        forEach regions (fun region ->
            match registersOf model |> List.filter (fun source -> source.Region = region) with
            | [] -> empty ()
            | inRegion ->
                concat {
                    h3 { region }

                    ul {
                        attr.``class`` "switches"

                        forEach inRegion (fun source ->
                            switchRow model dispatch source.Code (ValueSome source.Flag) source.Label)
                    }
                })
    }

let bulkCard () =
    div {
        attr.``class`` "card"
        h2 { "Bulk sources" }

        p {
            attr.``class`` "muted"
            "These registers publish files rather than an API, so a host downloads only the ones its users need, from a settings screen like this one, with BulkDownloads, and keeps an index of each on disk. This demo lists them without downloading: a browser builds an index of millions of rows on its one thread, which freezes the page for minutes."
        }

        table {
            thead {
                tr {
                    th { "Source" }
                    th { "Size" }
                    th { "Replaced every" }
                    th { "Licence" }
                    th { "" }
                }
            }

            tbody {
                forEach BulkCatalogue.all (fun entry ->
                    tr {
                        td {
                            flagOf (entry.Jurisdiction.Country.ToLowerInvariant())

                            a {
                                attr.href (string entry.Page)
                                attr.target "_blank"
                                entry.Title
                            }
                        }

                        td {
                            match entry.Delivery with
                            | BulkDelivery.Links links -> megabytes (links |> List.sumBy _.ApproximateBytes)
                            | BulkDelivery.Resolved resolver -> megabytes resolver.ApproximateBytes
                            | BulkDelivery.ByHand _ -> "—"
                        }

                        td { $"{entry.Refresh.TotalDays:F0} days" }
                        td { entry.Licence }

                        td {
                            match entry.Delivery with
                            | BulkDelivery.Links _ ->
                                span {
                                    attr.``class`` "muted"
                                    "BulkDownloads fetches it"
                                }
                            | BulkDelivery.Resolved resolver ->
                                span {
                                    attr.``class`` "muted"
                                    $"BulkDownloads finds the current files through {resolver.Portal} ({resolver.Dataset}) and fetches them"
                                }
                            | BulkDelivery.ByHand how ->
                                span {
                                    attr.``class`` "muted"
                                    how
                                }
                        }
                    })
            }
        }
    }

/// The file fields of a by-hand source, or of a resolved one built from files already on the server, and the Build index button; "not yet"
/// when the server can't build it.
let pathForm (model: Model) dispatch (item: BulkDto) =
    concat {
        div {
            attr.``class`` "muted compact"
            item.How
        }

        match item.Slots with
        | [||] ->
            div {
                attr.``class`` "muted compact"
                "Building it from files here: not yet."
            }
        | slots ->
            div {
                attr.``class`` "paths"

                forEach slots (fun slot ->
                    let key = $"{item.Id}:{slot.Name}"
                    let draft = model.PathDrafts.TryFind key |> Option.defaultValue ""

                    let caption =
                        slot.Label
                        + (if slot.Required then "" else " (optional)")
                        + (if slot.Many then ", one path per line" else "")

                    label {
                        caption

                        if slot.Many then
                            textarea {
                                attr.rows 2
                                bind.input.string draft (fun value -> dispatch (SetPathDraft(key, value)))
                            }
                        else
                            input { bind.input.string draft (fun value -> dispatch (SetPathDraft(key, value))) }
                    })

                button {
                    attr.``class`` "small"
                    on.click (fun _ -> dispatch (AddFiles item.Id))
                    "Build index from these files"
                }
            }
    }

/// A file list: each file's name on the portal, its date and size.
let fileList (files: BulkFileDto array) =
    ul {
        attr.``class`` "files compact"

        forEach files (fun file ->
            li {
                a {
                    attr.href file.Url
                    attr.target "_blank"
                    file.PortalName
                }

                let date =
                    match file.Modified with
                    | null -> ""
                    | modified ->
                        match DateTimeOffset.TryParse(modified, Globalization.CultureInfo.InvariantCulture) with
                        | true, at -> $", {at:``yyyy-MM-dd``}"
                        | _ -> ""

                span {
                    attr.``class`` "muted"
                    $"{date}, {megabytes file.Bytes}"
                }
            })
    }

/// Server mode: Download, progress and Remove per bulk source, or the path form for one fetched by hand.
let serverBulkCard (model: Model) dispatch =
    let removeButton (item: BulkDto) =
        button {
            attr.``class`` "small"
            on.click (fun _ -> dispatch (RemoveBulk item.Id))
            "Remove"
        }

    let downloadable (item: BulkDto) =
        item.Delivery = "links" || item.Delivery = "resolved"

    /// The resolved files saved now, under the source's state.
    let savedFiles (item: BulkDto) =
        match item.Files with
        | null
        | [||] -> empty ()
        | files -> fileList files

    /// For a resolved source not downloaded: what its listing names now, and the form for files already on the server.
    let resolvedExtras (item: BulkDto) =
        concat {
            div {
                attr.``class`` "muted compact"
                $"Found through {item.Portal}"
            }

            match model.Current.TryFind item.Id with
            | Some Reading ->
                div {
                    attr.``class`` "muted compact"
                    "Reading the listing…"
                }
            | Some(Listed files) -> fileList files
            | Some(NotListed reason) ->
                div {
                    attr.``class`` "bad compact"
                    reason
                }
            | None ->
                button {
                    attr.``class`` "link compact"
                    on.click (fun _ -> dispatch (ShowCurrent item.Id))
                    "Show the current files"
                }

            button {
                attr.``class`` "link compact"
                on.click (fun _ -> dispatch (ToggleOwnFiles item.Id))
                "or use files I already have"
            }

            if model.OwnFiles.Contains item.Id then
                pathForm model dispatch item
            else
                empty ()
        }

    div {
        attr.``class`` "card"
        h2 { "Bulk sources" }

        p {
            attr.``class`` "muted"
            "Download fetches a source's files into the server's data folder and builds its index there. Once the index is built, the source is listed with the registers: its lookups and name search work like any other's. Remove deletes its files and index. Nothing is fetched until you ask."
        }

        table {
            thead {
                tr {
                    th { "Source" }
                    th { "Size" }
                    th { "Replaced every" }
                    th { "Licence" }
                    th { "" }
                }
            }

            tbody {
                forEach model.Bulk (fun item ->
                    tr {
                        td {
                            flagOf item.Flag

                            a {
                                attr.href item.Page
                                attr.target "_blank"
                                item.Title
                            }
                        }

                        td {
                            if item.Bytes.HasValue then
                                megabytes item.Bytes.Value
                            else
                                "—"
                        }

                        td { $"{item.RefreshDays:F0} days" }
                        td { item.Licence }

                        td {
                            attr.``class`` "bulk-state"

                            match item.State with
                            | BulkState.Downloading ->
                                let total = if item.Expected.HasValue then item.Expected.Value else 0L
                                let done' = if item.Done.HasValue then item.Done.Value else 0L

                                concat {
                                    progress {
                                        attr.max (max total 1L)
                                        attr.value (min done' (max total 1L))
                                    }

                                    div {
                                        attr.``class`` "muted compact"

                                        text (
                                            match item.File with
                                            | null
                                            | "" -> "Starting the download…"
                                            | file ->
                                                $"Downloading {file}: {megabytes done'} of about {megabytes total}"
                                        )
                                    }
                                }
                            | BulkState.Indexing ->
                                concat {
                                    progress { attr.``class`` "busy" }

                                    div {
                                        attr.``class`` "muted compact"
                                        "Building the index…"
                                    }
                                }
                            | BulkState.IndexBuilt ->
                                concat {
                                    div {
                                        let size =
                                            if item.IndexBytes.HasValue then
                                                megabytes item.IndexBytes.Value
                                            else
                                                "—"

                                        text $"Ready; index {size}"

                                        if item.Stale then
                                            span {
                                                attr.``class`` "warn"
                                                "; older than its publisher's cadence"
                                            }
                                        else
                                            empty ()
                                    }

                                    savedFiles item

                                    if downloadable item then
                                        button {
                                            attr.``class`` "small"
                                            on.click (fun _ -> dispatch (Download item.Id))
                                            "Download again"
                                        }
                                    else
                                        empty ()

                                    " "
                                    removeButton item
                                }
                            | BulkState.Downloaded ->
                                concat {
                                    div { "Downloaded; no index yet" }
                                    savedFiles item

                                    button {
                                        attr.``class`` "small"
                                        on.click (fun _ -> dispatch (Download item.Id))
                                        "Build index"
                                    }

                                    " "
                                    removeButton item
                                }
                            | _ when item.Delivery = "links" ->
                                button {
                                    attr.``class`` "primary"
                                    on.click (fun _ -> dispatch (Download item.Id))
                                    "Download"
                                }
                            | _ when item.Delivery = "resolved" ->
                                concat {
                                    button {
                                        attr.``class`` "primary"
                                        on.click (fun _ -> dispatch (Download item.Id))
                                        "Download"
                                    }

                                    resolvedExtras item
                                }
                            | _ -> pathForm model dispatch item

                            match item.Error with
                            | null -> empty ()
                            | error ->
                                div {
                                    attr.``class`` "bad compact"
                                    error
                                }
                        }
                    })
            }
        }
    }

let settingsPage (model: Model) dispatch =
    match model.Mode with
    | OnServer ->
        concat {
            errorCard model
            serverKeysCard model dispatch
            registersCard model dispatch
            serverBulkCard model dispatch
        }
    | Detecting
    | InBrowser ->
        concat {
            keysCard model dispatch
            registersCard model dispatch
            bulkCard ()
        }

let aboutPage (model: Model) =
    div {
        attr.``class`` "card"
        h2 { "About this demo" }

        match model.Mode with
        | OnServer ->
            concat {
                p {
                    "It runs the CompanyDueDiligence library in a server on this computer, listening on localhost only, and this page in your browser asks it. Calls go from the server to each register."
                }

                p {
                    "Besides the keyless registers, it reads registers whose keys are saved in Settings, and bulk sources downloaded there, with an index of each kept in its data folder."
                }
            }
        | Detecting
        | InBrowser ->
            concat {
                p {
                    "It runs the CompanyDueDiligence library itself, compiled to WebAssembly. Every call goes from your browser straight to the register; nothing passes through a server of ours."
                }

                p {
                    "It reads only the keyless registers whose servers allow calls from a web page. Registers behind a key, and bulk files, need a desktop or server host, where keys stay private and files are kept in a folder."
                }
            }

        p {
            "Flags are from "

            a {
                attr.href "https://github.com/lipis/flag-icons"
                "flag-icons"
            }

            " by Panayiotis Lipiridis, under the MIT licence in flags/LICENSE."
        }

        p {
            "The library is unofficial and not affiliated with any register. "

            a {
                attr.href "https://github.com/Thorium/CompanyDueDiligence"
                "Source and documentation"
            }
        }
    }

let view model dispatch =
    concat {
        header {
            h1 { "CompanyDueDiligence" }

            nav {
                navButton model dispatch CasePage "Companies"
                navButton model dispatch SettingsPage "Settings"
                navButton model dispatch AboutPage "About"
            }
        }

        main {
            match model.Page with
            | CasePage -> casePage model dispatch
            | SettingsPage -> settingsPage model dispatch
            | AboutPage -> aboutPage model
        }
    }

type App() =
    inherit ProgramComponent<Model, Message>()

    override this.Program =
        let http = new HttpClient(BaseAddress = Uri this.NavigationManager.BaseUri)

        // The local server's access token; GitHub Pages ignores the header.
        match (this.JSRuntime :?> IJSInProcessRuntime).Invoke<string>("cddToken") with
        | null
        | "" -> ()
        | token -> http.DefaultRequestHeaders.Add("X-CDD-Token", token)

        Program.mkProgram (fun _ -> init this.JSRuntime http) (update this.JSRuntime http) view
