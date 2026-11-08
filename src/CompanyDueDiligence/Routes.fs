namespace CompanyDueDiligence

open System

/// How a register with no free source here can lawfully be read. None of these is scraped: a route is for a person or a contract.
[<RequireQualifiedAccess>]
type RouteKind =
    /// A paid vendor or a contract with the register.
    | PaidContract
    /// Approval or standing from an authority.
    | Entitlement

type CoverageRoute =
    {
        Register: string
        Jurisdiction: Jurisdiction
        Kind: RouteKind
        /// Who to go to.
        Route: string
        /// Why this package has no adapter for the register.
        WhyNoAdapter: string
    }

/// Registers declared as coverage gaps. The routes are as researched for this package and not contract-checked: confirm them before relying on one.
module CoverageRoutes =

    /// A paid API that can stand in for several of these. An option for a host, not something this package calls.
    [<Literal>]
    let OpenCorporates = "OpenCorporates, as a paid API"

    let delaware =
        {
            Register = "Delaware Division of Corporations"
            Jurisdiction = Jurisdiction.ofCode "US-DE"
            Kind = RouteKind.PaidContract
            Route = $"A paid vendor or registered agent's status report, or {OpenCorporates}"
            WhyNoAdapter = "The public search's terms forbid automated use"
        }

    let germany =
        {
            Register = "German commercial register (court + HRA/HRB + number)"
            Jurisdiction = Jurisdiction.ofCode "DE"
            Kind = RouteKind.Entitlement
            Route = $"Approval from the court at Hagen for automated retrieval, a paid vendor, or {OpenCorporates}"
            WhyNoAdapter = "The portal's terms forbid automated use without that approval"
        }

    let spain =
        {
            Register = "Registro Mercantil (Spain)"
            Jurisdiction = Jurisdiction.ofCode "ES"
            Kind = RouteKind.PaidContract
            Route = $"A paid extract from the Registradores, a paid vendor, or {OpenCorporates}"
            WhyNoAdapter = "No free route found that permits automated use"
        }

    let italy =
        {
            Register = "Registro delle Imprese (Italy)"
            Jurisdiction = Jurisdiction.ofCode "IT"
            Kind = RouteKind.PaidContract
            Route = $"A paid extract through InfoCamere, a paid vendor, or {OpenCorporates}"
            WhyNoAdapter = "No free route found that permits automated use"
        }

    let luxembourg =
        {
            Register = "Luxembourg Business Registers"
            Jurisdiction = Jurisdiction.ofCode "LU"
            Kind = RouteKind.PaidContract
            Route = $"A paid extract from the LBR, a paid vendor, or {OpenCorporates}"
            WhyNoAdapter = "No free route found that permits automated use"
        }

    let all = [ delaware; germany; spain; italy; luxembourg ]

    /// The route declared for a jurisdiction, if any.
    let forJurisdiction (routes: CoverageRoute list) (jurisdiction: Jurisdiction) =
        routes
        |> List.tryFind (fun route -> Jurisdiction.covers route.Jurisdiction jurisdiction)
        |> ValueOption.ofOption

//// Jurisdictions from a register's wording, for following a link. Wording not listed stays unplaced and becomes a gap: it is never guessed.
module JurisdictionWords =

    let private usStates =
        [
            "ALABAMA", "AL"
            "ALASKA", "AK"
            "ARIZONA", "AZ"
            "ARKANSAS", "AR"
            "CALIFORNIA", "CA"
            "COLORADO", "CO"
            "CONNECTICUT", "CT"
            "DELAWARE", "DE"
            "DISTRICT OF COLUMBIA", "DC"
            "FLORIDA", "FL"
            "GEORGIA", "GA"
            "HAWAII", "HI"
            "IDAHO", "ID"
            "ILLINOIS", "IL"
            "INDIANA", "IN"
            "IOWA", "IA"
            "KANSAS", "KS"
            "KENTUCKY", "KY"
            "LOUISIANA", "LA"
            "MAINE", "ME"
            "MARYLAND", "MD"
            "MASSACHUSETTS", "MA"
            "MICHIGAN", "MI"
            "MINNESOTA", "MN"
            "MISSISSIPPI", "MS"
            "MISSOURI", "MO"
            "MONTANA", "MT"
            "NEBRASKA", "NE"
            "NEVADA", "NV"
            "NEW HAMPSHIRE", "NH"
            "NEW JERSEY", "NJ"
            "NEW MEXICO", "NM"
            "NEW YORK", "NY"
            "NORTH CAROLINA", "NC"
            "NORTH DAKOTA", "ND"
            "OHIO", "OH"
            "OKLAHOMA", "OK"
            "OREGON", "OR"
            "PENNSYLVANIA", "PA"
            "RHODE ISLAND", "RI"
            "SOUTH CAROLINA", "SC"
            "SOUTH DAKOTA", "SD"
            "TENNESSEE", "TN"
            "TEXAS", "TX"
            "UTAH", "UT"
            "VERMONT", "VT"
            "VIRGINIA", "VA"
            "WASHINGTON", "WA"
            "WEST VIRGINIA", "WV"
            "WISCONSIN", "WI"
            "WYOMING", "WY"
        ]

    let private usStateCodes = usStates |> List.map snd |> Set.ofList

    /// Province and territory codes as a Canadian register writes them.
    let private canadianProvinceCodes =
        set [ "AB"; "BC"; "MB"; "NB"; "NL"; "NS"; "NT"; "NU"; "ON"; "PE"; "QC"; "SK"; "YT" ]

    /// Words a state's name can come with in a register's wording.
    let private statePrefixes =
        [ "STATE OF "; "COMMONWEALTH OF "; "SECRETARY OF STATE OF " ]

    let private stateSuffixes =
        [
            " UNITED STATES OF AMERICA"
            " UNITED STATES"
            " USA"
            " US"
            " SECRETARY OF STATE"
            " DIVISION OF CORPORATIONS"
        ]

    /// Keys are compared after NameMatching.normalise, so native names are listed with their accents and in their own script.
    let private normalised (pairs: (string * string) list) =
        pairs
        |> List.map (fun (wording, code) -> NameMatching.normalise wording, code)
        |> Map.ofList

    let private countries =
        normalised
            [
                "UNITED KINGDOM", "GB"
                "UK", "GB"
                "GREAT BRITAIN", "GB"
                "ENGLAND", "GB"
                "WALES", "GB"
                "SCOTLAND", "GB"
                "NORTHERN IRELAND", "GB"
                "ENGLAND AND WALES", "GB"
                "ENGLAND WALES", "GB"
                "COMPANIES HOUSE", "GB"
                "UNITED STATES", "US"
                "UNITED STATES OF AMERICA", "US"
                "USA", "US"
                "US", "US"
                "MALTA", "MT"
                "FRANCE", "FR"
                "FINLAND", "FI"
                "SUOMI", "FI"
                "GERMANY", "DE"
                "DEUTSCHLAND", "DE"
                "SPAIN", "ES"
                "ITALY", "IT"
                "LUXEMBOURG", "LU"
                "LUXEMBURG", "LU"
                "NETHERLANDS", "NL"
                "THE NETHERLANDS", "NL"
                "NEDERLAND", "NL"
                "BELGIUM", "BE"
                "BELGIË", "BE"
                "BELGIQUE", "BE"
                "IRELAND", "IE"
                "REPUBLIC OF IRELAND", "IE"
                "ÉIRE", "IE"
                "SWEDEN", "SE"
                "SVERIGE", "SE"
                "NORWAY", "NO"
                "NORGE", "NO"
                "DENMARK", "DK"
                "DANMARK", "DK"
                "AUSTRIA", "AT"
                "ÖSTERREICH", "AT"
                "SWITZERLAND", "CH"
                "SCHWEIZ", "CH"
                "SUISSE", "CH"
                "SVIZZERA", "CH"
                "POLAND", "PL"
                "POLSKA", "PL"
                "PORTUGAL", "PT"
                "GREECE", "GR"
                "HELLAS", "GR"
                "ΕΛΛΆΔΑ", "GR"
                "CYPRUS", "CY"
                "ΚΎΠΡΟΣ", "CY"
                "ESTONIA", "EE"
                "EESTI", "EE"
                "LATVIA", "LV"
                "LATVIJA", "LV"
                "LITHUANIA", "LT"
                "LIETUVA", "LT"
                "CZECH REPUBLIC", "CZ"
                "CZECHIA", "CZ"
                "ČESKÁ REPUBLIKA", "CZ"
                "ČESKO", "CZ"
                "SLOVAKIA", "SK"
                "SLOVAK REPUBLIC", "SK"
                "SLOVENSKÁ REPUBLIKA", "SK"
                "SLOVENSKO", "SK"
                "SLOVENIA", "SI"
                "SLOVENIJA", "SI"
                "HUNGARY", "HU"
                "MAGYARORSZÁG", "HU"
                "ROMANIA", "RO"
                "ROMÂNIA", "RO"
                "BULGARIA", "BG"
                "БЪЛГАРИЯ", "BG"
                "CROATIA", "HR"
                "HRVATSKA", "HR"
                "REPUBLIKA HRVATSKA", "HR"
                "LIECHTENSTEIN", "LI"
                "ICELAND", "IS"
                "JERSEY", "JE"
                "GUERNSEY", "GG"
                "ISLE OF MAN", "IM"
                "GIBRALTAR", "GI"
                "CANADA", "CA"
                "AUSTRALIA", "AU"
                "NEW ZEALAND", "NZ"
                "SINGAPORE", "SG"
                "HONG KONG", "HK"
                "UNITED ARAB EMIRATES", "AE"
                "BRITISH VIRGIN ISLANDS", "VG"
                "CAYMAN ISLANDS", "KY"
                "BERMUDA", "BM"
                "PANAMA", "PA"
                "MAURITIUS", "MU"
                "SEYCHELLES", "SC"
                "JAPAN", "JP"
                "日本", "JP"
                "KOREA", "KR"
                "REPUBLIC OF KOREA", "KR"
                "KOREA, REPUBLIC OF", "KR"
                "SOUTH KOREA", "KR"
                "대한민국", "KR"
                "TAIWAN", "TW"
                "台灣", "TW"
                "臺灣", "TW"
                "ISRAEL", "IL"
                "ישראל", "IL"
                "BRAZIL", "BR"
                "BRASIL", "BR"
                "ARGENTINA", "AR"
                "CHILE", "CL"
                "COLOMBIA", "CO"
                "COSTA RICA", "CR"
                "PERU", "PE"
                "PERÚ", "PE"
                "ECUADOR", "EC"
                "PARAGUAY", "PY"
                "MEXICO", "MX"
                "MÉXICO", "MX"
                "MOLDOVA", "MD"
                "REPUBLIC OF MOLDOVA", "MD"
                "REPUBLICA MOLDOVA", "MD"
                "SERBIA", "RS"
                "SRBIJA", "RS"
                "СРБИЈА", "RS"
                "UKRAINE", "UA"
                "УКРАЇНА", "UA"
                "RUSSIA", "RU"
                "RUSSIAN FEDERATION", "RU"
                "РОССИЯ", "RU"
                "РОССИЙСКАЯ ФЕДЕРАЦИЯ", "RU"
                "KENYA", "KE"
                "BOSNIA AND HERZEGOVINA", "BA"
                "BOSNA I HERCEGOVINA", "BA"
            ]

    /// Subdivisions outside the US, by their full names.
    let private subdivisions =
        normalised
            [
                "BRITISH COLUMBIA", "CA-BC"
                "ALBERTA", "CA-AB"
                "SASKATCHEWAN", "CA-SK"
                "MANITOBA", "CA-MB"
                "ONTARIO", "CA-ON"
                "QUEBEC", "CA-QC"
                "QUÉBEC", "CA-QC"
                "NEW BRUNSWICK", "CA-NB"
                "NOVA SCOTIA", "CA-NS"
                "PRINCE EDWARD ISLAND", "CA-PE"
                "NEWFOUNDLAND AND LABRADOR", "CA-NL"
                "YUKON", "CA-YT"
                "NORTHWEST TERRITORIES", "CA-NT"
                "NUNAVUT", "CA-NU"
                "CIUDAD AUTÓNOMA DE BUENOS AIRES", "AR-C"
                "CABA", "AR-C"
                "BRČKO", "BA-BRC"
                "BRČKO DISTRIKT", "BA-BRC"
            ]

    /// The US state the whole wording names, with only the words a state's name comes with around it.
    let private stateIn (normal: string) =
        let withoutPrefix =
            statePrefixes
            |> List.tryFind (fun (prefix: string) -> normal.StartsWith(prefix, StringComparison.Ordinal))
            |> Option.map (fun prefix -> normal.Substring prefix.Length)
            |> Option.defaultValue normal

        let bare =
            stateSuffixes
            |> List.tryFind (fun (suffix: string) -> withoutPrefix.EndsWith(suffix, StringComparison.Ordinal))
            |> Option.map (fun suffix -> withoutPrefix.Substring(0, withoutPrefix.Length - suffix.Length))
            |> Option.defaultValue withoutPrefix

        usStates
        |> List.tryPick (fun (name, code) -> if bare = name then Some code else None)

    /// A placed wording. MayBeState marks a bare two-letter code that is also a US state's postal code, such as DE, CO or MD.
    [<Struct>]
    type private Placed = { Code: string; MayBeState: bool }

    /// One wording, read whole: New South Wales isn't Wales, and Tbilisi, Georgia isn't the US state. A bare two-letter code on a record held in
    /// the US or Canada is that country's state or province code when it is one, since those registers write places that way; anywhere else it
    /// is a country code.
    let private placeOne (within: Jurisdiction voption) (wording: string) =
        let normal = NameMatching.normalise wording

        let placed code =
            Some { Code = code; MayBeState = false }

        match countries.TryFind normal, subdivisions.TryFind normal with
        | Some code, _
        | None, Some code -> placed code
        // Georgia is also a country, so the word alone places nothing.
        | None, None when normal = "GEORGIA" -> None
        | None, None when normal.Length = 2 && normal |> Seq.forall Char.IsAsciiLetterUpper ->
            match within with
            | ValueSome on when on.Country = "US" && usStateCodes.Contains normal -> placed ("US-" + normal)
            | ValueSome on when on.Country = "CA" && canadianProvinceCodes.Contains normal -> placed ("CA-" + normal)
            | _ ->
                Some
                    {
                        Code = normal
                        MayBeState = usStateCodes.Contains normal
                    }
        | None, None -> stateIn normal |> Option.bind (fun state -> placed ("US-" + state))

    let private placeFrom (within: Jurisdiction voption) (wordings: string voption list) =
        let placed =
            wordings |> List.choose ValueOption.toOption |> List.choose (placeOne within)

        // A bare code that may be a state is left unplaced when another wording names a US state.
        let namesState = placed |> List.exists (fun item -> item.Code.StartsWith "US-")

        let codes =
            placed
            |> List.filter (fun item -> not (namesState && item.MayBeState))
            |> List.map _.Code

        let chosen =
            match codes with
            | country :: others when not (country.Contains '-') ->
                others
                |> List.tryFind (fun code -> code.StartsWith(country + "-"))
                |> Option.orElse (Some country)
            | first :: _ -> Some first
            | [] -> None

        chosen |> Option.map Jurisdiction.tryParse |> Option.defaultValue ValueNone

    /// The jurisdiction the first readable wording names. A bare country takes its state or province from another wording, because a state's
    /// register is what can be read.
    let place (wordings: string voption list) = placeFrom ValueNone wordings

    /// As place, for wording on a record held in a jurisdiction, which decides what a bare two-letter code means.
    let placeWithin (on: Jurisdiction) (wordings: string voption list) = placeFrom (ValueSome on) wordings
