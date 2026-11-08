/// The registers the demo lists, with the names, regions and flags it shows. The browser page and the local server share them.
module CompanyDueDiligence.Web.Registers

open CompanyDueDiligence

/// A keyless lookup source, with a company to try.
type KeylessSource =
    {
        Code: string
        Label: string
        Region: string
        /// The flag-icons file name: the country's code in lower case.
        Flag: string
        Example: string
        /// True when its server allows calls from a web page, as checked on 2026-11-08.
        InBrowser: bool
        Make: SourceConnection -> ICompanyRegistry
    }

/// A source behind a key, with the settings KeyedSources.fromSettings reads for it.
type KeyedSource =
    {
        /// Unique among the registers listed. Zefix's is CH-ZEFIX, since the keyless UID register is CH.
        Code: string
        /// The jurisdiction a case asks it for.
        Jurisdiction: string
        Label: string
        Region: string
        Flag: string
        Example: string
        Settings: string list
    }

/// The regions the picker and the switches group registers under, in order.
let regions = [ "Europe"; "Americas"; "Asia"; "Oceania"; "Africa" ]

/// The region a country is listed under.
let regionOf (country: string) =
    match country with
    | "US"
    | "CA"
    | "BR"
    | "CL"
    | "AR"
    | "PE"
    | "EC"
    | "PY"
    | "CO"
    | "CR" -> "Americas"
    | "IL"
    | "SG"
    | "TW"
    | "JP"
    | "KR" -> "Asia"
    | "AU"
    | "NZ" -> "Oceania"
    | "KE" -> "Africa"
    | _ -> "Europe"

/// The flag-icons file name for a jurisdiction: its country in lower case.
let flagCodeOf (jurisdiction: Jurisdiction) = jurisdiction.Country.ToLowerInvariant()

let private keylessSource code label region flag example inBrowser make =
    {
        Code = code
        Label = label
        Region = region
        Flag = flag
        Example = example
        InBrowser = inBrowser
        Make = make
    }

/// Every keyless lookup source, in the order the page lists them.
let keyless =
    [
        keylessSource "NO" "Norway: Brønnøysund" "Europe" "no" "923609016" true Sources.norway
        keylessSource "CZ" "Czechia: ARES" "Europe" "cz" "00177041" true Sources.czechia
        keylessSource "PL" "Poland: KRS" "Europe" "pl" "19193" true Sources.poland
        keylessSource "FR" "France: Recherche d'entreprises" "Europe" "fr" "552032534" true Sources.france
        keylessSource "FI" "Finland: PRH/YTJ" "Europe" "fi" "0112038-9" true Sources.finland
        keylessSource "MT" "Malta: Business Registry" "Europe" "mt" "C 1" false Sources.malta
        keylessSource "SK" "Slovakia: RPO" "Europe" "sk" "35763469" false Sources.slovakia
        keylessSource "IE" "Ireland: CRO open data" "Europe" "ie" "104547" false Sources.ireland
        keylessSource "CH" "Switzerland: UID register" "Europe" "ch" "CHE-101.237.723" false Sources.switzerland
        keylessSource "NL" "Netherlands: KVK open dataset" "Europe" "nl" "59581883" false Sources.netherlands
        keylessSource "LV" "Latvia: Register of Enterprises" "Europe" "lv" "40003000642" false Sources.latvia
        keylessSource "US-CO" "Colorado" "Americas" "us" "18691000001" true Sources.colorado
        keylessSource "US-CT" "Connecticut" "Americas" "us" "645848" true Sources.connecticut
        keylessSource "US-NY" "New York" "Americas" "us" "416717" true Sources.newYork
        keylessSource "US-TX" "Texas Comptroller" "Americas" "us" "0800515257" true Sources.texas
        keylessSource "US-DC" "District of Columbia" "Americas" "us" "952891" true Sources.districtOfColumbia
        keylessSource "US-OR" "Oregon" "Americas" "us" "10003093" false Sources.oregon
        keylessSource "US-PA" "Pennsylvania" "Americas" "us" "0000000378" false Sources.pennsylvania
        keylessSource "CA-BC" "British Columbia: OrgBook" "Americas" "ca" "BC0772006" true Sources.britishColumbia
        keylessSource "CO" "Colombia: RUES" "Americas" "co" "899999068" true Sources.colombia
        keylessSource "CR" "Costa Rica: Hacienda (tax registration)" "Americas" "cr" "4000042139" true Sources.costaRica
        keylessSource "IL" "Israel: Companies Registrar" "Asia" "il" "515532778" true Sources.israel
        keylessSource "SG" "Singapore: ACRA" "Asia" "sg" "201201936C" true Sources.singapore
    ]

let private keyedSource code jurisdiction label flag example settings =
    {
        Code = code
        Jurisdiction = jurisdiction
        Label = label
        Region = regionOf (Jurisdiction.ofCode jurisdiction).Country
        Flag = flag
        Example = example
        Settings = settings
    }

/// Where each keyed register's issuer gives keys, and what it takes, as docs/keys.md records them on 2026-11-08.
let keyPages =
    Map.ofList
        [
            "GB", ("https://developer.company-information.service.gov.uk/", "Free; issued at once")
            "CA", ("https://api.ised-isde.canada.ca/", "Free; subscribe to the Corporations API Public Plan")
            "NZ", ("https://portal.api.business.govt.nz/", "Free; subscriptions may be reviewed")
            "SE",
            ("https://bolagsverket.se/apierochoppnadata/hamtaforetagsinformation/vardefulladatamangder/apiforvardefulladatamangder.5513.html",
             "Free; send the customer notification form")
            "JP", ("https://www.houjin-bangou.nta.go.jp/webapi/", "Free; the application ID takes 2 to 4 weeks")
            "CH-ZEFIX", ("mailto:zefix@bj.admin.ch?subject=Zefix%20PublicREST%20access", "Ask the EHRA by email")
            "US-CA", ("https://calicodev.sos.ca.gov/", "Government agencies only, per the portal")
            "AU", ("https://abr.business.gov.au/Tools/WebServices", "Free GUID by email; accept the agreement")
            "KR", ("https://www.data.go.kr/data/15081808/openapi.do", "Free; apply on data.go.kr")
            "HR", ("https://sudreg-data.gov.hr/", "Register on sudreg-data.gov.hr; approval unverified")
            "GR", ("https://opendata.businessportal.gr/register", "GEMI approves each application")
            "AT",
            ("https://justizonline.gv.at/jop/web/iwg/register", "The Ministry of Justice approves each application")
            "DK", ("https://datacvr.virk.dk/", "Erhvervsstyrelsen grants system-to-system access")
            "KE", ("https://developer.go.ke/", "KRA approves each app before it goes live")
        ]

/// Every source behind a key, in KeyedSources.settingNames order.
let keyed =
    [
        keyedSource "GB" "GB" "United Kingdom: Companies House" "gb" "09550128" [ "CDD_COMPANIES_HOUSE_KEY" ]
        keyedSource "CA" "CA" "Canada: Corporations Canada" "ca" "" [ "CDD_CANADA_KEY" ]
        keyedSource "NZ" "NZ" "New Zealand: NZBN" "nz" "" [ "CDD_NEW_ZEALAND_KEY" ]
        keyedSource "SE" "SE" "Sweden: Bolagsverket" "se" "" [ "CDD_SWEDEN_CLIENT_ID"; "CDD_SWEDEN_CLIENT_SECRET" ]
        keyedSource "JP" "JP" "Japan: NTA corporate number" "jp" "" [ "CDD_JAPAN_APPLICATION_ID" ]
        keyedSource "CH-ZEFIX" "CH" "Switzerland: Zefix" "ch" "" [ "CDD_ZEFIX_USER"; "CDD_ZEFIX_PASSWORD" ]
        keyedSource "US-CA" "US-CA" "California SOS" "us" "" [ "CDD_CALIFORNIA_KEY" ]
        keyedSource "AU" "AU" "Australia: ABN Lookup" "au" "" [ "CDD_AUSTRALIA_GUID" ]
        keyedSource "KR" "KR" "South Korea: NTS business status" "kr" "" [ "CDD_KOREA_SERVICE_KEY" ]
        keyedSource
            "HR"
            "HR"
            "Croatia: Sudski registar"
            "hr"
            ""
            [ "CDD_CROATIA_CLIENT_ID"; "CDD_CROATIA_CLIENT_SECRET" ]
        keyedSource "GR" "GR" "Greece: GEMI" "gr" "" [ "CDD_GREECE_KEY" ]
        keyedSource "AT" "AT" "Austria: Firmenbuch HVD" "at" "" [ "CDD_AUSTRIA_KEY" ]
        keyedSource "DK" "DK" "Denmark: CVR" "dk" "" [ "CDD_DENMARK_USER"; "CDD_DENMARK_PASSWORD" ]
        keyedSource "KE" "KE" "Kenya: KRA PIN checker" "ke" "" [ "CDD_KENYA_CONSUMER_KEY"; "CDD_KENYA_CONSUMER_SECRET" ]
    ]
