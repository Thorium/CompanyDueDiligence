namespace CompanyDueDiligence

open System
open System.Collections.Generic
open System.IO
open System.Text
open System.Text.RegularExpressions
open System.Threading
open System.Threading.Tasks

/// What one ONRC firm state code says, as the adapter places it.
[<RequireQualifiedAccess>]
type internal RomaniaState =
    /// Removed from the register (radiată).
    | Removed
    | Insolvent of proceeding: string
    | Dissolving of status: string
    /// In operation (funcțiune).
    | Operating
    /// In operation, with something against it.
    | OperatingConcern of concern: string
    /// A state or mention against the firm.
    | Concern of concern: string
    /// A reservation or a registration not completed: not a registered firm.
    | Pending of status: string
    /// A mention that says nothing of standing.
    | Noted

/// ONRC's open data on data.gov.ro, as published for 04.11.2026: OD_FIRME.CSV, OD_STARE_FIRMA.CSV with the N_STARE_FIRMA code list, and
/// OD_REPREZENTANTI_LEGALI.CSV, all delimited by ^.
module internal Romania =

    [<Literal>]
    let Registry = "ONRC open data (Romania)"

    /// The coverage, with the count of state file rows that name no firm the file can read, which leaves some firm's standing short of a code.
    let coverage (representatives: bool) (unplacedStates: int) : RegistryCoverage =
        {
            Registry = Registry
            Jurisdictions = [ Jurisdiction.ofCode "RO" ]
            Standing =
                if unplacedStates = 0 then
                    Support.Supported
                else
                    Support.Partial
                        $"{unplacedStates} rows of the firm state file have no registration number or code that can be read, so a firm's standing can miss a code"
            Concerns = Support.Partial "Insolvency and the unfiled accounts mentions the state codes carry are stated"
            Addresses = Support.Supported
            Search = Support.Partial "By name within the file, up to 50 matches"
            Officers =
                if representatives then
                    Support.Partial
                        "Legal representatives as the file lists them, with no dates, so whether they still serve isn't stated"
                else
                    Support.NotOffered "The host passed no legal representatives file"
            Owners = Support.NotOffered "The open data holds no shareholders or beneficial owners"
            Cost = AccessCost.Free
            RateLimit = RateLimitPolicy.NotPublished(1, TimeSpan.FromDays 14.)
            Licence = "CC BY 4.0, as the data.gov.ro dataset states"
        }

    let private oldCode = Regex @"\A([JFC])0*([0-9]{1,2})/0*([0-9]{1,7})/([0-9]{4})\z"
    let private newCode = Regex @"\A([JFC])([0-9]{4})([0-9]{6})([0-9]{3})\z"
    let private cuiCode = Regex @"\A(?:RO)?\s*0*([0-9]{1,10})\z"

    /// A registration number in either form, J40/123/2020 (county and sequence without leading zeros) or J2020000123400.
    let normalRegistration (number: string) =
        let normal =
            if isNull number then
                ""
            else
                number.Trim().ToUpperInvariant().Replace(" ", "")

        let old = oldCode.Match normal

        if old.Success then
            ValueSome $"{old.Groups[1].Value}{old.Groups[2].Value}/{old.Groups[3].Value}/{old.Groups[4].Value}"
        elif newCode.IsMatch normal then
            ValueSome normal
        else
            ValueNone

    /// A CUI's digits, with an RO prefix allowed; zero, which the file gives firms with none, isn't one.
    let normalCui (number: string) =
        let normal =
            if isNull number then
                ""
            else
                number.Trim().ToUpperInvariant()

        let found = cuiCode.Match normal

        if found.Success && found.Groups[1].Value <> "0" then
            ValueSome found.Groups[1].Value
        else
            ValueNone

    [<Literal>]
    let private CuiPrefix = "CUI "

    /// The key of a CUI no single firm holds.
    let cuiKey (cui: string) = CuiPrefix + cui

    /// Why a key the file doesn't hold can't be told absent: the file writes each firm's registration number in one of its two forms, and
    /// neither is derived from the other here. A CUI has one form, so a CUI the file doesn't hold is not held.
    let cantTellAbsent (key: string) =
        if key.StartsWith(CuiPrefix, StringComparison.Ordinal) then
            ValueNone
        else
            ValueSome
                "The file writes a firm's registration number in one of two forms, such as J40/123/2020 or J2020000123400, and one isn't derived from the other here, so a number the file doesn't hold in this form can be held in the other: ask by CUI"

    /// A number's key without the file: a registration number as itself, a CUI as its cuiKey. An index maps the cuiKey of a CUI one firm
    /// holds to that firm's key.
    let normalWithoutFile (number: string) =
        match normalRegistration number with
        | ValueSome key -> ValueSome key
        | ValueNone -> normalCui number |> ValueOption.map cuiKey

    /// Every code in N_STARE_FIRMA of 04.11.2026, placed. A code not listed is ValueNone.
    let state (code: int) =
        let concern text = ValueSome(RomaniaState.Concern text)
        let dissolving text = ValueSome(RomaniaState.Dissolving text)
        let insolvent text = ValueSome(RomaniaState.Insolvent text)
        let pending text = ValueSome(RomaniaState.Pending text)
        let noted = ValueSome RomaniaState.Noted

        match code with
        | 1084 -> ValueSome RomaniaState.Removed
        | 1048 -> ValueSome RomaniaState.Operating
        | 1112 ->
            ValueSome(
                RomaniaState.OperatingConcern
                    "în funcțiune, sub incidența Legii nr. 359/2004 de la expirarea termenului"
            )
        | 1070 -> insolvent "faliment"
        | 1057 -> insolvent "reorganizare judiciară"
        | 1073 -> insolvent "insolvabilă (art.117 din Legea nr. 64/1995)"
        | 1105 -> insolvent "insolvabilă (art.131 din O.G. nr. 61/2002)"
        | 1106 -> insolvent "deschidere procedură cf. Legii nr. 64/1995"
        | 1107 -> insolvent "insolvență"
        | 1114 -> insolvent "suspendare procedură faliment"
        | 1119 -> insolvent "suspendare procedură de insolvență"
        | 1121 -> insolvent "insolvență străină"
        | 1123 -> insolvent "suspendare provizorie a procedurii de insolvență"
        | 1083 -> insolvent "sub incidența Legii nr. 85/2006"
        | 1126 -> insolvent "sub incidența OUG nr. 91/2013"
        | 1139 -> insolvent "sub incidența Legii nr. 85/2014"
        | 1137 -> insolvent "redresare financiară prin administrare specială (Legea nr. 503/2004)"
        | 1138 -> insolvent "redresare financiară pe bază de plan (Legea nr. 503/2004)"
        | 1144 -> insolvent "închiderea redresării financiare și constatarea stării de insolvență"
        | 1049 -> dissolving "dizolvare"
        | 1052 -> dissolving "lichidare"
        | 1078 -> dissolving "declarare nulitate"
        | 1076 -> dissolving "dizolvare - capital sub limita impusă de lege"
        | 1094 -> dissolving "dizolvare urmare a expirării duratei de funcționare"
        | 1098 -> dissolving "dizolvare de drept nemajorare capital (Legea nr. 314/2001)"
        | 1100 -> dissolving "dizolvare de drept asociat unic (Legea nr. 314/2001)"
        | 1108 -> dissolving "închidere procedură cf. art.117 din Legea nr. 64/1995"
        | 1109 -> dissolving "dizolvare de drept (Legea nr. 359/2004)"
        | 1111 -> dissolving "dizolvare de drept cu desființarea încheierii judecătorului delegat"
        | 1113 -> dissolving "dizolvare judiciară (art.237 din Legea nr. 31/1990)"
        | 1116 -> dissolving "închiderea reorganizării judiciare și a falimentului (art.118 din Legea nr. 64/1995)"
        | 1118 -> dissolving "închiderea reorganizării judiciare și a falimentului (art.132 din Legea nr. 64/1995)"
        | 1120 -> dissolving "dizolvare (art.235 din Legea nr. 31/1990)"
        | 1133 -> dissolving "dizolvare (Legea nr. 286/2009)"
        | 1145 -> dissolving "dizolvare judiciară (art.227 din Legea nr. 31/1990)"
        | 2134 -> dissolving "prelungire perioadă de lichidare"
        | 1081 -> pending "în curs de înmatriculare"
        | 1080 -> pending "prelungire rezervare"
        | 1082 -> pending "rezervare"
        | 1079 -> pending "rezervare din oficiu"
        | 1088 -> pending "constituire prin subscripție publică"
        | 1075 -> pending "înmatriculare respinsă"
        | 1086 -> pending "înmatriculare perimată"
        | 1053
        | 1056
        | 1072
        | 1077
        | 1097
        | 1099
        | 1101
        | 1102
        | 1103
        | 1035
        | 2080
        | 2082
        | 2128
        | 2131
        | 2133
        | 2136
        | 2137
        | 2138
        | 2129
        | 2140
        | 2141
        | 2142
        | 2143
        | 2144
        | 2145
        | 2150 -> noted
        | 1050 -> concern "reorganizare - fuziune prin absorbție"
        | 1051 -> concern "reorganizare - fuziune prin contopire"
        | 1054 -> concern "reorganizare - divizare parțială"
        | 1055 -> concern "reorganizare - divizare totală"
        | 1065 -> concern "reorganizare RA în S.C."
        | 1066 -> concern "reorganizare subunitate a RA în S.C."
        | 1068 -> concern "reorganizare sucursală în S.C."
        | 1071 -> concern "reorganizare în cooperative de credit"
        | 1090 -> concern "reorganizare - alte moduri"
        | 1104 -> concern "reorganizare SA în Institut Național"
        | 1122 -> concern "fuziune efectivă la 01.06.2007"
        | 1125 -> concern "reorganizare prin fuziune transfrontalieră"
        | 1149 -> concern "reorganizare prin divizare transfrontalieră"
        | 1150 -> concern "reorganizare prin transformare transfrontalieră"
        | 1074 -> concern "întrerupere temporară de activitate"
        | 1085 -> concern "valabilitate sediu expirată"
        | 1087 -> concern "capital neechilibrat"
        | 1089 -> concern "retragere licență de export pe credit"
        | 1091 -> concern "suspendare provizorie a executării sentinței"
        | 1093 -> concern "suspendarea executării sentinței"
        | 1110 -> concern "intră sub incidența Legii nr. 359/2004"
        | 1115 -> concern "firma mamă în dizolvare"
        | 1117 -> concern "înmatriculare suspendată"
        | 1036 -> concern "trimitere în judecată"
        | 1037 -> concern "societatea mamă sub incidența Legii nr. 85/2006"
        | 1038 -> concern "societatea mamă sub incidența Legii nr. 85/2014"
        | 1146 -> concern "societatea mamă sub incidența Legii nr. 64/1995"
        | 1127 -> concern "societatea mamă sub incidența OUG nr. 91/2013"
        | 1067 -> concern "urmărire penală"
        | 1069 -> concern "condamnare penală"
        | 1128 -> concern "suspendare dizolvare (Legea nr. 286/2009)"
        | 1129 -> concern "suspendare lichidare (Legea nr. 286/2009)"
        | 1130 -> concern "suspendare fuziune (Legea nr. 286/2009)"
        | 1131 -> concern "suspendare divizare (Legea nr. 286/2009)"
        | 1132 -> concern "interzicerea unor activități (Legea nr. 286/2009)"
        | 1134 -> concern "suspendare activități (Legea nr. 286/2009)"
        | 1135 -> concern "suspendare insolvență (Legea nr. 286/2009)"
        | 1136 -> concern "suspendare faliment (Legea nr. 286/2009)"
        | 1140 -> concern "interdicția inițierii dizolvării sau lichidării (Legea nr. 286/2009)"
        | 1141 -> concern "interdicția inițierii fuziunii (Legea nr. 286/2009)"
        | 1142 -> concern "interdicția inițierii divizării (Legea nr. 286/2009)"
        | 1143 -> concern "interdicția inițierii reducerii de capital (Legea nr. 286/2009)"
        | 1147 -> concern "suspendare provizorie a executării sentinței de intrare în insolvență"
        | 1148 -> concern "suspendare provizorie a executării sentinței de intrare în faliment"
        | 1151 -> concern "suspendare/interzicere activitate (OG nr. 99/2000)"
        | 1152 -> concern "suspendare/interzicere activitate (Legea nr. 61/1991)"
        | 1153 -> concern "suspendare activitate (Legea nr. 349/2002)"
        | 2035 -> concern "hotărâre de înlocuire lichidator"
        | 2037 -> concern "cerere de radiere (art.260 din Legea nr. 31/1990) trimisă la tribunal"
        | 2038 -> concern "bilanțul anual nedepus pe anul 2009"
        | 2039 -> concern "situație financiară anuală nedepusă pe anul 2015"
        | 2040 -> concern "situație financiară anuală consolidată nedepusă pe anul 2015"
        | 2041 -> concern "declarație de inactivitate nedepusă pe anul 2015"
        | 2042 -> concern "raportări contabile nedepuse pe anul 2015"
        | 2043 -> concern "nu mai are organe statutare (art.237 alin.1 lit.a)"
        | 2044 -> concern "asociații au dispărut sau nu au domiciliul cunoscut (art.237 alin.1 lit.b)"
        | 2045 -> concern "condițiile sediului social nu mai sunt îndeplinite (art.237 alin.1 lit.c)"
        | 2046 -> concern "activitatea a încetat sau nu a fost reluată (art.237 alin.1 lit.d)"
        | 2047 -> concern "cerere de radiere (Legea nr. 314/2001) trimisă la tribunal"
        | 2048 -> concern "capitalul social necompletat (art.237 alin.1 lit.e)"
        | 2049 -> concern "cerere de fuziune transmisă la tribunal"
        | 2050 -> concern "cerere de divizare transmisă la tribunal"
        | 2051 -> concern "hotărâre judecătorească de fuziune"
        | 2052 -> concern "hotărâre judecătorească de divizare"
        | 2053 -> concern "cerere de radiere (art.237 din Legea nr. 31/1990) trimisă la tribunal"
        | 2054 -> concern "dosar trimis la tribunal pentru constatare perimare"
        | 2055 -> concern "cerere de radiere (Legea nr. 359/2004) trimisă la tribunal"
        | 2056 -> concern "dosar trimis la tribunal spre soluționare"
        | 2057 -> concern "cerere de înlocuire lichidator transmisă la tribunal"
        | 2060 -> concern "hotărâre de dizolvare judiciară (art.227 din Legea nr. 31/1990)"
        | 2062 -> concern "hotărâre de radiere (art.237 din Legea nr. 31/1990)"
        | 2065 -> concern "mandat administratori expirat"
        | 2066 -> concern "denumire indisponibilă"
        | 2067 -> concern "capital neechilibrat"
        | 2068 -> concern "certificat de înmatriculare nepreschimbat în termenul legal"
        | 2069 -> concern "sediu expirat"
        | 2070 -> concern "mandat cenzori expirat"
        | 2071 -> concern "sediu radiat"
        | 2072 -> concern "sechestru pe părți sociale sau acțiuni"
        | 2074 -> concern "bilanțul anual nedepus pe anul 2004"
        | 2075 -> concern "autorizație nepreschimbată în termenul legal"
        | 2076 -> concern "capital subscris sub limita legală"
        | 2077 -> concern "hotărâre a adunării generale privind dizolvarea"
        | 2078 -> concern "hotărâre de dizolvare judiciară (art.237 din Legea nr. 31/1990)"
        | 2079 -> concern "valabilitate mandat administratori nespecificată"
        | 2081 -> concern "recurs menționat"
        | 2083 -> concern "nu funcționează la sediul declarat"
        | 2084 -> concern "nu a fost numit administrator"
        | 2085 -> concern "valabilitate mandat cenzori nespecificată"
        | 2086 -> concern "durata societății expirată"
        | 2090 -> concern "lichidator numit fără revocarea administratorilor"
        | 2091 -> concern "cerere de dizolvare (art.237 din Legea nr. 31/1990) trimisă la tribunal"
        | 2093 -> concern "cerere de dizolvare (art.227 din Legea nr. 31/1990) trimisă la tribunal"
        | 2097 -> concern "capital neechilibrat ca urmare a denominării"
        | 2098 -> concern "notificare pe Legea nr. 64/1995"
        | 2099 -> concern "măsuri asiguratorii instituite"
        | 2100 -> concern "sechestru pe bunuri"
        | 2101 -> concern "bilanțul anual nedepus pe anul 2003"
        | 2102 -> concern "bilanțul anual nedepus pe anul 2002"
        | 2103 -> concern "bilanțul anual nedepus pe anul 2005"
        | 2104 -> concern "opoziție menționată"
        | 2105 -> concern "suspendare executare procedură insolvență"
        | 2106 -> concern "suspendare executare procedură reorganizare"
        | 2107 -> concern "suspendare executare procedură faliment"
        | 2108 -> concern "suspendare executare procedură lichidare"
        | 2109 -> concern "suspendare executare procedură dizolvare"
        | 2110 -> concern "bilanțul anual nedepus pe anul 2006"
        | 2112 -> concern "bilanțul anual nedepus pe anul 2007"
        | 2114 -> concern "bilanțul anual nedepus pe anul 2008"
        | 2115 -> concern "cerere de numire lichidator transmisă la tribunal"
        | 2118 -> concern "cerere de intervenție transmisă la tribunal"
        | 2119 -> concern "plângere transmisă la tribunal"
        | 2120 -> concern "perioadă de suspendare activitate expirată"
        | 2121 -> concern "hotărâre judecătorească în cererea de numire lichidator"
        | 2123 -> concern "neexercitarea dreptului de opțiune (OUG 44/2008)"
        | 2125 -> concern "hotărâre de radiere (Legea nr. 359/2004)"
        | 2126 -> concern "hotărâre de atragere a răspunderii patrimoniale"
        | 2127 -> concern "răspundere penală (Legea nr. 286/2009)"
        | 2130 -> concern "sechestru asupra tuturor bunurilor"
        | 2132 -> concern "confiscare specială"
        | 2135 -> concern "apel menționat"
        | 2139 -> concern "decizie a asociatului privind dizolvarea"
        | 2146 -> concern "sub incidența art.237 indice 2 din Legea nr. 31/1990"
        | 2147 -> concern "sub incidența art.260 alin.(7) din Legea nr. 31/1990"
        | 2148 -> concern "act de sediu desființat"
        | 2149 -> concern "transfer de sediu în alt stat"
        | 2151 -> concern "sancțiuni internaționale"
        | 2152 -> concern "înscrisuri desființate"
        | _ -> ValueNone

    /// The codes saying insolvency was opened, and those saying accounts weren't filed.
    let private isInsolvency (state: RomaniaState voption) =
        match state with
        | ValueSome(RomaniaState.Insolvent _) -> true
        | _ -> false

    let private accountsCodes =
        set [ 2038; 2039; 2040; 2041; 2042; 2074; 2101; 2102; 2103; 2110; 2112; 2114 ]

    /// A firm's standing from all its codes: removal, then insolvency, then dissolution, then a code not listed or a pending registration, then
    /// operation with any concerns.
    let standing (codes: int list) =
        let placed = codes |> List.map (fun code -> code, state code)
        let states = placed |> List.choose (snd >> ValueOption.toOption)

        let first pick = states |> List.tryPick pick

        let unlisted = placed |> List.filter (snd >> ValueOption.isNone) |> List.map fst

        let concerns =
            states
            |> List.choose (function
                | RomaniaState.Concern text
                | RomaniaState.OperatingConcern text -> Some text
                | _ -> None)

        let operating =
            states
            |> List.exists (function
                | RomaniaState.Operating
                | RomaniaState.OperatingConcern _ -> true
                | _ -> false)

        if List.contains RomaniaState.Removed states then
            CompanyStanding.Closed "radiată (removed from the register)"
        else
            match
                first (function
                    | RomaniaState.Insolvent text -> Some text
                    | _ -> None)
            with
            | Some proceeding -> CompanyStanding.InInsolvency proceeding
            | None ->
                match
                    first (function
                        | RomaniaState.Dissolving text -> Some text
                        | _ -> None)
                with
                | Some status -> CompanyStanding.WindingUp status
                | None ->
                    match
                        unlisted,
                        first (function
                            | RomaniaState.Pending text -> Some text
                            | _ -> None)
                    with
                    | code :: _, _ -> CompanyStanding.Unknown(ValueSome $"cod {code}")
                    | [], Some status -> CompanyStanding.Unknown(ValueSome status)
                    | [], None when operating && List.isEmpty concerns -> CompanyStanding.Active
                    | [], None when operating -> CompanyStanding.ActiveWithConcern(String.Join("; ", concerns))
                    | [], None when not (List.isEmpty concerns) ->
                        CompanyStanding.Unknown(ValueSome(String.Join("; ", concerns)))
                    | [], None -> CompanyStanding.Unknown ValueNone

    let concerns (codes: int list) =
        let stated =
            [
                if codes |> List.exists (state >> isInsolvency) then
                    CompanyConcern.InsolvencyHistory
                if codes |> List.exists accountsCodes.Contains then
                    CompanyConcern.AccountsOverdue
            ]

        {
            Stated = stated
            Unconfirmed = CompanyConcern.all |> List.except stated
        }

    /// The roles the file lists: administrators and the sole general manager are directors; every other keeps its wording.
    let officerRole (role: string voption) =
        match role with
        | ValueSome("administrator" | "administrator si reprezentant" | "administrator si conducator" | "director general unic") ->
            OfficerRole.Director
        | ValueSome other -> OfficerRole.Other other
        | ValueNone -> OfficerRole.Other "Not stated"

    /// A "dd/MM/yyyy" date at the start of a cell, which newer rows follow with a time, such as "07/09/2026 13:50:16".
    let private leadingDate (text: string voption) =
        text
        |> ValueOption.filter (fun text -> text.Length >= 10)
        |> ValueOption.bind (fun text -> BulkFile.date "dd/MM/yyyy" (ValueSome(text.Substring(0, 10))))

    let private officer (columns: IReadOnlyDictionary<string, int>) (record: DelimitedRecord) : Officer =
        let cell name = BulkFile.cell columns name record

        {
            Name = defaultValueArg (cell "PERSOANA_IMPUTERNICITA") PartyNames.NotGiven
            Kind = PartyKind.Unknown ValueNone
            Role = officerRole (cell "CALITATE")
            Current = true
            AppointedOn = ValueNone
            EndedOn = ValueNone
            BornIn =
                leadingDate (cell "DATA_NASTERE")
                |> ValueOption.map (fun born -> { Year = born.Year; Month = born.Month })
            Nationality = ValueNone
            CountryOfResidence = cell "TARA"
            Address = ValueNone
            Entity = ValueNone
            IdentityVerification = ValueNone
        }

    let private firmColumns =
        [
            "DENUMIRE"
            "CUI"
            "COD_INMATRICULARE"
            "DATA_INMATRICULARE"
            "FORMA_JURIDICA"
        ]

    let private joined (parts: string voption list) =
        match parts |> List.choose ValueOption.toOption with
        | [] -> ValueNone
        | parts -> ValueSome(String.Join(" ", parts))

    let private address (cell: string -> string voption) : CompanyAddress =
        let line label value =
            value |> ValueOption.map (fun value -> $"{label} {value}")

        {
            Kind = AddressKind.RegisteredOffice
            Address =
                {
                    Lines =
                        [
                            joined
                                [
                                    cell "ADR_DEN_STRADA"
                                    cell "ADR_NR_STRADA" |> ValueOption.map (fun number -> "nr. " + number)
                                ]
                            joined
                                [
                                    line "bl." (cell "ADR_BLOC")
                                    line "sc." (cell "ADR_SCARA")
                                    line "et." (cell "ADR_ETAJ")
                                    line "ap." (cell "ADR_APARTAMENT")
                                ]
                            cell "ADR_COMPLETARE"
                        ]
                        |> List.choose ValueOption.toOption
                    Locality = cell "ADR_LOCALITATE"
                    Region = cell "ADR_JUDET"
                    PostalCode = cell "ADR_COD_POSTAL"
                    Country = cell "ADR_TARA"
                }
        }

    /// A firm as the files hold it: its line in OD_FIRME, its state codes and its representatives' lines, as UTF-8.
    [<Struct>]
    type Held =
        {
            Line: byte array
            Codes: int array
            Representatives: byte array list
        }

    let private companyOf (names: IReadOnlyDictionary<int, string>) (cell: string -> string voption) (codes: int list) =
        let parent = cell "TARA_FIRMA_MAMA"

        {
            Name = defaultValueArg (cell "DENUMIRE") PartyNames.NotGiven
            OtherNames = []
            Number = defaultValueArg (cell "COD_INMATRICULARE") ""
            Jurisdiction = Jurisdiction.ofCode "RO"
            JurisdictionAsPublished = ValueNone
            Standing = standing codes
            Concerns = concerns codes
            StatusAsPublished =
                match codes with
                | [] -> ValueNone
                | codes ->
                    codes
                    |> List.map (fun code ->
                        match names.TryGetValue code with
                        | true, word -> word
                        | _ -> string code)
                    |> fun words -> ValueSome(String.Join("; ", words))
            LegalForm = cell "FORMA_JURIDICA"
            IncorporatedOn = leadingDate (cell "DATA_INMATRICULARE")
            CeasedOn = ValueNone
            Addresses = Available [ address cell ]
            Role =
                match parent with
                | ValueNone -> RecordRole.Home
                | ValueSome "România" -> RecordRole.NotStated
                | ValueSome _ -> RecordRole.Establishment
            HomeRegistration =
                match parent with
                | ValueSome country when country <> "România" ->
                    ValueSome
                        {
                            LegalName = ValueNone
                            LegalForm = ValueNone
                            GoverningLaw = ValueNone
                            Country = ValueSome country
                            Register = ValueNone
                            RegistrationNumber = ValueNone
                        }
                | _ -> ValueNone
            EstablishmentOf = ValueNone
        }

    /// A line of a ^-delimited file, which quotes nothing, from its UTF-8.
    let private recordOf (bytes: byte array) =
        let raw = Encoding.UTF8.GetString bytes
        { Raw = raw; Fields = raw.Split '^' }

    /// A line kept as UTF-8, half the size of a string, to be read when asked for.
    let private compact (line: string) = Encoding.UTF8.GetBytes line

    /// A registration number as written, without spaces and in upper case, to tie a state row that doesn't normalise to its firm.
    let private written (number: string) =
        number.Replace(" ", "").ToUpperInvariant()

    /// The header and lines of one of ONRC's ^-delimited files.
    let private table (file: string) (stream: Stream) =
        BulkFile.delimited file '^' false stream

    /// What the firm state file leaves beside the codes: the numbers of rows naming a firm without a readable code, as written or as keys, and
    /// the count of rows naming no number at all.
    [<Struct>]
    type private Unplaced = { Numbers: Set<string>; States: int }

    /// What the firms file leaves beside the index: the named firms read, the rows that couldn't be read, last first, and the count naming no
    /// number.
    [<Struct>]
    type private FirmsRead =
        {
            Firms: int
            Unreadable: (string * string) list
            WithoutNumber: int
        }

    /// Builds the register from the firms and firm state files, the state code list if the host passes it (for the words), and the legal
    /// representatives file if it passes that. Firms are looked up by registration number, or by CUI where one firm holds it. Each firm is held
    /// as its lines and read when asked for. The state codes, firms and CUIs are gathered in dictionaries filled here and then only read: the
    /// files hold over four million firms.
    let build
        (firms: Stream)
        (states: Stream)
        (stateNames: Stream voption)
        (representatives: Stream voption)
        (date: BulkFileDate)
        (ct: CancellationToken)
        : ICompanyRegistry =
        let names =
            match stateNames with
            | ValueSome stream ->
                let columns, rows = table "The state code list" stream
                BulkFile.requireColumns "The state code list" columns [ "COD"; "DENUMIRE" ]

                rows
                |> Seq.choose (fun row ->
                    match BulkFile.cell columns "COD" row, BulkFile.cell columns "DENUMIRE" row with
                    | ValueSome code, ValueSome name ->
                        match Int32.TryParse code with
                        | true, code -> Some(code, name)
                        | _ -> None
                    | _ -> None)
                |> readOnlyDict
            | ValueNone -> readOnlyDict []

        let codes = Dictionary<string, List<int>>()

        let unplaced =
            let columns, rows = table "The firm state file" states
            BulkFile.requireColumns "The firm state file" columns [ "COD_INMATRICULARE"; "COD" ]

            rows
            |> Seq.fold
                (fun (unplaced: Unplaced) row ->
                    ct.ThrowIfCancellationRequested()
                    let number = BulkFile.cell columns "COD_INMATRICULARE" row

                    match number |> ValueOption.bind normalRegistration, BulkFile.cell columns "COD" row with
                    | ValueSome key, ValueSome code ->
                        match Int32.TryParse code with
                        | true, code ->
                            match codes.TryGetValue key with
                            | true, held -> held.Add code
                            | _ -> codes[key] <- List<int>([ code ])

                            unplaced
                        | _ -> BulkFile.fail "The firm state file holds a code that isn't a number"
                    | ValueNone, _ when number.IsSome ->
                        { unplaced with
                            Numbers = unplaced.Numbers.Add(written number.Value)
                        }
                    | ValueSome key, ValueNone ->
                        { unplaced with
                            Numbers = unplaced.Numbers.Add key
                        }
                    | ValueNone, _ ->
                        { unplaced with
                            States = unplaced.States + 1
                        })
                { Numbers = Set.empty; States = 0 }

        let officerColumns, people =
            match representatives with
            | ValueSome stream ->
                let columns, rows = table "The legal representatives file" stream

                BulkFile.requireColumns
                    "The legal representatives file"
                    columns
                    [ "COD_INMATRICULARE"; "PERSOANA_IMPUTERNICITA"; "CALITATE" ]

                columns,
                rows
                |> Seq.choose (fun row ->
                    ct.ThrowIfCancellationRequested()

                    BulkFile.cell columns "COD_INMATRICULARE" row
                    |> ValueOption.bind normalRegistration
                    |> ValueOption.map (fun key -> struct (key, compact row.Raw))
                    |> ValueOption.toOption)
                |> Bulk.grouped
            | ValueNone -> readOnlyDict [], readOnlyDict []

        let columns, rows = table "The firms file" firms
        BulkFile.requireColumns "The firms file" columns firmColumns
        let index = BulkIndex<Held>()
        // A CUI names the firm whose rows give it; one given by rows of two firms names neither.
        let cuis = BulkIndex<string>()
        // Every row's key, readable or not, so a later row for it is seen as a repeat.
        let seen = HashSet<string>()

        let noteCui (key: string) (cui: string voption) =
            match cui with
            | ValueSome cui ->
                match cuis.Held.TryGetValue cui with
                | true, other when other = key -> ()
                | _ -> cuis.Add(cui, key)
            | ValueNone -> ()

        let read =
            rows
            |> Seq.fold
                (fun (read: FirmsRead) row ->
                    ct.ThrowIfCancellationRequested()
                    let cell name = BulkFile.cell columns name row

                    match
                        cell "COD_INMATRICULARE", cell "COD_INMATRICULARE" |> ValueOption.bind normalRegistration
                    with
                    | ValueSome number, ValueSome key ->
                        noteCui key (cell "CUI" |> ValueOption.bind normalCui)
                        let again = not (seen.Add key)

                        match cell "DENUMIRE" with
                        | _ when unplaced.Numbers.Contains(written number) || unplaced.Numbers.Contains key ->
                            index.Withhold key |> ignore

                            { read with
                                Unreadable =
                                    (number, "The firm state file holds a row for this number that can't be read")
                                    :: read.Unreadable
                            }
                        | ValueNone ->
                            if index.Withhold key then
                                index.Repeat key

                            { read with
                                Unreadable =
                                    (number, "The file's record for this number has no name") :: read.Unreadable
                            }
                        | ValueSome _ when index.Repeated.Contains key -> read
                        | ValueSome _ when again ->
                            index.Repeat key
                            read
                        | ValueSome _ ->
                            index.Add(
                                key,
                                {
                                    Line = compact row.Raw
                                    Codes =
                                        match codes.TryGetValue key with
                                        | true, found -> found.ToArray()
                                        | _ -> [||]
                                    Representatives = Bulk.listed people key
                                }
                            )

                            { read with Firms = read.Firms + 1 }
                    | _ ->
                        { read with
                            WithoutNumber = read.WithoutNumber + 1
                        })
                {
                    Firms = 0
                    Unreadable = []
                    WithoutNumber = 0
                }

        if index.Held.Count = 0 && index.Repeated.Count = 0 && read.Unreadable.IsEmpty then
            BulkFile.fail "The firms file holds no firm"

        let nameOf (firm: Held) =
            BulkFile.cell columns "DENUMIRE" (recordOf firm.Line)
            |> ValueOption.defaultValue PartyNames.NotGiven

        let build (firm: Held) : BulkRecord =
            let record = recordOf firm.Line

            {
                Company = companyOf names (fun name -> BulkFile.cell columns name record) (List.ofArray firm.Codes)
                Officers =
                    match firm.Representatives with
                    | [] -> ValueNone
                    | lines -> ValueSome(lines |> List.map (recordOf >> officer officerColumns))
                Owners = ValueNone
                Raw = record.Raw
            }

        // A registration number as itself; a CUI as the registration number of the one firm holding it, or as a key no firm has.
        let normal (number: string) =
            match normalRegistration number with
            | ValueSome key -> ValueSome key
            | ValueNone ->
                normalCui number
                |> ValueOption.map (fun cui ->
                    match cuis.Held.TryGetValue cui with
                    | true, key -> key
                    | _ -> cuiKey cui)

        let repeatedCui =
            cuis.Repeated
            |> Seq.map (fun cui -> cui, "Several firms in the file hold this CUI; ask by registration number")

        BulkAnswers.Over(
            coverage representatives.IsSome unplaced.States,
            normal,
            "A Romanian firm is asked by registration number, such as J40/123/2020 or J2020000123400, or by CUI",
            {
                Held = LazyRecords<Held>(index.Held, build, nameOf)
                Repeated = index.Repeated
                Records = read.Firms
                Unreadable = List.rev read.Unreadable
                WithoutNumber = read.WithoutNumber
            },
            date,
            BulkAbsence.NotHeld,
            (if representatives.IsSome then
                 "The legal representatives file lists no one for this firm"
             else
                 "The host passed no legal representatives file"),
            (fun purpose ->
                SourceAnswer.restrictedOwners
                    purpose
                    "Unverified: those the ONRC beneficial owner register rules allow, such as obliged entities under anti-money-laundering rules"
                    "Apply to ONRC for beneficial owner data, or use a vendor"),
            repeatedCui,
            cuis.Held |> Seq.map (fun pair -> struct (cuiKey pair.Key, pair.Value)),
            cantTellAbsent
        )
