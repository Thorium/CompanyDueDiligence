namespace CompanyDueDiligence

open System
open System.Collections.Generic
open System.IO
open System.IO.Compression
open System.Text.RegularExpressions
open System.Threading
open System.Threading.Tasks

/// The Crossroads Bank for Enterprises' open data full file (KboOpenData_<n>_<year>_<month>_Full.zip), written from Cookbook BCE Open Data
/// R018.00 and the code tables in KBO-codes-legal.xlsx. Not run live: the file needs a registration, which this adapter's author didn't make.
module internal Belgium =

    [<Literal>]
    let Registry = "CBE/KBO open data (Belgium)"

    let coverage (addresses: bool) : RegistryCoverage =
        {
            Registry = Registry
            Jurisdictions = [ Jurisdiction.ofCode "BE" ]
            Standing = Support.Partial "Active entities only, with their juridical situation"
            Concerns =
                Support.Partial "Bankruptcy, judicial reorganisation and a struck-off registered office are stated"
            Addresses =
                if addresses then
                    Support.Partial "Legal persons only: the file gives no registered address for a natural person"
                else
                    Support.NotOffered "The zip holds no address.csv"
            Search = Support.Partial "By name within the file, up to 50 matches"
            Officers = Support.NotOffered "The open data holds no functions"
            Owners = Support.NotOffered "The open data holds no shareholders; the UBO register is restricted"
            Cost =
                AccessCost.FreeWithKey
                    "Register at kbopub.economie.fgov.be/kbo-open-data and accept the licence to download the file"
            RateLimit = RateLimitPolicy.NotPublished(1, TimeSpan.FromDays 1.)
            Licence =
                "Licence BCE Open Data: free reuse for the purpose stated at registration, no direct marketing with personal data, source and update date cited. Not run live: the file wasn't downloaded"
        }

    let private numberRegex = Regex @"\A(?:BE)?0?([01][0-9]{9}|[1-9][0-9]{8})\z"

    /// An enterprise number: ten digits starting 0 or 1 (nine are read with a leading 0), whose last two are 97 less the first eight modulo 97.
    /// Written as the file writes it, 0123.456.789.
    let normalNumber (number: string) =
        let normal =
            if isNull number then
                ""
            else
                number.Trim().ToUpperInvariant().Replace(".", "").Replace(" ", "")

        let found = numberRegex.Match normal

        if found.Success then
            let digits = found.Groups[1].Value.PadLeft(10, '0')
            let body = Int64.Parse(digits.AsSpan(0, 8))
            let check = Int32.Parse(digits.AsSpan(8, 2))

            if 97L - body % 97L = int64 check then
                ValueSome $"{digits.Substring(0, 4)}.{digits.Substring(4, 3)}.{digits.Substring(7, 3)}"
            else
                ValueNone
        else
            ValueNone

    /// The Status and JuridicalSituation codes of the code table. The file holds AC entities only; a code not listed is Unknown.
    let standing (status: string voption) (situation: string voption) (described: string voption) =
        let word = described |> ValueOption.orElse situation

        let label (english: string) =
            match described with
            | ValueSome text -> $"{english} ({text})"
            | ValueNone -> english

        match status with
        | ValueSome "AF" -> CompanyStanding.Closed "Closed (Clôturé)"
        | ValueSome "AN" -> CompanyStanding.Closed "Cancelled (Annulé)"
        | ValueSome "ST" -> CompanyStanding.Closed "Stopped (Arrêté)"
        | ValueSome "BK" -> CompanyStanding.Registered(ValueSome "Identifié")
        | ValueSome "JU" -> CompanyStanding.Registered(ValueSome "Création juridique")
        | ValueSome "AC" ->
            match situation with
            | ValueSome("000" | "001" | "002" | "003" | "020" | "090") -> CompanyStanding.Active
            | ValueSome "100" -> CompanyStanding.Registered(ValueSome(label "Entity identification"))
            | ValueSome "042" -> CompanyStanding.ActiveWithConcern(label "Deferment of payment revoked")
            | ValueSome "043" -> CompanyStanding.ActiveWithConcern(label "End of deferment of payment")
            | ValueSome "010" -> CompanyStanding.WindingUp(label "Dissolution by operation of law")
            | ValueSome "012" -> CompanyStanding.WindingUp(label "Voluntary dissolution, liquidation")
            | ValueSome "013" -> CompanyStanding.WindingUp(label "Judicial dissolution or nullity")
            | ValueSome "112" -> CompanyStanding.WindingUp(label "Reopening of liquidation")
            | ValueSome "014" -> CompanyStanding.Dissolved
            | ValueSome("030" | "031") -> CompanyStanding.InInsolvency(label "Judicial settlement")
            | ValueSome("040" | "041") -> CompanyStanding.InInsolvency(label "Deferment of payment")
            | ValueSome("048" | "049" | "050") -> CompanyStanding.InInsolvency(label "Bankruptcy opened")
            | ValueSome "091" -> CompanyStanding.InInsolvency(label "Judicial reorganisation")
            | ValueSome("051" | "052" | "053") -> CompanyStanding.Closed(label "Bankruptcy closed")
            | ValueSome "006" -> CompanyStanding.Closed(label "Cessation due to number replacement")
            | ValueSome "011" -> CompanyStanding.Closed(label "Cessation of activities in Belgium")
            | ValueSome "015" -> CompanyStanding.Closed(label "Cessation of an entity without legal personality")
            | ValueSome "016" -> CompanyStanding.Closed(label "Cessation of activity")
            | ValueSome "017" -> CompanyStanding.Closed(label "Transfer of a natural person entity")
            | ValueSome "018" -> CompanyStanding.Closed(label "Cessation during identification")
            | ValueSome "019" -> CompanyStanding.Closed(label "Cessation of an EDRL or non-EU entity")
            | ValueSome("021" | "022") -> CompanyStanding.Closed(label "Merger")
            | ValueSome("023" | "024" | "025" | "026") -> CompanyStanding.Closed(label "Division")
            | ValueSome "111" -> CompanyStanding.Closed(label "Struck off after cessation in an EEA register")
            | ValueSome "999" -> CompanyStanding.Closed(label "Cancelled file")
            | _ -> CompanyStanding.Unknown word
        | other -> CompanyStanding.Unknown other

    /// The legal forms of foreign entities in the code table.
    let private foreignForms = set [ "023"; "030"; "160"; "230"; "235"; "453"; "454" ]

    let private dateOf (text: string voption) = BulkFile.date "dd-MM-yyyy" text

    /// code.csv as descriptions by category and code: the last French one, else the first Dutch one.
    let private readCodes (stream: Stream) =
        let columns, rows = BulkFile.delimited "code.csv" ',' true stream
        BulkFile.requireColumns "code.csv" columns [ "Category"; "Code"; "Language"; "Description" ]

        rows
        |> Seq.choose (fun row ->
            let cell name = BulkFile.cell columns name row

            match cell "Category", cell "Code", cell "Language", cell "Description" with
            | ValueSome category, ValueSome code, ValueSome("FR" | "NL" as language), ValueSome description ->
                Some(struct (category, code), (language, description))
            | _ -> None)
        |> Seq.groupBy fst
        |> Seq.map (fun (key, entries) ->
            let inLanguage language =
                entries
                |> Seq.filter (fun (_, (held, _)) -> held = language)
                |> Seq.map (snd >> snd)

            key,
            inLanguage "FR"
            |> Seq.tryLast
            |> Option.orElse (inLanguage "NL" |> Seq.tryHead)
            |> Option.get)
        |> readOnlyDict

    let private entry (archive: ZipArchive) (name: string) =
        archive.Entries
        |> Seq.tryFind (fun entry -> String.Equals(entry.Name, name, StringComparison.OrdinalIgnoreCase))

    let private withEntry (archive: ZipArchive) (name: string) (read: Stream -> 'T) =
        entry archive name
        |> Option.map (fun entry ->
            use stream = BulkFile.openEntry entry
            read stream)

    /// meta.csv's SnapshotDate.
    let private snapshot (stream: Stream) =
        let columns, rows = BulkFile.delimited "meta.csv" ',' true stream

        rows
        |> Seq.tryPick (fun row ->
            match BulkFile.cell columns "Variable" row with
            | ValueSome "SnapshotDate" -> Some(dateOf (BulkFile.cell columns "Value" row))
            | _ -> None)
        |> Option.defaultValue ValueNone

    /// An enterprise's name and its other names.
    type private Denominations =
        {
            Name: string voption
            Others: string list
        }

    /// Names by enterprise number: the first TypeOfDenomination 001 is the name; other languages, abbreviations (002), commercial names (003)
    /// and later 001s are other names, each once.
    let private readDenominations (ct: CancellationToken) (stream: Stream) =
        let columns, rows = BulkFile.delimited "denomination.csv" ',' true stream

        BulkFile.requireColumns
            "denomination.csv"
            columns
            [ "EntityNumber"; "Language"; "TypeOfDenomination"; "Denomination" ]

        rows
        |> Seq.choose (fun row ->
            ct.ThrowIfCancellationRequested()
            let cell name = BulkFile.cell columns name row

            match cell "EntityNumber", cell "TypeOfDenomination", cell "Denomination" with
            | ValueSome number, ValueSome kind, ValueSome text when number.Length = 12 ->
                Some(struct (number, (kind, text)))
            | _ -> None)
        |> Bulk.grouped
        |> Seq.map (fun pair ->
            let named = pair.Value |> List.tryFindIndex (fst >> (=) "001")

            pair.Key,
            {
                Name =
                    named
                    |> Option.map (fun position -> snd (List.item position pair.Value))
                    |> ValueOption.ofOption
                Others =
                    pair.Value
                    |> List.indexed
                    |> List.filter (fun (position, _) -> Some position <> named)
                    |> List.map (snd >> snd)
                    |> List.distinct
            })
        |> readOnlyDict

    /// Addresses by enterprise number; REGO is the registered office. A struck-off address is kept with its kind and flagged.
    let private readAddresses (ct: CancellationToken) (stream: Stream) =
        let columns, rows = BulkFile.delimited "address.csv" ',' true stream
        BulkFile.requireColumns "address.csv" columns [ "EntityNumber"; "TypeOfAddress" ]

        rows
        |> Seq.choose (fun row ->
            ct.ThrowIfCancellationRequested()
            let cell name = BulkFile.cell columns name row

            match cell "EntityNumber" with
            | ValueSome number when number.Length = 12 ->
                let street =
                    [
                        cell "StreetFR" |> ValueOption.orElse (cell "StreetNL")
                        cell "HouseNumber"
                        cell "Box" |> ValueOption.map (fun box -> "box " + box)
                    ]
                    |> List.choose ValueOption.toOption
                    |> fun parts -> String.Join(" ", parts)

                let address =
                    {
                        Kind =
                            match cell "TypeOfAddress" with
                            | ValueSome "REGO" -> AddressKind.RegisteredOffice
                            | ValueSome other -> AddressKind.Other other
                            | ValueNone -> AddressKind.Other "Not stated"
                        Address =
                            {
                                Lines =
                                    [
                                        (if street = "" then ValueNone else ValueSome street)
                                        cell "ExtraAddressInfo"
                                    ]
                                    |> List.choose ValueOption.toOption
                                Locality = cell "MunicipalityFR" |> ValueOption.orElse (cell "MunicipalityNL")
                                Region = ValueNone
                                PostalCode = cell "Zipcode"
                                Country = cell "CountryFR" |> ValueOption.orElse (cell "CountryNL")
                            }
                    }

                Some(struct (number, (address, (cell "DateStrikingOff").IsSome)))
            | _ -> None)
        |> Bulk.grouped

    let private insolvencySituations =
        set
            [
                "030"
                "031"
                "040"
                "041"
                "042"
                "048"
                "049"
                "050"
                "051"
                "052"
                "053"
                "091"
            ]

    /// Builds the register from the full zip as published: enterprise.csv and denomination.csv, with address.csv, code.csv and meta.csv when the
    /// zip holds them. The snapshot date in meta.csv is the as-of date when the host gives none.
    let build (zip: Stream) (date: BulkFileDate) (ct: CancellationToken) : ICompanyRegistry =
        use archive = BulkFile.openZip zip

        let codes =
            withEntry archive "code.csv" readCodes
            |> Option.defaultWith (fun () -> readOnlyDict [])

        let describe category code =
            code
            |> ValueOption.bind (fun code ->
                match codes.TryGetValue(struct (category, code)) with
                | true, text -> ValueSome text
                | _ -> ValueNone)

        let date =
            match date.AsOf with
            | ValueSome _ -> date
            | ValueNone ->
                { date with
                    AsOf = withEntry archive "meta.csv" snapshot |> Option.defaultValue ValueNone
                }

        let names =
            match withEntry archive "denomination.csv" (readDenominations ct) with
            | Some names -> names
            | None -> BulkFile.fail "The zip holds no denomination.csv"

        let addresses = withEntry archive "address.csv" (readAddresses ct)

        let row (columns: IReadOnlyDictionary<string, int>) (row: DelimitedRecord) =
            ct.ThrowIfCancellationRequested()
            let cell name = BulkFile.cell columns name row

            match cell "EnterpriseNumber" with
            | ValueNone -> BulkRow.WithoutNumber
            | ValueSome number ->
                let held =
                    match names.TryGetValue number with
                    | true, held -> held.Name |> ValueOption.map (fun name -> name, held.Others)
                    | _ -> ValueNone

                match held with
                | ValueNone -> BulkRow.Unreadable(number, "denomination.csv holds no name for this enterprise")
                | ValueSome(name, others) ->
                    let situation = cell "JuridicalSituation"
                    let form = cell "JuridicalFormCAC" |> ValueOption.orElse (cell "JuridicalForm")

                    let listed =
                        addresses
                        |> Option.map (fun addresses -> Bulk.listed addresses number)
                        |> Option.defaultValue []

                    let stated =
                        [
                            if situation |> ValueOption.exists insolvencySituations.Contains then
                                CompanyConcern.InsolvencyHistory
                            if
                                listed
                                |> List.exists (fun (address, struckOff) ->
                                    struckOff && address.Kind = AddressKind.RegisteredOffice)
                            then
                                CompanyConcern.RegisteredOfficeUndeliverable
                        ]

                    BulkRow.Record
                        {
                            Company =
                                {
                                    Name = name
                                    OtherNames = others |> List.filter ((<>) name)
                                    Number = number
                                    Jurisdiction = Jurisdiction.ofCode "BE"
                                    JurisdictionAsPublished = ValueNone
                                    Standing =
                                        standing (cell "Status") situation (describe "JuridicalSituation" situation)
                                    Concerns =
                                        {
                                            Stated = stated
                                            Unconfirmed = CompanyConcern.all |> List.except stated
                                        }
                                    StatusAsPublished =
                                        describe "JuridicalSituation" situation |> ValueOption.orElse situation
                                    LegalForm = describe "JuridicalForm" form |> ValueOption.orElse form
                                    IncorporatedOn = dateOf (cell "StartDate")
                                    CeasedOn = ValueNone
                                    Addresses =
                                        match addresses, listed with
                                        | None, _ -> NotAvailable "The zip holds no address.csv"
                                        | Some _, [] when cell "TypeOfEnterprise" = ValueSome "1" ->
                                            NotAvailable "The file gives no registered address for a natural person"
                                        | Some _, [] -> NotAvailable "address.csv holds no address for this enterprise"
                                        | Some _, listed -> Available(listed |> List.map fst)
                                    Role =
                                        match form with
                                        | ValueSome form when foreignForms.Contains form ->
                                            RecordRole.ForeignRegistration
                                        | ValueSome _ -> RecordRole.Home
                                        | ValueNone when cell "TypeOfEnterprise" = ValueSome "1" -> RecordRole.Home
                                        | ValueNone -> RecordRole.NotStated
                                    HomeRegistration = ValueNone
                                    EstablishmentOf = ValueNone
                                }
                            Officers = ValueNone
                            Owners = ValueNone
                            Raw = row.Raw
                        }

        let tally =
            match entry archive "enterprise.csv" with
            | None -> BulkFile.fail "The zip holds no enterprise.csv"
            | Some enterprises ->
                use stream = BulkFile.openEntry enterprises
                let columns, rows = BulkFile.delimited "enterprise.csv" ',' true stream

                BulkFile.requireColumns
                    "enterprise.csv"
                    columns
                    [
                        "EnterpriseNumber"
                        "Status"
                        "JuridicalSituation"
                        "TypeOfEnterprise"
                        "StartDate"
                    ]

                rows |> Seq.map (row columns) |> Bulk.tally normalNumber

        if tally.Records = 0 then
            BulkFile.fail "enterprise.csv holds no enterprise"

        BulkAnswers.Over(
            coverage addresses.IsSome,
            normalNumber,
            "An enterprise number has ten digits starting 0 or 1, with a valid check",
            tally,
            date,
            BulkAbsence.CantTell "The open data file holds active entities only; stopped and closed ones are left out",
            "The open data holds no functions",
            (fun purpose ->
                SourceAnswer.restrictedOwners
                    purpose
                    "Unverified: those the Belgian UBO register rules allow, such as obliged entities under anti-money-laundering rules"
                    "Consult the UBO register through MyMinfin (FPS Finance), or use a vendor")
        )
