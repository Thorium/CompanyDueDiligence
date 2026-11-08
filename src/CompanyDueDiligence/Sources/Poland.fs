namespace CompanyDueDiligence

open System
open System.Globalization
open System.Text.Json
open System.Text.RegularExpressions
open System.Threading
open CompanyDueDiligence.Json

/// The Ministry of Justice's KRS open API (OdpisAktualny and OdpisPelny), read as it answered on 2026-11-08, with the status list its
/// Portal Rejestrów Sądowych page gives: 200, 404 "podmiot nie znaleziony", 5xx.
module internal Poland =

    let coverage: RegistryCoverage =
        {
            Registry = "KRS (Poland)"
            Jurisdictions = [ Jurisdiction.ofCode "PL" ]
            Standing =
                Support.Partial
                    "Read from section 6 of the current extract: liquidation, dissolution and insolvency entries. With none it is Registered with no word; a deleted entity is read from the full extract"
            Concerns =
                Support.Partial
                    "Bankruptcy or restructuring entries in section 6 are stated; overdue filings aren't read"
            Addresses = Support.Partial "The seat's address, from the current extract"
            Search = Support.NotOffered "The API looks up a KRS number only"
            Officers =
                Support.Partial
                    "The board, supervisory body, prokurents and liquidators in the current extract. People's names are masked, so they come through as withheld"
            Owners =
                Support.Partial
                    "Partners of an sp. z o.o. holding 10% of the capital or more, and a sole shareholder of an S.A.: legal owners, with a gap unless one holds every share. People's names are masked"
            Cost = AccessCost.Free
            RateLimit = RateLimitPolicy.NotPublished(1, TimeSpan.FromSeconds 1.)
            Licence = "None named; reuse under the 2021 open data act, art. 24, per the KRS API page"
        }

    /// Not finding a number in either register's extracts: deleted entities can answer the same way, so it doesn't say the number was never used.
    [<Literal>]
    let notFound =
        "Neither the entrepreneurs' nor the associations' register has an extract under this number; an entity deleted long ago can answer the same, so this doesn't say the number isn't held"

    let private numberRegex = Regex @"\A[0-9]{1,10}\z"

    /// Up to ten digits, padded with zeros to ten as the register writes them. All zeros is no number.
    let normalNumber (number: string) =
        let normal = if isNull number then "" else number.Replace(" ", "")

        if numberRegex.IsMatch normal && normal.TrimStart '0' <> "" then
            ValueSome(normal.PadLeft(10, '0'))
        else
            ValueNone

    /// A "dd.MM.yyyy" date, as the extracts write them.
    let private polishDate name e =
        tryString name e
        |> ValueOption.bind (fun text ->
            match
                DateOnly.TryParseExact(text.Trim(), "dd.MM.yyyy", CultureInfo.InvariantCulture, DateTimeStyles.None)
            with
            | true, date -> ValueSome date
            | _ -> ValueNone)

    /// The sections of part 6 that hold something.
    let sectionsOf (part6: JsonElement voption) =
        match part6 with
        | ValueSome part when part.ValueKind = JsonValueKind.Object ->
            part.EnumerateObject()
            |> Seq.filter (fun section ->
                match section.Value.ValueKind with
                | JsonValueKind.Null
                | JsonValueKind.Undefined -> false
                | JsonValueKind.Array -> section.Value.GetArrayLength() > 0
                | JsonValueKind.Object -> section.Value.EnumerateObject() |> Seq.isEmpty |> not
                | _ -> true)
            |> Seq.map _.Name
            |> List.ofSeq
        | _ -> []

    /// Part 6 of the extract decides: insolvency, then liquidation, then dissolution. A merger, division or transformation entry is history and
    /// places nothing. Any other section is Unknown, and with none the entity is on the register without a status word.
    let standing (sections: string list) =
        let has (part: string) =
            sections
            |> List.exists (fun name -> name.Contains(part, StringComparison.OrdinalIgnoreCase))

        let others =
            sections
            |> List.filter (fun name ->
                not (
                    name.Contains("upadlos", StringComparison.OrdinalIgnoreCase)
                    || name.Contains("restrukturyzac", StringComparison.OrdinalIgnoreCase)
                    || name.Contains("naprawcz", StringComparison.OrdinalIgnoreCase)
                    || name = "likwidacja"
                    || name = "rozwiazanieUniewaznienie"
                    || name = "polaczeniePodzialPrzeksztalcenie"
                ))

        match () with
        | _ when has "upadlos" -> CompanyStanding.InInsolvency "Upadłość (bankruptcy)"
        | _ when has "restrukturyzac" || has "naprawcz" ->
            CompanyStanding.InInsolvency "Restrukturyzacja (restructuring)"
        | _ when has "likwidacja" -> CompanyStanding.WindingUp "Likwidacja (liquidation)"
        | _ when has "rozwiazanieUniewaznienie" ->
            CompanyStanding.WindingUp "Rozwiązanie lub unieważnienie (dissolution or annulment)"
        | _ ->
            match others with
            | [] -> CompanyStanding.Registered ValueNone
            | first :: _ -> CompanyStanding.Unknown(ValueSome first)

    let private extract (root: JsonElement) =
        match tryObject "odpis" root with
        | ValueSome odpis -> odpis
        | ValueNone -> raise (InvalidJson "The answer has no extract")

    let private part (name: string) (odpis: JsonElement) =
        tryObject "dane" odpis |> ValueOption.bind (tryObject name)

    let private addressOf (e: JsonElement) : Address =
        let street =
            [
                SourceAnswer.trimmed "ulica" e
                SourceAnswer.trimmed "nrDomu" e
                SourceAnswer.trimmed "nrLokalu" e
                |> ValueOption.map (fun unit -> "lok. " + unit)
            ]
            |> List.choose ValueOption.toOption
            |> String.concat " "

        {
            Lines = if street = "" then [] else [ street ]
            Locality = SourceAnswer.trimmed "miejscowosc" e
            Region = ValueNone
            PostalCode = SourceAnswer.trimmed "kodPocztowy" e
            Country = SourceAnswer.trimmed "kraj" e
        }

    /// A branch of a foreign entrepreneur names itself so in its legal form.
    let private role (form: string voption) =
        match form with
        | ValueSome word when word.Contains("ZAGRANICZN", StringComparison.OrdinalIgnoreCase) ->
            RecordRole.ForeignRegistration
        | ValueSome _ -> RecordRole.Home
        | ValueNone -> RecordRole.NotStated

    let private numberOf (header: JsonElement) = required "numerKRS" tryString header

    /// A current extract. The number and name are required.
    let company (root: JsonElement) : Company =
        let odpis = extract root

        let header =
            match tryObject "naglowekA" odpis with
            | ValueSome header -> header
            | ValueNone -> raise (InvalidJson "The extract has no header")

        let subject =
            part "dzial1" odpis
            |> ValueOption.bind (tryObject "danePodmiotu")
            |> ValueOption.defaultWith (fun () -> raise (InvalidJson "The extract has no entity data"))

        let sections = sectionsOf (part "dzial6" odpis)
        let standing = standing sections
        let form = SourceAnswer.trimmed "formaPrawna" subject

        let address =
            part "dzial1" odpis
            |> ValueOption.bind (tryObject "siedzibaIAdres")
            |> ValueOption.bind (tryObject "adres")

        {
            Name = required "nazwa" (fun name e -> SourceAnswer.trimmed name e) subject
            OtherNames = []
            Number = numberOf header
            Jurisdiction = Jurisdiction.ofCode "PL"
            JurisdictionAsPublished = ValueNone
            Standing = standing
            Concerns =
                match standing with
                | CompanyStanding.InInsolvency _ ->
                    {
                        Stated = [ CompanyConcern.InsolvencyHistory ]
                        Unconfirmed = CompanyConcern.all |> List.except [ CompanyConcern.InsolvencyHistory ]
                    }
                | _ -> CompanyConcerns.notPublished
            StatusAsPublished =
                match sections with
                | [] -> ValueNone
                | names -> ValueSome("dzial6: " + String.concat ", " names)
            LegalForm = form
            IncorporatedOn = ValueNone
            CeasedOn = ValueNone
            Addresses =
                match address with
                | ValueSome seat ->
                    Available
                        [
                            {
                                Kind = AddressKind.RegisteredOffice
                                Address = addressOf seat
                            }
                        ]
                | ValueNone -> NotAvailable "The extract holds no address"
            Role = role form
            HomeRegistration = ValueNone
            EstablishmentOf = ValueNone
        }

    let asOf (root: JsonElement) =
        tryObject "odpis" root
        |> ValueOption.bind (fun odpis ->
            tryObject "naglowekA" odpis |> ValueOption.orElse (tryObject "naglowekP" odpis))
        |> ValueOption.bind (polishDate "stanZDnia")

    let lookup (number: string) (root: JsonElement) =
        let found = company root

        if found.Number = number then
            found
        else
            raise (InvalidJson "The answer holds another entity")

    /// The last value of a full-extract field, which lists every value with the entries that brought it in and took it out.
    let private lastValue (field: string) (e: JsonElement) =
        array e field
        |> List.choose (fun entry -> SourceAnswer.trimmed field entry |> ValueOption.toOption)
        |> List.tryLast
        |> ValueOption.ofOption

    /// A full extract of an entity with no current one, which is what a deleted entity has. Its entry list says when it was deleted.
    let deleted (number: string) (root: JsonElement) : Company =
        let odpis = extract root

        let header =
            match tryObject "naglowekP" odpis with
            | ValueSome header -> header
            | ValueNone -> raise (InvalidJson "The full extract has no header")

        let held = numberOf header

        if held <> number then
            raise (InvalidJson "The answer holds another entity")

        let subject =
            part "dzial1" odpis
            |> ValueOption.bind (tryObject "danePodmiotu")
            |> ValueOption.defaultWith (fun () -> raise (InvalidJson "The extract has no entity data"))

        let deletion =
            array header "wpis"
            |> List.tryFindBack (fun entry ->
                match tryString "opis" entry with
                | ValueSome text -> text.StartsWith("WYKREŚLENIE", StringComparison.OrdinalIgnoreCase)
                | ValueNone -> false)
            |> ValueOption.ofOption

        let form = lastValue "formaPrawna" subject

        {
            Name =
                lastValue "nazwa" subject
                |> ValueOption.defaultWith (fun () -> raise (InvalidJson "Missing or invalid field 'nazwa'"))
            OtherNames =
                array subject "nazwa"
                |> List.choose (fun entry -> SourceAnswer.trimmed "nazwa" entry |> ValueOption.toOption)
                |> List.distinct
                |> fun names -> names |> List.take (max 0 (names.Length - 1))
            Number = held
            Jurisdiction = Jurisdiction.ofCode "PL"
            JurisdictionAsPublished = ValueNone
            Standing =
                match deletion with
                | ValueSome entry ->
                    CompanyStanding.Closed(tryString "opis" entry |> ValueOption.defaultValue "WYKREŚLENIE")
                | ValueNone -> CompanyStanding.Unknown(ValueSome "No current extract")
            Concerns = CompanyConcerns.notPublished
            StatusAsPublished = deletion |> ValueOption.bind (tryString "opis")
            LegalForm = form
            IncorporatedOn = ValueNone
            CeasedOn = deletion |> ValueOption.bind (polishDate "dataWpisu")
            Addresses = NotAvailable "The entity has no current extract, so no current address"
            Role = role form
            HomeRegistration = ValueNone
            EstablishmentOf = ValueNone
        }

    let private isMasked (text: string) = text.Contains '*'

    /// A person or a legal person in an extract. A person's masked name is withheld; the register shows its first letters only.
    let private party (e: JsonElement) =
        let person =
            [
                tryObject "imiona" e |> ValueOption.bind (tryString "imie")
                tryObject "imiona" e |> ValueOption.bind (tryString "imieDrugie")
                tryObject "nazwisko" e |> ValueOption.bind (tryString "nazwiskoICzlon")
            ]
            |> List.choose ValueOption.toOption
            |> String.concat " "

        match SourceAnswer.trimmed "nazwa" e, person with
        | ValueSome name, _ ->
            let number =
                tryObject "krs" e
                |> ValueOption.bind (tryString "krs")
                |> ValueOption.filter (fun krs -> krs.TrimStart '0' <> "")

            name,
            PartyKind.LegalEntity,
            ValueSome
                {
                    LegalName = ValueSome name
                    LegalForm = ValueNone
                    GoverningLaw = ValueNone
                    Country = number |> ValueOption.map (fun _ -> "PL")
                    Register = number |> ValueOption.map (fun _ -> coverage.Registry)
                    RegistrationNumber = number
                }
        | ValueNone, "" -> PartyNames.NotGiven, PartyKind.Unknown ValueNone, ValueNone
        | ValueNone, name when isMasked name -> PartyNames.NotPublished, PartyKind.Withheld, ValueNone
        | ValueNone, name -> name, PartyKind.Individual, ValueNone

    let private officer (role: OfficerRole) (e: JsonElement) : Officer =
        let name, kind, entity = party e

        {
            Name = name
            Kind = kind
            Role = role
            Current = true
            AppointedOn = ValueNone
            EndedOn = ValueNone
            BornIn = ValueNone
            Nationality = ValueNone
            CountryOfResidence = ValueNone
            Address = ValueNone
            Entity = entity
            IdentityVerification = ValueNone
        }

    /// Everyone the current extract lists, all current. A board member's function is kept; the board itself makes a director.
    let officers (root: JsonElement) =
        let odpis = extract root
        let part2 = part "dzial2" odpis
        let part6 = part "dzial6" odpis

        let boards =
            part2
            |> ValueOption.map (fun part -> oneOrMany "reprezentacja" part)
            |> ValueOption.defaultValue []
            |> List.collect (fun body ->
                let isBoard =
                    match tryString "nazwaOrganu" body with
                    | ValueSome name -> name.Contains("ZARZĄD", StringComparison.OrdinalIgnoreCase)
                    | ValueNone -> false

                array body "sklad"
                |> List.map (fun person ->
                    let duty = tryString "funkcjaWOrganie" person |> ValueOption.defaultValue ""

                    officer
                        (if isBoard then
                             OfficerRole.Director
                         else
                             OfficerRole.Other duty)
                        person))

        let supervisors =
            part2
            |> ValueOption.map (fun part -> oneOrMany "organNadzoru" part)
            |> ValueOption.defaultValue []
            |> List.collect (fun body ->
                let name = tryString "nazwa" body |> ValueOption.defaultValue "ORGAN NADZORU"
                array body "sklad" |> List.map (officer (OfficerRole.Other name)))

        let prokurents =
            part2
            |> ValueOption.map (fun part -> array part "prokurenci")
            |> ValueOption.defaultValue []
            |> List.map (fun person ->
                let kind =
                    tryString "rodzajProkury" person
                    |> ValueOption.map (fun kind -> $" ({kind})")
                    |> ValueOption.defaultValue ""

                officer (OfficerRole.Other("PROKURENT" + kind)) person)

        let liquidators =
            part6
            |> ValueOption.map (fun part -> oneOrMany "likwidacja" part)
            |> ValueOption.defaultValue []
            |> List.collect (fun liquidation -> array liquidation "likwidatorzy")
            |> List.map (officer (OfficerRole.Other "LIKWIDATOR"))

        match boards @ supervisors @ prokurents @ liquidators with
        | [] -> NotAvailable "The extract lists no officers, which doesn't say there are none"
        | found -> Available found

    let private owner (asPublished: string) (whole: bool) (e: JsonElement) : Owner =
        let name, kind, entity = party e

        {
            Name =
                if PartyNames.isPlaceholder name then
                    ValueNone
                else
                    ValueSome name
            Kind = kind
            Control =
                [
                    {
                        Kind = ControlKind.Ownership
                        Band =
                            if whole then
                                ValueSome { From = 100; To = 100 }
                            else
                                ValueNone
                        AsPublished = asPublished
                    }
                ]
            Current = true
            Since = ValueNone
            EndedOn = ValueNone
            BornIn = ValueNone
            Nationality = ValueNone
            CountryOfResidence = ValueNone
            Address = ValueNone
            Entity = entity
            IdentityVerification = ValueNone
        }

    /// Partners of an sp. z o.o. with their shares, and a sole shareholder of an S.A. KRS enters an sp. z o.o.'s partners only at 10% of the
    /// capital or more, so the listing leaves OwnerNotIdentified unless one owner is listed and the register says it holds every share.
    let owners (root: JsonElement) =
        let part1 = part "dzial1" (extract root)

        let partners =
            part1
            |> ValueOption.map (fun part -> oneOrMany "wspolnicySpzoo" part)
            |> ValueOption.defaultValue []
            |> List.map (fun partner ->
                owner
                    (tryString "posiadaneUdzialy" partner |> ValueOption.defaultValue "WSPÓLNIK")
                    (tryFlag "czyPosiadaCaloscUdzialow" partner = ValueSome true)
                    partner)

        let soleShareholder =
            part1
            |> ValueOption.map (fun part -> oneOrMany "jedynyAkcjonariusz" part)
            |> ValueOption.defaultValue []
            |> List.map (fun holder ->
                owner
                    "JEDYNY AKCJONARIUSZ (sole shareholder)"
                    (tryFlag "czyPosiadaCaloscAkcji" holder = ValueSome true)
                    holder)

        match partners @ soleShareholder with
        | [] ->
            NotAvailable
                "The extract lists no partners or sole shareholder; an S.A. with several shareholders has none listed, so this doesn't say there are none"
        | current ->
            Available
                {
                    Owners = current
                    Statements = []
                    Gaps =
                        [
                            match current with
                            | [ sole ] when
                                sole.Control
                                |> List.exists (fun control -> control.Band = ValueSome { From = 100; To = 100 })
                                ->
                                ()
                            | _ -> OwnershipGap.OwnerNotIdentified
                            if current |> List.exists (fun o -> o.Kind = PartyKind.Withheld) then
                                OwnershipGap.OwnerWithheld
                            if current |> List.exists (fun o -> o.Name.IsNone && o.Kind <> PartyKind.Withheld) then
                                OwnershipGap.OwnerUnreadable
                            if
                                current
                                |> List.exists (fun o ->
                                    o.Kind = PartyKind.LegalEntity
                                    && (o.Entity |> ValueOption.bind _.RegistrationNumber).IsNone)
                            then
                                OwnershipGap.EntityNotIdentified
                        ]
                }

/// What a lookup of one number found: a current extract, a deleted entity's full extract, or neither register's.
type internal PolandExtract =
    | Current of reply: SourceReply
    | Deleted of reply: SourceReply
    | NoExtract of reply: SourceReply

/// Poland as an ICompanyRegistry. The entrepreneurs' register is asked first and the associations' on a 404. A 204 for a current extract is a
/// deleted entity, read from its full extract.
[<Sealed>]
type internal PolandRegistry(connection: SourceConnection) =
    let client =
        SourceClient(connection, SourceAnswer.pacing 1 (TimeSpan.FromSeconds 1.))

    let coverage = Poland.coverage
    let baseUri = "https://api-krs.ms.gov.pl/api/krs"

    let offNumber number send =
        match Poland.normalNumber number with
        | ValueSome krs -> send krs
        | ValueNone ->
            Threading.Tasks.Task.FromResult(
                Error(RegistryFailure.InvalidRequest "A KRS number has up to ten digits, not all zero")
            )

    let rec read (krs: string) (registers: string list) (last: SourceReply voption) (ct: CancellationToken) =
        task {
            match registers, last with
            | [], ValueSome reply -> return Ok(NoExtract reply)
            | [], ValueNone -> return Error(RegistryFailure.UnreadableAnswer "No register was asked")
            | register :: rest, _ ->
                match! client.GetAsync(Uri $"{baseUri}/OdpisAktualny/{krs}?rejestr={register}&format=json", ct) with
                | Error failure -> return Error failure
                | Ok reply when reply.Status = 404 -> return! read krs rest (ValueSome reply) ct
                | Ok reply when reply.Status = 204 ->
                    match! client.GetAsync(Uri $"{baseUri}/OdpisPelny/{krs}?rejestr={register}&format=json", ct) with
                    | Error failure -> return Error failure
                    | Ok full when full.Status = 200 -> return Ok(Deleted full)
                    | Ok full ->
                        return
                            Error(
                                RegistryFailure.RequestRefused
                                    $"HTTP {full.Status} for the full extract of an entity with no current one"
                            )
                | Ok reply -> return Ok(Current reply)
        }

    let noExtract reply (reason: string) =
        Ok
            { SourceAnswer.evidence coverage reply ValueNone (NotAvailable reason) with
                Raw = ValueNone
            }

    /// Officers and owners come from a current extract only.
    let readCurrent (krs: string) (parse: JsonElement -> Availability<'T>) (ct: CancellationToken) =
        task {
            match! read krs [ "P"; "S" ] ValueNone ct with
            | Error failure -> return Error failure
            | Ok(NoExtract reply) -> return noExtract reply Poland.notFound
            | Ok(Deleted reply) ->
                return
                    SourceAnswer.json reply (fun root ->
                        Poland.deleted krs root |> ignore
                        Poland.asOf root)
                    |> Result.map (fun asOf ->
                        SourceAnswer.evidence
                            coverage
                            reply
                            asOf
                            (NotAvailable
                                "The entity has no current extract, as a deleted entity hasn't, so it has no current officers or owners"))
            | Ok(Current reply) ->
                return
                    SourceAnswer.json reply (fun root ->
                        Poland.lookup krs root |> ignore
                        parse root, Poland.asOf root)
                    |> Result.map (fun (value, asOf) -> SourceAnswer.evidence coverage reply asOf value)
        }

    interface ICompanyRegistry with
        member _.Coverage = coverage

        member _.TryFindAsync(number: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                offNumber number (fun krs ->
                    task {
                        match! read krs [ "P"; "S" ] ValueNone ct with
                        | Error failure -> return Error failure
                        | Ok(NoExtract reply) -> return noExtract reply Poland.notFound
                        | Ok(Deleted reply) ->
                            return
                                SourceAnswer.json reply (fun root -> Poland.deleted krs root, Poland.asOf root)
                                |> Result.map (fun (company, asOf) ->
                                    SourceAnswer.evidence coverage reply asOf (Available(ValueSome company)))
                        | Ok(Current reply) ->
                            return
                                SourceAnswer.json reply (fun root -> Poland.lookup krs root, Poland.asOf root)
                                |> Result.map (fun (company, asOf) ->
                                    SourceAnswer.evidence coverage reply asOf (Available(ValueSome company)))
                    }))

        member _.SearchAsync(_: string, jurisdiction: Jurisdiction, _: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                Threading.Tasks.Task.FromResult(
                    Ok(
                        SourceAnswer.unasked
                            coverage
                            client.Now
                            (NotAvailable "The KRS API looks up a KRS number only")
                    )
                ))

        member _.OfficersAsync(number: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                offNumber number (fun krs -> readCurrent krs Poland.officers ct))

        member _.OwnersAsync(number: string, jurisdiction: Jurisdiction, _: OwnerPurpose, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                offNumber number (fun krs -> readCurrent krs Poland.owners ct))
