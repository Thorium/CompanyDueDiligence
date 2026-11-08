namespace CompanyDueDiligence

open System
open System.Globalization
open System.Text
open System.Text.Json
open System.Text.RegularExpressions
open System.Threading
open CompanyDueDiligence.Json

/// The RUES extract of the chambers of commerce on datos.gov.co, resource c82u-588k ("Personas Naturales, Personas Jurídicas y Entidades Sin
/// Animo de Lucro"), read with its metadata and a $group of its statuses on 2026-11-08. Only category 01, a company or non-profit's principal
/// registration, is read: natural persons and branches (resource nb3d-v3n7) aren't.
module internal Colombia =

    let coverage: RegistryCoverage =
        {
            Registry = "RUES extract on datos.gov.co (Colombia)"
            Jurisdictions = [ Jurisdiction.ofCode "CO" ]
            Standing =
                Support.Partial
                    "The matrícula's state; liquidation and Ley 1116 proceedings of an active matrícula are read from the name's suffix"
            Concerns =
                Support.Partial
                    "A matrícula not renewed for the year before its last update is stated, and insolvency where the name says so; disputes aren't published"
            Addresses = Support.NotOffered "The extract holds no address"
            Search = Support.Partial "50 company names starting with the term"
            Officers = Support.Partial "The legal representative only, as the chamber last synchronised it"
            Owners = Support.NotOffered "The extract lists no owners"
            Cost = AccessCost.Free
            RateLimit = RateLimitPolicy.NotPublished(2, TimeSpan.FromSeconds 1.)
            Licence = "CC BY-SA 4.0, as the dataset's metadata states"
        }

    /// The extract holds what the chambers synchronised to RUES, so a number missing from it can still be registered.
    [<Literal>]
    let notListed =
        "The extract holds what the chambers synchronised to RUES, so not finding this number proves nothing"

    /// What a lookup asks for: a NIT, or a chamber's code and its matrícula.
    [<RequireQualifiedAccess>]
    type Lookup =
        | Nit of nit: string
        | Matricula of chamber: string * matricula: string

    let private nitRegex = Regex @"\A([0-9]{5,10})(-([0-9]))?\z"
    let private matriculaRegex = Regex @"\A([0-9]{2})/([0-9]{1,10})\z"

    /// DIAN's check digit: the digits from the right weighted 3, 7, 13, 17, 19, 23, 29, 37, 41, 43, the sum modulo 11, and 11 less a remainder
    /// over 1.
    let checkDigit (nit: string) =
        let weights = [| 3; 7; 13; 17; 19; 23; 29; 37; 41; 43 |]

        let remainder =
            nit
            |> Seq.rev
            |> Seq.mapi (fun position digit -> (int digit - int '0') * weights[position])
            |> Seq.sum
            |> fun total -> total % 11

        if remainder > 1 then 11 - remainder else remainder

    /// A NIT of five to ten digits, dots and spaces taken out, with its check digit after a hyphen if given, which has to match; or a chamber code
    /// and matrícula as 04/1291197.
    let lookupOf (number: string) =
        let normal =
            if isNull number then
                ""
            else
                number.Replace(".", "").Replace(" ", "")

        let nit = nitRegex.Match normal
        let matricula = matriculaRegex.Match normal

        if nit.Success then
            let digits = nit.Groups[1].Value

            if not nit.Groups[3].Success || int nit.Groups[3].Value = checkDigit digits then
                ValueSome(Lookup.Nit digits)
            else
                ValueNone
        elif matricula.Success then
            ValueSome(Lookup.Matricula(matricula.Groups[1].Value, matricula.Groups[2].Value))
        else
            ValueNone

    /// The SoQL condition for a lookup. A NIT is under nit, or under numero_identificacion when the identification class is 02, NIT.
    let whereOf (lookup: Lookup) =
        let principal = "codigo_categoria_matricula = '01'"

        match lookup with
        | Lookup.Nit nit ->
            let value = OpenData.literal nit
            $"{principal} AND (nit = {value} OR (codigo_clase_identificacion = '02' AND numero_identificacion = {value}))"
        | Lookup.Matricula(chamber, matricula) ->
            $"{principal} AND codigo_camara = {OpenData.literal chamber} AND matricula = {OpenData.literal matricula}"

    /// Every codigo_estado_matricula the extract held on 2026-11-08, by code. A code not listed is Unknown.
    let statusStanding (code: string voption) (word: string voption) =
        let named fallback =
            word |> ValueOption.defaultValue fallback

        match code with
        | ValueSome "01" -> CompanyStanding.Active
        | ValueSome "02" -> CompanyStanding.Unknown(ValueSome(named "INACTIVA"))
        | ValueSome "03" -> CompanyStanding.Closed(named "CANCELADA")
        | ValueSome "04" -> CompanyStanding.Unknown(ValueSome(named "NO COMPETENCIA DE LA CAMARA"))
        | ValueSome "05" -> CompanyStanding.Unknown(ValueSome(named "NO ASIGNADO"))
        | ValueSome "06" -> CompanyStanding.Unknown(ValueSome(named "NO MATRICULADO"))
        | ValueSome "07" -> CompanyStanding.Registered(ValueSome(named "MATRÍCULA NUEVA, CONSTITUCIÓN POR TRASLADO"))
        | ValueSome "09" -> CompanyStanding.Closed(named "MATRÍCULA CANCELADA POR TRASLADO DE DOMICILIO")
        | ValueSome "10" -> CompanyStanding.Closed(named "MATRÍCULA CANCELADA POR FUSIÓN")
        | ValueSome "13" -> CompanyStanding.Closed(named "MATRÍCULA CANCELADA POR RECONSTITUCIÓN.")
        | ValueSome "14" -> CompanyStanding.Unknown(ValueSome(named "SIN ESTADO"))
        | ValueSome "15" -> CompanyStanding.Closed(named "MATRÍCULA CANCELADA LEY 1429")
        | ValueSome "16" ->
            CompanyStanding.Unknown(ValueSome(named "MATRICULA INACTIVA POR PERDIDA DE CALIDAD DE COMERCIANTE"))
        | ValueSome other -> CompanyStanding.Unknown(ValueSome(named other))
        | ValueNone -> CompanyStanding.Unknown word

    /// An active matrícula last renewed for a year before the one preceding its last update has missed a renewal, due each year by 31 March.
    let renewalOverdue (renewed: int voption) (updated: DateOnly voption) =
        match renewed, updated with
        | ValueSome year, ValueSome date -> year < date.Year - 1
        | _ -> false

    /// What the name's suffix says of an active matrícula, accents and case aside: "liquidación judicial" or "reorganización" is an insolvency
    /// proceeding under Ley 1116, and "en liquidación" is being wound up.
    let nameStanding (name: string) =
        let plain =
            (if isNull name then "" else name.ToUpperInvariant()).Normalize NormalizationForm.FormD
            |> String.filter (fun c -> CharUnicodeInfo.GetUnicodeCategory c <> UnicodeCategory.NonSpacingMark)

        if plain.Contains "LIQUIDACION JUDICIAL" then
            ValueSome(CompanyStanding.InInsolvency "Liquidación judicial (judicial liquidation)")
        elif plain.Contains "REORGANIZACION" then
            ValueSome(CompanyStanding.InInsolvency "En reorganización (reorganisation)")
        elif plain.Contains "EN LIQUIDACION" then
            ValueSome(CompanyStanding.WindingUp "En liquidación (in liquidation)")
        else
            ValueNone

    /// The matrícula's state, then for an active one the name's suffix, then a missed renewal.
    let standing code word renewed updated name =
        match statusStanding code word with
        | CompanyStanding.Active ->
            match nameStanding name with
            | ValueSome placed -> placed
            | ValueNone when renewalOverdue renewed updated ->
                CompanyStanding.ActiveWithConcern $"matrícula last renewed for {renewed.Value}"
            | ValueNone -> CompanyStanding.Active
        | placed -> placed

    let private compactDate name (e: JsonElement) =
        match SourceAnswer.trimmed name e with
        | ValueNone -> ValueNone
        | ValueSome text ->
            match DateOnly.TryParseExact(text, "yyyyMMdd", CultureInfo.InvariantCulture, DateTimeStyles.None) with
            | true, date -> ValueSome date
            | _ -> raise (InvalidJson $"Field '{name}' is not a date")

    /// fecha_actualizacion, "2026/10/16 17:35:14.227000000": the date RUES last updated the row.
    let updatedOn (e: JsonElement) =
        SourceAnswer.trimmed "fecha_actualizacion" e
        |> ValueOption.bind (fun text ->
            if text.Length < 10 then
                ValueNone
            else
                match
                    DateOnly.TryParseExact(
                        text.Substring(0, 10),
                        "yyyy/MM/dd",
                        CultureInfo.InvariantCulture,
                        DateTimeStyles.None
                    )
                with
                | true, date -> ValueSome date
                | _ -> ValueNone)

    let numberOf (e: JsonElement) =
        OpenData.text "nit" e
        |> ValueOption.orElse (
            match OpenData.text "codigo_clase_identificacion" e with
            | ValueSome "02" -> OpenData.text "numero_identificacion" e
            | _ -> ValueNone
        )
        |> ValueOption.orElse (
            match OpenData.text "codigo_camara" e, OpenData.text "matricula" e with
            | ValueSome chamber, ValueSome matricula -> ValueSome $"{chamber}/{matricula}"
            | _ -> ValueNone
        )
        |> ValueOption.defaultWith (fun () -> raise (InvalidJson "The row has no NIT or matrícula"))

    let company (e: JsonElement) : Company =
        let code = OpenData.text "codigo_estado_matricula" e
        let word = SourceAnswer.trimmed "estado_matricula" e
        let renewed = tryInt "ultimo_ano_renovado" e
        let updated = updatedOn e
        let name = required "razon_social" SourceAnswer.trimmed e
        let standing = standing code word renewed updated name

        {
            Name = name
            OtherNames = SourceAnswer.trimmed "sigla" e |> ValueOption.toList
            Number = numberOf e
            Jurisdiction = Jurisdiction.ofCode "CO"
            JurisdictionAsPublished = ValueNone
            Standing = standing
            Concerns =
                match standing, renewed, updated with
                | CompanyStanding.InInsolvency _, _, _ ->
                    {
                        Stated = [ CompanyConcern.InsolvencyHistory ]
                        Unconfirmed = CompanyConcern.all |> List.except [ CompanyConcern.InsolvencyHistory ]
                    }
                | (CompanyStanding.Active | CompanyStanding.ActiveWithConcern _), ValueSome _, ValueSome _ ->
                    {
                        Stated =
                            if renewalOverdue renewed updated then
                                [ CompanyConcern.AnnualFilingOverdue ]
                            else
                                []
                        Unconfirmed = CompanyConcern.all |> List.except [ CompanyConcern.AnnualFilingOverdue ]
                    }
                | _ -> CompanyConcerns.notPublished
            StatusAsPublished =
                match word, renewed with
                | ValueSome status, ValueSome year -> ValueSome $"{status}; último año renovado {year}"
                | ValueSome status, ValueNone -> ValueSome status
                | ValueNone, _ -> code
            LegalForm = SourceAnswer.trimmed "organizacion_juridica" e
            // fecha_matricula is the registration with this chamber, which for a company that moved its domicile is the move.
            IncorporatedOn = compactDate "fecha_matricula" e
            CeasedOn = compactDate "fecha_cancelacion" e
            Addresses = NotAvailable "The extract holds no address"
            Role = RecordRole.Home
            HomeRegistration = ValueNone
            EstablishmentOf = ValueNone
        }

    /// The legal representative named on the row. A NIT as their identification is a legal entity.
    let representative (e: JsonElement) =
        match SourceAnswer.trimmed "representante_legal" e with
        | ValueNone ->
            NotAvailable "The extract names no legal representative for this company, which doesn't say it has none"
        | ValueSome name ->
            let identification = SourceAnswer.trimmed "clase_identificacion_rl" e

            let kind =
                match identification |> ValueOption.map _.ToUpperInvariant() with
                | ValueSome "NIT" -> PartyKind.LegalEntity
                | ValueSome word when word.Contains "CEDULA" || word.Contains "PASAPORTE" || word.Contains "TARJETA" ->
                    PartyKind.Individual
                | other -> PartyKind.Unknown other

            Available
                [
                    {
                        Name = name
                        Kind = kind
                        Role = OfficerRole.Other "Representante legal"
                        Current = true
                        AppointedOn = ValueNone
                        EndedOn = ValueNone
                        BornIn = ValueNone
                        Nationality = ValueNone
                        CountryOfResidence = ValueNone
                        Address = ValueNone
                        Entity =
                            if kind = PartyKind.LegalEntity then
                                ValueSome
                                    {
                                        LegalName = ValueSome name
                                        LegalForm = ValueNone
                                        GoverningLaw = ValueNone
                                        Country = ValueSome "Colombia"
                                        Register = ValueNone
                                        RegistrationNumber =
                                            SourceAnswer.trimmed "num_identificacion_representante_legal" e
                                    }
                            else
                                ValueNone
                        IdentityVerification = ValueNone
                    }
                ]

    let private movedAway (e: JsonElement) =
        OpenData.text "codigo_estado_matricula" e = ValueSome "09"

    /// The one row for a lookup, with the row it moved its domicile from, or why there isn't one: none proves nothing, and several can't be
    /// told apart. Two rows of which exactly one is code 09, cancelled by a move of domicile, are one company: the other row is read.
    let found (root: JsonElement) =
        match OpenData.socrataRows root with
        | [] -> Error notListed
        | [ only ] -> Ok(only, ValueNone)
        | [ first; second ] when movedAway first <> movedAway second ->
            if movedAway first then
                Ok(second, ValueSome first)
            else
                Ok(first, ValueSome second)
        | several -> Error $"{several.Length} rows carry this number, so the extract can't tell which is meant"

    /// The company with the move of domicile its code 09 row records noted in StatusAsPublished.
    let afterMove (movedFrom: JsonElement voption) (company: Company) =
        match movedFrom with
        | ValueNone -> company
        | ValueSome old ->
            let chamber =
                SourceAnswer.trimmed "camara_comercio" old
                |> ValueOption.orElse (OpenData.text "codigo_camara" old)
                |> ValueOption.defaultValue "another chamber"

            let matricula =
                OpenData.text "matricula" old |> ValueOption.defaultValue "not given"

            let note =
                $"moved its domicile from {chamber}, matrícula {matricula}, cancelled there by the move (code 09)"

            { company with
                StatusAsPublished =
                    company.StatusAsPublished
                    |> ValueOption.map (fun status -> $"{status}; {note}")
                    |> ValueOption.orElse (ValueSome note)
            }

/// Colombia as an ICompanyRegistry. A number not found is NotAvailable.
[<Sealed>]
type internal ColombiaRegistry(connection: SourceConnection) =
    static let searchLimit = 50

    let client =
        SourceClient(connection, SourceAnswer.pacing 2 (TimeSpan.FromSeconds 1.))

    let coverage = Colombia.coverage
    let dataset = "https://www.datos.gov.co/resource/c82u-588k.json"

    let offNumber number send =
        match Colombia.lookupOf number with
        | ValueSome lookup -> send lookup
        | ValueNone ->
            Threading.Tasks.Task.FromResult(
                Error(
                    RegistryFailure.InvalidRequest
                        "A Colombian number is a NIT of five to ten digits, with a matching check digit if given, or a chamber code and matrícula as 04/1291197"
                )
            )

    /// The row for a lookup, read by wanted with the row it moved its domicile from. The row's own JSON is kept as raw, or both rows after a
    /// move, and its last update as as-of.
    let readRow
        (lookup: Colombia.Lookup)
        (wanted: JsonElement -> JsonElement voption -> Availability<'T>)
        (ct: CancellationToken)
        =
        task {
            // Three rows are enough to tell a move of domicile, two rows, from a number that isn't unique.
            let uri =
                Uri $"{dataset}?$where={Uri.EscapeDataString(Colombia.whereOf lookup)}&$limit=3"

            match! client.GetAsync(uri, ct) with
            | Error failure -> return Error failure
            | Ok reply when reply.Status = 404 -> return Error(RegistryFailure.RequestRefused "HTTP 404")
            | Ok reply ->
                return
                    SourceAnswer.json reply (fun root ->
                        match Colombia.found root with
                        | Ok(row, ValueNone) ->
                            wanted row ValueNone, ValueSome(row.GetRawText()), Colombia.updatedOn row
                        | Ok(row, movedFrom) -> wanted row movedFrom, ValueSome reply.Body, Colombia.updatedOn row
                        | Error reason -> NotAvailable reason, ValueSome reply.Body, ValueNone)
                    |> Result.map (fun (value, raw, asOf) ->
                        { SourceAnswer.evidence coverage reply asOf value with
                            Raw = raw
                        })
        }

    /// Companies, category 01, whose name matches as asked.
    let search (name: string) (matching: NameMatch) (jurisdiction: Jurisdiction) (ct: CancellationToken) =
        SourceAnswer.inCoverage coverage jurisdiction (fun () ->
            NameSearch.withName name matching (fun () ->
                task {
                    let named = OpenData.nameCondition "upper(razon_social)" "like" name matching
                    let where = Uri.EscapeDataString $"codigo_categoria_matricula = '01' AND {named}"
                    let uri = Uri $"{dataset}?$where={where}&$limit={searchLimit}"

                    match! client.GetAsync(uri, ct) with
                    | Error failure -> return Error failure
                    | Ok reply when reply.Status = 404 -> return Error(RegistryFailure.RequestRefused "HTTP 404")
                    | Ok reply ->
                        return
                            SourceAnswer.json reply (fun root ->
                                OpenData.socrataRows root
                                |> List.map Colombia.company
                                |> OpenData.nameMatches name matching searchLimit)
                            |> Result.map (fun found ->
                                SourceAnswer.evidence coverage reply reply.LastModified (Available found))
                }))

    interface INameMatchSearch with
        member _.SearchByNameAsync
            (name: string, matching: NameMatch, jurisdiction: Jurisdiction, ct: CancellationToken)
            =
            search name matching jurisdiction ct

    interface ICompanyRegistry with
        member _.Coverage = coverage

        member _.TryFindAsync(number: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                offNumber number (fun lookup ->
                    readRow
                        lookup
                        (fun row movedFrom ->
                            Available(ValueSome(Colombia.company row |> Colombia.afterMove movedFrom)))
                        ct))

        member _.SearchAsync(name: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            search name NameMatch.StartsWith jurisdiction ct

        member _.OfficersAsync(number: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                offNumber number (fun lookup -> readRow lookup (fun row _ -> Colombia.representative row) ct))

        member _.OwnersAsync(_: string, jurisdiction: Jurisdiction, _: OwnerPurpose, _: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                Threading.Tasks.Task.FromResult(
                    Ok(SourceAnswer.unasked coverage client.Now (NotAvailable "The extract lists no owners"))
                ))
