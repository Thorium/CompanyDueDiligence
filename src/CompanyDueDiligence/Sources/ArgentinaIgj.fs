namespace CompanyDueDiligence

open System
open System.Collections.Generic
open System.IO
open System.IO.Compression
open System.Text
open System.Text.RegularExpressions
open System.Threading

/// The Inspección General de Justicia's "Entidades constituidas en la IGJ" open data (datos.jus.gob.ar), read from igj-2026-semestre-2.zip as
/// downloaded on 2026-11-08: comma-separated UTF-8 files of entities, addresses and authorities, one set a month, of which the latest is read.
module internal ArgentinaIgj =

    let jurisdiction = Jurisdiction.ofCode "AR-C"

    let coverage: RegistryCoverage =
        {
            Registry = "IGJ entidades constituidas (Argentina, Ciudad de Buenos Aires)"
            Jurisdictions = [ jurisdiction ]
            Standing =
                Support.Partial
                    "Whether the entity was removed (dada de baja) and why; the file has no other status, so one not removed reads as registered"
            Concerns = Support.NotOffered "The files state no filing dates, disputes or insolvency"
            Addresses = Support.Supported
            Search = Support.Partial "Names containing the term, over the file, up to 50"
            Officers =
                Support.Partial
                    "Authorities and representatives by name, with no posts or dates; each is read as current"
            Owners =
                Support.Partial
                    "Partners (socios) by name with no share; shareholders of a sociedad anónima aren't registered with the IGJ"
            Cost = AccessCost.Free
            RateLimit = RateLimitPolicy.NotPublished(1, TimeSpan.FromDays 1.)
            Licence = "Creative Commons Attribution 4.0 (CC BY 4.0), as the dataset states"
        }

    let invalidNumber = "An IGJ número correlativo has up to 7 digits; a CUIT has 11"

    /// The key of an IGJ número correlativo or of an 11-digit CUIT, with or without its dashes.
    let normalNumber (number: string) =
        let text = (if isNull number then "" else number).Trim()
        let digits = text.Replace("-", "")

        if digits <> "" && digits |> Seq.forall Char.IsAsciiDigit then
            if digits.Length = 11 then
                ValueSome("CUIT " + digits)
            elif digits.Length <= 7 && digits = text && digits.TrimStart('0') <> "" then
                ValueSome("IGJ " + digits.TrimStart('0'))
            else
                ValueNone
        else
            ValueNone

    /// dada_de_baja: "S" is removed, with detalle_baja as the reason; blank is not removed, and the file holds no other status. Any other is
    /// Unknown.
    let standing (removed: string voption) (reason: string voption) =
        match removed with
        | ValueNone -> CompanyStanding.Registered ValueNone
        | ValueSome "S" -> CompanyStanding.Closed(reason |> ValueOption.defaultValue "Dada de baja")
        | ValueSome other -> CompanyStanding.Unknown(ValueSome other)

    /// A foreign company or foreign non-profit registered with the IGJ is a foreign registration; any other kind is formed here.
    let role (kind: string voption) =
        match kind with
        | ValueSome "SOCIEDAD EXTRANJERA"
        | ValueSome "ENTIDAD EXTRANJERA SIN FINES DE LUCRO" -> RecordRole.ForeignRegistration
        | ValueSome _ -> RecordRole.Home
        | ValueNone -> RecordRole.NotStated

    let private fileRegex =
        Regex @"igj-(entidades|domicilios|autoridades)-(\d{6})\.csv\z"

    /// The month's entities, addresses and authorities files: the latest month the zip holds all three of.
    let files (archive: ZipArchive) =
        let found =
            archive.Entries
            |> Seq.choose (fun e ->
                let m = fileRegex.Match e.Name

                if m.Success then
                    Some(m.Groups[2].Value, (m.Groups[1].Value, e))
                else
                    None)
            |> Seq.groupBy fst
            |> Seq.map (fun (month, entries) -> month, entries |> Seq.map snd |> Map.ofSeq)
            |> Seq.filter (fun (_, kinds) -> kinds.Count = 3)
            |> Seq.sortByDescending fst
            |> Seq.tryHead

        match found with
        | Some(_, kinds) -> kinds["entidades"], kinds["domicilios"], kinds["autoridades"]
        | None -> raise (InvalidDataException "The zip holds no month with entities, addresses and authorities")

    /// Runs read over one CSV's rows and a reader of their columns, with a quoted line break kept inside its field; the entry is open while read
    /// runs. A header lacking a required column fails the file; an empty file has no rows.
    let private table
        (ct: CancellationToken)
        (required: string list)
        (entry: ZipArchiveEntry)
        (read: (string -> DelimitedRecord -> string voption) -> DelimitedRecord seq -> 'T)
        =
        use stream = BulkFile.openEntry entry

        match
            BulkFile.splitAt (fun _ -> true) (BulkFile.records ',' true (BulkFile.lines BulkFile.strictUtf8 stream))
        with
        | struct (_, ValueNone, _) -> read (fun _ _ -> ValueNone) Seq.empty
        | struct (_, ValueSome first, rows) ->
            let header = BulkFile.header first.Fields

            if not (required |> List.forall header.ContainsKey) then
                raise (InvalidDataException "A header lacks a column the layout has")

            rows
            |> Seq.map (fun row ->
                ct.ThrowIfCancellationRequested()
                row)
            |> read (fun name row -> BulkFile.cell header name row)

    type private Entity =
        {
            Line: string
            Number: string
            Name: string
            Kind: string voption
            Removed: string voption
            Reason: string voption
            Cuit: string voption
        }

    let private officer (name: string) (role: string) (document: string voption) : Officer =
        {
            Name = name
            Kind =
                match document with
                | ValueSome "OTROS"
                | ValueNone -> PartyKind.Unknown document
                | ValueSome _ -> PartyKind.Individual
            Role = OfficerRole.Other role
            Current = true
            AppointedOn = ValueNone
            EndedOn = ValueNone
            BornIn = ValueNone
            Nationality = ValueNone
            CountryOfResidence = ValueNone
            Address = ValueNone
            Entity = ValueNone
            IdentityVerification = ValueNone
        }

    let private partner (name: string) (document: string voption) : Owner =
        {
            Name = ValueSome name
            Kind =
                match document with
                | ValueSome "OTROS"
                | ValueNone -> PartyKind.Unknown document
                | ValueSome _ -> PartyKind.Individual
            Control =
                [
                    {
                        Kind = ControlKind.Ownership
                        Band = ValueNone
                        AsPublished = "SOCIO"
                    }
                ]
            Current = true
            Since = ValueNone
            EndedOn = ValueNone
            BornIn = ValueNone
            Nationality = ValueNone
            CountryOfResidence = ValueNone
            Address = ValueNone
            Entity = ValueNone
            IdentityVerification = ValueNone
        }

    /// A listing of partners: shares aren't published, and a partner whose kind the document type doesn't settle is unreadable.
    let listing (partners: Owner list) =
        {
            Owners = partners
            Statements = []
            Gaps =
                OwnershipGap.OwnerDetailsNotConfirmed
                :: (if partners |> List.exists (fun p -> p.Kind <> PartyKind.Individual) then
                        [ OwnershipGap.OwnerUnreadable ]
                    else
                        [])
        }

    /// Builds the register from the half-yearly zip as published, reading the latest month in it. Document numbers aren't kept. A número
    /// correlativo on two rows answers NotAvailable, and so do those rows' CUITs. Entities are gathered in a dictionary filled here and then only
    /// read, as the file holds a million.
    let build (zip: Stream) (date: BulkFileDate) (ct: CancellationToken) : ICompanyRegistry =
        use archive = BulkFile.openZip zip
        let entities, addresses, authorities = files archive
        let held = Dictionary<string, Entity>()

        let keysOf (entity: Entity) =
            ("IGJ " + entity.Number)
            :: (entity.Cuit |> ValueOption.map ((+) "CUIT ") |> ValueOption.toList)

        // The keys of every row of a número held twice, and the rows that couldn't be read.
        let struct (repeatedKeys, skipped) =
            table ct [ "numero_correlativo"; "razon_social"; "dada_de_baja"; "cuit" ] entities (fun value rows ->
                rows
                |> Seq.fold
                    (fun (struct (repeatedKeys: Set<string>, skipped)) row ->
                        let value name = value name row

                        match value "numero_correlativo" |> ValueOption.bind normalNumber, value "razon_social" with
                        | ValueSome number, ValueSome name when number.StartsWith("IGJ ", StringComparison.Ordinal) ->
                            let number = number.Substring 4

                            let entity =
                                {
                                    Line = String.concat "," row.Fields
                                    Number = number
                                    Name = name.Replace('\n', ' ')
                                    Kind = value "descripcion_tipo_societario"
                                    Removed = value "dada_de_baja"
                                    Reason = value "detalle_baja"
                                    Cuit =
                                        value "cuit"
                                        |> ValueOption.filter (fun c ->
                                            c.Length = 11 && c |> Seq.forall Char.IsAsciiDigit)
                                }

                            match held.TryGetValue number with
                            | true, earlier ->
                                held.Remove number |> ignore
                                struct (repeatedKeys + set (keysOf earlier @ keysOf entity), skipped)
                            | _ when repeatedKeys.Contains("IGJ " + number) ->
                                struct (repeatedKeys + set (keysOf entity), skipped)
                            | _ ->
                                held[number] <- entity
                                struct (repeatedKeys, skipped)
                        | _ -> struct (repeatedKeys, skipped + 1))
                    (struct (Set.empty, 0)))

        let heldNumber (number: string voption) =
            number
            |> ValueOption.map _.TrimStart('0')
            |> ValueOption.filter held.ContainsKey

        let places =
            table ct [ "numero_correlativo"; "tipo_domicilio"; "calle" ] addresses (fun value rows ->
                rows
                |> Seq.choose (fun row ->
                    let value name = value name row

                    match heldNumber (value "numero_correlativo") with
                    | ValueSome number ->
                        let street =
                            match [ value "calle"; value "numero" ] |> List.choose ValueOption.toOption with
                            | [] -> ValueNone
                            | parts -> ValueSome(String.concat " " parts)

                        Bulk.address
                            [ street ]
                            (value "localidad")
                            (value "provincia")
                            (value "codigo_postal")
                            ValueNone
                        |> ValueOption.map (fun address ->
                            let kind =
                                match value "tipo_domicilio" with
                                | ValueSome "L" -> AddressKind.RegisteredOffice
                                | _ ->
                                    AddressKind.Other(
                                        value "descripcion_tipo_domicilio" |> ValueOption.defaultValue "Domicilio"
                                    )

                            struct (number, { Kind = kind; Address = address }))
                        |> ValueOption.toOption
                    | ValueNone -> None)
                |> Bulk.grouped)

        // Each entity's authorities, partners as Choice1Of2 and officers as Choice2Of2, in the file's order.
        let people =
            table ct [ "numero_correlativo"; "apellido_nombre"; "tipo_administrador" ] authorities (fun value rows ->
                rows
                |> Seq.choose (fun row ->
                    let value name = value name row

                    match heldNumber (value "numero_correlativo"), value "apellido_nombre" with
                    | ValueSome number, ValueSome name ->
                        let document = value "descripcion_tipo_documento"

                        let person =
                            match value "tipo_administrador" with
                            | ValueSome "S" -> Choice1Of2(partner name document)
                            | ValueSome _ ->
                                Choice2Of2(
                                    officer
                                        name
                                        (value "descripcion_tipo_administrador"
                                         |> ValueOption.defaultValue "AUTORIDAD")
                                        document
                                )
                            | ValueNone -> Choice2Of2(officer name "(type not given)" document)

                        Some(struct (number, person))
                    | _ -> None)
                |> Bulk.grouped)

        let index = BulkIndex<BulkRecord>()

        held.Values
        |> Seq.iter (fun entity ->
            let authorities = Bulk.listed people entity.Number

            let leaders =
                authorities
                |> List.choose (function
                    | Choice2Of2 officer -> Some officer
                    | Choice1Of2 _ -> None)

            let members =
                authorities
                |> List.choose (function
                    | Choice1Of2 partner -> Some partner
                    | Choice2Of2 _ -> None)

            let addresses = Bulk.listed places entity.Number
            let standing = standing entity.Removed entity.Reason

            let raw =
                [
                    yield entity.Line
                    for address in addresses do
                        let street = String.concat " " address.Address.Lines
                        let locality = address.Address.Locality |> ValueOption.defaultValue String.Empty
                        yield $"domicilio: {street}, {locality}"
                    for leader in leaders do
                        let role =
                            match leader.Role with
                            | OfficerRole.Other role -> role
                            | OfficerRole.Director
                            | OfficerRole.Secretary -> ""

                        yield $"{role}: {leader.Name}"
                    for owner in members do
                        yield $"SOCIO: {owner.Name |> ValueOption.defaultValue String.Empty}"
                ]
                |> String.concat "\n"

            let record =
                {
                    Company =
                        {
                            Name = entity.Name
                            OtherNames = []
                            Number = entity.Number
                            Jurisdiction = jurisdiction
                            JurisdictionAsPublished = ValueNone
                            Standing = standing
                            Concerns = CompanyConcerns.notPublished
                            StatusAsPublished =
                                match entity.Removed, entity.Reason with
                                | ValueSome removed, ValueSome reason -> ValueSome $"dada_de_baja {removed}: {reason}"
                                | ValueSome removed, ValueNone -> ValueSome $"dada_de_baja {removed}"
                                | ValueNone, _ -> ValueNone
                            LegalForm = entity.Kind
                            IncorporatedOn = ValueNone
                            CeasedOn = ValueNone
                            Addresses =
                                match addresses with
                                | [] -> NotAvailable "The addresses file holds none for this entity"
                                | list -> Available list
                            Role = role entity.Kind
                            HomeRegistration = ValueNone
                            EstablishmentOf = ValueNone
                        }
                    Officers =
                        if List.isEmpty leaders then
                            ValueNone
                        else
                            ValueSome leaders
                    Owners =
                        if List.isEmpty members then
                            ValueNone
                        else
                            ValueSome(listing members)
                    Raw = raw
                }

            index.AddEach(keysOf entity, record))

        let repeated = HashSet<string>(Seq.append index.Repeated repeatedKeys)

        new BulkRegistry(
            coverage,
            normalNumber,
            invalidNumber,
            index.Held,
            date,
            BulkAbsence.CantTell(
                $"The file holds removed entities too, but a CUIT only for some, and {skipped} rows couldn't be read; a number not found may still be registered"
            ),
            "The file lists no authorities for this entity",
            (fun _ ->
                NotAvailable
                    "The file lists no partners for this entity; shareholders of a sociedad anónima aren't registered with the IGJ"),
            repeated
        )
