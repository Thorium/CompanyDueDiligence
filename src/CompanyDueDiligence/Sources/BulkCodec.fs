namespace CompanyDueDiligence

open System
open System.Buffers
open System.IO
open System.IO.Compression
open System.Runtime.InteropServices
open System.Text
open System.Text.Unicode

/// The binary encoding of what a bulk index holds, read back value for value. Every DU is a tag byte then its case's fields; a voption is a
/// byte, 0 or 1, then the value; a list is its length as a 7-bit integer then its items; a date is its day number; integers are little-endian.
/// Any byte sequence the writer can't have produced raises InvalidDataException, and one cut short EndOfStreamException.
module internal BulkCodec =

    /// The version of every encoding here and of the index files built on them. Raise it whenever any of them changes.
    [<Literal>]
    let Format = 2

    /// A record frame's encoding byte: the record's bytes as written.
    [<Literal>]
    let private Plain = 0uy

    /// A record frame's encoding byte: the record's bytes deflated.
    [<Literal>]
    let private Deflated = 1uy

    /// Records shorter than this are framed plain: deflating them saves too little.
    [<Literal>]
    let private DeflateFrom = 256

    let private bad (what: string) =
        raise (InvalidDataException $"Not a {what} this version writes")

    // Writing

    /// A string as a 7-bit header then its bytes. The header is 0 for null, 2n + 1 for n bytes of UTF-8, and 2n + 2 for n UTF-16 code units,
    /// used for a string with a lone surrogate, which UTF-8 can't carry.
    let text (writer: BinaryWriter) (value: string) =
        match value with
        | null -> writer.Write7BitEncodedInt 0
        | _ ->
            let buffer = ArrayPool<byte>.Shared.Rent(value.Length * 3)

            try
                // Out parameters of the encoder.
                let mutable read = 0
                let mutable written = 0

                match Utf8.FromUtf16(value.AsSpan(), buffer.AsSpan(), &read, &written, false, true) with
                | OperationStatus.Done ->
                    writer.Write7BitEncodedInt(written * 2 + 1)
                    writer.Write(ReadOnlySpan<byte>(buffer, 0, written))
                | _ ->
                    writer.Write7BitEncodedInt(value.Length * 2 + 2)
                    writer.Write(MemoryMarshal.AsBytes(value.AsSpan()))
            finally
                ArrayPool<byte>.Shared.Return buffer

    let private voption (writer: BinaryWriter) (item: BinaryWriter -> 'T -> unit) (value: 'T voption) =
        match value with
        | ValueNone -> writer.Write 0uy
        | ValueSome held ->
            writer.Write 1uy
            item writer held

    let private list (writer: BinaryWriter) (item: BinaryWriter -> 'T -> unit) (values: 'T list) =
        writer.Write7BitEncodedInt values.Length
        values |> List.iter (item writer)

    let private texts writer values = list writer text values
    let private textOption writer value = voption writer text value
    let private day (writer: BinaryWriter) (value: DateOnly) = writer.Write value.DayNumber
    let private dayOption writer value = voption writer day value

    let private tag (writer: BinaryWriter) (value: int) = writer.Write(byte value)

    let private jurisdiction (writer: BinaryWriter) (value: Jurisdiction) = text writer value.Code

    let private timeSpan (writer: BinaryWriter) (value: TimeSpan) = writer.Write value.Ticks

    let private moment (writer: BinaryWriter) (value: DateTimeOffset) =
        writer.Write value.Ticks
        writer.Write(int16 value.Offset.TotalMinutes)

    let private availability (writer: BinaryWriter) (item: BinaryWriter -> 'T -> unit) (value: Availability<'T>) =
        match value with
        | Available held ->
            tag writer 0
            item writer held
        | NotAvailable reason ->
            tag writer 1
            text writer reason
        | NeedsEntitlement entitlement ->
            tag writer 2
            text writer entitlement.Who
            text writer entitlement.Route

    let private address (writer: BinaryWriter) (value: Address) =
        texts writer value.Lines
        textOption writer value.Locality
        textOption writer value.Region
        textOption writer value.PostalCode
        textOption writer value.Country

    let private addressKind (writer: BinaryWriter) (value: AddressKind) =
        match value with
        | AddressKind.RegisteredOffice -> tag writer 0
        | AddressKind.Service -> tag writer 1
        | AddressKind.PrincipalOffice -> tag writer 2
        | AddressKind.RegisteredAgent -> tag writer 3
        | AddressKind.Postal -> tag writer 4
        | AddressKind.Other label ->
            tag writer 5
            text writer label

    let private companyAddress (writer: BinaryWriter) (value: CompanyAddress) =
        addressKind writer value.Kind
        address writer value.Address

    let private standing (writer: BinaryWriter) (value: CompanyStanding) =
        match value with
        | CompanyStanding.Active -> tag writer 0
        | CompanyStanding.ActiveWithConcern concern ->
            tag writer 1
            text writer concern
        | CompanyStanding.Registered status ->
            tag writer 2
            textOption writer status
        | CompanyStanding.InInsolvency proceeding ->
            tag writer 3
            text writer proceeding
        | CompanyStanding.WindingUp status ->
            tag writer 4
            text writer status
        | CompanyStanding.Dissolved -> tag writer 5
        | CompanyStanding.Closed status ->
            tag writer 6
            text writer status
        | CompanyStanding.Unknown status ->
            tag writer 7
            textOption writer status

    let private concern (writer: BinaryWriter) (value: CompanyConcern) =
        match value with
        | CompanyConcern.AccountsOverdue -> tag writer 0
        | CompanyConcern.AnnualFilingOverdue -> tag writer 1
        | CompanyConcern.RegisteredOfficeDisputed -> tag writer 2
        | CompanyConcern.RegisteredOfficeUndeliverable -> tag writer 3
        | CompanyConcern.InsolvencyHistory -> tag writer 4

    let private role (writer: BinaryWriter) (value: RecordRole) =
        match value with
        | RecordRole.Home -> tag writer 0
        | RecordRole.ForeignRegistration -> tag writer 1
        | RecordRole.Establishment -> tag writer 2
        | RecordRole.NotStated -> tag writer 3

    let private registration (writer: BinaryWriter) (value: EntityRegistration) =
        textOption writer value.LegalName
        textOption writer value.LegalForm
        textOption writer value.GoverningLaw
        textOption writer value.Country
        textOption writer value.Register
        textOption writer value.RegistrationNumber

    let company (writer: BinaryWriter) (value: Company) =
        text writer value.Name
        texts writer value.OtherNames
        text writer value.Number
        jurisdiction writer value.Jurisdiction
        textOption writer value.JurisdictionAsPublished
        standing writer value.Standing
        list writer concern value.Concerns.Stated
        list writer concern value.Concerns.Unconfirmed
        textOption writer value.StatusAsPublished
        textOption writer value.LegalForm
        dayOption writer value.IncorporatedOn
        dayOption writer value.CeasedOn
        availability writer (fun writer addresses -> list writer companyAddress addresses) value.Addresses
        role writer value.Role
        voption writer registration value.HomeRegistration
        textOption writer value.EstablishmentOf

    let private partyKind (writer: BinaryWriter) (value: PartyKind) =
        match value with
        | PartyKind.Individual -> tag writer 0
        | PartyKind.LegalEntity -> tag writer 1
        | PartyKind.Withheld -> tag writer 2
        | PartyKind.Unknown asPublished ->
            tag writer 3
            textOption writer asPublished

    let private yearMonth (writer: BinaryWriter) (value: YearMonth) =
        writer.Write value.Year
        writer.Write value.Month

    let private verification (writer: BinaryWriter) (value: IdentityVerification) =
        dayOption writer value.VerifiedOn
        textOption writer value.VerifiedBy
        texts writer value.VerifierSupervisors
        dayOption writer value.StatementPossibleFrom
        dayOption writer value.StatementDueOn
        dayOption writer value.StatementSuppliedOn
        dayOption writer value.StatementRemovedOn
        textOption writer value.PreferredName

    let private officerRole (writer: BinaryWriter) (value: OfficerRole) =
        match value with
        | OfficerRole.Director -> tag writer 0
        | OfficerRole.Secretary -> tag writer 1
        | OfficerRole.Other asPublished ->
            tag writer 2
            text writer asPublished

    let private officer (writer: BinaryWriter) (value: Officer) =
        text writer value.Name
        partyKind writer value.Kind
        officerRole writer value.Role
        writer.Write value.Current
        dayOption writer value.AppointedOn
        dayOption writer value.EndedOn
        voption writer yearMonth value.BornIn
        textOption writer value.Nationality
        textOption writer value.CountryOfResidence
        voption writer address value.Address
        voption writer registration value.Entity
        voption writer verification value.IdentityVerification

    let private controlKind (writer: BinaryWriter) (value: ControlKind) =
        match value with
        | ControlKind.Ownership -> tag writer 0
        | ControlKind.VotingRights -> tag writer 1
        | ControlKind.AppointsBoard -> tag writer 2
        | ControlKind.SignificantInfluence -> tag writer 3
        | ControlKind.Other -> tag writer 4

    let private band (writer: BinaryWriter) (value: PercentBand) =
        writer.Write value.From
        writer.Write value.To

    let private control (writer: BinaryWriter) (value: NatureOfControl) =
        controlKind writer value.Kind
        voption writer band value.Band
        text writer value.AsPublished

    let private owner (writer: BinaryWriter) (value: Owner) =
        textOption writer value.Name
        partyKind writer value.Kind
        list writer control value.Control
        writer.Write value.Current
        dayOption writer value.Since
        dayOption writer value.EndedOn
        voption writer yearMonth value.BornIn
        textOption writer value.Nationality
        textOption writer value.CountryOfResidence
        voption writer address value.Address
        voption writer registration value.Entity
        voption writer verification value.IdentityVerification

    let private gap (writer: BinaryWriter) (value: OwnershipGap) =
        match value with
        | OwnershipGap.OwnerNotIdentified -> tag writer 0
        | OwnershipGap.OwnerDetailsNotConfirmed -> tag writer 1
        | OwnershipGap.InquiriesIncomplete -> tag writer 2
        | OwnershipGap.OwnerNotCooperating -> tag writer 3
        | OwnershipGap.OwnerWithheld -> tag writer 4
        | OwnershipGap.OwnerUnreadable -> tag writer 5
        | OwnershipGap.EntityNotIdentified -> tag writer 6
        | OwnershipGap.StatementContradictsOwners -> tag writer 7
        | OwnershipGap.StatementNotUnderstood asPublished ->
            tag writer 8
            text writer asPublished

    let private meaning (writer: BinaryWriter) (value: OwnerStatementMeaning) =
        match value with
        | OwnerStatementMeaning.NoOwnerToList -> tag writer 0
        | OwnerStatementMeaning.AllOwnersIdentified -> tag writer 1
        | OwnerStatementMeaning.ChangesOnly -> tag writer 2
        | OwnerStatementMeaning.LeavesGap held ->
            tag writer 3
            gap writer held

    let private statement (writer: BinaryWriter) (value: OwnerStatement) =
        meaning writer value.Meaning
        text writer value.AsPublished

    let ownerListing (writer: BinaryWriter) (value: OwnerListing) =
        list writer owner value.Owners
        list writer statement value.Statements
        list writer gap value.Gaps

    let record (writer: BinaryWriter) (value: BulkRecord) =
        company writer value.Company
        voption writer (fun writer officers -> list writer officer officers) value.Officers
        voption writer ownerListing value.Owners
        text writer value.Raw

    let owners (writer: BinaryWriter) (value: Availability<OwnerListing>) = availability writer ownerListing value

    let private support (writer: BinaryWriter) (value: Support) =
        match value with
        | Support.Supported -> tag writer 0
        | Support.Partial caveat ->
            tag writer 1
            text writer caveat
        | Support.NotOffered reason ->
            tag writer 2
            text writer reason

    let coverage (writer: BinaryWriter) (value: RegistryCoverage) =
        text writer value.Registry
        list writer jurisdiction value.Jurisdictions
        support writer value.Standing
        support writer value.Concerns
        support writer value.Addresses
        support writer value.Search
        support writer value.Officers
        support writer value.Owners

        match value.Cost with
        | AccessCost.Free -> tag writer 0
        | AccessCost.FreeWithKey how ->
            tag writer 1
            text writer how
        | AccessCost.Paid route ->
            tag writer 2
            text writer route

        match value.RateLimit with
        | RateLimitPolicy.Published(calls, per, source) ->
            tag writer 0
            writer.Write calls
            timeSpan writer per
            text writer source
        | RateLimitPolicy.NotPublished(pacedCalls, per) ->
            tag writer 1
            writer.Write pacedCalls
            timeSpan writer per

        text writer value.Licence

    let date (writer: BinaryWriter) (value: BulkFileDate) =
        moment writer value.DownloadedAt
        dayOption writer value.AsOf

    let absence (writer: BinaryWriter) (value: BulkAbsence) =
        match value with
        | BulkAbsence.NotHeld -> tag writer 0
        | BulkAbsence.CantTell reason ->
            tag writer 1
            text writer reason

    // Reading

    /// Reads exactly count bytes, raising EndOfStreamException when fewer are left, before allocating them when the stream knows its length.
    let private bytes (reader: BinaryReader) (count: int) =
        let stream = reader.BaseStream

        if stream.CanSeek && int64 count > stream.Length - stream.Position then
            raise (EndOfStreamException())

        let read = reader.ReadBytes count

        if read.Length < count then
            raise (EndOfStreamException())

        read

    /// What text wrote.
    let readText (reader: BinaryReader) : string =
        match reader.Read7BitEncodedInt() with
        | 0 -> null
        | header when header < 0 -> bad "string"
        | header when header % 2 = 1 -> Encoding.UTF8.GetString(bytes reader (header / 2))
        | header -> String(MemoryMarshal.Cast<byte, char>(ReadOnlySpan(bytes reader (header - 2))))

    let private readTag (reader: BinaryReader) = int (reader.ReadByte())

    let private readVoption (reader: BinaryReader) (item: BinaryReader -> 'T) : 'T voption =
        match readTag reader with
        | 0 -> ValueNone
        | 1 -> ValueSome(item reader)
        | _ -> bad "voption"

    let private readList (reader: BinaryReader) (item: BinaryReader -> 'T) : 'T list =
        match reader.Read7BitEncodedInt() with
        | count when count < 0 -> bad "list"
        | count -> List.init count (fun _ -> item reader)

    let private readTexts reader = readList reader readText
    let private readTextOption reader = readVoption reader readText

    let private readDay (reader: BinaryReader) =
        DateOnly.FromDayNumber(reader.ReadInt32())

    let private readDayOption reader = readVoption reader readDay

    let private readJurisdiction (reader: BinaryReader) =
        match Jurisdiction.tryParse (readText reader) with
        | ValueSome held -> held
        | ValueNone -> bad "jurisdiction"

    let private readTimeSpan (reader: BinaryReader) = TimeSpan(reader.ReadInt64())

    let private readMoment (reader: BinaryReader) =
        let ticks = reader.ReadInt64()
        DateTimeOffset(ticks, TimeSpan.FromMinutes(float (reader.ReadInt16())))

    let private readAvailability (reader: BinaryReader) (item: BinaryReader -> 'T) : Availability<'T> =
        match readTag reader with
        | 0 -> Available(item reader)
        | 1 -> NotAvailable(readText reader)
        | 2 ->
            let who = readText reader
            NeedsEntitlement { Who = who; Route = readText reader }
        | _ -> bad "availability"

    let private readAddress (reader: BinaryReader) : Address =
        let lines = readTexts reader
        let locality = readTextOption reader
        let region = readTextOption reader
        let postalCode = readTextOption reader

        {
            Lines = lines
            Locality = locality
            Region = region
            PostalCode = postalCode
            Country = readTextOption reader
        }

    let private readAddressKind (reader: BinaryReader) =
        match readTag reader with
        | 0 -> AddressKind.RegisteredOffice
        | 1 -> AddressKind.Service
        | 2 -> AddressKind.PrincipalOffice
        | 3 -> AddressKind.RegisteredAgent
        | 4 -> AddressKind.Postal
        | 5 -> AddressKind.Other(readText reader)
        | _ -> bad "address kind"

    let private readCompanyAddress (reader: BinaryReader) : CompanyAddress =
        let kind = readAddressKind reader

        {
            Kind = kind
            Address = readAddress reader
        }

    let private readStanding (reader: BinaryReader) =
        match readTag reader with
        | 0 -> CompanyStanding.Active
        | 1 -> CompanyStanding.ActiveWithConcern(readText reader)
        | 2 -> CompanyStanding.Registered(readTextOption reader)
        | 3 -> CompanyStanding.InInsolvency(readText reader)
        | 4 -> CompanyStanding.WindingUp(readText reader)
        | 5 -> CompanyStanding.Dissolved
        | 6 -> CompanyStanding.Closed(readText reader)
        | 7 -> CompanyStanding.Unknown(readTextOption reader)
        | _ -> bad "standing"

    let private readConcern (reader: BinaryReader) =
        match readTag reader with
        | 0 -> CompanyConcern.AccountsOverdue
        | 1 -> CompanyConcern.AnnualFilingOverdue
        | 2 -> CompanyConcern.RegisteredOfficeDisputed
        | 3 -> CompanyConcern.RegisteredOfficeUndeliverable
        | 4 -> CompanyConcern.InsolvencyHistory
        | _ -> bad "concern"

    let private readRole (reader: BinaryReader) =
        match readTag reader with
        | 0 -> RecordRole.Home
        | 1 -> RecordRole.ForeignRegistration
        | 2 -> RecordRole.Establishment
        | 3 -> RecordRole.NotStated
        | _ -> bad "record role"

    let private readRegistration (reader: BinaryReader) : EntityRegistration =
        let legalName = readTextOption reader
        let legalForm = readTextOption reader
        let governingLaw = readTextOption reader
        let country = readTextOption reader
        let register = readTextOption reader

        {
            LegalName = legalName
            LegalForm = legalForm
            GoverningLaw = governingLaw
            Country = country
            Register = register
            RegistrationNumber = readTextOption reader
        }

    let readCompany (reader: BinaryReader) : Company =
        let name = readText reader
        let otherNames = readTexts reader
        let number = readText reader
        let jurisdiction = readJurisdiction reader
        let jurisdictionAsPublished = readTextOption reader
        let standing = readStanding reader
        let stated = readList reader readConcern
        let unconfirmed = readList reader readConcern
        let statusAsPublished = readTextOption reader
        let legalForm = readTextOption reader
        let incorporatedOn = readDayOption reader
        let ceasedOn = readDayOption reader

        let addresses =
            readAvailability reader (fun reader -> readList reader readCompanyAddress)

        let role = readRole reader
        let homeRegistration = readVoption reader readRegistration

        {
            Name = name
            OtherNames = otherNames
            Number = number
            Jurisdiction = jurisdiction
            JurisdictionAsPublished = jurisdictionAsPublished
            Standing = standing
            Concerns =
                {
                    Stated = stated
                    Unconfirmed = unconfirmed
                }
            StatusAsPublished = statusAsPublished
            LegalForm = legalForm
            IncorporatedOn = incorporatedOn
            CeasedOn = ceasedOn
            Addresses = addresses
            Role = role
            HomeRegistration = homeRegistration
            EstablishmentOf = readTextOption reader
        }

    let private readPartyKind (reader: BinaryReader) =
        match readTag reader with
        | 0 -> PartyKind.Individual
        | 1 -> PartyKind.LegalEntity
        | 2 -> PartyKind.Withheld
        | 3 -> PartyKind.Unknown(readTextOption reader)
        | _ -> bad "party kind"

    let private readYearMonth (reader: BinaryReader) : YearMonth =
        let year = reader.ReadInt32()

        {
            Year = year
            Month = reader.ReadInt32()
        }

    let private readVerification (reader: BinaryReader) : IdentityVerification =
        let verifiedOn = readDayOption reader
        let verifiedBy = readTextOption reader
        let supervisors = readTexts reader
        let possibleFrom = readDayOption reader
        let dueOn = readDayOption reader
        let suppliedOn = readDayOption reader
        let removedOn = readDayOption reader

        {
            VerifiedOn = verifiedOn
            VerifiedBy = verifiedBy
            VerifierSupervisors = supervisors
            StatementPossibleFrom = possibleFrom
            StatementDueOn = dueOn
            StatementSuppliedOn = suppliedOn
            StatementRemovedOn = removedOn
            PreferredName = readTextOption reader
        }

    let private readOfficerRole (reader: BinaryReader) =
        match readTag reader with
        | 0 -> OfficerRole.Director
        | 1 -> OfficerRole.Secretary
        | 2 -> OfficerRole.Other(readText reader)
        | _ -> bad "officer role"

    let private readOfficer (reader: BinaryReader) : Officer =
        let name = readText reader
        let kind = readPartyKind reader
        let role = readOfficerRole reader
        let current = reader.ReadBoolean()
        let appointedOn = readDayOption reader
        let endedOn = readDayOption reader
        let bornIn = readVoption reader readYearMonth
        let nationality = readTextOption reader
        let residence = readTextOption reader
        let address = readVoption reader readAddress
        let entity = readVoption reader readRegistration

        {
            Name = name
            Kind = kind
            Role = role
            Current = current
            AppointedOn = appointedOn
            EndedOn = endedOn
            BornIn = bornIn
            Nationality = nationality
            CountryOfResidence = residence
            Address = address
            Entity = entity
            IdentityVerification = readVoption reader readVerification
        }

    let private readControlKind (reader: BinaryReader) =
        match readTag reader with
        | 0 -> ControlKind.Ownership
        | 1 -> ControlKind.VotingRights
        | 2 -> ControlKind.AppointsBoard
        | 3 -> ControlKind.SignificantInfluence
        | 4 -> ControlKind.Other
        | _ -> bad "control kind"

    let private readBand (reader: BinaryReader) : PercentBand =
        let from = reader.ReadInt32()
        { From = from; To = reader.ReadInt32() }

    let private readControl (reader: BinaryReader) : NatureOfControl =
        let kind = readControlKind reader
        let band = readVoption reader readBand

        {
            Kind = kind
            Band = band
            AsPublished = readText reader
        }

    let private readOwner (reader: BinaryReader) : Owner =
        let name = readTextOption reader
        let kind = readPartyKind reader
        let control = readList reader readControl
        let current = reader.ReadBoolean()
        let since = readDayOption reader
        let endedOn = readDayOption reader
        let bornIn = readVoption reader readYearMonth
        let nationality = readTextOption reader
        let residence = readTextOption reader
        let address = readVoption reader readAddress
        let entity = readVoption reader readRegistration

        {
            Name = name
            Kind = kind
            Control = control
            Current = current
            Since = since
            EndedOn = endedOn
            BornIn = bornIn
            Nationality = nationality
            CountryOfResidence = residence
            Address = address
            Entity = entity
            IdentityVerification = readVoption reader readVerification
        }

    let private readGap (reader: BinaryReader) =
        match readTag reader with
        | 0 -> OwnershipGap.OwnerNotIdentified
        | 1 -> OwnershipGap.OwnerDetailsNotConfirmed
        | 2 -> OwnershipGap.InquiriesIncomplete
        | 3 -> OwnershipGap.OwnerNotCooperating
        | 4 -> OwnershipGap.OwnerWithheld
        | 5 -> OwnershipGap.OwnerUnreadable
        | 6 -> OwnershipGap.EntityNotIdentified
        | 7 -> OwnershipGap.StatementContradictsOwners
        | 8 -> OwnershipGap.StatementNotUnderstood(readText reader)
        | _ -> bad "ownership gap"

    let private readMeaning (reader: BinaryReader) =
        match readTag reader with
        | 0 -> OwnerStatementMeaning.NoOwnerToList
        | 1 -> OwnerStatementMeaning.AllOwnersIdentified
        | 2 -> OwnerStatementMeaning.ChangesOnly
        | 3 -> OwnerStatementMeaning.LeavesGap(readGap reader)
        | _ -> bad "statement meaning"

    let private readStatement (reader: BinaryReader) : OwnerStatement =
        let meaning = readMeaning reader

        {
            Meaning = meaning
            AsPublished = readText reader
        }

    let readOwnerListing (reader: BinaryReader) : OwnerListing =
        let owners = readList reader readOwner
        let statements = readList reader readStatement

        {
            Owners = owners
            Statements = statements
            Gaps = readList reader readGap
        }

    let readRecord (reader: BinaryReader) : BulkRecord =
        let company = readCompany reader
        let officers = readVoption reader (fun reader -> readList reader readOfficer)
        let owners = readVoption reader readOwnerListing

        {
            Company = company
            Officers = officers
            Owners = owners
            Raw = readText reader
        }

    let readOwners (reader: BinaryReader) =
        readAvailability reader readOwnerListing

    let private readSupport (reader: BinaryReader) =
        match readTag reader with
        | 0 -> Support.Supported
        | 1 -> Support.Partial(readText reader)
        | 2 -> Support.NotOffered(readText reader)
        | _ -> bad "support"

    let readCoverage (reader: BinaryReader) : RegistryCoverage =
        let registry = readText reader
        let jurisdictions = readList reader readJurisdiction
        let standing = readSupport reader
        let concerns = readSupport reader
        let addresses = readSupport reader
        let search = readSupport reader
        let officers = readSupport reader
        let owners = readSupport reader

        let cost =
            match readTag reader with
            | 0 -> AccessCost.Free
            | 1 -> AccessCost.FreeWithKey(readText reader)
            | 2 -> AccessCost.Paid(readText reader)
            | _ -> bad "access cost"

        let rateLimit =
            match readTag reader with
            | 0 ->
                let calls = reader.ReadInt32()
                let per = readTimeSpan reader
                RateLimitPolicy.Published(calls, per, readText reader)
            | 1 ->
                let calls = reader.ReadInt32()
                RateLimitPolicy.NotPublished(calls, readTimeSpan reader)
            | _ -> bad "rate limit"

        {
            Registry = registry
            Jurisdictions = jurisdictions
            Standing = standing
            Concerns = concerns
            Addresses = addresses
            Search = search
            Officers = officers
            Owners = owners
            Cost = cost
            RateLimit = rateLimit
            Licence = readText reader
        }

    let readDate (reader: BinaryReader) : BulkFileDate =
        let downloadedAt = readMoment reader

        {
            DownloadedAt = downloadedAt
            AsOf = readDayOption reader
        }

    let readAbsence (reader: BinaryReader) =
        match readTag reader with
        | 0 -> BulkAbsence.NotHeld
        | 1 -> BulkAbsence.CantTell(readText reader)
        | _ -> bad "absence"

    // Whole values

    /// value as bytes, by write.
    let encode (write: BinaryWriter -> 'T -> unit) (value: 'T) =
        use buffer = new MemoryStream()

        do
            use writer = new BinaryWriter(buffer, Encoding.UTF8, true)
            write writer value

        buffer.ToArray()

    /// What encode wrote, raising InvalidDataException when bytes are left over.
    let decode (read: BinaryReader -> 'T) (bytes: byte array) =
        use reader = new BinaryReader(new MemoryStream(bytes), Encoding.UTF8)
        let value = read reader

        if reader.BaseStream.Position <> reader.BaseStream.Length then
            bad "value"

        value

    /// The size of a frame's header: its length and its encoding byte.
    [<Literal>]
    let FrameHeader = 5

    /// A record as a frame for records.bin: its length as an int32, an encoding byte, then its bytes, deflated when that is shorter.
    let frame (value: BulkRecord) =
        let plain = encode record value

        let encoding, body =
            if plain.Length < DeflateFrom then
                Plain, plain
            else
                use deflated = new MemoryStream()

                do
                    use deflate = new DeflateStream(deflated, CompressionLevel.Fastest, true)
                    deflate.Write(plain, 0, plain.Length)

                if deflated.Length < int64 plain.Length then
                    Deflated, deflated.ToArray()
                else
                    Plain, plain

        let framed = Array.zeroCreate<byte>(body.Length + FrameHeader)
        BitConverter.TryWriteBytes(Span(framed, 0, 4), body.Length) |> ignore
        framed[4] <- encoding
        Buffer.BlockCopy(body, 0, framed, FrameHeader, body.Length)
        framed

    /// The body length a frame header gives, raising InvalidDataException for a negative one.
    let frameLength (header: ReadOnlySpan<byte>) =
        match BitConverter.ToInt32 header with
        | length when length < 0 -> bad "record frame"
        | length -> length

    /// The record of a frame's encoding byte and body.
    let unframe (encoding: byte) (body: byte array) =
        match encoding with
        | Plain -> decode readRecord body
        | Deflated ->
            use inflated = new MemoryStream()

            do
                use inflate = new DeflateStream(new MemoryStream(body), CompressionMode.Decompress)
                inflate.CopyTo inflated

            decode readRecord (inflated.ToArray())
        | _ -> bad "record frame"
