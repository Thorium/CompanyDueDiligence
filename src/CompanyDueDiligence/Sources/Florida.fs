namespace CompanyDueDiligence

open System
open System.Collections.Generic
open System.IO
open System.Text
open System.Text.RegularExpressions
open System.Threading

/// The Florida Division of Corporations' corporate data file: fixed-width records of 1440 characters, read from the layout at
/// dos.sunbiz.org/data-definitions/cor.html on 2026-11-08. The quarterly file comes as cordata.zip split in ten, and daily files over SFTP. No file
/// was downloaded, so the parser follows the layout alone; dates are read as MMDDYYYY, which the layout doesn't state.
module internal Florida =

    let coverage: RegistryCoverage =
        {
            Registry = "Florida Division of Corporations data file"
            Jurisdictions = [ Jurisdiction.ofCode "US-FL" ]
            Standing = Support.Partial "A for active or I for inactive, with no reason for inactivity"
            Concerns = Support.NotOffered "The file states no overdue filings, disputes or insolvency"
            Addresses = Support.Supported
            Search = Support.Partial "50 names containing the term, from the files passed"
            Officers =
                Support.Partial
                    "Up to six officers and the registered agent; a record flagged as having more answers NotAvailable for officers"
            Owners = Support.NotOffered "The file lists no owners"
            Cost = AccessCost.Free
            RateLimit = RateLimitPolicy.NotPublished(1, TimeSpan.FromDays 1.)
            Licence = "None stated on the layout page"
        }

    [<Literal>]
    let recordLength = 1440

    /// A field by its 1-based start and length, as the layout gives them, trimmed.
    let field (line: string) (start: int) (length: int) =
        line.Substring(start - 1, length).Trim()

    let private numberRegex = Regex @"\A(?=.*[0-9])[A-Z0-9]{4,12}\z"

    /// A document number: up to twelve letters and digits, with a digit among them.
    let normalNumber (number: string) =
        let text =
            if isNull number then
                ""
            else
                number.Trim().ToUpperInvariant()

        if numberRegex.IsMatch text then
            ValueSome text
        else
            ValueNone

    [<Literal>]
    let invalidNumber = "A Florida document number has up to twelve letters and digits"

    /// The layout's two status codes.
    let standing (status: string) =
        match status with
        | "A" -> CompanyStanding.Active
        | "I" -> CompanyStanding.Closed "Inactive"
        | "" -> CompanyStanding.Unknown ValueNone
        | other -> CompanyStanding.Unknown(ValueSome other)

    /// The layout's filing types: FOR* are foreign registrations, DOM* and FLAL formed in Florida, and the rest don't say.
    let role (filingType: string) =
        match filingType with
        | "FORP"
        | "FORNP"
        | "FORLP"
        | "FORL" -> RecordRole.ForeignRegistration
        | "DOMP"
        | "DOMNP"
        | "DOMLP"
        | "FLAL" -> RecordRole.Home
        | _ -> RecordRole.NotStated

    let filingTypeWord (filingType: string) =
        match filingType with
        | "DOMP" -> "Domestic Profit"
        | "DOMNP" -> "Domestic Non-Profit"
        | "FORP" -> "Foreign Profit"
        | "FORNP" -> "Foreign Non-Profit"
        | "DOMLP" -> "Domestic Limited Partnership"
        | "FORLP" -> "Foreign Limited Partnership"
        | "FLAL" -> "Florida Limited Liability Co."
        | "FORL" -> "Foreign Limited Liability Co."
        | "NPREG" -> "Non-Profit, Registration"
        | "TRUST" -> "Declaration of Trust"
        | "AGENT" -> "Designation of Registered Agent"
        | other -> other

    let private titleWord (code: char) =
        match code with
        | 'P' -> ValueSome "President"
        | 'T' -> ValueSome "Treasurer"
        | 'C' -> ValueSome "Chairman"
        | 'V' -> ValueSome "Vice President"
        | 'S' -> ValueSome "Secretary"
        | 'D' -> ValueSome "Director"
        | _ -> ValueNone

    /// An officer's title: D and S alone are Director and Secretary, other letters from the layout's list are spelled out, joined when
    /// combined, and a title of other letters stays as published.
    let officerRole (title: string) =
        match title with
        | "D" -> OfficerRole.Director
        | "S" -> OfficerRole.Secretary
        | _ ->
            let words = title |> Seq.map titleWord |> List.ofSeq

            if title <> "" && words |> List.forall ValueOption.isSome then
                OfficerRole.Other(words |> List.map ValueOption.get |> String.concat ", ")
            else
                OfficerRole.Other title

    let private partyKind (code: string) =
        match code with
        | "P" -> PartyKind.Individual
        | "C" -> PartyKind.LegalEntity
        | "" -> PartyKind.Unknown ValueNone
        | other -> PartyKind.Unknown(ValueSome other)

    let private date (text: string) = BulkFile.dateFrom1800 "MMddyyyy" text

    /// An address from the layout's fixed-width parts, absent when every part is blank.
    let private address
        (lines: string list)
        (locality: string)
        (region: string)
        (postalCode: string)
        (country: string)
        =
        Bulk.address
            (lines |> List.map BulkFile.nonBlank)
            (BulkFile.nonBlank locality)
            (BulkFile.nonBlank region)
            (BulkFile.nonBlank postalCode)
            (BulkFile.nonBlank country)

    let private officer (line: string) (start: int) : Officer voption =
        let name = field line (start + 5) 42

        if name = "" then
            ValueNone
        else
            ValueSome
                {
                    Name = name
                    Kind = partyKind (field line (start + 4) 1)
                    Role = officerRole (field line start 4)
                    Current = true
                    AppointedOn = ValueNone
                    EndedOn = ValueNone
                    BornIn = ValueNone
                    Nationality = ValueNone
                    CountryOfResidence = ValueNone
                    Address =
                        address
                            [ field line (start + 47) 42 ]
                            (field line (start + 89) 28)
                            (field line (start + 117) 2)
                            (field line (start + 119) 9)
                            ""
                    Entity = ValueNone
                    IdentityVerification = ValueNone
                }

    /// The layout's "more than six officers" flag, at 495: Y when the record lists only six of them.
    let moreThanSixOfficers (line: string) = line.Length >= 495 && line[494] = 'Y'

    [<Literal>]
    let moreOfficers =
        "The record says the company has more than six officers, and the file holds only six, so the list isn't whole"

    /// The registered agent, listed with the officers so that the name is kept.
    let private agent (line: string) : Officer voption =
        let name = field line 545 42

        if name = "" then
            ValueNone
        else
            ValueSome
                {
                    Name = name
                    Kind = partyKind (field line 587 1)
                    Role = OfficerRole.Other "Registered agent"
                    Current = true
                    AppointedOn = ValueNone
                    EndedOn = ValueNone
                    BornIn = ValueNone
                    Nationality = ValueNone
                    CountryOfResidence = ValueNone
                    Address = address [ field line 588 42 ] (field line 630 28) (field line 658 2) (field line 660 9) ""
                    Entity = ValueNone
                    IdentityVerification = ValueNone
                }

    /// One record. A line longer than 1440 characters, or one without a number or name, fails the file; a shorter one is padded, as trailing
    /// blanks can be lost.
    let record (line: string) : BulkRecord =
        if line.Length > recordLength then
            BulkFile.fail $"A record is longer than {recordLength} characters"

        let line = line.PadRight recordLength
        let number = field line 1 12
        let name = field line 13 192

        if number = "" || name = "" then
            BulkFile.fail "A record has no document number or name"

        let status = field line 205 1
        let filingType = field line 206 15
        let stateCountry = field line 504 2

        let addresses =
            [
                address
                    [ field line 221 42; field line 263 42 ]
                    (field line 305 28)
                    (field line 333 2)
                    (field line 335 10)
                    (field line 345 2)
                |> ValueOption.map (fun address ->
                    {
                        Kind = AddressKind.PrincipalOffice
                        Address = address
                    })
                address
                    [ field line 347 42; field line 389 42 ]
                    (field line 431 28)
                    (field line 459 2)
                    (field line 461 10)
                    (field line 471 2)
                |> ValueOption.map (fun address ->
                    {
                        Kind = AddressKind.Postal
                        Address = address
                    })
                address [ field line 588 42 ] (field line 630 28) (field line 658 2) (field line 660 9) ""
                |> ValueOption.map (fun address ->
                    {
                        Kind = AddressKind.RegisteredAgent
                        Address = address
                    })
            ]
            |> List.choose ValueOption.toOption

        let officers =
            [ 669..128..1309 ] |> List.choose (officer line >> ValueOption.toOption)

        let role = role filingType

        {
            Company =
                {
                    Name = name
                    OtherNames = []
                    Number = number
                    Jurisdiction = Jurisdiction.ofCode "US-FL"
                    JurisdictionAsPublished = BulkFile.nonBlank stateCountry
                    Standing = standing status
                    Concerns = CompanyConcerns.notPublished
                    StatusAsPublished = BulkFile.nonBlank status
                    LegalForm =
                        if filingType = "" then
                            ValueNone
                        else
                            ValueSome(filingTypeWord filingType)
                    IncorporatedOn = date (field line 473 8)
                    CeasedOn = ValueNone
                    Addresses =
                        match addresses with
                        | [] -> NotAvailable "The record holds no address"
                        | held -> Available held
                    Role = role
                    HomeRegistration =
                        match role, stateCountry with
                        | RecordRole.ForeignRegistration, code when code <> "" ->
                            ValueSome
                                {
                                    LegalName = ValueSome name
                                    LegalForm = ValueNone
                                    GoverningLaw = ValueNone
                                    Country = ValueSome code
                                    Register = ValueNone
                                    RegistrationNumber = ValueNone
                                }
                        | _ -> ValueNone
                    EstablishmentOf = ValueNone
                }
            Officers =
                match officers @ (agent line |> ValueOption.toList) with
                | [] -> ValueNone
                | listed -> ValueSome listed
            Owners = ValueNone
            Raw = line.TrimEnd()
        }

    /// The register over the files passed, in order: cordata.zip, then daily files. Each stream is one file: the parts of a zip are read as one,
    /// so a number in two parts of the quarterly file is repeated. A later file's record replaces an earlier one's. Absence proves nothing, as the
    /// quarterly file may hold active records only and a daily file holds only that day's filings. A record flagged as having more than six
    /// officers answers NotAvailable for officers.
    let build (files: Stream list) (date: BulkFileDate) (ct: CancellationToken) : ICompanyRegistry =
        let layers =
            files
            |> Seq.map (fun file ->
                BulkFile.each file (fun _ openFile ->
                    BulkFile.lines Encoding.Latin1 (openFile ())
                    |> Seq.filter (String.IsNullOrWhiteSpace >> not)
                    |> Seq.map (fun line ->
                        ct.ThrowIfCancellationRequested()
                        record line)))

        let records, repeated =
            Bulk.layered (fun (record: BulkRecord) -> normalNumber record.Company.Number) layers

        let partial =
            records
            |> Seq.filter (fun pair -> moreThanSixOfficers pair.Value.Raw)
            |> Seq.map _.Key
            |> HashSet

        let registry =
            BulkRegistry(
                coverage,
                normalNumber,
                invalidNumber,
                records,
                date,
                BulkAbsence.CantTell
                    "The quarterly file may hold active records only, and a daily file holds only that day's filings, so not finding this number proves nothing",
                "The record lists no officers",
                (fun _ -> NotAvailable "The file lists no owners"),
                repeated
            )

        BulkPartiesWithheld(
            registry,
            normalNumber,
            (fun key ->
                if partial.Contains key then
                    ValueSome moreOfficers
                else
                    ValueNone),
            (fun _ -> ValueNone)
        )
