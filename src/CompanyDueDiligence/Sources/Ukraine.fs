namespace CompanyDueDiligence

open System
open System.Collections.Generic
open System.IO
open System.Text
open System.Text.RegularExpressions
open System.Threading
open System.Xml
open System.Xml.Linq

/// The Unified State Register's legal entities (ЄДР) on data.gov.ua, dataset 03cc1239-3988-4451-aa0d-aadb77448714: weekly uo.zip holding UO.xml,
/// read with UO_schema.xsd and the first 19,421 records of the file of 2026-11-03. The XML is windows-1251, as its declaration says. Its schema
/// holds no address, and founders, beneficial owners, signers and board members are free text in the register's wording.
module internal Ukraine =

    let coverage: RegistryCoverage =
        {
            Registry = "Unified State Register of Ukraine (ЄДР) open data"
            Jurisdictions = [ Jurisdiction.ofCode "UA" ]
            Standing = Support.Supported
            Concerns =
                Support.Partial "Bankruptcy is stated when recorded; overdue filings and disputes aren't published"
            Addresses = Support.NotOffered "The open data holds no address: its schema has none"
            Search = Support.Partial "50 names containing the term"
            Officers =
                Support.Partial
                    "Signers, board members, liquidators and insolvency officers as free text, current ones only"
            Owners = Support.Partial "Founders and beneficial owners as free text, current ones only"
            Cost = AccessCost.Free
            RateLimit = RateLimitPolicy.NotPublished(1, TimeSpan.FromDays 1.)
            Licence = "Creative Commons Attribution, as the dataset's data.gov.ua metadata states"
        }

    let private codeRegex = Regex @"\A[0-9]{8}\z"
    let private shortCodeRegex = Regex @"\A[0-9]{5,7}\z"
    let private dateRegex = Regex @"\b([0-9]{2}\.[0-9]{2}\.[0-9]{4})\b"
    let private percentRegex = Regex @"відсоток частки\s*-\s*([0-9]+)"

    /// An EDRPOU code: eight digits.
    let normalCode (number: string) =
        let text = if isNull number then "" else number.Trim()
        if codeRegex.IsMatch text then ValueSome text else ValueNone

    [<Literal>]
    let invalidNumber = "An EDRPOU code has eight digits"

    /// The register's states of activity (стан), with the two seen in records' termination and bankruptcy blocks.
    let standing (state: string) =
        match state with
        | "зареєстровано" -> CompanyStanding.Registered(ValueSome "зареєстровано")
        | "зареєстровано, свідоцтво про державну реєстрацію недійсне" ->
            CompanyStanding.Registered(ValueSome "зареєстровано, свідоцтво про державну реєстрацію недійсне")
        | "в стані припинення" -> CompanyStanding.WindingUp "в стані припинення"
        | "порушено справу про банкрутство" -> CompanyStanding.InInsolvency "порушено справу про банкрутство"
        | "порушено справу про банкрутство (санація)" ->
            CompanyStanding.InInsolvency "порушено справу про банкрутство (санація)"
        | "припинено" -> CompanyStanding.Closed "припинено"
        | "скасовано" -> CompanyStanding.Closed "скасовано"
        | "" -> CompanyStanding.Unknown ValueNone
        | other -> CompanyStanding.Unknown(ValueSome other)

    let private text (e: XElement) = BulkFile.nonBlank e.Value

    let private child (name: string) (e: XElement) =
        match e.Element(XName.Get name) with
        | null -> ValueNone
        | found -> text found

    let private items (block: string) (item: string) (e: XElement) =
        match e.Element(XName.Get block) with
        | null -> []
        | found ->
            found.Elements(XName.Get item)
            |> Seq.choose (text >> ValueOption.toOption)
            |> List.ofSeq

    let private firstDate (text: string) =
        let found = dateRegex.Match text

        if found.Success then
            BulkFile.dateFrom1800 "dd.MM.yyyy" found.Groups[1].Value
        else
            ValueNone

    let private segments (text: string) =
        text.Split(';') |> Array.map _.Trim() |> Array.filter ((<>) "") |> List.ofArray

    let private afterLabel (label: string) (parts: string list) =
        parts
        |> List.tryFind (fun part -> part.StartsWith(label, StringComparison.OrdinalIgnoreCase))
        |> Option.map (fun part -> part.Substring(label.Length).Trim())
        |> ValueOption.ofOption

    let private ukraine =
        {
            LegalName = ValueNone
            LegalForm = ValueNone
            GoverningLaw = ValueNone
            Country = ValueSome "UA"
            Register = ValueSome "ЄДР"
            RegistrationNumber = ValueNone
        }

    /// A signer, board member or liquidator: the name before the first semicolon or " - ", the role after the last " - " or, for a board member,
    /// after the semicolon, and the first date as the appointment.
    let officer (fallbackRole: string) (entry: string) : Officer =
        let dash = entry.LastIndexOf(" - ", StringComparison.Ordinal)
        let semicolon = entry.IndexOf ';'

        let nameEnd =
            [ dash; semicolon ]
            |> List.filter (fun at -> at >= 0)
            |> List.fold min entry.Length

        let role =
            if dash >= 0 then
                entry.Substring(dash + 3).Trim()
            elif semicolon >= 0 && fallbackRole = "" then
                entry.Substring(semicolon + 1).Trim()
            else
                fallbackRole

        {
            Name = entry.Substring(0, nameEnd).Trim()
            Kind = PartyKind.Individual
            Role = OfficerRole.Other(if role = "" then "підписант" else role)
            Current = true
            AppointedOn = firstDate (entry.Substring nameEnd)
            EndedOn = ValueNone
            BornIn = ValueNone
            Nationality = ValueNone
            CountryOfResidence = ValueNone
            Address = ValueNone
            Entity = ValueNone
            IdentityVerification = ValueNone
        }

    /// A founder: name; an EDRPOU code for a Ukrainian legal entity; citizenship for a person; the share in hryvnias.
    let founder (entry: string) : Owner =
        let parts = segments entry
        let code = parts |> List.tryFind codeRegex.IsMatch |> ValueOption.ofOption
        let citizenship = afterLabel "громадянство:" parts

        let share =
            parts
            |> List.tryFind (fun part -> part.StartsWith("розмір частки", StringComparison.OrdinalIgnoreCase))

        {
            Name = parts |> List.tryHead |> ValueOption.ofOption
            Kind =
                match code, citizenship with
                | ValueSome _, _ -> PartyKind.LegalEntity
                | ValueNone, ValueSome _ -> PartyKind.Individual
                | ValueNone, ValueNone -> PartyKind.Unknown ValueNone
            Control =
                [
                    {
                        Kind = ControlKind.Ownership
                        Band = ValueNone
                        AsPublished =
                            share
                            |> Option.map (fun share -> $"засновник (учасник); {share}")
                            |> Option.defaultValue "засновник (учасник)"
                    }
                ]
            Current = true
            Since = ValueNone
            EndedOn = ValueNone
            BornIn = ValueNone
            Nationality = citizenship
            CountryOfResidence = ValueNone
            Address = ValueNone
            Entity =
                code
                |> ValueOption.map (fun code ->
                    { ukraine with
                        LegalName = parts |> List.tryHead |> ValueOption.ofOption
                        RegistrationNumber = ValueSome code
                    })
            IdentityVerification = ValueNone
        }

    /// A beneficial owner: name; citizenship; the kind of influence; the percentage, read as a band from and to that figure.
    let beneficiary (entry: string) : Owner =
        let parts = segments entry
        let percent = percentRegex.Match entry

        let band =
            if percent.Success then
                match Int32.TryParse percent.Groups[1].Value with
                | true, value -> ValueSome { From = value; To = value }
                | _ -> ValueNone
            else
                ValueNone

        {
            Name = parts |> List.tryHead |> ValueOption.ofOption
            Kind = PartyKind.Individual
            Control =
                [
                    {
                        Kind =
                            if band.IsSome then
                                ControlKind.Ownership
                            else
                                ControlKind.SignificantInfluence
                        Band = band
                        AsPublished =
                            "кінцевий бенефіціарний власник; "
                            + (parts |> List.skip (min 1 parts.Length) |> String.concat "; ")
                    }
                ]
            Current = true
            Since = ValueNone
            EndedOn = ValueNone
            BornIn = ValueNone
            Nationality = afterLabel "громадянство:" parts
            CountryOfResidence = ValueNone
            Address = ValueNone
            Entity = ValueNone
            IdentityVerification = ValueNone
        }

    [<Literal>]
    let private absenceLabel = "причина відсутності:"

    /// A stated reason for listing no beneficial owner. That no person meets the definition says there is no one to list; any other reason is a gap.
    let absenceStatement (entry: string) : OwnerStatement =
        let reason = entry.Substring(absenceLabel.Length).Trim()

        {
            Meaning =
                if reason.Contains("відсутні фізичні особи", StringComparison.OrdinalIgnoreCase) then
                    OwnerStatementMeaning.NoOwnerToList
                else
                    OwnerStatementMeaning.LeavesGap(OwnershipGap.StatementNotUnderstood reason)
            AsPublished = entry
        }

    let private owners (founders: string list) (beneficiaries: string list) : OwnerListing voption =
        let isAbsence (entry: string) =
            entry.StartsWith(absenceLabel, StringComparison.OrdinalIgnoreCase)

        let statements = beneficiaries |> List.filter isAbsence |> List.map absenceStatement

        let listed =
            (founders |> List.map founder)
            @ (beneficiaries |> List.filter (isAbsence >> not) |> List.map beneficiary)

        match listed, statements with
        | [], [] -> ValueNone
        | _ ->
            let gaps =
                [
                    for owner in listed do
                        if
                            owner.Kind = PartyKind.LegalEntity
                            && owner.Entity |> ValueOption.bind _.RegistrationNumber |> ValueOption.isNone
                        then
                            yield OwnershipGap.EntityNotIdentified

                        match owner.Kind with
                        | PartyKind.Unknown _ -> yield OwnershipGap.OwnerUnreadable
                        | _ -> ()
                    // A founder's share is in hryvnias, not a percentage of the company.
                    if not founders.IsEmpty then
                        yield OwnershipGap.OwnerDetailsNotConfirmed
                    for statement in statements do
                        match statement.Meaning with
                        | OwnerStatementMeaning.LeavesGap gap -> yield gap
                        | _ -> ()
                ]
                |> List.distinct

            ValueSome
                {
                    Owners = listed
                    Statements = statements
                    Gaps = gaps
                }

    /// Parts of a record that aren't read, left out of Raw to keep it small.
    let private unread =
        [ "STATUTE"; "EXCHANGE_DATA"; "BRANCHES"; "PREDECESSORS"; "ASSIGNEES" ]

    /// One SUBJECT. ValueNone for a record with no EDRPOU code, which can't be looked up; a record with a code and no name fails the file.
    let record (subject: XElement) : BulkRecord voption =
        match child "EDRPOU" subject with
        | ValueNone -> ValueNone
        | ValueSome code ->
            let name =
                match child "NAME" subject with
                | ValueSome name -> name
                | ValueNone -> BulkFile.fail "A record has an EDRPOU code and no name"

            // A code that lost its leading zeros is put back to eight digits.
            let number =
                if shortCodeRegex.IsMatch code then
                    code.PadLeft(8, '0')
                else
                    code

            let state = child "STAN" subject |> ValueOption.defaultValue ""
            let terminated = child "TERMINATED_INFO" subject
            let bankruptcy = subject.Element(XName.Get "BANKRUPTCY_READJUSTMENT_INFO")

            let insolvent =
                (not (isNull bankruptcy) && bankruptcy.HasElements)
                || terminated
                   |> ValueOption.exists (fun info -> info.Contains("банкрут", StringComparison.OrdinalIgnoreCase))

            let liquidators =
                [
                    yield! items "TERMINATION_STARTED_INFO" "SIGNER_NAME" subject |> List.map (officer "")
                    yield!
                        items "BANKRUPTCY_READJUSTMENT_INFO" "BANKRUPTCY_READJUSTMENT_HEAD_NAME" subject
                        |> List.map (officer "")
                ]

            let officers =
                (items "SIGNERS" "SIGNER" subject |> List.map (officer "підписант"))
                @ (items "MEMBERS" "MEMBER" subject |> List.map (officer ""))
                @ liquidators

            let raw = XElement subject

            raw.Elements()
            |> Seq.filter (fun e -> List.contains e.Name.LocalName unread)
            |> List.ofSeq
            |> List.iter _.Remove()

            ValueSome
                {
                    Company =
                        {
                            Name = name
                            OtherNames =
                                child "SHORT_NAME" subject
                                |> ValueOption.filter ((<>) name)
                                |> ValueOption.toList
                            Number = number
                            Jurisdiction = Jurisdiction.ofCode "UA"
                            JurisdictionAsPublished = ValueNone
                            Standing = standing state
                            Concerns = Bulk.insolvencyConcerns insolvent
                            StatusAsPublished = BulkFile.nonBlank state
                            LegalForm = child "OPF" subject
                            IncorporatedOn = child "REGISTRATION" subject |> ValueOption.bind firstDate
                            CeasedOn = terminated |> ValueOption.bind firstDate
                            Addresses = NotAvailable "The open data holds no address: its schema has none"
                            Role = RecordRole.Home
                            HomeRegistration = ValueNone
                            EstablishmentOf = ValueNone
                        }
                    Officers =
                        if List.isEmpty officers then
                            ValueNone
                        else
                            ValueSome officers
                    Owners = owners (items "FOUNDERS" "FOUNDER" subject) (items "BENEFICIARIES" "BENEFICIARY" subject)
                    Raw = raw.ToString SaveOptions.DisableFormatting
                }

    let private declaredEncodingRegex = Regex "encoding=[\"']([A-Za-z0-9_.:-]+)[\"']"

    /// The encoding the XML declaration names, read from the file's first bytes; UTF-8 without one. UTF-8 fails on bytes that aren't UTF-8.
    let private declaredEncoding (stream: Stream) =
        let head = Array.zeroCreate<byte> 256
        let read = stream.ReadAtLeast(head, head.Length, false)
        let found = declaredEncodingRegex.Match(Encoding.Latin1.GetString(head, 0, read))

        if not found.Success then
            BulkFile.strictUtf8
        else
            let name = found.Groups[1].Value

            let encoding =
                match CodePagesEncodingProvider.Instance.GetEncoding name with
                | null ->
                    try
                        Encoding.GetEncoding name
                    with :? ArgumentException ->
                        BulkFile.fail "The XML declares an encoding that can't be read"
                | encoding -> encoding

            if encoding.CodePage = Encoding.UTF8.CodePage then
                BulkFile.strictUtf8
            else
                encoding

    /// The records of one XML file as they stream: each SUBJECT is read, turned into its record and let go.
    let private readXml (ct: CancellationToken) (openFile: unit -> Stream) : BulkRecord seq =
        seq {
            let encoding = declaredEncoding (openFile ())

            let settings =
                XmlReaderSettings(
                    DtdProcessing = DtdProcessing.Prohibit,
                    IgnoreWhitespace = true,
                    IgnoreComments = true
                )

            use text = new StreamReader(openFile (), encoding, false, 65536, true)
            use reader = XmlReader.Create(text, settings)

            yield!
                BulkFile.elements "SUBJECT" reader
                |> Seq.choose (fun subject ->
                    ct.ThrowIfCancellationRequested()
                    record subject |> ValueOption.toOption)
        }

    /// The records of uo.zip, or of the XML files in it, as they stream.
    let records (file: Stream) (ct: CancellationToken) : BulkRecord seq =
        BulkFile.each file (fun name openFile ->
            if name = "" || name.EndsWith(".xml", StringComparison.OrdinalIgnoreCase) then
                readXml ct openFile
            else
                Seq.empty)

    /// A record's key.
    let keyOf (record: BulkRecord) = normalCode record.Company.Number

    /// The register over records by key, with the keys several records give. A code not in the file can still be held: a branch's code sits
    /// in its parent's record, public formations without legal personality are in fsu.zip, and some records carry no code.
    let over
        (records: IReadOnlyDictionary<string, BulkRecord>)
        (repeated: IReadOnlySet<string>)
        (date: BulkFileDate)
        : ICompanyRegistry =
        new BulkRegistry(
            coverage,
            normalCode,
            invalidNumber,
            records,
            date,
            BulkAbsence.CantTell
                "uo.zip lists legal entities, ended ones too, but a branch's code sits in its parent's record, public formations are in fsu.zip and some records carry no code",
            "The record lists no signers or board members",
            (fun _ -> NotAvailable "The record lists no founders or beneficial owners"),
            repeated
        )

    /// The register over uo.zip, or the XML files in it, as over describes it.
    let build (file: Stream) (date: BulkFileDate) (ct: CancellationToken) : ICompanyRegistry =
        let held, repeated = records file ct |> Bulk.index keyOf
        over held repeated date
