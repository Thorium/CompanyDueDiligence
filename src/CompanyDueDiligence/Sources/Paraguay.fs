namespace CompanyDueDiligence

open System
open System.Collections.Generic
open System.IO
open System.Text
open System.Text.RegularExpressions

/// DNIT's "Listado de RUC con sus equivalencias": ruc0.zip to ruc9.zip, each a pipe-delimited text of RUC|name|DV|old RUC|status|, read from
/// ruc0.zip as published on 2026-11-01. The list is of taxpayers, people included: a person's RUC is their identity card number, and companies
/// generally hold RUCs from 80000000. It holds cancelled RUCs too.
module internal Paraguay =

    let coverage: RegistryCoverage =
        {
            Registry = "DNIT RUC list (Paraguay)"
            Jurisdictions = [ Jurisdiction.ofCode "PY" ]
            Standing = Support.Partial "The taxpayer's RUC status at DNIT, not a company register's standing"
            Concerns = Support.NotOffered "The list states no overdue filings, disputes or insolvency"
            Addresses = Support.NotOffered "The list holds no address"
            Search = Support.Partial "50 names containing the term, from the files passed; people are listed too"
            Officers = Support.NotOffered "The list names no officers"
            Owners = Support.NotOffered "The list names no owners"
            Cost = AccessCost.Free
            RateLimit = RateLimitPolicy.NotPublished(1, TimeSpan.FromDays 1.)
            Licence = "None stated; the site says all rights reserved"
        }

    let private rucRegex = Regex @"\A([0-9]{1,8}[A-Z]?)(-([0-9]))?\z"

    /// DNIT's check digit: letters count as their character codes, then weights 2 to 11 from the right, modulo 11, with 0 for a remainder
    /// under 2. It held for every one of the 202,720 rows of ruc0.txt on 2026-11-01.
    let checkDigit (ruc: string) =
        let digits =
            ruc
            |> Seq.map (fun c -> if Char.IsAsciiDigit c then string c else string (int c))
            |> String.concat ""

        let total =
            digits
            |> Seq.rev
            |> Seq.mapi (fun position c -> (int c - int '0') * (position % 10 + 2))
            |> Seq.sum

        let remainder = total % 11
        if remainder > 1 then 11 - remainder else 0

    /// The RUC without its check digit, from a RUC with or without "-DV". A check digit that doesn't match makes it invalid.
    let normalRuc (number: string) =
        let text =
            if isNull number then
                ""
            else
                number.Trim().ToUpperInvariant()

        let found = rucRegex.Match text

        if not found.Success then
            ValueNone
        else
            let ruc = found.Groups[1].Value

            if found.Groups[3].Success && found.Groups[3].Value <> string (checkDigit ruc) then
                ValueNone
            else
                ValueSome ruc

    [<Literal>]
    let invalidNumber =
        "A RUC has up to eight digits, sometimes with a letter, and may end with a hyphen and its check digit"

    /// The five words the list uses. They are a tax registration's status, so an active RUC is Registered with that said, and a suspended or
    /// blocked one is Unknown.
    let standing (status: string) =
        match status with
        | "ACTIVO" -> CompanyStanding.Registered(ValueSome "Activo (tax registration)")
        | "SUSPENSION TEMPORAL" -> CompanyStanding.Unknown(ValueSome "Suspensión temporal (tax registration)")
        | "BLOQUEADO" -> CompanyStanding.Unknown(ValueSome "Bloqueado (tax registration)")
        | "CANCELADO" -> CompanyStanding.Closed "Cancelado"
        | "CANCELADO DEFINITIVO" -> CompanyStanding.Closed "Cancelado definitivo"
        | "" -> CompanyStanding.Unknown ValueNone
        | other -> CompanyStanding.Unknown(ValueSome other)

    let private record (line: string) (ruc: string) (name: string) (digit: string) (status: string) : BulkRecord =
        {
            Company =
                {
                    Name = name
                    OtherNames = []
                    Number = $"{ruc}-{digit}"
                    Jurisdiction = Jurisdiction.ofCode "PY"
                    JurisdictionAsPublished = ValueNone
                    Standing = standing status
                    Concerns = CompanyConcerns.notPublished
                    StatusAsPublished = BulkFile.nonBlank status
                    LegalForm = ValueNone
                    IncorporatedOn = ValueNone
                    CeasedOn = ValueNone
                    Addresses = NotAvailable "The list holds no address"
                    Role = RecordRole.NotStated
                    HomeRegistration = ValueNone
                    EstablishmentOf = ValueNone
                }
            Officers = ValueNone
            Owners = ValueNone
            Raw = line
        }

    /// The register's answer for a row that can't be read.
    let private unreadableRow (ruc: string voption) =
        match ruc with
        | ValueSome key -> BulkRow.Unreadable(key, "The file's row for this RUC can't be read")
        | ValueNone -> BulkRow.WithoutNumber

    /// One row: RUC, name, check digit, old RUC, status and a closing pipe. A row of another shape, without a RUC or name, or whose check digit
    /// doesn't match, can't be read; it is for the RUC its RUC and check digit give when they agree. The real files hold names with a stray
    /// pipe, so a longer row with the check digit fourth from the end names the RUC it is for.
    let row (line: string) : BulkRow =
        let fields = line.Split '|'
        let ruc = fields[0].Trim()
        let wellShaped = fields.Length = 5 || (fields.Length = 6 && fields[5] = "")

        if wellShaped then
            let name = fields[1].Trim()
            let digit = fields[2].Trim()

            match normalRuc $"{ruc}-{digit}" with
            | ValueSome _ when name <> "" && ruc <> "" -> BulkRow.Record(record line ruc name digit (fields[4].Trim()))
            | key -> unreadableRow key
        elif fields.Length > 6 && fields[fields.Length - 1] = "" && ruc <> "" then
            unreadableRow (normalRuc $"{ruc}-{fields[fields.Length - 4].Trim()}")
        else
            BulkRow.WithoutNumber

    /// The last digit of a RUC, which names the file it is published in.
    let private fileDigit (ruc: string) =
        ruc.TrimEnd([| 'A' .. 'Z' |]) |> Seq.tryLast |> ValueOption.ofOption

    /// The register over the files passed. With RUCs ending in every digit, the ten files are all there and a RUC not in them isn't held, unless
    /// a row couldn't be read without telling which RUC it is for. A row that can't be read answers NotAvailable for its RUC, even where a
    /// readable row also gives it.
    let build (files: Stream list) (date: BulkFileDate) (ct: Threading.CancellationToken) : ICompanyRegistry =
        let read =
            files
            |> Seq.collect (fun file ->
                BulkFile.each file (fun _ openFile ->
                    BulkFile.lines BulkFile.strictUtf8 (openFile ())
                    |> Seq.filter (String.IsNullOrWhiteSpace >> not)
                    |> Seq.map (fun line ->
                        ct.ThrowIfCancellationRequested()
                        row line)))
            |> Bulk.tally normalRuc

        let withheld = read.Unreadable |> List.map fst |> set

        let tally =
            if withheld.IsEmpty then
                read
            else
                { read with
                    Held =
                        read.Held
                        |> Seq.filter (fun pair -> not (withheld.Contains pair.Key))
                        |> Seq.map (fun pair -> pair.Key, pair.Value)
                        |> readOnlyDict
                    Repeated = read.Repeated |> Seq.filter (withheld.Contains >> not) |> HashSet :> IReadOnlySet<_>
                }

        if tally.Held.Count = 0 && tally.Repeated.Count = 0 then
            BulkFile.fail "The files hold no row that can be read"

        let digits =
            Seq.concat [ tally.Held.Keys; tally.Repeated :> string seq; withheld :> string seq ]
            |> Seq.choose (fileDigit >> ValueOption.toOption)
            |> Set.ofSeq

        let missing = [ '0' .. '9' ] |> List.filter (digits.Contains >> not)

        let absence =
            match missing with
            | [] -> BulkAbsence.NotHeld
            | _ ->
                let names =
                    missing |> List.map (fun digit -> $"ruc{digit}.zip") |> String.concat ", "

                BulkAbsence.CantTell
                    $"The files passed leave out {names}, where a RUC ending in that digit is published"

        BulkAnswers.Over(
            coverage,
            normalRuc,
            invalidNumber,
            tally,
            date,
            absence,
            "The list names no officers",
            (fun _ -> NotAvailable "The list names no owners")
        )
