namespace CompanyDueDiligence

open System
open System.Collections.Generic
open System.Globalization
open System.IO
open System.Text
open System.Text.RegularExpressions
open System.Threading

/// AJPES's list of entities in the Slovenian Business Register (PRS) on podatki.gov.si, opsiprs.csv, refreshed twice a month. Not downloaded: the
/// portal refused this network with HTTP 407. Its column names aren't published with the file, so each column is found by the PRS attribute
/// names in atributimr20.xml (display name or internal code); a file with neither for the number or name is unreadable, never guessed.
module internal Slovenia =

    [<Literal>]
    let Registry = "AJPES PRS open data (Slovenia)"

    let coverage: RegistryCoverage =
        {
            Registry = Registry
            Jurisdictions = [ Jurisdiction.ofCode "SI" ]
            Standing =
                Support.Partial
                    "Being on the list is all it says: Registered, with no word for liquidation, bankruptcy or deletion"
            Concerns = Support.NotOffered "The list holds no concerns"
            Addresses = Support.Partial "The address as the list gives it"
            Search = Support.Partial "By name within the file, up to 50 matches"
            Officers =
                Support.NotOffered "The open data list holds no representatives; AJPES's ePRS does, after registration"
            Owners = Support.NotOffered "The open data list holds no founders or beneficial owners"
            Cost = AccessCost.Free
            RateLimit = RateLimitPolicy.NotPublished(1, TimeSpan.FromDays 14.)
            Licence = "CC BY 4.0, as the podatki.gov.si dataset page states. Not run live: the file wasn't downloaded"
        }

    /// Lower case, no diacritics, no spaces or underscores, for comparing column names.
    let private folded (name: string) =
        let decomposed = name.Trim().TrimStart('﻿').Normalize NormalizationForm.FormD

        decomposed
        |> Seq.filter (fun c ->
            CharUnicodeInfo.GetUnicodeCategory c <> UnicodeCategory.NonSpacingMark
            && Char.IsLetterOrDigit c)
        |> Seq.map Char.ToLowerInvariant
        |> Seq.toArray
        |> String

    /// The PRS attribute names, display name and internal code, a column can carry.
    let columnNames =
        [
            "number", [ "Matična številka"; "Ident_stevilka"; "Maticna_stevilka"; "Maticna" ]
            "name", [ "Naziv"; "Popolno_ime"; "Dolgi naziv poslovnega subjekta"; "Polno ime" ]
            "shortName", [ "Kratki naziv enote poslovnega registra"; "Kratko_ime" ]
            "form", [ "Pravno organizacijska oblika"; "Oblika"; "Pravnoorganizacijska oblika" ]
            "street", [ "Ulica" ]
            "house", [ "Hišna številka"; "Hisna_stevilka" ]
            "houseSuffix", [ "Dodatek k hišni številki"; "Dodatek" ]
            "settlement", [ "Naselje" ]
            "postCode", [ "Poštna številka"; "Postna_stevilka" ]
            "postTown", [ "Poštni kraj"; "Postni_kraj" ]
            "taxNumber", [ "Davčna številka"; "St_davcna" ]
        ]

    /// The header's position for each column found, by the names above.
    let resolve (header: string array) =
        let positions =
            header
            |> Array.mapi (fun position name -> folded name, position)
            |> Array.distinctBy fst
            |> dict

        columnNames
        |> List.choose (fun (column, names) ->
            names
            |> List.tryPick (fun name ->
                match positions.TryGetValue(folded name) with
                | true, position -> Some(column, position)
                | _ -> None))
        |> readOnlyDict

    /// The delimiter of the header line: the most frequent of semicolon, tab, comma and pipe.
    let delimiterOf (line: string) =
        [ ';'; '\t'; ','; '|' ]
        |> List.maxBy (fun delimiter -> line |> Seq.filter ((=) delimiter) |> Seq.length)

    let private numberRegex = Regex @"\A([0-9]{7})([0-9]{3})?\z"

    /// A registration number (matična številka): seven digits, or ten with the unit's three; seven read as the main unit, 000.
    let normalNumber (number: string) =
        let normal = if isNull number then "" else number.Trim().Replace(" ", "")
        let found = numberRegex.Match normal

        if found.Success then
            let unit =
                if found.Groups[2].Success then
                    found.Groups[2].Value
                else
                    "000"

            ValueSome(found.Groups[1].Value + unit)
        else
            ValueNone

    /// Builds the register from opsiprs.csv as the host downloaded it.
    let build (file: Stream) (date: BulkFileDate) (ct: CancellationToken) : ICompanyRegistry =
        match BulkFile.splitAt (fun _ -> true) (BulkFile.linesToEnd BulkFile.strictUtf8 file) with
        | struct (_, ValueNone, _) -> BulkFile.fail "opsiprs.csv is empty"
        | struct (_, ValueSome header, rest) ->
            let headerLine = header.Text
            let delimiter = delimiterOf headerLine
            let names = BulkFile.split delimiter true headerLine
            let columns = resolve names
            let lines = BulkFile.whole (ValueSome names.Length) delimiter true rest

            for column in [ "number"; "name" ] do
                if not (columns.ContainsKey column) then
                    let names = columnNames |> List.find (fst >> (=) column) |> snd

                    BulkFile.fail
                        $"""opsiprs.csv has no column for the {column}: expected one of {String.Join(", ", names)}"""

            let row (row: DelimitedRecord) =
                ct.ThrowIfCancellationRequested()
                let cell column = BulkFile.cell columns column row

                match cell "number" with
                | ValueNone -> BulkRow.WithoutNumber
                | ValueSome number when (normalNumber number).IsNone -> BulkRow.WithoutNumber
                | ValueSome number ->
                    match cell "name" |> ValueOption.orElse (cell "shortName") with
                    | ValueNone -> BulkRow.Unreadable(number, "The file's record for this number has no name")
                    | ValueSome name ->
                        let street =
                            [ cell "street"; cell "house"; cell "houseSuffix" ]
                            |> List.choose ValueOption.toOption
                            |> fun parts -> String.Join(" ", parts)

                        let address: Address =
                            {
                                Lines = if street = "" then [] else [ street ]
                                Locality = cell "postTown" |> ValueOption.orElse (cell "settlement")
                                Region = ValueNone
                                PostalCode = cell "postCode"
                                Country = ValueNone
                            }

                        BulkRow.Record
                            {
                                Company =
                                    {
                                        Name = name
                                        OtherNames =
                                            cell "shortName" |> ValueOption.filter ((<>) name) |> ValueOption.toList
                                        Number = number
                                        Jurisdiction = Jurisdiction.ofCode "SI"
                                        JurisdictionAsPublished = ValueNone
                                        Standing = CompanyStanding.Registered ValueNone
                                        Concerns = CompanyConcerns.notPublished
                                        StatusAsPublished = ValueNone
                                        LegalForm = cell "form"
                                        IncorporatedOn = ValueNone
                                        CeasedOn = ValueNone
                                        Addresses =
                                            if address.Lines.IsEmpty && address.Locality.IsNone then
                                                NotAvailable "The list gives no address for this entity"
                                            else
                                                Available
                                                    [
                                                        {
                                                            Kind = AddressKind.Other "Address on the PRS list"
                                                            Address = address
                                                        }
                                                    ]
                                        Role = RecordRole.NotStated
                                        HomeRegistration = ValueNone
                                        EstablishmentOf = ValueNone
                                    }
                                Officers = ValueNone
                                Owners = ValueNone
                                Raw = row.Raw
                            }

            let tally =
                BulkFile.records delimiter true lines |> Seq.map row |> Bulk.tally normalNumber

            if tally.Records = 0 then
                BulkFile.fail "opsiprs.csv holds no entity"

            BulkAnswers.Over(
                coverage,
                normalNumber,
                "A registration number has seven digits, or ten with the unit",
                tally,
                date,
                BulkAbsence.CantTell
                    "The open data list holds entities on the register now; whether deleted ones are on it isn't stated",
                "The open data list holds no representatives; AJPES's ePRS does, after registration",
                (fun _ -> NotAvailable "The open data list holds no founders or beneficial owners")
            )
