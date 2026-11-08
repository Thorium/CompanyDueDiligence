namespace CompanyDueDiligence

open System
open System.IO
open System.Text.Json
open CompanyDueDiligence.Json

/// The Serbian Business Registers Agency's (APR) open data of companies, read as openapi.apr.gov.rs/api/opendata/companies answered on
/// 2026-11-08 (DatumPreseka 2026-10-31).
module internal Serbia =

    let jurisdiction = Jurisdiction.ofCode "RS"

    let coverage: RegistryCoverage =
        {
            Registry = "APR Business Register (Serbia), open data"
            Jurisdictions = [ jurisdiction ]
            Standing = Support.Supported
            Concerns = Support.Partial "Insolvency only, from the status"
            Addresses = Support.Partial "The municipality only"
            Search = Support.Partial "Names containing the term, over the file, up to 50"
            Officers = Support.NotOffered "The open data lists no representatives"
            Owners = Support.NotOffered "The open data lists no members or shareholders"
            Cost = AccessCost.Free
            RateLimit = RateLimitPolicy.NotPublished(1, TimeSpan.FromDays 1.)
            Licence = "Open Data Licence of the Republic of Serbia, per APR's open data notes; not re-read"
        }

    let invalidNumber = "A matični broj has 8 digits"

    /// An 8-digit matični broj. Its check digit isn't tested: the published rule doesn't hold for most numbers in the file.
    let normalNumber (number: string) =
        let digits = if isNull number then "" else number.Trim()

        if digits.Length = 8 && digits |> Seq.forall Char.IsAsciiDigit then
            ValueSome digits
        else
            ValueNone

    /// The four NazivStatus words in the file of 2026-10-31. Any other is Unknown.
    let standing (word: string voption) =
        match word |> ValueOption.map _.Trim() with
        | ValueNone
        | ValueSome "" -> CompanyStanding.Unknown ValueNone
        | ValueSome "Активан" -> CompanyStanding.Active
        | ValueSome("У ликвидацији" as word)
        | ValueSome("У принудној ликвидацији" as word) -> CompanyStanding.WindingUp word
        | ValueSome("У стечају" as word) -> CompanyStanding.InInsolvency word
        | ValueSome other -> CompanyStanding.Unknown(ValueSome other)

    /// A branch or representative office of a foreign company is an establishment; any other form is a company formed here.
    let role (form: string voption) =
        match form with
        | ValueSome "Огранак страног привредног друштва"
        | ValueSome "Представништво страног привредног друштва" -> RecordRole.Establishment
        | ValueSome _ -> RecordRole.Home
        | ValueNone -> RecordRole.NotStated

    /// One company under its matični broj, or ValueNone when the number or name is missing.
    let record (number: string) (e: JsonElement) : BulkRecord voption =
        match normalNumber number, SourceAnswer.trimmed "PoslovnoIme" e with
        | ValueSome number, ValueSome name ->
            let status = SourceAnswer.trimmed "NazivStatus" e
            let standing = standing status
            let form = SourceAnswer.trimmed "NazivPravneForme" e

            ValueSome
                {
                    Company =
                        {
                            Name = name
                            OtherNames = []
                            Number = number
                            Jurisdiction = jurisdiction
                            JurisdictionAsPublished = ValueNone
                            Standing = standing
                            Concerns = Bulk.insolvencyConcerns standing.IsInInsolvency
                            StatusAsPublished = status
                            LegalForm = form
                            IncorporatedOn = tryDate "DatumOsnivanja" e
                            CeasedOn = ValueNone
                            Addresses =
                                match SourceAnswer.trimmed "NazivOpstine" e with
                                | ValueSome municipality ->
                                    Available
                                        [
                                            {
                                                Kind = AddressKind.Other "Municipality (opština)"
                                                Address =
                                                    {
                                                        Lines = []
                                                        Locality = ValueSome municipality
                                                        Region = ValueNone
                                                        PostalCode = ValueNone
                                                        Country = ValueNone
                                                    }
                                            }
                                        ]
                                | ValueNone -> NotAvailable "The record holds no municipality"
                            Role = role form
                            HomeRegistration = ValueNone
                            EstablishmentOf = ValueNone
                        }
                    Officers = ValueNone
                    Owners = ValueNone
                    Raw = $"\"{number}\": {e.GetRawText()}"
                }
        | _ -> ValueNone

    /// Builds the register from the companies answer as downloaded: { "DatumPreseka": date, "Podaci": { number: company } }. The file is parsed
    /// once from its bytes, and DatumPreseka is the AsOf.
    let build (file: Stream) (date: BulkFileDate) (ct: Threading.CancellationToken) : ICompanyRegistry =
        use document = JsonDocument.Parse(file)
        let root = document.RootElement

        match tryObject "Podaci" root with
        | ValueNone -> BulkFile.fail "The file has no Podaci object"
        | ValueSome companies ->
            let tally =
                companies.EnumerateObject()
                |> Seq.map (fun company ->
                    ct.ThrowIfCancellationRequested()

                    match record company.Name company.Value with
                    | ValueSome record -> BulkRow.Record record
                    | ValueNone -> BulkRow.WithoutNumber)
                |> Bulk.tally normalNumber

            BulkRegistry(
                coverage,
                normalNumber,
                invalidNumber,
                tally.Held,
                Bulk.asOf date (tryDate "DatumPreseka" root),
                BulkAbsence.CantTell(
                    $"The open data leaves out deleted companies and entrepreneurs, and {tally.WithoutNumber} records couldn't be read; a number it doesn't hold may still be registered"
                ),
                "The open data lists no representatives",
                (fun _ -> NotAvailable "The open data lists no members or shareholders"),
                tally.Repeated
            )
