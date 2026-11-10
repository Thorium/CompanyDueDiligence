namespace CompanyDueDiligence

open System
open System.Text.Json
open System.Text.RegularExpressions
open System.Threading
open CompanyDueDiligence.Json

/// The Malta Business Registry's open data, read as openapi.baros.mbr.mt answered on 2026-11-06.
module internal Malta =

    let coverage: RegistryCoverage =
        {
            Registry = "Malta Business Registry"
            Jurisdictions = [ Jurisdiction.ofCode "MT" ]
            Standing = Support.Supported
            Concerns = Support.NotOffered "The open data states no filing dates, disputes or insolvency"
            Addresses = Support.Supported
            Search = Support.NotOffered "The API looks up by number or lists everything; it has no search by name"
            Officers = Support.NotOffered "The open data lists no officers"
            Owners = Support.NotOffered "The open data lists no owners"
            Cost = AccessCost.Free
            RateLimit = RateLimitPolicy.NotPublished(2, TimeSpan.FromSeconds 1.)
            Licence = "CC BY 4.0, per data.gov.mt's Basic Company Information record"
        }

    /// The words in companies_opendata.csv on 2026-11-06, all 14 of them. Any other is Unknown.
    let standing (word: string voption) =
        match word |> ValueOption.map _.Trim() with
        | ValueNone
        | ValueSome "" -> CompanyStanding.Unknown ValueNone
        | ValueSome word ->
            match word with
            | "Active" -> CompanyStanding.Active
            | "In Process of Merging"
            | "In Process of Converting"
            | "In Process of Dividing" -> CompanyStanding.ActiveWithConcern word
            | "In Dissolution"
            | "Simplified Dissolution" -> CompanyStanding.WindingUp word
            | "Struck Off"
            | "Struck Off as Defunct"
            | "Struck Off Following Merger"
            | "Struck Off Following Division"
            | "Struck Off Following Cross-Border Conversion"
            | "Struck Off Following Cross-Border Merger"
            | "Converted to SE" -> CompanyStanding.Closed word
            | other -> CompanyStanding.Unknown(ValueSome other)

    let private numberRegex = Regex @"\A([A-Z]+)[\s.\-/]*([0-9][0-9A-Z/\-]*)\z"

    /// A number in the register's spelling, a prefix, a space and the number, such as "C 1234": "C1234", "C-1234" and "C/1234" are that
    /// number. The prefix is what tells its registers apart, so one without a prefix isn't sent.
    let normalNumber (number: string) =
        let found =
            numberRegex.Match(
                if isNull number then
                    ""
                else
                    number.Trim().ToUpperInvariant()
            )

        if found.Success then
            ValueSome $"{found.Groups[1].Value} {found.Groups[2].Value}"
        else
            ValueNone

    /// The register's address field can hold a note in place of an address, such as "STRUCK OFF WEF 23/04/81".
    let private addresses (e: JsonElement) =
        match SourceAnswer.trimmed "address" e with
        | ValueSome text when text.StartsWith("STRUCK OFF", StringComparison.OrdinalIgnoreCase) ->
            NotAvailable "The address field holds a note, not an address"
        | ValueNone -> NotAvailable "The answer holds no address"
        | ValueSome text ->
            Available
                [
                    {
                        Kind = AddressKind.Other "Address on the Malta Business Registry"
                        Address =
                            {
                                Lines = [ text ]
                                Locality = SourceAnswer.trimmed "locality" e
                                Region = ValueNone
                                PostalCode = SourceAnswer.trimmed "postcode" e
                                Country = SourceAnswer.trimmed "country" e
                            }
                    }
                ]

    let company (e: JsonElement) : Company =
        let form = tryString "type" e
        let status = tryString "state" e

        {
            Name = required "name" tryString e
            Number = required "registration_number" tryString e
            Jurisdiction = Jurisdiction.ofCode "MT"
            JurisdictionAsPublished = ValueNone
            Standing = standing status
            Concerns = CompanyConcerns.notPublished
            StatusAsPublished = status
            LegalForm = form
            IncorporatedOn = tryDate "registration_date" e
            CeasedOn = ValueNone
            Addresses = addresses e
            Role =
                match form with
                | ValueSome form when form.StartsWith("Overseas", StringComparison.OrdinalIgnoreCase) ->
                    RecordRole.ForeignRegistration
                | ValueSome _ -> RecordRole.Home
                | ValueNone -> RecordRole.NotStated
            HomeRegistration = ValueNone
            OtherNames = []
            EstablishmentOf = ValueNone
        }

    /// The company asked for. Another number in the answer fails the read rather than being taken for it.
    let answer (number: string) (e: JsonElement) =
        match tryObject "data" e with
        | ValueSome data ->
            let found = company data

            if normalNumber found.Number = ValueSome number then
                found
            else
                raise (InvalidJson "The answer holds another company")
        | ValueNone -> raise (InvalidJson "The answer has no data object")

    /// The registry's own answer for a number it doesn't hold, as seen on 2026-11-06: an errors object whose detail is "Not Found".
    let isNotFound (e: JsonElement) =
        tryObject "errors" e |> ValueOption.bind (tryString "detail") = ValueSome "Not Found"

/// The Malta Business Registry as an ICompanyRegistry. It answers a number it doesn't hold with a 404 carrying its own error, which reads as not
/// held; any other 404 is refused.
[<Sealed>]
type internal MaltaRegistry(connection: SourceConnection) =
    let client =
        SourceClient(connection, SourceAnswer.pacing 2 (TimeSpan.FromSeconds 1.))

    let coverage = Malta.coverage

    let offNumber number send =
        match Malta.normalNumber number with
        | ValueSome normal -> send normal
        | ValueNone ->
            Threading.Tasks.Task.FromResult(
                Error(RegistryFailure.InvalidRequest "A Malta number has a letter prefix and a number, such as C 1234")
            )

    interface ICompanyRegistry with
        member _.Coverage = coverage

        member _.TryFindAsync(number: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                offNumber number (fun normal ->
                    task {
                        let uri =
                            Uri $"https://openapi.baros.mbr.mt/api/v1/companies/{Uri.EscapeDataString normal}"

                        match! client.GetAsync(uri, ct) with
                        | Error failure -> return Error failure
                        | Ok reply when reply.Status = 404 ->
                            return
                                match SourceAnswer.json reply Malta.isNotFound with
                                | Ok true -> Ok(SourceAnswer.evidence coverage reply ValueNone (Available ValueNone))
                                | _ ->
                                    Error(RegistryFailure.RequestRefused "HTTP 404 without the registry's own error")
                        | Ok reply ->
                            return
                                SourceAnswer.json reply (Malta.answer normal)
                                |> Result.map (fun company ->
                                    SourceAnswer.evidence coverage reply ValueNone (Available(ValueSome company)))
                    }))

        member _.SearchAsync(_: string, jurisdiction: Jurisdiction, _: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                Threading.Tasks.Task.FromResult(
                    Ok(
                        SourceAnswer.unasked
                            coverage
                            client.Now
                            (NotAvailable "The API has no search by name; companies_opendata.csv holds every company")
                    )
                ))

        member _.OfficersAsync(_: string, jurisdiction: Jurisdiction, _: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                Threading.Tasks.Task.FromResult(
                    Ok(SourceAnswer.unasked coverage client.Now (NotAvailable "The open data lists no officers"))
                ))

        member _.OwnersAsync(_: string, jurisdiction: Jurisdiction, purpose: OwnerPurpose, _: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                Threading.Tasks.Task.FromResult(
                    Ok(
                        SourceAnswer.unasked
                            coverage
                            client.Now
                            (SourceAnswer.restrictedOwners
                                purpose
                                "Unverified: firms under anti-money-laundering rules, as the register defines them"
                                "Apply to the Malta Business Registry for beneficial owner access, or use a vendor")
                    )
                ))
