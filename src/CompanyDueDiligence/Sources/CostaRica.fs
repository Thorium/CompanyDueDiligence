namespace CompanyDueDiligence

open System
open System.Text.Json
open System.Text.RegularExpressions
open System.Threading
open CompanyDueDiligence.Json

/// The Ministerio de Hacienda's public API, /fe/ae, read with its documentation at api.hacienda.go.cr/docs on 2026-11-08. It gives a taxpayer's
/// tax registration, not the Registro Nacional's record of the company.
module internal CostaRica =

    let coverage: RegistryCoverage =
        {
            Registry = "Ministerio de Hacienda taxpayer situation (Costa Rica)"
            Jurisdictions = [ Jurisdiction.ofCode "CR" ]
            Standing =
                Support.Partial
                    "Tax registration status, not the Registro Nacional's legal standing; arrears (moroso) or a missing return (omiso) is ActiveWithConcern"
            Concerns =
                Support.NotOffered
                    "Tax arrears and missing returns are in the standing's word; no company concerns are published"
            Addresses = Support.NotOffered "The API gives no address"
            Search = Support.NotOffered "The API looks up one identification number"
            Officers = Support.NotOffered "The API lists no officers"
            Owners = Support.NotOffered "The API lists no owners"
            Cost = AccessCost.Free
            RateLimit =
                RateLimitPolicy.Published(
                    10,
                    TimeSpan.FromSeconds 1.,
                    "api.hacienda.go.cr/docs: 10 a second sustained over 120 seconds and 20 a second in 5-second bursts, past which the IP is blocked for 10 minutes; paced at 2 a second"
                )
            Licence = "None found; the docs answered 403 on 2026-11-08"
        }

    /// A 404 says Hacienda holds no taxpayer with this number, which says nothing of the Registro Nacional.
    [<Literal>]
    let notATaxpayer =
        "Hacienda holds no taxpayer with this number, which says nothing of whether the Registro Nacional holds the company"

    let private cedulaRegex = Regex @"\A[234][0-9]{9}\z"

    /// A cédula jurídica: ten digits, the first 2 (state), 3 (private, such as 3-101 for a sociedad anónima) or 4 (autonomous institution).
    /// Hyphens and spaces are taken out. A person's number isn't looked up.
    let normalCedula (number: string) =
        let normal =
            if isNull number then
                ""
            else
                number.Replace("-", "").Replace(" ", "")

        if cedulaRegex.IsMatch normal then
            ValueSome normal
        else
            ValueNone

    /// "Inscrito" and words starting "Desinscrito", such as "Desinscrito oficio", have been seen; the documentation lists none. A registered
    /// taxpayer is Registered, and one in arrears (moroso) or with a return missing (omiso) is ActiveWithConcern, naming which. A deregistered
    /// one is Closed with its word. Any other is Unknown.
    let standing (estado: string voption) (moroso: string voption) (omiso: string voption) =
        match estado with
        | ValueSome "Inscrito" ->
            let qualifiers =
                [
                    if moroso = ValueSome "SI" then
                        "moroso: in arrears"
                    if omiso = ValueSome "SI" then
                        "omiso: returns missing"
                ]

            match qualifiers with
            | [] -> CompanyStanding.Registered(ValueSome "Inscrito")
            | _ -> CompanyStanding.ActiveWithConcern(String.concat "; " ("Inscrito" :: qualifiers))
        | ValueSome word when word.StartsWith("Desinscrito", StringComparison.Ordinal) -> CompanyStanding.Closed word
        | other -> CompanyStanding.Unknown other

    let company (cedula: string) (e: JsonElement) : Company =
        let situation = tryObject "situacion" e

        let field name =
            situation |> ValueOption.bind (SourceAnswer.trimmed name)

        let estado = field "estado"
        let moroso = field "moroso"
        let omiso = field "omiso"

        {
            Name = required "nombre" SourceAnswer.trimmed e
            OtherNames = []
            // The answer doesn't repeat the number, so it is the one asked.
            Number = cedula
            Jurisdiction = Jurisdiction.ofCode "CR"
            JurisdictionAsPublished = ValueNone
            Standing = standing estado moroso omiso
            Concerns = CompanyConcerns.notPublished
            StatusAsPublished =
                estado
                |> ValueOption.map (fun word ->
                    let flag name value =
                        value
                        |> ValueOption.map (fun v -> $"; {name} {v}")
                        |> ValueOption.defaultValue ""

                    word + flag "moroso" moroso + flag "omiso" omiso)
            LegalForm = ValueNone
            IncorporatedOn = ValueNone
            CeasedOn = ValueNone
            Addresses = NotAvailable "The API gives no address"
            Role = RecordRole.NotStated
            HomeRegistration = ValueNone
            EstablishmentOf = ValueNone
        }

    /// A 404 is a statement only when it carries the API's own error object.
    let isNotFound (e: JsonElement) = tryInt "code" e = ValueSome 404

/// Costa Rica as an ICompanyRegistry. A number Hacienda doesn't hold is NotAvailable: this is the tax register, not the company register.
[<Sealed>]
type internal CostaRicaRegistry(connection: SourceConnection) =
    /// Well below the published 10 a second, and no retry after a 429, since excess blocks the IP for 10 minutes.
    let client =
        SourceClient(
            connection,
            { SourceAnswer.pacing 2 (TimeSpan.FromSeconds 1.) with
                Retries = 0
            }
        )

    let coverage = CostaRica.coverage

    let notOffered reason jurisdiction =
        SourceAnswer.inCoverage coverage jurisdiction (fun () ->
            Threading.Tasks.Task.FromResult(Ok(SourceAnswer.unasked coverage client.Now (NotAvailable reason))))

    interface ICompanyRegistry with
        member _.Coverage = coverage

        member _.TryFindAsync(number: string, jurisdiction: Jurisdiction, ct: CancellationToken) =
            SourceAnswer.inCoverage coverage jurisdiction (fun () ->
                match CostaRica.normalCedula number with
                | ValueNone ->
                    Threading.Tasks.Task.FromResult(
                        Error(RegistryFailure.InvalidRequest "A cédula jurídica has ten digits, starting 2, 3 or 4")
                    )
                | ValueSome cedula ->
                    task {
                        let uri = Uri $"https://api.hacienda.go.cr/fe/ae?identificacion={cedula}"

                        match! client.GetAsync(uri, ct) with
                        | Error failure -> return Error failure
                        | Ok reply when reply.Status = 404 ->
                            match SourceAnswer.json reply CostaRica.isNotFound with
                            | Ok true ->
                                return
                                    Ok(
                                        SourceAnswer.evidence
                                            coverage
                                            reply
                                            ValueNone
                                            (NotAvailable CostaRica.notATaxpayer)
                                    )
                            | _ -> return Error(RegistryFailure.RequestRefused "HTTP 404")
                        | Ok reply ->
                            return
                                SourceAnswer.json reply (CostaRica.company cedula)
                                |> Result.map (fun company ->
                                    SourceAnswer.evidence coverage reply ValueNone (Available(ValueSome company)))
                    })

        member _.SearchAsync(_: string, jurisdiction: Jurisdiction, _: CancellationToken) =
            notOffered "The API looks up one identification number" jurisdiction

        member _.OfficersAsync(_: string, jurisdiction: Jurisdiction, _: CancellationToken) =
            notOffered "The API lists no officers" jurisdiction

        member _.OwnersAsync(_: string, jurisdiction: Jurisdiction, _: OwnerPurpose, _: CancellationToken) =
            notOffered "The API lists no owners" jurisdiction
