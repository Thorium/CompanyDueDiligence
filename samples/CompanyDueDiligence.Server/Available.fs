namespace CompanyDueDiligence.Server

open System
open System.Collections.Generic
open CompanyDueDiligence
open CompanyDueDiligence.Web.Registers
open CompanyDueDiligence.Web.Dto

/// A register the server can read now, as listed and as built.
type AvailableRegister =
    {
        Listed: RegisterDto
        Registry: ICompanyRegistry
    }

/// The registers available now: every keyless source, each keyed source whose settings are all given, and each bulk source whose index is
/// built. A keyed source's setting is read from keys.json, else from the environment variable of the same name.
type ServerRegisters(connection: SourceConnection, keys: KeyStore, bulk: BulkService) =

    let keylessRegisters =
        keyless
        |> List.map (fun source ->
            let registry = source.Make connection

            {
                Listed =
                    registerOf
                        source.Code
                        source.Label
                        source.Region
                        source.Flag
                        Kind.Keyless
                        source.Code
                        source.Example
                        registry
                Registry = registry
            })

    let gate = obj ()

    /// Each keyed registry with the values it was built from, so a source holding pacing is built again only when its keys change.
    let built = Dictionary<string, string list * AvailableRegister>()

    let fromEnvironment (name: string) =
        Environment.GetEnvironmentVariable name
        |> ValueOption.ofObj
        |> ValueOption.filter (String.IsNullOrWhiteSpace >> not)

    let setting (name: string) =
        keys.TryGet name
        |> ValueOption.filter (String.IsNullOrWhiteSpace >> not)
        |> ValueOption.orElse (fromEnvironment name)

    /// The source's settings as KeyedSources.fromSettings reads them, the others hidden.
    let settingsOf (source: KeyedSource) =
        KeyedSources.fromSettings connection (fun name ->
            if List.contains name source.Settings then
                setting name
            else
                ValueNone)

    let keyedRegister (source: KeyedSource) =
        let values = source.Settings |> List.map (setting >> ValueOption.map _.Trim())

        if values |> List.forall ValueOption.isSome then
            let given = values |> List.map ValueOption.get

            lock gate (fun () ->
                match built.TryGetValue source.Code with
                | true, (was, register) when was = given -> ValueSome register
                | _ ->
                    match (settingsOf source).Registries with
                    | [ registry ] ->
                        let register =
                            {
                                Listed =
                                    registerOf
                                        source.Code
                                        source.Label
                                        source.Region
                                        source.Flag
                                        Kind.Keyed
                                        source.Jurisdiction
                                        source.Example
                                        registry
                                Registry = registry
                            }

                        built[source.Code] <- (given, register)
                        ValueSome register
                    | _ -> ValueNone)
        else
            ValueNone

    /// Every register available now: Companies House first, then the keyless, the other keyed and the bulk sources.
    member _.AllAsync() =
        task {
            let! opened = bulk.Registries()

            let keyedNow = keyed |> List.choose (keyedRegister >> ValueOption.toOption)

            let companiesHouse, otherKeyed =
                keyedNow |> List.partition (fun register -> register.Listed.Code = "GB")

            let bulkRegisters =
                opened
                |> List.map (fun (entry, registry) ->
                    {
                        Listed =
                            registerOf
                                entry.Id
                                entry.Title
                                (regionOf entry.Jurisdiction.Country)
                                (flagCodeOf entry.Jurisdiction)
                                Kind.Bulk
                                entry.Jurisdiction.Code
                                ""
                                registry
                        Registry = registry
                    })

            return companiesHouse @ keylessRegisters @ otherKeyed @ bulkRegisters
        }

    /// Each keyed source with its settings, saved or from the environment, never their values.
    member _.Keys() =
        keyed
        |> List.map (fun source ->
            let missing = source.Settings |> List.filter (setting >> ValueOption.isNone)

            {
                KeyedRegisterDto.Code = source.Code
                Label = source.Label
                Flag = source.Flag
                Settings =
                    source.Settings
                    |> List.map (fun name ->
                        {
                            KeySettingDto.Name = name
                            Saved = keys.IsSaved name
                            FromEnvironment = (fromEnvironment name).IsSome
                        })
                    |> Array.ofList
                Active = (keyedRegister source).IsSome
                Problem =
                    if missing.IsEmpty || missing.Length = source.Settings.Length then
                        null
                    else
                        String.Join(", ", missing) + " not given"
            })
