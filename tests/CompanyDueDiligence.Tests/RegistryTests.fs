/// The contract, run the same way against Companies House over a fake handler and a second, made-up register.
module RegistryTests

open System
open System.Threading
open System.Threading.Tasks
open Xunit
open CompanyDueDiligence
open CompanyDueDiligence.CompaniesHouse
open Fake

let private zz = Jurisdiction.ofCode "ZZ"
let private gb = Jurisdiction.ofCode "GB"
let private fixedTime = DateTimeOffset(2026, 11, 6, 12, 0, 0, TimeSpan.Zero)

/// A register for a made-up jurisdiction. It publishes companies and officers but not owners, and no standing for one of its companies.
type private FakeRegistry() =
    let answered value : Evidence<_> =
        {
            Source = "Zedland Register"
            RetrievedAt = fixedTime
            AsOf = ValueNone
            Raw = ValueNone
            Value = value
        }

    let company number standing concerns status : Company =
        {
            Name = $"ZEDCO {number}"
            Number = number
            Jurisdiction = zz
            JurisdictionAsPublished = ValueSome "Zedland"
            Standing = standing
            Concerns = concerns
            StatusAsPublished = status
            LegalForm = ValueNone
            IncorporatedOn = ValueNone
            CeasedOn = ValueNone
            Addresses = NotAvailable "Zedland publishes no addresses"
            Role = RecordRole.Home
            HomeRegistration = ValueNone
            EstablishmentOf = ValueNone
            OtherNames = []
        }

    let covered jurisdiction (answer: 'T) =
        if RegistryCoverage.covers FakeRegistry.Coverage jurisdiction then
            Task.FromResult(Ok(answered answer))
        else
            Task.FromResult(Error(RegistryFailure.OutsideCoverage jurisdiction))

    static member Coverage: RegistryCoverage =
        {
            Registry = "Zedland Register"
            Jurisdictions = [ zz ]
            Standing = Support.Partial "Not every company has a published status"
            Concerns = Support.NotOffered "Zedland publishes no filing dates or disputes"
            Addresses = Support.NotOffered "Zedland publishes no addresses"
            Search = Support.Supported
            Officers = Support.Supported
            Owners = Support.NotOffered "Zedland publishes no owners"
            Cost = AccessCost.Free
            RateLimit = RateLimitPolicy.NotPublished(1, TimeSpan.FromSeconds 1.)
            Licence = "None stated"
        }

    interface ICompanyRegistry with
        member _.Coverage = FakeRegistry.Coverage

        member _.TryFindAsync(number, jurisdiction, _) =
            covered
                jurisdiction
                (Available(
                    match number with
                    | "ZZ-1" ->
                        ValueSome(company number CompanyStanding.Active CompanyConcerns.notPublished (ValueSome "GOOD"))
                    | "ZZ-2" ->
                        ValueSome(
                            company number (CompanyStanding.Unknown ValueNone) CompanyConcerns.notPublished ValueNone
                        )
                    | _ -> ValueNone
                ))

        member _.SearchAsync(_, jurisdiction, _) =
            covered
                jurisdiction
                (Available
                    {
                        Matches = []
                        TotalMatches = ValueSome 0
                        Complete = true
                    })

        member _.OfficersAsync(_, jurisdiction, _) = covered jurisdiction (Available [])

        member _.OwnersAsync(_, jurisdiction, _, _) =
            covered jurisdiction (NotAvailable "Zedland publishes no owners")

let private profileJson =
    """{"company_number":"00000006","company_name":"MARINE AND GENERAL","company_status":"active","jurisdiction":"england-wales",
        "type":"ltd","date_of_creation":"1900-01-01","registered_office_address":{"address_line_1":"1 Road","postal_code":"AB1 2CD"},
        "has_insolvency_history":false,"has_been_liquidated":false,"registered_office_is_in_dispute":false,"undeliverable_registered_office_address":false,
        "accounts":{"next_accounts":{"overdue":false}},"confirmation_statement":{"overdue":false}}"""

let private pscJson =
    """{"kind":"individual-person-with-significant-control","name":"Ms Jane Doe","notified_on":"2016-04-06","nationality":"British",
        "date_of_birth":{"month":3,"year":1970},"natures_of_control":["ownership-of-shares-75-to-100-percent","voting-rights-more-than-25-percent-registered-overseas-entity","right-to-appoint-and-remove-directors","significant-influence-or-control","something-new"]}"""

let private statementJson code =
    $$"""{"statement":"{{code}}","notified_on":"2016-04-06"}"""

/// A PSC or statement list as the spec has it, counting an item as ceased if it says so. The list after the first page is empty.
let private pscListAt (call: Call) (items: string list) =
    let ceased =
        items
        |> List.filter (fun item -> item.Contains "ceased_on" || item.Contains "\"ceased\":true")
        |> List.length

    pscsJson items.Length ceased (if startIndexOf call = 0 then items else [])

/// A Companies House fake holding one company, with the given PSC and statement lists and profile.
let private companiesHouseWith (profile: string) (pscs: string list) (statements: string list) (call: Call) =
    let path = Uri(call.Uri).AbsolutePath

    if path.EndsWith "/search/companies" then
        ok (
            listJson
                "total_results"
                44
                [
                    """{"company_number":"00000006","title":"MARINE AND GENERAL","company_status":"active"}"""
                ]
        )
    elif not (path.Contains "00000006") then
        notFound
    elif path.EndsWith "/persons-with-significant-control" then
        ok (pscListAt call pscs)
    elif path.EndsWith "/persons-with-significant-control-statements" then
        ok (pscListAt call statements)
    elif path.EndsWith "/officers" then
        ok (
            officersJson
                2
                1
                (if startIndexOf call = 0 then
                     [
                         """{"name":"DOE, Jane","officer_role":"director","appointed_on":"2010-05-06"}"""
                         """{"name":"A FIRM LTD","officer_role":"corporate-secretary","resigned_on":"2011-05-06"}"""
                     ]
                 else
                     [])
        )
    else
        ok profile

let private companiesHouse = companiesHouseWith profileJson

/// Answers the company's profile with the one given, and any other call as respond does: a list is read against its profile.
let private withProfileOf (profile: string) (respond: Call -> Answer) (call: Call) =
    if Uri(call.Uri).AbsolutePath.EndsWith "/company/00000006" then
        ok profile
    else
        respond call

let private withProfile = withProfileOf profileJson

let private registryOver (handler: FakeHandler) : ICompanyRegistry =
    CompaniesHouseRegistry(clientWith handler, fun () -> fixedTime)

let private expectEvidence (result: Result<Evidence<'T>, RegistryFailure>) =
    result |> Result.defaultWith (RegistryFailure.describe >> failwith)

/// The company in an answer that found one.
let private companyIn (evidence: Evidence<Availability<Company voption>>) =
    match evidence.Value with
    | Available(ValueSome company) -> company
    | other -> failwith $"%A{other}"

/// What a test reads owners with.
let private purpose = OwnerPurpose.CustomerDueDiligence

/// True when nothing on the record keeps a company from reading as active with no concerns. Application code in these tests writes its own rule: the library has none.
let private nothingAgainst (company: Company) =
    CompanyStanding.isActive company.Standing
    && List.isEmpty company.Concerns.Stated
    && List.isEmpty company.Concerns.Unconfirmed

/// The rules any registry keeps: run through the interface alone.
let private keepsTheContract
    (registry: ICompanyRegistry)
    (inside: Jurisdiction)
    (known: string)
    (missing: string)
    (outside: Jurisdiction)
    =
    task {
        let coverage = registry.Coverage
        Assert.True(RegistryCoverage.covers coverage inside)
        Assert.False(RegistryCoverage.covers coverage outside)

        let! found = registry.TryFindAsync(known, inside, ct)
        let found = expectEvidence found
        Assert.Equal(coverage.Registry, found.Source)
        Assert.Equal(fixedTime, found.RetrievedAt)
        Assert.Equal(known, (companyIn found).Number)

        // "Not found" is evidence too, and carries its source.
        let! notFound = registry.TryFindAsync(missing, inside, ct)
        let notFound = expectEvidence notFound
        Assert.Equal(Available ValueNone, notFound.Value)
        Assert.Equal(coverage.Registry, notFound.Source)
        Assert.Equal(fixedTime, notFound.RetrievedAt)

        // A jurisdiction outside coverage is a failure, never "this company doesn't exist".
        let! elsewhere = registry.TryFindAsync(known, outside, ct)
        Assert.Equal(Error(RegistryFailure.OutsideCoverage outside), elsewhere)
        let! elsewhereSearch = registry.SearchAsync("a", outside, ct)
        Assert.Equal(Error(RegistryFailure.OutsideCoverage outside), elsewhereSearch)
        let! elsewhereOfficers = registry.OfficersAsync(known, outside, ct)
        Assert.Equal(Error(RegistryFailure.OutsideCoverage outside), elsewhereOfficers)
        let! elsewhereOwners = registry.OwnersAsync(known, outside, purpose, ct)
        Assert.Equal(Error(RegistryFailure.OutsideCoverage outside), elsewhereOwners)

        // What Coverage says isn't offered answers NotAvailable, not an empty list.
        let! owners = registry.OwnersAsync(known, inside, purpose, ct)

        match coverage.Owners, (expectEvidence owners).Value with
        | Support.NotOffered _, NotAvailable _ -> ()
        | Support.NotOffered _, other -> failwith $"Declared no owners, answered {other}"
        | _ -> ()

        // What Coverage says isn't offered is unconfirmed, never clear.
        match coverage.Concerns, companyIn found with
        | Support.NotOffered _, company ->
            Assert.Equal<CompanyConcern list>(CompanyConcern.all, company.Concerns.Unconfirmed)
            Assert.False(nothingAgainst company)
        | _ -> ()
    }

[<Fact>]
let ``Companies House keeps the contract`` () : Task =
    task {
        use handler = new FakeHandler(companiesHouse [ pscJson ] [])
        do! keepsTheContract (registryOver handler) gb "00000006" "99999999" zz
    }

[<Fact>]
let ``a second register for a made-up jurisdiction keeps the same contract`` () : Task =
    keepsTheContract (FakeRegistry()) zz "ZZ-1" "ZZ-404" gb

/// Application code that knows only the contract.
let private onboardingVerdict (registries: ICompanyRegistry list) (number: string) (jurisdiction: Jurisdiction) =
    task {
        match Registries.forJurisdiction registries jurisdiction with
        | ValueNone -> return "no register"
        | ValueSome registry ->
            match! registry.TryFindAsync(number, jurisdiction, ct) with
            | Error failure -> return RegistryFailure.describe failure
            | Ok { Value = NotAvailable _ }
            | Ok { Value = NeedsEntitlement _ } -> return "review: the source can't tell"
            | Ok { Value = Available ValueNone } -> return "not on the register"
            | Ok { Value = Available(ValueSome company) } when not (CompanyStanding.isActive company.Standing) ->
                return "review: standing"
            | Ok { Value = Available(ValueSome company) } when not (nothingAgainst company) -> return "review: concerns"
            | Ok _ ->
                match! registry.OwnersAsync(number, jurisdiction, purpose, ct) with
                | Ok { Value = NotAvailable _ } -> return "review: owners not available"
                | Ok { Value = NeedsEntitlement _ } -> return "review: owners need entitlement"
                | Ok { Value = Available listing } when not (OwnerListing.isComplete listing) ->
                    return "review: ownership gaps"
                | Ok { Value = Available _; Source = source } -> return $"pass, from {source}"
                | Error failure -> return RegistryFailure.describe failure
    }

[<Fact>]
let ``one piece of application code reads both registers through the contract`` () : Task =
    task {
        use handler = new FakeHandler(companiesHouse [ pscJson ] [])
        let registries = [ registryOver handler; FakeRegistry() :> ICompanyRegistry ]

        let! uk = onboardingVerdict registries "00000006" gb
        Assert.Equal("pass, from Companies House", uk)

        let! scottish =
            onboardingVerdict registries "00000006" (Jurisdiction.ofCode "GB-SCT")

        Assert.Equal("pass, from Companies House", scottish)

        let! ukMissing = onboardingVerdict registries "99999999" gb
        Assert.Equal("not on the register", ukMissing)

        // The made-up register publishes no concerns or owners, and those read as gaps rather than as none.
        let! zed = onboardingVerdict registries "ZZ-1" zz
        Assert.Equal("review: concerns", zed)

        let! zedUnknown = onboardingVerdict registries "ZZ-2" zz
        Assert.Equal("review: standing", zedUnknown)

        let! elsewhere = onboardingVerdict registries "1" (Jurisdiction.ofCode "US-DE")
        Assert.Equal("no register", elsewhere)
    }

[<Theory>]
[<InlineData("psc-exists-but-not-identified")>]
[<InlineData("psc-details-not-confirmed")>]
[<InlineData("a-statement-added-later")>]
let ``application code doesn't pass an owner with a statement that qualifies the listing`` (code: string) : Task =
    task {
        use handler = new FakeHandler(companiesHouse [ pscJson ] [ statementJson code ])
        let! verdict = onboardingVerdict [ registryOver handler ] "00000006" gb
        Assert.Equal("review: ownership gaps", verdict)
    }

[<Theory>]
[<InlineData("has_insolvency_history")>]
[<InlineData("has_been_liquidated")>]
[<InlineData("registered_office_is_in_dispute")>]
[<InlineData("undeliverable_registered_office_address")>]
let ``application code doesn't pass an active company with a red flag`` (flag: string) : Task =
    task {
        let profile = profileJson.Replace($"\"{flag}\":false", $"\"{flag}\":true")
        Assert.NotEqual<string>(profileJson, profile)
        use handler = new FakeHandler(companiesHouseWith profile [ pscJson ] [])
        let! verdict = onboardingVerdict [ registryOver handler ] "00000006" gb
        Assert.Equal("review: concerns", verdict)
    }

let private concernsOf (profile: string) =
    task {
        use handler = new FakeHandler(fun _ -> ok profile)
        let! result = (registryOver handler).TryFindAsync("00000006", gb, ct)
        return companyIn (expectEvidence result)
    }

[<Fact>]
let ``overdue filings, disputed and undeliverable offices and insolvency all reach the contract, standing unchanged``
    ()
    : Task =
    task {
        let! company =
            concernsOf
                """{"company_number":"00000006","company_name":"A","company_status":"active","has_insolvency_history":true,"has_been_liquidated":false,
                    "registered_office_is_in_dispute":true,"undeliverable_registered_office_address":true,
                    "accounts":{"overdue":true,"next_accounts":{"overdue":true}},"confirmation_statement":{"overdue":true}}"""

        Assert.Equal(CompanyStanding.Active, company.Standing)
        Assert.Equal<CompanyConcern list>(CompanyConcern.all, company.Concerns.Stated)
        Assert.Empty company.Concerns.Unconfirmed
        Assert.False(nothingAgainst company)
    }

[<Fact>]
let ``a profile silent on its flags leaves them unconfirmed, and isn't good`` () : Task =
    task {
        let! company =
            concernsOf """{"company_number":"00000006","company_name":"A","company_status":"active"}"""

        Assert.Equal(CompanyStanding.Active, company.Standing)
        Assert.Empty company.Concerns.Stated
        Assert.Equal<CompanyConcern list>(CompanyConcern.all, company.Concerns.Unconfirmed)
        Assert.False(nothingAgainst company)
    }

[<Fact>]
let ``a profile silent on has_insolvency_history leaves insolvency unconfirmed`` () : Task =
    task {
        let profile = profileJson.Replace("\"has_insolvency_history\":false,", "")
        Assert.NotEqual<string>(profileJson, profile)
        let! company = concernsOf profile
        Assert.Equal<CompanyConcern list>([ CompanyConcern.InsolvencyHistory ], company.Concerns.Unconfirmed)
        Assert.False(nothingAgainst company)
    }

[<Fact>]
let ``a profile without the deprecated has_been_liquidated is settled by has_insolvency_history`` () : Task =
    task {
        let profile = profileJson.Replace("\"has_been_liquidated\":false,", "")
        Assert.NotEqual<string>(profileJson, profile)
        let! company = concernsOf profile
        Assert.Empty(company.Concerns.Unconfirmed)
        Assert.True(nothingAgainst company)
    }

[<Fact>]
let ``a profile answering every flag no is good`` () : Task =
    task {
        let! company = concernsOf profileJson
        Assert.True(nothingAgainst company)
    }

[<Fact>]
let ``coverage shows a gap before any call`` () =
    let fake = FakeRegistry() :> ICompanyRegistry
    use handler = new FakeHandler(fun _ -> failwith "Coverage must not call out")
    let uk = registryOver handler

    match fake.Coverage.Owners, uk.Coverage.Owners with
    | Support.NotOffered _, Support.Partial _ -> ()
    | other -> failwith $"%A{other}"

    Assert.Empty handler.Calls
    Assert.Equal("Companies House", uk.Coverage.Registry)
    Assert.Equal<Jurisdiction list>([ gb ], uk.Coverage.Jurisdictions)

[<Fact>]
let ``an ISO subdivision falls under its country, and not the other way round`` () =
    Assert.True(Jurisdiction.covers gb (Jurisdiction.ofCode "GB-SCT"))
    Assert.False(Jurisdiction.covers (Jurisdiction.ofCode "GB-SCT") gb)
    Assert.False(Jurisdiction.covers gb (Jurisdiction.ofCode "GI"))

[<Theory; InlineData("gb", "GB"); InlineData("us-de", "US-DE")>]
let ``a jurisdiction reads in upper case`` (code: string, expected: string) =
    Assert.Equal(expected, (Jurisdiction.ofCode code).Code)

[<Theory>]
[<InlineData("")>]
[<InlineData("G")>]
[<InlineData("GBR")>]
[<InlineData("GB-")>]
[<InlineData("GB\n")>]
[<InlineData(null)>]
let ``a value that isn't an ISO 3166 shape is refused`` (code: string) =
    Assert.Equal(ValueNone, Jurisdiction.tryParse code)

let private ownersOf (pscs: string list) (statements: string list) =
    task {
        use handler = new FakeHandler(companiesHouse pscs statements)
        let! result = (registryOver handler).OwnersAsync("00000006", gb, purpose, ct)
        return (expectEvidence result).Value
    }

[<Fact>]
let ``no PSC and no statement is not available, never an empty list`` () : Task =
    task {
        match! ownersOf [] [] with
        | NotAvailable _ -> ()
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a 404 on both PSC lists is not available, never an empty list`` () : Task =
    task {
        use handler =
            new FakeHandler(fun call ->
                if call.Target.Contains "persons-with" then
                    notFound
                else
                    ok profileJson)

        let! result = (registryOver handler).OwnersAsync("00000006", gb, purpose, ct)

        match (expectEvidence result).Value with
        | NotAvailable _ -> ()
        | other -> failwith $"%A{other}"
    }

[<Theory>]
[<InlineData("no-individual-or-entity-with-signficant-control")>]
[<InlineData("no-individual-or-entity-with-signficant-control-partnership")>]
let ``a statement that there is no one to list is an available, empty listing`` (code: string) : Task =
    task {
        match! ownersOf [] [ statementJson code ] with
        | Available listing ->
            Assert.Empty listing.Owners
            Assert.Equal<string list>([ code ], listing.Statements |> List.map _.AsPublished)
            Assert.True(OwnerListing.isComplete listing)
        | other -> failwith $"%A{other}"
    }

[<Theory>]
[<InlineData("psc-exists-but-not-identified")>]
[<InlineData("steps-to-find-psc-not-yet-completed")>]
[<InlineData("psc-details-not-confirmed")>]
[<InlineData("no-beneficial-owner-identified")>]
[<InlineData("a-statement-added-later")>]
let ``any other statement with no owners listed is not available, naming the statement`` (code: string) : Task =
    task {
        match! ownersOf [] [ statementJson code ] with
        | NotAvailable reason -> Assert.Contains(code, reason)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a statement since withdrawn doesn't count`` () : Task =
    task {
        let withdrawn =
            """{"statement":"no-individual-or-entity-with-signficant-control","ceased_on":"2020-01-01"}"""

        match! ownersOf [] [ withdrawn ] with
        | NotAvailable _ -> ()
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``only former owners, with nothing said since, is not available`` () : Task =
    task {
        let former =
            """{"name":"Mr Former","kind":"individual-person-with-significant-control","ceased_on":"2020-01-01","natures_of_control":[]}"""

        match! ownersOf [ former ] [] with
        | NotAvailable _ -> ()
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a ceased flag without a date ends an owner too`` () : Task =
    task {
        let former =
            """{"name":"Mr Former","kind":"individual-person-with-significant-control","ceased":true,"natures_of_control":[]}"""

        match! ownersOf [ former ] [] with
        | NotAvailable _ -> ()
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``an owner is read in neutral terms, with its natures of control`` () : Task =
    task {
        match! ownersOf [ pscJson ] [ statementJson "psc-details-not-confirmed" ] with
        | Available listing ->
            Assert.Equal<string list>([ "psc-details-not-confirmed" ], listing.Statements |> List.map _.AsPublished)
            Assert.False(OwnerListing.isComplete listing)
            let owner = listing.Owners.Head
            Assert.Equal(ValueSome "Ms Jane Doe", owner.Name)
            Assert.Equal(PartyKind.Individual, owner.Kind)
            Assert.True owner.Current
            Assert.Equal(ValueSome { Year = 1970; Month = 3 }, owner.BornIn)

            let expected =
                [
                    ControlKind.Ownership, ValueSome { From = 75; To = 100 }, "ownership-of-shares-75-to-100-percent"
                    ControlKind.VotingRights,
                    ValueSome { From = 25; To = 100 },
                    "voting-rights-more-than-25-percent-registered-overseas-entity"
                    ControlKind.AppointsBoard, ValueNone, "right-to-appoint-and-remove-directors"
                    ControlKind.SignificantInfluence, ValueNone, "significant-influence-or-control"
                    ControlKind.Other, ValueNone, "something-new"
                ]

            Assert.Equal<(ControlKind * PercentBand voption * string) list>(
                expected,
                owner.Control
                |> List.map (fun nature -> nature.Kind, nature.Band, nature.AsPublished)
            )
        | other -> failwith $"%A{other}"
    }

[<Theory>]
[<InlineData("corporate-entity-person-with-significant-control", "LegalEntity")>]
[<InlineData("legal-person-person-with-significant-control", "LegalEntity")>]
[<InlineData("individual-beneficial-owner", "Individual")>]
[<InlineData("super-secure-person-with-significant-control", "Withheld")>]
[<InlineData("something-new", "Unknown")>]
let ``an owner's kind is read, an unknown one staying unknown`` (kind: string, expected: string) : Task =
    task {
        match! ownersOf [ $$"""{"kind":"{{kind}}","natures_of_control":[]}""" ] [] with
        | Available listing ->
            let owner = listing.Owners.Head
            Assert.Contains(expected, $"%A{owner.Kind}")
            Assert.Equal(ValueNone, owner.Name)
            Assert.False(OwnerListing.isComplete listing)
        | other -> failwith $"%A{other}"
    }

let private gapsOf (pscs: string list) (statements: string list) =
    task {
        match! ownersOf pscs statements with
        | Available listing -> return listing.Gaps
        | other -> return failwith $"%A{other}"
    }

[<Fact>]
let ``an identified individual and no qualifying statement is complete`` () : Task =
    task {
        let! gaps = gapsOf [ pscJson ] []
        Assert.Empty gaps
    }

[<Theory>]
[<InlineData("psc-exists-but-not-identified", "OwnerNotIdentified")>]
[<InlineData("psc-exists-but-not-identified-partnership", "OwnerNotIdentified")>]
[<InlineData("no-beneficial-owner-identified", "OwnerNotIdentified")>]
[<InlineData("at-least-one-beneficial-owner-unidentified", "OwnerNotIdentified")>]
[<InlineData("at-least-one-beneficial-owner-unidentified-and-information-not-provided-for-at-least-one-beneficial-owner",
             "OwnerNotIdentified")>]
[<InlineData("psc-details-not-confirmed", "OwnerDetailsNotConfirmed")>]
[<InlineData("psc-details-not-confirmed-partnership", "OwnerDetailsNotConfirmed")>]
[<InlineData("information-not-provided-for-at-least-one-beneficial-owner", "OwnerDetailsNotConfirmed")>]
[<InlineData("awaiting-confirmation-from-psc", "OwnerDetailsNotConfirmed")>]
[<InlineData("steps-to-find-psc-not-yet-completed", "InquiriesIncomplete")>]
[<InlineData("steps-to-find-psc-not-yet-completed-partnership", "InquiriesIncomplete")>]
[<InlineData("psc-contacted-but-no-response", "OwnerNotCooperating")>]
[<InlineData("psc-contacted-but-no-response-partnership", "OwnerNotCooperating")>]
[<InlineData("psc-has-failed-to-confirm-changed-details", "OwnerNotCooperating")>]
[<InlineData("psc-has-failed-to-confirm-changed-details-partnership", "OwnerNotCooperating")>]
[<InlineData("restrictions-notice-issued-to-psc", "OwnerNotCooperating")>]
[<InlineData("restrictions-notice-issued-to-psc-partnership", "OwnerNotCooperating")>]
let ``a statement leaving owners open can't read as complete, with an owner listed``
    (code: string, gap: string)
    : Task =
    task {
        match! ownersOf [ pscJson ] [ statementJson code ] with
        | Available listing ->
            Assert.False(OwnerListing.isComplete listing)
            Assert.Equal(gap, $"%A{listing.Gaps.Head}")
            Assert.Equal(1, listing.Gaps.Length)
            Assert.Equal(code, listing.Statements.Head.AsPublished)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a statement not recognised leaves a gap naming it`` () : Task =
    task {
        let! gaps = gapsOf [ pscJson ] [ statementJson "a-statement-added-later" ]
        Assert.Equal<OwnershipGap list>([ OwnershipGap.StatementNotUnderstood "a-statement-added-later" ], gaps)
    }

[<Theory>]
[<InlineData("all-beneficial-owners-identified", "AllOwnersIdentified")>]
[<InlineData("nobody-has-become-or-ceased-to-be-a-beneficial-owner", "ChangesOnly")>]
[<InlineData("somebody-has-become-or-ceased-to-be-a-beneficial-owner", "ChangesOnly")>]
[<InlineData("no-change-beneficial-owner-relevant-period", "ChangesOnly")>]
[<InlineData("change-beneficial-owner-relevant-period", "ChangesOnly")>]
[<InlineData("no-trust-involved-relevant-period", "ChangesOnly")>]
[<InlineData("trust-involved-relevant-period", "ChangesOnly")>]
[<InlineData("no-change-beneficiary-relevant-period", "ChangesOnly")>]
[<InlineData("change-beneficiary-relevant-period", "ChangesOnly")>]
let ``a statement that leaves the list to speak for itself is no gap`` (code: string, meaning: string) : Task =
    task {
        match! ownersOf [ pscJson ] [ statementJson code ] with
        | Available listing ->
            Assert.Empty listing.Gaps
            Assert.Equal(meaning, $"%A{listing.Statements.Head.Meaning}")
        | other -> failwith $"%A{other}"
    }

[<Theory>]
[<InlineData("all-beneficial-owners-identified")>]
[<InlineData("nobody-has-become-or-ceased-to-be-a-beneficial-owner")>]
let ``such a statement with no owner listed still isn't available`` (code: string) : Task =
    task {
        match! ownersOf [] [ statementJson code ] with
        | NotAvailable _ -> ()
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a statement since withdrawn leaves no gap`` () : Task =
    task {
        let withdrawn =
            """{"statement":"psc-exists-but-not-identified","ceased_on":"2020-01-01"}"""

        let! gaps = gapsOf [ pscJson ] [ withdrawn ]
        Assert.Empty gaps
    }

[<Fact>]
let ``a statement that there's no one, against a current owner, is a contradiction`` () : Task =
    task {
        let! gaps =
            gapsOf [ pscJson ] [ statementJson "no-individual-or-entity-with-signficant-control" ]

        Assert.Equal<OwnershipGap list>([ OwnershipGap.StatementContradictsOwners ], gaps)
    }

[<Fact>]
let ``a statement that there's no one, with only former owners, is complete`` () : Task =
    task {
        let former =
            """{"name":"Mr Former","kind":"individual-person-with-significant-control","ceased_on":"2020-01-01","natures_of_control":[]}"""

        let! gaps =
            gapsOf [ former ] [ statementJson "no-individual-or-entity-with-signficant-control" ]

        Assert.Empty gaps
    }

[<Fact>]
let ``a former owner, withheld or unreadable, leaves no gap of its own`` () : Task =
    task {
        let former =
            """{"kind":"super-secure-person-with-significant-control","ceased":true}"""

        let! gaps = gapsOf [ pscJson; former ] []
        Assert.Empty gaps
    }

[<Theory>]
[<InlineData("""{"kind":"super-secure-person-with-significant-control"}""", "OwnerWithheld")>]
[<InlineData("""{"kind":"something-new","name":"A"}""", "OwnerUnreadable")>]
[<InlineData("""{"name":"A"}""", "OwnerUnreadable")>]
[<InlineData("""{"kind":"individual-person-with-significant-control"}""", "OwnerUnreadable")>]
[<InlineData("""{"kind":"corporate-entity-person-with-significant-control","name":"A"}""", "EntityNotIdentified")>]
[<InlineData("""{"kind":"corporate-entity-person-with-significant-control","name":"A","identification":{"legal_form":"Ltd"}}""",
             "EntityNotIdentified")>]
[<InlineData("""{"kind":"legal-person-person-with-significant-control","name":"A","identification":{"legal_form":"Body"}}""",
             "EntityNotIdentified")>]
let ``a current owner who can't be taken as identified leaves a gap`` (item: string, gap: string) : Task =
    task {
        let! gaps = gapsOf [ item ] []
        Assert.Equal<string list>([ gap ], gaps |> List.map (fun gap -> $"%A{gap}"))
    }

[<Fact>]
let ``a corporate owner's registration reaches the contract, enough to follow it`` () : Task =
    task {
        let item =
            """{"kind":"corporate-entity-person-with-significant-control","name":"HOLDINGS BV","natures_of_control":[],
                "identification":{"legal_authority":"Dutch law","legal_form":"BV","place_registered":"Kamer van Koophandel","registration_number":"12345678","country_registered":"Netherlands"}}"""

        match! ownersOf [ item ] [] with
        | Available listing ->
            Assert.True(OwnerListing.isComplete listing)

            Assert.Equal(
                ValueSome
                    {
                        LegalName = ValueSome "HOLDINGS BV"
                        LegalForm = ValueSome "BV"
                        GoverningLaw = ValueSome "Dutch law"
                        Country = ValueSome "Netherlands"
                        Register = ValueSome "Kamer van Koophandel"
                        RegistrationNumber = ValueSome "12345678"
                    },
                listing.Owners.Head.Entity
            )
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a PSC item with nothing in it fails the answer, instead of hiding that owners aren't available`` () : Task =
    task {
        use handler =
            new FakeHandler(fun call ->
                if call.Target.Contains "statements" then
                    ok (pscsJson 0 0 [])
                else
                    ok (pscsJson 1 0 [ "{}" ]))

        match! (registryOver handler).OwnersAsync("00000006", gb, purpose, ct) with
        | Error(RegistryFailure.UnreadableAnswer _) -> ()
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a verification block reaches the contract on officers and owners`` () : Task =
    task {
        let block =
            """"identity_verification_details":{"identity_verified_on":"2025-12-25"}"""

        let verified = ValueSome(DateOnly(2025, 12, 25))

        use officers =
            new FakeHandler(
                withProfile (fun call ->
                    ok (
                        officersJson
                            1
                            0
                            (if startIndexOf call = 0 then
                                 [ $$"""{"name":"A","officer_role":"director",{{block}}}""" ]
                             else
                                 [])
                    ))
            )

        let! read = (registryOver officers).OfficersAsync("00000006", gb, ct)

        match (expectEvidence read).Value with
        | Available [ officer ] -> Assert.Equal(verified, officer.IdentityVerification.Value.VerifiedOn)
        | other -> failwith $"%A{other}"

        let psc =
            $$"""{"name":"A","kind":"individual-person-with-significant-control",{{block}}}"""

        match! ownersOf [ psc ] [] with
        | Available listing -> Assert.Equal(verified, listing.Owners.Head.IdentityVerification.Value.VerifiedOn)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a corporate officer's registration reaches the contract`` () : Task =
    task {
        let item =
            """{"name":"A FIRM LTD","officer_role":"corporate-director","identification":{"registration_number":"123","place_registered":"A Register"}}"""

        use handler =
            new FakeHandler(
                withProfile (fun call -> ok (officersJson 1 0 (if startIndexOf call = 0 then [ item ] else [])))
            )

        let! read = (registryOver handler).OfficersAsync("00000006", gb, ct)

        match (expectEvidence read).Value with
        | Available [ officer ] ->
            Assert.Equal(ValueSome "123", officer.Entity.Value.RegistrationNumber)
            Assert.Equal(ValueSome "A Register", officer.Entity.Value.Register)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a failure reading statements fails the answer, rather than dropping them`` () : Task =
    task {
        use handler =
            new FakeHandler(fun call ->
                if call.Target.Contains "statements" then
                    answer 500 ""
                else
                    ok (pscListAt call [ pscJson ]))

        match! (registryOver handler).OwnersAsync("00000006", gb, purpose, ct) with
        | Error(RegistryFailure.Unavailable _) -> ()
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a company reads in neutral terms`` () : Task =
    task {
        use handler = new FakeHandler(companiesHouse [] [])
        let! result = (registryOver handler).TryFindAsync("00000006", gb, ct)
        let company = companyIn (expectEvidence result)
        Assert.Equal("MARINE AND GENERAL", company.Name)
        Assert.Equal(gb, company.Jurisdiction)
        Assert.Equal(ValueSome "england-wales", company.JurisdictionAsPublished)
        Assert.Equal(CompanyStanding.Active, company.Standing)
        Assert.Equal(ValueSome "ltd", company.LegalForm)

        match company.Addresses with
        | Available [ {
                          Kind = AddressKind.RegisteredOffice
                          Address = address
                      } ] -> Assert.Equal(ValueSome "AB1 2CD", address.PostalCode)
        | other -> failwith $"%A{other}"
    }

[<Theory>]
[<InlineData("scotland", "GB-SCT")>]
[<InlineData("northern-ireland", "GB-NIR")>]
[<InlineData("wales", "GB-WLS")>]
[<InlineData("england", "GB-ENG")>]
[<InlineData("england-wales", "GB")>]
[<InlineData("noneu", "GB")>]
let ``a company's jurisdiction is ISO 3166`` (published: string, expected: string) : Task =
    task {
        let body =
            $$"""{"company_number":"00000006","company_name":"A","jurisdiction":"{{published}}"}"""

        use handler = new FakeHandler(fun _ -> ok body)
        let! result = (registryOver handler).TryFindAsync("00000006", gb, ct)
        Assert.Equal(expected, (companyIn (expectEvidence result)).Jurisdiction.Code)
    }

[<Fact>]
let ``a profile with no address says its addresses aren't available`` () : Task =
    task {
        use handler =
            new FakeHandler(fun _ -> ok """{"company_number":"00000006","company_name":"A"}""")

        let! result = (registryOver handler).TryFindAsync("00000006", gb, ct)

        match (companyIn (expectEvidence result)).Addresses with
        | NotAvailable _ -> ()
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``officers read in neutral terms, current and former`` () : Task =
    task {
        use handler = new FakeHandler(companiesHouse [] [])
        let! result = (registryOver handler).OfficersAsync("00000006", gb, ct)

        match (expectEvidence result).Value with
        | Available [ director; secretary ] ->
            Assert.Equal(ValueNone, director.Entity)
            Assert.Equal(ValueNone, director.IdentityVerification)
            Assert.Equal(OfficerRole.Director, director.Role)
            Assert.Equal(PartyKind.Individual, director.Kind)
            Assert.True director.Current
            Assert.Equal(OfficerRole.Secretary, secretary.Role)
            Assert.Equal(PartyKind.LegalEntity, secretary.Kind)
            Assert.False secretary.Current
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``no officer list for a number is not available`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> notFound)
        let! result = (registryOver handler).OfficersAsync("00000006", gb, ct)

        match (expectEvidence result).Value with
        | NotAvailable _ -> ()
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a search returns its matches and the register's total`` () : Task =
    task {
        use handler = new FakeHandler(companiesHouse [] [])
        let! result = (registryOver handler).SearchAsync("marine", gb, ct)

        match (expectEvidence result).Value with
        | Available search ->
            Assert.Equal(ValueSome 44, search.TotalMatches)
            Assert.Equal("00000006", search.Matches.Head.Number)
            Assert.False search.Complete
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a search holding every match is complete, and an empty one with a total of zero too`` () : Task =
    task {
        let one = """{"company_number":"00000006","title":"MARINE AND GENERAL"}"""

        for total, items in [ 1, [ one ]; 0, [] ] do
            use handler = new FakeHandler(fun _ -> ok (listJson "total_results" total items))
            let! result = (registryOver handler).SearchAsync("marine", gb, ct)

            match (expectEvidence result).Value with
            | Available search -> Assert.True search.Complete
            | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a search with no total fails rather than reading as no matches`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok """{"items":[]}""")

        match! (registryOver handler).SearchAsync("marine", gb, ct) with
        | Error(RegistryFailure.UnreadableAnswer _) -> ()
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a list with no total, through the contract, fails rather than reading as none`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok """{"items":[]}""")

        match! (registryOver handler).OfficersAsync("00000006", gb, ct) with
        | Error(RegistryFailure.UnreadableAnswer _) -> ()
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``an understated total, through the contract, is a failure and not part of the list`` () : Task =
    task {
        let officer (n: int) =
            $$"""{"name":"OFFICER {{n}}","officer_role":"director"}"""

        use handler =
            new FakeHandler(fun call ->
                match startIndexOf call with
                | 0 -> ok (officersJson 2 0 [ officer 0; officer 1 ])
                | _ -> ok (officersJson 2 0 [ officer 2; officer 3; officer 4 ]))

        match! (registryOver handler).OfficersAsync("00000006", gb, ct) with
        | Error(RegistryFailure.Unavailable _) -> ()
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a list past the page cap, through the contract, isn't one to retry`` () : Task =
    task {
        let officer (n: int) =
            $$"""{"name":"OFFICER {{n}}","officer_role":"director"}"""

        use handler =
            new FakeHandler(fun call -> ok (officersJson 100000000 0 [ officer (startIndexOf call) ]))

        match! (registryOver handler).OfficersAsync("00000006", gb, ct) with
        | Error(RegistryFailure.UnreadableAnswer _ as failure) -> Assert.False(RegistryFailure.isTemporary failure)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a search's 404 isn't read as no matches`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> notFound)
        let! result = (registryOver handler).SearchAsync("marine", gb, ct)

        match (expectEvidence result).Value with
        | NotAvailable _ -> ()
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``Companies House failures map to neutral ones, holding no key or body`` () : Task =
    task {
        let cases: (int * string * (RegistryFailure -> bool)) list =
            [
                401,
                "secret-body",
                (function
                | RegistryFailure.AccessRefused _ -> true
                | _ -> false)
                429,
                "secret-body",
                (function
                | RegistryFailure.RateLimited(ValueSome _) -> true
                | _ -> false)
                500,
                "secret-body",
                (function
                | RegistryFailure.Unavailable _ -> true
                | _ -> false)
                200,
                "secret-body",
                (function
                | RegistryFailure.UnreadableAnswer _ -> true
                | _ -> false)
                400,
                "secret-body",
                (function
                | RegistryFailure.RequestRefused _ -> true
                | _ -> false)
                403,
                "secret-body",
                (function
                | RegistryFailure.RequestRefused _ -> true
                | _ -> false)
                416,
                "secret-body",
                (function
                | RegistryFailure.RequestRefused _ -> true
                | _ -> false)
                302,
                "secret-body",
                (function
                | RegistryFailure.RequestRefused _ -> true
                | _ -> false)
                503,
                "secret-body",
                (function
                | RegistryFailure.Unavailable _ -> true
                | _ -> false)
            ]

        for status, body, expected in cases do
            use handler = new FakeHandler(fun _ -> answer status body)

            match! (registryOver handler).TryFindAsync("00000006", gb, ct) with
            | Error failure ->
                Assert.True(expected failure, $"%A{failure}")
                let printed = $"%A{failure} {RegistryFailure.describe failure}"
                Assert.DoesNotContain("secret-body", printed)
                Assert.DoesNotContain(Key, printed)
            | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a neutral failure says to retry exactly when the client's own action does`` () =
    let failures =
        [
            InvalidRequest "r"
            KeyRefused "b"
            RateLimited RateLimit.none
            RedirectRefused(302, "r")
            ApiError(400, "b")
            ApiError(403, "b")
            ApiError(416, "b")
            ApiError(418, "b")
            ApiError(500, "b")
            ApiError(503, "b")
            IncompleteList(1, 2)
            RepeatedPage 1
            InconsistentList(1, "r")
            ListTooLong(100, 100, 200)
            TransportError(exn "e")
            InvalidResponse
                {
                    HttpStatus = 200
                    Body = "b"
                    Reason = "r"
                }
        ]

    for failure in failures do
        Assert.Equal(
            CompaniesHouseFailure.action failure = FailureAction.RetryLater,
            RegistryFailure.isTemporary (Neutral.failure fixedTime failure)
        )

[<Fact>]
let ``a bad number through the contract is InvalidRequest, not not-found`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> notFound)

        match! (registryOver handler).TryFindAsync("123456", gb, ct) with
        | Error(RegistryFailure.InvalidRequest _) -> Assert.Empty handler.Calls
        | other -> failwith $"%A{other}"
    }

let private filingItem (n: int) =
    $"""{{"transaction_id":"t{n}","category":"accounts","date":"2020-01-01"}}"""

/// Answers the officer list and filing history with empty lists, as Companies House does for a number it doesn't hold, and the profile with held.
let private emptyLists (held: bool) (call: Call) =
    if call.Target.Contains "/officers" then
        ok (officersJson 0 0 [])
    elif call.Target.Contains "/filing-history" then
        ok (listJson "total_count" 0 [])
    elif held then
        ok profileJson
    else
        notFound

[<Fact>]
let ``an empty officer list for a number Companies House doesn't hold is NotAvailable`` () : Task =
    task {
        use handler = new FakeHandler(emptyLists false)

        match! (registryOver handler).OfficersAsync("00000006", gb, ct) with
        | Ok { Value = NotAvailable _ } ->
            Assert.Contains(handler.Calls, fun call -> call.Target.EndsWith "/company/00000006")
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``an empty filing history for a number Companies House doesn't hold is NotAvailable`` () : Task =
    task {
        use handler = new FakeHandler(emptyLists false)

        let registry =
            CompaniesHouseRegistry(clientWith handler, fun () -> fixedTime) :> IFilingRegister

        match! registry.FilingsAsync("00000006", ct) with
        | Ok { Value = NotAvailable _ } -> ()
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``an empty officer list for a held company stays empty, and a company already found isn't asked for again``
    ()
    : Task =
    task {
        use handler = new FakeHandler(emptyLists true)
        let registry = registryOver handler

        match! registry.OfficersAsync("00000006", gb, ct) with
        | Ok { Value = Available [] } -> ()
        | other -> failwith $"%A{other}"

        let! _ = registry.OfficersAsync("00000006", gb, ct)

        Assert.Equal(
            1,
            handler.Calls
            |> List.filter (fun call -> call.Target.EndsWith "/company/00000006")
            |> List.length
        )
    }

[<Fact>]
let ``a long filing history reads the newest MostRead filings and says it stopped`` () : Task =
    task {
        let total = Api.FilingHistory.MostRead + 250

        use handler =
            new FakeHandler(
                withProfile (fun call ->
                    let start = startIndexOf call
                    let size = min 100 (total - start)
                    ok (listJson "total_count" total [ for n in start .. start + size - 1 -> filingItem n ]))
            )

        let registry =
            CompaniesHouseRegistry(clientWith handler, fun () -> fixedTime) :> IFilingRegister

        match! registry.FilingsAsync("00000006", ct) with
        | Ok { Value = Available history } ->
            Assert.Equal(Api.FilingHistory.MostRead, history.Filings.Length)
            Assert.Equal(ValueSome total, history.Total)
            Assert.False history.Complete
            // The pages of the history, and the profile it is read against.
            Assert.Equal(Api.FilingHistory.MostRead / Api.Paging.PageSize + 1, handler.Calls.Length)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a short filing history is read whole and Complete`` () : Task =
    task {
        use handler =
            new FakeHandler(
                withProfile (fun call ->
                    match startIndexOf call with
                    | 0 -> ok (listJson "total_count" 2 [ filingItem 0; filingItem 1 ])
                    | _ -> ok (listJson "total_count" 2 []))
            )

        let registry =
            CompaniesHouseRegistry(clientWith handler, fun () -> fixedTime) :> IFilingRegister

        match! registry.FilingsAsync("00000006", ct) with
        | Ok { Value = Available history } ->
            Assert.Equal(2, history.Filings.Length)
            Assert.True history.Complete
        | other -> failwith $"%A{other}"
    }

// An officer's kind

let private officersOf (profile: string) (items: string list) =
    task {
        use handler =
            new FakeHandler(
                withProfileOf profile (fun call ->
                    ok (officersJson items.Length 0 (if startIndexOf call = 0 then items else [])))
            )

        let! read = (registryOver handler).OfficersAsync("00000006", gb, ct)
        return (expectEvidence read).Value
    }

[<Theory>]
[<InlineData("""{"name":"A LTD","officer_role":"corporate-director"}""", "LegalEntity")>]
[<InlineData("""{"name":"A LTD","officer_role":"general-partner-in-a-limited-partnership","identification":{"registration_number":"123"}}""",
             "LegalEntity")>]
[<InlineData("""{"name":"A LTD","officer_role":"director","identification":{"place_registered":"A Register"}}""",
             "LegalEntity")>]
[<InlineData("""{"name":"A","officer_role":"director"}""", "Individual")>]
[<InlineData("""{"name":"A","officer_role":"llp-member"}""", "Individual")>]
[<InlineData("""{"name":"A","officer_role":"general-partner-in-a-limited-partnership","date_of_birth":{"month":3,"year":1970}}""",
             "Individual")>]
[<InlineData("""{"name":"A","officer_role":"general-partner-in-a-limited-partnership"}""", "Unknown")>]
[<InlineData("""{"name":"A","officer_role":"limited-partner-in-a-limited-partnership"}""", "Unknown")>]
[<InlineData("""{"name":"A","officer_role":"judicial-factor"}""", "Unknown")>]
[<InlineData("""{"name":"A","officer_role":"a-new-role"}""", "Unknown")>]
[<InlineData("""{"name":"A"}""", "Unknown")>]
let ``an officer is a legal entity by its role or registration, a person by a role that says so, and otherwise unknown``
    (item: string, expected: string)
    : Task =
    task {
        match! officersOf profileJson [ item ] with
        | Available [ officer ] -> Assert.StartsWith(expected, $"%A{officer.Kind}")
        | other -> failwith $"%A{other}"
    }

// What a profile says keeps its lists from being whole

/// The profile with one more field at its top level.
let private profileWith (extra: string) =
    let profile = profileJson.TrimEnd()
    profile.Substring(0, profile.Length - 1) + "," + extra + "}"

let private partialProfile =
    profileWith "\"partial_data_available\":\"full-data-available-from-financial-conduct-authority\""

[<Fact>]
let ``an empty officer list of a company Companies House isn't the primary source for is not available`` () : Task =
    task {
        match! officersOf partialProfile [] with
        | NotAvailable reason ->
            Assert.Contains("partial_data_available", reason)
            Assert.Contains("full-data-available-from-financial-conduct-authority", reason)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``officers of a company Companies House isn't the primary source for end with one not given`` () : Task =
    task {
        match! officersOf partialProfile [ """{"name":"DOE, Jane","officer_role":"director"}""" ] with
        | Available [ listed; elsewhere ] ->
            Assert.Equal("DOE, Jane", listed.Name)
            Assert.True(PartyNames.isPlaceholder elsewhere.Name)
            Assert.True elsewhere.Current

            match elsewhere.Role with
            | OfficerRole.Other words -> Assert.Contains("partial_data_available", words)
            | other -> failwith $"%A{other}"
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``each super secure managing officer the profile counts is an officer withheld`` () : Task =
    task {
        let profile = profileWith "\"super_secure_managing_officer_count\":2"

        match! officersOf profile [ """{"name":"A LTD","officer_role":"corporate-managing-officer"}""" ] with
        | Available [ listed; first; second ] ->
            Assert.Equal(PartyKind.LegalEntity, listed.Kind)

            for withheld in [ first; second ] do
                Assert.Equal(PartyKind.Withheld, withheld.Kind)
                Assert.True withheld.Current

                match withheld.Role with
                | OfficerRole.Other words -> Assert.Contains("super_secure_managing_officer_count", words)
                | other -> failwith $"%A{other}"
        | other -> failwith $"%A{other}"

        // With no officer listed, the withheld ones are still there.
        match! officersOf profile [] with
        | Available [ first; second ] ->
            Assert.Equal<PartyKind list>([ PartyKind.Withheld; PartyKind.Withheld ], [ first.Kind; second.Kind ])
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a profile without the markers leaves the officer list as it is`` () : Task =
    task {
        match!
            officersOf
                (profileWith "\"super_secure_managing_officer_count\":0")
                [ """{"name":"A","officer_role":"director"}""" ]
        with
        | Available [ officer ] -> Assert.Equal("A", officer.Name)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``owners of a company Companies House isn't the primary source for carry a gap naming the marker`` () : Task =
    task {
        use handler = new FakeHandler(companiesHouseWith partialProfile [ pscJson ] [])
        let! result = (registryOver handler).OwnersAsync("00000006", gb, purpose, ct)

        match (expectEvidence result).Value with
        | Available listing ->
            Assert.False(OwnerListing.isComplete listing)

            Assert.Contains(
                listing.Gaps,
                fun gap ->
                    match gap with
                    | OwnershipGap.StatementNotUnderstood words -> words.Contains "partial_data_available"
                    | _ -> false
            )
        | other -> failwith $"%A{other}"

        use whole = new FakeHandler(companiesHouse [ pscJson ] [])
        let! result = (registryOver whole).OwnersAsync("00000006", gb, purpose, ct)

        match (expectEvidence result).Value with
        | Available listing ->
            Assert.DoesNotContain(
                listing.Gaps,
                fun gap ->
                    match gap with
                    | OwnershipGap.StatementNotUnderstood words -> words.Contains "partial_data_available"
                    | _ -> false
            )
        | other -> failwith $"%A{other}"
    }

[<Theory>]
[<InlineData("", "", true)>]
[<InlineData("", "filing-history-available", true)>]
[<InlineData("", "filing-history-available-limited-partnership-from-2014", false)>]
[<InlineData("", "something-new", false)>]
[<InlineData("partial", "", false)>]
[<InlineData("partial", "filing-history-available", false)>]
let ``a filing history read to its end is Complete only when neither its status nor the profile says it isn't whole``
    (profile: string, status: string, complete: bool)
    : Task =
    task {
        let list (items: string list) =
            let body = String.concat "," items

            let stated =
                if status = "" then
                    ""
                else
                    $""","filing_history_status":"{status}" """

            $"""{{"total_count":2,"items_per_page":{items.Length},"start_index":0,"items":[{body}]{stated}}}"""

        use handler =
            new FakeHandler(
                withProfileOf (if profile = "" then profileJson else partialProfile) (fun call ->
                    match startIndexOf call with
                    | 0 -> ok (list [ filingItem 0; filingItem 1 ])
                    | _ -> ok (list []))
            )

        let registry =
            CompaniesHouseRegistry(clientWith handler, fun () -> fixedTime) :> IFilingRegister

        match! registry.FilingsAsync("00000006", ct) with
        | Ok { Value = Available history } ->
            Assert.Equal(2, history.Filings.Length)
            Assert.Equal(complete, history.Complete)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a list read after the profile asks for no profile again`` () : Task =
    task {
        use handler = new FakeHandler(companiesHouse [] [])
        let registry = registryOver handler
        let! _ = registry.TryFindAsync("00000006", gb, ct)
        let! _ = registry.OfficersAsync("00000006", gb, ct)

        Assert.Equal(
            1,
            handler.Calls
            |> List.filter (fun call -> Uri(call.Uri).AbsolutePath.EndsWith "/company/00000006")
            |> List.length
        )
    }
