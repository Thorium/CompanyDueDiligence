/// Following links and reading registers' answers into a case: GLEIF in every outcome, several registers for one place, coverage caveats, data
/// dates, control and unnamed officers. Names and numbers are made up.
module InvestigationFixTests

open System
open System.Threading.Tasks
open Xunit
open CompanyDueDiligence
open Fake

let private at = DateTimeOffset(2026, 11, 6, 12, 0, 0, TimeSpan.Zero)
let private zz = Jurisdiction.ofCode "ZZ"

let private options =
    { InvestigationOptions.defaults with
        ListMinimums = ListMinimums.anyEntries
        Now = fun () -> at
    }

let private coverageOf name (scopes: string list) : RegistryCoverage =
    {
        Registry = name
        Jurisdictions = scopes |> List.map Jurisdiction.ofCode
        Standing = Support.Supported
        Concerns = Support.Supported
        Addresses = Support.Supported
        Search = Support.Supported
        Officers = Support.Supported
        Owners = Support.Supported
        Cost = AccessCost.Free
        RateLimit = RateLimitPolicy.NotPublished(1, TimeSpan.FromSeconds 1.)
        Licence = "None stated"
    }

let private company number name (jurisdiction: Jurisdiction) : Company =
    {
        Name = name
        OtherNames = []
        Number = number
        Jurisdiction = jurisdiction
        JurisdictionAsPublished = ValueNone
        Standing = CompanyStanding.Active
        Concerns = { Stated = []; Unconfirmed = [] }
        StatusAsPublished = ValueSome "active"
        LegalForm = ValueNone
        IncorporatedOn = ValueNone
        CeasedOn = ValueNone
        Addresses = Available []
        Role = RecordRole.Home
        HomeRegistration = ValueNone
        EstablishmentOf = ValueNone
    }

let private noOwners: OwnerListing =
    {
        Owners = []
        Statements = []
        Gaps = []
    }

/// What a scripted register answers, and with what dates.
type private Script =
    {
        Coverage: RegistryCoverage
        AsOf: DateOnly voption
        RetrievedAt: DateTimeOffset
        Find: string -> Jurisdiction -> Result<Availability<Company voption>, RegistryFailure>
        Officers: Result<Availability<Officer list>, RegistryFailure>
        Owners: Result<Availability<OwnerListing>, RegistryFailure>
    }

let private script
    name
    scopes
    (find: string -> Jurisdiction -> Result<Availability<Company voption>, RegistryFailure>)
    =
    {
        Coverage = coverageOf name scopes
        AsOf = ValueNone
        RetrievedAt = at
        Find = find
        Officers = Ok(Available [])
        Owners = Ok(Available noOwners)
    }

/// A register holding one company under each number given.
let private holding name scopes (companies: Company list) =
    script name scopes (fun number _ ->
        match companies |> List.tryFind (fun held -> held.Number = number) with
        | Some held -> Ok(Available(ValueSome held))
        | None -> Ok(Available ValueNone))

type private Scripted(script: Script) =
    let answer value : Evidence<_> =
        {
            Source = script.Coverage.Registry
            RetrievedAt = script.RetrievedAt
            AsOf = script.AsOf
            Raw = ValueNone
            Value = value
        }

    interface ICompanyRegistry with
        member _.Coverage = script.Coverage

        member _.TryFindAsync(number, jurisdiction, _) =
            Task.FromResult(script.Find number jurisdiction |> Result.map answer)

        member _.SearchAsync(_, _, _) =
            Task.FromResult(Ok(answer (NotAvailable "Not scripted")))

        member _.OfficersAsync(_, _, _) =
            Task.FromResult(script.Officers |> Result.map answer)

        member _.OwnersAsync(_, _, _, _) =
            Task.FromResult(script.Owners |> Result.map answer)

let private registry script = Scripted script :> ICompanyRegistry

let private sourcesWith registries : CaseSources =
    {
        Registries = registries
        Filings = []
        CrossCheck = ValueNone
        Routes = CoverageRoutes.all
        KnownAddresses = []
    }

let private openWith sources number (jurisdiction: Jurisdiction) =
    Investigation.openAsync
        sources
        options
        {
            CaseId = "CASE"
            Number = number
            Jurisdiction = jurisdiction
            NameGiven = "As given"
        }
        ct

let private openOn registries number jurisdiction =
    openWith (sourcesWith registries) number jurisdiction

let private gapsOn (subject: string) (case: CaseFile) =
    case.Gaps |> List.filter (fun gap -> gap.Subject = subject)

// GLEIF in every outcome.

[<Literal>]
let private lei1 = "AAAAAAAAAAAAAAAAAA11"

[<Literal>]
let private lei2 = "BBBBBBBBBBBBBBBBBB22"

let private leiRecord (lei: string) (name: string) (number: string) (jurisdiction: string) =
    """{"type":"lei-records","id":"%LEI%","attributes":{"lei":"%LEI%","entity":{"legalName":{"name":"%NAME%"},"registeredAs":"%NUMBER%","jurisdiction":"%JURISDICTION%","status":"ACTIVE"},"registration":{"status":"ISSUED"}}}"""
        .Replace("%LEI%", lei)
        .Replace("%NAME%", name)
        .Replace("%NUMBER%", number)
        .Replace("%JURISDICTION%", jurisdiction)

/// GLEIF holding SUB SARL (B1, LU), whose direct parent is TOP SA (B2, LU).
let private gleifHandler () =
    new FakeHandler(fun call ->
        let target = Uri.UnescapeDataString call.Target
        let meta = "\"meta\":{\"goldenCopy\":{\"publishDate\":\"2026-11-06T08:00:00Z\"}}"

        if target.Contains "filter[entity.registeredAs]=" then
            let number = target.Substring(target.IndexOf('=') + 1).Split('&')[0]

            let items =
                if number = "B1" then
                    leiRecord lei1 "SUB SARL" "B1" "LU"
                else
                    ""

            ok ("{" + meta + ",\"data\":[" + items + "]}")
        elif target.EndsWith(lei1 + "/direct-parent") then
            ok ("{" + meta + ",\"data\":" + leiRecord lei2 "TOP SA" "B2" "LU" + "}")
        else
            notFound)

let private withGleif (handler: FakeHandler) registries =
    { sourcesWith registries with
        CrossCheck =
            ValueSome(Sources.gleif (SourceConnection(handler, (fun () -> at), (fun _ _ -> Task.CompletedTask))))
    }

let private lu = Jurisdiction.ofCode "LU"

let private hasLei (case: CaseFile) =
    let subject = case.Entities |> List.find (fun entity -> entity.Key = case.Subject)
    Assert.Contains(EntityIdentity.Lei lei1, subject.Identities)
    Assert.Contains(case.Evidence, (fun item -> item.About = case.Subject && item.Origin = EvidenceOrigin.CrossCheck))
    let parent = case.Links |> List.find (fun link -> link.Kind = LinkKind.Parent)
    Assert.Equal("LU:B2", parent.To)

[<Fact>]
let ``an entity with only a route still gets its LEI record and parents`` () : Task =
    task {
        use handler = gleifHandler ()
        let! case = openWith (withGleif handler []) "B1" lu
        hasLei case

        Assert.Contains(
            case.Gaps,
            (fun gap -> gap.About = ValueSome case.Subject && gap.Kind = GapKind.NeedsPaidContract)
        )
        // The parent is followed into its own route gap and its own LEI lookup.
        Assert.Contains(case.Gaps, (fun gap -> gap.About = ValueSome "LU:B2" && gap.Kind = GapKind.NeedsPaidContract))
    }

[<Theory>]
[<InlineData("not available")>]
[<InlineData("not held")>]
[<InlineData("failure")>]
[<InlineData("outside coverage")>]
let ``a register that doesn't give the record still leaves the LEI read`` (outcome: string) : Task =
    task {
        let answer =
            match outcome with
            | "not available" -> Ok(NotAvailable "Left out")
            | "not held" -> Ok(Available ValueNone)
            | "failure" -> Error(RegistryFailure.Unavailable "x")
            | _ -> Error(RegistryFailure.OutsideCoverage lu)

        use handler = gleifHandler ()

        let! case =
            openWith (withGleif handler [ registry (script "Lux" [ "LU" ] (fun _ _ -> answer)) ]) "B1" lu

        hasLei case
    }

// Several registers for one place.

[<Fact>]
let ``the registry naming the exact code is preferred over one covering the country`` () =
    let country = registry (holding "Federal" [ "CA" ] [])
    let province = registry (holding "Provincial" [ "CA-BC" ] [])

    let pick code =
        (Registries.forJurisdiction [ country; province ] (Jurisdiction.ofCode code)).Value.Coverage.Registry

    Assert.Equal("Provincial", pick "CA-BC")
    Assert.Equal("Federal", pick "CA")
    Assert.Equal("Federal", pick "CA-ON")

[<Fact>]
let ``a country register listed first doesn't hide the subdivision's own`` () : Task =
    task {
        let bc = Jurisdiction.ofCode "CA-BC"

        let federal =
            registry (script "Federal" [ "CA" ] (fun _ asked -> Error(RegistryFailure.OutsideCoverage asked)))

        let province =
            registry (holding "Provincial" [ "CA-BC" ] [ company "BC1" "WEST CO" bc ])

        let! case = openOn [ federal; province ] "BC1" bc
        Assert.Equal(ValueSome CompanyStanding.Active, case.Entities.Head.Standing)
        Assert.DoesNotContain(case.Gaps, (fun gap -> gap.Kind = GapKind.SourceFailed))
    }

[<Theory; InlineData("outside coverage"); InlineData("not available")>]
let ``the next register covering the place is read when one can't give the record`` (outcome: string) : Task =
    task {
        let first =
            registry (
                script "First" [ "ZZ" ] (fun _ asked ->
                    match outcome with
                    | "outside coverage" -> Error(RegistryFailure.OutsideCoverage asked)
                    | _ -> Ok(NotAvailable "This file leaves the company out"))
            )

        let second = registry (holding "Second" [ "ZZ" ] [ company "1" "ZEDCO" zz ])
        let! case = openOn [ first; second ] "1" zz
        Assert.Equal("ZEDCO", case.Entities.Head.Name)
        Assert.Equal(ValueSome CompanyStanding.Active, case.Entities.Head.Standing)
        Assert.Empty(case.Gaps |> List.filter (fun gap -> gap.Subject.StartsWith "company record"))

        if outcome = "not available" then
            Assert.Contains(case.Evidence, (fun item -> item.Source = "First" && item.Topic = "company record"))
    }

[<Fact>]
let ``each register's record is kept, so they are compared`` () : Task =
    task {
        let first =
            registry (holding "Business register" [ "ZZ" ] [ company "1" "ZEDCO" zz ])

        let dissolved =
            { company "1" "ZEDCO" zz with
                Standing = CompanyStanding.Dissolved
                StatusAsPublished = ValueSome "dissolved"
            }

        // The tax register holds the number asked, and gives its own number of another scheme.
        let second =
            registry (
                script "Tax register" [ "ZZ" ] (fun _ _ ->
                    Ok(
                        Available(
                            ValueSome
                                { dissolved with
                                    Number = "51000000001"
                                }
                        )
                    ))
            )

        let! case = openOn [ first; second ] "1" zz

        let sources =
            case.Evidence
            |> List.filter (fun item -> item.Topic = "company record")
            |> List.map _.Source

        Assert.Equal<string list>([ "Business register"; "Tax register" ], sources)

        Assert.Contains(
            case.Flags,
            (fun flag -> flag.Kind = FlagKind.Disagreement && flag.Statement.Contains "dissolved")
        )

        Assert.DoesNotContain(
            case.Flags,
            (fun flag -> flag.Kind = FlagKind.Disagreement && flag.Statement.Contains "51000000001")
        )

        Assert.Contains(case.Flags, (fun flag -> flag.Kind = FlagKind.StatusConcern && flag.Source = "Tax register"))
        // The record followed is the first register's.
        Assert.Equal(ValueSome CompanyStanding.Active, case.Entities.Head.Standing)
    }

[<Fact>]
let ``a province code no register reads is no route, not a failure`` () : Task =
    task {
        let on = Jurisdiction.ofCode "CA-ON"

        let federal =
            registry (
                script "Federal" [ "CA" ] (fun _ asked ->
                    if asked.Code = "CA" then
                        Ok(Available ValueNone)
                    else
                        Error(RegistryFailure.OutsideCoverage asked))
            )

        let! case = openOn [ federal ] "1" on

        let gap =
            case.Gaps
            |> List.find (fun gap -> gap.Subject.StartsWith "The register in CA-ON")

        Assert.Equal(GapKind.NoRoute, gap.Kind)
        Assert.Contains("Federal", gap.Detail)
        Assert.DoesNotContain(case.Gaps, (fun gap -> gap.Kind = GapKind.SourceFailed))
    }

// Coverage caveats and standing.

[<Fact>]
let ``each partial part of a register's coverage is a gap with its caveat`` () : Task =
    task {
        let caveated =
            { holding
                  "Tax register"
                  [ "ZZ" ]
                  [
                      { company "1" "ZEDCO" zz with
                          Standing = CompanyStanding.Registered(ValueSome "registered")
                      }
                  ] with
                Coverage =
                    { coverageOf "Tax register" [ "ZZ" ] with
                        Standing = Support.Partial "Tax registration only"
                        Concerns = Support.Partial "Only an undeliverable address is stated"
                        Addresses = Support.Partial "The legal address only"
                    }
            }

        let! case = openOn [ registry caveated ] "1" zz

        let caveat subject (text: string) =
            Assert.Contains(
                gapsOn subject case,
                (fun gap -> gap.Detail.Contains text && gap.Reach = GapReach.ThisEntity)
            )

        caveat "standing" "Tax registration only"
        caveat "filings and disputes" "Only an undeliverable address"
        caveat "addresses" "The legal address only"
    }

[<Fact>]
let ``a partial concerns caveat isn't repeated when the unconfirmed concerns are already a gap`` () : Task =
    task {
        let unconfirmed =
            { company "1" "ZEDCO" zz with
                Concerns =
                    {
                        Stated = []
                        Unconfirmed = [ CompanyConcern.AccountsOverdue ]
                    }
            }

        let caveated =
            { holding "Register" [ "ZZ" ] [ unconfirmed ] with
                Coverage =
                    { coverageOf "Register" [ "ZZ" ] with
                        Concerns = Support.Partial "Only an overdue report is stated"
                    }
            }

        let! case = openOn [ registry caveated ] "1" zz

        Assert.DoesNotContain(
            gapsOn "filings and disputes" case,
            (fun gap -> gap.Detail.Contains "Only an overdue report")
        )

        Assert.NotEmpty(gapsOn "filings and disputes" case)
    }

[<Fact>]
let ``a register with no status word leaves standing unconfirmed`` () : Task =
    task {
        let held =
            registry (
                holding
                    "Formations"
                    [ "ZZ" ]
                    [
                        { company "1" "ZEDCO" zz with
                            Standing = CompanyStanding.Registered ValueNone
                        }
                    ]
            )

        let! case = openOn [ held ] "1" zz
        Assert.Empty case.Flags
        Assert.Contains(gapsOn "standing" case, (fun gap -> gap.Detail.Contains "no status word"))
    }

// Data dates.

[<Theory; InlineData("as of"); InlineData("retrieved")>]
let ``data older than the case date is a gap naming its date`` (dated: string) : Task =
    task {
        let base' = holding "Bulk file" [ "ZZ" ] [ company "1" "ZEDCO" zz ]

        let old =
            match dated with
            | "as of" ->
                { base' with
                    AsOf = ValueSome(DateOnly(2026, 10, 1))
                }
            | _ ->
                { base' with
                    RetrievedAt = DateTimeOffset(2026, 10, 1, 9, 0, 0, TimeSpan.Zero)
                }

        let! case = openOn [ registry old ] "1" zz
        let gap = gapsOn "data date" case |> List.exactlyOne
        Assert.Contains("as of 2026-10-01", gap.Detail)
        Assert.Equal(GapReach.ThisEntity, gap.Reach)

        let! current = openOn [ registry base' ] "1" zz
        Assert.Empty(gapsOn "data date" current)
    }

[<Fact>]
let ``not held in dated data says the date, and the company may be newer`` () : Task =
    task {
        let old =
            { holding "Bulk file" [ "ZZ" ] [] with
                AsOf = ValueSome(DateOnly(2026, 10, 1))
            }

        let! case = openOn [ registry old ] "404" zz
        let flag = case.Flags |> List.exactlyOne
        Assert.Equal(FlagKind.NotOnRegister, flag.Kind)
        Assert.Contains("2026-10-01", flag.Statement)
        Assert.Contains(gapsOn "data date" case, (fun gap -> gap.Detail.Contains "formed since"))
    }

// Owners' control.

let private corporateOwner (controls: NatureOfControl list) : Owner =
    {
        Name = ValueSome "PARENT LTD"
        Kind = PartyKind.LegalEntity
        Control = controls
        Current = true
        Since = ValueNone
        EndedOn = ValueNone
        BornIn = ValueNone
        Nationality = ValueNone
        CountryOfResidence = ValueNone
        Address = ValueNone
        Entity =
            ValueSome
                {
                    LegalName = ValueSome "PARENT LTD"
                    LegalForm = ValueNone
                    GoverningLaw = ValueNone
                    Country = ValueSome "Atlantis"
                    Register = ValueNone
                    RegistrationNumber = ValueSome "9"
                }
        IdentityVerification = ValueNone
    }

let private withOwners (owners: Owner list) (support: Support) =
    { holding "Zedland" [ "ZZ" ] [ company "1" "ZEDCO" zz ] with
        Owners = Ok(Available { noOwners with Owners = owners })
        Coverage =
            { coverageOf "Zedland" [ "ZZ" ] with
                Owners = support
            }
    }

[<Fact>]
let ``an owner's control and band are on its link`` () : Task =
    task {
        let control =
            {
                Kind = ControlKind.Ownership
                Band = ValueSome { From = 75; To = 100 }
                AsPublished = "ownership-of-shares-75-to-100-percent"
            }

        let! case =
            openOn [ registry (withOwners [ corporateOwner [ control ] ] Support.Supported) ] "1" zz

        let link = case.Links |> List.find (fun link -> link.Kind = LinkKind.Owner)
        Assert.Contains("ownership 75 to 100%", link.Note.Value)
        Assert.Contains("ownership-of-shares-75-to-100-percent", link.Note.Value)
        Assert.Empty(gapsOn "shareholdings" case)
    }

[<Fact>]
let ``current owners with no band at all are a gap, and a partial owners coverage is too`` () : Task =
    task {
        let control =
            {
                Kind = ControlKind.Ownership
                Band = ValueNone
                AsPublished = "partner"
            }

        let! case =
            openOn
                [
                    registry (withOwners [ corporateOwner [ control ] ] (Support.Partial "Legal owners only"))
                ]
                "1"
                zz

        Assert.Contains("no share or voting band", (gapsOn "shareholdings" case |> List.exactlyOne).Detail)
        Assert.Contains(gapsOn "owners" case, (fun gap -> gap.Detail.Contains "Legal owners only"))
        Assert.Contains("no band given", (case.Links |> List.find (fun link -> link.Kind = LinkKind.Owner)).Note.Value)
    }

// Unnamed officers.

let private unnamedOfficer role : Officer =
    {
        Name = PartyNames.NotPublished
        Kind = PartyKind.Withheld
        Role = role
        Current = true
        AppointedOn = ValueNone
        EndedOn = ValueNone
        BornIn = ValueNone
        Nationality = ValueNone
        CountryOfResidence = ValueNone
        Address = ValueNone
        Entity = ValueNone
        IdentityVerification = ValueNone
    }

[<Fact>]
let ``each withheld officer is its own entity and its own gap`` () : Task =
    task {
        let officers =
            [
                unnamedOfficer OfficerRole.Director
                unnamedOfficer OfficerRole.Director
                unnamedOfficer (OfficerRole.Other "prokurent")
            ]

        let masked =
            { holding "Masking register" [ "ZZ" ] [ company "1" "ZEDCO" zz ] with
                Officers = Ok(Available officers)
            }

        let! case = openOn [ registry masked ] "1" zz

        Assert.Equal(
            3,
            case.Entities
            |> List.filter (fun entity -> entity.Party = PartyKind.Withheld)
            |> List.length
        )

        let gaps = gapsOn "officers" case
        Assert.Equal(3, gaps.Length)
        Assert.Contains(gaps, (fun gap -> gap.Detail.Contains "prokurent"))
    }

// Home records with a place and no number.

let private foreignRegistration number (place: string) (jurisdiction: Jurisdiction) =
    { company number "ACME HOLDINGS, INC." jurisdiction with
        Role = RecordRole.ForeignRegistration
        HomeRegistration =
            ValueSome
                {
                    LegalName = ValueSome "ACME HOLDINGS INC"
                    LegalForm = ValueNone
                    GoverningLaw = ValueNone
                    Country = ValueSome place
                    Register = ValueNone
                    RegistrationNumber = ValueNone
                }
    }

[<Fact>]
let ``a home record with a place and no number is placed, and gets the place's route`` () : Task =
    task {
        let co = Jurisdiction.ofCode "US-CO"

        let! case =
            openOn
                [
                    registry (holding "Colorado" [ "US-CO" ] [ foreignRegistration "1" "DE" co ])
                ]
                "1"
                co

        let link = case.Links |> List.find (fun link -> link.Kind = LinkKind.HomeRecord)
        let home = case.Entities |> List.find (fun entity -> entity.Key = link.To)
        Assert.Equal(ValueSome(Jurisdiction.ofCode "US-DE"), home.Jurisdiction)
        let gap = case.Gaps |> List.find (fun gap -> gap.About = ValueSome home.Key)
        Assert.Equal(GapKind.NeedsPaidContract, gap.Kind)
        Assert.Contains("US-DE", gap.Detail)
        Assert.Contains("from: DE", gap.Detail)
    }

[<Fact>]
let ``registrations in several states of one company reach one home entity`` () : Task =
    task {
        let co, ny = Jurisdiction.ofCode "US-CO", Jurisdiction.ofCode "US-NY"

        let colorado =
            registry (holding "Colorado" [ "US-CO" ] [ foreignRegistration "1" "DE" co ])

        let newYork =
            registry (holding "New York" [ "US-NY" ] [ foreignRegistration "2" "DELAWARE" ny ])

        let sources = sourcesWith [ colorado; newYork ]
        let! case = openWith sources "1" co

        let nyEntity =
            CaseFile.entityOf
                (EntityIdentity.OnRegister("New York", ny, "2"))
                "ACME HOLDINGS INC"
                PartyKind.LegalEntity
                (ValueSome ny)

        let who =
            {
                By = "A. Officer"
                At = at
                Source = "A search by name"
            }

        let declared =
            case
            |> CaseFile.declareLink case.Subject nyEntity (LinkKind.StatedRelationship "same name") ValueNone who
            |> Result.defaultWith (fun refused -> failwith $"%A{refused}")

        let! followed =
            Investigation.followAsync sources options nyEntity.Key "2" ny declared ct

        let full = followed |> Result.defaultWith (fun refused -> failwith $"%A{refused}")
        let homes = full.Links |> List.filter (fun link -> link.Kind = LinkKind.HomeRecord)
        Assert.Equal(2, homes.Length)
        Assert.Equal(1, homes |> List.map _.To |> List.distinct |> List.length)
    }

// Jurisdiction words.

[<Theory>]
[<InlineData("Japan", "JP")>]
[<InlineData("Republic of Korea", "KR")>]
[<InlineData("Korea, Republic of", "KR")>]
[<InlineData("South Korea", "KR")>]
[<InlineData("Taiwan", "TW")>]
[<InlineData("Israel", "IL")>]
[<InlineData("Brasil", "BR")>]
[<InlineData("Argentina", "AR")>]
[<InlineData("Chile", "CL")>]
[<InlineData("Colombia", "CO")>]
[<InlineData("Costa Rica", "CR")>]
[<InlineData("Perú", "PE")>]
[<InlineData("Ecuador", "EC")>]
[<InlineData("Paraguay", "PY")>]
[<InlineData("Moldova", "MD")>]
[<InlineData("Serbia", "RS")>]
[<InlineData("Україна", "UA")>]
[<InlineData("Russian Federation", "RU")>]
[<InlineData("Kenya", "KE")>]
[<InlineData("Bosnia and Herzegovina", "BA")>]
[<InlineData("Eesti", "EE")>]
[<InlineData("Česká republika", "CZ")>]
[<InlineData("Slovenská republika", "SK")>]
[<InlineData("Polska", "PL")>]
[<InlineData("Norge", "NO")>]
[<InlineData("România", "RO")>]
[<InlineData("British Columbia", "CA-BC")>]
[<InlineData("Ciudad Autónoma de Buenos Aires", "AR-C")>]
[<InlineData("CABA", "AR-C")>]
[<InlineData("Brčko distrikt", "BA-BRC")>]
[<InlineData("BRCKO", "BA-BRC")>]
[<InlineData("Barbados", "BB")>]
[<InlineData("Virgin Islands, British", "VG")>]
[<InlineData("Côte d’Ivoire", "CI")>]
[<InlineData("Korea, Democratic People's Republic of", "KP")>]
[<InlineData("Türkiye", "TR")>]
[<InlineData("Guinea-Bissau", "GW")>]
let ``countries with sources and their subdivisions are placed, in English and native names``
    (wording: string, expected: string)
    =
    Assert.Equal(ValueSome expected, JurisdictionWords.place [ ValueSome wording ] |> ValueOption.map _.Code)

[<Fact>]
let ``a bare two-letter code gives way to a US state named in another wording`` () =
    let placed wordings =
        JurisdictionWords.place (wordings |> List.map ValueSome)
        |> ValueOption.map _.Code

    Assert.Equal(ValueSome "US-DE", placed [ "DE"; "Delaware Secretary of State" ])
    Assert.Equal(ValueSome "US-MD", placed [ "MD"; "State of Maryland" ])
    Assert.Equal(ValueSome "DE", placed [ "DE" ])
    Assert.Equal(ValueSome "CA-BC", placed [ "Canada"; "British Columbia" ])

[<Fact>]
let ``a bare two-letter code on a US or Canadian record is that country's state or province`` () =
    let within code wordings =
        JurisdictionWords.placeWithin (Jurisdiction.ofCode code) (wordings |> List.map ValueSome)
        |> ValueOption.map _.Code

    Assert.Equal(ValueSome "US-DE", within "US-CO" [ "DE" ])
    Assert.Equal(ValueSome "US-CO", within "US-FL" [ "CO" ])
    Assert.Equal(ValueSome "CA-AB", within "CA-BC" [ "AB" ])
    Assert.Equal(ValueSome "DE", within "GB" [ "DE" ])
    Assert.Equal(ValueSome "MX", within "US-CO" [ "MX" ])

[<Fact>]
let ``a US state named within a register's or law's wording places the state, as Companies House writes them`` () =
    let placed wordings =
        JurisdictionWords.place (wordings |> List.map ValueSome)
        |> ValueOption.map _.Code

    Assert.Equal(
        ValueSome "US-CA",
        placed
            [
                "UNITED STATES"
                "Secretary Of State, State Of California"
                "General Corporation Law, State Of California"
            ]
    )

    Assert.Equal(ValueSome "US-NY", placed [ "UNITED STATES"; "New York State Business Corporation Law 1963" ])
    Assert.Equal(ValueSome "US-WV", placed [ "UNITED STATES"; "West Virginia Business Corporation Act" ])
    Assert.Equal(ValueSome "US", placed [ "UNITED STATES"; "Laws of Texas and of New York" ])
    Assert.Equal(ValueSome "US-DC", placed [ "UNITED STATES"; "Department of Licensing, Washington, D.C." ])
    Assert.Equal(ValueSome "US-WA", placed [ "UNITED STATES"; "Secretary of State, Washington" ])
    Assert.Equal(ValueNone, placed [ "Ministry of Justice of Georgia" ])

[<Theory>]
[<InlineData("N/A", false)>]
[<InlineData("NONE", false)>]
[<InlineData("0", false)>]
[<InlineData("000-000", false)>]
[<InlineData("", false)>]
[<InlineData("C0382019", true)>]
[<InlineData("B164962", true)>]
[<InlineData("982 715 040", true)>]
let ``a number field without a digit, or all zeros, holds no number`` (number: string, expected: bool) =
    Assert.Equal(expected, Reading.isNumber number)
