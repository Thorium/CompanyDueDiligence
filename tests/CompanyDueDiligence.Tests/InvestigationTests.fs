/// Following links and the case study's requirements, each with made-up names and numbers. Capabilities are tested through neutral registers and the
/// Companies House client over a fake handler; nothing here is special-cased for a named company.
module InvestigationTests

open System
open System.Threading
open System.Threading.Tasks
open Xunit
open CompanyDueDiligence
open CompanyDueDiligence.CompaniesHouse
open Fake

let private at = DateTimeOffset(2026, 11, 6, 12, 0, 0, TimeSpan.Zero)
let private gb = Jurisdiction.ofCode "GB"
let private zz = Jurisdiction.ofCode "ZZ"
let private usDe = Jurisdiction.ofCode "US-DE"
let private usTx = Jurisdiction.ofCode "US-TX"
let private usUt = Jurisdiction.ofCode "US-UT"
let private usMo = Jurisdiction.ofCode "US-MO"

let private options =
    { InvestigationOptions.defaults with
        ListMinimums = ListMinimums.anyEntries
        Now = fun () -> at
    }

let private officer =
    {
        By = "A. Officer"
        At = at
        Source = "Filing read by hand"
    }

let private sourcesWith registries : CaseSources =
    {
        Registries = registries
        Filings = []
        CrossCheck = ValueNone
        Routes = CoverageRoutes.all
        KnownAddresses = []
    }

let private baseCompany number name jurisdiction : Company =
    {
        Name = name
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
        OtherNames = []
    }

let private ownerOf (entity: EntityRegistration) : Owner =
    {
        Name = entity.LegalName
        Kind = PartyKind.LegalEntity
        Control = []
        Current = true
        Since = ValueNone
        EndedOn = ValueNone
        BornIn = ValueNone
        Nationality = ValueNone
        CountryOfResidence = ValueNone
        Address = ValueNone
        Entity = ValueSome entity
        IdentityVerification = ValueNone
    }

let private registration name number country : EntityRegistration =
    {
        LegalName = ValueSome name
        LegalForm = ValueNone
        GoverningLaw = ValueNone
        Country = ValueSome country
        Register = ValueNone
        RegistrationNumber = ValueSome number
    }

/// What a scripted register says of one company.
type private Entry =
    {
        Find: Result<Availability<Company voption>, RegistryFailure>
        Officers: Result<Availability<Officer list>, RegistryFailure>
        Owners: Result<Availability<OwnerListing>, RegistryFailure>
    }

let private entry (company: Company) =
    {
        Find = Ok(Available(ValueSome company))
        Officers = Ok(Available [])
        Owners =
            Ok(
                Available
                    {
                        Owners = []
                        Statements = []
                        Gaps = []
                    }
            )
    }

let private owned (owners: Owner list) (company: Company) =
    { entry company with
        Owners =
            Ok(
                Available
                    {
                        Owners = owners
                        Statements = []
                        Gaps = []
                    }
            )
    }

/// A register that answers from a map. A number not in it isn't held.
type private ScriptedRegistry(name: string, scope: Jurisdiction, entries: Map<string, Entry>) =
    let calls = Collections.Concurrent.ConcurrentQueue<string>()

    let answer value : Evidence<_> =
        {
            Source = name
            RetrievedAt = at
            AsOf = ValueNone
            Raw = ValueSome "{\"raw\":true}"
            Value = value
        }

    member _.Calls = List.ofSeq calls

    interface ICompanyRegistry with
        member _.Coverage =
            {
                Registry = name
                Jurisdictions = [ scope ]
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

        member _.TryFindAsync(number, _, _) =
            calls.Enqueue number

            match entries.TryFind number with
            | Some entry -> Task.FromResult(entry.Find |> Result.map answer)
            | None -> Task.FromResult(Ok(answer (Available ValueNone)))

        member _.SearchAsync(_, _, _) =
            Task.FromResult(Ok(answer (NotAvailable "Not scripted")))

        member _.OfficersAsync(number, _, _) =
            Task.FromResult((entries[number]).Officers |> Result.map answer)

        member _.OwnersAsync(number, _, _, _) =
            Task.FromResult((entries[number]).Owners |> Result.map answer)

let private openOn registries number jurisdiction =
    Investigation.openAsync
        (sourcesWith registries)
        options
        {
            CaseId = "CASE-1"
            Number = number
            Jurisdiction = jurisdiction
            NameGiven = "As the customer gave it"
        }
        ct

let private expectOk result =
    result |> Result.defaultWith (fun refused -> failwith $"%A{refused}")

let private gapsOn (subject: string) (case: CaseFile) =
    case.Gaps |> List.filter (fun gap -> gap.Subject.Contains subject)

[<Fact>]
let ``a company with nothing against it still has gaps, and no result`` () : Task =
    task {
        let register =
            ScriptedRegistry("Zedland Register", zz, Map [ "1", entry (baseCompany "1" "ZEDCO" zz) ])

        let! case = openOn [ register ] "1" zz
        Assert.Equal("ZEDCO", case.Entities.Head.Name)
        Assert.Equal(ValueSome CompanyStanding.Active, case.Entities.Head.Standing)
        Assert.Empty case.Flags
        Assert.NotEmpty(gapsOn "vendor check" case)
        Assert.NotEmpty(gapsOn "sanctions screening" case)
        Assert.NotEmpty(CaseFile.reviewNeeded case)
    }

[<Fact>]
let ``evidence carries source, retrieved time, its origin and the raw answer`` () : Task =
    task {
        let register =
            ScriptedRegistry("Zedland Register", zz, Map [ "1", entry (baseCompany "1" "ZEDCO" zz) ])

        let! case = openOn [ register ] "1" zz

        let item =
            case.Evidence |> List.find (fun evidence -> evidence.Topic = "company record")

        Assert.Equal("Zedland Register", item.Source)
        Assert.Equal(EvidenceOrigin.Register, item.Origin)
        Assert.Equal(at, item.RetrievedAt)
        Assert.Equal(ValueSome "{\"raw\":true}", item.Raw)
        Assert.Contains(Fact.RegistrationNumber "1", item.Facts)
    }

[<Fact>]
let ``a corporate owner is followed into whichever register covers its country`` () : Task =
    task {
        let holdings = registration "HOLDINGS BV" "22" "Zedland"

        let uk =
            ScriptedRegistry(
                "UK",
                gb,
                Map
                    [
                        "00000001",
                        owned
                            [
                                ownerOf
                                    { holdings with
                                        Country = ValueSome "ZZ"
                                    }
                            ]
                            (baseCompany "00000001" "SUB LTD" gb)
                    ]
            )

        let zed =
            ScriptedRegistry(
                "Zedland",
                zz,
                Map
                    [
                        "22",
                        entry
                            { baseCompany "22" "HOLDINGS BV" zz with
                                Standing = CompanyStanding.InInsolvency "administration"
                            }
                    ]
            )

        let! case = openOn [ uk; zed ] "00000001" gb
        Assert.Equal<string list>([ "22" ], zed.Calls)
        let link = case.Links |> List.find (fun item -> item.Kind = LinkKind.Owner)
        Assert.Equal(LinkProvenance.Register "UK", link.Provenance)
        Assert.Contains(case.Flags, (fun flag -> flag.On = link.To && flag.Kind = FlagKind.Insolvency))

        match CaseFile.flagsSeenFrom case.Subject case with
        | [ seen ] -> Assert.Equal(LinkKind.Owner, seen.Through.Head.Kind)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``an owner's number leading to a company of another name is a disagreement, on one register too`` () : Task =
    task {
        let register =
            ScriptedRegistry(
                "UK",
                gb,
                Map
                    [
                        "00000001",
                        owned
                            [ ownerOf (registration "STATED NAME LTD" "2" "UK") ]
                            (baseCompany "00000001" "SUB LTD" gb)
                        "00000002", entry (baseCompany "00000002" "OTHER NAME LTD" gb)
                    ]
            )

        let! case = openOn [ register ] "00000001" gb
        let flag = case.Flags |> List.find (fun item -> item.Kind = FlagKind.Disagreement)
        Assert.Equal("GB:00000002", flag.On)
        Assert.Contains("STATED NAME LTD", flag.Statement)
        Assert.Contains("OTHER NAME LTD", flag.Statement)
        Assert.DoesNotContain(case.Flags, (fun item -> item.Statement.Contains "number"))
    }

[<Fact>]
let ``a loop of owners ends, each read once`` () : Task =
    task {
        let register =
            ScriptedRegistry(
                "UK",
                gb,
                Map
                    [
                        "00000001",
                        owned [ ownerOf (registration "B" "00000002" "United Kingdom") ] (baseCompany "00000001" "A" gb)
                        "00000002",
                        owned [ ownerOf (registration "A" "00000001" "United Kingdom") ] (baseCompany "00000002" "B" gb)
                    ]
            )

        let! case = openOn [ register ] "00000001" gb
        Assert.Equal<string list>([ "00000001"; "00000002" ], register.Calls)
        Assert.Equal(2, case.Links.Length)
        Assert.Empty(gapsOn "company record" case)
    }

[<Fact>]
let ``past the depth limit, a linked company is a gap on the whole case`` () : Task =
    task {
        let register =
            ScriptedRegistry(
                "UK",
                gb,
                Map
                    [
                        "00000001", owned [ ownerOf (registration "B" "00000002" "UK") ] (baseCompany "00000001" "A" gb)
                        "00000002", owned [ ownerOf (registration "C" "00000003" "UK") ] (baseCompany "00000002" "B" gb)
                        "00000003", entry (baseCompany "00000003" "C" gb)
                    ]
            )

        let! case =
            Investigation.openAsync
                (sourcesWith [ register ])
                { options with MaxDepth = 1 }
                {
                    CaseId = "C"
                    Number = "00000001"
                    Jurisdiction = gb
                    NameGiven = "A"
                }
                ct

        Assert.Equal<string list>([ "00000001"; "00000002" ], register.Calls)

        match gapsOn "company record" case with
        | [ gap ] ->
            Assert.Equal(GapReach.WholeCase, gap.Reach)
            Assert.Equal(GapKind.NotChecked, gap.Kind)
            Assert.Contains("depth limit of 1", gap.Detail)
            Assert.Contains("followAsync", gap.WouldClose)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``past the entity limit, what is left is a gap`` () : Task =
    task {
        let register =
            ScriptedRegistry(
                "UK",
                gb,
                Map
                    [
                        "00000001",
                        owned
                            [
                                ownerOf (registration "B" "00000002" "UK")
                                ownerOf (registration "C" "00000003" "UK")
                            ]
                            (baseCompany "00000001" "A" gb)
                        "00000002", entry (baseCompany "00000002" "B" gb)
                    ]
            )

        let! case =
            Investigation.openAsync
                (sourcesWith [ register ])
                { options with MaxEntities = 2 }
                {
                    CaseId = "C"
                    Number = "00000001"
                    Jurisdiction = gb
                    NameGiven = "A"
                }
                ct

        Assert.Equal<string list>([ "00000001"; "00000002" ], register.Calls)
        Assert.Contains(gapsOn "company record" case, (fun gap -> gap.Detail.Contains "limit of 2"))
    }

[<Fact>]
let ``a German owner's identity holds its court, and Germany is a gap naming the court's approval`` () : Task =
    task {
        let owner =
            ownerOf
                { registration "BEISPIEL GMBH" "HRB 12345" "Germany" with
                    Register = ValueSome "Amtsgericht Köln"
                }

        let register =
            ScriptedRegistry("UK", gb, Map [ "00000001", owned [ owner ] (baseCompany "00000001" "SUB LTD" gb) ])

        let! case = openOn [ register ] "00000001" gb
        let entity = case.Entities |> List.find (fun item -> item.Name = "BEISPIEL GMBH")
        Assert.Contains(EntityIdentity.GermanCourtRegister("Köln", "HRB", "12345"), entity.Identities)
        let gap = case.Gaps |> List.find (fun item -> item.About = ValueSome entity.Key)
        Assert.Equal(GapKind.NeedsEntitlement, gap.Kind)
        Assert.Contains("Hagen", gap.WouldClose)
        Assert.Equal(GapReach.WholeCase, gap.Reach)
    }

[<Fact>]
let ``a number that has lost its leading zeros is followed in the UK, with the published one in the identity's register``
    ()
    : Task =
    task {
        let register =
            ScriptedRegistry(
                "UK",
                gb,
                Map
                    [
                        "00000001", owned [ ownerOf (registration "B" "2" "England") ] (baseCompany "00000001" "A" gb)
                        "00000002", entry (baseCompany "00000002" "B" gb)
                    ]
            )

        let! _ = openOn [ register ] "00000001" gb
        Assert.Equal<string list>([ "00000001"; "00000002" ], register.Calls)
    }

let private withOwnerAnswer (owners: Result<Availability<OwnerListing>, RegistryFailure>) =
    ScriptedRegistry(
        "Zedland",
        zz,
        Map
            [
                "1",
                { entry (baseCompany "1" "ZEDCO" zz) with
                    Owners = owners
                }
            ]
    )

[<Fact>]
let ``a corporate owner with no number becomes a gap on the whole case`` () : Task =
    task {
        let owner =
            ownerOf
                { registration "NO NUMBER LTD" "" "Zedland" with
                    RegistrationNumber = ValueNone
                }

        let register =
            withOwnerAnswer (
                Ok(
                    Available
                        {
                            Owners = [ owner ]
                            Statements = []
                            Gaps = []
                        }
                )
            )

        let! case = openOn [ register ] "1" zz
        let gap = gapsOn "NO NUMBER LTD" case |> List.exactlyOne
        Assert.Equal(GapReach.WholeCase, gap.Reach)
        Assert.Equal(GapKind.NoRoute, gap.Kind)
    }

[<Fact>]
let ``wording that places no jurisdiction becomes a gap, and isn't guessed`` () : Task =
    task {
        let register =
            withOwnerAnswer (
                Ok(
                    Available
                        {
                            Owners = [ ownerOf (registration "FAR LTD" "9" "Atlantis") ]
                            Statements = []
                            Gaps = []
                        }
                )
            )

        let! case = openOn [ register ] "1" zz
        let gap = gapsOn "FAR LTD" case |> List.exactlyOne
        Assert.Contains("Atlantis", gap.Detail)
        Assert.Equal(GapKind.NoRoute, gap.Kind)
        Assert.Equal<string list>([ "1" ], register.Calls)
    }

[<Fact>]
let ``an owner gap, an individual owner and a withheld one each show`` () : Task =
    task {
        let person =
            { ownerOf (registration "x" "x" "x") with
                Name = ValueSome "Jane Doe"
                Kind = PartyKind.Individual
                Entity = ValueNone
                BornIn = ValueSome { Year = 1970; Month = 3 }
                Nationality = ValueSome "Zedlander"
            }

        let withheld =
            { person with
                Name = ValueNone
                Kind = PartyKind.Withheld
            }

        let former =
            { person with
                Name = ValueSome "Past Owner"
                Current = false
            }

        let register =
            withOwnerAnswer (
                Ok(
                    Available
                        {
                            Owners = [ person; withheld; former ]
                            Statements = []
                            Gaps = [ OwnershipGap.OwnerWithheld ]
                        }
                )
            )

        let! case = openOn [ register ] "1" zz

        Assert.Contains(
            gapsOn "owners" case,
            (fun gap -> gap.Detail.Contains "withholds" && gap.Reach = GapReach.WholeCase)
        )

        Assert.Contains(case.Entities, (fun entity -> entity.Name = "Jane Doe" && entity.Party = PartyKind.Individual))
        Assert.Contains(case.Entities, (fun entity -> entity.Party = PartyKind.Withheld))
        Assert.DoesNotContain(case.Entities, (fun entity -> entity.Name = "Past Owner"))

        Assert.Contains(
            case.Evidence,
            (fun item -> item.Facts |> List.contains (Fact.BornIn { Year = 1970; Month = 3 }))
        )
    }

[<Fact>]
let ``owners not published and owners needing entitlement are different gaps, each saying what closes them`` () : Task =
    task {
        let! notPublished = openOn [ withOwnerAnswer (Ok(NotAvailable "Exempt")) ] "1" zz

        let! entitled =
            openOn
                [
                    withOwnerAnswer (
                        Ok(
                            NeedsEntitlement
                                {
                                    Who = "Obliged entities"
                                    Route = "Apply to the register"
                                }
                        )
                    )
                ]
                "1"
                zz

        Assert.Equal(GapKind.NotPublished, (gapsOn "owners" notPublished).Head.Kind)
        let gap = (gapsOn "owners" entitled).Head
        Assert.Equal(GapKind.NeedsEntitlement, gap.Kind)
        Assert.Equal("Apply to the register", gap.WouldClose)
        Assert.Contains("Obliged entities", gap.Detail)
    }

[<Fact>]
let ``the purpose reaches the register`` () : Task =
    task {
        use handler =
            new FakeHandler(fun _ ->
                ok """{"data":{"name":"X","registration_number":"C 1","state":"Active","type":"Y"}}""")

        let malta =
            Sources.malta (SourceConnection(handler, (fun () -> at), (fun _ _ -> Task.CompletedTask)))

        let mt = Jurisdiction.ofCode "MT"

        let run purpose =
            Investigation.openAsync
                (sourcesWith [ malta ])
                { options with Purpose = purpose }
                {
                    CaseId = "C"
                    Number = "C 1"
                    Jurisdiction = mt
                    NameGiven = "X"
                }
                ct

        let! diligence = run OwnerPurpose.CustomerDueDiligence
        let! open' = run OwnerPurpose.PublicRead
        Assert.Equal(GapKind.NeedsEntitlement, (gapsOn "owners" diligence).Head.Kind)
        Assert.Equal(GapKind.NotAvailableForCountry, (gapsOn "owners" open').Head.Kind)
    }

// Fail closed: each failing source is a gap, and the case comes back.

[<Theory>]
[<InlineData("Unavailable", "again later")>]
[<InlineData("UnreadableAnswer", "person read")>]
[<InlineData("RequestRefused", "Fix the access")>]
[<InlineData("InvalidRequest", "Correct the number")>]
[<InlineData("RateLimited", "again later")>]
let ``a failing register is a gap saying what closes it`` (kind: string, wouldClose: string) : Task =
    task {
        let failure =
            match kind with
            | "Unavailable" -> RegistryFailure.Unavailable "x"
            | "UnreadableAnswer" -> RegistryFailure.UnreadableAnswer "x"
            | "RequestRefused" -> RegistryFailure.RequestRefused "x"
            | "InvalidRequest" -> RegistryFailure.InvalidRequest "x"
            | _ -> RegistryFailure.RateLimited ValueNone

        let register =
            ScriptedRegistry(
                "Zedland",
                zz,
                Map
                    [
                        "1",
                        { entry (baseCompany "1" "X" zz) with
                            Find = Error failure
                        }
                    ]
            )

        let! case = openOn [ register ] "1" zz
        let gap = gapsOn "company record" case |> List.exactlyOne
        Assert.Equal(GapKind.SourceFailed, gap.Kind)
        Assert.Equal(GapReach.WholeCase, gap.Reach)
        Assert.Contains(wouldClose, gap.WouldClose)
        Assert.Equal(ValueNone, case.Entities.Head.Standing)
    }

[<Fact>]
let ``failing officers and owners are gaps, and the company's record stays`` () : Task =
    task {
        let register =
            ScriptedRegistry(
                "Zedland",
                zz,
                Map
                    [
                        "1",
                        { entry (baseCompany "1" "X" zz) with
                            Officers = Error(RegistryFailure.Unavailable "x")
                            Owners = Error(RegistryFailure.UnreadableAnswer "y")
                        }
                    ]
            )

        let! case = openOn [ register ] "1" zz
        Assert.Equal(GapKind.SourceFailed, (gapsOn "officers" case).Head.Kind)
        Assert.Equal(GapKind.SourceFailed, (gapsOn "owners" case).Head.Kind)
        Assert.Equal(ValueSome CompanyStanding.Active, case.Entities.Head.Standing)
    }

/// A register that raises, as a host's own adapter might.
type private ThrowingRegistry() =
    interface ICompanyRegistry with
        member _.Coverage =
            {
                Registry = "Throwing"
                Jurisdictions = [ zz ]
                Standing = Support.Supported
                Concerns = Support.Supported
                Addresses = Support.Supported
                Search = Support.Supported
                Officers = Support.Supported
                Owners = Support.Supported
                Cost = AccessCost.Free
                RateLimit = RateLimitPolicy.NotPublished(1, TimeSpan.FromSeconds 1.)
                Licence = "None"
            }

        member _.TryFindAsync(_, _, _) = failwith "secret text from the adapter"
        member _.SearchAsync(_, _, _) = failwith "unused"
        member _.OfficersAsync(_, _, _) = failwith "unused"
        member _.OwnersAsync(_, _, _, _) = failwith "unused"

[<Fact>]
let ``an adapter that raises is a gap, holding none of its message`` () : Task =
    task {
        let! case = openOn [ ThrowingRegistry() ] "1" zz
        let gap = gapsOn "company record" case |> List.exactlyOne
        Assert.Equal(GapKind.SourceFailed, gap.Kind)
        Assert.DoesNotContain("secret", gap.Detail)
    }

[<Fact>]
let ``cancelling throws, and leaves no gap pretending to be a failure`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok "{}")

        let malta =
            Sources.malta (SourceConnection(handler, (fun () -> at), (fun _ _ -> Task.CompletedTask)))

        use cancelled = new CancellationTokenSource()
        do! cancelled.CancelAsync()

        let! thrown =
            Assert.ThrowsAnyAsync<OperationCanceledException>(fun () ->
                Investigation.openAsync
                    (sourcesWith [ malta ])
                    options
                    {
                        CaseId = "C"
                        Number = "C 1"
                        Jurisdiction = Jurisdiction.ofCode "MT"
                        NameGiven = "X"
                    }
                    cancelled.Token
                :> Task)

        Assert.NotNull thrown
    }

[<Fact>]
let ``a register saying it holds no such number is a neutral flag`` () : Task =
    task {
        let register = ScriptedRegistry("Zedland", zz, Map.empty)
        let! case = openOn [ register ] "404" zz
        let flag = case.Flags |> List.exactlyOne
        Assert.Equal(FlagKind.NotOnRegister, flag.Kind)
        Assert.Contains("holds no number 404", flag.Statement)
    }

[<Fact>]
let ``a source that can't tell is a gap, never not on the register`` () : Task =
    task {
        let register =
            ScriptedRegistry(
                "Zedland",
                zz,
                Map
                    [
                        "1",
                        { entry (baseCompany "1" "X" zz) with
                            Find = Ok(NotAvailable "Non-diffusible companies are left out")
                        }
                    ]
            )

        let! case = openOn [ register ] "1" zz
        Assert.Empty case.Flags
        let gap = gapsOn "company record" case |> List.exactlyOne
        Assert.Equal(GapKind.NotPublished, gap.Kind)
        Assert.Contains("Non-diffusible", gap.Detail)

        // The answer is kept as evidence, though it settles nothing.
        let item =
            case.Evidence |> List.find (fun evidence -> evidence.Topic = "company record")

        Assert.Equal(ValueSome "{\"raw\":true}", item.Raw)
        Assert.Empty item.Facts
    }

[<Fact>]
let ``concerns unconfirmed are a gap, and stated ones are flags`` () : Task =
    task {
        let register =
            ScriptedRegistry(
                "Zedland",
                zz,
                Map
                    [
                        "1",
                        entry
                            { baseCompany "1" "X" zz with
                                Concerns =
                                    {
                                        Stated =
                                            [ CompanyConcern.AccountsOverdue; CompanyConcern.RegisteredOfficeDisputed ]
                                        Unconfirmed = [ CompanyConcern.InsolvencyHistory ]
                                    }
                            }
                    ]
            )

        let! case = openOn [ register ] "1" zz
        Assert.Contains(case.Flags, (fun flag -> flag.Kind = FlagKind.OverdueFiling))
        Assert.Contains(case.Flags, (fun flag -> flag.Kind = FlagKind.RegisteredOfficeProblem))
        Assert.Contains("insolvency history", (gapsOn "filings and disputes" case).Head.Detail)
    }

[<Theory>]
[<InlineData("Dissolved")>]
[<InlineData("Closed")>]
[<InlineData("WindingUp")>]
[<InlineData("Unknown")>]
[<InlineData("ActiveWithConcern")>]
let ``a standing other than live shows as a status concern`` (word: string) : Task =
    task {
        let standing =
            match word with
            | "Dissolved" -> CompanyStanding.Dissolved
            | "Closed" -> CompanyStanding.Closed "Void"
            | "WindingUp" -> CompanyStanding.WindingUp "In Dissolution"
            | "Unknown" -> CompanyStanding.Unknown ValueNone
            | _ -> CompanyStanding.ActiveWithConcern "proposal to strike off"

        let register =
            ScriptedRegistry(
                "Zedland",
                zz,
                Map
                    [
                        "1",
                        entry
                            { baseCompany "1" "X" zz with
                                Standing = standing
                            }
                    ]
            )

        let! case = openOn [ register ] "1" zz
        Assert.Equal(FlagKind.StatusConcern, case.Flags.Head.Kind)
    }

[<Fact>]
let ``a register with no status word raises no status flag`` () : Task =
    task {
        let register =
            ScriptedRegistry(
                "Zedland",
                zz,
                Map
                    [
                        "1",
                        entry
                            { baseCompany "1" "X" zz with
                                Standing = CompanyStanding.Registered ValueNone
                            }
                    ]
            )

        let! case = openOn [ register ] "1" zz
        Assert.Empty case.Flags
    }

// Companies House through the client, for what only it provides.

let private chSources (handler: FakeHandler) =
    let registry = CompaniesHouseRegistry(clientWith handler, fun () -> at)

    {
        Registries = [ registry :> ICompanyRegistry ]
        Filings = [ registry :> IFilingRegister ]
        CrossCheck = ValueNone
        Routes = CoverageRoutes.all
        KnownAddresses = []
    }

let private profile (number: string) (name: string) (extra: string) =
    """{"company_number":"%NUMBER%","company_name":"%NAME%","company_status":"active","type":"ltd",
         "has_insolvency_history":false,"has_been_liquidated":false,"registered_office_is_in_dispute":false,"undeliverable_registered_office_address":false,
         "accounts":{"next_accounts":{"overdue":false}},"confirmation_statement":{"overdue":false}%EXTRA%}"""
        .Replace("%NUMBER%", number)
        .Replace("%NAME%", name)
        .Replace("%EXTRA%", extra)

/// A fake Companies House: the profile, officers, PSCs and filings of each number. A number not held answers 404.
let private companiesHouse (companies: Map<string, string * string list * string list * string list>) (call: Call) =
    let path = Uri(call.Uri).AbsolutePath

    let number =
        path.Split('/', StringSplitOptions.RemoveEmptyEntries) |> Array.tryItem 1

    match number |> Option.bind companies.TryFind with
    | None -> notFound
    | Some(body, officers, pscs, filings) ->
        let atStart items =
            if startIndexOf call = 0 then items else []

        let endedIn (items: string list) =
            items
            |> List.filter (fun item -> item.Contains "resigned_on" || item.Contains "ceased_on")
            |> List.length

        if path.EndsWith "/officers" then
            ok (officersJson officers.Length (endedIn officers) (atStart officers))
        elif path.EndsWith "/persons-with-significant-control" then
            ok (pscsJson pscs.Length (endedIn pscs) (atStart pscs))
        elif path.EndsWith "/persons-with-significant-control-statements" then
            ok (pscsJson 0 0 [])
        elif path.EndsWith "/filing-history" then
            ok (listJson "total_count" filings.Length (atStart filings))
        elif path.EndsWith "/insolvency" then
            ok
                """{"status":"administration-order","cases":[{"type":"in-administration","number":"1","dates":[],"practitioners":[]}]}"""
        elif path.EndsWith "/uk-establishments" then
            ok
                """{"items":[{"company_number":"BR000001","company_name":"EXAMPLE WIDGETS (UK BRANCH)","company_status":"open"}]}"""
        else
            ok body

let private start number =
    {
        CaseId = "CASE"
        Number = number
        Jurisdiction = gb
        NameGiven = "As given"
    }

/// Case study 1: a UK overseas-company record names a home registration in Delaware.
let private overseasProfile =
    profile
        "FC000001"
        "EXAMPLE WIDGETS INC"
        ""","type":"oversea-company","foreign_company_details":{"originating_registry":{"country":"United States","name":"Delaware Secretary of State"},"registration_number":"7654321","governed_by":"Delaware","legal_form":"Corporation"}"""

let private overseasFilings =
    [
        """{"transaction_id":"T1","category":"overseas","type":"OSIN01","date":"2015-01-01","description":"overseas-company-registration","pages":12,"links":{"document_metadata":"https://document-api.example/document/doc-1"}}"""
    ]

[<Fact>]
let ``scenario 1: a UK overseas record leads to its home record, kept apart, and the home record is a gap naming the lawful routes``
    ()
    : Task =
    task {
        use handler =
            new FakeHandler(companiesHouse (Map [ "FC000001", (overseasProfile, [], [], overseasFilings) ]))

        let! case = Investigation.openAsync (chSources handler) options (start "FC000001") ct
        let subject = case.Entities |> List.find (fun entity -> entity.Key = case.Subject)
        Assert.Equal("EXAMPLE WIDGETS INC", subject.Name)
        Assert.Equal(RecordRole.ForeignRegistration, subject.Role)

        let link = case.Links |> List.find (fun item -> item.Kind = LinkKind.HomeRecord)
        Assert.Equal(LinkProvenance.Register "Companies House", link.Provenance)
        Assert.True(CaseLink.isEvidenced link)
        let home = case.Entities |> List.find (fun entity -> entity.Key = link.To)
        Assert.Equal("US-DE:7654321", home.Key)
        Assert.Contains(EntityIdentity.OnRegister("Delaware Secretary of State", usDe, "7654321"), home.Identities)
        Assert.Equal(ValueNone, home.Standing)

        // No adapter reads Delaware, and no call was attempted anywhere but Companies House.
        Assert.All(handler.Calls, (fun call -> Assert.Contains("company-information", call.Host)))
        let gap = gapsOn "Delaware" case |> List.exactlyOne
        Assert.Equal(GapKind.NeedsPaidContract, gap.Kind)
        Assert.Equal(GapReach.WholeCase, gap.Reach)
        Assert.Contains("forbid automated use", gap.Detail)
        Assert.Contains("OpenCorporates", gap.WouldClose)

        // The filing is surfaced, with its link, kind and page count.
        let document = case.Documents |> List.exactlyOne
        Assert.Equal(ValueSome 12, document.Pages)
        Assert.Equal(DocumentKind.Constitutional, document.Kind)
        Assert.Equal(ValueSome "doc-1", document.DocumentId)
        Assert.Contains("document-api.example", document.Link.Value)
    }

[<Fact>]
let ``scenario 1: what the UK form and the home papers say of capital is set side by side`` () : Task =
    task {
        use handler =
            new FakeHandler(companiesHouse (Map [ "FC000001", (overseasProfile, [], [], overseasFilings) ]))

        let! case = Investigation.openAsync (chSources handler) options (start "FC000001") ct
        let document = case.Documents.Head

        let home =
            case.Links |> List.find (fun item -> item.Kind = LinkKind.HomeRecord) |> _.To

        let capital kind count =
            Fact.Capital
                {
                    Kind = kind
                    Shares = ValueSome count
                    Amount = ValueNone
                    Currency = ValueNone
                }

        let read, readingId =
            CaseFile.attachReading
                document.Id
                [ capital CapitalKind.Issued 10000000m ]
                "The form states 10,000,000 shares issued."
                { officer with
                    Source = "UK form OSIN01"
                }
                case
            |> expectOk

        let withHome =
            read
            |> CaseFile.addFinding
                home
                "home certificate"
                [ capital CapitalKind.Authorised 10000000m; Fact.LegalForm "Corporation" ]
                ValueNone
                { officer with
                    Source = "Certificate of incorporation"
                }
            |> expectOk
            |> CaseFile.addFinding
                home
                "founders' resolution"
                [ capital CapitalKind.Issued 9000000m ]
                ValueNone
                { officer with
                    Source = "Founders' resolution"
                }
            |> expectOk

        Assert.StartsWith("E", readingId)

        let flags =
            withHome.Flags
            |> List.filter (fun flag -> flag.Kind = FlagKind.Disagreement)
            |> List.map _.Statement

        Assert.Contains(
            flags,
            (fun text ->
                text.Contains "10,000,000 shares issued"
                && text.Contains "10,000,000 shares authorised")
        )

        Assert.Contains(
            flags,
            (fun text ->
                text.Contains "10,000,000 shares issued"
                && text.Contains "9,000,000 shares issued")
        )

        // The UK record's stated legal form and the home certificate's agree, and aren't flagged.
        Assert.DoesNotContain(flags, (fun text -> text.Contains "legal form"))

        // What the officer read closed that document's own gap, and left the home record's open.
        Assert.Contains(gapsOn "filing to read" withHome, (fun gap -> gap.ClosedBy.IsSome))
        Assert.Contains(gapsOn "Delaware" withHome, (fun gap -> gap.ClosedBy.IsNone))
    }

[<Fact>]
let ``the UK record's statement of the home form is compared with what the home record says`` () : Task =
    task {
        use handler =
            new FakeHandler(companiesHouse (Map [ "FC000001", (overseasProfile, [], [], overseasFilings) ]))

        let! case = Investigation.openAsync (chSources handler) options (start "FC000001") ct

        let home =
            case.Links |> List.find (fun item -> item.Kind = LinkKind.HomeRecord) |> _.To

        let told =
            case
            |> CaseFile.addFinding
                home
                "home certificate"
                [ Fact.LegalForm "LLC"; Fact.RegistrationNumber "1111111" ]
                ValueNone
                { officer with Source = "Certificate" }
            |> expectOk

        let flags =
            told.Flags
            |> List.filter (fun flag -> flag.On = home && flag.Kind = FlagKind.Disagreement)
            |> List.map _.Statement

        Assert.Contains(flags, (fun text -> text.Contains "legal form" && text.Contains "LLC"))
        Assert.Contains(flags, (fun text -> text.Contains "1111111" && text.Contains "7654321"))
    }

[<Fact>]
let ``a foreign registration that doesn't say where home is is a gap`` () : Task =
    task {
        let body = profile "FC000002" "NO HOME INC" ""","type":"oversea-company" """

        use handler =
            new FakeHandler(companiesHouse (Map [ "FC000002", (body, [], [], []) ]))

        let! case = Investigation.openAsync (chSources handler) options (start "FC000002") ct
        Assert.Equal(GapKind.NotPublished, (gapsOn "home record" case).Head.Kind)
    }

[<Fact>]
let ``an overseas company's UK establishments are linked and followed`` () : Task =
    task {
        let branch =
            profile
                "BR000001"
                "EXAMPLE WIDGETS (UK BRANCH)"
                ""","type":"uk-establishment","branch_company_details":{"parent_company_number":"FC000001","parent_company_name":"EXAMPLE WIDGETS INC"}"""

        use handler =
            new FakeHandler(
                companiesHouse (Map [ "FC000001", (overseasProfile, [], [], []); "BR000001", (branch, [], [], []) ])
            )

        let! case = Investigation.openAsync (chSources handler) options (start "FC000001") ct

        let link =
            case.Links |> List.find (fun item -> item.Kind = LinkKind.UkEstablishment)

        Assert.Equal(case.Subject, link.From)
        Assert.Equal("GB:BR000001", link.To)

        Assert.Equal(
            RecordRole.Establishment,
            (case.Entities |> List.find (fun entity -> entity.Key = "GB:BR000001")).Role
        )

        Assert.Equal(
            1,
            handler.Calls
            |> List.filter (fun call -> call.Target.EndsWith "/company/FC000001")
            |> List.length
        )
    }

[<Fact>]
let ``an establishment reached first leads back to its overseas company`` () : Task =
    task {
        let branch =
            profile
                "BR000001"
                "EXAMPLE WIDGETS (UK BRANCH)"
                ""","type":"uk-establishment","branch_company_details":{"parent_company_number":"FC000001"}"""

        use handler =
            new FakeHandler(
                companiesHouse (Map [ "FC000001", (overseasProfile, [], [], []); "BR000001", (branch, [], [], []) ])
            )

        let! case = Investigation.openAsync (chSources handler) options (start "BR000001") ct

        let link =
            case.Links |> List.find (fun item -> item.Kind = LinkKind.UkEstablishment)

        Assert.Equal("GB:FC000001", link.From)
        Assert.Equal("GB:BR000001", link.To)

        Assert.Equal(
            "EXAMPLE WIDGETS INC",
            (case.Entities |> List.find (fun entity -> entity.Key = "GB:FC000001")).Name
        )
    }

/// Texas rows in the Comptroller's shape, made up.
let private texasRow number name sos right organisation =
    $$"""[{"taxpayer_number":"32000000001","taxpayer_name":"{{name}}","taxpayer_address":"1 Agent Way","taxpayer_city":"AUSTIN","taxpayer_state":"TX","taxpayer_zip":"78701",
         "taxpayer_organizational_type":"{{organisation}}","record_type_code":"U","secretary_of_state_sos_or_coa_file_number":"{{number}}","sos_status_code":"{{sos}}","right_to_transact_business_code":"{{right}}"}]"""

[<Fact>]
let ``scenario 2: a forfeited branch shows on that branch, and says nothing about the home record`` () : Task =
    task {
        // A dissolved UK company whose owner is in administration.
        let dissolved =
            (profile "00000011" "EXAMPLE LENDING LTD" "")
                .Replace("\"company_status\":\"active\"", "\"company_status\":\"dissolved\"")

        let administered =
            (profile "00000012" "EXAMPLE HOLDINGS LTD" "")
                .Replace("\"company_status\":\"active\"", "\"company_status\":\"administration\"")
                .Replace("\"has_insolvency_history\":false", "\"has_insolvency_history\":true")

        let owner =
            """{"kind":"corporate-entity-person-with-significant-control","name":"EXAMPLE HOLDINGS LTD","natures_of_control":["ownership-of-shares-75-to-100-percent"],
                "identification":{"legal_form":"Limited","country_registered":"England","place_registered":"Companies House","registration_number":"00000012"}}"""

        use ch =
            new FakeHandler(
                companiesHouse (
                    Map
                        [
                            "00000011", (dissolved, [], [ owner ], [])
                            "00000012", (administered, [], [], [])
                        ]
                )
            )

        use texasHandler =
            new FakeHandler(fun _ -> ok (texasRow "0800000001" "EXAMPLE LENDING INC" "F" "N" "CF"))

        let texas =
            Sources.texas (SourceConnection(texasHandler, (fun () -> at), (fun _ _ -> Task.CompletedTask)))

        let sources =
            { chSources ch with
                Registries = (chSources ch).Registries @ [ texas ]
            }

        let! case = Investigation.openAsync sources options (start "00000011") ct
        Assert.Contains(case.Flags, (fun flag -> flag.On = case.Subject && flag.Kind = FlagKind.StatusConcern))
        Assert.Contains(case.Flags, (fun flag -> flag.On = "GB:00000012" && flag.Kind = FlagKind.Insolvency))

        Assert.Contains(
            case.Flags,
            (fun flag -> flag.On = "GB:00000012" && flag.Statement.Contains "in-administration")
        )

        // No register connects the same-named US company. An officer declares that, and its registrations.
        let home =
            CaseFile.entityOf
                (EntityIdentity.OnRegister("Delaware Division of Corporations", usDe, "5550001"))
                "EXAMPLE LENDING INC"
                PartyKind.LegalEntity
                (ValueSome usDe)

        let branch state number =
            let jurisdiction = Jurisdiction.ofCode $"US-{state}"

            CaseFile.entityOf
                (EntityIdentity.OnRegister($"{state} register", jurisdiction, number))
                "EXAMPLE LENDING INC"
                PartyKind.LegalEntity
                (ValueSome jurisdiction)

        let tx, ut, mo =
            branch "TX" "0800000001", branch "UT" "6660001", branch "MO" "7770001"

        let declared =
            case
            |> CaseFile.declareLink
                case.Subject
                home
                (LinkKind.StatedRelationship "same name, no register link")
                (ValueSome "Name match only")
                officer
            |> expectOk
            |> CaseFile.declareLink home.Key tx LinkKind.ForeignRegistration ValueNone officer
            |> expectOk
            |> CaseFile.declareLink home.Key ut LinkKind.ForeignRegistration ValueNone officer
            |> expectOk
            |> CaseFile.declareLink home.Key mo LinkKind.ForeignRegistration ValueNone officer
            |> expectOk

        let follow (entity: CaseEntity) (number: string) jurisdiction state =
            Investigation.followAsync sources options entity.Key number jurisdiction state ct

        let! afterDelaware = follow home "5550001" usDe declared
        let! afterTexas = follow tx "0800000001" usTx (expectOk afterDelaware)
        let! afterUtah = follow ut "6660001" usUt (expectOk afterTexas)
        let! afterMissouri = follow mo "7770001" usMo (expectOk afterUtah)
        let full = expectOk afterMissouri

        // Texas's forfeiture is on Texas's record alone.
        let texasEntity = full.Entities |> List.find (fun entity -> entity.Key = tx.Key)
        Assert.Equal(ValueSome(CompanyStanding.Closed "Forfeited Franchise Tax"), texasEntity.Standing)
        Assert.Equal(RecordRole.ForeignRegistration, texasEntity.Role)
        let homeEntity = full.Entities |> List.find (fun entity -> entity.Key = home.Key)
        Assert.Equal(ValueNone, homeEntity.Standing)
        Assert.DoesNotContain(full.Flags, (fun flag -> flag.On = home.Key))

        // It is visible from the home record, marked as coming through the link, which is declared.
        let seen =
            CaseFile.flagsSeenFrom home.Key full
            |> List.find (fun item -> item.Flag.On = tx.Key)

        Assert.Equal(LinkKind.ForeignRegistration, seen.Through.Head.Kind)
        Assert.False seen.Through.Head.Evidenced
        let text = CaseFile.describeSeen full seen
        Assert.Contains("shows only that a link exists", text)
        Assert.Contains("declared, not evidenced", text)

        // Delaware has a declared route, and Utah and Missouri have none.
        Assert.Equal(
            GapKind.NeedsPaidContract,
            (gapsOn "Delaware" full |> List.find (fun gap -> gap.About = ValueSome home.Key)).Kind
        )

        Assert.Equal(GapKind.NoRoute, (full.Gaps |> List.find (fun gap -> gap.About = ValueSome ut.Key)).Kind)
        Assert.Equal(GapKind.NoRoute, (full.Gaps |> List.find (fun gap -> gap.About = ValueSome mo.Key)).Kind)

        // The UK company's own trouble doesn't become an allegation against the US record.
        Assert.All(
            full.Flags |> List.filter (fun flag -> flag.On = home.Key),
            (fun _ -> failwith "No flag belongs here")
        )

        // Reachable from the subject, every declared link is listed as such.
        let review = CaseFile.reviewNeeded full

        Assert.Equal(
            4,
            review
            |> List.filter (fun item -> item.Text.StartsWith "Declared link, not evidenced")
            |> List.length
        )
    }

[<Fact>]
let ``following an entity the case doesn't hold is refused`` () : Task =
    task {
        let register =
            ScriptedRegistry("Zedland", zz, Map [ "1", entry (baseCompany "1" "X" zz) ])

        let! case = openOn [ register ] "1" zz

        let! refused =
            Investigation.followAsync (sourcesWith [ register ]) options "no" "1" zz case ct

        Assert.Equal(Error(CaseEditRefused.UnknownEntity "no"), refused)
    }

/// Case study 3: a scanned insolvency filing names creditors that no register records.
let private insolvencyFilings =
    [
        """{"transaction_id":"T9","category":"insolvency","type":"AM03","date":"2021-05-06","description":"administrator-proposals","pages":40,"links":{"document_metadata":"https://document-api.example/document/doc-9"}}"""
        """{"transaction_id":"T8","category":"accounts","type":"AA","date":"2019-05-06","description":"accounts","pages":12}"""
        """{"transaction_id":"T7","category":"resolution","type":"RES15","date":"2018-05-06","description":"resolution","links":{"document_metadata":"https://document-api.example/document/doc-7"}}"""
    ]

let private filedCompany = profile "00000021" "EXAMPLE FINANCE LTD" ""

[<Fact>]
let ``scenario 3: filings are surfaced, insolvency and constitutional ones first, each unread one a gap`` () : Task =
    task {
        use handler =
            new FakeHandler(companiesHouse (Map [ "00000021", (filedCompany, [], [], insolvencyFilings) ]))

        let! case = Investigation.openAsync (chSources handler) options (start "00000021") ct
        Assert.Equal(3, case.Documents.Length)

        let insolvency =
            case.Documents |> List.find (fun document -> document.TransactionId = "T9")

        Assert.Equal(DocumentKind.Insolvency, insolvency.Kind)
        Assert.True(CaseDocument.isHighlighted insolvency)
        Assert.Equal(ValueSome 40, insolvency.Pages)

        let constitutional =
            case.Documents |> List.find (fun document -> document.TransactionId = "T7")

        Assert.Equal(DocumentKind.Constitutional, constitutional.Kind)

        let accounts =
            case.Documents |> List.find (fun document -> document.TransactionId = "T8")

        Assert.False(CaseDocument.isHighlighted accounts)
        Assert.Equal(2, (gapsOn "filing to read" case).Length)
        Assert.Contains("*[", CaseExport.toText false case)
    }

[<Fact>]
let ``scenario 3: the filing's creditors and lender become links resting on its reading, which no register holds``
    ()
    : Task =
    task {
        use handler =
            new FakeHandler(companiesHouse (Map [ "00000021", (filedCompany, [], [], insolvencyFilings) ]))

        let! case = Investigation.openAsync (chSources handler) options (start "00000021") ct
        let document = case.Documents |> List.find (fun item -> item.TransactionId = "T9")

        let read, reading =
            CaseFile.attachReading
                document.Id
                [
                    Fact.Other("creditors", "Example Fund Managers LLP; Example Securitisation SA")
                ]
                "Page 12 names the creditors."
                { officer with
                    Source = "AM03, page 12"
                }
                case
            |> expectOk

        let named name jurisdiction =
            CaseFile.entityOf (EntityIdentity.NameOnly(name, "AM03 of 2021")) name PartyKind.LegalEntity jurisdiction

        let fund = named "Example Fund Managers LLP" (ValueSome gb)

        let securitisation =
            named "Example Securitisation SA" (ValueSome(Jurisdiction.ofCode "LU"))

        let lender = named "Example Lending Inc" (ValueSome(Jurisdiction.ofCode "US"))

        let linked =
            read
            |> CaseFile.linkFromDocument case.Subject fund LinkKind.Creditor document.Id reading ValueNone
            |> expectOk
            |> CaseFile.linkFromDocument case.Subject securitisation LinkKind.Creditor document.Id reading ValueNone
            |> expectOk
            |> CaseFile.linkFromDocument
                case.Subject
                lender
                (LinkKind.StatedRelationship "related lender")
                document.Id
                reading
                ValueNone
            |> expectOk

        let links =
            linked.Links
            |> List.filter (fun link -> link.Provenance = LinkProvenance.FilingDocument(document.Id, reading))

        Assert.Equal(3, links.Length)
        Assert.All(links, (fun link -> Assert.True(CaseLink.isEvidenced link)))
        Assert.All(links, (fun link -> Assert.Equal<string list>([ reading ], link.EvidenceIds)))

        let evidence = linked.Evidence |> List.find (fun item -> item.Id = reading)
        Assert.Equal(EvidenceOrigin.FilingDocument, evidence.Origin)
        Assert.Equal(ValueSome officer.By, evidence.Attribution |> ValueOption.map _.By)
        Assert.Equal(ValueSome document.Id, evidence.DocumentId)
        Assert.Equal(ValueSome(DateOnly(2021, 5, 6)), evidence.AsOf)
        Assert.Equal(ValueSome "Page 12 names the creditors.", evidence.Raw)

        // It closes that document's gap, and the other filing's stays.
        let open' =
            gapsOn "filing to read" linked |> List.filter (fun gap -> gap.ClosedBy.IsNone)

        Assert.Equal(1, open'.Length)
    }

[<Fact>]
let ``a link from a document needs that document and its reading`` () : Task =
    task {
        use handler =
            new FakeHandler(companiesHouse (Map [ "00000021", (filedCompany, [], [], insolvencyFilings) ]))

        let! case = Investigation.openAsync (chSources handler) options (start "00000021") ct
        let first, second = case.Documents[0], case.Documents[1]

        let far =
            CaseFile.entityOf (EntityIdentity.NameOnly("N", "c")) "N" PartyKind.LegalEntity ValueNone

        let read, reading =
            CaseFile.attachReading first.Id [] "Text" officer case |> expectOk

        Assert.Equal(
            Error(CaseEditRefused.UnknownDocument "D99"),
            CaseFile.linkFromDocument case.Subject far LinkKind.Creditor "D99" reading ValueNone read
        )

        Assert.Equal(
            Error(CaseEditRefused.UnknownEvidence "E99"),
            CaseFile.linkFromDocument case.Subject far LinkKind.Creditor first.Id "E99" ValueNone read
        )

        Assert.Equal(
            Error(CaseEditRefused.NotAReadingOfThatDocument(reading, second.Id)),
            CaseFile.linkFromDocument case.Subject far LinkKind.Creditor second.Id reading ValueNone read
        )

        Assert.Equal(
            Error(CaseEditRefused.UnknownEntity "no"),
            CaseFile.linkFromDocument "no" far LinkKind.Creditor first.Id reading ValueNone read
        )

        Assert.Equal(
            Error(CaseEditRefused.UnknownDocument "D99"),
            CaseFile.attachReading "D99" [] "T" officer case |> Result.map fst
        )

        Assert.Equal(
            Error CaseEditRefused.NotAttributed,
            CaseFile.attachReading first.Id [] "T" { officer with By = "" } case
            |> Result.map fst
        )
    }

/// Reads whatever it is given, or fails as told.
type private TestReader(answer: Result<string * Fact list, string>) =
    interface IDocumentReader with
        member _.Name = "Test reader"
        member _.ReadAsync(_, _, _) = Task.FromResult answer

/// Raises while reading, as a host's reader might.
type private RaisingReader() =
    interface IDocumentReader with
        member _.Name = "Raising reader"
        member _.ReadAsync(_, _, _) = failwith "reader's own text"

/// A filing register whose document answer is scripted.
type private FilingScript(document: Result<DocumentFile voption, RegistryFailure>) =
    interface IFilingRegister with
        member _.Register = "Script"
        member _.Jurisdictions = [ gb ]
        member _.FilingsAsync(_, _) = failwith "unused"
        member _.InsolvencyAsync(_, _) = failwith "unused"
        member _.EstablishmentsAsync(_, _) = failwith "unused"
        member _.DocumentAsync(_, _) = Task.FromResult document

let private caseWithFiling () =
    task {
        use handler =
            new FakeHandler(companiesHouse (Map [ "00000021", (filedCompany, [], [], insolvencyFilings) ]))

        return! Investigation.openAsync (chSources handler) options (start "00000021") ct
    }

let private file: DocumentFile =
    {
        ContentType = "application/pdf"
        Content = [| 1uy |]
    }

[<Fact>]
let ``a reader's text and facts become evidence, attributed to the reader`` () : Task =
    task {
        let! case = caseWithFiling ()
        let document = case.Documents |> List.find (fun item -> item.DocumentId.IsSome)

        let! read =
            Investigation.readDocumentAsync
                (FilingScript(Ok(ValueSome file)))
                (TestReader(Ok("Read text", [ Fact.Other("a", "b") ])))
                document.Id
                options
                case
                ct

        let read = expectOk read

        let evidence =
            read.Evidence |> List.find (fun item -> item.DocumentId = ValueSome document.Id)

        Assert.Equal(ValueSome "Read text", evidence.Raw)
        Assert.Equal(ValueSome "Test reader", evidence.Attribution |> ValueOption.map _.By)
    }

[<Fact>]
let ``a document that can't be fetched or read stays unread, with why`` () : Task =
    task {
        let! case = caseWithFiling ()
        let document = case.Documents |> List.find (fun item -> item.TransactionId = "T9")

        let unreadCount (state: CaseFile) =
            gapsOn "filing to read" state
            |> List.filter (fun gap -> gap.ClosedBy.IsNone)
            |> List.length

        let before = unreadCount case

        let attempts: (IFilingRegister * IDocumentReader * string) list =
            [
                FilingScript(Error(RegistryFailure.Unavailable "x")), TestReader(Ok("t", [])), "unavailable"
                FilingScript(Ok ValueNone), TestReader(Ok("t", [])), "no such document"
                FilingScript(Ok(ValueSome file)), TestReader(Error "All images"), "All images"
                FilingScript(Ok(ValueSome file)), RaisingReader(), "raised"
            ]

        for register, reader, expected in attempts do
            let! read =
                Investigation.readDocumentAsync register reader document.Id options case ct

            let read = expectOk read
            Assert.True(unreadCount read > before)
            Assert.Contains(gapsOn "filing to read" read, (fun gap -> gap.Detail.Contains expected))
            Assert.DoesNotContain(read.Evidence, (fun item -> item.DocumentId = ValueSome document.Id))
            Assert.DoesNotContain(gapsOn "filing to read" read, (fun gap -> gap.Detail.Contains "reader's own text"))
    }

[<Fact>]
let ``a filing with no image, and an unknown document, are said so`` () : Task =
    task {
        let! case = caseWithFiling ()
        let accounts = case.Documents |> List.find (fun item -> item.TransactionId = "T8")

        let! read =
            Investigation.readDocumentAsync
                (FilingScript(Ok(ValueSome file)))
                (TestReader(Ok("t", [])))
                accounts.Id
                options
                case
                ct

        Assert.Contains(gapsOn "filing to read" (expectOk read), (fun gap -> gap.Detail.Contains "no image"))

        let! unknown =
            Investigation.readDocumentAsync
                (FilingScript(Ok(ValueSome file)))
                (TestReader(Ok("t", [])))
                "D99"
                options
                case
                ct

        Assert.Equal(Error(CaseEditRefused.UnknownDocument "D99"), unknown)
    }

[<Fact>]
let ``an insolvency record that fails to read is a gap on the whole case, with the flag from the profile standing``
    ()
    : Task =
    task {
        let troubled =
            (profile "00000051" "X LTD" "")
                .Replace("\"has_insolvency_history\":false", "\"has_insolvency_history\":true")

        use handler =
            new FakeHandler(fun call ->
                if call.Target.EndsWith "/insolvency" then
                    answer 500 ""
                else
                    companiesHouse (Map [ "00000051", (troubled, [], [], []) ]) call)

        let! case = Investigation.openAsync (chSources handler) options (start "00000051") ct
        let gap = gapsOn "insolvency record" case |> List.exactlyOne
        Assert.Equal(GapKind.SourceFailed, gap.Kind)
        Assert.Equal(GapReach.WholeCase, gap.Reach)
        Assert.Contains(case.Flags, (fun flag -> flag.Kind = FlagKind.Insolvency))
    }

[<Fact>]
let ``filings that fail, or that the register doesn't publish, are gaps`` () : Task =
    task {
        use failing =
            new FakeHandler(fun call ->
                if call.Target.Contains "filing-history" then
                    answer 500 ""
                else
                    companiesHouse (Map [ "00000021", (filedCompany, [], [], []) ]) call)

        let! case = Investigation.openAsync (chSources failing) options (start "00000021") ct

        Assert.Equal(
            GapKind.SourceFailed,
            (gapsOn "filings" case |> List.find (fun gap -> gap.Subject = "filings")).Kind
        )

        use notHeld =
            new FakeHandler(fun call ->
                if call.Target.Contains "filing-history" then
                    notFound
                else
                    companiesHouse (Map [ "00000021", (filedCompany, [], [], []) ]) call)

        let! absent =
            Investigation.openAsync (chSources notHeld) options (start "00000021") ct

        Assert.Equal(
            GapKind.NotPublished,
            (gapsOn "filings" absent |> List.find (fun gap -> gap.Subject = "filings")).Kind
        )

        let register =
            ScriptedRegistry("Zedland", zz, Map [ "1", entry (baseCompany "1" "X" zz) ])

        let! noRoute = openOn [ register ] "1" zz

        Assert.Equal(
            GapKind.NotAvailableForCountry,
            (gapsOn "filings" noRoute |> List.find (fun gap -> gap.Subject = "filings")).Kind
        )
    }

/// Case study 4.
[<Fact>]
let ``scenario 4: a virtual address, nominal capital and a past-due verification are shown, in neutral words``
    ()
    : Task =
    task {
        let body =
            profile
                "00000031"
                "EXAMPLE VENTURES LTD"
                ""","registered_office_address":{"address_line_1":"1 Virtual Row","locality":"London","postal_code":"EC1A 1AA"}"""

        let director =
            """{"name":"DOE, Jane","officer_role":"director","appointed_on":"2010-05-06",
                "identity_verification_details":{"appointment_verification_statement_due_on":"2026-01-02"}}"""

        let newinc =
            """{"transaction_id":"T1","category":"incorporation","type":"NEWINC","date":"2020-01-01","description":"incorporation-company","description_values":{"capital":[{"currency":"GBP","figure":"1"}]}}"""

        use handler =
            new FakeHandler(companiesHouse (Map [ "00000031", (body, [ director ], [], [ newinc ]) ]))

        let sources =
            { chSources handler with
                KnownAddresses =
                    [
                        {
                            Source = "The host's provider list"
                            Dated = DateOnly(2026, 1, 1)
                            Addresses =
                                [
                                    {
                                        Text = "1 Virtual Row"
                                        Label = "a virtual office provider"
                                    }
                                ]
                        }
                    ]
            }

        let! case = Investigation.openAsync sources options (start "00000031") ct

        let flagOf kind =
            case.Flags |> List.find (fun flag -> flag.Kind = kind)

        Assert.Contains("a virtual office provider", (flagOf FlagKind.VirtualAddress).Statement)
        Assert.Contains("1.00 GBP", (flagOf FlagKind.NominalCapital).Statement)
        Assert.Contains("due on 2026-01-02", (flagOf FlagKind.IdentityVerificationPastDue).Statement)

        // Past-due shows on the person, and is visible from the company through the officer link.
        let verification = flagOf FlagKind.IdentityVerificationPastDue
        Assert.NotEqual<string>(case.Subject, verification.On)

        Assert.Contains(
            CaseFile.flagsSeenFrom case.Subject case,
            (fun seen -> seen.Flag.Id = verification.Id && seen.Through.Head.Kind = LinkKind.Officer)
        )

        for flag in case.Flags do
            let words = flag.Statement.ToLowerInvariant()

            for word in
                [
                    "fraud"
                    "suspicious"
                    "wrongdoing"
                    "shell"
                    "illegal"
                    "breach"
                    "criminal"
                    "sham"
                ] do
                Assert.DoesNotContain(word, words)

            Assert.NotEmpty flag.Source
    }

[<Fact>]
let ``a capital figure above the limit is a fact and no flag, and the limit is the host's`` () : Task =
    task {
        let filing (figure: string) =
            """{"transaction_id":"T1","category":"capital","type":"SH01","date":"2020-01-01","description_values":{"capital":[{"currency":"GBP","figure":"%FIGURE%"}]}}"""
                .Replace("%FIGURE%", figure)

        use handler =
            new FakeHandler(
                companiesHouse (Map [ "00000031", (profile "00000031" "X LTD" "", [], [], [ filing "5000" ]) ])
            )

        let! above =
            Investigation.openAsync (chSources handler) options (start "00000031") ct

        Assert.DoesNotContain(above.Flags, (fun flag -> flag.Kind = FlagKind.NominalCapital))

        Assert.Contains(
            above.Evidence,
            (fun item ->
                item.Facts
                |> List.exists (function
                    | Fact.Capital _ -> true
                    | _ -> false))
        )

        let! raised =
            Investigation.openAsync
                (chSources handler)
                { options with
                    NominalCapitalAtMost = 5000m
                }
                (start "00000031")
                ct

        Assert.Contains(raised.Flags, (fun flag -> flag.Kind = FlagKind.NominalCapital))
    }

[<Fact>]
let ``a capital figure that can't be read is left out, and stays in the raw answer`` () : Task =
    task {
        let filing =
            """{"transaction_id":"T1","category":"capital","type":"SH01","date":"2020-01-01","description_values":{"capital":[{"currency":"GBP","figure":"unknown"}]}}"""

        use handler =
            new FakeHandler(companiesHouse (Map [ "00000031", (profile "00000031" "X LTD" "", [], [], [ filing ]) ]))

        let! case = Investigation.openAsync (chSources handler) options (start "00000031") ct

        Assert.DoesNotContain(
            case.Evidence,
            (fun item ->
                item.Facts
                |> List.exists (function
                    | Fact.Capital _ -> true
                    | _ -> false))
        )

        Assert.Contains(case.Evidence, (fun item -> item.Topic = "filing history" && item.Raw.Value.Contains "unknown"))
    }

// Case study 5.

let private listDate = DateOnly(2026, 11, 6)

let private uk =
    let header =
        "Last Updated,Unique ID,OFSI Group ID,UN Reference Number,Name 6,Name 1,Name 2,Name 3,Name 4,Name 5,Name type,Alias strength,Title,Name non-latin script,Non-latin script type,Non-latin script language,Regime Name,Designation Type,Designation source,Sanctions Imposed,Other Information,UK Statement of Reasons,Address Line 1,Address Line 2,Address Line 3,Address Line 4,Address Line 5,Address Line 6,Address Postal Code,Address Country,Phone number,Website,Email address,Date Designated,D.O.B,Nationality(/ies),National Identifier number,National Identifier additional information,Passport number,Passport additional information,Position,Gender,Town of birth,Country of birth,Type of entity,Subsidiaries,Parent company,Business registration number (s),IMO number,Current owner/operator (s),Previous owner/operator (s),Current believed flag of ship,Previous flags,Type of ship,Tonnage of ship,Length of ship,Year Built,Hull identification number (HIN)"

    let row pid family given designation dob nationality =
        let cells = Array.create 58 ""
        cells[1] <- pid
        cells[4] <- family
        cells[5] <- given
        cells[10] <- "Primary name"
        cells[17] <- designation
        cells[34] <- dob
        cells[35] <- nationality
        String.concat "," cells

    String.concat
        "\n"
        [
            "Report Date: 06-Nov-2026"
            header
            row "ZED0001" "ZEDLAND PARTNERS" "" "Entity" "" ""
            row "ZED0002" "QUILLBY" "Ansel" "Individual" "1960" "Zedland"
        ]

let private screeningCase companyName officerName (owners: string list) statements =
    task {
        let body = profile "00000041" companyName ""

        let director =
            $$"""{"name":"{{officerName}}","officer_role":"director","date_of_birth":{"year":1960,"month":3},"nationality":"Zedland"}"""

        use handler =
            new FakeHandler(fun call ->
                if call.Target.Contains "persons-with-significant-control-statements" then
                    ok (pscsJson (List.length statements) 0 (if startIndexOf call = 0 then statements else []))
                else
                    companiesHouse (Map [ "00000041", (body, [ director ], owners, []) ]) call)

        return! Investigation.openAsync (chSources handler) options (start "00000041") ct
    }

[<Fact>]
let ``scenario 5: companies, officers and owners are screened, and a hit is a possible match for a person`` () : Task =
    task {
        let owner =
            """{"kind":"individual-person-with-significant-control","name":"Sam Rowan","natures_of_control":[]}"""

        let! case = screeningCase "ZEDLAND PARTNERS LTD" "QUILLBY, Ansel" [ owner ] []
        let screened = Investigation.screen [ ListText.Uk(uk, listDate) ] options case
        let screening = screened.Screenings.Head

        Assert.Contains("ZEDLAND PARTNERS LTD", screening.NamesScreened)
        Assert.Contains("QUILLBY, Ansel", screening.NamesScreened)
        Assert.Contains("Sam Rowan", screening.NamesScreened)

        let person =
            screening.PossibleMatches
            |> List.find (fun found -> found.ScreenedName = "QUILLBY, Ansel")

        Assert.Equal(DetailComparison.BothSidesMatch "1960", person.BirthYear)
        Assert.Equal(DetailComparison.BothSidesMatch "ZEDLAND", person.Country)
        Assert.NotEmpty person.ScoreReason

        let flags =
            screened.Flags
            |> List.filter (fun flag -> flag.Kind = FlagKind.PossibleSanctionsMatch)

        Assert.Equal(2, flags.Length)
        Assert.All(flags, (fun flag -> Assert.StartsWith("Possible match for review", flag.Statement)))

        // Each flag stands on the entity whose name it is, and the company sees its officer's through the link.
        Assert.Contains(flags, (fun flag -> flag.On = screened.Subject))
        Assert.Contains(flags, (fun flag -> flag.On <> screened.Subject))
    }

[<Fact>]
let ``scenario 5: a clean name result doesn't clear a company with unknown owners`` () : Task =
    task {
        let! case =
            screeningCase
                "HARBOUR FINCH LTD"
                "ROWAN, Sam"
                []
                [
                    """{"statement":"psc-exists-but-not-identified","notified_on":"2016-04-06"}"""
                ]

        let screened = Investigation.screen [ ListText.Uk(uk, listDate) ] options case
        let screening = screened.Screenings.Head

        Assert.Empty screening.PossibleMatches

        Assert.Contains(
            screening.ListsRead,
            (fun list -> list.List = SanctionsListName.UkFcdo && list.Dated = listDate)
        )

        // Who wasn't screened is said, and stays open on the whole case.
        Assert.Contains(screening.NamesNotScreened, (fun name -> name.Who.StartsWith "owners of"))

        let gaps =
            gapsOn "sanctions screening" screened
            |> List.filter (fun gap -> gap.Detail.StartsWith "Not screened")

        Assert.NotEmpty gaps
        Assert.All(gaps, (fun gap -> Assert.Equal(GapReach.WholeCase, gap.Reach)))
        Assert.NotEmpty(gapsOn "owners" screened)

        let text = (CaseExport.toText false screened).ToLowerInvariant()
        Assert.DoesNotContain("not sanctioned", text)
        Assert.Contains("which shows only what these names resembled", text)
        Assert.Contains("not screened: owners of", text)
    }

[<Fact>]
let ``scenario 5: every screening states lists not used, and the EU as needing entitlement`` () : Task =
    task {
        let! case = screeningCase "HARBOUR FINCH LTD" "ROWAN, Sam" [] []

        let screened =
            Investigation.screen [ ListText.Uk(uk, listDate); ListText.Un("broken", listDate) ] options case

        let gaps = gapsOn "sanctions screening" screened

        Assert.Contains(
            gaps,
            (fun gap -> gap.Kind = GapKind.NeedsEntitlement && gap.Detail.StartsWith "EuConsolidated")
        )

        Assert.Contains(
            gaps,
            (fun gap -> gap.Detail.StartsWith "UnConsolidated" && gap.Detail.Contains "couldn't be read")
        )

        Assert.Contains(
            gaps,
            (fun gap -> gap.Detail.StartsWith "OfacSdn" && gap.Detail.Contains "No text was supplied")
        )

        Assert.DoesNotContain(gaps, (fun gap -> gap.Detail = "No sanctions screening has been run"))
    }

[<Fact>]
let ``a later screening replaces the gaps of the one before, and keeps both results`` () : Task =
    task {
        let! case = screeningCase "HARBOUR FINCH LTD" "ROWAN, Sam" [] []
        let first = Investigation.screen [] options case
        Assert.Contains(gapsOn "sanctions screening" first, (fun gap -> gap.Detail.StartsWith "UkFcdo"))
        let second = Investigation.screen [ ListText.Uk(uk, listDate) ] options first
        Assert.DoesNotContain(gapsOn "sanctions screening" second, (fun gap -> gap.Detail.StartsWith "UkFcdo"))
        Assert.Equal(2, second.Screenings.Length)
    }

[<Fact>]
let ``a withheld owner has no name to screen, and that is said`` () : Task =
    task {
        let withheld =
            """{"kind":"super-secure-person-with-significant-control","natures_of_control":[]}"""

        let! case = screeningCase "HARBOUR FINCH LTD" "ROWAN, Sam" [ withheld ] []
        let screened = Investigation.screen [ ListText.Uk(uk, listDate) ] options case
        let screening = screened.Screenings.Head
        Assert.DoesNotContain("(name not given)", screening.NamesScreened)
        Assert.Contains(screening.NamesNotScreened, (fun name -> name.Reason.Contains "withholds"))
    }

[<Fact>]
let ``a vendor's sanctions Clear leaves the screening's own gaps open`` () : Task =
    task {
        let! case = screeningCase "HARBOUR FINCH LTD" "ROWAN, Sam" [] []
        let screened = Investigation.screen [ ListText.Uk(uk, listDate) ] options case

        let check =
            {
                Vendor = "Plaid"
                Product = "Monitor entity screening"
                Reference = "scr_1"
                CheckedAt = at
                OutcomeWords = "cleared"
                Hits = []
                Gaps = []
                Facts = []
                Raw = ValueNone
            }

        let withVendor = CaseFile.addVendorCheck screened.Subject check screened |> expectOk
        Assert.Equal<CaseGap list>(gapsOn "sanctions screening" screened, gapsOn "sanctions screening" withVendor)
    }

// GLEIF within an investigation.

let private gleifRecord (lei: string) (name: string) (number: string) (jurisdiction: string) =
    """{"type":"lei-records","id":"%LEI%","attributes":{"lei":"%LEI%","entity":{"legalName":{"name":"%NAME%"},"legalAddress":{"addressLines":["1 Road"],"city":"London","country":"GB","postalCode":"AB1"},"headquartersAddress":{"addressLines":["9 Tower"],"city":"Leeds","country":"GB","postalCode":"LS1"},"registeredAt":{"id":"RA1"},"registeredAs":"%NUMBER%","jurisdiction":"%JURISDICTION%","status":"ACTIVE"},"registration":{"status":"ISSUED"}}}"""
        .Replace("%LEI%", lei)
        .Replace("%NAME%", name)
        .Replace("%NUMBER%", number)
        .Replace("%JURISDICTION%", jurisdiction)

[<Literal>]
let private lei1 = "AAAAAAAAAAAAAAAAAA11"

[<Literal>]
let private lei2 = "BBBBBBBBBBBBBBBBBB22"

/// A GLEIF fake: by-number lookup, parents and exceptions as scripted.
let private gleif
    (byNumber: Map<string, string list>)
    (parents: Map<string, string>)
    (exceptions: Map<string, string>)
    (call: Call)
    =
    let target = Uri.UnescapeDataString call.Target
    let meta = """"meta":{"goldenCopy":{"publishDate":"2026-11-06T08:00:00Z"}}"""

    if target.Contains "filter[entity.registeredAs]=" then
        let number = target.Substring(target.IndexOf('=') + 1).Split('&')[0]
        let items = byNumber.TryFind number |> Option.defaultValue []
        ok ("{" + meta + ",\"data\":[" + String.concat "," items + "]}")
    elif target.Contains "-parent-reporting-exception" then
        let lei = target.Split('/')[4]

        match exceptions.TryFind lei with
        | Some reason -> ok ("{" + meta + ",\"data\":{\"attributes\":{\"reason\":\"" + reason + "\"}}}")
        | None -> notFound
    elif target.EndsWith "-parent" then
        let lei = target.Split('/')[4]

        match parents.TryFind lei with
        | Some record -> ok ("{" + meta + ",\"data\":" + record + "}")
        | None -> notFound
    else
        notFound

let private gleifSources (handler: FakeHandler) registries : CaseSources =
    { sourcesWith registries with
        CrossCheck =
            ValueSome(Sources.gleif (SourceConnection(handler, (fun () -> at), (fun _ _ -> Task.CompletedTask))))
    }

[<Fact>]
let ``GLEIF is kept as a cross-check, and its parent shows as a gap on the whole case when it can't be read``
    ()
    : Task =
    task {
        let register =
            ScriptedRegistry("UK", gb, Map [ "00000001", entry (baseCompany "00000001" "SUB LTD" gb) ])

        use handler =
            new FakeHandler(
                gleif
                    (Map [ "00000001", [ gleifRecord lei1 "SUB LTD" "00000001" "GB" ] ])
                    (Map [ lei1, gleifRecord lei2 "PARENT SARL" "B123" "LU" ])
                    Map.empty
            )

        let! case =
            Investigation.openAsync
                (gleifSources handler [ register ])
                options
                {
                    CaseId = "C"
                    Number = "00000001"
                    Jurisdiction = gb
                    NameGiven = "SUB LTD"
                }
                ct

        let subject = case.Entities |> List.find (fun entity -> entity.Key = case.Subject)
        Assert.Contains(EntityIdentity.Lei lei1, subject.Identities)

        let item =
            case.Evidence
            |> List.find (fun evidence -> evidence.Origin = EvidenceOrigin.CrossCheck)

        Assert.Equal("GLEIF (cross-check)", item.Source)
        Assert.Contains("not a register", item.Note.Value)
        Assert.Equal(ValueSome(DateOnly(2026, 11, 6)), item.AsOf)

        let link =
            case.Links |> List.find (fun candidate -> candidate.Kind = LinkKind.Parent)

        Assert.Equal(LinkProvenance.CrossCheck "GLEIF (cross-check)", link.Provenance)
        Assert.Equal("LU:B123", link.To)

        // Luxembourg has a declared route and no adapter: the parent is a whole-case gap saying what would close it.
        let gap = gapsOn "Luxembourg" case |> List.head
        Assert.Equal(GapKind.NeedsPaidContract, gap.Kind)
        Assert.Equal(GapReach.WholeCase, gap.Reach)
        Assert.NotEmpty gap.WouldClose
    }

[<Fact>]
let ``GLEIF's reported address differing from the register's is labelled or flagged by kind`` () : Task =
    task {
        let company =
            { baseCompany "00000001" "SUB LTD" gb with
                Addresses =
                    Available
                        [
                            {
                                Kind = AddressKind.RegisteredOffice
                                Address =
                                    {
                                        Lines = [ "5 Other Lane" ]
                                        Locality = ValueSome "York"
                                        Region = ValueNone
                                        PostalCode = ValueSome "YO1"
                                        Country = ValueNone
                                    }
                            }
                        ]
            }

        let register = ScriptedRegistry("UK", gb, Map [ "00000001", entry company ])

        use handler =
            new FakeHandler(
                gleif (Map [ "00000001", [ gleifRecord lei1 "SUB LTD" "00000001" "GB" ] ]) Map.empty Map.empty
            )

        let! case =
            Investigation.openAsync
                (gleifSources handler [ register ])
                options
                {
                    CaseId = "C"
                    Number = "00000001"
                    Jurisdiction = gb
                    NameGiven = "SUB LTD"
                }
                ct

        let found =
            Discrepancies.forEntity case case.Subject
            |> List.filter (fun item -> item.Field = DiscrepancyField.Address)

        Assert.Contains(found, (fun item -> item.Label = DiscrepancyLabel.Disagreement))
        Assert.Contains(found, (fun item -> item.Label <> DiscrepancyLabel.Disagreement))
    }

[<Fact>]
let ``a reporting exception and no parent at all are gaps, and several LEIs aren't picked from`` () : Task =
    task {
        let register =
            ScriptedRegistry("UK", gb, Map [ "00000001", entry (baseCompany "00000001" "SUB LTD" gb) ])

        let run handler =
            Investigation.openAsync
                (gleifSources handler [ register ])
                options
                {
                    CaseId = "C"
                    Number = "00000001"
                    Jurisdiction = gb
                    NameGiven = "SUB LTD"
                }
                ct

        use withException =
            new FakeHandler(
                gleif
                    (Map [ "00000001", [ gleifRecord lei1 "SUB LTD" "00000001" "GB" ] ])
                    Map.empty
                    (Map [ lei1, "NO_KNOWN_PERSON" ])
            )

        let! excepted = run withException
        Assert.Contains("NO_KNOWN_PERSON", (gapsOn "direct parent" excepted).Head.Detail)

        use withNothing =
            new FakeHandler(
                gleif (Map [ "00000001", [ gleifRecord lei1 "SUB LTD" "00000001" "GB" ] ]) Map.empty Map.empty
            )

        let! nothing = run withNothing
        Assert.Contains("neither a parent nor", (gapsOn "ultimate parent" nothing).Head.Detail)

        use withTwo =
            new FakeHandler(
                gleif
                    (Map
                        [
                            "00000001",
                            [
                                gleifRecord lei1 "SUB LTD" "00000001" "GB"
                                gleifRecord lei2 "SUB TWO LTD" "00000001" "GB"
                            ]
                        ])
                    Map.empty
                    Map.empty
            )

        let! two = run withTwo
        Assert.Contains("More than one", (gapsOn "LEI cross-check" two).Head.Detail)
        Assert.DoesNotContain(two.Evidence, (fun item -> item.Origin = EvidenceOrigin.CrossCheck))

        use failing = new FakeHandler(fun _ -> answer 500 "")
        let! failed = run failing
        Assert.Equal(GapKind.SourceFailed, (gapsOn "LEI cross-check" failed).Head.Kind)

        use noLei = new FakeHandler(gleif Map.empty Map.empty Map.empty)
        let! none = run noLei
        Assert.Empty(gapsOn "LEI cross-check" none)
    }

[<Fact>]
let ``leaving GLEIF out of the sources is said`` () : Task =
    task {
        let register =
            ScriptedRegistry("Zedland", zz, Map [ "1", entry (baseCompany "1" "X" zz) ])

        let! case = openOn [ register ] "1" zz
        Assert.Contains("wasn't among the sources", (gapsOn "LEI cross-check" case).Head.Detail)
    }

[<Fact>]
let ``a whole case round-trips through JSON after an investigation`` () : Task =
    task {
        use handler =
            new FakeHandler(companiesHouse (Map [ "FC000001", (overseasProfile, [], [], overseasFilings) ]))

        let! case = Investigation.openAsync (chSources handler) options (start "FC000001") ct
        let screened = Investigation.screen [ ListText.Uk(uk, listDate) ] options case
        Assert.Equal(Ok screened, CaseJson.ofJson (CaseJson.toJson screened))
    }

// Every name held is screened, and what isn't says so.

/// An OFAC file that lists these names as entities.
let private listing (names: string list) =
    let rows =
        names
        |> List.mapi (fun index name ->
            $"{index + 1},\"{name}\",-0- ,\"ZED\",-0- ,-0- ,-0- ,-0- ,-0- ,-0- ,-0- ,-0- \r\n")
        |> String.concat ""

    ListText.OfacSdn(rows + "\u001a", ValueNone, ValueNone, listDate)

let private raisedFor (case: CaseFile) =
    case.Screenings
    |> List.last
    |> _.PossibleMatches
    |> List.map _.ScreenedName
    |> List.distinct

let private notScreenedIn (case: CaseFile) =
    case.Screenings |> List.last |> _.NamesNotScreened |> List.map _.Who

[<Fact>]
let ``a stated owner's name and the customer's are screened beside the register's`` () : Task =
    task {
        let register =
            ScriptedRegistry(
                "Zedland",
                zz,
                Map
                    [
                        "1",
                        owned
                            [ ownerOf (registration "STATED OWNER WORKS" "2" "ZZ") ]
                            (baseCompany "1" "ZEDCO WORKS" zz)
                        "2", entry (baseCompany "2" "RESOLVED OWNER MILLS" zz)
                    ]
            )

        let! case = openOn [ register ] "1" zz

        let screened =
            Investigation.screen [ listing [ "STATED OWNER WORKS"; "AS THE CUSTOMER GAVE IT" ] ] options case

        Assert.Equal<string list>(
            [ "AS THE CUSTOMER GAVE IT"; "STATED OWNER WORKS" ],
            raisedFor screened |> List.map _.ToUpperInvariant() |> List.sort
        )

        let names = (List.last screened.Screenings).NamesScreened

        for name in
            [
                "ZEDCO WORKS"
                "RESOLVED OWNER MILLS"
                "STATED OWNER WORKS"
                "As the customer gave it"
            ] do
            Assert.Contains(name, names)

        let flags =
            screened.Flags
            |> List.filter (fun flag -> flag.Kind = FlagKind.PossibleSanctionsMatch)

        Assert.Equal(2, flags.Length)
    }

[<Fact>]
let ``an owner's name with no legal name is screened after the register answers with another`` () : Task =
    task {
        let owner =
            { ownerOf
                  { registration "" "2" "ZZ" with
                      LegalName = ValueNone
                  } with
                Name = ValueSome "STATED ONLY HERE"
            }

        let register =
            ScriptedRegistry(
                "Zedland",
                zz,
                Map
                    [
                        "1", owned [ owner ] (baseCompany "1" "ZEDCO WORKS" zz)
                        "2", entry (baseCompany "2" "RESOLVED OWNER MILLS" zz)
                    ]
            )

        let! case = openOn [ register ] "1" zz
        let screened = Investigation.screen [ listing [ "STATED ONLY HERE" ] ] options case
        Assert.Equal<string list>([ "STATED ONLY HERE" ], raisedFor screened)
    }

[<Fact>]
let ``a former name on the register reaches the screening`` () : Task =
    task {
        let body =
            profile
                "00000041"
                "NEW ZEDCO LTD"
                ""","previous_company_names":[{"name":"FORMER ZEDCO WORKS LTD","effective_from":"2001-01-01","ceased_on":"2010-01-01"}]"""

        use handler =
            new FakeHandler(companiesHouse (Map [ "00000041", (body, [], [], []) ]))

        let! case = Investigation.openAsync (chSources handler) options (start "00000041") ct

        let screened =
            Investigation.screen [ listing [ "FORMER ZEDCO WORKS" ] ] options case

        Assert.Equal<string list>([ "FORMER ZEDCO WORKS LTD" ], raisedFor screened)
    }

[<Fact>]
let ``LEI names are screened`` () : Task =
    task {
        let register =
            ScriptedRegistry("UK", gb, Map [ "00000001", entry (baseCompany "00000001" "SUB LTD" gb) ])

        let record =
            (gleifRecord lei1 "LEI ZEDCO WORKS" "00000001" "GB")
                .Replace("\"registeredAs\"", "\"otherNames\":[{\"name\":\"LEI OTHER MILLS\"}],\"registeredAs\"")

        use handler =
            new FakeHandler(gleif (Map [ "00000001", [ record ] ]) Map.empty Map.empty)

        let! case =
            Investigation.openAsync
                (gleifSources handler [ register ])
                options
                {
                    CaseId = "C"
                    Number = "00000001"
                    Jurisdiction = gb
                    NameGiven = "SUB LTD"
                }
                ct

        let screened =
            Investigation.screen [ listing [ "LEI ZEDCO WORKS"; "LEI OTHER MILLS" ] ] options case

        Assert.Equal<string list>([ "LEI OTHER MILLS"; "LEI ZEDCO WORKS" ], raisedFor screened |> List.sort)
    }

[<Fact>]
let ``a name with nothing to compare appears under not screened`` () : Task =
    task {
        let register =
            ScriptedRegistry("Zedland", zz, Map [ "1", entry (baseCompany "1" "ZEDCO WORKS" zz) ])

        let! case =
            Investigation.openAsync
                (sourcesWith [ register ])
                options
                {
                    CaseId = "C"
                    Number = "1"
                    Jurisdiction = zz
                    NameGiven = "Ltd"
                }
                ct

        let screened = Investigation.screen [ listing [ "SOMEONE ELSE" ] ] options case
        Assert.Contains("Ltd", notScreenedIn screened)
    }

let private partyOfficer name kind : Officer =
    {
        Name = name
        Kind = kind
        Role = OfficerRole.Director
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
let ``a placeholder for a missing name isn't screened, and the party is under not screened`` () : Task =
    task {
        let company =
            { entry (baseCompany "1" "ZEDCO WORKS" zz) with
                Officers = Ok(Available [ partyOfficer "(name not published)" PartyKind.LegalEntity ])
            }

        let register = ScriptedRegistry("Zedland", zz, Map [ "1", company ])
        let! case = openOn [ register ] "1" zz

        let screened =
            Investigation.screen [ listing [ "NAME NOT PUBLISHED" ] ] options case

        let screening = List.last screened.Screenings
        Assert.DoesNotContain("(name not published)", screening.NamesScreened)
        Assert.Empty screening.PossibleMatches

        let placeholder =
            case.Entities |> List.find (fun entity -> entity.Name = "(name not published)")

        Assert.Contains(placeholder.Key, notScreenedIn screened)
    }

[<Fact>]
let ``a failed officers read is under not screened`` () : Task =
    task {
        let company =
            { entry (baseCompany "1" "ZEDCO WORKS" zz) with
                Officers = Error(RegistryFailure.Unavailable "down")
            }

        let register = ScriptedRegistry("Zedland", zz, Map [ "1", company ])
        let! case = openOn [ register ] "1" zz
        let screened = Investigation.screen [ listing [ "SOMEONE ELSE" ] ] options case
        Assert.Contains(notScreenedIn screened, (fun who -> who.StartsWith "officers of"))
        Assert.Contains(gapsOn "sanctions screening" screened, (fun gap -> gap.Detail.Contains "officers of"))
    }

[<Fact>]
let ``a failed LEI read is under not screened`` () : Task =
    task {
        let register =
            ScriptedRegistry("UK", gb, Map [ "00000001", entry (baseCompany "00000001" "SUB LTD" gb) ])

        use handler = new FakeHandler(fun _ -> { ok "" with Status = 503 })

        let! case =
            Investigation.openAsync
                (gleifSources handler [ register ])
                options
                {
                    CaseId = "C"
                    Number = "00000001"
                    Jurisdiction = gb
                    NameGiven = "SUB LTD"
                }
                ct

        let screened = Investigation.screen [ listing [ "SOMEONE ELSE" ] ] options case
        Assert.Contains(notScreenedIn screened, (fun who -> who.StartsWith "LEI cross-check of"))
    }

[<Fact>]
let ``an officer a source names with no entity of its own is screened`` () : Task =
    task {
        let register =
            ScriptedRegistry("Zedland", zz, Map [ "1", entry (baseCompany "1" "ZEDCO WORKS" zz) ])

        let! case = openOn [ register ] "1" zz

        let check =
            {
                Vendor = "Experian"
                Product = "KYB"
                Reference = "REF-1"
                CheckedAt = at
                OutcomeWords = "Clear"
                Hits = []
                Gaps = []
                Facts = [ Fact.Officer "VENDOR ONLY OFFICER" ]
                Raw = ValueNone
            }

        let withVendor = CaseFile.addVendorCheck case.Subject check case |> expectOk

        let screened =
            Investigation.screen [ listing [ "VENDOR ONLY OFFICER" ] ] options withVendor

        Assert.Equal<string list>([ "VENDOR ONLY OFFICER" ], raisedFor screened)
    }

// Whatever comes after a screening isn't covered by it.

[<Fact>]
let ``an entity declared after a screening is a gap until the next screening`` () : Task =
    task {
        let register =
            ScriptedRegistry("Zedland", zz, Map [ "1", entry (baseCompany "1" "ZEDCO WORKS" zz) ])

        let! case = openOn [ register ] "1" zz
        let screened = Investigation.screen [ listing [ "SOMEONE ELSE" ] ] options case
        Assert.DoesNotContain(gapsOn "sanctions screening" screened, (fun gap -> gap.Detail.Contains "LATE PARTY"))

        let late =
            CaseFile.entityOf
                (EntityIdentity.NameOnly("LATE PARTY", "a person"))
                "LATE PARTY"
                PartyKind.LegalEntity
                ValueNone

        let declared =
            screened
            |> CaseFile.declareLink screened.Subject late LinkKind.Owner ValueNone officer
            |> expectOk

        let gap =
            gapsOn "sanctions screening" declared
            |> List.find (fun gap -> gap.Detail.Contains "LATE PARTY")

        Assert.Equal(ValueSome late.Key, gap.About)
        Assert.Equal(GapKind.NotChecked, gap.Kind)

        let again = Investigation.screen [ listing [ "SOMEONE ELSE" ] ] options declared
        Assert.DoesNotContain(gapsOn "sanctions screening" again, (fun gap -> gap.Detail.Contains "LATE PARTY"))
    }

[<Fact>]
let ``entities read by following after a screening are gaps, one each`` () : Task =
    task {
        let register =
            ScriptedRegistry(
                "Zedland",
                zz,
                Map
                    [
                        "1", entry (baseCompany "1" "ZEDCO WORKS" zz)
                        "2",
                        owned
                            [ ownerOf (registration "FAR OWNER MILLS" "3" "ZZ") ]
                            (baseCompany "2" "FOLLOWED MILLS" zz)
                        "3", entry (baseCompany "3" "FAR OWNER MILLS" zz)
                    ]
            )

        let sources = sourcesWith [ register ]
        let! case = openOn [ register ] "1" zz
        let screened = Investigation.screen [ listing [ "SOMEONE ELSE" ] ] options case

        let late =
            CaseFile.entityOf
                (EntityIdentity.OnRegister("Zedland", zz, "2"))
                "DECLARED NAME"
                PartyKind.LegalEntity
                (ValueSome zz)

        let declared =
            screened
            |> CaseFile.declareLink screened.Subject late LinkKind.Owner ValueNone officer
            |> expectOk

        let! followed = Investigation.followAsync sources options late.Key "2" zz declared ct
        let details = gapsOn "sanctions screening" (expectOk followed) |> List.map _.Detail

        for name in [ "DECLARED NAME"; "FOLLOWED MILLS"; "FAR OWNER MILLS" ] do
            Assert.Single(details, (fun (detail: string) -> detail.Contains $"Not screened: {name}."))
            |> ignore
    }

[<Fact>]
let ``a list whose file states another date is a gap, and the export prints that date`` () : Task =
    task {
        let register =
            ScriptedRegistry("Zedland", zz, Map [ "1", entry (baseCompany "1" "ZEDCO WORKS" zz) ])

        let! case = openOn [ register ] "1" zz
        let stale = uk.Replace("Report Date: 06-Nov-2026", "Report Date: 11-Mar-2019")
        let screened = Investigation.screen [ ListText.Uk(stale, listDate) ] options case

        Assert.Contains(
            gapsOn "sanctions screening" screened,
            (fun gap -> gap.Detail.Contains "2019-03-11" && gap.Detail.Contains "2026-11-06")
        )

        Assert.Contains("the file states 2019-03-11", CaseExport.toText false screened)

        let current = Investigation.screen [ ListText.Uk(uk, listDate) ] options case
        Assert.DoesNotContain(gapsOn "sanctions screening" current, (fun gap -> gap.Detail.Contains "the file states"))
        Assert.Contains("the file states 2026-11-06", CaseExport.toText false current)
    }

[<Fact>]
let ``a list below the screening's minimum isn't used`` () : Task =
    task {
        let register =
            ScriptedRegistry("Zedland", zz, Map [ "1", entry (baseCompany "1" "ZEDCO WORKS" zz) ])

        let! case = openOn [ register ] "1" zz

        let defaults =
            { options with
                ListMinimums = ListMinimums.defaults
            }

        let screened = Investigation.screen [ ListText.Uk(uk, listDate) ] defaults case
        let screening = List.last screened.Screenings
        Assert.Empty screening.ListsRead

        Assert.Contains(
            screening.ListsNotUsed,
            (fun list -> list.List = SanctionsListName.UkFcdo && list.Reason.Contains "below the minimum")
        )
    }

// A country that can't be placed isn't followed.

[<Theory; InlineData("New South Wales", ""); InlineData("New South Wales", "Companies House")>]
let ``an unplaceable country stays a gap, and no unrelated company with that number replaces the owner``
    (country: string, place: string)
    : Task =
    task {
        let owner =
            ownerOf
                { registration "FAR OWNER PTY" "00000002" country with
                    Register = if place = "" then ValueNone else ValueSome place
                }

        let register =
            ScriptedRegistry(
                "UK",
                gb,
                Map
                    [
                        "00000001", owned [ owner ] (baseCompany "00000001" "SUB LTD" gb)
                        "00000002", entry (baseCompany "00000002" "UNRELATED UK LTD" gb)
                    ]
            )

        let! case = openOn [ register ] "00000001" gb
        Assert.Equal<string list>([ "00000001" ], register.Calls)
        Assert.DoesNotContain(case.Entities, (fun entity -> entity.Name = "UNRELATED UK LTD"))
        let gap = gapsOn "FAR OWNER PTY" case |> List.exactlyOne
        Assert.Equal(GapKind.NoRoute, gap.Kind)
        Assert.Contains("New South Wales", gap.Detail)
    }

// A vendor's check closes only what it is about.

let private vendorCheckOn (reference: string) =
    {
        Vendor = "Experian"
        Product = "KYB"
        Reference = reference
        CheckedAt = at
        OutcomeWords = "Clear"
        Hits = []
        Gaps = []
        Facts = []
        Raw = ValueNone
    }

[<Fact>]
let ``a vendor check on one person leaves the case's vendor gap open, and one on the subject closes it`` () : Task =
    task {
        let company =
            { entry (baseCompany "1" "ZEDCO WORKS" zz) with
                Officers = Ok(Available [ partyOfficer "ANSEL ROWAN" PartyKind.Individual ])
            }

        let register = ScriptedRegistry("Zedland", zz, Map [ "1", company ])
        let! case = openOn [ register ] "1" zz
        let person = case.Entities |> List.find (fun entity -> entity.Name = "ANSEL ROWAN")

        let onPerson =
            CaseFile.addVendorCheck person.Key (vendorCheckOn "REF-1") case |> expectOk

        Assert.Single(gapsOn "vendor check" onPerson) |> ignore

        let onSubject =
            CaseFile.addVendorCheck case.Subject (vendorCheckOn "REF-2") onPerson
            |> expectOk

        Assert.Empty(gapsOn "vendor check" onSubject)
    }

// A GLEIF record of another jurisdiction isn't attached.

[<Theory; InlineData("US-DE"); InlineData("US")>]
let ``a GLEIF record of another or no state stays a possible match, and isn't attached`` (stated: string) : Task =
    task {
        let register =
            ScriptedRegistry("Texas", usTx, Map [ "1", entry (baseCompany "1" "ZEDCO WORKS" usTx) ])

        use handler =
            new FakeHandler(
                gleif (Map [ "1", [ gleifRecord lei1 "OTHER STATE WORKS" "1" stated ] ]) Map.empty Map.empty
            )

        let! case =
            Investigation.openAsync
                (gleifSources handler [ register ])
                options
                {
                    CaseId = "C"
                    Number = "1"
                    Jurisdiction = usTx
                    NameGiven = "ZEDCO WORKS"
                }
                ct

        let subject = case.Entities |> List.find (fun entity -> entity.Key = case.Subject)
        Assert.DoesNotContain(EntityIdentity.Lei lei1, subject.Identities)
        Assert.DoesNotContain(case.Evidence, (fun evidence -> evidence.Origin = EvidenceOrigin.CrossCheck))
        let gap = gapsOn "LEI cross-check" case |> List.exactlyOne
        Assert.Contains("Possible LEI match", gap.Detail)
        Assert.Contains(lei1, gap.Detail)
    }

[<Fact>]
let ``a GLEIF record in the state asked is attached`` () : Task =
    task {
        let register =
            ScriptedRegistry("Texas", usTx, Map [ "1", entry (baseCompany "1" "ZEDCO WORKS" usTx) ])

        use handler =
            new FakeHandler(gleif (Map [ "1", [ gleifRecord lei1 "ZEDCO WORKS" "1" "US-TX" ] ]) Map.empty Map.empty)

        let! case =
            Investigation.openAsync
                (gleifSources handler [ register ])
                options
                {
                    CaseId = "C"
                    Number = "1"
                    Jurisdiction = usTx
                    NameGiven = "ZEDCO WORKS"
                }
                ct

        let subject = case.Entities |> List.find (fun entity -> entity.Key = case.Subject)
        Assert.Contains(EntityIdentity.Lei lei1, subject.Identities)
    }

[<Fact>]
let ``a GLEIF match among more records than were read isn't attached, and the records not read are a gap`` () : Task =
    task {
        let register =
            ScriptedRegistry("Texas", usTx, Map [ "1", entry (baseCompany "1" "ZEDCO WORKS" usTx) ])

        let page (first: bool) =
            let others =
                List.init 99 (fun index ->
                    gleifRecord $"""CCCCCCCCCCCCCCC{(if first then "1" else "2")}%04d{index}""" "OTHER" "1" "FR")

            let items =
                if first then
                    gleifRecord lei1 "ZEDCO WORKS" "1" "US-TX" :: others
                else
                    gleifRecord "DDDDDDDDDDDDDDDDDD99" "OTHER" "1" "FR" :: others

            ok (
                """{"meta":{"goldenCopy":{"publishDate":"2026-11-06T08:00:00Z"},"pagination":{"total":10395}},"data":["""
                + String.concat "," items
                + "]}"
            )

        use handler =
            new FakeHandler(fun call ->
                let target = Uri.UnescapeDataString call.Target

                if target.Contains "filter[entity.registeredAs]=" then
                    page (target.Contains "page[number]=1")
                else
                    gleif Map.empty Map.empty Map.empty call)

        let! case =
            Investigation.openAsync
                (gleifSources handler [ register ])
                options
                {
                    CaseId = "C"
                    Number = "1"
                    Jurisdiction = usTx
                    NameGiven = "ZEDCO WORKS"
                }
                ct

        let subject = case.Entities |> List.find (fun entity -> entity.Key = case.Subject)
        Assert.DoesNotContain(EntityIdentity.Lei lei1, subject.Identities)
        Assert.DoesNotContain(case.Evidence, (fun evidence -> evidence.Origin = EvidenceOrigin.CrossCheck))
        let gap = gapsOn "LEI cross-check" case |> List.exactlyOne
        Assert.Contains("10195 more LEI records", gap.Detail)
    }

[<Fact>]
let ``an establishment that names no company it belongs to is a home record gap on the whole case`` () : Task =
    task {
        let office =
            { baseCompany "1" "ZEDCO REPRESENTATIVE OFFICE" zz with
                Role = RecordRole.Establishment
            }

        let register = ScriptedRegistry("Zedland Register", zz, Map [ "1", entry office ])
        let! case = openOn [ register ] "1" zz
        let gap = gapsOn "home record" case |> List.exactlyOne
        Assert.Equal(GapReach.WholeCase, gap.Reach)
    }

[<Fact>]
let ``a register answering with a number of another scheme keeps it, not compared`` () : Task =
    task {
        let register =
            ScriptedRegistry(
                "Zedland Tax Register",
                zz,
                Map [ "004085616", entry (baseCompany "51004085616" "ZEDCO" zz) ]
            )

        let! case = openOn [ register ] "004085616" zz
        let facts = case.Evidence |> List.collect _.Facts
        Assert.Contains(Fact.Other("number on this register", "51004085616"), facts)
        Assert.DoesNotContain(Fact.RegistrationNumber "51004085616", facts)

        let same =
            ScriptedRegistry("Zedland Register", zz, Map [ "0001", entry (baseCompany "1" "ZEDCO" zz) ])

        let! sameCase = openOn [ same ] "0001" zz
        Assert.Contains(Fact.RegistrationNumber "1", sameCase.Evidence |> List.collect _.Facts)
    }

[<Fact>]
let ``two owners with no name are two entities, each with its own link`` () : Task =
    task {
        let unnamed =
            { ownerOf (registration "X" "9" "ZZ") with
                Name = ValueNone
                Kind = PartyKind.Withheld
                Entity = ValueNone
            }

        let register =
            ScriptedRegistry(
                "Zedland Register",
                zz,
                Map [ "1", owned [ unnamed; unnamed ] (baseCompany "1" "ZEDCO" zz) ]
            )

        let! case = openOn [ register ] "1" zz
        let owners = case.Links |> List.filter (fun link -> link.Kind = LinkKind.Owner)
        Assert.Equal(2, owners.Length)
        Assert.Equal(2, owners |> List.map _.To |> List.distinct |> List.length)
    }
