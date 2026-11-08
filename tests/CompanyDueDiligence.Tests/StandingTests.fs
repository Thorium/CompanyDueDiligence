module StandingTests

open Xunit
open CompanyDueDiligence
open CompanyDueDiligence.CompaniesHouse

let private profile status detail : CompanyProfile =
    {
        CompanyNumber = "00000006"
        CompanyName = "A"
        Type = ValueNone
        Status = status
        StatusDetail = detail
        Jurisdiction = ValueNone
        CreatedOn = ValueNone
        CeasedOn = ValueNone
        HasInsolvencyHistory = ValueSome false
        HasBeenLiquidated = ValueSome false
        InsolvencyLink = ValueNone
        HasCharges = ValueSome false
        RegisteredOfficeInDispute = ValueSome false
        UndeliverableRegisteredOffice = ValueSome false
        AccountsOverdue = ValueSome false
        ConfirmationStatementOverdue = ValueSome false
        AnnualReturnOverdue = ValueNone
        SicCodes = []
        PreviousNames = []
        RegisteredOffice = ValueNone
        Foreign = ValueNone
        Branch = ValueNone
        ExternalRegistrationNumber = ValueNone
        ServiceAddress = ValueNone
        Raw = "{}"
    }

let private standing status detail =
    CompanyProfile.standing (profile status detail)

[<Fact>]
let ``active with nothing against its status is the only active standing`` () =
    Assert.Equal(CompanyStanding.Active, standing (ValueSome "active") ValueNone)
    Assert.Equal(CompanyStanding.Active, standing (ValueSome "active") (ValueSome ""))
    Assert.Equal(CompanyStanding.Active, standing (ValueSome "active") (ValueSome "active"))
    Assert.True(CompanyStanding.isActive (standing (ValueSome "active") ValueNone))

[<Fact>]
let ``an active company proposed for striking off isn't in active standing`` () =
    let result =
        standing (ValueSome "active") (ValueSome "active-proposal-to-strike-off")

    Assert.Equal(CompanyStanding.ActiveWithConcern "proposal to strike off", result)
    Assert.False(CompanyStanding.isActive result)

let private active = profile (ValueSome "active") ValueNone

[<Fact>]
let ``a profile with every flag false has no concerns`` () =
    Assert.Equal<CompanyConcerns>({ Stated = []; Unconfirmed = [] }, CompanyProfile.concerns active)

let private concernCases: (CompanyConcern * CompanyProfile * CompanyProfile) list =
    [
        CompanyConcern.AccountsOverdue,
        { active with
            AccountsOverdue = ValueSome true
        },
        { active with
            AccountsOverdue = ValueNone
        }

        CompanyConcern.AnnualFilingOverdue,
        { active with
            ConfirmationStatementOverdue = ValueSome true
        },
        { active with
            ConfirmationStatementOverdue = ValueNone
        }

        CompanyConcern.RegisteredOfficeDisputed,
        { active with
            RegisteredOfficeInDispute = ValueSome true
        },
        { active with
            RegisteredOfficeInDispute = ValueNone
        }

        CompanyConcern.RegisteredOfficeUndeliverable,
        { active with
            UndeliverableRegisteredOffice = ValueSome true
        },
        { active with
            UndeliverableRegisteredOffice = ValueNone
        }

        CompanyConcern.InsolvencyHistory,
        { active with
            HasInsolvencyHistory = ValueSome true
        },
        { active with
            HasInsolvencyHistory = ValueNone
        }
    ]

[<Fact>]
let ``each flag that is true is stated, and each one missing is unconfirmed rather than clear`` () =
    for concern, stated, missing in concernCases do
        Assert.Equal<CompanyConcerns>(
            {
                Stated = [ concern ]
                Unconfirmed = []
            },
            CompanyProfile.concerns stated
        )

        Assert.Equal<CompanyConcerns>(
            {
                Stated = []
                Unconfirmed = [ concern ]
            },
            CompanyProfile.concerns missing
        )

[<Fact>]
let ``a flag doesn't change legal standing`` () =
    for _, stated, missing in concernCases do
        Assert.Equal(CompanyStanding.Active, CompanyProfile.standing stated)
        Assert.Equal(CompanyStanding.Active, CompanyProfile.standing missing)

[<Fact>]
let ``being liquidated, or an insolvency link, is an insolvency history whatever the other flag says`` () =
    let history =
        {
            Stated = [ CompanyConcern.InsolvencyHistory ]
            Unconfirmed = []
        }

    Assert.Equal<CompanyConcerns>(
        history,
        CompanyProfile.concerns
            { active with
                HasBeenLiquidated = ValueSome true
            }
    )

    Assert.Equal<CompanyConcerns>(
        history,
        CompanyProfile.concerns
            { active with
                InsolvencyLink = ValueSome "/company/00000006/insolvency"
            }
    )

    Assert.Equal<CompanyConcerns>(
        history,
        CompanyProfile.concerns
            { active with
                HasInsolvencyHistory = ValueNone
                HasBeenLiquidated = ValueSome true
            }
    )

[<Fact>]
let ``only one insolvency flag saying no doesn't clear it`` () =
    Assert.Equal<CompanyConcern list>(
        [ CompanyConcern.InsolvencyHistory ],
        (CompanyProfile.concerns
            { active with
                HasBeenLiquidated = ValueNone
            })
            .Unconfirmed
    )

[<Fact>]
let ``an overdue annual return is an overdue annual filing`` () =
    Assert.Equal<CompanyConcern list>(
        [ CompanyConcern.AnnualFilingOverdue ],
        (CompanyProfile.concerns
            { active with
                AnnualReturnOverdue = ValueSome true
            })
            .Stated
    )

    Assert.Equal<CompanyConcern list>(
        [ CompanyConcern.AnnualFilingOverdue ],
        (CompanyProfile.concerns
            { active with
                ConfirmationStatementOverdue = ValueNone
                AnnualReturnOverdue = ValueSome true
            })
            .Stated
    )

[<Fact>]
let ``an annual return in time settles it where there is no confirmation statement`` () =
    Assert.Equal<CompanyConcerns>(
        { Stated = []; Unconfirmed = [] },
        CompanyProfile.concerns
            { active with
                ConfirmationStatementOverdue = ValueNone
                AnnualReturnOverdue = ValueSome false
            }
    )

[<Theory>]
[<InlineData("liquidation")>]
[<InlineData("receivership")>]
[<InlineData("administration")>]
[<InlineData("voluntary-arrangement")>]
[<InlineData("insolvency-proceedings")>]
let ``insolvency statuses read as in insolvency`` (status: string) =
    Assert.Equal(CompanyStanding.InInsolvency status, standing (ValueSome status) ValueNone)
    Assert.False(CompanyStanding.isActive (standing (ValueSome status) ValueNone))

[<Fact>]
let ``dissolved, closed and registered statuses aren't good`` () =
    Assert.Equal(CompanyStanding.Dissolved, standing (ValueSome "dissolved") ValueNone)

    Assert.Equal(
        CompanyStanding.Dissolved,
        standing (ValueSome "dissolved") (ValueSome "petition-to-restore-dissolved")
    )

    for status in [ "closed"; "converted-closed"; "removed" ] do
        Assert.Equal(CompanyStanding.Closed status, standing (ValueSome status) ValueNone)

    for status in [ "open"; "registered" ] do
        Assert.Equal(CompanyStanding.Registered(ValueSome status), standing (ValueSome status) ValueNone)
        Assert.False(CompanyStanding.isActive (standing (ValueSome status) ValueNone))

[<Theory>]
[<InlineData("something-new")>]
[<InlineData("Active")>]
[<InlineData("ACTIVE")>]
[<InlineData(" active")>]
[<InlineData("")>]
let ``an unknown status is Unknown, never good`` (status: string) =
    let result = standing (ValueSome status) ValueNone
    Assert.Equal(CompanyStanding.Unknown(ValueSome status), result)
    Assert.False(CompanyStanding.isActive result)

[<Fact>]
let ``no status at all is Unknown`` () =
    Assert.Equal(CompanyStanding.Unknown ValueNone, standing ValueNone ValueNone)
    Assert.False(CompanyStanding.isActive (standing ValueNone ValueNone))

[<Theory; InlineData("transferred-from-uk"); InlineData("something-new")>]
let ``active with a detail that isn't understood is Unknown`` (detail: string) =
    let result = standing (ValueSome "active") (ValueSome detail)
    Assert.False(CompanyStanding.isActive result)

    match result with
    | CompanyStanding.Unknown _ -> ()
    | other -> failwith $"%A{other}"
