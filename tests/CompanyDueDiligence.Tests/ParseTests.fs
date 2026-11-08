/// Field reading, from answers shaped by the bundled spec. They aren't recorded live answers.
module ParseTests

open System
open System.Threading.Tasks
open Xunit
open CompanyDueDiligence
open CompanyDueDiligence.CompaniesHouse
open Fake

let private date (y: int) (m: int) (d: int) = ValueSome(DateOnly(y, m, d))

[<Fact>]
let ``a profile's fields are read, and absent flags say nothing`` () : Task =
    task {
        let body =
            """{"company_number":"SC123456","company_name":"ACME LTD","type":"ltd","company_status":"active","jurisdiction":"scotland",
                "date_of_creation":"2001-02-03","date_of_cessation":"2020-01-31","sic_codes":["62012"],"unknown_new_member":{"a":1},
                "has_charges":true,"registered_office_is_in_dispute":true,"accounts":{"next_accounts":{"overdue":true}},
                "confirmation_statement":{"overdue":true},
                "previous_company_names":[{"name":"OLD LTD","effective_from":"1999-01-01","ceased_on":"2000-01-01"}],
                "registered_office_address":{"care_of":"Ms A","premises":"1","address_line_1":"High St","locality":"Leith","postal_code":"EH6 1AA","country":"Scotland"}}"""

        use handler = new FakeHandler(fun _ -> ok body)
        let! result = Api.Companies.getAsync (clientWith handler) "SC123456" ct
        let profile = (expectOk result).Value
        Assert.Equal("ACME LTD", profile.CompanyName)
        Assert.Equal(ValueSome "ltd", profile.Type)
        Assert.Equal(date 2001 2 3, profile.CreatedOn)
        Assert.Equal(date 2020 1 31, profile.CeasedOn)
        Assert.Equal(ValueSome true, profile.HasCharges)
        Assert.Equal(ValueSome true, profile.RegisteredOfficeInDispute)
        Assert.Equal(ValueSome true, profile.AccountsOverdue)
        Assert.Equal(ValueSome true, profile.ConfirmationStatementOverdue)
        Assert.Equal(ValueNone, profile.HasInsolvencyHistory)
        Assert.Equal(ValueNone, profile.AnnualReturnOverdue)
        Assert.Equal<string list>([ "62012" ], profile.SicCodes)
        Assert.Equal("OLD LTD", profile.PreviousNames.Head.Name)
        let office = profile.RegisteredOffice.Value
        Assert.Equal<string list>([ "Ms A"; "1"; "High St" ], office.Lines)
        Assert.Equal(ValueSome "EH6 1AA", office.PostalCode)
        Assert.Contains("unknown_new_member", profile.Raw)
    }

[<Fact>]
let ``an unreadable optional date is ValueNone, kept in Raw`` () : Task =
    task {
        use handler =
            new FakeHandler(fun _ ->
                ok """{"company_number":"00000006","company_name":"A","date_of_creation":"soon"}""")

        let! result = Api.Companies.getAsync (clientWith handler) "00000006" ct
        let profile = (expectOk result).Value
        Assert.Equal(ValueNone, profile.CreatedOn)
        Assert.Contains("soon", profile.Raw)
    }

[<Theory>]
[<InlineData("""{"accounts":{"next_accounts":{"overdue":true}}}""", true)>]
[<InlineData("""{"accounts":{"overdue":true,"next_accounts":{"overdue":false}}}""", true)>]
[<InlineData("""{"accounts":{"overdue":false}}""", false)>]
[<InlineData("""{"accounts":{"next_accounts":{"overdue":false}}}""", false)>]
let ``accounts are overdue if either flag says so`` (accounts: string, expected: bool) : Task =
    task {
        let body =
            "{"
            + accounts.Substring(1, accounts.Length - 2)
            + """,'company_number':'00000006','company_name':'A'}""".Replace("'", "\"")

        use handler = new FakeHandler(fun _ -> ok body)
        let! result = Api.Companies.getAsync (clientWith handler) "00000006" ct
        Assert.Equal(ValueSome expected, (expectOk result).Value.AccountsOverdue)
    }

[<Theory>]
[<InlineData("""{"company_number":"00000006","company_name":"A"}""")>]
[<InlineData("""{"company_number":"00000006","company_name":"A","accounts":{}}""")>]
[<InlineData("""{"company_number":"00000006","company_name":"A","accounts":{"next_accounts":{}}}""")>]
[<InlineData("""{"company_number":"00000006","company_name":"A","accounts":{"overdue":null}}""")>]
let ``accounts with no overdue flag say nothing`` (body: string) : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok body)
        let! result = Api.Companies.getAsync (clientWith handler) "00000006" ct
        Assert.Equal(ValueNone, (expectOk result).Value.AccountsOverdue)
    }

[<Fact>]
let ``annual_return.overdue and the insolvency link are read`` () : Task =
    task {
        let body =
            """{"company_number":"00000006","company_name":"A","annual_return":{"overdue":true},"links":{"insolvency":"/company/00000006/insolvency"}}"""

        use handler = new FakeHandler(fun _ -> ok body)
        let! result = Api.Companies.getAsync (clientWith handler) "00000006" ct
        let profile = (expectOk result).Value
        Assert.Equal(ValueSome true, profile.AnnualReturnOverdue)
        Assert.Equal(ValueSome "/company/00000006/insolvency", profile.InsolvencyLink)
    }

[<Theory>]
[<InlineData("registered_office_is_in_dispute")>]
[<InlineData("undeliverable_registered_office_address")>]
[<InlineData("has_been_liquidated")>]
[<InlineData("has_charges")>]
let ``a flag that isn't a boolean is refused, not read as false`` (flag: string) : Task =
    task {
        let body = $$"""{"company_number":"00000006","company_name":"A","{{flag}}":"no"}"""

        use handler = new FakeHandler(fun _ -> ok body)

        match! Api.Companies.getAsync (clientWith handler) "00000006" ct with
        | Error(InvalidResponse answer) -> Assert.Contains(flag, answer.Reason)
        | other -> failwith $"%A{other}"
    }

[<Theory; InlineData("06/05/2011"); InlineData("")>]
let ``an end date that can't be read fails the answer, on each list`` (date: string) : Task =
    task {
        let check (name: string) (list: string -> Task<string>) =
            task {
                let! printed = list date
                Assert.Contains("InvalidResponse", printed)
                Assert.DoesNotContain("ValueSome [", printed)
                ignore name
            }

        let shown (result: Task<Result<'T, CompaniesHouseFailure>>) =
            task { let! r = result in return $"%A{r}" }

        let over (body: string) =
            clientWith (new FakeHandler(fun _ -> ok body))

        do!
            check "resigned_on" (fun date ->
                let item = $$"""{"name":"A","officer_role":"director","resigned_on":"{{date}}"}"""
                shown (Api.Officers.allAsync (over (officersJson 1 1 [ item ])) "00000006" ct))

        do!
            check "ceased_on" (fun date ->
                let item =
                    $$"""{"name":"A","kind":"individual-person-with-significant-control","ceased_on":"{{date}}"}"""

                shown (Api.Psc.allAsync (over (pscsJson 1 1 [ item ])) "00000006" ct))

        do!
            check "statement ceased_on" (fun date ->
                let item = $$"""{"statement":"psc-details-not-confirmed","ceased_on":"{{date}}"}"""
                shown (Api.PscStatements.allAsync (over (pscsJson 1 1 [ item ])) "00000006" ct))

        do!
            check "date_of_cessation" (fun date ->
                let body =
                    $$"""{"company_number":"00000006","company_name":"A","date_of_cessation":"{{date}}"}"""

                shown (Api.Companies.getAsync (over body) "00000006" ct))
    }

[<Theory>]
[<InlineData("""{"resigned_on":20110506,"name":"A","officer_role":"director"}""")>]
[<InlineData("""{"resigned_on":{"date":"2011-05-06"},"name":"A","officer_role":"director"}""")>]
let ``an end date of the wrong type fails the answer too`` (item: string) : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (officersJson 1 1 [ item ]))

        match! Api.Officers.allAsync (clientWith handler) "00000006" ct with
        | Error(InvalidResponse answer) -> Assert.Contains("resigned_on", answer.Reason)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a null end date is no end date`` () : Task =
    task {
        let item = """{"name":"A","officer_role":"director","resigned_on":null}"""

        use handler =
            new FakeHandler(fun call ->
                match startIndexOf call with
                | 0 -> ok (officersJson 1 0 [ item ])
                | _ -> ok (officersJson 1 0 []))

        let! result = Api.Officers.allAsync (clientWith handler) "00000006" ct
        Assert.Equal(ValueNone, (expectOk result).Value.Head.ResignedOn)
    }

[<Theory>]
[<InlineData("{}")>]
[<InlineData("""{"natures_of_control":[]}""")>]
[<InlineData("""{"kind":7,"name":8}""")>]
let ``a PSC with neither a kind nor a name is refused`` (item: string) : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (pscsJson 1 0 [ item ]))

        match! Api.Psc.allAsync (clientWith handler) "00000006" ct with
        | Error(InvalidResponse _) -> ()
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a corporate PSC's identification is read`` () : Task =
    task {
        let item =
            """{"kind":"corporate-entity-person-with-significant-control","name":"HOLDINGS BV","natures_of_control":[],
                "identification":{"legal_authority":"Dutch law","legal_form":"BV","place_registered":"Kamer van Koophandel","registration_number":"12345678","country_registered":"Netherlands"}}"""

        use handler = new FakeHandler(fun _ -> ok (pscsJson 1 0 [ item ]))
        let! result = Api.Psc.pageAsync (clientWith handler) "00000006" 0 1 ct

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
            (expectOk result).Value.Items.Head.Identification
        )
    }

[<Fact>]
let ``a registration number sent as a number is read`` () : Task =
    task {
        let item =
            """{"kind":"corporate-entity-person-with-significant-control","name":"A","identification":{"registration_number":123}}"""

        use handler = new FakeHandler(fun _ -> ok (pscsJson 1 0 [ item ]))
        let! result = Api.Psc.pageAsync (clientWith handler) "00000006" 0 1 ct
        Assert.Equal(ValueSome "123", (expectOk result).Value.Items.Head.Identification.Value.RegistrationNumber)
    }

let private verificationJson =
    """"identity_verification_details":{"anti_money_laundering_supervisory_bodies":["A BODY"],"appointment_verification_end_on":"2027-01-05",
       "appointment_verification_statement_date":"2026-01-01","appointment_verification_statement_due_on":"2026-02-02","appointment_verification_start_on":"2026-01-03",
       "authorised_corporate_service_provider_name":"A PROVIDER","identity_verified_on":"2025-12-25","preferred_name":"Jay"}"""

let private verification =
    {
        VerifiedOn = date 2025 12 25
        VerifiedBy = ValueSome "A PROVIDER"
        VerifierSupervisors = [ "A BODY" ]
        StatementPossibleFrom = date 2026 1 1
        StatementDueOn = date 2026 2 2
        StatementSuppliedOn = date 2026 1 3
        StatementRemovedOn = date 2027 1 5
        PreferredName = ValueSome "Jay"
    }

[<Fact>]
let ``a director's and a PSC's identity verification are read as published`` () : Task =
    task {
        let officer = $$"""{"name":"A","officer_role":"director",{{verificationJson}}}"""
        use officers = new FakeHandler(fun _ -> ok (officersJson 1 0 [ officer ]))
        let! result = Api.Officers.pageAsync (clientWith officers) "00000006" 0 1 ct
        Assert.Equal(ValueSome verification, (expectOk result).Value.Items.Head.IdentityVerification)

        let psc =
            $$"""{"name":"A","kind":"individual-person-with-significant-control",{{verificationJson}}}"""

        use pscs = new FakeHandler(fun _ -> ok (pscsJson 1 0 [ psc ]))
        let! result = Api.Psc.pageAsync (clientWith pscs) "00000006" 0 1 ct
        Assert.Equal(ValueSome verification, (expectOk result).Value.Items.Head.IdentityVerification)
    }

[<Fact>]
let ``no verification block is ValueNone, and a block of part is only that part`` () : Task =
    task {
        let items =
            [
                """{"name":"A","officer_role":"director"}"""
                """{"name":"B","officer_role":"director","identity_verification_details":{"appointment_verification_statement_due_on":"2026-02-02"}}"""
            ]

        use handler = new FakeHandler(fun _ -> ok (officersJson 2 0 items))
        let! result = Api.Officers.pageAsync (clientWith handler) "00000006" 0 2 ct

        match (expectOk result).Value.Items with
        | [ none; part ] ->
            Assert.Equal(ValueNone, none.IdentityVerification)
            Assert.Equal(ValueNone, part.IdentityVerification.Value.VerifiedOn)
            Assert.Equal(date 2026 2 2, part.IdentityVerification.Value.StatementDueOn)
        | other -> failwith $"%A{other}"
    }

[<Theory>]
[<InlineData("identity_verified_on")>]
[<InlineData("appointment_verification_statement_date")>]
[<InlineData("appointment_verification_statement_due_on")>]
[<InlineData("appointment_verification_start_on")>]
[<InlineData("appointment_verification_end_on")>]
let ``a verification date that can't be read fails the answer`` (field: string) : Task =
    task {
        let item =
            $$$"""{"name":"A","officer_role":"director","identity_verification_details":{"{{{field}}}":"soon"}}"""

        use handler = new FakeHandler(fun _ -> ok (officersJson 1 0 [ item ]))

        match! Api.Officers.pageAsync (clientWith handler) "00000006" 0 1 ct with
        | Error(InvalidResponse answer) -> Assert.Contains(field, answer.Reason)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``an officer's month and year of birth read as numbers or strings`` () : Task =
    task {
        let body =
            officersJson
                2
                1
                [
                    """{"name":"DOE, Jane","officer_role":"director","appointed_on":"2010-05-06","date_of_birth":{"month":3,"year":1970},
                      "links":{"officer":{"appointments":"/officers/abc/appointments"}}}"""
                    """{"name":"ROE, Rick","resigned_on":"2011-05-06","date_of_birth":{"month":"4","year":"1971"}}"""
                ]

        use handler = new FakeHandler(fun _ -> ok body)
        let! result = Api.Officers.pageAsync (clientWith handler) "00000006" 0 2 ct

        match (expectOk result).Value.Items with
        | [ jane; rick ] ->
            Assert.Equal(ValueSome { Year = 1970; Month = 3 }, jane.DateOfBirth)
            Assert.Equal(ValueSome "/officers/abc/appointments", jane.AppointmentsLink)
            Assert.Equal(ValueSome "abc", jane.OfficerId)
            Assert.Equal(ValueNone, rick.OfficerId)
            Assert.Equal(ValueSome { Year = 1971; Month = 4 }, rick.DateOfBirth)
            Assert.Equal(ValueNone, rick.Role)
        | other -> failwith $"%A{other}"
    }

[<Theory>]
[<InlineData("""{"classification":{"type":"charge-description","description":"A charge"},"particulars":{"type":"short-particulars","description":"All assets"}}""")>]
[<InlineData("""{"classification":[{"type":"charge-description","description":"A charge"}],"particulars":[{"type":"short-particulars","description":"All assets"}]}""")>]
let ``a charge reads its object fields as an object or, as the spec types them, a list`` (item: string) : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (listJson "total_count" 1 [ item ]))
        let! result = Api.Charges.pageAsync (clientWith handler) "00000006" 0 1 ct
        let charge = (expectOk result).Value.Items.Head
        Assert.Equal(ValueSome "A charge", charge.Classification)
        Assert.Equal<string list>([ "All assets" ], charge.Particulars)
    }

[<Fact>]
let ``a charge's identifying fields are read`` () : Task =
    task {
        let item =
            """{"id":"c1","charge_code":"000000060001","charge_number":1,"status":"outstanding","created_on":"2015-01-02","satisfied_on":"2016-01-02",
                "persons_entitled":[{"name":"A BANK PLC"}]}"""

        use handler = new FakeHandler(fun _ -> ok (listJson "total_count" 1 [ item ]))
        let! result = Api.Charges.pageAsync (clientWith handler) "00000006" 0 1 ct
        let charge = (expectOk result).Value.Items.Head
        Assert.Equal(ValueSome 1, charge.ChargeNumber)
        Assert.Equal(ValueSome "outstanding", charge.Status)
        Assert.Equal(date 2016 1 2, charge.SatisfiedOn)
        Assert.Equal<string list>([ "A BANK PLC" ], charge.PersonsEntitled)
    }

[<Theory; InlineData("""{"address_line_1":"1 Road"}"""); InlineData("""[{"address_line_1":"1 Road"}]""")>]
let ``an insolvency case reads its practitioner's address either way`` (address: string) : Task =
    task {
        let body =
            """{"status":"liquidation","cases":[{"type":"compulsory-liquidation","number":7,"notes":["n"],
                  "dates":[{"type":"wound-up-on","date":"2020-03-04"}],
                  "practitioners":[{"name":"A Liquidator","role":"practitioner","appointed_on":"2020-01-02","address":ADDRESS}]}]}"""
                .Replace("ADDRESS", address)

        use handler = new FakeHandler(fun _ -> ok body)
        let! result = Api.Insolvency.getAsync (clientWith handler) "00000006" ct
        let insolvency = (expectOk result).Value
        Assert.Equal(ValueSome "liquidation", insolvency.Status)
        let case = insolvency.Cases.Head
        Assert.Equal(ValueSome "compulsory-liquidation", case.Type)
        Assert.Equal(ValueSome "7", case.Number)
        Assert.Equal(date 2020 3 4, case.Dates.Head.Date)
        Assert.Equal<string list>([ "1 Road" ], case.Practitioners.Head.Address.Value.Lines)
    }

[<Fact>]
let ``filing history reads a filing and its document id`` () : Task =
    task {
        let item =
            """{"transaction_id":"t1","category":"incorporation","type":"NEWINC","date":"2001-02-03","description":"incorporation-company","pages":12,
                "links":{"self":"/company/00000006/filing-history/t1","document_metadata":"https://document-api.company-information.service.gov.uk/document/doc-1"}}"""

        use handler = new FakeHandler(fun _ -> ok (listJson "total_count" 1 [ item ]))

        let! result =
            Api.FilingHistory.pageAsync (clientWith handler) "00000006" [ "incorporation" ] 0 1 ct

        let filing = (expectOk result).Value.Items.Head
        Assert.Equal(ValueSome "NEWINC", filing.Type)
        Assert.Equal(ValueSome 12, filing.Pages)
        Assert.Equal(ValueSome "doc-1", filing.DocumentId)
        Assert.Contains("category=incorporation", handler.Calls.Head.Target)
    }

[<Theory; InlineData("https://example.gov.uk/not-a-document/doc-1"); InlineData("/document/")>]
let ``a link that isn't a document's gives no id`` (link: string) : Task =
    task {
        let item =
            """{"transaction_id":"t1","links":{"document_metadata":"LINK"}}""".Replace("LINK", link)

        use handler = new FakeHandler(fun _ -> ok (listJson "total_count" 1 [ item ]))
        let! result = Api.FilingHistory.pageAsync (clientWith handler) "00000006" [] 0 1 ct
        Assert.Equal(ValueNone, (expectOk result).Value.Items.Head.DocumentId)
    }

[<Fact>]
let ``a PSC statement needs its code`` () : Task =
    task {
        use handler =
            new FakeHandler(fun _ -> ok (pscsJson 1 0 [ """{"notified_on":"2020-01-01"}""" ]))

        match! Api.PscStatements.allAsync (clientWith handler) "00000006" ct with
        | Error(InvalidResponse _) -> ()
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a search reads its hits and total`` () : Task =
    task {
        let item =
            """{"company_number":"00000006","title":"MARINE AND GENERAL","company_status":"active","company_type":"ltd","date_of_creation":"1900-01-01","address":{"address_line_1":"1 Road"}}"""

        use handler = new FakeHandler(fun _ -> ok (listJson "total_results" 44 [ item ]))
        let! result = Api.Search.companiesAsync (clientWith handler) "marine" 0 1 ct
        let page = (expectOk result).Value
        Assert.Equal(44, page.Total)
        Assert.Equal("MARINE AND GENERAL", page.Items.Head.Title)
    }

[<Fact>]
let ``a blank search isn't sent`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok "{}")

        match! Api.Search.companiesAsync (clientWith handler) "  " 0 1 ct with
        | Error(InvalidRequest _) -> Assert.Empty handler.Calls
        | other -> failwith $"%A{other}"
    }
