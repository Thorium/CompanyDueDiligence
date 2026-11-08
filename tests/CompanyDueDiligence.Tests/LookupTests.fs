/// Officer appointments, UK establishments, disqualified officers and reading a search, from answers shaped by the bundled specs.
module LookupTests

open System
open System.Threading.Tasks
open Xunit
open CompanyDueDiligence.CompaniesHouse
open Fake

let private date (y: int) (m: int) (d: int) = ValueSome(DateOnly(y, m, d))

let private appointment =
    """{"name":"DOE, Jane","officer_role":"director","appointed_on":"2010-05-06","resigned_on":"2012-01-02","nationality":"British",
        "appointed_to":{"company_number":"00000006","company_name":"MARINE AND GENERAL","company_status":"active"},
        "links":{"company":"/company/00000006"}}"""

[<Fact>]
let ``an officer's appointments read across pages, with the company of each`` () : Task =
    task {
        use handler =
            new FakeHandler(fun call ->
                match startIndexOf call with
                | 0 -> ok (listJson "total_results" 1 [ appointment ])
                | _ -> ok (listJson "total_results" 1 []))

        let! result = Api.Appointments.allAsync (clientWith handler) "abc" ct
        let read = (expectOk result).Value.Head
        Assert.Equal("00000006", read.CompanyNumber)
        Assert.Equal(ValueSome "MARINE AND GENERAL", read.CompanyName)
        Assert.Equal(ValueSome "active", read.CompanyStatus)
        Assert.Equal(date 2012 1 2, read.ResignedOn)
        Assert.Equal<int list>([ 0; 1 ], handler.Calls |> List.map startIndexOf)
    }

[<Theory>]
[<InlineData("""{"items":[]}""")>]
[<InlineData("""{"total_results":1,"items":[{"name":"A","officer_role":"director"}]}""")>]
[<InlineData("""{"total_results":1,"items":[{"name":"A","appointed_to":{"company_name":"B"}}]}""")>]
[<InlineData("""{"total_results":1,"items":[{"name":"A","appointed_to":{"company_number":"1"},"resigned_on":"soon"}]}""")>]
let ``an appointments answer with no total, no company or an unreadable end is a failure`` (body: string) : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok body)

        match! Api.Appointments.allAsync (clientWith handler) "abc" ct with
        | Error(InvalidResponse _) -> ()
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``appointments that stop short of their total are IncompleteList`` () : Task =
    task {
        use handler =
            new FakeHandler(fun call ->
                match startIndexOf call with
                | 0 -> ok (listJson "total_results" 5 [ appointment ])
                | _ -> notFound)

        match! Api.Appointments.allAsync (clientWith handler) "abc" ct with
        | Error(IncompleteList(1, 5)) -> ()
        | other -> failwith $"%A{other}"
    }

[<Theory; InlineData(""); InlineData(".."); InlineData(null)>]
let ``an officer id that can't be one isn't sent`` (id: string) : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok "{}")

        match! Api.Appointments.pageAsync (clientWith handler) id 0 1 ct with
        | Error(InvalidRequest _) -> Assert.Empty handler.Calls
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``an id off an officer list reaches its appointments`` () : Task =
    task {
        let officer =
            """{"name":"A","officer_role":"director","links":{"officer":{"appointments":"/officers/a%2Fb/appointments"}}}"""

        use handler = new FakeHandler(fun _ -> ok (officersJson 1 0 [ officer ]))
        let client = clientWith handler
        let! officers = Api.Officers.pageAsync client "00000006" 0 1 ct
        let id = (expectOk officers).Value.Items.Head.OfficerId.Value
        Assert.Equal("a/b", id)
        let! _ = Api.Appointments.pageAsync client id 0 1 ct
        Assert.Contains("/officers/a%2Fb/appointments", (List.last handler.Calls).Uri)
    }

[<Fact>]
let ``UK establishments are read in one call, the spec giving no paging`` () : Task =
    task {
        let body =
            """{"etag":"e","kind":"ukestablishment-companies","items":[{"company_number":"BR000001","company_name":"ACME GMBH","company_status":"open","locality":"Leeds","links":{"company":"/company/BR000001"}}]}"""

        use handler = new FakeHandler(fun _ -> ok body)
        let! result = Api.UkEstablishments.getAsync (clientWith handler) "FC000001" ct
        let read = (expectOk result).Value.Head
        Assert.Equal("BR000001", read.CompanyNumber)
        Assert.Equal("ACME GMBH", read.CompanyName)
        Assert.Equal(ValueSome "open", read.Status)
        Assert.Equal(ValueSome "Leeds", read.Locality)
        Assert.Single handler.Calls |> ignore
    }

[<Theory>]
[<InlineData("""{"etag":"e"}""")>]
[<InlineData("""{"items":[{"company_name":"A"}]}""")>]
[<InlineData("""{"total_results":2,"items":[{"company_number":"BR000001","company_name":"A"}]}""")>]
let ``UK establishments without their list, a number or a matching total are a failure`` (body: string) : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok body)

        match! Api.UkEstablishments.getAsync (clientWith handler) "FC000001" ct with
        | Error(InvalidResponse _) -> ()
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``no UK establishments held at the address is ValueNone`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> notFound)

        match! Api.UkEstablishments.getAsync (clientWith handler) "FC000001" ct with
        | Ok ValueNone -> ()
        | other -> failwith $"%A{other}"
    }

let private hit (n: int) =
    $$$"""{"kind":"searchresults#disqualified-officer","title":"JANE DOE {{{n}}}","description":"Born on 1 March 1970","date_of_birth":"1970-03-01",
         "address":{"address_line_1":"1 Road"},"links":{"self":"/disqualified-officers/natural/id{{{n}}}"}}"""

[<Fact>]
let ``a name that matched is only a possible match, with what to look up`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (listJson "total_results" 1 [ hit 0 ]))

        match! Api.DisqualifiedOfficers.searchAsync (clientWith handler) "Jane Doe" ct with
        | Ok(DisqualificationSearch.PossibleMatches read) ->
            Assert.True read.Complete
            let item = read.Found.Head
            Assert.Equal("JANE DOE 0", item.Title)
            Assert.Equal(date 1970 3 1, item.DateOfBirth)
            Assert.Equal(ValueSome(DisqualifiedOfficerId.Natural "id0"), item.OfficerId)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``no entry for a name, with everything read, is no name match and nothing stronger`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (listJson "total_results" 0 []))

        match! Api.DisqualifiedOfficers.searchAsync (clientWith handler) "Jane Doe" ct with
        | Ok DisqualificationSearch.NoNameMatch -> ()
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``no entries against a stated total, or a 404, settles nothing`` () : Task =
    task {
        use stated = new FakeHandler(fun _ -> ok (listJson "total_results" 3 []))

        match! Api.DisqualifiedOfficers.searchAsync (clientWith stated) "Jane Doe" ct with
        | Ok(DisqualificationSearch.Inconclusive _) -> ()
        | other -> failwith $"%A{other}"

        use missing = new FakeHandler(fun _ -> notFound)

        match! Api.DisqualifiedOfficers.searchAsync (clientWith missing) "Jane Doe" ct with
        | Ok(DisqualificationSearch.Inconclusive _) -> ()
        | other -> failwith $"%A{other}"
    }

[<Theory>]
[<InlineData(500)>]
[<InlineData(403)>]
[<InlineData(400)>]
[<InlineData(401)>]
let ``a failed search is a failure, never an answer about the person`` (status: int) : Task =
    task {
        use handler = new FakeHandler(fun _ -> answer status "")

        match! Api.DisqualifiedOfficers.searchAsync (clientWith handler) "Jane Doe" ct with
        | Error _ -> ()
        | other -> failwith $"%A{other}"
    }

[<Theory; InlineData("""{"items":[]}"""); InlineData("not json")>]
let ``a search answer that can't be read is a failure, never no match`` (body: string) : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok body)

        match! Api.DisqualifiedOfficers.searchAsync (clientWith handler) "Jane Doe" ct with
        | Error(InvalidResponse _) -> ()
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a failure on a later page of a search is a failure`` () : Task =
    task {
        use handler =
            new FakeHandler(fun call ->
                match startIndexOf call with
                | 0 -> ok (listJson "total_results" 3 [ hit 0 ])
                | _ -> answer 500 "")

        match! Api.DisqualifiedOfficers.searchAsync (clientWith handler) "Jane Doe" ct with
        | Error(ApiError(500, _)) -> ()
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a blank name isn't sent`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok "{}")

        match! Api.DisqualifiedOfficers.searchAsync (clientWith handler) "  " ct with
        | Error(InvalidRequest _) -> Assert.Empty handler.Calls
        | other -> failwith $"%A{other}"
    }

[<Theory>]
[<InlineData("/disqualified-officers/corporate/id1", "Corporate")>]
[<InlineData("/somewhere/else", "None")>]
let ``an entry's id says which details call it is for`` (link: string, expected: string) : Task =
    task {
        let item = $$$"""{"title":"A LTD","links":{"self":"{{{link}}}"}}"""
        use handler = new FakeHandler(fun _ -> ok (listJson "total_results" 1 [ item ]))

        let! result =
            Api.DisqualifiedOfficers.searchPageAsync (clientWith handler) "a" 0 1 ct

        match (expectOk result).Value.Items.Head.OfficerId, expected with
        | ValueSome(DisqualifiedOfficerId.Corporate "id1"), "Corporate"
        | ValueNone, "None" -> ()
        | other -> failwith $"%A{other}"
    }

let private natural =
    """{"kind":"natural-disqualification","title":"Ms","forename":"Jane","other_forenames":"Ann","surname":"DOE","date_of_birth":"1970-03-01","nationality":"British","person_number":"123",
        "links":{"self":"/x"},
        "disqualifications":[{"case_identifier":"c1","disqualification_type":"court-order","disqualified_from":"2020-01-01","disqualified_until":"2025-01-01","court_name":"A Court",
            "company_names":["A LTD"],"address":{"address_line_1":"1 Road"},"reason":{"description_identifier":"unfitness","act":"company-directors-disqualification-act-1986"}}],
        "permissions_to_act":[{"company_names":["B LTD"],"court_name":"A Court","granted_on":"2021-01-01","expires_on":"2022-01-01"}]}"""

[<Fact>]
let ``a natural officer's disqualifications are read`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok natural)

        let! result =
            Api.DisqualifiedOfficers.detailsAsync (clientWith handler) (DisqualifiedOfficerId.Natural "id") ct

        let officer = (expectOk result).Value
        Assert.Equal("Ms Jane Ann DOE", officer.Name)
        Assert.Equal(date 1970 3 1, officer.DateOfBirth)
        Assert.Equal(ValueSome "123", officer.PersonNumber)
        let disqualification = officer.Disqualifications.Head
        Assert.Equal(date 2020 1 1, disqualification.From)
        Assert.Equal(date 2025 1 1, disqualification.Until)
        Assert.Equal(ValueSome "unfitness", disqualification.Reason)
        Assert.Equal(ValueSome "company-directors-disqualification-act-1986", disqualification.Act)
        Assert.Equal<string list>([ "A LTD" ], disqualification.CompanyNames)
        Assert.Equal(date 2022 1 1, officer.PermissionsToAct.Head.ExpiresOn)
        Assert.Equal("/disqualified-officers/natural/id", Uri(handler.Calls.Head.Uri).AbsolutePath)
    }

[<Fact>]
let ``a corporate officer's record is read`` () : Task =
    task {
        let body =
            """{"kind":"corporate-disqualification","name":"A LTD","company_number":"123","country_of_registration":"Wales","disqualifications":[]}"""

        use handler = new FakeHandler(fun _ -> ok body)

        let! result =
            Api.DisqualifiedOfficers.detailsAsync (clientWith handler) (DisqualifiedOfficerId.Corporate "id") ct

        let officer = (expectOk result).Value
        Assert.Equal("A LTD", officer.Name)
        Assert.Equal(ValueSome "123", officer.CompanyNumber)
        Assert.Equal(ValueSome "Wales", officer.CountryOfRegistration)
    }

[<Theory>]
[<InlineData("""{"name":"A LTD"}""")>]
[<InlineData("""{"disqualifications":[]}""")>]
[<InlineData("""{"name":"A","disqualifications":[{"disqualified_until":"soon"}]}""")>]
[<InlineData("""{"name":"A","disqualifications":[{"disqualified_from":"soon"}]}""")>]
[<InlineData("""{"name":"A","disqualifications":[],"permissions_to_act":[{"expires_on":"soon"}]}""")>]
let ``details without disqualifications, a name or a readable end are a failure, not none`` (body: string) : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok body)

        match! Api.DisqualifiedOfficers.detailsAsync (clientWith handler) (DisqualifiedOfficerId.Natural "id") ct with
        | Error(InvalidResponse _) -> ()
        | other -> failwith $"%A{other}"
    }

let private company (n: int) =
    $$"""{"company_number":"{{n}}","title":"MARINE {{n}}"}"""

[<Fact>]
let ``reading a search stops at its total, complete`` () : Task =
    task {
        use handler =
            new FakeHandler(fun call ->
                let start = startIndexOf call
                ok (listJson "total_results" 150 [ for n in start .. min 149 (start + 99) -> company n ]))

        let! result = Api.Search.companiesReadAsync (clientWith handler) "marine" ct
        let read = (expectOk result).Value
        Assert.Equal(150, read.Found.Length)
        Assert.True read.Complete
        Assert.Equal(2, handler.Calls.Length)
    }

[<Fact>]
let ``reading a search stops at its cap and says it isn't complete`` () : Task =
    task {
        use handler =
            new FakeHandler(fun call ->
                let start = startIndexOf call
                ok (listJson "total_results" 5000 [ for n in start .. start + 99 -> company n ]))

        let! result = Api.Search.companiesReadAsync (clientWith handler) "marine" ct
        let read = (expectOk result).Value
        Assert.Equal(Api.Search.MostRead, read.Found.Length)
        Assert.Equal(5000, read.StatedTotal)
        Assert.False read.Complete
        Assert.Equal(10, handler.Calls.Length)
    }

[<Fact>]
let ``a search that gives one item at a time stops at the page cap and says it isn't complete`` () : Task =
    task {
        use handler =
            new FakeHandler(fun call -> ok (listJson "total_results" 5000 [ company (startIndexOf call) ]))

        let! result = Api.Search.companiesReadAsync (clientWith handler) "marine" ct
        let read = (expectOk result).Value
        Assert.False read.Complete
        Assert.Equal(Api.Paging.MaxPages, handler.Calls.Length)
    }

[<Fact>]
let ``a 416 after the first page ends the read, not complete`` () : Task =
    task {
        use handler =
            new FakeHandler(fun call ->
                match startIndexOf call with
                | 0 -> ok (listJson "total_results" 300 [ for n in 0..99 -> company n ])
                | _ -> answer 416 "")

        let! result = Api.Search.companiesReadAsync (clientWith handler) "marine" ct
        let read = (expectOk result).Value
        Assert.Equal(100, read.Found.Length)
        Assert.False read.Complete
    }

[<Fact>]
let ``a 416 on the first page is a failure`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> answer 416 "")

        match! Api.Search.companiesReadAsync (clientWith handler) "marine" ct with
        | Error(ApiError(416, _)) -> ()
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``an empty page below the total ends the read, not complete`` () : Task =
    task {
        use handler =
            new FakeHandler(fun call ->
                match startIndexOf call with
                | 0 -> ok (listJson "total_results" 300 [ company 0 ])
                | _ -> ok (listJson "total_results" 300 []))

        let! result = Api.Search.companiesReadAsync (clientWith handler) "marine" ct
        Assert.False (expectOk result).Value.Complete
    }

[<Fact>]
let ``a search page repeating is RepeatedPage`` () : Task =
    task {
        use handler =
            new FakeHandler(fun _ -> ok (listJson "total_results" 300 [ company 0 ]))

        match! Api.Search.companiesReadAsync (clientWith handler) "marine" ct with
        | Error(RepeatedPage 1) -> ()
        | other -> failwith $"%A{other}"
    }
