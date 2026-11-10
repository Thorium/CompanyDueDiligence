module PagingTests

open System.Threading.Tasks
open Xunit
open CompanyDueDiligence.CompaniesHouse
open Fake

let private officer (n: int) =
    $"""{{"name":"OFFICER {n}","officer_role":"director"}}"""

/// Answers from a list of the given length, at most pageCap to a page, as the officers list does.
let private listOf (length: int) (pageCap: int) (call: Call) =
    let start = startIndexOf call
    let items = [ start .. min (length - 1) (start + pageCap - 1) ] |> List.map officer
    ok (officersJson length 0 items)

/// Stops a test whose code under test would otherwise keep asking.
let private bounded (calls: int ref) (respond: Call -> Answer) (call: Call) =
    calls.Value <- calls.Value + 1

    if calls.Value > 25 then
        failwith "Still asking after 25 pages"

    respond call

let private names (result: Result<CompanyOfficer list voption, CompaniesHouseFailure>) =
    (expectOk result).Value |> List.map _.Name

[<Fact>]
let ``paging follows start_index to the end, at the page size it asked for`` () : Task =
    task {
        use handler = new FakeHandler(listOf 250 Api.Paging.PageSize)
        let! result = Api.Officers.allAsync (clientWith handler) "00000006" ct
        Assert.Equal<string list>([ for n in 0..249 -> $"OFFICER {n}" ], names result)
        Assert.Equal<int list>([ 0; 100; 200; 250 ], handler.Calls |> List.map startIndexOf)
        Assert.All(handler.Calls, fun call -> Assert.Contains("items_per_page=100", call.Target))
    }

[<Fact>]
let ``a server giving fewer than asked still reaches the end without a gap`` () : Task =
    task {
        use handler = new FakeHandler(listOf 95 35)
        let! result = Api.Officers.allAsync (clientWith handler) "00000006" ct
        Assert.Equal<string list>([ for n in 0..94 -> $"OFFICER {n}" ], names result)
        Assert.Equal<int list>([ 0; 35; 70; 95 ], handler.Calls |> List.map startIndexOf)
    }

[<Fact>]
let ``filing history's total_count ends the list too`` () : Task =
    task {
        use handler =
            new FakeHandler(fun call ->
                match startIndexOf call with
                | 0 -> ok (listJson "total_count" 1 [ """{"transaction_id":"t1"}""" ])
                | _ -> ok (listJson "total_count" 1 []))

        let! result = Api.FilingHistory.allAsync (clientWith handler) "00000006" [] ct
        Assert.Equal(1, (expectOk result).Value.Length)
    }

[<Theory>]
[<InlineData("""{"items":[]}""")>]
[<InlineData("""{"items":[{"name":"A","officer_role":"director"}]}""")>]
[<InlineData("""{"total_results":"many","active_count":1,"resigned_count":0,"items":[{"name":"A","officer_role":"director"}]}""")>]
[<InlineData("""{"total_results":-1,"active_count":1,"resigned_count":0,"items":[]}""")>]
let ``a list without a readable total is a failure, whatever follows`` (body: string) : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok body)

        match! Api.Officers.allAsync (clientWith handler) "00000006" ct with
        | Error(InvalidResponse answer) -> Assert.Contains("total", answer.Reason)
        | other -> failwith $"%A{other}"

        Assert.Single handler.Calls |> ignore
    }

[<Fact>]
let ``a total understated on every page is contradicted by the read past it`` () : Task =
    task {
        use handler =
            new FakeHandler(fun call ->
                match startIndexOf call with
                | 0 -> ok (officersJson 2 0 [ officer 0; officer 1 ])
                | _ -> ok (officersJson 2 0 [ officer 2; officer 3; officer 4 ]))

        match! Api.Officers.allAsync (clientWith handler) "00000006" ct with
        | Error(InconsistentList(2, _) as failure) ->
            Assert.Equal(FailureAction.RetryLater, CompaniesHouseFailure.action failure)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``one page holding more items than its total is contradicted`` () : Task =
    task {
        use handler =
            new FakeHandler(fun _ -> ok (officersJson 1 0 [ officer 0; officer 1 ]))

        match! Api.Officers.allAsync (clientWith handler) "00000006" ct with
        | Error(InconsistentList(0, _)) -> Assert.Single handler.Calls |> ignore
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a total that changes between pages is a list that changed`` () : Task =
    task {
        use handler =
            new FakeHandler(fun call ->
                match startIndexOf call with
                | 0 -> ok (officersJson 150 0 [ for n in 0..99 -> officer n ])
                | _ -> ok (officersJson 149 0 [ for n in 101..149 -> officer n ]))

        match! Api.Officers.allAsync (clientWith handler) "00000006" ct with
        | Error(InconsistentList(100, reason)) -> Assert.Contains("changed", reason)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a total that changes on a list with no counts is a list that changed`` () : Task =
    task {
        use handler =
            new FakeHandler(fun call ->
                match startIndexOf call with
                | 0 -> ok (listJson "total_count" 3 [ """{"id":"c1"}""" ])
                | 1 -> ok (listJson "total_count" 2 [ """{"id":"c2"}""" ])
                | _ -> ok (listJson "total_count" 2 []))

        match! Api.Charges.allAsync (clientWith handler) "00000006" ct with
        | Error(InconsistentList(1, reason)) -> Assert.Contains("changed", reason)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a counted read of pages that state no counts is a failure`` () : Task =
    task {
        let page: Page<string> =
            {
                Items = []
                Total = 0
                Counts = ValueNone
                ItemsRaw = []
                Status = ValueNone
            }

        match! Api.Paging.allCountedAsync (fun _ -> false) (fun _ _ -> Task.FromResult(Ok(ValueSome page))) with
        | Error(InconsistentList(0, reason)) -> Assert.Contains("no counts", reason)
        | other -> failwith $"%A{other}"
    }

[<Theory>]
[<InlineData("""{"total_results":3,"active_count":3,"resigned_count":0}""")>]
[<InlineData("""{"total_results":3,"active_count":3,"resigned_count":0,"items":null}""")>]
[<InlineData("""{"total_results":3,"active_count":3,"resigned_count":0,"items":{}}""")>]
let ``a total above zero with no items list is a failure, not an empty list`` (body: string) : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok body)

        match! Api.Officers.allAsync (clientWith handler) "00000006" ct with
        | Error(InvalidResponse answer) -> Assert.Contains("items", answer.Reason)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a total that changes on the empty read past the end is a list that changed`` () : Task =
    task {
        use handler =
            new FakeHandler(fun call ->
                match startIndexOf call with
                | 0 -> ok (officersJson 1 0 [ officer 0 ])
                | _ -> ok (officersJson 2 0 []))

        match! Api.Officers.allAsync (clientWith handler) "00000006" ct with
        | Error(InconsistentList(1, _)) -> ()
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``counts that change between pages are a list that changed`` () : Task =
    task {
        use handler =
            new FakeHandler(fun call ->
                match startIndexOf call with
                | 0 -> ok (officersJson 2 0 [ officer 0 ])
                | _ -> ok (officersJson 2 1 [ officer 1 ]))

        match! Api.Officers.allAsync (clientWith handler) "00000006" ct with
        | Error(InconsistentList(1, reason)) -> Assert.Contains("changed", reason)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``an item shifted onto a later page, with the total the same, is a list that changed`` () : Task =
    task {
        use handler =
            new FakeHandler(fun call ->
                match startIndexOf call with
                | 0 -> ok (officersJson 4 0 [ officer 0; officer 1 ])
                | _ -> ok (officersJson 4 0 [ officer 1; officer 2 ]))

        match! Api.Officers.allAsync (clientWith handler) "00000006" ct with
        | Error(InconsistentList(2, reason)) -> Assert.Contains("moved", reason)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``counts that don't add up to the total are a failure`` () : Task =
    task {
        let body =
            $"""{{"total_results":1,"active_count":5,"resigned_count":0,"items":[{officer 0}]}}"""

        use handler =
            new FakeHandler(fun call ->
                match startIndexOf call with
                | 0 -> ok body
                | _ -> ok """{"total_results":1,"active_count":5,"resigned_count":0,"items":[]}""")

        match! Api.Officers.allAsync (clientWith handler) "00000006" ct with
        | Error(InconsistentList(1, reason)) -> Assert.Contains("add up", reason)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a resigned count the items don't bear out is a failure`` () : Task =
    task {
        use handler =
            new FakeHandler(fun call ->
                match startIndexOf call with
                | 0 -> ok (officersJson 1 1 [ officer 0 ])
                | _ -> ok (officersJson 1 1 []))

        match! Api.Officers.allAsync (clientWith handler) "00000006" ct with
        | Error(InconsistentList(1, reason)) -> Assert.Contains("ended", reason)
        | other -> failwith $"%A{other}"
    }

[<Theory>]
[<InlineData("""{"total_results":1,"resigned_count":0,"items":[]}""")>]
[<InlineData("""{"total_results":1,"active_count":1,"items":[]}""")>]
[<InlineData("""{"total_results":1,"active_count":"x","resigned_count":0,"items":[]}""")>]
let ``officers without their counts are a failure`` (body: string) : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok body)

        match! Api.Officers.allAsync (clientWith handler) "00000006" ct with
        | Error(InvalidResponse _) -> ()
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``inactive officers, a count the spec doesn't list, still add up`` () : Task =
    task {
        let answerWith (items: string) =
            $"""{{"total_results":2,"active_count":1,"inactive_count":1,"resigned_count":0,"items":[{items}]}}"""

        use handler =
            new FakeHandler(fun call ->
                match startIndexOf call with
                | 0 -> ok (answerWith $"{officer 0},{officer 1}")
                | _ -> ok (answerWith ""))

        let! result = Api.Officers.allAsync (clientWith handler) "00000006" ct
        Assert.Equal(2, (names result).Length)
    }

[<Fact>]
let ``a PSC list is cross-checked too, against its ceased count`` () : Task =
    task {
        let item = """{"kind":"individual-person-with-significant-control","name":"A"}"""

        use handler =
            new FakeHandler(fun call ->
                match startIndexOf call with
                | 0 -> ok (pscsJson 1 1 [ item ])
                | _ -> ok (pscsJson 1 1 []))

        match! Api.Psc.allAsync (clientWith handler) "00000006" ct with
        | Error(InconsistentList(1, _)) -> ()
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a statement list is cross-checked too, against its ceased count`` () : Task =
    task {
        use handler =
            new FakeHandler(fun call ->
                match startIndexOf call with
                | 0 -> ok (pscsJson 1 1 [ """{"statement":"psc-details-not-confirmed"}""" ])
                | _ -> ok (pscsJson 1 1 []))

        match! Api.PscStatements.allAsync (clientWith handler) "00000006" ct with
        | Error(InconsistentList(1, _)) -> ()
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a list that never ends stops at the page cap, with a clear failure`` () : Task =
    task {
        use handler =
            new FakeHandler(fun call -> ok (officersJson 100000000 0 [ officer (startIndexOf call) ]))

        match! Api.Officers.allAsync (clientWith handler) "00000006" ct with
        | Error(ListTooLong(pages, read, expected) as failure) ->
            Assert.Equal(Api.Paging.MaxPages, pages)
            Assert.Equal(Api.Paging.MaxPages, read)
            Assert.Equal(100000000, expected)
            Assert.Equal(Api.Paging.MaxPages, handler.Calls.Length)
            Assert.Equal(FailureAction.FixRequest, CompaniesHouseFailure.action failure)
            Assert.Contains("100 pages", CompaniesHouseFailure.describe failure)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a list that fits within the cap reads whole, its last call included`` () : Task =
    task {
        use handler = new FakeHandler(listOf 2 1)

        let! result =
            Api.Paging.allUpToAsync 3 (fun start size ->
                Api.Officers.pageAsync (clientWith handler) "00000006" start size ct)

        Assert.Equal(2, (names result).Length)

        let! short =
            Api.Paging.allUpToAsync 2 (fun start size ->
                Api.Officers.pageAsync (clientWith handler) "00000006" start size ct)

        match short with
        | Error(ListTooLong(2, 2, 2)) -> ()
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a cap that allows no call is InvalidRequest`` () : Task =
    task {
        use handler = new FakeHandler(listOf 2 1)

        let! result =
            Api.Paging.allUpToAsync 0 (fun start size ->
                Api.Officers.pageAsync (clientWith handler) "00000006" start size ct)

        match result with
        | Error(InvalidRequest _) -> Assert.Empty handler.Calls
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a 404 on the read at the total ends the list`` () : Task =
    task {
        use handler =
            new FakeHandler(fun call ->
                match startIndexOf call with
                | 0 -> ok (officersJson 1 0 [ officer 0 ])
                | _ -> notFound)

        let! result = Api.Officers.allAsync (clientWith handler) "00000006" ct
        Assert.Equal(1, (names result).Length)
    }

[<Fact>]
let ``an empty page with a total of 0 at the total ends the list, as the sandbox answers`` () : Task =
    task {
        use handler =
            new FakeHandler(fun call ->
                match startIndexOf call with
                | 0 -> ok (officersJson 2 0 [ officer 0; officer 1 ])
                | _ -> ok (officersJson 0 0 []))

        let! result = Api.Officers.allAsync (clientWith handler) "00000006" ct
        Assert.Equal(2, (names result).Length)
    }

[<Fact>]
let ``an empty page with a total of 0 below the total is still IncompleteList`` () : Task =
    task {
        use handler =
            new FakeHandler(fun call ->
                match startIndexOf call with
                | 0 -> ok (officersJson 3 0 [ officer 0; officer 1 ])
                | _ -> ok (officersJson 0 0 []))

        match! Api.Officers.allAsync (clientWith handler) "00000006" ct with
        | Error(InconsistentList(2, _))
        | Error(IncompleteList(2, 3)) -> ()
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a page repeating the one before is RepeatedPage, not a loop`` () : Task =
    task {
        let calls = ref 0

        use handler =
            new FakeHandler(bounded calls (fun _ -> ok (officersJson 1000 0 [ officer 0; officer 1 ])))

        match! Api.Officers.allAsync (clientWith handler) "00000006" ct with
        | Error(RepeatedPage 2) -> Assert.Equal(2, handler.Calls.Length)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``pages repeating in a cycle are RepeatedPage too`` () : Task =
    task {
        let calls = ref 0

        use handler =
            new FakeHandler(
                bounded calls (fun call ->
                    let page = (startIndexOf call / 2) % 2
                    ok (officersJson 1000 0 [ officer page; officer (page + 10) ]))
            )

        match! Api.Officers.allAsync (clientWith handler) "00000006" ct with
        | Error(RepeatedPage 4) -> Assert.Equal(3, handler.Calls.Length)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``pages ending below the total are IncompleteList, not a short list`` () : Task =
    task {
        use handler =
            new FakeHandler(fun call ->
                match startIndexOf call with
                | 0 -> ok (officersJson 5 0 [ officer 0; officer 1 ])
                | _ -> ok (officersJson 5 0 []))

        match! Api.Officers.allAsync (clientWith handler) "00000006" ct with
        | Error(IncompleteList(2, 5) as failure) ->
            Assert.Equal(FailureAction.RetryLater, CompaniesHouseFailure.action failure)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a 404 part way through a list below its total is IncompleteList`` () : Task =
    task {
        use handler =
            new FakeHandler(fun call ->
                match startIndexOf call with
                | 0 -> ok (officersJson 5 0 [ officer 0 ])
                | _ -> notFound)

        match! Api.Officers.allAsync (clientWith handler) "00000006" ct with
        | Error(IncompleteList(1, 5)) -> ()
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a 404 on the first page is ValueNone`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> notFound)

        match! Api.Officers.allAsync (clientWith handler) "00000006" ct with
        | Ok ValueNone -> ()
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a failure part way through is the answer, not the pages so far`` () : Task =
    task {
        use handler =
            new FakeHandler(fun call ->
                match startIndexOf call with
                | 0 -> ok (officersJson 5 0 [ officer 0 ])
                | _ -> answer 429 "")

        match! Api.Officers.allAsync (clientWith handler) "00000006" ct with
        | Error(RateLimited _) -> ()
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a list with no items and a total of zero is empty, list or no list, in one call`` () : Task =
    task {
        for body in
            [
                """{"total_results":0,"active_count":0,"resigned_count":0}"""
                """{"total_results":0,"active_count":0,"resigned_count":0,"items":[]}"""
            ] do
            use handler = new FakeHandler(fun _ -> ok body)
            let! result = Api.Officers.allAsync (clientWith handler) "00000006" ct
            Assert.Empty(names result)
            Assert.Single handler.Calls |> ignore
    }

[<Fact>]
let ``an empty first page below its total is IncompleteList`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (officersJson 3 0 []))

        match! Api.Officers.allAsync (clientWith handler) "00000006" ct with
        | Error(IncompleteList(0, 3)) -> ()
        | other -> failwith $"%A{other}"
    }

[<Theory; InlineData(-1, 10); InlineData(0, 0)>]
let ``a page outside range is InvalidRequest`` (start: int, size: int) : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok "{}")

        match! Api.Officers.pageAsync (clientWith handler) "00000006" start size ct with
        | Error(InvalidRequest _) -> Assert.Empty handler.Calls
        | other -> failwith $"%A{other}"
    }
