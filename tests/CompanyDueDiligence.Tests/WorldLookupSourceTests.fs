/// Taiwan, Colombia, Costa Rica, Chile and Kenya, replayed from answers recorded on 2026-11-08 with people's names redacted; Kenya from its
/// documented examples, since it needs credentials. Anything a source could answer that isn't understood has to come back as a failure or
/// NotAvailable, never as a company or "not held".
module WorldLookupSourceTests

open System
open System.IO
open System.Text
open System.Threading.Tasks
open Xunit
open CompanyDueDiligence
open Fake

let private fixture (name: string) =
    File.ReadAllText(Path.Combine(AppContext.BaseDirectory, "Fixtures", name))

let private fixedTime = DateTimeOffset(2026, 11, 8, 12, 0, 0, TimeSpan.Zero)

let private connectionOver (handler: FakeHandler) =
    SourceConnection(handler, (fun () -> fixedTime), (fun _ _ -> Task.CompletedTask))

/// A connection on its own clock, which any wait moves on at once, for a source making more calls than its pace allows at one time.
let private movingConnectionOver (handler: FakeHandler) (clock: FakeClock) =
    SourceConnection(handler, (fun () -> clock.Now), clock.Delay)

let private tw = Jurisdiction.ofCode "TW"
let private co = Jurisdiction.ofCode "CO"
let private cr = Jurisdiction.ofCode "CR"
let private cl = Jurisdiction.ofCode "CL"
let private ke = Jurisdiction.ofCode "KE"

[<Literal>]
let private Secret = "the-consumer-secret-value"

let private kraCredentials = ClientCredentials("the-consumer-key", Secret)

let private findIn (registry: ICompanyRegistry) (number: string) (jurisdiction: Jurisdiction) =
    task {
        match! registry.TryFindAsync(number, jurisdiction, ct) with
        | Ok evidence -> return evidence
        | Error failure -> return failwith (RegistryFailure.describe failure)
    }

let private companyOf (evidence: Evidence<Availability<Company voption>>) =
    match evidence.Value with
    | Available(ValueSome company) -> company
    | other -> failwith $"%A{other}"

let private notSentWith
    (registry: ICompanyRegistry)
    (handler: FakeHandler)
    (number: string)
    (jurisdiction: Jurisdiction)
    =
    task {
        match! registry.TryFindAsync(number, jurisdiction, ct) with
        | Error(RegistryFailure.InvalidRequest _) -> Assert.Empty handler.Calls
        | other -> failwith $"%A{other}"
    }

let private queryOf (call: Call) = Uri.UnescapeDataString call.Target

/// Every lookup source here, with a number of the right shape.
let private allRegistries (handler: FakeHandler) =
    let connection = connectionOver handler

    [
        Sources.taiwan connection, "22099131", tw
        Sources.colombia connection, "899999068", co
        Sources.costaRica connection, "3101012009", cr
        Sources.kenya connection kraCredentials, "P318295670X", ke
    ]

let private chileFrom (files: string list) =
    task {
        let streams =
            files
            |> List.map (fun text -> new MemoryStream(Encoding.UTF8.GetBytes text) :> Stream)

        let date =
            {
                DownloadedAt = fixedTime
                AsOf = ValueSome(DateOnly(2026, 9, 30))
            }

        return! BulkSources.chileAsync streams date ct
    }

let private chileRegistry () =
    task {
        match! chileFrom [ fixture "chile-2013.csv" ] with
        | Ok registry -> return registry
        | Error failure -> return failwith (RegistryFailure.describe failure)
    }

// Taiwan

[<Fact>]
let ``Taiwan reads a company with its location and dates in the Republic of China calendar`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "taiwan-company.json"))
        let! evidence = findIn (Sources.taiwan (connectionOver handler)) "22099131" tw
        let company = companyOf evidence
        Assert.Equal("MOEA company registration open data (Taiwan)", evidence.Source)
        Assert.Equal(fixedTime, evidence.RetrievedAt)
        Assert.Equal(ValueNone, evidence.AsOf)
        Assert.StartsWith("{\"Business_Accounting_NO\":\"22099131\"", evidence.Raw.Value)
        Assert.Equal("台灣積體電路製造股份有限公司", company.Name)
        Assert.Equal("22099131", company.Number)
        Assert.Equal(CompanyStanding.Active, company.Standing)
        Assert.Equal(ValueSome "核准設立", company.StatusAsPublished)
        Assert.Equal(RecordRole.NotStated, company.Role)
        Assert.Equal(ValueSome(DateOnly(1987, 2, 21)), company.IncorporatedOn)
        Assert.Equal(ValueNone, company.CeasedOn)

        match company.Addresses with
        | Available [ {
                          Kind = AddressKind.RegisteredOffice
                          Address = address
                      } ] -> Assert.Equal<string list>([ "新竹科學園區新竹市力行六路8號" ], address.Lines)
        | other -> failwith $"%A{other}"

        let call = handler.Calls.Head
        Assert.Equal("data.gcis.nat.gov.tw", call.Host)
        Assert.Contains("/od/data/api/5F64D864-61CB-4D0D-8AD9-492047CC1EA6?$format=json", call.Target)
        Assert.Contains("$filter=Business_Accounting_NO eq 22099131", queryOf call)
    }

[<Fact>]
let ``Taiwan reads a dissolved company as winding up, with the date it gives`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "taiwan-dissolved.json"))
        let! evidence = findIn (Sources.taiwan (connectionOver handler)) "27493581" tw
        let company = companyOf evidence
        Assert.StartsWith("WindingUp", $"%A{company.Standing}")
        Assert.Equal(ValueSome(DateOnly(2011, 8, 30)), company.CeasedOn)
        Assert.Equal(ValueSome(DateOnly(2004, 11, 18)), company.IncorporatedOn)
    }

[<Theory; InlineData(""); InlineData("[]"); InlineData("  ")>]
let ``Taiwan's empty answer proves nothing, since it holds companies only`` (body: string) : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok body)
        let! evidence = findIn (Sources.taiwan (connectionOver handler)) "12345675" tw

        match evidence.Value with
        | NotAvailable reason -> Assert.Contains("proves nothing", reason)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``another company in Taiwan's answer fails the read`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "taiwan-company.json"))

        match! (Sources.taiwan (connectionOver handler)).TryFindAsync("27493581", tw, ct) with
        | Error(RegistryFailure.UnreadableAnswer _) -> ()
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a Taiwan date that can't be read fails the read rather than reading as none`` () : Task =
    task {
        let body =
            """[{"Business_Accounting_NO":"22099131","Company_Name":"X","Company_Status_Desc":"解散","Revoke_App_Date":"1001330"}]"""

        use handler = new FakeHandler(fun _ -> ok body)

        match! (Sources.taiwan (connectionOver handler)).TryFindAsync("22099131", tw, ct) with
        | Error(RegistryFailure.UnreadableAnswer _) -> ()
        | other -> failwith $"%A{other}"
    }

[<Theory>]
[<InlineData("核准設立", "Active")>]
[<InlineData("核准設立，但已命令解散", "WindingUp")>]
[<InlineData("重整", "InInsolvency")>]
[<InlineData("解散", "WindingUp")>]
[<InlineData("撤銷", "Closed")>]
[<InlineData("破產", "InInsolvency")>]
[<InlineData("合併解散", "Closed")>]
[<InlineData("撤回登記", "Closed")>]
[<InlineData("廢止", "Closed")>]
[<InlineData("廢止認許", "Closed")>]
[<InlineData("解散已清算完結", "Dissolved")>]
[<InlineData("撤銷已清算完結", "Closed")>]
[<InlineData("廢止已清算完結", "Closed")>]
[<InlineData("撤回登記已清算完結", "Closed")>]
[<InlineData("撤銷登記已清算完結", "Closed")>]
[<InlineData("廢止登記已清算完結", "Closed")>]
[<InlineData("撤銷登記", "Closed")>]
[<InlineData("分割解散", "Closed")>]
[<InlineData("終止破產", "InInsolvency")>]
[<InlineData("中止破產", "InInsolvency")>]
[<InlineData("塗銷破產", "Unknown")>]
[<InlineData("破產程序終結(終止)", "InInsolvency")>]
[<InlineData("破產程序終結(終止)清算中", "InInsolvency")>]
[<InlineData("破產已清算完結", "Closed")>]
[<InlineData("接管", "InInsolvency")>]
[<InlineData("撤銷無需清算", "Closed")>]
[<InlineData("撤銷許可", "Closed")>]
[<InlineData("廢止許可", "Closed")>]
[<InlineData("撤銷許可已清算完結", "Closed")>]
[<InlineData("廢止許可已清算完結", "Closed")>]
[<InlineData("清理", "WindingUp")>]
[<InlineData("撤銷公司設立", "Closed")>]
[<InlineData("清理完結", "Closed")>]
[<InlineData("新的狀態", "Unknown")>]
let ``every status in Taiwan's code list has a standing, and a new one is unknown`` (word: string, expected: string) =
    Assert.StartsWith(expected, $"%A{Taiwan.statusStanding (ValueSome word)}")

[<Theory>]
[<InlineData("停業", "ActiveWithConcern")>]
[<InlineData("延展開業", "ActiveWithConcern")>]
[<InlineData("復業", "Active")>]
[<InlineData("其他", "Unknown")>]
let ``a live Taiwan company's suspension qualifies it`` (word: string, expected: string) =
    let standing =
        Taiwan.standing (ValueSome "核准設立") (ValueSome word) (ValueSome(DateOnly(2026, 1, 1))) ValueNone

    Assert.StartsWith(expected, $"%A{standing}")
    Assert.Equal(ValueSome "停業", Taiwan.suspensionWord ValueNone (ValueSome "1"))

[<Fact>]
let ``a Taiwan bankruptcy status is stated as insolvency history`` () : Task =
    task {
        let body =
            """[{"Business_Accounting_NO":"22099131","Company_Name":"X","Company_Status_Desc":"破產"}]"""

        use handler = new FakeHandler(fun _ -> ok body)
        let! evidence = findIn (Sources.taiwan (connectionOver handler)) "22099131" tw
        Assert.Equal<CompanyConcern list>([ CompanyConcern.InsolvencyHistory ], (companyOf evidence).Concerns.Stated)
    }

[<Theory; InlineData("22099131"); InlineData("20828393"); InlineData("12345675"); InlineData("2209 9131")>]
let ``a UBN passing the check is taken`` (number: string) =
    Assert.True((Taiwan.normalUbn number).IsSome)

[<Theory; InlineData("22099132"); InlineData("1234567"); InlineData("abcdefgh"); InlineData(""); InlineData(null)>]
let ``a value that can't be a UBN isn't sent`` (number: string) : Task =
    task {
        use handler = new FakeHandler(fun _ -> failwith "Nothing should be sent")
        do! notSentWith (Sources.taiwan (connectionOver handler)) handler number tw
    }

[<Fact>]
let ``Taiwan's directors are read, with a juristic person and its representative both listed`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "taiwan-directors.json"))

        match! (Sources.taiwan (connectionOver handler)).OfficersAsync("22099131", tw, ct) with
        | Ok {
                 Value = Available officers
                 Raw = ValueSome _
             } ->
            Assert.Equal(5, officers.Length)
            Assert.All(officers, (fun officer -> Assert.True officer.Current))
            let chair = officers.Head
            Assert.Equal("PERSON ONE", chair.Name)
            Assert.Equal(OfficerRole.Director, chair.Role)
            Assert.Equal(PartyKind.Individual, chair.Kind)

            let entity =
                officers |> List.find (fun officer -> officer.Kind = PartyKind.LegalEntity)

            Assert.Equal("行政院國家發展基金管理會", entity.Name)
            Assert.Equal(OfficerRole.Director, entity.Role)
            Assert.Equal(ValueSome "行政院國家發展基金管理會", entity.Entity.Value.LegalName)

            let representative =
                officers |> List.find (fun officer -> officer.Name = "PERSON THREE")

            Assert.Equal(OfficerRole.Other "Representative of 行政院國家發展基金管理會 as 董事", representative.Role)
            Assert.Contains("4E5F7653-1B91-4DDC-99D5-468530FAE396", handler.Calls.Head.Target)
        | other -> failwith $"%A{other}"
    }

[<Theory; InlineData(""); InlineData("[]")>]
let ``Taiwan listing no directors says nothing`` (body: string) : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok body)

        match! (Sources.taiwan (connectionOver handler)).OfficersAsync("27493581", tw, ct) with
        | Ok { Value = NotAvailable _ } -> ()
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a full page of Taiwan's directors may be cut short, so it says nothing`` () : Task =
    task {
        let row index =
            $"""{{"Person_Position_Name":"董事","Person_Name":"PERSON {index}"}}"""

        let page count =
            "[" + String.concat "," (List.init count row) + "]"

        use full = new FakeHandler(fun _ -> ok (page 50))

        match! (Sources.taiwan (connectionOver full)).OfficersAsync("22099131", tw, ct) with
        | Ok { Value = NotAvailable reason } -> Assert.Contains("cut short", reason)
        | other -> failwith $"%A{other}"

        Assert.Contains("$top=50", queryOf full.Calls.Head)
        use short = new FakeHandler(fun _ -> ok (page 49))

        match! (Sources.taiwan (connectionOver short)).OfficersAsync("22099131", tw, ct) with
        | Ok { Value = Available officers } -> Assert.Equal(49, officers.Length)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``Taiwan's search asks for the term once per status, and nothing that would change the filter`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "taiwan-search.json"))

        match! (Sources.taiwan (movingConnectionOver handler (FakeClock()))).SearchAsync("台積電 eq '01'", tw, ct) with
        | Ok { Value = Available result } ->
            Assert.Equal(2, result.Matches.Length)
            Assert.Equal("27493581", result.Matches.Head.Number)
            Assert.False result.Complete
            Assert.Equal(9, handler.Calls.Length)

            Assert.All(
                handler.Calls,
                fun call -> Assert.Contains("$filter=Company_Name like 台積電eq01 and Company_Status eq ", queryOf call)
            )
        | other -> failwith $"%A{other}"

        use none = new FakeHandler(fun _ -> failwith "Nothing should be sent")

        match! (Sources.taiwan (connectionOver none)).SearchAsync(" '%' ", tw, ct) with
        | Error(RegistryFailure.InvalidRequest _) -> Assert.Empty none.Calls
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``Taiwan's search reads live and ended companies, one call per status, within the pace, and a UBN met twice once``
    ()
    : Task =
    task {
        // Answers of 2026-11-08: 台積電 under 01 and 04, and 台灣 under 10 (廢止登記), cut to three rows.
        let byStatus (call: Call) =
            let query = queryOf call

            if query.Contains "Company_Status eq 01" then
                ok (fixture "taiwan-search-active.json")
            elif query.Contains "Company_Status eq 04" || query.Contains "Company_Status eq 05" then
                ok (fixture "taiwan-search.json")
            elif query.Contains "Company_Status eq 10" then
                ok (fixture "taiwan-search-annulled.json")
            else
                ok ""

        use handler = new FakeHandler(byStatus)
        let clock = FakeClock()
        let started = clock.Now

        match! (Sources.taiwan (movingConnectionOver handler clock)).SearchAsync("台積電", tw, ct) with
        | Ok {
                 Value = Available result
                 Raw = ValueSome raw
             } ->
            Assert.Equal<string list>(
                [
                    "54900838"
                    "90312187"
                    "27493581"
                    "80701308"
                    "24788598"
                    "27946661"
                    "28982257"
                ],
                result.Matches |> List.map _.Number
            )

            Assert.Equal(
                ValueSome "廢止登記",
                (result.Matches |> List.find (fun found -> found.Number = "24788598")).StatusAsPublished
            )

            Assert.False result.Complete
            Assert.StartsWith("[[", raw)

            let statuses =
                handler.Calls
                |> List.map (fun call ->
                    let query = queryOf call
                    let at = query.IndexOf "Company_Status eq " + "Company_Status eq ".Length
                    query.Substring(at, 2))
                |> List.sort

            Assert.Equal<string list>([ "01"; "04"; "05"; "07"; "08"; "09"; "10"; "11"; "13" ], statuses)
            // Two calls a second: the ninth goes four seconds after the first.
            Assert.True(clock.Now - started >= TimeSpan.FromSeconds 4.)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``Taiwan's 廢止登記, code 10, is Closed`` () =
    Assert.Equal(CompanyStanding.Closed "廢止登記: registration annulled", Taiwan.statusStanding (ValueSome "廢止登記"))

// Colombia

[<Fact>]
let ``Colombia reads a company with its renewal, its last update as of, and the row alone as raw`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "colombia-active.json"))
        let! evidence = findIn (Sources.colombia (connectionOver handler)) "899.999.068-1" co
        let company = companyOf evidence
        Assert.Equal("RUES extract on datos.gov.co (Colombia)", evidence.Source)
        Assert.Equal(ValueSome(DateOnly(2026, 10, 16)), evidence.AsOf)
        Assert.StartsWith("{\"codigo_camara\":\"04\"", evidence.Raw.Value)
        Assert.Equal("ECOPETROL S A", company.Name)
        Assert.Equal("899999068", company.Number)
        Assert.Equal(CompanyStanding.Active, company.Standing)
        Assert.Equal(ValueSome "ACTIVA; último año renovado 2026", company.StatusAsPublished)
        Assert.Equal(ValueSome "SOCIEDAD ANONIMA", company.LegalForm)
        Assert.Equal(RecordRole.Home, company.Role)
        Assert.Equal(ValueSome(DateOnly(2003, 7, 18)), company.IncorporatedOn)
        Assert.Empty company.Concerns.Stated
        Assert.DoesNotContain(CompanyConcern.AnnualFilingOverdue, company.Concerns.Unconfirmed)

        match company.Addresses with
        | NotAvailable _ -> ()
        | other -> failwith $"%A{other}"

        let call = handler.Calls.Head
        Assert.Equal("www.datos.gov.co", call.Host)

        Assert.Contains(
            "codigo_categoria_matricula = '01' AND (nit = '899999068' OR (codigo_clase_identificacion = '02' AND numero_identificacion = '899999068'))",
            queryOf call
        )
    }

[<Fact>]
let ``Colombia reads a cancelled company, numbered by its identification when it has no nit field`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "colombia-cancelled.json"))
        let! evidence = findIn (Sources.colombia (connectionOver handler)) "900835212" co
        let company = companyOf evidence
        Assert.Equal("900835212", company.Number)
        Assert.Equal(CompanyStanding.Closed "CANCELADA", company.Standing)
        Assert.Equal(ValueSome(DateOnly(2016, 12, 30)), company.CeasedOn)
        Assert.Equal(CompanyConcerns.notPublished, company.Concerns)
    }

[<Fact>]
let ``an active Colombian matrícula that missed its renewal is a concern`` () : Task =
    task {
        let body =
            """[{"codigo_camara":"04","matricula":"1","razon_social":"X SAS","nit":"899999068","codigo_estado_matricula":"01","estado_matricula":"ACTIVA","ultimo_ano_renovado":"2020","fecha_actualizacion":"2026/10/16 17:35:14.227000000"}]"""

        use handler = new FakeHandler(fun _ -> ok body)
        let! evidence = findIn (Sources.colombia (connectionOver handler)) "899999068" co
        let company = companyOf evidence
        Assert.Equal(CompanyStanding.ActiveWithConcern "matrícula last renewed for 2020", company.Standing)
        Assert.Equal<CompanyConcern list>([ CompanyConcern.AnnualFilingOverdue ], company.Concerns.Stated)
    }

[<Theory>]
[<InlineData("X SAS EN LIQUIDACION", "WindingUp")>]
[<InlineData("X S.A.S. en liquidación", "WindingUp")>]
[<InlineData("X SAS EN LIQUIDACION JUDICIAL", "InInsolvency")>]
[<InlineData("X SAS EN REORGANIZACIÓN", "InInsolvency")>]
[<InlineData("LIQUIDOS DEL CARIBE SAS", "Active")>]
let ``an active Colombian matrícula's name suffix says it is wound up or in a Ley 1116 proceeding``
    (name: string, expected: string)
    : Task =
    task {
        let body =
            $"""[{{"codigo_camara":"04","matricula":"1","razon_social":"{name}","nit":"899999068","codigo_estado_matricula":"01","estado_matricula":"ACTIVA","ultimo_ano_renovado":"2026","fecha_actualizacion":"2026/10/16 17:35:14.227000000"}}]"""

        use handler = new FakeHandler(fun _ -> ok body)
        let! evidence = findIn (Sources.colombia (connectionOver handler)) "899999068" co
        let company = companyOf evidence
        Assert.StartsWith(expected, $"%A{company.Standing}")

        Assert.Equal(
            (expected = "InInsolvency"),
            company.Concerns.Stated |> List.contains CompanyConcern.InsolvencyHistory
        )
    }

[<Fact>]
let ``absence from Colombia's extract proves nothing, and two rows aren't told apart`` () : Task =
    task {
        use none = new FakeHandler(fun _ -> ok "[]")
        let! evidence = findIn (Sources.colombia (connectionOver none)) "899999068" co

        match evidence.Value with
        | NotAvailable reason -> Assert.Contains("proves nothing", reason)
        | other -> failwith $"%A{other}"

        let row = (fixture "colombia-active.json").Trim().TrimStart('[').TrimEnd(']')
        use two = new FakeHandler(fun _ -> ok $"[{row},{row}]")
        let! evidence = findIn (Sources.colombia (connectionOver two)) "899999068" co

        match evidence.Value with
        | NotAvailable reason -> Assert.Contains("2 rows", reason)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a Colombian NIT with a row cancelled by a move of domicile reads the other row, noting the move`` () : Task =
    task {
        // 901832308 as the extract answered on 2026-11-08: code 09 in Bucaramanga, then code 07 in La Guajira.
        let body = fixture "colombia-moved.json"
        use handler = new FakeHandler(fun _ -> ok body)
        let! evidence = findIn (Sources.colombia (connectionOver handler)) "901832308" co
        let company = companyOf evidence
        Assert.Equal("COSTA MAR DE INDUSALES S.A.S", company.Name)

        Assert.Equal(
            CompanyStanding.Registered(ValueSome "MATRÍCULA NUEVA, CONSTITUCIÓN POR TRASLADO"),
            company.Standing
        )

        Assert.Equal(ValueNone, company.CeasedOn)
        Assert.Contains("moved its domicile from BUCARAMANGA, matrícula 673456", company.StatusAsPublished.Value)
        Assert.Equal(ValueSome body, evidence.Raw)
        Assert.Contains("$limit=3", queryOf handler.Calls.Head)

        let rows =
            Json.JsonDocument.Parse(body).RootElement.EnumerateArray()
            |> Seq.map _.GetRawText()
            |> List.ofSeq

        use reversed = new FakeHandler(fun _ -> ok $"[{rows[1]},{rows[0]}]")
        let! evidence = findIn (Sources.colombia (connectionOver reversed)) "901832308" co
        Assert.Equal("COSTA MAR DE INDUSALES S.A.S", (companyOf evidence).Name)

        use bothMoved = new FakeHandler(fun _ -> ok $"[{rows[0]},{rows[0]}]")
        let! evidence = findIn (Sources.colombia (connectionOver bothMoved)) "901832308" co

        match evidence.Value with
        | NotAvailable reason -> Assert.Contains("2 rows", reason)
        | other -> failwith $"%A{other}"

        use three = new FakeHandler(fun _ -> ok $"[{rows[0]},{rows[1]},{rows[1]}]")
        let! evidence = findIn (Sources.colombia (connectionOver three)) "901832308" co

        match evidence.Value with
        | NotAvailable reason -> Assert.Contains("3 rows", reason)
        | other -> failwith $"%A{other}"
    }

[<Theory>]
[<InlineData("01", "Active")>]
[<InlineData("02", "Unknown")>]
[<InlineData("03", "Closed")>]
[<InlineData("04", "Unknown")>]
[<InlineData("05", "Unknown")>]
[<InlineData("06", "Unknown")>]
[<InlineData("07", "Registered")>]
[<InlineData("09", "Closed")>]
[<InlineData("10", "Closed")>]
[<InlineData("13", "Closed")>]
[<InlineData("14", "Unknown")>]
[<InlineData("15", "Closed")>]
[<InlineData("16", "Unknown")>]
[<InlineData("4", "Unknown")>]
[<InlineData("99", "Unknown")>]
let ``every Colombian status code has a standing, and a new one is unknown`` (code: string, expected: string) =
    Assert.StartsWith(expected, $"%A{Colombia.statusStanding (ValueSome code) ValueNone}")

[<Theory; InlineData("899999068", 1); InlineData("860002964", 4); InlineData("900835212", 3)>]
let ``a NIT's check digit is DIAN's`` (nit: string, digit: int) =
    Assert.Equal(digit, Colombia.checkDigit nit)

[<Fact>]
let ``a Colombian matrícula is looked up in its chamber`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "colombia-active.json"))
        let! _ = findIn (Sources.colombia (connectionOver handler)) "04/1291197" co
        Assert.Contains("codigo_camara = '04' AND matricula = '1291197'", queryOf handler.Calls.Head)
    }

[<Theory; InlineData("899999068-2"); InlineData("abc"); InlineData("1234"); InlineData("4/1291197"); InlineData("")>]
let ``a value that can't be a Colombian number isn't sent`` (number: string) : Task =
    task {
        use handler = new FakeHandler(fun _ -> failwith "Nothing should be sent")
        do! notSentWith (Sources.colombia (connectionOver handler)) handler number co
    }

[<Fact>]
let ``Colombia's legal representative is read from the row`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "colombia-active.json"))

        match! (Sources.colombia (connectionOver handler)).OfficersAsync("899999068", co, ct) with
        | Ok {
                 Value = Available [ officer ]
                 AsOf = ValueSome _
             } ->
            Assert.Equal("PERSON ONE", officer.Name)
            Assert.Equal(PartyKind.Individual, officer.Kind)
            Assert.Equal(OfficerRole.Other "Representante legal", officer.Role)
        | other -> failwith $"%A{other}"

        use none =
            new FakeHandler(fun _ -> ok """[{"razon_social":"X","nit":"899999068"}]""")

        match! (Sources.colombia (connectionOver none)).OfficersAsync("899999068", co, ct) with
        | Ok { Value = NotAvailable _ } -> ()
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``Colombia's search keeps a quote from ending the query`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "colombia-search.json"))

        match! (Sources.colombia (connectionOver handler)).SearchAsync("Ecopetrol'", co, ct) with
        | Ok { Value = Available result } ->
            Assert.Equal(3, result.Matches.Length)
            Assert.True result.Complete
            Assert.Contains("upper(razon_social) like '%E%COPETROL%'", queryOf handler.Calls.Head)
        | other -> failwith $"%A{other}"
    }

// Costa Rica

[<Fact>]
let ``Costa Rica reads a taxpayer's registration, which isn't legal standing`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "costarica-company.json"))
        let! evidence = findIn (Sources.costaRica (connectionOver handler)) "3-101-012009" cr
        let company = companyOf evidence
        Assert.Equal("Ministerio de Hacienda taxpayer situation (Costa Rica)", evidence.Source)
        Assert.True evidence.Raw.IsSome
        Assert.Equal("BANCO BAC SAN JOSE SOCIEDAD ANONIMA", company.Name)
        Assert.Equal("3101012009", company.Number)
        Assert.Equal(CompanyStanding.Registered(ValueSome "Inscrito"), company.Standing)
        Assert.False(CompanyStanding.isActive company.Standing)
        Assert.Equal(ValueSome "Inscrito; moroso NO; omiso NO", company.StatusAsPublished)
        Assert.Equal(RecordRole.NotStated, company.Role)
        Assert.Equal("/fe/ae?identificacion=3101012009", handler.Calls.Head.Target)
    }

[<Fact>]
let ``a Costa Rican taxpayer in arrears says so in its standing`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok (fixture "costarica-moroso.json"))
        let! evidence = findIn (Sources.costaRica (connectionOver handler)) "4000042139" cr
        Assert.Equal(CompanyStanding.ActiveWithConcern "Inscrito; moroso: in arrears", (companyOf evidence).Standing)
    }

[<Theory>]
[<InlineData("Inscrito", "NO", "NO", "Registered")>]
[<InlineData("Inscrito", "SI", "SI", "ActiveWithConcern")>]
[<InlineData("Inscrito", "NO", "SI", "ActiveWithConcern")>]
[<InlineData("Desinscrito", "NO", "NO", "Closed")>]
[<InlineData("Desinscrito oficio", "NO", "NO", "Closed")>]
[<InlineData("Suspendido", "NO", "NO", "Unknown")>]
let ``a Costa Rican estado has a standing, and one not seen is unknown``
    (estado: string, moroso: string, omiso: string, expected: string)
    =
    Assert.StartsWith(expected, $"%A{CostaRica.standing (ValueSome estado) (ValueSome moroso) (ValueSome omiso)}")

[<Fact>]
let ``a Costa Rican taxpayer deregistered by Hacienda is Closed with the word`` () : Task =
    task {
        // 3101101010 as Hacienda answered it on 2026-11-08.
        use handler = new FakeHandler(fun _ -> ok (fixture "costarica-desinscrito.json"))
        let! evidence = findIn (Sources.costaRica (connectionOver handler)) "3101101010" cr
        let company = companyOf evidence
        Assert.Equal(CompanyStanding.Closed "Desinscrito oficio", company.Standing)
        Assert.Equal(ValueSome "Desinscrito oficio; moroso NO; omiso NO", company.StatusAsPublished)
    }

[<Fact>]
let ``Costa Rica's 404 with its own error says Hacienda holds no such taxpayer, which isn't not held`` () : Task =
    task {
        use handler =
            new FakeHandler(fun _ -> answer 404 (fixture "costarica-not-found.json"))

        let! evidence = findIn (Sources.costaRica (connectionOver handler)) "3101999999" cr

        match evidence.Value with
        | NotAvailable reason -> Assert.Contains("Registro Nacional", reason)
        | other -> failwith $"%A{other}"

        use other = new FakeHandler(fun _ -> notFound)

        match! (Sources.costaRica (connectionOver other)).TryFindAsync("3101999999", cr, ct) with
        | Error(RegistryFailure.RequestRefused _) -> ()
        | result -> failwith $"%A{result}"
    }

[<Theory; InlineData("3-101-01200"); InlineData("1234567890"); InlineData("110450123"); InlineData(""); InlineData(null)>]
let ``a value that can't be a cédula jurídica isn't sent`` (number: string) : Task =
    task {
        use handler = new FakeHandler(fun _ -> failwith "Nothing should be sent")
        do! notSentWith (Sources.costaRica (connectionOver handler)) handler number cr
    }

[<Fact>]
let ``Costa Rica isn't asked again after a 429, since excess blocks the address`` () : Task =
    task {
        use handler =
            new FakeHandler(fun _ ->
                {
                    Status = 429
                    Body = ""
                    Headers = [ "Retry-After", "1" ]
                })

        match! (Sources.costaRica (connectionOver handler)).TryFindAsync("3101012009", cr, ct) with
        | Error(RegistryFailure.RateLimited _) -> Assert.Equal(1, handler.Calls.Length)
        | other -> failwith $"%A{other}"
    }

// Chile

[<Fact>]
let ``Chile reads a formation from the host's file, as of the file's date`` () : Task =
    task {
        let! registry = chileRegistry ()
        let! evidence = findIn registry "76.286.049-k" cl
        let company = companyOf evidence
        Assert.Equal("Registro de Empresas y Sociedades formations (Chile)", evidence.Source)
        Assert.Equal(fixedTime, evidence.RetrievedAt)
        Assert.Equal(ValueSome(DateOnly(2026, 9, 30)), evidence.AsOf)
        Assert.StartsWith("84;76286049-K;", evidence.Raw.Value)
        Assert.Equal("Servicios Digitales Enubis Cam limitada", company.Name)
        Assert.Equal("76286049-K", company.Number)
        Assert.Equal(CompanyStanding.Registered ValueNone, company.Standing)
        Assert.Equal(ValueSome "SRL", company.LegalForm)
        Assert.Equal(RecordRole.Home, company.Role)
        Assert.Equal(ValueSome(DateOnly(2013, 5, 2)), company.IncorporatedOn)

        match company.Addresses with
        | Available [ { Address = address } ] ->
            Assert.Equal(ValueSome "MAIPU", address.Locality)
            Assert.Equal(ValueSome "13", address.Region)
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``a RUT not in Chile's files proves nothing, and one that can't be a RUT isn't looked up`` () : Task =
    task {
        let! registry = chileRegistry ()
        let! evidence = findIn registry "78325555-3" cl

        match evidence.Value with
        | NotAvailable reason -> Assert.Contains("Ley 20.659", reason)
        | other -> failwith $"%A{other}"

        for number in [ "76286049-1"; "abc"; ""; null ] do
            match! registry.TryFindAsync(number, cl, ct) with
            | Error(RegistryFailure.InvalidRequest _) -> ()
            | other -> failwith $"%A{other}"
    }

[<Theory; InlineData("78325555", '3'); InlineData("76286049", 'K'); InlineData("76313428", '8')>]
let ``a RUT's check character is modulo 11`` (digits: string, expected: char) =
    Assert.Equal(expected, Chile.checkCharacter digits)

[<Fact>]
let ``a Chilean row without a name keeps its RUT, and one RUT in two files isn't told apart`` () : Task =
    task {
        let header = (fixture "chile-2013.csv").Split('\n').[0].TrimEnd('\r')

        let unnamed =
            "39452;76313428-8;;25-10-2013;25-10-2013;25-10-2013;2013;Octubre;PUENTE ALTO;13;EIRL;CONSTITUCIÓN;800000;;"

        match!
            chileFrom
                [
                    fixture "chile-2013.csv"
                    header
                    + "\r\n"
                    + unnamed
                    + "\r\n"
                    + "84;76286049-K;Other;02-05-2013;02-05-2013;02-05-2013;2013;Mayo;MAIPU;13;SRL;CONSTITUCIÓN;1;MAIPU;13"
                ]
        with
        | Ok registry ->
            let! evidence = findIn registry "76313428-8" cl
            let company = companyOf evidence
            Assert.Equal(PartyNames.NotGiven, company.Name)

            match company.Addresses with
            | NotAvailable _ -> ()
            | other -> failwith $"%A{other}"

            let! repeated = findIn registry "76286049-K" cl

            match repeated.Value with
            | NotAvailable _ -> ()
            | other -> failwith $"%A{other}"
        | Error failure -> failwith (RegistryFailure.describe failure)
    }

[<Fact>]
let ``a Chilean file without its columns, or with a short row, is unreadable`` () : Task =
    task {
        match! chileFrom [ "ID,RUT,Razon Social\r\n1,76286049-K,X" ] with
        | Error(RegistryFailure.UnreadableAnswer _) -> ()
        | other -> failwith $"%A{other}"

        match! chileFrom [ fixture "chile-2013.csv" + "\r\n84;76286049-K;X" ] with
        | Error(RegistryFailure.UnreadableAnswer reason) -> Assert.Contains("line", reason)
        | other -> failwith $"%A{other}"

        match! chileFrom [ "" ] with
        | Error(RegistryFailure.UnreadableAnswer _) -> ()
        | other -> failwith $"%A{other}"
    }

[<Fact>]
let ``Chile offers no officers or owners, searches names, and answers only for CL`` () : Task =
    task {
        let! registry = chileRegistry ()
        let! officers = registry.OfficersAsync("76286049-K", cl, ct)

        let! owners =
            registry.OwnersAsync("76286049-K", cl, OwnerPurpose.CustomerDueDiligence, ct)

        match officers, owners with
        | Ok { Value = NotAvailable _ }, Ok { Value = NotAvailable _ } -> ()
        | other -> failwith $"%A{other}"

        match! registry.SearchAsync("fast good", cl, ct) with
        | Ok { Value = Available result } -> Assert.Equal("76286055-4", result.Matches.Head.Number)
        | other -> failwith $"%A{other}"

        let! elsewhere = registry.TryFindAsync("76286049-K", tw, ct)
        Assert.Equal(Error(RegistryFailure.OutsideCoverage tw), elsewhere)
    }

// Kenya

let private kraAnswer (lookup: string) (call: Call) =
    if call.Target.StartsWith "/v1/token/generate" then
        ok (fixture "kenya-token.json")
    else
        ok lookup

[<Fact>]
let ``Kenya gets a token with the credentials in a Basic header, then checks the PIN with the token`` () : Task =
    task {
        use handler = new FakeHandler(kraAnswer (fixture "kenya-pin.json"))
        let clock = FakeClock()

        let registry =
            Sources.kenya (SourceConnection(handler, (fun () -> clock.Now), clock.Delay)) kraCredentials

        let! evidence = findIn registry "p318295670x" ke
        let company = companyOf evidence
        Assert.Equal("KRA PIN Checker (Kenya)", evidence.Source)
        Assert.True evidence.Raw.IsSome
        Assert.Equal("T. AN 052318TEST NA NA", company.Name)
        Assert.Equal("P318295670X", company.Number)
        Assert.Equal(CompanyStanding.Registered(ValueSome "PIN Active"), company.Standing)
        Assert.Equal(RecordRole.NotStated, company.Role)

        match handler.Calls with
        | [ token; check ] ->
            Assert.Equal("https://api.kra.go.ke/v1/token/generate?grant_type=client_credentials", token.Uri)

            let basic =
                Convert.ToBase64String(Encoding.UTF8.GetBytes("the-consumer-key:" + Secret))

            Assert.Equal("Basic " + basic, token.Headers["Authorization"])
            Assert.Equal("https://api.kra.go.ke/checker/v1/pinbypin", check.Uri)
            Assert.Equal("Bearer qblts2rRqMKBG7mvP7AkLZfnWjER", check.Headers["Authorization"])
        | calls -> failwith $"%A{calls}"

        let! _ = findIn registry "P318295670X" ke
        Assert.Equal(3, handler.Calls.Length)
    }

[<Fact>]
let ``Kenya's Invalid PIN isn't not held, and an individual's record isn't read`` () : Task =
    task {
        use invalid = new FakeHandler(kraAnswer (fixture "kenya-invalid.json"))

        let! evidence =
            findIn (Sources.kenya (connectionOver invalid) kraCredentials) "P318295670X" ke

        match evidence.Value with
        | NotAvailable reason -> Assert.Contains("19005", reason)
        | other -> failwith $"%A{other}"

        let individual = (fixture "kenya-pin.json").Replace("Non Individual", "Individual")
        use person = new FakeHandler(kraAnswer individual)

        let! evidence =
            findIn (Sources.kenya (connectionOver person) kraCredentials) "P318295670X" ke

        match evidence.Value with
        | NotAvailable reason -> Assert.Contains("individual", reason)
        | other -> failwith $"%A{other}"

        let another = (fixture "kenya-pin.json").Replace("P318295670X", "P000000000A")
        use wrong = new FakeHandler(kraAnswer another)

        match! (Sources.kenya (connectionOver wrong) kraCredentials).TryFindAsync("P318295670X", ke, ct) with
        | Error(RegistryFailure.UnreadableAnswer _) -> ()
        | other -> failwith $"%A{other}"
    }

[<Theory>]
[<InlineData("Active", "Registered")>]
[<InlineData("Suspended", "Unknown")>]
[<InlineData("Stopped", "Unknown")>]
[<InlineData("Cancelled", "Closed")>]
[<InlineData("Dormant", "Unknown")>]
let ``every documented PIN status has a standing, and a new one is unknown`` (status: string, expected: string) =
    Assert.StartsWith(expected, $"%A{Kenya.standing (ValueSome status)}")

[<Theory;
  InlineData("A744610021G");
  InlineData("P31829567X");
  InlineData("P3182956701");
  InlineData("");
  InlineData(null)>]
let ``a value that can't be a company's KRA PIN isn't sent`` (number: string) : Task =
    task {
        use handler = new FakeHandler(fun _ -> failwith "Nothing should be sent")
        do! notSentWith (Sources.kenya (connectionOver handler) kraCredentials) handler number ke
    }

[<Fact>]
let ``refused KRA credentials are AccessRefused, and the secret is never in a failure`` () : Task =
    task {
        use handler =
            new FakeHandler(fun _ ->
                answer 401 """{"requestId":"x","errorCode":"401","errorMessage":"Client credentials are invalid"}""")

        let registry = Sources.kenya (connectionOver handler) kraCredentials

        match! registry.TryFindAsync("P318295670X", ke, ct) with
        | Error(RegistryFailure.AccessRefused reason as failure) ->
            Assert.DoesNotContain(Secret, reason)
            Assert.DoesNotContain(Secret, RegistryFailure.describe failure)
            Assert.Equal(1, handler.Calls.Length)
        | other -> failwith $"%A{other}"

        Assert.DoesNotContain(Secret, $"%A{kraCredentials}")
        Assert.DoesNotContain(Secret, $"%A{registry.Coverage}")
    }

// Fail closed, the same for every lookup source here.

[<Theory; InlineData("not json"); InlineData("<html>Service page</html>")>]
let ``an answer that isn't JSON is unreadable, on every source`` (body: string) : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok body)

        for registry, number, jurisdiction in allRegistries handler do
            match! registry.TryFindAsync(number, jurisdiction, ct) with
            | Error(RegistryFailure.UnreadableAnswer reason) -> Assert.DoesNotContain("html", reason.ToLowerInvariant())
            | other -> failwith $"%s{registry.Coverage.Registry}: %A{other}"
    }

[<Fact>]
let ``an empty answer is unreadable on every source but Taiwan, where it proves nothing`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok "")

        for registry, number, jurisdiction in allRegistries handler do
            match! registry.TryFindAsync(number, jurisdiction, ct) with
            | Error(RegistryFailure.UnreadableAnswer _) -> Assert.NotEqual(tw, jurisdiction)
            | Ok { Value = NotAvailable _ } -> Assert.Equal(tw, jurisdiction)
            | other -> failwith $"%s{registry.Coverage.Registry}: %A{other}"
    }

[<Fact>]
let ``JSON of the wrong shape is unreadable, on every source`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> ok """{"unexpected":true}""")

        for registry, number, jurisdiction in allRegistries handler do
            match! registry.TryFindAsync(number, jurisdiction, ct) with
            | Error(RegistryFailure.UnreadableAnswer _) -> ()
            | other -> failwith $"%s{registry.Coverage.Registry}: %A{other}"
    }

[<Fact>]
let ``a record missing its name or number is unreadable`` () : Task =
    task {
        for body in [ """[{"Business_Accounting_NO":"22099131"}]"""; """[{"Company_Name":"X"}]""" ] do
            use handler = new FakeHandler(fun _ -> ok body)

            match! (Sources.taiwan (connectionOver handler)).TryFindAsync("22099131", tw, ct) with
            | Error(RegistryFailure.UnreadableAnswer _) -> ()
            | other -> failwith $"%A{other}"

        for body in [ """[{"nit":"899999068"}]"""; """[{"razon_social":"X"}]""" ] do
            use handler = new FakeHandler(fun _ -> ok body)

            match! (Sources.colombia (connectionOver handler)).TryFindAsync("899999068", co, ct) with
            | Error(RegistryFailure.UnreadableAnswer _) -> ()
            | other -> failwith $"%A{other}"

        use costaRica =
            new FakeHandler(fun _ -> ok """{"situacion":{"estado":"Inscrito"}}""")

        match! (Sources.costaRica (connectionOver costaRica)).TryFindAsync("3101012009", cr, ct) with
        | Error(RegistryFailure.UnreadableAnswer _) -> ()
        | other -> failwith $"%A{other}"

        use kenya =
            new FakeHandler(kraAnswer """{"ResponseCode":"23000","Status":"OK","PINDATA":{"KRAPIN":"P318295670X"}}""")

        match! (Sources.kenya (connectionOver kenya) kraCredentials).TryFindAsync("P318295670X", ke, ct) with
        | Error(RegistryFailure.UnreadableAnswer _) -> ()
        | other -> failwith $"%A{other}"
    }

[<Theory; InlineData(500); InlineData(503)>]
let ``a server failure is Unavailable, on every source`` (status: int) : Task =
    task {
        use handler = new FakeHandler(fun _ -> answer status "secret-body")

        for registry, number, jurisdiction in allRegistries handler do
            match! registry.TryFindAsync(number, jurisdiction, ct) with
            | Error(RegistryFailure.Unavailable reason) -> Assert.DoesNotContain("secret-body", reason)
            | other -> failwith $"%s{registry.Coverage.Registry}: %A{other}"
    }

[<Theory; InlineData(400); InlineData(403); InlineData(422); InlineData(302)>]
let ``a refusal or redirect is RequestRefused, on every source`` (status: int) : Task =
    task {
        use handler = new FakeHandler(fun _ -> answer status "secret-body")

        for registry, number, jurisdiction in allRegistries handler do
            match! registry.TryFindAsync(number, jurisdiction, ct) with
            | Error(RegistryFailure.RequestRefused reason) -> Assert.DoesNotContain("secret-body", reason)
            | other -> failwith $"%s{registry.Coverage.Registry}: %A{other}"
    }

[<Fact>]
let ``a 404 that isn't a statement is refused`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> notFound)

        for registry, number, jurisdiction in allRegistries handler do
            match! registry.TryFindAsync(number, jurisdiction, ct) with
            | Error(RegistryFailure.RequestRefused _) -> ()
            | other -> failwith $"%s{registry.Coverage.Registry}: %A{other}"
    }

[<Fact>]
let ``outside its coverage every source fails, and sends nothing`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> failwith "Nothing should be sent")
        let elsewhere = Jurisdiction.ofCode "ZZ"

        for registry, number, _ in allRegistries handler do
            let! found = registry.TryFindAsync(number, elsewhere, ct)
            Assert.Equal(Error(RegistryFailure.OutsideCoverage elsewhere), found)
            let! officers = registry.OfficersAsync(number, elsewhere, ct)
            Assert.Equal(Error(RegistryFailure.OutsideCoverage elsewhere), officers)
            let! owners = registry.OwnersAsync(number, elsewhere, OwnerPurpose.PublicRead, ct)
            Assert.Equal(Error(RegistryFailure.OutsideCoverage elsewhere), owners)
            let! search = registry.SearchAsync("x", elsewhere, ct)
            Assert.Equal(Error(RegistryFailure.OutsideCoverage elsewhere), search)

        Assert.Empty handler.Calls
    }

[<Fact>]
let ``what a source doesn't publish is NotAvailable, without calling`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> failwith "Nothing should be sent")
        let connection = connectionOver handler

        for registry, number, jurisdiction in allRegistries handler do
            match! registry.OwnersAsync(number, jurisdiction, OwnerPurpose.CustomerDueDiligence, ct) with
            | Ok {
                     Value = NotAvailable _
                     Raw = ValueNone
                 } -> ()
            | other -> failwith $"%s{registry.Coverage.Registry}: %A{other}"

        for registry, number, jurisdiction in
            [
                Sources.costaRica connection, "3101012009", cr
                Sources.kenya connection kraCredentials, "P318295670X", ke
            ] do
            let! officers = registry.OfficersAsync(number, jurisdiction, ct)
            let! search = registry.SearchAsync("x", jurisdiction, ct)

            match officers, search with
            | Ok { Value = NotAvailable _ }, Ok { Value = NotAvailable _ } -> ()
            | other -> failwith $"%s{registry.Coverage.Registry}: %A{other}"

        Assert.Empty handler.Calls
    }

[<Fact>]
let ``each source declares what it costs, its pace and its licence`` () =
    use handler = new FakeHandler(fun _ -> failwith "Nothing should be sent")

    for registry, _, jurisdiction in allRegistries handler do
        Assert.NotEmpty registry.Coverage.Licence

        match registry.Coverage.Cost with
        | AccessCost.FreeWithKey how ->
            Assert.Equal(ke, jurisdiction)
            Assert.Contains("developer.go.ke", how)
        | AccessCost.Free -> Assert.NotEqual(ke, jurisdiction)
        | other -> failwith $"%A{other}"

    let connection = connectionOver handler
    Assert.Contains("usage notice", (Sources.taiwan connection).Coverage.Licence)
    Assert.Contains("Not run live", (Sources.kenya connection kraCredentials).Coverage.Licence)

    match (Sources.costaRica connection).Coverage.RateLimit with
    | RateLimitPolicy.Published(10, per, source) ->
        Assert.Equal(TimeSpan.FromSeconds 1., per)
        Assert.Contains("10 minutes", source)
    | other -> failwith $"%A{other}"
