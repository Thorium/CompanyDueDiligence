/// Building the sources behind a key from named settings.
module KeyedSettingsTests

open System
open System.Net.Http
open System.Threading.Tasks
open Xunit
open CompanyDueDiligence
open Fake

let private connection () =
    SourceConnection(new FakeHandler(fun _ -> failwith "Nothing should be sent"))

let private settingsOf (pairs: (string * string) list) =
    let held = Map.ofList pairs
    fun name -> held.TryFind name |> ValueOption.ofOption

[<Fact>]
let ``no settings make no registry, and every source is listed as not configured`` () =
    let result = KeyedSources.fromSettings (connection ()) (settingsOf [])
    Assert.Empty result.Registries
    Assert.Empty result.Incomplete
    Assert.Equal(14, result.NotConfigured.Length)

[<Fact>]
let ``a key and a full pair each make their registry`` () =
    let result =
        KeyedSources.fromSettings
            (connection ())
            (settingsOf
                [
                    "CDD_CANADA_KEY", "canada-key"
                    "CDD_SWEDEN_CLIENT_ID", "id"
                    "CDD_SWEDEN_CLIENT_SECRET", "secret"
                ])

    let names = result.Registries |> List.map _.Coverage.Jurisdictions
    Assert.Equal(2, result.Registries.Length)
    Assert.Contains([ Jurisdiction.ofCode "CA" ], names)
    Assert.Contains([ Jurisdiction.ofCode "SE" ], names)
    Assert.DoesNotContain("Corporations Canada", result.NotConfigured)

[<Fact>]
let ``half a pair makes no registry, and names the setting missing without its value`` () =
    let result =
        KeyedSources.fromSettings (connection ()) (settingsOf [ "CDD_DENMARK_USER", "the-user-value" ])

    Assert.Empty result.Registries
    let problem = Assert.Single result.Incomplete
    Assert.Contains("CDD_DENMARK_PASSWORD", problem)
    Assert.DoesNotContain("the-user-value", problem)

[<Fact>]
let ``a blank setting counts as not given`` () =
    let result =
        KeyedSources.fromSettings (connection ()) (settingsOf [ "CDD_GREECE_KEY", "   " ])

    Assert.Empty result.Registries
    Assert.Contains("GEMI", result.NotConfigured)

[<Fact>]
let ``Companies House is built from its key`` () =
    let result =
        KeyedSources.fromSettings (connection ()) (settingsOf [ "CDD_COMPANIES_HOUSE_KEY", "a-key" ])

    let registry = Assert.Single result.Registries
    Assert.Contains(Jurisdiction.ofCode "GB", registry.Coverage.Jurisdictions)

[<Fact>]
let ``every setting name is distinct and shaped as an environment variable`` () =
    Assert.Equal(KeyedSources.settingNames.Length, (List.distinct KeyedSources.settingNames).Length)

    for name in KeyedSources.settingNames do
        Assert.Matches(@"\ACDD_[A-Z_]+\z", name)

[<Fact>]
let ``a registry built from settings sends its key, and a failure doesn't quote it`` () : Task =
    task {
        use handler = new FakeHandler(fun _ -> answer 500 "down")

        let result =
            KeyedSources.fromSettings (SourceConnection handler) (settingsOf [ "CDD_CANADA_KEY", "the-canada-key" ])

        let registry = Assert.Single result.Registries

        match! registry.TryFindAsync("1234567", Jurisdiction.ofCode "CA", ct) with
        | Error failure -> Assert.DoesNotContain("the-canada-key", RegistryFailure.describe failure)
        | Ok other -> failwith $"%A{other}"

        Assert.Equal("the-canada-key", handler.Calls.Head.Headers["user-key"])
    }

/// A handler passing everything to the one it wraps.
type private Passing(inner: HttpMessageHandler) =
    inherit DelegatingHandler(inner)

[<Fact>]
let ``a handler that follows redirects is refused, as it would carry a key to another host`` () =
    Assert.Throws<ArgumentException>(fun () -> SourceConnection(new HttpClientHandler()) |> ignore)
    |> ignore

    Assert.Throws<ArgumentException>(fun () ->
        SourceConnection(new SocketsHttpHandler(AllowAutoRedirect = true)) |> ignore)
    |> ignore

    Assert.Throws<ArgumentException>(fun () -> SourceConnection(new Passing(new HttpClientHandler())) |> ignore)
    |> ignore

    SourceConnection(new HttpClientHandler(AllowAutoRedirect = false)) |> ignore

    SourceConnection(new Passing(new SocketsHttpHandler(AllowAutoRedirect = false)))
    |> ignore

    SourceConnection.Create() |> ignore

[<Theory; InlineData("overflow"); InlineData("range"); InlineData("format")>]
let ``a value out of range or of the wrong form in an answer is unreadable, not thrown`` (fault: string) =
    let reply: SourceReply =
        {
            Status = 200
            Body = "{}"
            At = DateTimeOffset.UnixEpoch
            LastModified = ValueNone
        }

    let parse (_: Text.Json.JsonElement) : int =
        match fault with
        | "overflow" -> raise (OverflowException())
        | "range" -> raise (ArgumentOutOfRangeException())
        | _ -> raise (FormatException())

    match SourceAnswer.json reply parse with
    | Error(RegistryFailure.UnreadableAnswer _) -> ()
    | other -> failwith $"%A{other}"
