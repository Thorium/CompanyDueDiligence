/// The local server: the Bolero page and its API, on loopback only.
module CompanyDueDiligence.Server.App

open System
open System.IO
open System.Threading.Tasks
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.DataProtection
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.HostFiltering
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open CompanyDueDiligence
open CompanyDueDiligence.Web.Dto

/// The port the server listens on, at http://localhost only, unless --port names another.
[<Literal>]
let Port = 5190

/// The Host headers a request may carry. Any other, as a DNS rebinding attack sends, is refused with 400.
let allowedHosts = [ "localhost"; "127.0.0.1" ]

/// The data folder: the --data argument, else %LOCALAPPDATA%\CompanyDueDiligence on Windows, and $XDG_DATA_HOME/CompanyDueDiligence or
/// ~/.local/share/CompanyDueDiligence elsewhere.
let dataFolder (given: string voption) =
    match given with
    | ValueSome folder when not (String.IsNullOrWhiteSpace folder) -> Path.GetFullPath folder
    | _ when OperatingSystem.IsWindows() ->
        Path.Combine(Environment.GetFolderPath Environment.SpecialFolder.LocalApplicationData, "CompanyDueDiligence")
    | _ ->
        let root =
            match Environment.GetEnvironmentVariable "XDG_DATA_HOME" with
            | null
            | "" -> Path.Combine(Environment.GetFolderPath Environment.SpecialFolder.UserProfile, ".local", "share")
            | xdg -> xdg

        Path.Combine(root, "CompanyDueDiligence")

/// The header every API call but api/mode carries the access token in.
[<Literal>]
let TokenHeader = "X-CDD-Token"

/// The access token in the data folder's token file, made on the first start with --token: a request to the API carries it, so that another account on this
/// machine, which can reach localhost but can't read the owner's data folder or browser, can't use the keys.
let accessToken (data: string) =
    let path = Path.Combine(data, "token")

    match
        (if File.Exists path then
             File.ReadAllText(path).Trim()
         else
             "")
    with
    | token when token.Length >= 32 -> token
    | _ ->
        let token =
            Convert
                .ToBase64String(Security.Cryptography.RandomNumberGenerator.GetBytes 32)
                .TrimEnd('=')
                .Replace('+', '-')
                .Replace('/', '_')

        File.WriteAllText(path, token)

        if not (OperatingSystem.IsWindows()) then
            File.SetUnixFileMode(path, UnixFileMode.UserRead ||| UnixFileMode.UserWrite)

        token

/// Lets an API call through only with the access token; api/mode answers without it, as the page asks it to tell the server from GitHub Pages.
let private withToken (token: string) =
    let expected = Text.Encoding.UTF8.GetBytes token

    Func<HttpContext, RequestDelegate, Task>(fun context next ->
        let path = context.Request.Path

        let exempt =
            not (path.StartsWithSegments(PathString "/api"))
            || path.Equals(PathString "/api/mode", StringComparison.OrdinalIgnoreCase)

        let given =
            Text.Encoding.UTF8.GetBytes(context.Request.Headers[TokenHeader].ToString())

        if
            exempt
            || Security.Cryptography.CryptographicOperations.FixedTimeEquals(given, expected)
        then
            next.Invoke context
        else
            context.Response.StatusCode <- StatusCodes.Status401Unauthorized

            context.Response.WriteAsJsonAsync
                {
                    ErrorDto.Error =
                        "This page needs the server's access token: open the address the server printed when it started, http://localhost:<port>/?token=…"
                })

/// Lets a request that changes something through only from this page: a cross-site page can send a simple POST to localhost, which the
/// browser marks with its Origin and Sec-Fetch-Site.
let private sameOrigin =
    Func<HttpContext, RequestDelegate, Task>(fun context next ->
        let request = context.Request

        let reads =
            HttpMethods.IsGet request.Method
            || HttpMethods.IsHead request.Method
            || HttpMethods.IsOptions request.Method

        let origin = request.Headers.Origin.ToString()
        let site = request.Headers["Sec-Fetch-Site"].ToString()
        let own = $"{request.Scheme}://{request.Host}"

        let fromThisPage =
            (origin = "" || String.Equals(origin, own, StringComparison.OrdinalIgnoreCase))
            && (site = "" || site = "same-origin" || site = "none")

        if reads || fromThisPage then
            next.Invoke context
        else
            context.Response.StatusCode <- StatusCodes.Status403Forbidden

            context.Response.WriteAsJsonAsync
                {
                    ErrorDto.Error = "Only this server's own page may change its keys and files"
                })

/// The server for the command line's arguments: --data <dir> sets the data folder, --port <n> the port at localhost, and --token makes every
/// API call but api/mode carry the access token, for a computer shared with other accounts. configure runs on the
/// builder last, as a test host does.
let create (args: string array) (configure: WebApplicationBuilder -> unit) =
    // A flag with no value, taken out before configuration reads the rest as --name value pairs.
    let withAccessToken = args |> Array.contains "--token"
    let args = args |> Array.filter (fun arg -> arg <> "--token")

    let builder =
        WebApplication.CreateBuilder(WebApplicationOptions(Args = args, ContentRootPath = AppContext.BaseDirectory))

    let data = dataFolder (builder.Configuration["data"] |> ValueOption.ofObj)
    Directory.CreateDirectory data |> ignore

    if not (OperatingSystem.IsWindows()) then
        File.SetUnixFileMode(data, UnixFileMode.UserRead ||| UnixFileMode.UserWrite ||| UnixFileMode.UserExecute)

    // The client's files, from its build output when run from source, or from wwwroot when published.
    builder.WebHost.UseStaticWebAssets() |> ignore
    builder.Logging.AddFilter("Microsoft.AspNetCore", LogLevel.Warning) |> ignore

    let port =
        match Int32.TryParse(builder.Configuration["port"]) with
        | true, port when port > 0 && port < 65536 -> port
        | _ -> Port

    builder.WebHost.ConfigureKestrel(fun options -> options.ListenLocalhost port)
    |> ignore

    builder.Services.Configure<HostFilteringOptions>(fun (options: HostFilteringOptions) ->
        options.AllowedHosts <- ResizeArray allowedHosts
        options.AllowEmptyHosts <- false)
    |> ignore

    let protection =
        builder.Services
            .AddDataProtection()
            .SetApplicationName("CompanyDueDiligence")
            .PersistKeysToFileSystem(DirectoryInfo(Path.Combine(data, "protection")))

    if OperatingSystem.IsWindows() then
        protection.ProtectKeysWithDpapi() |> ignore

    configure builder
    let app = builder.Build()

    let protector =
        app.Services.GetRequiredService<IDataProtectionProvider>().CreateProtector "CompanyDueDiligence.Keys"

    let keys = KeyStore(Path.Combine(data, "keys.json"), protector)
    let bulk = new BulkService(Path.Combine(data, "bulk"), app.Logger)
    let connection = SourceConnection.Create()
    let available = ServerRegisters(connection, keys, bulk)

    app.Lifetime.ApplicationStopped.Register(fun () -> (bulk :> IDisposable).Dispose())
    |> ignore

    app.Logger.LogInformation("Data folder: {Folder}", data)
    // On the console only, not in the log: with --token the address carries the access token.
    let token =
        if withAccessToken then
            ValueSome(accessToken data)
        else
            ValueNone

    let address =
        match token with
        | ValueSome token -> $"http://localhost:{port}/?token={token}"
        | ValueNone -> $"http://localhost:{port}"

    app.Lifetime.ApplicationStarted.Register(fun () -> Console.WriteLine $"Open {address}")
    |> ignore

    app.Use sameOrigin |> ignore

    match token with
    | ValueSome token -> app.Use(withToken token) |> ignore
    | ValueNone -> ()

    app.UseBlazorFrameworkFiles() |> ignore
    app.UseStaticFiles() |> ignore
    Api.map app connection keys bulk available
    app.MapFallbackToFile "index.html" |> ignore
    app
