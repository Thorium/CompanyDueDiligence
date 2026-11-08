module CompanyDueDiligence.Web.Program

open Microsoft.AspNetCore.Components.WebAssembly.Hosting

[<EntryPoint>]
let main args =
    let builder = WebAssemblyHostBuilder.CreateDefault(args)
    builder.RootComponents.Add<Main.App>("#main")
    builder.Build().RunAsync() |> ignore
    0
