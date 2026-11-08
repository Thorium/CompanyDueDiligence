module CompanyDueDiligence.Server.Program

[<EntryPoint>]
let main args =
    let app = App.create args ignore
    app.Run()
    0
