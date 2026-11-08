namespace CompanyDueDiligence.Server

open System
open System.Collections.Generic
open System.IO
open System.Security.Cryptography
open System.Text.Json
open Microsoft.AspNetCore.DataProtection

/// The keys saved from Settings, in keys.json: a JSON object of setting name to value, each value protected with ASP.NET Core data protection.
/// On Windows the protection keys are encrypted with DPAPI for the user running the server; elsewhere they are plain files beside keys.json, so
/// the values are only as private as the data folder, which is made readable by its owner alone. A value that can't be unprotected reads as
/// not saved. Values never leave this type except to build a registry.
type KeyStore(path: string, protector: IDataProtector) =

    let gate = obj ()

    let read () =
        if File.Exists path then
            JsonSerializer.Deserialize<Dictionary<string, string>>(File.ReadAllText path)
            |> Option.ofObj
            |> Option.map (Seq.map (fun pair -> pair.Key, pair.Value) >> Map.ofSeq)
            |> Option.defaultValue Map.empty
        else
            Map.empty

    let write (values: Map<string, string>) =
        Directory.CreateDirectory(Path.GetDirectoryName path) |> ignore
        let part = path + ".part"

        File.WriteAllText(
            part,
            JsonSerializer.Serialize(Dictionary values, JsonSerializerOptions(WriteIndented = true))
        )

        if not (OperatingSystem.IsWindows()) then
            File.SetUnixFileMode(part, UnixFileMode.UserRead ||| UnixFileMode.UserWrite)

        File.Move(part, path, true)

    let unprotect (stored: string) =
        try
            ValueSome(protector.Unprotect stored)
        with :? CryptographicException ->
            ValueNone

    /// The saved value of a setting, unprotected.
    member _.TryGet(name: string) =
        lock gate (fun () ->
            read ()
            |> Map.tryFind name
            |> ValueOption.ofOption
            |> ValueOption.bind unprotect)

    /// True when a value is saved for the setting and can be read.
    member this.IsSaved(name: string) = (this.TryGet name).IsSome

    member _.Save(name: string, value: string) =
        lock gate (fun () -> read () |> Map.add name (protector.Protect value) |> write)

    member _.Remove(name: string) =
        lock gate (fun () ->
            let values = read ()

            if values.ContainsKey name then
                write (values.Remove name))
