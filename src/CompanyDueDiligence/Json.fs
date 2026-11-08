/// Readers over JsonElement. An optional reader treats a missing property, JSON null and a value of another type alike.
module internal CompanyDueDiligence.Json

open System
open System.Globalization
open System.Text.Json

/// A required field is missing or has the wrong type. The message names the field only.
exception InvalidJson of string

let tryProp (name: string) (e: JsonElement) =
    if e.ValueKind <> JsonValueKind.Object then
        ValueNone
    else
        match e.TryGetProperty name with
        | true, value when
            value.ValueKind <> JsonValueKind.Null
            && value.ValueKind <> JsonValueKind.Undefined
            ->
            ValueSome value
        | _ -> ValueNone

let tryString name e =
    tryProp name e
    |> ValueOption.bind (fun v ->
        if v.ValueKind = JsonValueKind.String then
            ValueSome(v.GetString())
        else
            ValueNone)

/// A string or a number as text, for identifiers the spec and answers type either way.
let tryText name e =
    tryProp name e
    |> ValueOption.bind (fun v ->
        match v.ValueKind with
        | JsonValueKind.String -> ValueSome(v.GetString())
        | JsonValueKind.Number -> ValueSome(v.GetRawText())
        | _ -> ValueNone)

/// A number, or a string holding one.
let tryInt name e =
    tryText name e
    |> ValueOption.bind (fun text ->
        match Int32.TryParse(text, NumberStyles.Integer, CultureInfo.InvariantCulture) with
        | true, n -> ValueSome n
        | _ -> ValueNone)

let tryObject name e =
    tryProp name e
    |> ValueOption.bind (fun v ->
        if v.ValueKind = JsonValueKind.Object then
            ValueSome v
        else
            ValueNone)

/// A "yyyy-MM-dd" date.
let tryDate name e =
    tryString name e
    |> ValueOption.bind (fun text ->
        match DateOnly.TryParseExact(text, "yyyy-MM-dd", CultureInfo.InvariantCulture, DateTimeStyles.None) with
        | true, date -> ValueSome date
        | _ -> ValueNone)

let tryTimestamp name e =
    tryString name e
    |> ValueOption.bind (fun text ->
        match DateTimeOffset.TryParse(text, CultureInfo.InvariantCulture, DateTimeStyles.AssumeUniversal) with
        | true, at -> ValueSome at
        | _ -> ValueNone)

/// A "yyyy-MM-dd" date that decides something, such as whether an appointment has ended. ValueNone when missing or null; anything else that isn't
/// a date is refused, so that an end date that can't be read doesn't read as no end.
let dateIfAny name e =
    match tryProp name e with
    | ValueNone -> ValueNone
    | ValueSome _ ->
        match tryDate name e with
        | ValueSome date -> ValueSome date
        | ValueNone -> raise (InvalidJson $"Field '{name}' is not a date")

/// ValueNone when missing or null, which says nothing either way. Any other non-boolean is refused.
let tryFlag name e =
    match tryProp name e with
    | ValueNone -> ValueNone
    | ValueSome v when v.ValueKind = JsonValueKind.True -> ValueSome true
    | ValueSome v when v.ValueKind = JsonValueKind.False -> ValueSome false
    | ValueSome _ -> raise (InvalidJson $"Field '{name}' is not a boolean")

/// For an indicator whose presence is what counts: false when missing or null. Any other non-boolean is refused.
let flag name e =
    tryFlag name e |> ValueOption.defaultValue false

/// Elements of an array property; empty when it is missing or null.
let array e name =
    match tryProp name e with
    | ValueSome v when v.ValueKind = JsonValueKind.Array -> v.EnumerateArray() |> List.ofSeq
    | _ -> []

/// The elements of a property sent as one object or as a list of them. The spec types several object fields as lists.
let oneOrMany name e =
    match tryProp name e with
    | ValueSome v when v.ValueKind = JsonValueKind.Array -> v.EnumerateArray() |> List.ofSeq
    | ValueSome v when v.ValueKind = JsonValueKind.Object -> [ v ]
    | _ -> []

let strings e name =
    array e name
    |> List.choose (fun v ->
        if v.ValueKind = JsonValueKind.String then
            Some(v.GetString())
        else
            None)

let required name (reader: string -> JsonElement -> 'T voption) e =
    match reader name e with
    | ValueSome value -> value
    | ValueNone -> raise (InvalidJson $"Missing or invalid field '{name}'")
