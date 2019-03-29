module Common.Json

open FSharp.Data

type Json = JsonValue

let toStr (json: JsonValue) = json.AsString()

let getProp (json: JsonValue) key = json.GetProperty(key)

let tryGetProp (json: JsonValue) key = json.TryGetProperty(key)

let getPropStr (json: JsonValue) key = json.GetProperty(key).AsString()

let getPropInt (json: JsonValue) key = json.GetProperty(key).AsInteger()

let getPropInt64 (json: JsonValue) key = json.GetProperty(key).AsInteger64()

let getPropStrs (json: JsonValue) key =
  json.GetProperty(key).AsArray() |> Array.map toStr

let getPropMap (json: JsonValue) key =
  json.GetProperty(key).Properties() |> Map.ofArray
  |> Map.map (fun k v -> toStr v)

let getBool (json: JsonValue) key = json.GetProperty(key).AsBoolean()

let toArray (json: JsonValue) = json.AsArray()

let loadJson (fname: string) = JsonValue.Load fname

let asyncLoadJson (fname: string) = JsonValue.AsyncLoad fname
