module Main.Preprocess

open Common
open Common.Utils
open AST.Parser
open AST.Loader
open AST.CodeGen
open Analyzer
open Analyzer.Instrument

let parseSeed conf =
  let jsonDir = conf.PreprocDir +/ "json"
  if existDir jsonDir then ()
  else mkDir jsonDir |> parseAll conf.SeedDir
  loads jsonDir

let rewrite rewriteDir loader ctx (name, ast) = async {
  let rewriteName = rewriteDir +/ name
  rewriteProg ctx ast |> fst |> progToCode Map.empty
  |> String.add loader
  |> writeFile rewriteName
}

let rewrites conf asts =
  let rewriteDir = conf.PreprocDir +/ "rewrite" |> mkDir
  let loader = mkLoader conf.Engine
  let ctx = Context.init conf
  Array.map (rewrite rewriteDir loader ctx) asts
  |> Async.Parallel
  |> Async.RunSynchronously
  |> ignore

[<Literal>]
let startKey = "=== CodeAlchemist Start ==="

[<Literal>]
let endKey = "=== CodeAlchemist End ==="


let splitFst (key: string) (target: string) =
  let target = target.Split key
  if Array.length target > 1 then Array.get target 0
  else ""

let parseResult (out: string) =
  let out = out.Split startKey
  if Array.length out > 1 then
    Array.map (splitFst endKey) out
    |> Array.filter (String.neq "")
    |> String.concat "," |> sprintf "[%s]"
  else "[]"

let instruments conf =
  let rewriteDir = conf.PreprocDir +/ "rewrite"
  let outDir = conf.PreprocDir +/ "type" |> mkDir
  let exec = Executor.getAsyncExec conf rewriteDir
  let instrument js = async {
    let! struct (_, out, _) = exec js
    writeFile (outDir +/ js) (parseResult out)
  }
  getFiles [||] rewriteDir
  |> Array.map instrument
  |> Async.Parallel
  |> Async.RunSynchronously
  |> ignore

let rewriteAll conf = parseSeed conf |> rewrites conf

let asyncFrag ctx (_, ast) = async {
  return rewriteProg ctx ast |> snd
}

let fragmentize conf asts =
  let folder (pool, gMap) (p1, g1) = Map.merge pool p1, Map.merge gMap g1
  let ctx = Context.init conf
  Array.map (asyncFrag ctx) asts
  |> Async.Parallel
  |> Async.RunSynchronously
  |> Array.fold folder (Map.empty, Map.empty)

let parseTypes json key =
  Json.getPropMap json key |> Map.mapVal JSType.ofStr

let parseJson ret json =
  try
    let res = Json.getPropInt64 json "hval", parseTypes json "pre",
              parseTypes json "post"
    Set.add res ret
  with e -> eprintfn "%A" e; ret

let loadType fname = async {
  let! json = Json.asyncLoadJson fname
  return Json.toArray json |> Array.fold parseJson Set.empty
}

let loadTypes dir =
  getFiles [||] dir
  |> Array.map loadType
  |> Async.Parallel
  |> Async.RunSynchronously
  |> Array.fold Set.union Set.empty

let loadConst pool gMap ret (hval, pre, post) =
  let loader = CodeBrick.loadTypes pre post
  match Map.tryFind hval pool, Map.tryFind hval gMap with
  | Some brick, Some gHash ->
    let guard = Map.find gHash pool
    (loader guard) :: (loader brick) :: ret
  | Some brick, None -> (loader brick) :: ret
  | None, _ -> failwithf "%d not found" hval

let loadConsts dir (pool, gMap)=
  loadTypes dir
  |> Set.fold (loadConst pool gMap) []

let loadBricks conf =
  let jsonDir = conf.PreprocDir +/ "json"
  let typeDir = conf.PreprocDir +/ "type"
  loads jsonDir
  |> fragmentize conf
  |> loadConsts typeDir
