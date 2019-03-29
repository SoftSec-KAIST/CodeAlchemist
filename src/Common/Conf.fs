namespace Common

open Common.Json

type Engine =
  | V8
  | Chakra
  | JSC
  | MOZ

type Conf = {
  Engine: Engine
  TimeOut: int
  BinPath: string
  Argv: string array
  Env: Map<string, string>
  SeedDir: string
  TmpDir: string
  BugDir: string
  PreprocDir: string
  BuiltIns: string array
  Filters: string array
  Jobs: int
  ProbBlk: int
  IterBlk: int
  IterMax: int
  DepthMax: int
}

module Conf =
  let private toEngine = function
    | "V8" -> V8
    | "Chakra" -> Chakra
    | "JSC" -> JSC
    | "MOZ" -> MOZ
    | e -> failwithf "Not supported engine: %s" e

  let private getProp json key =
    match tryGetProp json key with
    | Some v -> v
    | None -> failwithf "%s field is required" key

  let load fname pBlk iBlk iMax dMax =
    let json = loadJson fname
    {
      Engine = getPropStr json "engine" |> toEngine
      TimeOut = getPropInt json "timeout"
      BinPath = getPropStr json "engine_path"
      Argv = getPropStrs json "argv"
      Env = getPropMap json "env"
      SeedDir = getPropStr json "seed_path"
      TmpDir = getPropStr json "tmp_dir"
      BugDir = getPropStr json "bug_dir"
      PreprocDir = getPropStr json "preproc_dir"
      BuiltIns = getPropStrs json "built-ins"
      Filters = getPropStrs json "filters"
      Jobs = getPropInt json "jobs"
      ProbBlk = pBlk
      IterBlk = iBlk
      IterMax = iMax
      DepthMax = dMax
    }

  let engineToStr = function
    | V8 -> "V8"
    | Chakra -> "Chakra"
    | JSC -> "JSC"
    | MOZ -> "MOZ"
