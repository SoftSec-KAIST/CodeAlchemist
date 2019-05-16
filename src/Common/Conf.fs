namespace Common

open System
open System.Diagnostics
open Common.Json
open Common.Utils

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
  let private builtInGetter =
    let path = Reflection.Assembly.GetAssembly(typeof<Conf>).Location
    (getDirName path) +/ "BuiltInGetter.js"

  let private toEngine = function
    | "V8" -> V8
    | "Chakra" -> Chakra
    | "JSC" -> JSC
    | "MOZ" -> MOZ
    | e -> Logger.error "Not supported engine: %s" e

  let private getProp json key =
    match tryGetProp json key with
    | Some v -> v
    | None -> Logger.error "%s field is required" key

  let private getBuiltIns binPath argv =
    let argv = Array.append argv [|builtInGetter|]
    let pInfo =
      ProcessStartInfo (
        FileName = binPath,
        Arguments = String.concat " " argv,
        RedirectStandardInput = true,
        RedirectStandardOutput = true,
        RedirectStandardError = true,
        UseShellExecute = false)
    let proc  = new Process (StartInfo = pInfo)
    proc.Start () |> ignore
    proc.WaitForExit ()
    let ret =
      proc.StandardOutput.ReadToEnd().Split '\n'
      |> Array.filter (String.neq "")
    String.concat ", " ret |> Logger.info "BuiltIns:\n[| %s |]"
    ret

  let load fname pBlk iBlk iMax dMax =
    let json = loadJson fname
    let binPath = getPropStr json "engine_path"
    let argv = getPropStrs json "argv"
    {
      Engine = getPropStr json "engine" |> toEngine
      TimeOut = getPropInt json "timeout"
      BinPath = binPath
      Argv = argv
      Env = getPropMap json "env"
      SeedDir = getPropStr json "seed_path"
      TmpDir = getPropStr json "tmp_dir"
      BugDir = getPropStr json "bug_dir"
      PreprocDir = getPropStr json "preproc_dir"
      BuiltIns = getBuiltIns binPath argv
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
