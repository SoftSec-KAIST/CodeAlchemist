module Main.Run

open Main.Preprocess
open Common
open Fuzzer
open OptParse

type Opts = {
  ProbBlk: int
  IterBlk: int
  IterMax: int
  DepthMax: int
}

let defaultOpts = {
  ProbBlk = 16
  IterBlk = 3
  IterMax = 8
  DepthMax = 3
}

let spec = [
  OptParse.Option (
    descr = "",
    extra = 1,
    callback = (fun (opts: Opts) argv -> { opts with ProbBlk = int argv.[0] }),
    long = "--pBlk"
  );

  OptParse.Option (
    descr = "",
    extra = 1,
    callback = (fun (opts: Opts) argv -> { opts with IterBlk = int argv.[0] }),
    long = "--iBlk"
  );

  OptParse.Option (
    descr = "",
    extra = 1,
    callback = (fun (opts: Opts) argv -> { opts with IterMax = int argv.[0] }),
    long = "--iMax"
  );

  OptParse.Option (
    descr = "",
    extra = 1,
    callback = (fun (opts: Opts) argv -> { opts with DepthMax = int argv.[0] }),
    long = "--dMax"
  );
]

let loadConf fname argv =
  let prog = "CodeAlchemist"
  let usage () = "[Usage]\n commands [confPath] %o"
  let _, opts =
    try optParse spec usage "CodeAlchemist" argv defaultOpts
    with
      | SpecErr msg ->
        Logger.error "Invalid spec: %s" msg
      | RuntimeErr msg ->
        Logger.errorNoExit "Invalid args given by user: %s" msg
        usagePrint spec prog usage (fun () -> exit 1)
  Conf.load fname opts.ProbBlk opts.IterBlk opts.IterMax opts.DepthMax

let reproduce conf =
  let exec = Executor.getAsyncExec conf "/"
  let oracle = Fuzzer.Oracle.getOracle conf.Engine
  let checkOne js =
    let ret = exec js |> Async.RunSynchronously
    if oracle ret then
      let struct (ret, out, err) = ret
      Logger.info "Reproducess success: %s\nSIGNAL: %d\nSTDOUT:\n%s\nSTDERR:\n%s"
        js ret out err
    else
      Logger.info "Reproduce fail: %s" js
      Utils.renameFile js (js + ".not")

  Utils.getFiles [||] conf.BugDir
  |> Array.filter (fun str -> str.EndsWith (".js"))
  |> Array.iter checkOne

[<EntryPoint>]
let main argv =
  if argv.[0] = "rewrite" then loadConf argv.[1] [||] |> rewriteAll
  elif argv.[0] = "instrument" then loadConf argv.[1] [||] |> instruments
  elif argv.[0] = "fuzz" then
    let conf = loadConf argv.[1] argv.[2..]
    loadBricks conf |> Fuzzer.fuzz conf
  elif argv.[0] = "reproduce" then loadConf argv.[1] [||] |> reproduce
  else ()
  0
