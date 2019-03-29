namespace Fuzzer

open Common

type Signal =
  | Normal
  | SIGILL
  | SIGABRT
  | SIGFPE
  | SIGKILL
  | SIGSEGV
  | SIGALRM
  | OTHER

module Signal =
  let ofRet = function
    | -4 -> SIGILL
    | -6 -> SIGABRT
    | -8 -> SIGFPE
    | -9 -> SIGKILL
    | -11 -> SIGSEGV
    | -14 -> SIGALRM
    | ret when ret < 0 -> OTHER
    | _ -> Normal

  let isCrash = function
    | Normal | SIGALRM | SIGKILL | SIGABRT -> false
    | _ -> true

module Oracle =
  let isCrash = Signal.ofRet >> Signal.isCrash

  let isV8Bug (struct (ret, out, err)) = isCrash ret

  let isChakraBug (struct (ret, out, err)) = isCrash ret

  let isJSCBug (struct (ret, out, err)) =
    if isCrash ret then String.contains err "WTFCrash" |> not
    else false

  let isMOZBug (struct (ret, out, err)) = isCrash ret

  let getOracle = function
    | V8 -> isV8Bug
    | Chakra -> isChakraBug
    | JSC -> isJSCBug
    | MOZ -> isMOZBug
