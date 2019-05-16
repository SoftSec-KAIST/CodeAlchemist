namespace Common

module Logger =
  let mkFmt color txt = sprintf "\x1b[0;%dm%s\x1b[0m" color txt
  let infoFmt = mkFmt 32 "[INFO]"
  let warnFmt = mkFmt 33 "[WARN]"
  let errorFmt = mkFmt 31 "[ERROR]"

  let printMsg (fmt: string) (msg: string) =
    System.Console.WriteLine (fmt + " " + msg)

  let printMsgExit fmt msg =
    printMsg fmt msg
    exit 1

  let info fmt = Printf.kprintf (printMsg infoFmt) fmt

  let warn fmt = Printf.kprintf (printMsg warnFmt) fmt

  let errorNoExit fmt = Printf.kprintf (printMsg errorFmt) fmt

  let error fmt = Printf.kprintf (printMsgExit errorFmt) fmt
