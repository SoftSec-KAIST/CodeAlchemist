module Common.Executor

open System
open System.Text
open System.Runtime.InteropServices
open System.Diagnostics

[<DllImport("lib\libexec.dll")>]
extern int readFd (int fd, byte[] buf, int size);

[<DllImport("lib\libexec.dll")>]
extern void closeFd (int fd);

[<DllImport("lib\libexec.dll")>]
extern int waitForExit (int pid);

[<DllImport("lib\libexec.dll")>]
extern void exec (int argc, string[] argv, string dir, int timeout, int [] ret);

let [<Literal>] bufSize = 4096

let readAll fd =
  let mutable buf = [||]
  let rec loop () =
    let tmp = Array.zeroCreate bufSize
    let size = readFd (fd, tmp, bufSize)
    if size < bufSize then buf <- Array.sub tmp 0 size |> Array.append buf
    else buf <- Array.append buf tmp; loop ()
  loop ()
  Encoding.UTF8.GetString buf

let execNodeJs js argv =
  let pInfo =
    ProcessStartInfo (
      FileName = "node",
      Arguments = sprintf "%s %s" js argv)
  let proc  = new Process (StartInfo = pInfo)
  proc.Start () |> ignore
  proc.WaitForExit ()

let asyncExec timeout binPath argv dir js = async {
  let ret = [|-1; -1; -1|]
  let argv = Array.append argv [|js|]
  exec (Array.length argv, argv, dir, timeout, ret)
  let retCode = waitForExit (ret.[0])
  let out = readAll ret.[1]
  let err = readAll ret.[2]
  closeFd (ret.[1])
  closeFd (ret.[2])
  return struct (retCode, out, err)
}

let getAsyncExec conf =
  let binPath = conf.BinPath
  let argv = Array.filter (String.neq "") conf.Argv |> Array.append [|binPath|]
  Map.iter (fun k v -> Environment.SetEnvironmentVariable (k, v)) conf.Env
  asyncExec conf.TimeOut binPath argv
