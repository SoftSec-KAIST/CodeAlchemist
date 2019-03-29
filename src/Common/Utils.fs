module Common.Utils

open System.IO
open System.Text

let rec getFiles ret dir =
  let dir = new DirectoryInfo (dir)
  let dirs = dir.GetDirectories () |> Array.map (fun dir -> dir.FullName)
  let files = dir.GetFiles () |> Array.map (fun file -> file.FullName)
  Array.fold getFiles (Array.append files ret) dirs

let inline writeFile name (data: string) =
  File.WriteAllBytes (name, Encoding.UTF8.GetBytes data)

let inline rmFile name = File.Delete (name)

let getFileName path = Path.GetFileName (path)

let getDirName path = Path.GetDirectoryName (path)

let existDir dir = Directory.Exists dir

let (+/)  p1 p2 = Path.Combine (p1, getFileName p2)

let mkDir dir = Directory.CreateDirectory dir |> ignore; dir

let checkDir path =
  let dir = getDirName path
  if existDir dir then path
  else mkDir dir |> ignore; path

let renameFile src dst = File.Move (src, dst)
