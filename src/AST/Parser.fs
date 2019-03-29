module AST.Parser

open System
open Common.Utils
open Common.Executor

let parseAll jsonDir saveDir =
  let path = Reflection.Assembly.GetAssembly(typeof<Program>).Location
  let js = (getDirName path) +/ "Parser.js"
  execNodeJs js (jsonDir + " " + saveDir)
