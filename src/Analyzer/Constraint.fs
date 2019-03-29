namespace Analyzer

open AST
open Common

type ScopeLv =
  | Block
  | Func
  | Glob
  | Pre

type PreCond = Map<Id, JSType>

type PostCond = Map<Id, JSType * ScopeLv>

type SyntaxCond = {
  Func: bool
  Try: bool
  Loop: bool
  Class: bool
  Gen: bool
  Async: bool
}

type Constraint = {
  Pre: PreCond
  Post: PostCond
  Syntax: SyntaxCond
}

module Constraint =
  let emptySyntax = {
    Func = false
    Try = false
    Loop = false
    Class = false
    Gen = false
    Async = false
  }

  let empty = {
    Pre = Map.empty
    Post = Map.empty
    Syntax = emptySyntax
  }

  let isEmptyVars cons = cons.Pre = Map.empty && cons.Post = Map.empty

  let isSubSyntax big small =
    (big.Func || not small.Func) &&
    (big.Try || not small.Try) &&
    (big.Loop || not small.Loop) &&
    (big.Class || not small.Class) &&
    (big.Gen || not small.Gen) &&
    (big.Async || not small.Async)

  let initPre id ty = { empty with Pre = Map.add id ty  Map.empty}

  let addPost id ty lv cons = { cons with Post = Map.add id (ty, lv) cons.Post }

  let addUndefPost lv post id = Map.add id (Undef, lv) post

  let addUndefPre pre id = Map.add id Undef pre

  let addOut cons lv out =
    { cons with Post = Set.fold (addUndefPost lv) cons.Post out }

  let addInOut out lv cons =
    { cons with Post = Set.fold (addUndefPost lv) cons.Post out
                Pre = Set.fold addUndefPre cons.Pre out }

  let initLabel = function
    | Some label -> { empty with Pre = Map.add label Label Map.empty
                                 Syntax = { emptySyntax with Loop = true } }
    | None -> { empty with Syntax = {emptySyntax with Loop = true } }

  let addLabel cons label = addPost label JSType.Label Glob cons

  let initId isBuiltIn id =
    if isBuiltIn id then empty
    else { empty with Pre = Map.add id Undef Map.empty
                      Post = Map.add id (Undef, Pre) Map.empty }

  let setFunc cons = { cons with Syntax = { cons.Syntax with Func = true } }

  let unsetFunc isGen isAsync cons =
    let syntax = cons.Syntax
    let syntax = { syntax with Func = false
                               Gen = syntax.Gen && (not isGen)
                               Async = syntax.Async && (not isAsync) }
    { cons with Syntax = syntax }

  let setTry cons = { cons with Syntax = { cons.Syntax with Try = true } }
  let unsetTry cons = { cons with Syntax = { cons.Syntax with Try = false } }

  let setLoop cons = { cons with Syntax = { cons.Syntax with Loop = true } }
  let unsetLoop cons = { cons with Syntax = { cons.Syntax with Loop = false } }

  let setClass cons = { cons with Syntax = { cons.Syntax with Class = true } }
  let unsetClass cons = { cons with Syntax = { cons.Syntax with Class = false } }

  let setGen cons = { cons with Syntax = { cons.Syntax with Gen = true } }

  let setAsync cons = { cons with Syntax = { cons.Syntax with Async = true } }

  let super = { empty with Syntax = { emptySyntax with Class = true } }
  let metaProp = { empty with Syntax = { emptySyntax with Func = true } }
  let this = metaProp

  let mergeSyntax s1 s2 = {
    Func = s1.Func || s2.Func
    Try = s1.Try || s2.Try
    Loop = s1.Loop || s2.Loop
    Class = s1.Class || s2.Class
    Gen = s1.Gen || s2.Gen
    Async = s1.Async || s2.Async
  }

  let glue cons1 cons2 = {
    Pre = Map.getKeys cons1.Post |> Map.delKeys cons2.Pre |> Map.merge cons1.Pre
    Post = Map.merge cons1.Post cons2.Post
    Syntax = mergeSyntax cons1.Syntax cons2.Syntax
  }

  let union cons1 cons2 = {
    Pre = Map.merge cons1.Pre cons2.Pre
    Post = Map.merge cons1.Post cons2.Post
    Syntax = mergeSyntax cons1.Syntax cons2.Syntax
  }

  let normalize cons map =
    { cons with Pre = Map.mapKey (Map.get map) cons.Pre
                Post = Map.mapKey (Map.get map) cons.Post }

  let rmPost id cons = { cons with Post = Map.remove id cons.Post }

  let logPre = Expr.Id "codealchemist_log_type_pre" |> Callee.Expr

  let logPost = Expr.Id "codealchemist_log_type_post" |> Callee.Expr

  let undef = Expr.Id "undefined"
  let undefStr = Literal.String "'undefined'" |> Expr.Literal

  let mkLoggerVal id =
    let test = Binary (BinOp.Neq, Unary (UnOp.TypeOf, id), undefStr)
    Cond (test, id, undef) |> PropVal.Expr

  let mkLoggerVar map id =
    let nId = Map.find id map |> Expr.Id
    nId, Expr.Id id |> mkLoggerVal, PropKind.Init, false, false

  let mkLogger func hval map cond =
    let vars = Map.filterVal JSType.isUndef cond |> Map.getKeys
               |> Array.map (mkLoggerVar map)
               |> Expr.Object |> Arg.Expr
    Stmt.Expr (Expr.Call (func, [|hval; vars|]), None)

  let getLogger (hval: int64) map cons =
    let hval = sprintf "'%d'" hval |> Literal.String |> Expr.Literal |> Arg.Expr
    mkLogger logPre hval map cons.Pre,
    Map.mapVal fst cons.Post |> mkLogger logPost hval map

  let assign op out cons =
    match op with
    | AssignOp.Assign -> addOut cons Pre out
    | _ -> addInOut out Pre cons

  let filterPost tlv _ (_, lv) = tlv <> lv

  let filtPost lv cons =
    { cons with Post = Map.filter (filterPost lv) cons.Post }

  let finiBlock cons = filtPost Block cons

  let finiFuncExpr id isGen isAsync params_ cons1 cons2 =
    let cons1 =
      match id with
      | Some id -> Set.add id params_
      | None -> params_
      |> Set.add "arguments" |> addOut cons1 Func
    glue cons1 { cons2 with Post = Map.empty }
    |> filtPost Func |> unsetFunc isGen isAsync

  let finiFuncDecl id isGen isAsync params_ cons1 cons2 =
    finiFuncExpr (Some id) isGen isAsync params_ cons1 cons2
    |> addPost id JSType.Function Func

  let finiFor cons = finiBlock cons |> unsetLoop

  let finiSwitch = finiFor

  let finiTry cons = finiBlock cons |> unsetTry

  let finiVarDecl kind out cons =
    let lv = match kind with
             | Let -> Block
             | _ -> Func
    addOut cons lv out

  let finiClass id cons = addPost id JSType.Object Func cons |> unsetClass

  let finiClassExpr id cons1 cons2 =
    match id with
    | Some id -> glue (addPost id JSType.Object Func cons1) cons2 |> rmPost id
    | None -> glue cons1 cons2
    |> unsetClass

  let finiCatch out cons1 cons2 =
    glue (addOut cons1 Block out) cons2 |> finiBlock

  let loadTypePost post id ty =
    match Map.tryFind id post with
    | Some (_, scope) -> Map.add id (ty, scope) post
    | None -> post

  let loadTypePre pre id ty =
    match Map.tryFind id pre with
    | Some _ -> Map.add id ty pre
    | _ -> pre

  let loadTypes cons pre post =
    { cons with Pre = Map.fold loadTypePre cons.Pre pre
                Post = Map.fold loadTypePost cons.Post post }
