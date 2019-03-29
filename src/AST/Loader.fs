module AST.Loader

open Common.Json
open Common.Utils

exception LoadASTException of string

let getType json = getPropStr json "type"

let toArrayProp json key = getProp json key |> toArray

let getStrOpt json key =
  match tryGetProp json key with
  | Some json -> toStr json |> Some
  | None -> None

let isDecl = function
  | "ClassDeclaration"
  | "FunctionDeclaration"
  | "VariableDeclaration" -> true
  | _ -> false

let isStmt = function
  | "BlockStatement"
  | "BreakStatement"
  | "ContinueStatement"
  | "DebuggerStatement"
  | "DoWhileStatement"
  | "EmptyStatement"
  | "ExpressionStatement"
  | "ForStatement"
  | "ForInStatement"
  | "ForOfStatement"
  | "FunctionDeclaration"
  | "IfStatement"
  | "LabeledStatement"
  | "ReturnStatement"
  | "SwitchStatement"
  | "ThrowStatement"
  | "TryStatement"
  | "VariableDeclaration"
  | "WhileStatement"
  | "WithStatement" -> true
  | _ -> false

let isExpr = function
  | "ThisExpression"
  | "Identifier"
  | "TemplateLiteral"
  | "Literal"
  | "ArrayExpression"
  | "ObjectExpression"
  | "FunctionExpression"
  | "ArrowFunctionExpression"
  | "ClassExpression"
  | "TaggedTemplateExpression"
  | "MemberExpression"
  | "Super"
  | "MetaProperty"
  | "NewExpression"
  | "CallExpression"
  | "UpdateExpression"
  | "AwaitExpression"
  | "UnaryExpression"
  | "BinaryExpression"
  | "LogicalExpression"
  | "ConditionalExpression"
  | "YieldExpression"
  | "AssignmentExpression"
  | "SequenceExpression" -> true
  | _ -> false

let isMemberExpr = function
  | "MemberExpression" -> true
  | _ -> false

let isId = function
  | "Identifier" -> true
  | _ -> false

let isBindingPt = function
  | "ArrayPattern"
  | "ObjectPattern" -> true
  | _ -> false

let isArrayPt = function
  | "ArrayPattern" -> true
  | _ -> false

let isObjectPt = function
  | "ObjectPattern" -> true
  | _ -> false

let isBinding = function
  | "Identifier"
  | "ArrayPattern"
  | "ObjectPattern"
  | "AssignmentPattern"
  | "MemberExpression" -> true
  | _ -> false

let isAssignPt = function
  | "AssignmentPattern" -> true
  | _ -> false

let isRestElem = function
  | "RestElement" -> true
  | _ -> false

let isVarDecl = function
  | "VariableDeclaration" -> true
  | _ -> false

let isSpreadElem = function
  | "SpreadElement" -> true
  | _ -> false

let isImport  = function
  | "Import" -> true
  | _ -> false

let isBlock = function
  | "BlockStatement" -> true
  | _ -> false

let loadId json = getPropStr json "name"

let loadIdProp json key = getProp json key |> loadId

let loadIdOpt json key =
  match getProp json key with
  | Json.Null -> None
  | json -> loadId json |> Some

let getOp json = getPropStr json "operator"

let getKind json = getPropStr json "kind"

let loadUpOp json =
  match getOp json with
  | "++" -> Inc
  | "--" -> Dec
  | _ -> raise (LoadASTException "UpOp")

let loadUnOp json =
  match getOp json with
  | "+" -> Pos
  | "-" -> Neg
  | "~" -> BitNot
  | "!" -> Not
  | "delete" -> Delete
  | "void" -> Void
  | "typeof" -> TypeOf
  | _ -> raise (LoadASTException "UnOp")

let loadBinOp json =
  match getOp json with
  | "+" -> BinOp.Add
  | "-" -> BinOp.Sub
  | "*" -> BinOp.Mul
  | "/" -> BinOp.Div
  | "%" -> BinOp.Mod
  | "**" -> BinOp.Power
  | "|" -> BinOp.Or
  | "^" -> BinOp.Xor
  | "&" -> BinOp.And
  | "<<" -> BinOp.LShift
  | ">>" -> BinOp.RShift
  | ">>>" -> BinOp.RShiftZ
  | "instanceof" -> InstanceOf
  | "in" -> In
  | "==" -> Eq
  | "!=" -> Neq
  | "===" -> AbsEq
  | "!==" -> AbsNeq
  | "<" -> Lt
  | ">" -> Gt
  | "<=" -> Le
  | ">=" -> Ge
  | _ -> raise (LoadASTException "BinOp")

let loadLogicOp json =
  match getOp json with
  | "||" -> LogicOp.Or
  | "&&" -> LogicOp.And
  | _ -> raise (LoadASTException "LogicOp")

let loadAssignOp json =
  match getOp json with
  | "=" -> AssignOp.Assign
  | "*=" -> Mul
  | "**=" -> Power
  | "/=" -> Div
  | "%=" -> Mod
  | "+=" -> Add
  | "-=" -> Sub
  | "<<=" -> LShift
  | ">>=" -> RShift
  | ">>>=" -> RShiftZ
  | "&=" -> And
  | "^=" -> Xor
  | "|=" -> Or
  | _ -> raise (LoadASTException "AssignOp")

let loadVarKind json =
  match getKind json with
  | "var" -> Var
  | "const" -> Const
  | "let" -> Let
  | _ -> raise (LoadASTException "VarKind")

let loadPropKind json =
  match getKind json with
  | "get" -> PropKind.Get
  | "set" -> PropKind.Set
  | "init" -> PropKind.Init
  | _ -> raise (LoadASTException "PropKind")

let loadMethodKind json =
  match getKind json with
  | "method" -> Method
  | "constructor" -> Constructor
  | "set" -> Set
  | "get" -> Get
  | _ -> raise (LoadASTException "MethodKind")

let loadLiteral json =
  let raw = getPropStr json "raw"
  match getProp json "value" with
  | Json.Null -> Literal.Null
  | Json.Boolean v -> Literal.Bool v
  | Json.Number _ -> Literal.Number raw
  | Json.Float _ -> Literal.Number raw
  | Json.String _ -> Literal.String raw
  | Json.Record _ -> Literal.Regex raw
  | _ -> raise (LoadASTException "Literal")

let rec loadStmtList json = toArray json |> Array.map loadStmtListItem

and loadStmtListProp json key = getProp json key |> loadStmtList

and loadStmtListOpt json key =
  match getProp json key with
  | Json.Null -> None
  | json -> loadStmtList json |> Some

and loadStmtListItem json =
  let ty = getType json
  if isDecl ty then loadDecl json |> StmtListItem.Decl
  elif isStmt ty then loadStmt json |> StmtListItem.Stmt
  else raise (LoadASTException "StmtListItem")

and loadDecl json =
  match getType json with
  | "ClassDeclaration" -> loadClassDecl json |> Decl.ClassDecl
  | "FunctionDeclaration" -> loadFuncDecl json |> Decl.FuncDecl
  | "VariableDeclaration" -> loadVarDecl json |> Decl.VarDecl
  | _ -> raise (LoadASTException "Decl")

and loadStmt json =
  match getType json with
  | "BlockStatement" -> loadStmtListProp json "body" |> Stmt.Block
  | "BreakStatement" -> loadIdOpt json "label" |> Stmt.Break
  | "ContinueStatement" -> loadIdOpt json "label"|> Stmt.Continue
  | "DebuggerStatement" -> Stmt.Debugger
  | "DoWhileStatement" ->
    (loadStmtProp json "body", loadExprProp json "test") |> Stmt.DoWhile
  | "EmptyStatement" -> Stmt.Empty
  | "ExpressionStatement" ->
    (loadExprProp json "expression", getStrOpt json "directive") |> Stmt.Expr
  | "ForStatement" ->
    (loadForInitOpt json, loadExprOpt json "test", loadExprOpt json "update",
     loadStmtProp json "body") |> For
  | "ForInStatement" -> loadForInOf json |> ForIn
  | "ForOfStatement" -> loadForInOf json |> ForOf
  | "FunctionDeclaration" -> loadFuncDecl json |> Stmt.FuncDecl
  | "IfStatement" ->
    (loadExprProp json "test", loadStmtProp json "consequent",
     loadStmtOpt json "alternate") |> Stmt.If
  | "LabeledStatement" ->
    (loadIdProp json "label", loadStmtProp json "body") |> Stmt.Labeled
  | "ReturnStatement" -> loadExprOpt json "argument" |> Stmt.Return
  | "SwitchStatement" ->
    (loadExprProp json "discriminant", loadCases json) |> Stmt.Switch
  | "ThrowStatement" -> loadExprProp json "argument" |> Stmt.Throw
  | "TryStatement" ->
    (loadBlock json "block", loadCatchOpt json,
     loadBlockOpt json "finalizer") |> Stmt.Try
  | "VariableDeclaration" -> loadVarDecl json |> Stmt.VarDecl
  | "WhileStatement" ->
    (loadExprProp json "test", loadStmtProp json "body") |> Stmt.While
  | "WithStatement" ->
    (loadExprProp json "object", loadStmtProp json "body") |> Stmt.With
  | _ -> raise (LoadASTException "Stmt")

and loadStmtProp json key = getProp json key |> loadStmt

and loadStmtOpt json key =
  match getProp json key with
  | Json.Null -> None
  | json -> loadStmt json |> Some

and loadStmts json key = toArrayProp json key |> Array.map loadStmt

and loadExpr json =
  match getType json with
  | "ThisExpression" -> Expr.This
  | "Identifier" -> loadId json |> Expr.Id
  | "Literal" -> loadLiteral json |> Expr.Literal
  | "TemplateLiteral" -> loadTempLiteral json |> Expr.TempLiteral
  | "ArrayExpression" -> toArray json |> Array.map loadArrayElem |> Expr.Array
  | "ObjectExpression" -> loadProps json |> Expr.Object
  | "FunctionExpression" -> loadFuncExpr json |> Expr.Function
  | "ArrowFunctionExpression" -> loadArrowFuncExpr json |> Expr.ArrowFunction
  | "ClassExpression" ->
    (loadIdOpt json "id", loadExprOpt json "superClass", loadClassBody json)
    |> Expr.Class
  | "TaggedTemplateExpression" ->
    (loadExprProp json "tag", loadTempLiteralProp json "quasi") |> TaggedTemp
  | "MemberExpression" -> loadMemberExpr json |> Expr.Member
  | "Super" -> Expr.Super
  | "MetaProperty" ->
    (loadIdProp json "meta", loadIdProp json "property") |> Expr.MetaProp
  | "NewExpression" -> (loadExprProp json "callee", loadArgs json) |> Expr.New
  | "CallExpression" -> (loadCallee json, loadArgs json) |> Expr.Call
  | "UpdateExpression" ->
    (loadUpOp json, loadExprProp json "argument", getBool json "prefix")
    |> Expr.Update
  | "AwaitExpression" -> loadExprProp json "argument" |> Expr.Await
  | "UnaryExpression" ->
    (loadUnOp json, loadExprProp json "argument") |> Expr.Unary
  | "BinaryExpression" ->
    (loadBinOp json, loadExprProp json "left", loadExprProp json "right")
    |> Expr.Binary
  | "LogicalExpression" ->
    (loadLogicOp json, loadExprProp json "left", loadExprProp json "right")
    |> Expr.Logic
  | "ConditionalExpression" ->
    (loadExprProp json "test", loadExprProp json "consequent",
     loadExprProp json "alternate") |> Expr.Cond
  | "YieldExpression" ->
    (loadExprOpt json "argument", getBool json "delegate") |> Expr.Yield
  | "AssignmentExpression" ->
    (loadAssignOp json, loadAssignLeft json, loadExprProp json "right")
    |> Expr.Assign
  | "SequenceExpression" ->
    toArrayProp json "expressions" |> Array.map loadExpr |> Expr.Seq
  | _ -> raise (LoadASTException "Expr")

and loadExprProp json key = getProp json key |> loadExpr

and loadExprOpt json key =
  match getProp json key with
  | Json.Null -> None
  | json -> loadExpr json |> Some

and loadClassDecl json =
  loadIdProp json "id", loadExprOpt json "superClass", loadClassBody json

and loadFuncDecl json =
  loadIdProp json "id", loadParams json, loadBlock json "body",
  getBool json "generator", getBool json "async"

and loadBlock json key = loadStmtListProp (getProp json key) "body"

and loadBlockOpt json key =
  match getProp json key with
  | Json.Null -> None
  | json -> loadStmtListProp json "body" |> Some

and loadVarDecl json =
  loadVarKind json, toArrayProp json "declarations" |> Array.map loadVarDeclr

and loadVarDeclr json = loadBindingProp json "id", loadExprOpt json "init"

and loadForInitOpt json =
  match getProp json "init" with
  | Json.Null -> None
  | json ->
    let ty = getType json
    if isExpr ty then loadExpr json |> ForInit.Expr |> Some
    elif isVarDecl ty then loadVarDecl json |> ForInit.VarDecl |> Some
    else raise (LoadASTException "ForInit")

and loadForInOf json =
  (loadForBind json, loadExprProp json "right", loadStmtProp json "body")

and loadForBind json =
  let json = getProp json "left"
  let ty = getType json
  if isVarDecl ty then loadVarDecl json |> ForBind.VarDecl
  elif isBinding ty then loadBinding json |> ForBind.Binding
  else raise (LoadASTException ("ForBind: "+ ty))

and loadCases json = toArray json |> Array.map loadCase

and loadCase json = loadExprOpt json "test", loadStmts json "consequent"

and loadCatchOpt json =
  match getProp json "handler" with
  | Json.Null -> None
  | json -> (loadBindingOpt json "param", loadBlock json "body") |> Some

and loadTempLiteral json =
  toArrayProp json "quasis" |> Array.map loadTempElem,
  toArrayProp json "expressions" |> Array.map loadExpr

and loadTempElem json =
  let value = getProp json "value"
  getPropStr value "raw"

and loadTempLiteralProp json key = getProp json key |> loadTempLiteral

and loadBinding json =
  let ty = getType json
  if isId ty then loadId json |> Binding.Id
  elif isBindingPt ty then loadBindingPt json |> Binding.BindingPt
  elif isAssignPt ty then loadAssignPt json |> Binding.AssignPt
  elif isMemberExpr ty then loadMemberExpr json |> Binding.MemberExpr
  else raise (LoadASTException "Binding")

and loadBindingProp json key = getProp json key |> loadBinding

and loadBindingOpt json key =
  match getProp json key with
  | Json.Null -> None
  | json -> loadBinding json |> Some

and loadBindingPt json =
  let ty = getType json
  if isArrayPt ty then
    toArrayProp json "elements" |> Array.map loadArrayPtElem
    |> BindingPt.ArrayPt
  elif isObjectPt ty then loadProps json |> BindingPt.ObjectPt
  else raise (LoadASTException "BindingPt")

and loadArrayPtElem json =
  match json with
  | Json.Null -> ArrayPtElem.Empty
  | json ->
    let ty = getType json
    if isId ty then loadId json |> ArrayPtElem.Id
    elif isBindingPt ty then loadBindingPt json |> ArrayPtElem.BindingPt
    elif isAssignPt ty then loadAssignPt json |> ArrayPtElem.AssignPt
    elif isRestElem ty then loadBindingProp json "argument" |> RestElem
    elif isMemberExpr ty then loadMemberExpr json |> ArrayPtElem.MemberExpr
    else raise (LoadASTException "ArrayPtElem")

and loadParams json = toArrayProp json "params" |> Array.map loadParam

and loadParam json =
  let ty = getType json
  if isId ty then loadId json |> Param.Id
  elif isBindingPt ty then loadBindingPt json |> Param.BindingPt
  elif isAssignPt ty then loadAssignPt json |> Param.AssignPt
  elif isRestElem ty then loadBindingProp json "argument" |> Param.RestElem
  else raise (LoadASTException "Param")

and loadAssignPt json = loadBindingProp json "left", loadExprProp json "right"

and loadArgs json = toArrayProp json "arguments" |> Array.map loadArg

and loadArg json =
  let ty = getType json
  if isExpr ty then loadExpr json |> Arg.Expr
  elif isSpreadElem ty then getProp json "argument" |> loadExpr |> Arg.Spread
  else raise (LoadASTException "Arg")

and loadCallee json =
  let json = getProp json "callee"
  let ty = getType json
  if isExpr ty then loadExpr json |> Callee.Expr
  elif isImport ty then Callee.Import
  else raise (LoadASTException "Callee")

and loadArrayElem = function
  | Json.Null -> ArrayElem.Empty
  | json ->
    let ty = getType json
    if isExpr ty then loadExpr json |> ArrayElem.Expr
    elif isSpreadElem ty then loadExprProp json "argument" |> ArrayElem.Spread
    else raise (LoadASTException "ArrayElem")

and loadProp json =
  loadExprProp json "key", loadPropVal json, loadPropKind json,
  getBool json "computed", getBool json "shorthand"

and loadPropVal json =
  match getProp json "value" with
  | Json.Null -> PropVal.Empty
  | json ->
    let ty = getType json
    if isExpr ty then loadExpr json |> PropVal.Expr
    elif isBindingPt ty then loadBindingPt json |> PropVal.BindingPt
    elif isAssignPt ty then loadAssignPt json |> PropVal.AssignPt
    else raise (LoadASTException "PropVal")

and loadProps json = toArrayProp json "properties" |> Array.map loadProp

and loadClassBody json = toArrayProp json "body" |> Array.map loadMethodDef

and loadMethodDef json =
  loadExprProp json "key", getProp json "value" |> loadFuncExpr,
  loadMethodKind json, getBool json "computed", getBool json "static"

and loadFuncExpr json =
  loadIdOpt json "id", loadParams json, loadBlock json "body",
  getBool json "generator", getBool json "async"

and loadArrowFuncExpr json =
  loadIdOpt json "id", loadParams json, loadArrowFuncBody json,
  getBool json "generator", getBool json "async"

and loadArrowFuncBody json =
  let json = getProp json "body"
  let ty = getType json
  if isExpr ty then loadExpr json |> ArrowFuncBody.Expr
  elif isBlock ty then loadStmtListProp json "body" |> ArrowFuncBody.Block
  else raise (LoadASTException "ArrowFuncBody")

and loadAssignLeft json =
  let json = getProp json "left"
  let ty = getType json
  if isBinding ty then loadBinding json |> AssignLeft.Binding
  elif isExpr ty then loadExpr json |> AssignLeft.Expr
  else raise (LoadASTException "AssignLeft")

and loadMemberExpr json =
  (loadExprProp json "object", loadExprProp json "property",
   getBool json "computed")

let loadProg json =
  match getPropStr json "sourceType" with
  | "script" -> loadStmtListProp json "body" |> Script
  | "module" -> failwith "todo"
  | _ -> raise (LoadASTException "Prog")

let failToLoad fname =
  eprintfn "fail to load seed: %s" fname
  fname, Script [||]

let load fname =
  try fname, loadJson fname |> loadProg
  with | _ -> failToLoad fname

let asyncLoad fname = async {
  try
    let! json = asyncLoadJson fname
    return fname, loadProg json
  with | _ -> return failToLoad fname
}

let loads dir =
  getFiles [||] dir |> Array.map asyncLoad
  |> Async.Parallel
  |> Async.RunSynchronously
