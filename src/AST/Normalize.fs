module AST.Normalize

open Common

let private (=>) (map, ret) f  = (map, f ret)

let private folder f (map, ret) arg =
  let map, arg = f map arg
  map, arg :: ret

let private fold filter map f args =
  Array.fold (folder (f filter)) (map, []) args => Array.revList

let getFuncFilter filter id =
  if filter id then true
  else id = "arguments"

let getNewId map id =
  let nid = Map.count map |> sprintf "v%d"
  Map.add id nid map, nid

let normalizeId filter map id =
  match Map.tryFind id map with
  | Some id -> map, id
  | None -> if filter id then Map.add id id map, id else getNewId map id

let normalizeIdOpt filter map = function
  | Some id -> normalizeId filter map id => Some
  | None -> map, None

let rec normalizeStmtList filter map items =
  fold filter map normalizeStmtItem items

and normalizeStmtListOpt filter map = function
  | Some stmts -> normalizeStmtList filter map stmts => Some
  | None -> map, None

and normalizeStmtItem filter map = function
  | StmtListItem.Stmt stmt -> normalizeStmt filter map stmt => StmtListItem.Stmt
  | StmtListItem.Decl decl -> normalizeDecl filter map decl => StmtListItem.Decl

and normalizeStmt filter map = function
  | Stmt.Block stmts -> normalizeStmtList filter map stmts => Stmt.Block
  | Break label -> normalizeIdOpt filter map label => Break
  | Continue label -> normalizeIdOpt filter map label => Continue
  | Debugger -> map, Debugger
  | DoWhile (stmt, expr) -> normalizeDoWhile filter map stmt expr
  | Stmt.Empty -> map, Stmt.Empty
  | Stmt.Expr (expr, dir) -> normalizeStmtExpr filter map expr dir
  | For (init, test, up, body) -> normalizeFor filter map init test up body
  | ForIn (bind, expr, body) ->
    normalizeForInOf filter map bind expr body => ForIn
  | ForOf (bind, expr, body) ->
    normalizeForInOf filter map bind expr body => ForOf
  | Stmt.FuncDecl decl -> normalizeFuncDecl filter map decl => Stmt.FuncDecl
  | If (test, tStmt, fStmt) -> normalizeIf filter map test tStmt fStmt
  | Labeled (id, body) -> normalizeLabeled filter map id body
  | Return arg -> normalizeExprOpt filter map arg => Return
  | Switch (test, cases) -> normalizeSwitch filter map test cases
  | Throw arg -> normalizeExpr filter map arg => Throw
  | Try (body, catch, final) -> normalizeTry filter map body catch final
  | Stmt.VarDecl decl -> normalizeVarDecl filter map decl => Stmt.VarDecl
  | While (test, body) -> normalizeExprBody filter map test body => While
  | With (expr, body) -> normalizeExprBody filter map expr body => With

and normalizeStmtOpt filter map = function
  | Some stmt -> normalizeStmt filter map stmt => Some
  | None -> map, None

and normalizeDecl filter map = function
  | Decl.ClassDecl decl -> normalizeClassDecl filter map decl => Decl.ClassDecl
  | Decl.VarDecl decl -> normalizeVarDecl filter map decl => Decl.VarDecl
  | Decl.FuncDecl decl -> normalizeFuncDecl filter map decl => Decl.FuncDecl

and normalizeExpr filter map = function
  | Expr.Id id -> normalizeId filter map id => Expr.Id
  | TempLiteral temp -> normalizeTempLiteral filter map temp => TempLiteral
  | Array elems -> fold filter map normalizeArrayElem elems => Array
  | Object props -> fold filter map normalizeProp props => Object
  | Function expr -> normalizeFuncExpr filter map expr => Function
  | ArrowFunction (id, params_, body, isGen, isAsync) ->
    normalizeArrowFunc filter map id params_ body isGen isAsync
  | Expr.Class (id, extends, defs) ->
    normalizeClassExpr filter map id extends defs
  | TaggedTemp (expr, temp) -> normalizeTaggedTemp filter map expr temp
  | Member expr -> normalizeMemberExpr filter map expr => Member
  | New (expr, args) -> normalizeNew filter map expr args
  | Call (callee, args) -> normalizeCall filter map callee args
  | Update (op, expr, isPre) -> normalizeUpExpr filter map op expr isPre
  | Await expr -> normalizeExpr filter map expr => Await
  | Unary (op, expr) -> normalizeUnExpr filter map op expr
  | Binary (op, e1, e2) -> normalizeBinExpr filter map op e1 e2
  | Logic (op, e1, e2) -> normalizeLogicExpr filter map op e1 e2
  | Cond (cond, e1, e2) -> normalizeCondExpr filter map cond e1 e2
  | Yield (expr, isGen) -> normalizeYieldExpr filter map expr isGen
  | Expr.Assign (op, left, expr) -> normalizeAssign filter map op left expr
  | Seq exprs -> fold filter map normalizeExpr exprs => Seq
  // This, Literal, Super, MetaProp
  | expr -> map, expr

and normalizeExprOpt filter map = function
  | Some expr -> normalizeExpr filter map expr => Some
  | None -> map, None

and normalizeDoWhile filter map body test =
  let map, body = normalizeStmt filter map body
  let map, test = normalizeExpr filter map test
  map, DoWhile (body, test)

and normalizeStmtExpr filter map expr dir =
  let map, expr = normalizeExpr filter map expr
  map, Stmt.Expr (expr, dir)

and normalizeFor filter map init test up body =
  let map, init = normalizeForInit filter map init
  let map, test = normalizeExprOpt filter map test
  let map, up = normalizeExprOpt filter map up
  let map, body = normalizeStmt filter map body
  map, For (init, test, up, body)

and normalizeForInit filter map = function
  | Some (ForInit.Expr expr) ->
    normalizeExpr filter map expr => (ForInit.Expr >> Some)
  | Some (ForInit.VarDecl decl) ->
    normalizeVarDecl filter map decl => (ForInit.VarDecl >> Some)
  | None -> map, None

and normalizeForInOf filter map bind expr body =
  let map, bind = normalizeForBind filter map bind
  let map, expr = normalizeExpr filter map expr
  let map, body = normalizeStmt filter map body
  map, (bind, expr, body)

and normalizeForBind filter map = function
  | ForBind.VarDecl decl -> normalizeVarDecl filter map decl => ForBind.VarDecl
  | ForBind.Binding bind -> normalizeBinding filter map bind => ForBind.Binding

and normalizeIf filter map test tStmt fStmt =
  let map, test = normalizeExpr filter map test
  let map, tStmt = normalizeStmt filter map tStmt
  let map, fStmt = normalizeStmtOpt filter map fStmt
  map, If (test, tStmt, fStmt)

and normalizeLabeled filter map id body =
  let map, id = normalizeId filter map id
  let map, body = normalizeStmt filter map body
  map, Labeled (id, body)

and normalizeSwitch filter map test cases =
  let map, test = normalizeExpr filter map test
  let map, cases = fold filter map normalizeCase cases
  map, Switch (test, cases)

and normalizeCase filter map (test, body) =
  let map, test = normalizeExprOpt filter map test
  let map, body = fold filter map normalizeStmt body
  map, (test, body)

and normalizeTry filter map body catch final =
  let map, body = normalizeStmtList filter map body
  let map, catch = normalizeCatch filter map catch
  let map, final = normalizeStmtListOpt filter map final
  map, Try (body, catch, final)

and normalizeCatch filter map = function
  | Some (bind, body) ->
    let map, bind = normalizeBindingOpt filter map bind
    let map, body = normalizeStmtList filter map body
    map, Some (bind, body)
  | None -> map, None

and normalizeExprBody filter map expr body =
  let map, expr = normalizeExpr filter map expr
  let map, body = normalizeStmt filter map body
  map, (expr, body)

and normalizeClassDecl filter map (id, extends, defs) =
  let map, id = normalizeId filter map id
  let map, extends = normalizeExprOpt filter map extends
  let map, defs = fold filter map normalizeMethodDef defs
  map, (id, extends, defs)

and normalizeMethodDef filter map (key, body, kind, isComputed, isStatic) =
  let map, key = normalizeKey filter map key isComputed
  let map, body = normalizeFuncExpr filter map body
  map, (key, body, kind, isComputed, isStatic)

and normalizeKey filter map key = function
  | true -> normalizeExpr filter map key
  | false -> map, key

and normalizeVarDecl filter map (kind, declrs) =
  let map, declrs = fold filter map normalizeVarDeclr declrs
  map, (kind, declrs)

and normalizeVarDeclr filter map (bind, init) =
  let map, bind = normalizeBinding filter map bind
  let map, init = normalizeExprOpt filter map init
  map, (bind, init)

and normalizeFuncDecl filter map (id, params_, body, isGen, isAsync) =
  let filter = getFuncFilter filter
  let map, id = normalizeId filter map id
  let map, params_ = fold filter map normalizeParam params_
  let map, body = normalizeStmtList filter map body
  map, (id, params_, body, isGen, isAsync)

and normalizeParam filter map = function
  | Param.Id id -> normalizeId filter map id => Param.Id
  | Param.BindingPt pt -> normalizeBindingPt filter map pt => Param.BindingPt
  | Param.AssignPt pt -> normalizeAssignPt filter map pt => Param.AssignPt
  | Param.RestElem bind -> normalizeBinding filter map bind => Param.RestElem

and normalizeTempLiteral filter map (strs, exprs) =
  let map, exprs = fold filter map normalizeExpr exprs
  map, (strs, exprs)

and normalizeArrayElem filter map = function
  | ArrayElem.Expr expr -> normalizeExpr filter map expr => ArrayElem.Expr
  | ArrayElem.Spread expr -> normalizeExpr filter map expr => ArrayElem.Spread
  | ArrayElem.Empty -> map, ArrayElem.Empty

and normalizeProp filter map (key, value, kind, isComputed, isShort) =
  let map, key = normalizeKey filter map key isComputed
  let map, value = normalizePropVal filter map value
  map, (key, value, kind, isComputed, isShort)

and normalizePropVal filter map = function
  | PropVal.Expr expr -> normalizeExpr filter map expr => PropVal.Expr
  | PropVal.BindingPt pt ->
    normalizeBindingPt filter map pt => PropVal.BindingPt
  | PropVal.AssignPt pt -> normalizeAssignPt filter map pt => PropVal.AssignPt
  | PropVal.Empty -> map, PropVal.Empty

and normalizeFuncExpr filter map (id, params_, body, isGen, isAsync) =
  let filter = getFuncFilter filter
  let map, id = normalizeIdOpt filter map id
  let map, params_ = fold filter map normalizeParam params_
  let map, body = normalizeStmtList filter map body
  map, (id, params_, body, isGen, isAsync)

and normalizeArrowFunc filter map id params_ body isGen isAsync =
  let filter = getFuncFilter filter
  let map, id = normalizeIdOpt filter map id
  let map, params_ = fold filter map normalizeParam params_
  let map, body =
    match body with
    | ArrowFuncBody.Block body ->
      normalizeStmtList filter map body => ArrowFuncBody.Block
    | ArrowFuncBody.Expr expr ->
      normalizeExpr filter map expr => ArrowFuncBody.Expr
  map, ArrowFunction (id, params_, body, isGen, isAsync)

and normalizeClassExpr filter map id extends defs =
  let map, id = normalizeIdOpt filter map id
  let map, extends = normalizeExprOpt filter map extends
  let map, defs = fold filter map normalizeMethodDef defs
  map, Class (id, extends, defs)

and normalizeTaggedTemp filter map expr temp =
  let map, expr = normalizeExpr filter map expr
  let map, temp = normalizeTempLiteral filter map temp
  map, TaggedTemp (expr, temp)

and normalizeMemberExpr filter map (expr, key, isComputed) =
  let map, expr = normalizeExpr filter map expr
  let map, key = normalizeKey filter map key isComputed
  map, (expr, key, isComputed)

and normalizeNew filter map expr args =
  let map, expr = normalizeExpr filter map expr
  let map, args = fold filter map normalizeArg args
  map, New (expr, args)

and normalizeArg filter map = function
  | Arg.Expr expr -> normalizeExpr filter map expr => Arg.Expr
  | Arg.Spread expr -> normalizeExpr filter map expr => Arg.Spread

and normalizeCall filter map callee args =
  let map, callee =
    match callee with
    | Callee.Expr expr -> normalizeExpr filter map expr => Callee.Expr
    | Callee.Import -> map, callee
  let map, args = fold filter map normalizeArg args
  map, Call (callee, args)

and normalizeUpExpr filter map op expr isPre =
  let map, expr = normalizeExpr filter map expr
  map, Update (op, expr, isPre)

and normalizeUnExpr filter map op expr =
  let map, expr = normalizeExpr filter map expr
  map, Unary (op, expr)

and normalizeBinExpr filter map op e1 e2 =
  let map, e1 = normalizeExpr filter map e1
  let map, e2 = normalizeExpr filter map e2
  map, Binary (op, e1, e2)

and normalizeLogicExpr filter map op e1 e2 =
  let map, e1 = normalizeExpr filter map e1
  let map, e2 = normalizeExpr filter map e2
  map, Logic (op, e1, e2)

and normalizeCondExpr filter map cond e1 e2 =
  let map, cond = normalizeExpr filter map cond
  let map, e1 = normalizeExpr filter map e1
  let map, e2 = normalizeExpr filter map e2
  map, Cond (cond, e1, e2)

and normalizeYieldExpr filter map expr isGen =
  let map, expr = normalizeExprOpt filter map expr
  map, Yield (expr, isGen)

and normalizeAssign filter map op left expr =
  let map, left =
    match left with
    | AssignLeft.Expr expr -> normalizeExpr filter map expr => AssignLeft.Expr
    | AssignLeft.Binding bind ->
      normalizeBinding filter map bind => AssignLeft.Binding
  let map, expr = normalizeExpr filter map expr
  map, Assign (op, left, expr)

and normalizeBinding filter map = function
  | Binding.Id id -> normalizeId filter map id => Binding.Id
  | Binding.BindingPt pt ->
    normalizeBindingPt filter map pt => Binding.BindingPt
  | Binding.AssignPt pt -> normalizeAssignPt filter map pt => Binding.AssignPt
  | Binding.MemberExpr expr ->
    normalizeMemberExpr filter map expr => Binding.MemberExpr

and normalizeBindingOpt filter map = function
  | Some bind -> normalizeBinding filter map bind => Some
  | None -> map, None

and normalizeBindingPt filter map = function
  | ArrayPt pts -> fold filter map normalizeArrayPtElem pts => ArrayPt
  | ObjectPt props -> fold filter map normalizeProp props => ObjectPt

and normalizeArrayPtElem filter map = function
  | ArrayPtElem.Id id -> normalizeId filter map id => ArrayPtElem.Id
  | ArrayPtElem.BindingPt pt ->
    normalizeBindingPt filter map pt => ArrayPtElem.BindingPt
  | ArrayPtElem.AssignPt pt ->
    normalizeAssignPt filter map pt => ArrayPtElem.AssignPt
  | ArrayPtElem.RestElem bind ->
    normalizeBinding filter map bind => ArrayPtElem.RestElem
  | ArrayPtElem.MemberExpr expr ->
    normalizeMemberExpr filter map expr => ArrayPtElem.MemberExpr
  | ArrayPtElem.Empty -> map, ArrayPtElem.Empty

and normalizeAssignPt filter map (bind, expr) =
  let map, bind = normalizeBinding filter map bind
  let map, expr = normalizeExpr filter map expr
  map, (bind, expr)
