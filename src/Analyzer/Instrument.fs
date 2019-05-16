module Analyzer.Instrument

open System
open AST
open Common
open Common.Utils

let private (=>) (ast, ctx, cons) f = (f ast, ctx, cons)
let private (==>) (ast, ctx, cons, out) f = (f ast, ctx, cons, out)
let private (+>) cons1 cons2 = Constraint.glue cons1 cons2
let private (+=) cons1 cons2 = Constraint.union cons1 cons2
let private packOut f (ast, ctx, cons) = (f ast, ctx, cons, Set.empty)

let private folder f g (args, ctx, cons) arg =
  let arg, ctx, cons1 = f arg ctx
  arg :: args, ctx, g cons cons1

let private fold ctx f g args =
  Array.fold (folder f g) ([], ctx, Constraint.empty) args => Array.revList

let private foldG ctx f args = fold ctx f (+>) args
let private foldU ctx f args = fold ctx f (+=) args

let private folder2 f (args, ctx, cons, out) arg =
  let arg, ctx, cons1, out1 = f arg ctx
  arg :: args, ctx, cons +> cons1, out + out1

let private fold2 ctx f args =
  Array.fold (folder2 f) ([], ctx, Constraint.empty, Set.empty) args
  ==> Array.revList

let private folder3 f g (args, ctx, cons) arg =
  let args1, ctx, cons1 = f arg ctx
  args1 :: args, ctx, g cons cons1

let private fold3 ctx f g args =
  Array.fold (folder3 f g) ([], ctx, Constraint.empty) args
  => (Array.revList >> Array.concat)

let private fold3G ctx f args = fold3 ctx f (+>) args
let private fold3U ctx f args = fold3 ctx f (+=) args

let toStmtList stmts = Array.map StmtListItem.Stmt stmts

let toBlock stmts =
  match stmts with
  | [| Stmt.Block stmts |] -> Stmt.Block stmts
  | stmts -> toStmtList stmts |> Stmt.Block

let mkVarStmt id expr =
  (Var, [| Binding.Id id, Some expr |]) |> Stmt.VarDecl

let addRetThrowLogger conv brick ctx cons arg =
  let pre, post = CodeBrick.getLoggers brick cons
  let id = CodeBrick.getTempVar brick
  let assign = mkVarStmt id arg
  [|pre; assign; post; Expr.Id id |> conv|], ctx, cons

let addRetLogger brick ctx cons = function
  | Some expr -> addRetThrowLogger (Some >> Return) brick ctx cons expr
  | None -> [|Return None|], ctx, cons

let addStmtLogger brick (stmt, ctx, cons) =
  match stmt with
  | Stmt.Block _ | Break _ | Continue _ | Debugger | Stmt.Empty ->
    [|stmt|], ctx, cons
  | Stmt.Return arg -> addRetLogger brick ctx cons arg
  | Stmt.Throw arg -> addRetThrowLogger Throw brick ctx cons arg
  | stmt -> let pre, post = CodeBrick.getLoggers brick cons
            [|pre; stmt; post|], ctx, cons

let addDeclLogger brick (decl, ctx, cons) =
  let pre, post = CodeBrick.getLoggers brick cons
  [|StmtListItem.Stmt pre; decl; StmtListItem.Stmt post|], ctx, cons

let addBlockLogger brick (stmts, ctx, cons) =
  let pre, post = CodeBrick.getLoggers brick cons
  Array.concat [|[|StmtListItem.Stmt pre|]; stmts; [|StmtListItem.Stmt post|]|],
  ctx, cons

let updateStmt brick (stmt, ctx, cons) =
  match Context.addBrick ctx cons brick with
  | ctx, true -> addStmtLogger brick (stmt, ctx, cons)
  | ctx, false -> [|stmt|], ctx, cons

let updateDecl brick (decl, ctx, cons) =
  match Context.addBrick ctx cons brick with
  | ctx, true -> addDeclLogger brick (decl, ctx, cons)
  | ctx, false -> [|decl|], ctx, cons

let updateGuard ctx cons sHash guard =
  CodeBrick.ofGuard guard ctx |> Context.addGBrick ctx cons sHash

let updateBlock brick (block, ctx, cons) =
  match Context.addBrick ctx cons brick with
  | ctx, true -> addBlockLogger brick (block, ctx, cons) => Stmt.Block
  | ctx, false -> Stmt.Block block, ctx, cons

let inline toUnary op (expr, ctx, cons) = Unary (op, expr), ctx, cons

let rec rewriteStmtList stmts ctx = fold3G ctx rewriteStmtItem stmts

and rewriteStmtItem item ctx =
  match item with
  | StmtListItem.Stmt stmt -> rewriteStmt stmt ctx => toStmtList
  | StmtListItem.Decl decl -> rewriteDecl decl ctx

and rewriteStmt stmt ctx =
  let brick = CodeBrick.ofStmt stmt ctx
  match stmt with
  | Stmt.Block stmts -> rewriteBlock stmts ctx |> updateBlock brick
  | Break label -> rewriteLabel Break label ctx
  | Continue label -> rewriteLabel Continue label ctx
  | DoWhile (body, test) -> rewriteDoWhile body test ctx brick.Hash
  | Stmt.Expr (expr, dir) -> rewriteExprStmt expr dir ctx
  | For (init, test, update, body) ->
    rewriteFor init test update body ctx brick.Hash
  | ForIn (bind, expr, body) -> rewriteForIn bind expr body ctx brick.Hash
  | ForOf (bind, expr, body) -> rewriteForOf bind expr body ctx brick.Hash
  | Stmt.FuncDecl decl -> rewriteFuncDecl decl ctx
  | If (test, tStmt, fStmt) -> rewriteIf test tStmt fStmt ctx
  | Labeled (label, body) -> rewriteLabeled label body ctx
  | Return arg -> rewriteReturn arg ctx
  | Switch (test, cases) -> rewriteSwitch test cases ctx
  | Throw arg -> rewriteThrow arg ctx
  | Try (body, catch, final) -> rewriteTry body catch final ctx
  | Stmt.VarDecl decl -> rewriteVarDeclStmt decl ctx
  | While (test, body) -> rewriteWhile test body ctx brick.Hash
  | With (expr, body) -> rewriteWith expr body ctx
  // Debugger, Empty
  | stmt -> rewriteEmpty stmt ctx
  |> updateStmt brick

and rewriteStmtOpt stmt ctx =
  match stmt with
  | Some stmt -> rewriteStmt stmt ctx => (toBlock >> Some)
  | None -> None, ctx, Constraint.empty

and rewriteDecl decl ctx =
  match decl with
  | Decl.ClassDecl decl -> rewriteClassDecl decl ctx => StmtListItem.Decl
  | Decl.VarDecl decl -> rewriteVarDeclStmt decl ctx => StmtListItem.Stmt
  | Decl.FuncDecl decl -> rewriteFuncDecl decl ctx => StmtListItem.Stmt
  |> updateDecl (CodeBrick.ofDecl decl ctx)

and rewriteExpr expr ctx =
  match expr with
  | Expr.Id id -> expr, ctx, Constraint.initId ctx.IsBuiltIn id
  | TempLiteral temp -> rewriteTempLiteral temp ctx => TempLiteral
  | Array elems -> foldG ctx rewriteArrayElem elems => Array
  | Object props -> rewriteObject props ctx
  | Function expr -> rewriteFuncExpr expr ctx => Function
  | ArrowFunction (id, params_, body, isGen, isAsync) ->
    rewriteArrowFunc id params_ body isGen isAsync ctx
  | Class (id, extends, defs) -> rewriteClassExpr id extends defs ctx
  | TaggedTemp (expr, temp) -> rewriteTaggedTemp expr temp ctx
  | Member expr -> rewriteMemberExpr expr ctx => Member
  | Super -> expr, ctx, Constraint.super
  | New (expr, args) -> rewriteNew expr args ctx
  | Call (callee, args) -> rewriteCall callee args ctx
  | Update (op, expr, isPre) -> rewriteUpdate op expr isPre ctx
  | Await expr -> rewriteAwait expr ctx
  | Unary (op, expr) -> rewriteExpr expr ctx |> toUnary op
  | Binary (op, e1, e2) -> rewriteBinary op e1 e2 ctx
  | Logic (op, e1, e2) -> rewriteLogic op e1 e2 ctx
  | Cond (cond, tExpr, fExpr) -> rewriteCond cond tExpr fExpr ctx
  | Yield (arg, isGen) -> rewriteYield arg isGen ctx
  | Assign (op, left, expr) -> rewriteAssign op left expr ctx
  | Seq exprs -> foldG ctx rewriteExpr exprs => Seq
  | MetaProp _ -> expr, ctx, Constraint.metaProp
  | This -> This, ctx, Constraint.this
  // Literal
  | _ -> expr, ctx, Constraint.empty

and rewriteExprOpt expr ctx =
  match expr with
  | Some expr -> rewriteExpr expr ctx => Some
  | None -> None, ctx, Constraint.empty

and rewriteBlock stmts ctx =
  let stmts, ctx, cons = rewriteStmtList stmts ctx
  stmts, ctx, Constraint.finiBlock cons

and rewriteBlockOpt stmts ctx =
  match stmts with
  | Some stmts -> rewriteBlock stmts ctx => Some
  | None -> None, ctx, Constraint.empty

and rewriteLabel conv label ctx = conv label, ctx, Constraint.initLabel label

and rewriteDoWhile body test ctx sHash =
  let guard = Guard.DoWhile test
  let body, ctx, cons1 = rewriteStmt body ctx => toBlock
  let test, ctx, cons2 = rewriteExpr test ctx
  let ctx = updateGuard ctx cons2 sHash guard
  (DoWhile (body, test), ctx, cons1 +> cons2 |> Constraint.unsetLoop)

and rewriteExprStmt expr dir ctx =
  let expr, ctx, cons = rewriteExpr expr ctx
  Stmt.Expr (expr, dir), ctx, cons

and rewriteFor init test update body ctx sHash =
  let guard = Guard.For (init, test, update)
  let init, ctx, cons1 = rewriteForInit init ctx
  let test, ctx, cons2 = rewriteExprOpt test ctx
  let body, ctx, cons3 = rewriteStmt body ctx => toBlock
  let update, ctx, cons4 = rewriteExprOpt update ctx
  let ctx = updateGuard ctx (cons1 +> cons2 +> cons4) sHash guard
  ( For (init, test, update, body), ctx,
    cons1 +> cons2 +> cons3 +> cons4 |> Constraint.finiFor )

and rewriteForInit init ctx =
  match init with
  | Some (ForInit.Expr expr) -> rewriteExpr expr ctx => (ForInit.Expr >> Some)
  | Some (ForInit.VarDecl decl) ->
    rewriteVarDecl decl ctx => (ForInit.VarDecl >> Some)
  | None -> None, ctx, Constraint.empty

and rewriteVarDecl (kind, declr) ctx =
  let declr, ctx, cons, out = fold2 ctx rewriteVarDeclr declr
  (kind, declr), ctx, Constraint.finiVarDecl kind out cons

and rewriteVarDeclr (bind, init) ctx =
  let bind, ctx, cons1, out = rewriteBinding bind ctx
  let init, ctx, cons2 = rewriteExprOpt init ctx
  (bind, init), ctx, cons1 +> cons2, out

and rewriteForIn bind expr body ctx sHash =
  let guard = Guard.ForIn (bind, expr)
  let bind, ctx, cons1 = rewriteForBind bind ctx
  let expr, ctx, cons2 = rewriteExpr expr ctx
  let body, ctx, cons3 = rewriteStmt body ctx => toBlock
  let ctx = updateGuard ctx (cons1 +> cons2) sHash guard
  ForIn (bind, expr, body), ctx, cons1 +> cons2 +> cons3 |> Constraint.finiFor

and rewriteForOf bind expr body ctx sHash =
  let guard = Guard.ForOf (bind, expr)
  let bind, ctx, cons1 = rewriteForBind bind ctx
  let expr, ctx, cons2 = rewriteExpr expr ctx
  let body, ctx, cons3 = rewriteStmt body ctx => toBlock
  let ctx = updateGuard ctx (cons1 +> cons2) sHash guard
  ForOf (bind, expr, body), ctx, cons1 +> cons2 +> cons3 |> Constraint.finiFor

and rewriteForBind bind ctx =
  match bind with
  | ForBind.VarDecl decl -> rewriteVarDecl decl ctx => ForBind.VarDecl
  | ForBind.Binding bind ->
    let bind, ctx, cons, out = rewriteBinding bind ctx
    ForBind.Binding bind, ctx, Constraint.addOut cons Glob out

and rewriteFuncDecl (id, params_, body, isGen, isAsync) ctx =
  let params_, ctx, cons1, out = fold2 ctx rewriteParam params_
  let body, ctx, cons2 = rewriteBlock body ctx
  ( Stmt.FuncDecl (id, params_, body, isGen, isAsync), ctx,
    Constraint.finiFuncDecl id isGen isAsync out cons1 cons2 )

and rewriteParam param ctx =
  match param with
  | Param.Id id -> param, ctx, Constraint.empty, Set.init id
  | Param.BindingPt pt -> rewriteBindingPt pt ctx ==> Param.BindingPt
  | Param.AssignPt pt -> rewriteAssignPt pt ctx ==> Param.AssignPt
  | Param.RestElem bind -> rewriteBinding bind ctx ==> Param.RestElem

and rewriteIf test tStmt fStmt ctx =
  let test, ctx, cons1 = rewriteExpr test ctx
  let tStmt, ctx, cons2 = rewriteStmt tStmt ctx => toBlock
  let fStmt, ctx, cons3 = rewriteStmtOpt fStmt ctx
  If (test, tStmt, fStmt), ctx, cons1 +> (cons2 += cons3)

and rewriteLabeled label body ctx =
  let body, ctx, cons = rewriteStmt body ctx => toBlock
  Labeled (label, body), ctx, Constraint.addLabel cons label

and rewriteReturn arg ctx =
  let arg, ctx, cons = rewriteExprOpt arg ctx
  Return arg, ctx, Constraint.setFunc cons

and rewriteSwitch test cases ctx =
  let test, ctx, cons1 = rewriteExpr test ctx
  let cases, ctx, cons2 = foldU ctx rewriteCase cases
  Switch (test, cases), ctx, cons1 +> cons2 |> Constraint.finiSwitch

and rewriteCase (expr, body) ctx =
  let expr, ctx, cons1 = rewriteExprOpt expr ctx
  let body, ctx, cons2 = fold3U ctx rewriteStmt body
  Case (expr, body), ctx, cons1 +> cons2

and rewriteThrow arg ctx =
  let arg, ctx, cons = rewriteExpr arg ctx
  Throw arg, ctx, Constraint.setTry cons

and rewriteTry body catch final ctx =
  let body, ctx, cons1 = rewriteBlock body ctx
  let catch, ctx, cons2 = rewriteCatch catch ctx
  let final, ctx, cons3 = rewriteBlockOpt final ctx
  Try (body, catch, final), ctx, (Constraint.finiTry cons1) +> cons2 +> cons3

and rewriteCatch catch ctx =
  match catch with
  | Some (bind, body) ->
    let bind, ctx, cons1, out = rewriteBindingOpt bind ctx
    let body, ctx, cons2 = rewriteBlock body ctx
    Some (bind, body), ctx, Constraint.finiCatch out cons1 cons2
  | None -> None, ctx, Constraint.empty

and rewriteVarDeclStmt decl ctx = rewriteVarDecl decl ctx => Stmt.VarDecl

and rewriteWhile test body ctx sHash =
  let guard = Guard.While test
  let test, ctx, cons1 = rewriteExpr test ctx
  let body, ctx, cons2 = rewriteStmt body ctx => toBlock
  let ctx = updateGuard ctx cons1 sHash guard
  While (test, body), ctx, cons1 +> cons2 |> Constraint.unsetLoop

and rewriteWith expr body ctx =
  let expr, ctx, cons1 = rewriteExpr expr ctx
  let body, ctx, cons2 = rewriteStmt body ctx => toBlock
  With (expr, body), ctx, cons1 +> cons2

and rewriteEmpty stmt ctx = stmt, ctx, Constraint.empty

and rewriteClassDecl (id, extends, defs) ctx =
  let extends, ctx, cons1 = rewriteExprOpt extends ctx
  let defs, ctx, cons2 = foldU ctx rewriteDef defs
  ClassDecl (id, extends, defs), ctx, cons1 +> cons2 |> Constraint.finiClass id

and rewriteDef (key, body, kind, isComputed, isStatic) ctx =
  let key, ctx, cons1 = rewriteMemName key isComputed ctx
  let body, ctx, cons2 = rewriteFuncExpr body ctx
  (key, body, kind, isComputed, isStatic), ctx, cons1 +> cons2

and rewriteTempLiteral (strs, exprs) ctx =
  let exprs, ctx, cons = foldG ctx rewriteExpr exprs
  (strs, exprs), ctx, cons

and rewriteArrayElem elem ctx =
  match elem with
  | ArrayElem.Expr expr -> rewriteExpr expr ctx => ArrayElem.Expr
  | ArrayElem.Spread expr -> rewriteExpr expr ctx => ArrayElem.Spread
  | ArrayElem.Empty -> elem, ctx, Constraint.empty

and rewriteObject props ctx =
  let props, ctx, cons, _ = fold2 ctx (rewriteProp true) props
  Object props, ctx, cons

and rewriteProp isExpr (key, value, kind, isComputed, isShort) ctx =
  let key, ctx, cons1 = rewriteMemName key isComputed ctx
  let value, ctx, cons2, out = rewritePropVal isExpr value ctx
  (key, value, kind, isComputed, isShort), ctx, cons1 +> cons2, out

and rewritePropVal isExpr value ctx =
  match value with
  | PropVal.Expr ((Expr.Id id) as expr) ->
    if isExpr then rewriteExpr expr ctx |> packOut PropVal.Expr
    else value, ctx, Constraint.empty, Set.init id
  | PropVal.Expr expr -> rewriteExpr expr ctx |> packOut PropVal.Expr
  | PropVal.BindingPt pt -> rewriteBindingPt pt ctx ==> PropVal.BindingPt
  | PropVal.AssignPt pt -> rewriteAssignPt pt ctx ==> PropVal.AssignPt
  | PropVal.Empty -> value, ctx, Constraint.empty, Set.empty

and rewriteFuncExpr (id, params_, body, isGen, isAsync) ctx =
  let params_, ctx, cons1, out = fold2 ctx rewriteParam params_
  let body, ctx, cons2 = rewriteBlock body ctx
  ( (id, params_, body, isGen, isAsync), ctx,
    Constraint.finiFuncExpr id isGen isAsync out cons1 cons2 )

and rewriteArrowFunc id params_ body isGen isAsync ctx =
  let params_, ctx, cons1, out = fold2 ctx rewriteParam params_
  let body, ctx, cons2 = rewriteArrowFuncBody body ctx
  ( ArrowFunction (id, params_, body, isGen, isAsync), ctx,
    Constraint.finiFuncExpr id isGen isAsync out cons1 cons2 )

and rewriteArrowFuncBody body ctx =
  match body with
  | ArrowFuncBody.Block stmts -> rewriteBlock stmts ctx => ArrowFuncBody.Block
  | ArrowFuncBody.Expr expr -> rewriteExpr expr ctx => ArrowFuncBody.Expr

and rewriteClassExpr id extends defs ctx =
  let extends, ctx, cons1 = rewriteExprOpt extends ctx
  let defs, ctx, cons2 = foldU ctx rewriteDef defs
  Expr.Class (id, extends, defs), ctx, Constraint.finiClassExpr id cons1 cons2

and rewriteTaggedTemp expr temp ctx =
  let expr, ctx, cons1 = rewriteExpr expr ctx
  let temp, ctx, cons2 = rewriteTempLiteral temp ctx
  TaggedTemp (expr, temp), ctx, cons1 +> cons2

and rewriteMemberExpr (expr, key, isComputed) ctx =
  let expr, ctx, cons1 = rewriteExpr expr ctx
  let key, ctx, cons2 = rewriteMemName key isComputed ctx
  (expr, key, isComputed), ctx, cons1 +> cons2

and rewriteMemName key isComputed ctx =
  if isComputed then rewriteExpr key ctx
  else key, ctx, Constraint.empty

and rewriteNew expr args ctx =
  let expr, ctx, cons1 = rewriteExpr expr ctx
  let args, ctx, cons2 = foldG ctx rewriteArg args
  New (expr, args), ctx, cons1 +> cons2

and rewriteArg arg ctx =
  match arg with
  | Arg.Expr expr -> rewriteExpr expr ctx => Arg.Expr
  | Arg.Spread expr -> rewriteExpr expr ctx => Arg.Spread

and rewriteCall callee args ctx =
  let callee, ctx, cons1 =
    match callee with
    | Callee.Expr expr -> rewriteExpr expr ctx => Callee.Expr
    | Callee.Import -> callee, ctx, Constraint.empty
  let args, ctx, cons2 = foldG ctx rewriteArg args
  Call (callee, args), ctx, cons1 +> cons2

and rewriteUpdate op expr isPre ctx =
  let expr, ctx, cons = rewriteExpr expr ctx
  Update (op, expr, isPre), ctx, cons

and rewriteAwait expr ctx =
  let expr, ctx, cons = rewriteExpr expr ctx
  Await expr, ctx, Constraint.setAsync cons

and rewriteBinary op e1 e2 ctx =
  let e1, ctx, cons1 = rewriteExpr e1 ctx
  let e2, ctx, cons2 = rewriteExpr e2 ctx
  Binary (op, e1, e2), ctx, cons1 +> cons2

and rewriteLogic op e1 e2 ctx =
  let e1, ctx, cons1 = rewriteExpr e1 ctx
  let e2, ctx, cons2 = rewriteExpr e2 ctx
  Logic (op, e1, e2), ctx, cons1 +> cons2

and rewriteCond cond tExpr fExpr ctx =
  let cond, ctx, cons1 = rewriteExpr cond ctx
  let tExpr, ctx, cons2 = rewriteExpr tExpr ctx
  let fExpr, ctx, cons3 = rewriteExpr fExpr ctx
  Cond (cond, tExpr, fExpr), ctx, cons1 +> (cons2 += cons3)

and rewriteYield arg isGen ctx =
  let arg, ctx, cons = rewriteExprOpt arg ctx
  Yield (arg, isGen), ctx, Constraint.setGen cons

and rewriteAssign op left expr ctx =
  let left, ctx, cons1, out = rewriteAssignLeft left ctx
  let expr, ctx, cons2 = rewriteExpr expr ctx
  ( Assign (op, left, expr), ctx, cons1 +> cons2 |> Constraint.assign op out )

and rewriteAssignLeft left ctx =
  match left with
  | AssignLeft.Binding bind -> rewriteBinding bind ctx ==> AssignLeft.Binding
  | AssignLeft.Expr expr -> rewriteExpr expr ctx |> packOut AssignLeft.Expr

and rewriteBinding bind ctx =
  match bind with
  | Binding.Id id -> bind, ctx, Constraint.empty, Set.init id
  | Binding.BindingPt pt -> rewriteBindingPt pt ctx ==> Binding.BindingPt
  | Binding.AssignPt pt -> rewriteAssignPt pt ctx ==> Binding.AssignPt
  | Binding.MemberExpr expr -> rewriteMemberPt expr ctx ==> Binding.MemberExpr

and rewriteBindingOpt bind ctx =
  match bind with
  | Some bind -> rewriteBinding bind ctx ==> Some
  | None -> None, ctx, Constraint.empty, Set.empty

and rewriteBindingPt pt ctx =
  match pt with
  | ArrayPt elems -> fold2 ctx rewriteArrayPtElem elems ==> ArrayPt
  | ObjectPt props -> fold2 ctx (rewriteProp false) props ==> ObjectPt

and rewriteArrayPtElem elem ctx =
  match elem with
  | ArrayPtElem.Id id -> elem, ctx, Constraint.empty, Set.init id
  | ArrayPtElem.BindingPt pt ->
    rewriteBindingPt pt ctx ==> ArrayPtElem.BindingPt
  | ArrayPtElem.AssignPt pt -> rewriteAssignPt pt ctx ==> ArrayPtElem.AssignPt
  | ArrayPtElem.RestElem bind ->
    rewriteBinding bind ctx ==> ArrayPtElem.RestElem
  | ArrayPtElem.MemberExpr expr ->
    rewriteMemberPt expr ctx ==> ArrayPtElem.MemberExpr
  | ArrayPtElem.Empty -> elem, ctx, Constraint.empty, Set.empty

and rewriteAssignPt (bind, expr) ctx =
  let bind, ctx, cons1, out = rewriteBinding bind ctx
  let expr, ctx, cons2 = rewriteExpr expr ctx
  (bind, expr), ctx, cons1 +> cons2, out

and rewriteMemberPt expr ctx =
  let expr, ctx, cons = rewriteMemberExpr expr ctx
  expr, ctx, cons, Set.empty

let rewriteProg ctx = function
  | Script stmts -> let stmts, ctx, _ = rewriteStmtList stmts ctx
                    Script stmts, (ctx.Pool, ctx.GuardMap)
  | Module _ -> Logger.error "Module is not supported"

let getJsLibPath engine =
  let dir = Reflection.Assembly.GetExecutingAssembly().Location |> getDirName
  let js = (Conf.engineToStr engine) + ".js"
  dir +/ "jsLib" +/ js

let getLoader = function
  | Chakra -> sprintf "WScript.LoadScriptFile('%s');\n"
  | engine -> sprintf "load('%s');\n"

let mkLoader engine = getJsLibPath engine |> getLoader engine
