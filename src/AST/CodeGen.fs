module AST.CodeGen

open System

exception ToCodeException of string

type SB = System.Text.StringBuilder

let addStr (sb: SB) (str: string) = sb.Append (str) |> ignore
let addSpc (sb: SB) = sb.Append (" ") |> ignore
let addSemiCol (sb: SB) = sb.Append (";") |> ignore
let addComma (sb: SB) = sb.Append (", ") |> ignore
let trimComma (sb: SB) = function
  | 0 -> ()
  | _ -> sb.Remove (sb.Length - 2, 2) |> ignore

let varKindToStr = function
  | Var -> "var "
  | Const -> "const "
  | Let -> "let "

let assignOpToStr = function
  | AssignOp.Assign -> " = "
  | AssignOp.Mul -> " *= "
  | AssignOp.Div -> " /= "
  | AssignOp.Mod -> " %= "
  | AssignOp.Add -> " += "
  | AssignOp.Sub -> " -= "
  | AssignOp.Power -> " **= "
  | AssignOp.LShift -> " <<= "
  | AssignOp.RShift -> " >>= "
  | AssignOp.RShiftZ -> " >>>= "
  | AssignOp.And -> " &= "
  | AssignOp.Xor -> " ^= "
  | AssignOp.Or -> " |= "

let binOpToStr = function
  | BinOp.Add -> " + "
  | BinOp.Sub -> " - "
  | BinOp.Mul -> " * "
  | BinOp.Div -> " / "
  | BinOp.Mod -> " % "
  | BinOp.Power -> " ** "
  | BinOp.Or -> " | "
  | BinOp.Xor -> " ^ "
  | BinOp.And -> " & "
  | BinOp.LShift -> " << "
  | BinOp.RShift -> " >> "
  | BinOp.RShiftZ -> " >>> "
  | BinOp.InstanceOf -> " instanceof "
  | BinOp.In -> " in "
  | BinOp.Eq -> " == "
  | BinOp.Neq -> " != "
  | BinOp.AbsEq -> " === "
  | BinOp.AbsNeq -> " !== "
  | BinOp.Gt -> " > "
  | BinOp.Ge -> " >= "
  | BinOp.Lt -> " < "
  | BinOp.Le -> " <= "

let logicOpToStr = function
  | LogicOp.Or -> " || "
  | LogicOp.And -> " && "

let unOpToStr = function
  | Pos -> "+ "
  | Neg -> "- "
  | Not -> "! "
  | BitNot -> "~ "
  | Delete -> "delete "
  | Void -> "void "
  | TypeOf -> "typeof "

let upOpToStr = function
  | Inc -> "++"
  | Dec -> "--"

let literalToStr = function
  | Null -> "null"
  | Bool true -> "true"
  | Bool false -> "false"
  | Number str -> str
  | String str -> str
  | Regex str -> str

let funcHeadToCode sb isGen isAsync =
  if isAsync then addStr sb "async "
  else ()
  if isGen then addStr sb "function* "
  else addStr sb "function "

let methodHeadToCode sb isGen isAsync =
  if isAsync then addStr sb "async "
  else ()
  if isGen then addStr sb " * "
  else ()

let methodKindToCode sb = function
  | Method
  | Constructor -> ()
  | Set -> addStr sb "set "
  | Get -> addStr sb "get "

let propKindToCode sb = function
  | PropKind.Get -> addStr sb "get "
  | PropKind.Set -> addStr sb "set "
  | PropKind.Init -> ()

let nameToCode sb = function
  | Expr.Id id -> addStr sb id
  | Expr.Literal literal -> literalToStr literal |> addStr sb
  | e -> failwithf "nameToCode fail: %A" e

let idToCode map sb id =
  match Map.tryFind id map with
  | Some iter -> addStr sb iter
  | None -> addStr sb id

let idOptToCode map sb = function
  | Some id -> idToCode map sb id
  | None -> ()

let labelToCode map sb key label =
  addStr sb key
  idOptToCode map sb label
  addSemiCol sb

let rec stmtListToCode map sb stmts = Array.iter (stmtItemToCode map sb) stmts

and stmtItemToCode map sb = function
  | Stmt stmt -> stmtToCode map sb stmt
  | Decl decl -> declToCode map sb decl

and stmtToCode map sb stmt =
  match stmt with
  | Stmt.Block body -> blockToCode map sb body
  | Stmt.Break label -> labelToCode map sb "break " label
  | Stmt.Continue label -> labelToCode map sb "continue " label
  | Stmt.Debugger -> addStr sb "debugger;"
  | Stmt.DoWhile (body, test) -> doWhileToCode map sb body test
  | Stmt.Empty -> addSemiCol sb
  | Stmt.Expr (expr, _) -> exprToCode map sb expr; addSemiCol sb
  | Stmt.For (init, cond, up, body) -> forToCode map sb init cond up body
  | Stmt.ForIn (bind, expr, body) -> forInOfToCode map sb " in " bind expr body
  | Stmt.ForOf (bind, expr, body) -> forInOfToCode map sb " of " bind expr body
  | Stmt.FuncDecl decl -> funcDeclToCode map sb decl
  | Stmt.If (test, tStmt, fStmt) -> ifToCode map sb test tStmt fStmt
  | Stmt.Labeled (id, body) -> labeledToCode map sb id body
  | Stmt.Return None -> addStr sb "return;"
  | Stmt.Return (Some arg) -> argStmtToCode map sb "return " arg
  | Stmt.Switch (test, cases) -> switchToCode map sb test cases
  | Stmt.Throw arg -> argStmtToCode map sb "throw " arg
  | Stmt.Try (body, catch, final) -> tryToCode map sb body catch final
  | Stmt.VarDecl decl -> varDeclToCode map sb true decl
  | Stmt.While (test, body) -> whileToCode map sb test body
  | Stmt.With (expr, body) -> withToCode map sb expr body
  addStr sb "\n"

and declToCode map sb decl =
  match decl with
  | Decl.ClassDecl decl -> classDeclToCode map sb decl
  | Decl.VarDecl decl -> varDeclToCode map sb true decl
  | Decl.FuncDecl decl -> funcDeclToCode map sb decl
  addStr sb "\n"

and exprToCode map sb = function
  | Expr.This -> addStr sb "this"
  | Expr.Id id -> idToCode map sb id
  | Expr.Literal literal -> literalToStr literal |> addStr sb
  | Expr.TempLiteral temp -> tempLiteralToCode map sb temp
  | Expr.Array elems -> arrayToCode map sb elems
  | Expr.Object props -> objToCode map sb props
  | Expr.Function expr -> funcExprToCode map sb expr
  | Expr.ArrowFunction (id, params_, body, isGen, isAsync) ->
    arrowFuncExprToCode map sb id params_ body isGen isAsync
  | Class (id, extends, body) -> classExprToCode map sb id extends body
  | TaggedTemp (expr, temp) -> taggedTempToCode map sb expr temp
  | Expr.Member expr -> memberExprToCode map sb expr
  | Expr.Super -> addStr sb "super"
  | Expr.MetaProp (id, key) -> metaPropToCode map sb id key
  | Expr.New (expr, args) -> newToCode map sb expr args
  | Expr.Call (callee, args) -> callToCode map sb callee args
  | Expr.Update (op, expr, prefix) -> updateToCode map sb op expr prefix
  | Expr.Await expr -> unExprToCode map sb expr "await "
  | Expr.Unary (op, expr) -> unOpToStr op |> unExprToCode map sb expr
  | Expr.Binary (op, l, r) -> binOpToStr op |> binExprToCode map sb l r
  | Expr.Logic (op, l, r) -> logicOpToStr op |> binExprToCode map sb l r
  | Expr.Cond (cond, tExpr, fExpr) -> condToCode map sb cond tExpr fExpr
  | Expr.Yield (arg, isGen) -> yieldToCode map sb arg isGen
  | Expr.Assign (op, left, init) -> assignExprToCode map sb op left init
  | Expr.Seq exprs -> seqExprToCode map sb exprs

and exprParenToCode map sb expr =
  addStr sb "("
  exprToCode map sb expr
  addStr sb ")"

and exprOptToCode map sb = function
  | Some expr -> exprToCode map sb expr
  | None -> ()

and exprOptKeyToCode map sb key = function
  | Some expr ->
    addStr sb key
    exprToCode map sb expr
  | None -> ()

and blockToCode map sb body =
  addStr sb "{\n"
  stmtListToCode map sb body
  addStr sb "}"

and bodyToCode map sb = function
  | Stmt.Block block -> blockToCode map sb block
  | stmt ->
    addStr sb "{\n"
    stmtToCode map sb stmt
    addStr sb "}"

and doWhileToCode map sb body test =
  addStr sb "do "
  bodyToCode map sb body
  addStr sb "while("
  exprToCode map sb test
  addStr sb ")"

and forToCode map sb init cond update body =
  addStr sb "for("
  forInitToCode map sb init
  addStr sb ";"
  exprOptToCode map sb cond
  addStr sb ";"
  exprOptToCode map sb update
  addStr sb ")"
  bodyToCode map sb body

and forInitToCode map sb = function
  | Some (ForInit.Expr expr) -> exprToCode map sb expr
  | Some (ForInit.VarDecl decl) -> varDeclToCode map sb false decl
  | None -> ()

and forInOfToCode map sb key bind expr body =
  addStr sb "for("
  forBindToCode map sb bind
  addStr sb key
  exprToCode map sb expr
  addStr sb ")"
  bodyToCode map sb body

and forBindToCode map sb = function
  | ForBind.VarDecl decl -> varDeclToCode map sb false decl
  | ForBind.Binding bind -> bindingToCode map sb bind

and ifToCode map sb test tStmt fStmt =
  addStr sb "if("
  exprToCode map sb test
  addStr sb ")"
  bodyToCode map sb tStmt
  match fStmt with
  | Some fStmt ->
    addStr sb "else "
    bodyToCode map sb fStmt
  | _ -> ()

and labeledToCode map sb id body =
  idToCode map sb id
  addStr sb ":\n"
  stmtToCode map sb body

and argStmtToCode map sb key arg =
  addStr sb key
  exprToCode map sb arg
  addSemiCol sb

and switchToCode map sb test cases =
  addStr sb "switch("
  exprToCode map sb test
  addStr sb "){\n"
  Array.iter (caseToCode map sb) cases
  addStr sb "}"

and caseToCode map sb (test, body) =
  match test with
  | Some test ->
    addStr sb "case "
    exprToCode map sb test
  | None -> addStr sb "default"
  addStr sb ":\n"
  Array.iter (stmtToCode map sb) body

and tryToCode map sb body catch final =
  addStr sb "try"
  blockToCode map sb body
  catchToCode map sb catch
  match final with
  | Some final ->
    addStr sb "finally"
    blockToCode map sb final
  | None -> ()

and catchToCode map sb = function
  | Some (bindOpt, body) ->
    addStr sb "catch("
    bindingOptToCode map sb bindOpt
    addStr sb ")"
    blockToCode map sb body
  | None -> ()

and whileToCode map sb test body =
  addStr sb "while("
  exprToCode map sb test
  addStr sb ")"
  bodyToCode map sb body

and withToCode map sb expr body =
  addStr sb "with("
  exprToCode map sb expr
  addStr sb ")"
  bodyToCode map sb body

and classDeclToCode map sb (id, extends, body) =
  addStr sb "class "
  idToCode map sb id
  exprOptKeyToCode map sb " extends " extends
  addStr sb "{\n"
  Array.iter (methodToCode map sb) body
  addStr sb "}"

and methodToCode map sb (key, body, kind, isComputed, isStatic) =
  let _, params_, body, isGen, isAsync = body
  if isStatic then addStr sb "static "
  else ()
  methodHeadToCode sb isGen isAsync
  methodKindToCode sb kind
  keyNameToCode map sb key isComputed
  paramsToCode map sb params_
  blockToCode map sb body

and varDeclToCode map sb needSemi (kind, declrs) =
  varKindToStr kind |> addStr sb
  Array.iter (varDeclrToCode map sb) declrs
  trimComma sb (Array.length declrs)
  if needSemi then addSemiCol sb

and varDeclrToCode map sb (bind, init) =
  bindingToCode map sb bind
  exprOptKeyToCode map sb " = " init
  addComma sb

and funcDeclToCode map sb (id, params_, body, isGen, isAsync) =
  funcHeadToCode sb isGen isAsync
  idToCode map sb id
  paramsToCode map sb params_
  blockToCode map sb body

and paramsToCode map sb params_ =
  addStr sb "("
  Array.iter (paramToCode map sb) params_
  trimComma sb (Array.length params_)
  addStr sb ")"

and paramToCode map sb param =
  match param with
  | Param.Id id -> idToCode map sb id
  | Param.BindingPt pt -> bindingPtToCode map sb pt
  | Param.AssignPt pt -> assignPtToCode map sb pt
  | Param.RestElem bind -> addStr sb "..."; bindingToCode map sb bind
  addComma sb

and bindingPtToCode map sb = function
  | ArrayPt elems -> arrayPtToCode map sb elems
  | ObjectPt props -> propsToCode map sb props

and arrayPtToCode map sb elems =
  addStr sb "["
  Array.iter (arrayPtElemToCode map sb) elems
  trimComma sb (Array.length elems)
  addStr sb "]"

and arrayPtElemToCode map sb elem =
  match elem with
  | ArrayPtElem.Id id -> idToCode map sb id
  | ArrayPtElem.BindingPt pt -> bindingPtToCode map sb pt
  | ArrayPtElem.AssignPt pt -> assignPtToCode map sb pt
  | ArrayPtElem.RestElem bind -> addStr sb "..."; bindingToCode map sb bind
  | ArrayPtElem.MemberExpr expr -> memberExprToCode map sb expr
  | ArrayPtElem.Empty -> ()
  addComma sb

and assignPtToCode map sb (bind, expr) =
  bindingToCode map sb bind
  addStr sb " = "
  exprToCode map sb expr

and bindingToCode map sb = function
  | Binding.Id id -> idToCode map sb id
  | Binding.BindingPt pt -> bindingPtToCode map sb pt
  | Binding.AssignPt pt -> assignPtToCode map sb pt
  | Binding.MemberExpr expr -> memberExprToCode map sb expr

and bindingOptToCode map sb = function
  | Some bind -> bindingToCode map sb bind
  | None -> ()

and tempLiteralToCode map sb (elems, exprs) =
  addStr sb "`"
  Array.iteri (Array.length exprs|> tempLiteralIter map sb exprs) elems
  addStr sb "`"

and tempLiteralIter map sb exprs last idx elem =
  addStr sb elem
  if idx < last then
    addStr sb "${"
    Array.get exprs idx |> exprToCode map sb
    addStr sb "}"
  else ()

and arrayToCode map sb elems =
  addStr sb "["
  Array.iter (arrayElemToCode map sb) elems
  trimComma sb (Array.length elems)
  addStr sb "]"

and arrayElemToCode map sb elem =
  match elem with
  | ArrayElem.Expr expr -> exprToCode map sb expr
  | ArrayElem.Spread expr -> addStr sb "..."; exprToCode map sb expr
  | ArrayElem.Empty -> ()
  addComma sb

and objToCode map sb props =
  addStr sb "("
  propsToCode map sb props
  addStr sb ")"

and propsToCode map sb props =
  addStr sb "{"
  Array.iter (propToCode map sb) props
  trimComma sb (Array.length props)
  addStr sb "}"

and propToCode map sb (key, value, kind, isComputed, isShort) =
  match kind with
  | PropKind.Init -> propInitToCode map sb key value isComputed isShort
  | kind ->
    propKindToCode sb kind
    keyNameToCode map sb key isComputed
    accToCode map sb value
  addComma sb

and propInitToCode map sb key value isComputed isShort =
  if isShort then propValToCode map sb value
  else
    keyNameToCode map sb key isComputed
    addStr sb " : "
    propValToCode map sb value

and propValToCode map sb = function
  | PropVal.Expr expr -> exprToCode map sb expr
  | PropVal.BindingPt pt -> bindingPtToCode map sb pt
  | PropVal.AssignPt pt -> assignPtToCode map sb pt
  | PropVal.Empty -> ()

and accToCode map sb = function
  | PropVal.Expr (Expr.Function (_, params_, body, false, false)) ->
    paramsToCode map sb params_
    blockToCode map sb body
  | _ -> raise (ToCodeException "accToCode")

and funcExprToCode map sb (id, params_, body, isGen, isAsync) =
  addStr sb "("
  funcHeadToCode sb isGen isAsync
  idOptToCode map sb id
  paramsToCode map sb params_
  blockToCode map sb body
  addStr sb ")"

and arrowFuncExprToCode map sb id params_ body isGen isAsync =
  addStr sb "("
  if isAsync then addStr sb "async "
  else ()
  if isGen then addStr sb "*"
  else ()
  idOptToCode map sb id
  paramsToCode map sb params_
  addStr sb "=>"
  arrowFuncBodyToCode map sb body
  addStr sb ")"

and arrowFuncBodyToCode map sb = function
  | ArrowFuncBody.Block body -> blockToCode map sb body
  | ArrowFuncBody.Expr expr -> exprToCode map sb expr

and classExprToCode map sb id extends body =
  addStr sb "(class "
  idOptToCode map sb id
  exprOptKeyToCode map sb " extends " extends
  addStr sb "{\n"
  Array.iter (methodToCode map sb) body
  addStr sb "})"

and taggedTempToCode map sb expr temp =
  exprToCode map sb expr
  addStr sb " "
  tempLiteralToCode map sb temp

and memberExprToCode map sb (expr, key, isComputed) =
  exprToCode map sb expr
  memNameToCode map sb key isComputed

and memNameToCode map sb key isComputed =
  if isComputed then
    addStr sb "["
    exprToCode map sb key
    addStr sb "]"
  else
    addStr sb "."
    nameToCode sb key

and keyNameToCode map sb key isComputed =
  if isComputed then
    addStr sb "["
    exprToCode map sb key
    addStr sb "]"
  else nameToCode sb key

and metaPropToCode map sb id key =
  idToCode map sb id
  addStr sb "."
  idToCode map sb key

and newToCode map sb expr args =
  addStr sb "new "
  exprToCode map sb expr
  argsToCode map sb args

and callToCode map sb callee args =
  calleeToCode map sb callee
  argsToCode map sb args

and calleeToCode map sb = function
  | Callee.Expr expr -> exprToCode map sb expr
  | Callee.Import -> addStr sb "import"

and argsToCode map sb args =
  addStr sb "("
  Array.iter (argToCode map sb) args
  trimComma sb (Array.length args)
  addStr sb ")"

and argToCode map sb arg =
  match arg with
  | Arg.Expr expr -> exprToCode map sb expr
  | Arg.Spread expr ->
    addStr sb "..."
    exprToCode map sb expr
  addComma sb

and updateToCode map sb op expr prefix =
  if prefix then
    upOpToStr op |> addStr sb
    exprToCode map sb expr
  else
    exprToCode map sb expr
    upOpToStr op |> addStr sb

and unExprToCode map sb expr key =
  addStr sb key
  exprToCode map sb expr

and binExprToCode map sb left right op =
  exprParenToCode map sb left
  addStr sb op
  exprParenToCode map sb right

and condToCode map sb cond tExpr fExpr =
  exprParenToCode map sb cond
  addStr sb " ? "
  exprParenToCode map sb tExpr
  addStr sb " : "
  exprParenToCode map sb fExpr

and yieldToCode map sb arg isGen =
  if isGen then addStr sb "yield* "
  else addStr sb "yield "
  exprOptToCode map sb arg

and assignExprToCode map sb op left expr =
  assignLeftToCode map sb left
  assignOpToStr op |> addStr sb
  exprToCode map sb expr

and assignLeftToCode map sb = function
  | AssignLeft.Expr expr -> exprToCode map sb expr
  | AssignLeft.Binding bind ->
    addStr sb "("
    bindingToCode map sb bind
    addStr sb ")"

and seqExprToCode map sb exprs =
  addStr sb "("
  Array.iter (seqElemToCode map sb) exprs
  trimComma sb (Array.length exprs)
  addStr sb ")"

and seqElemToCode map sb expr =
  exprToCode map sb expr
  addComma sb

let progToCode map prog =
  let sb = new SB ()
  match prog with
  | Script stmts -> stmtListToCode map sb stmts
  | _ -> failwith "todo"
  sb.ToString ()
