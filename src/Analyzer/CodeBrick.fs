namespace Analyzer

open System
open System.Text
open AST
open AST.Normalize
open AST.CodeGen
open Common

type Guard =
  | For of ForInit option * Expr option * Expr option
  | ForIn of ForBind * Expr
  | ForOf of ForBind * Expr
  | While of Expr
  | DoWhile of Expr

type BrickBody =
  | StmtListItem of StmtListItem
  | Guard of Guard

type CodeBrick = {
  Hash: int64
  Body: BrickBody
  Constraint: Constraint
  NSymMap: Map<Id, Id>
}

type CodeBricks = Map<int64, CodeBrick>

type Context = {
  Pool: CodeBricks
  GuardMap: Map<int64, int64> // StmtListItem -> Guard
  IsBuiltIn: string -> bool
  IsFilters: CodeBrick -> bool
}

module Context =
  let init conf = {
    Pool = Map.empty
    GuardMap = Map.empty
    IsBuiltIn = Array.includes conf.BuiltIns
    IsFilters =
      (fun x -> Map.foranyKeys (Array.includes conf.Filters) x.NSymMap)
  }

  let prepareBrick brick cons = {
    brick with Constraint = Constraint.normalize cons brick.NSymMap
               NSymMap = Map.filter String.neq brick.NSymMap
  }

  let addBrick ctx cons brick =
    if ctx.IsFilters brick then ctx, false
    else
      let hval = brick.Hash
      let pool = ctx.Pool
      match Map.tryFind hval pool with
      | Some _ -> ctx, true
      | None ->
        let brick = prepareBrick brick cons
        { ctx with Pool = Map.add hval brick pool }, true

  let addGBrick ctx cons sHash brick =
    match addBrick ctx cons brick with
    | ctx, true -> { ctx with GuardMap = Map.add sHash brick.Hash ctx.GuardMap }
    | ctx, false -> ctx

module CodeBrick =
  let calcHash sb =
    use md5 = System.Security.Cryptography.MD5.Create()
    (sb.ToString () |> Encoding.UTF8.GetBytes |> md5.ComputeHash, 0)
    |> BitConverter.ToInt64

  let ofStmt stmt ctx =
    let nMap, nStmt = normalizeStmt ctx.IsBuiltIn Map.empty stmt
    let sb = new SB ()
    stmtToCode Map.empty sb nStmt
    { Hash = calcHash sb
      Body = StmtListItem.Stmt nStmt |> BrickBody.StmtListItem
      Constraint = Constraint.empty
      NSymMap = nMap }

  let ofDecl decl ctx =
    let nMap, nDecl = normalizeDecl ctx.IsBuiltIn Map.empty decl
    let sb = new SB ()
    declToCode Map.empty sb nDecl
    { Hash = calcHash sb
      Body = StmtListItem.Decl nDecl |> BrickBody.StmtListItem
      Constraint = Constraint.empty
      NSymMap = nMap }

  let guardToStmt = function
    | For (init, cond, up) -> Stmt.For (init, cond, up, Stmt.Empty)
    | ForIn (bind, expr) -> Stmt.ForIn (bind, expr, Stmt.Empty)
    | ForOf (bind, expr) -> Stmt.ForOf (bind, expr, Stmt.Empty)
    | While cond -> Stmt.While (cond, Stmt.Empty)
    | DoWhile cond -> Stmt.DoWhile (Stmt.Empty, cond)

  let stmtToGuard = function
    | Stmt.For (init, cond, up, Stmt.Empty) -> For (init, cond, up)
    | Stmt.ForIn (bind, expr, Stmt.Empty) -> ForIn (bind, expr)
    | Stmt.ForOf (bind, expr, Stmt.Empty) -> ForOf (bind, expr)
    | Stmt.While (cond, Stmt.Empty) -> While cond
    | Stmt.DoWhile (Stmt.Empty, cond) -> DoWhile cond
    | stmt -> failwithf "stmtToGuard fail: %A" stmt

  let normalizeGuard filter guard sb =
    addStr sb "// Guard\n"
    let nMap, nStmt = guardToStmt guard |> normalizeStmt filter Map.empty
    stmtToCode Map.empty sb nStmt
    nMap, stmtToGuard nStmt, sb

  let ofGuard guard ctx =
    let nMap, nGuard, sb = new SB () |> normalizeGuard ctx.IsBuiltIn guard
    { Hash = calcHash sb
      Body = BrickBody.Guard nGuard
      Constraint = Constraint.empty
      NSymMap = nMap }

  let getLoggers brick cons = Constraint.getLogger brick.Hash brick.NSymMap cons

  let getTempVar brick =
    let hval = brick.Hash
    if hval < 0L then sprintf "temp_%d" (-hval)
    else sprintf "temp_%d" hval

  let loadTypes pre post brick =
    { brick with Constraint = Constraint.loadTypes brick.Constraint pre post }

  let inline private whileToCodeInit map sb cond =
    addStr sb "while("
    exprToCode map sb cond
    addStr sb "){\n"

  let inline private forToCodeInit map sb init cond update =
    addStr sb "for("
    forInitToCode map sb init
    addStr sb ";"
    exprOptToCode map sb cond
    addStr sb ";"
    exprOptToCode map sb update
    addStr sb "){\n"

  let inline private forInOfToCode map sb key bind expr =
    addStr sb "for("
    forBindToCode map sb bind
    addStr sb key
    exprToCode map sb expr
    addStr sb "){\n"

  let guardToCodeInit map sb guard =
    addStr sb "// GenBlkBrick\n"
    match guard with
    | DoWhile _ -> addStr sb "do {\n"
    | For (init, cond, update) -> forToCodeInit map sb init cond update
    | ForIn (bind, expr) -> forInOfToCode map sb " in " bind expr
    | ForOf (bind, expr) -> forInOfToCode map sb " of " bind expr
    | While cond -> whileToCodeInit map sb cond

  let guardToCodeFini map sb = function
    | DoWhile cond ->
      addStr sb "} while("
      exprToCode map sb cond
      addStr sb ");\n"
    | _ -> addStr sb "}\n"

  let setGuardSyntax syntax = function
    | _ -> { syntax with Loop = true }

  let getGuardLv = function
    | _ -> ScopeLv.Block
