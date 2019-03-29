namespace Fuzzer

open AST
open Analyzer
open Common

type CountMap = Map<JSType, int> * int

type Cond = Map<Id, JSType>

type ScopeCond = Map<Id, ScopeLv>

type StmtBrick = StmtListItem * PreCond * ScopeCond

type GuardBrick = Guard * PreCond * ScopeCond

type StmtPool = ((CountMap * SyntaxCond) * (StmtBrick * Cond array) array) array

type GuardPool = ((CountMap * SyntaxCond) * (GuardBrick * Cond array) array) array

module CountMap =
  let empty = Map.empty, 0

  let private counter ret id ty =
    match Map.tryFind ty ret with
    | Some v -> Map.add ty (v + 1) ret
    | None -> Map.add ty 1 ret

  let ofCond cond = Map.fold counter Map.empty cond, Map.count cond

  let inline private isSubHelper big k v =
    match Map.tryFind k big with
    | Some x -> x >= v
    | None -> false

  let inline isSub big small =
    if Map.count big < Map.count small then false
    else Map.forall (isSubHelper big) small

module Pool =

  let toCond post = Map.mapVal fst post

  let toScopeCond post = Map.mapVal snd post

  let addBrickMap hval brick post map =
    match Map.tryFind hval map with
    | Some (brick, posts) -> Map.add hval (brick, (post :: posts)) map
    | None -> Map.add hval (brick, [post]) map

  let addPool pool pre hval body syntax post =
    let key = CountMap.ofCond pre, syntax
    let brick = body, pre, toScopeCond post
    let post = toCond post
    match Map.tryFind key pool with
    | Some map -> Map.add key (addBrickMap hval brick post map) pool
    | None -> Map.add key (Map.init hval (brick, [post])) pool

  let toSStmtPool sPool =
    let filter (_, syntax) =
      (syntax.Func || syntax.Try || syntax.Loop || syntax.Class || syntax.Gen)
      |> not
    let mapper ((k, _), arr) =
      k, Array.map (fun ((body, pre, _), conds) -> (body, pre), conds) arr

    Array.filter (fst >> filter) sPool
    |> Array.map mapper

  let conv pool =
    let mapper (_, (brick, conds)) = (brick, List.toArray conds)
    Map.mapVal (Map.toArray >> Array.map mapper) pool
    |> Map.toArray

  let count pool =
    let folder ret (_, arr) =
      Array.fold (fun ret v -> (snd v |> Array.length) + ret) ret arr
    Array.fold folder 0 pool

  let filterEmptySyntax x = (fst x |> snd) = Constraint.emptySyntax

  let initPools bricks =
    let folder (sPool, gPool) brick =
      let cons = brick.Constraint
      match brick.Body with
      | Guard guard ->
        sPool, addPool gPool cons.Pre brick.Hash guard cons.Syntax cons.Post
      | StmtListItem stmt ->
        addPool sPool cons.Pre brick.Hash stmt cons.Syntax cons.Post, gPool
    let sPool, gPool = List.fold folder (Map.empty, Map.empty) bricks
    let sPool = conv sPool
    (Array.filter filterEmptySyntax sPool, sPool), conv gPool
