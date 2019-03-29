namespace Fuzzer

open AST
open Analyzer
open Common

type TypeMap = Map<JSType, Id array>

type ScopeMap = Map<Id, ScopeLv * JSType>

type Context = (struct (TypeMap * CountMap * int * ScopeMap list * SyntaxCond))

module Context =
  let empty = struct (Map.empty, CountMap.empty, 0, [], Constraint.emptySyntax)

  let private convTMap tMap id ty =
    match Map.tryFind ty tMap with
    | Some ids -> Map.add ty (id :: ids) tMap
    | None -> Map.add ty [id] tMap

  let inline private toTMap post =
    Map.fold convTMap Map.empty post |> Map.mapVal List.toArray

  let private convCMap (cmap, cnt) ty ids =
    let length = Array.length ids
    Map.add ty length cmap, cnt + length

  let toCMap tMap = Map.fold convCMap (Map.empty, 0) tMap

  let private merger ret ty ids =
    match Map.tryFind ty ret with
    | Some ids2 -> Map.add ty (Array.append ids2 ids) ret
    | None -> Map.add ty ids ret

  let private merger2 ret ty ids =
    match Map.tryFind ty ret with
    | Some ids2 -> Map.add ty (Array.append ids2 ids |> Array.distinct) ret
    | None -> Map.add ty ids ret

  let inline private mergeTMap t1 t2 = Map.fold merger t1 t2

  let inline private mergeTMap2 t1 t2 = Map.fold merger2 t1 t2

  let private delFolder tMap id ty =
    Map.add ty (Map.find ty tMap |> Array.remove id) tMap

  let private delTMap tMap dMap =
    Map.fold delFolder tMap dMap

  let private scopeMerger cur id (lv, ty) =
    match Map.tryFind id cur with
    | Some (lv2, ty2) ->
      if lv = Pre then Map.add id (lv2, ty) cur
      else Map.add id (lv, ty) cur
    | _ -> Map.add id (lv, ty) cur

  let private mergeScope cur scope = Map.fold scopeMerger cur scope

  let update struct (tMap, cMap, idx, scopes, syntax) scope dMap post =
    let idx = idx + (Map.count post)
    let tMap = mergeTMap (delTMap tMap dMap) (toTMap post)
    let cMap = toCMap tMap
    match scopes with
    | [] -> struct (tMap, cMap, idx, scopes, syntax)
    | cur :: before ->
      struct (tMap, cMap, idx, (mergeScope cur scope) :: before, syntax)

  let inline prepareFilt struct (_, cmap, _, _, syntax) = struct (cmap, syntax)

  let inline preparePickCond struct (cur, (_, _), idx, _, _) = struct (cur, idx)

  let initBlk struct (tMap, cMap, idx, scopes, syntax) guard scope dMap post =
    let idx = idx + (Map.count post)
    let tMap = mergeTMap (delTMap tMap dMap) (toTMap post)
    let cMap = toCMap tMap
    struct (tMap, cMap, idx, scope :: scopes,
            CodeBrick.setGuardSyntax syntax guard)

  let private rmTMapItem tMap ty id =
    let ids = Map.find ty tMap |> Array.remove id
    Map.add ty ids tMap

  let private filterScope tMap scope lv =
    let folder tMap id (lv2, ty) =
      if lv2 = lv then rmTMapItem tMap ty id
      else tMap
    Map.fold folder tMap scope

  let finiBlk guard ctx0 ctx =
    let struct (tMap0, _, _, _, syntax) = ctx0
    let struct (tMap, _, idx, scopes, _) = ctx
    let scope, scopes = List.partitionFst scopes
    let tMap = CodeBrick.getGuardLv guard |> filterScope tMap scope
               |> mergeTMap2 tMap0
    let cMap = toCMap tMap
    struct (tMap, cMap, idx, scopes, syntax)
