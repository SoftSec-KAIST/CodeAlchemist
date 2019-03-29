module Fuzzer.Selector

open Common
open Analyzer

let private getRmapMapper rnd cur ty cnt =
  Map.find ty cur |> Random.sampleN rnd cnt

let inline private adjustHelper vars rmap imap idx id ty =
  let rId = Array.get (Map.find ty vars) idx
  Map.add id rId rmap, Map.add ty (idx + 1) imap

let private adjustRMap vars (rmap, imap) id ty =
  match Map.tryFind ty imap with
  | Some idx -> adjustHelper vars rmap imap idx id ty
  | None -> adjustHelper vars rmap imap 0 id ty

let getReplaceMap rnd cur cmap pre =
  let vars = Map.map (getRmapMapper rnd cur) cmap
  Map.fold (adjustRMap vars) (Map.empty, Map.empty) pre
  |> fst

let private updater (rMap, post, idx) id ty =
  match Map.tryFind id rMap with
  | Some x -> (rMap, Map.add x ty post, idx)
  | None ->
    let nId = sprintf "v%d" idx
    (Map.add id nId rMap, Map.add nId ty post, idx + 1)

let updatePost idx rMap post =
  let rMap, post, _ = Map.fold updater (rMap, Map.empty, idx) post
  rMap, post

let getDelMap rMap pre post =
  let folder (dMap, post) id ty =
    match Map.tryFind id pre with
    | Some pTy ->
      if pTy = ty then (dMap, Map.remove id post)
      else (Map.add (Map.find id rMap) pTy dMap, post)
    | None -> (dMap, post)
  Map.fold folder (Map.empty, post) post

let pickCond rnd ctx (cmap, _) ((stmt, pre, scope), posts) =
  let struct (cur, idx) = Context.preparePickCond ctx
  let rMap = getReplaceMap rnd cur cmap pre
  let post = Random.sample rnd posts
  let dMap, post = getDelMap rMap pre post
  let rMap, post = updatePost idx rMap post
  let folder ret id lv =
    let nId = Map.find id rMap
    match Map.tryFind nId post with
    | Some ty -> Map.add nId (lv, ty) ret
    | None -> Map.add nId (lv, Map.find id pre) ret
  let scope = Map.fold folder Map.empty scope
  struct (stmt, rMap, scope, dMap, post)

let filtSBrick map cnt (((map2, cnt2), syntax2), _) =
  if cnt < cnt2 then false
  else CountMap.isSub map map2

let filtBrick map cnt syntax (((map2, cnt2), syntax2), _) =
  if cnt < cnt2 then false
  else Constraint.isSubSyntax syntax syntax2 && CountMap.isSub map map2

let filterPool ctx (ssPool, sPool) =
  let struct ((map, cnt), syntax) = Context.prepareFilt ctx
  if syntax = Constraint.emptySyntax then
    Array.filter (filtSBrick map cnt) ssPool
  else Array.filter (filtBrick map cnt syntax) sPool

let pickBrick rnd pool ctx =
  let (cmap, _), cands = filterPool ctx pool |> Random.sample rnd
  Random.sample rnd cands |> pickCond rnd ctx cmap

let pickCtx rnd struct (tMap, (cMap, _), idx, scopes, syntax) =
  let folder (tMap2, (cMap2, cnt)) ty ids =
    let n = Map.find ty cMap |> Random.nxt rnd
    if n = 0 then (tMap2, (cMap2, cnt))
    else (Map.add ty (Random.sampleN rnd n ids) tMap2,
          (Map.add ty n cMap2, cnt + n))
  let tMap, cMap = Map.fold folder (Map.empty, (Map.empty, 0)) tMap
  struct (tMap, cMap, idx, scopes, syntax)

let pickGuard rnd gPool ctx =
  let struct ((map, cnt), syntax) = Context.prepareFilt ctx
  let gPool = Array.filter (filtBrick map cnt syntax) gPool
  if Array.length gPool = 0  then None
  else
    let (cmap, _), cands = Random.sample rnd gPool
    Random.sample rnd cands |> pickCond rnd ctx cmap |> Some
