namespace Common

module List =
  let partitionFst = function
    | first :: remain -> first, remain
    | _ -> failwith "List.partitionFst fail"

module Array =
  let revList arr = Array.ofList arr |> Array.rev

  let includes arr x = Array.contains x arr

  let remove item arr =
    let idx  = Array.findIndex (fun x -> x = item) arr
    if idx = 0 then arr.[1..]
    else Array.append arr.[.. idx - 1] arr.[idx + 1 ..]

module Map =
  let inline count (map: Map<_,_>) = map.Count

  let private mkValFunc f _ v = f v

  let private mkKeyFunc f k _ = f k

  let private mkKeyMap f map k v = Map.add (f k) v map

  let mapVal f map = Map.map (mkValFunc f) map

  let mapKey f map = Map.fold (mkKeyMap f) Map.empty map

  let get map key = Map.find key map

  let filterVal f map = Map.filter (mkValFunc f) map

  let filterKey f map = Map.filter (mkKeyFunc f) map

  let inline containsKey2 map key = Map.containsKey key map

  let getKeys map = Map.toArray map |> Array.map fst

  let private mergeHelper map k v = Map.add k v map

  let merge map1 map2 = Map.fold mergeHelper map1 map2

  let private delKeysHelper keys k v = Array.contains k keys |> not

  let delKeys map keys = Map.filter (delKeysHelper keys) map

  let private foranyKeyHelper f k _ = f k |> not

  let foranyKeys f map = Map.forall (foranyKeyHelper f) map |> not

  let init k v = Map.add k v Map.empty

module Tuple =
  let inline map f (a, b) = (f a, f b)

module Set =
  let init item = Set.add item Set.empty

module String =
  let neq a b = a <> b

  let add (a: string) (b: string) = a + b

  let lastIndexOf (a: string) (b: string) = b.LastIndexOf (a)

  let contains (big: string) (small: string) = big.Contains small

module Random =
  let initSeed n =
    let rnd = new System.Random ()
    Array.init n (fun _ -> rnd.Next ())

  let inline nxt (rnd: System.Random) max = rnd.Next (max)

  let inline sample rnd arr = Array.length arr |> nxt rnd |> Array.get arr

  let weightedSample rnd f arr =
    let map x =
      let ret = f x
      if ret = 0  then 1 else ret
    let length = Array.length arr
    if length = 1 then Array.get arr 0
    else
      let weights = Array.map map arr
      let rec getter idx remain =
        if idx < length then
          let w  = Array.get weights idx
          if w > remain then idx
          else getter (idx + 1) (remain - w)
        else length - 1

      Array.sum weights |> nxt rnd |> getter 0 |> Array.get arr

  let private init rnd n _ = nxt rnd n

  let private getCnt arr length item =
    let rec loop idx cnt =
      if cnt < 2 && idx < length then
        if Array.get arr idx = item then loop (idx + 1) (cnt + 1)
        else loop (idx + 1) cnt
      else cnt
    loop 0 0

  let private getUnique init arr length =
    let rec loop item =
      if getCnt arr length item = 0 then item
      else init 0 |> loop
    init 0 |> loop

  let rec private deDup init length arr idx =
    if idx < length  then
      if Array.get arr idx |> getCnt arr length = 2 then
        arr.[idx] <- getUnique init arr length
        deDup init length arr (idx + 1)
      else deDup init length arr (idx + 1)
    else arr

  let sampleN rnd n arr =
    let init = Array.length arr |> init rnd
    let first = Array.init n init
    deDup init n first 0 |> Array.map (Array.get arr)
