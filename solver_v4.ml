(* A solver that solves by pruning "exclusives" before prioritizing cells with
 * the least number of candidates *)

module OrderedInt = struct
  type t = int
  let compare = (-)
end
module IMap = Map.Make (OrderedInt)

module IntMap = struct
  include IMap

  let fold_left f acc m = fold (fun key a b -> f b key a) m acc

  let fold_while f acc m =
    let rec aux seq_thunk accum =
      match seq_thunk () with
      | Seq.Nil -> accum
      | Seq.Cons ((key, a), seq_thunk') ->
          match f accum key a with
          | accum', `Stop -> accum'
          | accum', `Continue -> aux seq_thunk' accum'
    in aux (to_seq m) acc
end

module EltSet = struct
  type t = { len : int; set : int }
  let empty = { len = 0; set = 0 }
  let full = { len = 9; set = 511 }
  let cardinal t = t.len
  let mem elt t = t.set lor (1 lsl (elt - 1)) = t.set
  let add elt t =
    if mem elt t then t
    else { len = t.len + 1; set = t.set + (1 lsl (elt - 1)) }
  let remove elt t =
    if mem elt t then { len = t.len - 1; set = t.set - (1 lsl (elt - 1)) }
    else t
  let fold_left f acc t =
    let rec aux i acc' =
      if i > 9 then acc'
      else if mem i t then aux (i + 1) (f acc' i)
      else aux (i + 1) acc' in
    aux 1 acc
  let choose t =
    let rec aux i =
      if i > 9 then None
      else if mem i t then Some i
      else aux (i + 1) in
    aux 1
end

module IndexSet = struct
  type t = { count : int; table : int * int; keys : int list }
  let empty = { count = 0; table = 0, 0; keys = [] }
  let cardinal t = t.count
  let elt_bit a =
    if a <= 61 then 0, 1 lsl a
    else (1 lsl (a - 61)), 0
  let mem elt t =
    let cmp f = f t.table lor f (elt_bit elt) = f t.table in
    cmp fst && cmp snd
  let add elt t =
    if mem elt t then t
    else
      let aux f = f t.table + f (elt_bit elt) in
      let table = aux fst, aux snd in
      { count = t.count + 1; table; keys = elt :: t.keys }
  let fold f t = List.fold_right f t.keys
  let compare a b =  (* might need to be changed *)
    let cmp f = f a.table - f b.table in
    if (cmp fst) > 0 then 1
    else if (cmp fst) < 0 then -1
    else (cmp snd)
end

module IntSetMap = Map.Make (IndexSet)

let (>>=) = Option.bind
let (>>) f g = fun x -> g (f x)  (* reverse composition *)

(* sector functions *)
let col_of x = x mod 9
let row_of x = x / 9
let sec_of x = row_of x / 3 * 3 + col_of x / 3
let anchor_of_sec_indx x = x / 3 * 27 + x mod 3 * 3
let sec_anchor_of = sec_of >> anchor_of_sec_indx
let row_of_indx i = List.init 9 (fun x -> (row_of i) * 9 + x)
let col_of_indx i = List.init 9 (fun x -> (col_of i) + 9 * x)
let sec_of_indx i = List.init 9 (fun x -> (sec_anchor_of i) + x / 3 * 9 + x mod 3)

let print_solution = function
  | None -> print_endline " Solution not found."
  | Some solution ->
      IntMap.iter
      (fun i -> function
        | `Candidates _ -> assert false
        | `Filled x ->
            print_int x;
            if col_of i = 8 then print_newline () else print_char ' ')
      solution

let filter_filled_cells mtrx =
  List.filter_map
  (fun i ->
    match IntMap.find i mtrx with
    | `Filled _ -> None
    | `Candidates c -> Some (i, c))

let list_exclusives =
  List.fold_left
  (fun acc (indx, elt_set) ->
    EltSet.fold_left
    (fun acc' elt ->
      IntMap.update elt
      Option.(fold ~none:IndexSet.empty ~some:Fun.id >> IndexSet.add indx >> some)
      acc')
    acc
    elt_set)
  IntMap.empty
  >> IntMap.fold_left
     (fun acc elt indx_set ->
       IntSetMap.update indx_set
       Option.(fold ~none:EltSet.empty ~some:Fun.id >> EltSet.add elt >> some)
       acc)
     IntSetMap.empty
  >> IntSetMap.to_seq
  >> Seq.filter (fun (is, es) -> IndexSet.cardinal is = EltSet.cardinal es)

let prune_inds x mtrx inds =
  let aux idx =
    IntMap.update idx
    (fun elt ->
      elt >>= function
        | `Filled _ -> elt
        | `Candidates set -> Some (`Candidates (EltSet.remove x set))) in
  List.fold_right aux inds mtrx

let prune_exclusives_in_sector inds mtrx =
  filter_filled_cells mtrx inds
    |> list_exclusives
    |> Seq.fold_left
       (fun acc (indx_set, (elt_set : EltSet.t)) ->
         IndexSet.(fold
         (fun i ->
           IntMap.update i (fun _ -> Some (`Candidates elt_set)))
         indx_set acc))
       mtrx

let prune_exclusives mtrx =
  let row_start_f = ( * ) 9 in
  let col_start_f = Fun.id in
  let exclusives start_indx sec_inds_f =
    List.fold_right
    (fun i -> prune_exclusives_in_sector (sec_inds_f i))
    (List.init 9 start_indx) in
  exclusives row_start_f row_of_indx mtrx
      |> exclusives col_start_f col_of_indx
      |> exclusives anchor_of_sec_indx sec_of_indx

let prune_filled i x mtrx =
  List.map (fun f -> f i) [row_of_indx; col_of_indx; sec_of_indx]
    |> List.fold_left (prune_inds x) mtrx

let prune i x = prune_filled i x >> prune_exclusives

let fill i x = IntMap.update i (fun _ -> Some (`Filled x)) >> prune i x

let initialize mtrx =
  prune_exclusives
  @@ IntMap.fold
     (fun indx cell acc ->
       match cell with
       | `Filled n -> prune_filled indx n acc
       | _ -> acc)
     mtrx mtrx

let lowest_candidate_count =
  IntMap.fold_while
  (fun acc i cell ->
    match cell with
    | `Filled _ -> acc, `Continue
    | `Candidates candidates ->
        match acc with
        | `NoCandidate -> assert false
        | `Uninitialized ->
            let cnt = EltSet.cardinal candidates in
            if cnt = 1 then `LowestCount (i, cnt, candidates), `Stop
            else `LowestCount (i, cnt, candidates), `Continue
        | `LowestCount (_, cnt', _) ->
            let cnt = EltSet.cardinal candidates in
            if cnt = 0 then `NoCandidate, `Stop
            else if cnt = 1 then `LowestCount (i, cnt, candidates), `Stop
            else if cnt <= cnt' then `LowestCount (i, cnt, candidates), `Continue
            else acc, `Continue)
  `Uninitialized

let rec plug_n_chug i mtrx candidates =
  EltSet.choose candidates >>= fun x ->
    let next_candidates = EltSet.remove x candidates in
    match solve (fill i x mtrx) with
    | None -> plug_n_chug i mtrx next_candidates
    | s -> s

and solve mtrx =
  match lowest_candidate_count mtrx with
  | `NoCandidate -> None
  | `Uninitialized -> Some mtrx
  | `LowestCount (i, _, candidates) -> plug_n_chug i mtrx candidates

let parse str =
  let convert (i, x) =
    if x = '.' then i, `Candidates EltSet.full
    else i, `Filled (int_of_string @@ Char.escaped x) in
  (String.to_seqi >> Seq.map convert >> IntMap.of_seq) str

let () =
  let rec line_stream () =
    try Seq.Cons (input_line stdin, line_stream)
    with End_of_file -> Seq.Nil in
  Seq.iter (parse >> initialize >> solve >> print_solution) line_stream
