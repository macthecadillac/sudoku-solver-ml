(* A solver that solves by pruning "exclusives" before prioritizing cells with
 * the least number of candidates *)

module OrderedInt = struct
  type t = int
  let compare = (-)
end
module IntSet = Set.Make (OrderedInt)
module IMap = Map.Make (OrderedInt)
module IntSetMap = Map.Make (struct type t = IntSet.t let compare = compare end)

module IntMap = struct
  include IMap

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

let (let*) = Option.bind
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
  | `Deadend -> print_endline " Solution not found."
  | `Solution solution ->
      IntMap.iter
      (fun i -> function
        | `Candidates _ -> assert false
        | `Filled x ->
            print_int x;
            if col_of i = 8 then print_newline () else print_char ' ')
      solution

let used_numbers secfn mtrx =
  IntMap.fold
  (fun i cell acc ->
    IntMap.update (secfn i)
    (function
      | None -> (match cell with
          | `Filled x -> Some (IntSet.add x IntSet.empty)
          | _ -> Some IntSet.empty)
      | Some set -> match cell with
          | `Filled x -> Some (IntSet.add x set)
          | _ -> Some set)
    acc)
  mtrx
  IntMap.empty

let analyze mtrx =
  let secfn = [row_of; col_of; sec_of] in
  let used = List.map (fun f -> used_numbers f mtrx) secfn in
  let available_numbers_of_cell i =
    List.map2 (fun f u -> IntMap.find (f i) u) secfn used
      |> List.fold_left IntSet.union IntSet.empty
      |> IntSet.diff (IntSet.of_list (List.init 9 ((+) 1))) in
  IntMap.mapi
  (fun i cell ->
    match cell with
    | `Filled _ -> cell
    | `Candidates _ -> `Candidates (available_numbers_of_cell i))
  mtrx

let filter_filled_cells inds mtrx =
  List.fold_left
  (fun set_map x ->
    match IntMap.find x mtrx with
    | `Filled _ -> set_map
    | `Candidates c -> IntMap.add x c set_map)
  IntMap.empty inds

let list_exclusives candidates =
  List.init 9 (( + ) 1)
    |> List.map
       (fun x ->
         x, IntMap.fold
            (fun indx s acc -> if IntSet.mem x s then IntSet.add indx acc else acc)
            candidates
            IntSet.empty)
    |> List.filter (fun (_, s) -> IntSet.cardinal s < 4)
    |> List.fold_left
       (fun acc (elt, indx_set) ->
         IntSetMap.update indx_set
         IntSet.(Option.fold ~none:empty ~some:Fun.id >> add elt >> Option.some)
         acc)
       IntSetMap.empty
    |> IntSetMap.to_seq
    |> Seq.filter IntSet.(fun (is, es) -> cardinal is = cardinal es)

let prune_inds inds x mtrx =
  let aux idx =
    IntMap.update idx
    (fun elt ->
      let* cell = elt in
      match cell with
      | `Filled _ -> elt
      | `Candidates set -> Some (`Candidates (IntSet.remove x set))) in
  List.fold_right aux inds mtrx

let prune_exclusives_in_sector mtrx inds =
  filter_filled_cells inds mtrx
    |> list_exclusives
    |> Seq.fold_left
       (fun acc (indx_set, elt_set) ->
         IntSet.(fold
         (fun i acc' ->
           IntMap.update i (fun _ -> Some (`Candidates elt_set)) acc')
         indx_set acc)
         |> List.fold_right
            (fun i acc ->
              if IntSet.mem i indx_set then acc
              else match IntMap.find i acc with
              | `Filled _ -> acc
              | `Candidates set ->
                  IntMap.update i (fun _ ->
                    Some (`Candidates IntSet.(inter elt_set set |> diff set))) acc)
            inds)
       mtrx

let prune_exclusives mtrx =
  let row_start_f = ( * ) 9 in
  let col_start_f = Fun.id in
  let exclusives start_indx sec_inds_f =
    List.fold_right
    (fun i acc -> prune_exclusives_in_sector acc (sec_inds_f i))
    (List.init 9 start_indx) in
  exclusives row_start_f row_of_indx mtrx
      |> exclusives col_start_f col_of_indx
      |> exclusives anchor_of_sec_indx sec_of_indx

let prune_filled i x mtrx =
  List.map (fun f -> f i) [row_of_indx; col_of_indx; sec_of_indx]
    |> List.fold_left (fun acc inds -> prune_inds inds x acc) mtrx

let prune i x = prune_filled i x >> prune_exclusives

let fill i x = IntMap.update i (fun _ -> Some (`Filled x)) >> prune i x

let lowest_candidate_count =
  IntMap.fold_while
  (fun acc i cell ->
    match cell with
    | `Filled _ -> acc, `Continue
    | `Candidates candidates ->
        match acc with
        | `NoCandidate -> assert false
        | `Uninitialized ->
            let cnt = IntSet.cardinal candidates in
            `LowestCount (i, cnt, candidates), `Continue
        | `LowestCount (_, cnt', _) ->
            let cnt = IntSet.cardinal candidates in
            if cnt = 0 then `NoCandidate, `Stop
            else if cnt = 1 then `LowestCount (i, cnt, candidates), `Stop
            else if cnt <= cnt' then `LowestCount (i, cnt, candidates), `Continue
            else acc, `Continue)
  `Uninitialized

let rec plug_n_chug i mtrx candidates =
  match IntSet.min_elt_opt candidates with
  | None -> `Deadend
  | Some x ->
      Printf.printf "%i -> (%i, %i)\n" x (row_of i + 1) (col_of i + 1);
      let next_candidates = IntSet.remove x candidates in
      match solve (fill i x mtrx) with
      | `Deadend -> plug_n_chug i mtrx next_candidates
      | s -> s

and solve mtrx =
  match lowest_candidate_count mtrx with
  | `NoCandidate -> `Deadend
  | `Uninitialized -> `Solution mtrx
  | `LowestCount (i, _, candidates) -> plug_n_chug i mtrx candidates

let parse str =
  let convert (i, x) =
    if x = '.' then i, `Candidates IntSet.empty
    else i, `Filled (int_of_string @@ Char.escaped x) in
  (String.to_seqi >> Seq.map convert >> IntMap.of_seq) str

let () =
  let rec line_stream () =
    try Seq.Cons (input_line stdin, line_stream)
    with End_of_file -> Seq.Nil in
  Seq.iter (parse >> analyze >> prune_exclusives >> solve >> print_solution) line_stream
