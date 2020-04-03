(* A solver that solves by pruning "exclusives" before prioritizing cells with
 * the least number of candidates *)

module OrderedInt = struct
  type t = int
  let compare = (-)
end
module IntSet = Set.Make (OrderedInt)
module IMap = Map.Make (OrderedInt)
module IntSetMap = Map.Make (IntSet)

module IntMap = struct
  include IMap

  let fold_left f acc m = fold (fun key a b -> f b key a) m acc

  let fold_while f acc m =
    let rec aux accum = function
      | [] -> accum
      | (key, a)::l' ->
          match f accum key a with
          | accum', `Stop -> accum'
          | accum', `Continue -> aux accum' l' in
    aux acc (bindings m)
end

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
  let add x o = Some ((IntSet.add x) @@ Option.value ~default:IntSet.empty o) in
  List.fold_left
  (fun acc (indx, elts) ->
    IntSet.fold (fun elt acc' -> IntMap.update elt (add indx) acc') elts acc)
  IntMap.empty
  >> IntMap.fold_left
     (fun acc elt inds -> IntSetMap.update inds (add elt) acc)
     IntSetMap.empty
  >> IntSetMap.bindings
  >> List.filter IntSet.(fun (is, es) -> cardinal is = cardinal es)

let prune_cell x idx mtrx =
  match IntMap.find idx mtrx with
  | `Filled _ -> Some mtrx
  | `Candidates set ->
      let set' = IntSet.remove x set in
      if IntSet.cardinal set' = 0 then None
      else Some (IntMap.add idx (`Candidates set') mtrx)

let prune_filled_by_index x mtrx =
  List.fold_left (fun acc i -> acc >>= prune_cell x i) (Some mtrx)

let prune_exclusives_by_index mtrx =
  filter_filled_cells mtrx
    >> list_exclusives
    >> List.fold_left
       (fun acc (inds, elts) ->
         IntSet.fold (fun i -> IntMap.add i (`Candidates elts)) inds acc)
       mtrx

let prune_exclusives mtrx =
  let row_start_f = ( * ) 9 in
  let col_start_f = Fun.id in
  let exclusives start_indx sec_inds_f =
    List.fold_right
    (fun i mtrx -> prune_exclusives_by_index mtrx (sec_inds_f i))
    (List.init 9 start_indx) in
  exclusives row_start_f row_of_indx mtrx
      |> exclusives col_start_f col_of_indx
      |> exclusives anchor_of_sec_indx sec_of_indx

let prune_filled i x mtrx =
  let aux indf acc = prune_filled_by_index x acc (indf i) in
  aux row_of_indx mtrx >>= aux col_of_indx >>= aux sec_of_indx

let fill i x =
  IntMap.add i (`Filled x) >> prune_filled i x >> Option.map prune_exclusives

let initialize mtrx =
  Option.map prune_exclusives
  @@ IntMap.fold
     (fun indx cell acc ->
       acc >>= fun m ->
       match cell with
       | `Filled n -> prune_filled indx n m
       | _ -> acc)
     mtrx (Some mtrx)

let find_min_cell =
  IntMap.fold_while
  (fun acc i cell ->
    match cell with
    | `Filled _ -> acc, `Continue
    | `Candidates candidates ->
        let n = IntSet.cardinal candidates in
        if n = 1 then Some (i, n, candidates), `Stop
        else
          match acc with
          | None -> Some (i, n, candidates), `Continue
          | Some (_, n', _) when n <= n' -> Some (i, n, candidates), `Continue
          | _ -> acc, `Continue)
  None

let rec plug_n_chug i mtrx candidates =
  IntSet.choose_opt candidates >>= fun x ->
    let next_candidates = IntSet.remove x candidates in
    match solve (fill i x mtrx) with
    | None -> plug_n_chug i mtrx next_candidates
    | s -> s

and solve mtrx =
  mtrx >>= fun m ->
  Option.fold ~none:mtrx ~some:(fun (i, _, cs) -> plug_n_chug i m cs) (find_min_cell m)

let parse str =
  let convert (i, x) =
    if x = '.' then i, `Candidates (IntSet.of_list @@ List.init 9 (( + ) 1))
    else i, `Filled (int_of_string @@ Char.escaped x) in
  (String.to_seqi >> Seq.map convert >> IntMap.of_seq) str

let () =
  let rec line_stream () =
    try Seq.Cons (input_line stdin, line_stream)
    with End_of_file -> Seq.Nil in
  Seq.iter (parse >> initialize >> solve >> print_solution) line_stream
