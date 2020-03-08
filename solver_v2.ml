(* A solver that solves by prioritizing cells with the least number of
 * candidates *)

module OrderedInt = struct
  type t = int
  let compare = (-)
end
module IntSet = Set.Make (OrderedInt)
module IMap = Map.Make (OrderedInt)

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
let (>>) f g = fun x -> g (f x)

(* sector functions *)
let col_of x = x mod 9
let row_of x = x / 9
let sector_of x = row_of x / 3 + col_of x / 3 * 3

let print_solution = function
  | `Deadend -> print_endline " Solution not found."
  | `Solution solution ->
      IntMap.iter
      (fun i -> function
        | `Candidates _ -> assert false
        | `Filled x ->
            Printf.printf "%i " x;
            if col_of i = 8 then print_newline ())
      solution

let prune i x mtrx =
  let row, col = (row_of i), (col_of i) in
  let row_cells = List.init 9 (fun x -> row * 9 + x) in
  let col_cells = List.init 9 (fun x -> 9 * x + col) in
  let sector_anchor = (i / 9 / 3) * 27 + (i mod 9 / 3) * 3 in
  let sector_cells = List.init 9 (fun x -> sector_anchor + (x / 3) * 9 + x mod 3) in
  let remove_elt idx acc =
    IntMap.update idx
    (fun elt ->
      let* cell = elt in
      match cell with
      | `Filled _ -> elt
      | `Candidates set -> Some (`Candidates (IntSet.remove x set)))
    acc in
  List.fold_right remove_elt row_cells mtrx
      |> List.fold_right remove_elt col_cells
      |> List.fold_right remove_elt sector_cells

let fill_mtrx i x = IntMap.update i (fun _ -> Some (`Filled x)) >> prune i x

let initialize mtrx =
  IntMap.fold
  (fun indx cell acc ->
    match cell with
    | `Filled n -> prune indx n acc
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
      let next_candidates = IntSet.remove x candidates in
      match solve (fill_mtrx i x mtrx) with
      | `Deadend -> plug_n_chug i mtrx next_candidates
      | s -> s

and solve mtrx =
  match lowest_candidate_count mtrx with
  | `NoCandidate -> `Deadend
  | `Uninitialized -> `Solution mtrx
  | `LowestCount (i, _, candidates) -> plug_n_chug i mtrx candidates

let parse str =
  let convert (i, x) =
    if x = '.' then i, `Candidates (IntSet.of_list (List.init 9 (( + ) 1)))
    else i, `Filled (int_of_string @@ Char.escaped x) in
  (String.to_seqi >> Seq.map convert >> IntMap.of_seq) str

let () =
  let rec line_stream () =
    try Seq.Cons (input_line stdin, line_stream)
    with End_of_file -> Seq.Nil in
  Seq.iter (parse >> initialize >> solve >> print_solution) line_stream
