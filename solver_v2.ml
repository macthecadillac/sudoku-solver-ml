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

  let fold_while f m acc =
    let rec aux seq_thunk accum =
      match seq_thunk () with
      | Seq.Nil -> accum
      | Seq.Cons ((key, a), seq_thunk') ->
          match f key a accum with
          | accum', `Stop -> accum'
          | accum', `Continue -> aux seq_thunk' accum'
    in aux (to_seq m) acc
end

(* sector functions *)
let col x = x mod 9
let row x = x / 9
let cell x = row x / 3 + col x / 3 * 3

let print_solution = function
  | `Deadend -> print_endline " Solution not found."
  | `Solution solution ->
      IntMap.iter
      (fun i x ->
        Printf.printf "%i " x;
        if col i = 8 then print_newline ())
      solution

let used_numbers secfn mtrx =
  IntMap.fold
  (fun i x acc ->
    IntMap.update (secfn i)
    (function
      | None -> Some (IntSet.add x IntSet.empty)
      | Some set -> Some (if x <> 0 then IntSet.add x set else set))
    acc)
  mtrx
  IntMap.empty

let lowest_candidate_count mtrx =
  let secfn = [row; col; cell] in
  let used = List.map (fun f -> used_numbers f mtrx) secfn in
  let available_numbers_of_cell i mtrx =
    List.map2 (fun f u -> IntMap.find (f i) u) secfn used
    |> List.fold_left IntSet.union IntSet.empty
    |> IntSet.diff (IntSet.of_list [1; 2; 3; 4; 5; 6; 7; 8; 9])
    |> IntSet.elements in
  IntMap.fold_while
  (fun i x acc ->
    let thk () =
      let avail = available_numbers_of_cell i mtrx in
      List.length avail, avail in
    match acc with
    | `NoCandidate -> assert false
    | `Uninitialized when x <> 0 -> `Uninitialized, `Continue
    | `Uninitialized -> let cnt, av = thk () in `LowestCount (i, cnt, av), `Continue
    | `LowestCount (_, cnt', _) ->
        if x <> 0 then acc, `Continue
        else let cnt, av = thk () in
        if cnt = 0 then `NoCandidate, `Stop
        else if cnt = 1 then `LowestCount (i, cnt, av), `Stop
        else if cnt <= cnt' then `LowestCount (i, cnt, av), `Continue
        else acc, `Continue)
  mtrx
  `Uninitialized

let rec plug_n_chug i mtrx = function
  | [] -> `Deadend
  | x::next_candidates ->
      match solve (IntMap.update i (fun _ -> Some x) mtrx) with
      | `Deadend -> plug_n_chug i mtrx next_candidates
      | s -> s

and solve mtrx =
  match lowest_candidate_count mtrx with
  | `NoCandidate -> `Deadend
  | `Uninitialized -> `Solution mtrx
  | `LowestCount (i, _, candidates) -> plug_n_chug i mtrx candidates

let () =
  List.init 9 (fun _ -> input_line stdin)
    |> List.map (String.split_on_char ' ')
    |> List.concat
    |> List.mapi (fun i x -> i, int_of_string x)
    |> List.to_seq
    |> IntMap.of_seq
    |> solve
    |> print_solution
