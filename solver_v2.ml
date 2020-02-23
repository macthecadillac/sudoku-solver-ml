(* A solver that solves by prioritizing cells with the least number of
 * candidates *)

(* TODO:
 * Optimizations:
 * 1. Automatically fill in cells with only one candidate instead of folding
 *    over the entire matrix every pass
 * 2. Try fold and exit once a cell with only two candidates is found in
 *    least_candidate_count
 *)

module OrderedInt = struct
  type t = int
  let compare = (-)
end
module IntSet = Set.Make (OrderedInt)
module IntMap = Map.Make (OrderedInt)

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

let available_numbers_of_cell i mtrx =
  List.to_seq [row; col; cell]
    |> Seq.map (fun f -> IntMap.find (f i) (used_numbers f mtrx))
    |> Seq.fold_left IntSet.union IntSet.empty
    |> IntSet.diff (IntSet.of_list [1; 2; 3; 4; 5; 6; 7; 8; 9])
    |> IntSet.elements

let least_candidate_count mtrx =
  IntMap.fold
  (fun i x acc ->
    let cnt, av =
      let avail = available_numbers_of_cell i mtrx in
      List.length avail, avail in
    match acc with
    | `NoCandidate -> `NoCandidate
    | `Uninitialized when x <> 0 -> `Uninitialized
    | `Uninitialized -> `LowestCount (i, cnt, av)
    | `LowestCount (_, cnt', _) ->
        if x <> 0 then acc
        else if cnt = 0 && x = 0 then `NoCandidate
        else if cnt <= cnt' then `LowestCount (i, cnt, av)
        else acc)
  mtrx
  `Uninitialized

let rec plug_n_chug i mtrx = function
  | [] -> `Deadend
  | x::next_candidates ->
      match solve (IntMap.update i (fun _ -> Some x) mtrx) with
      | `Deadend -> plug_n_chug i mtrx next_candidates
      | s -> s

and solve mtrx =
  match least_candidate_count mtrx with
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
