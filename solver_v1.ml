(* Simple brute force solver *)

module IntSet = Set.Make (struct
  type t = int
  let compare = (-)
end)

let print_solution = function
  | `Deadend -> print_endline " Solution not found."
  | `Solution s ->
      List.iteri
      (fun i (_, x) ->
        Printf.printf "%i " x;
        if i mod 9 = 8 then print_newline ()) s

let used indxf i =
  List.fold_left
  (fun s (i', x) ->
    if indxf i = indxf i' && x <> 0 then IntSet.add x s else s)
  IntSet.empty

let unused_numbers i mtrx =
  (* (S ∖ rowS) ∩ [(S ∖ colS) ∩ (S ∖ sqS)] = S ∖ [rowS ∪ (colS ∪ sqS)] *)
  IntSet.union (used (fun x -> x / 9) i mtrx) (used (fun x -> x mod 9) i mtrx)
    |> IntSet.union (used (fun x -> (x / 9) / 3 + 3 * (x mod 9 / 3)) i mtrx)
    |> IntSet.diff (IntSet.of_list [1; 2; 3; 4; 5; 6; 7; 8; 9])
    |> IntSet.elements

let rec solve l r =
  match r with
  | [] -> `Solution (List.rev l)
  | (i, x) as hd :: tl ->
      if x <> 0 then solve (hd :: l) tl
      else search i l tl (unused_numbers i (List.rev_append l r))

and search i l tl = function
  | [] -> `Deadend
  | x' :: tl' ->
      match solve ((i, x') :: l) tl with
      | `Deadend -> search i l tl tl'
      | s -> s

let () =
  List.init 9 (fun _ -> input_line stdin)
    |> List.map (String.split_on_char ' ')
    |> List.concat
    |> List.mapi (fun i x -> i, int_of_string x)
    |> solve []
    |> print_solution
