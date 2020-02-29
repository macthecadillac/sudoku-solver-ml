String.iteri
(fun i c ->
  Printf.printf "%c " c;
  if i mod 9 = 8 then print_newline ())
(input_line stdin)
