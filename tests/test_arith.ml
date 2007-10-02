open Peg;;
open Arith;;

let rec eval = function
  | Node(N_Root, _, [x]) -> eval x
  | Node(N_add, _, [x;y]) -> (eval x) + (eval y)
  | Node(N_mul, _, [x;y]) -> (eval x) * (eval y)
  | Node(N_sub, _, [x;y]) -> (eval x) - (eval y)
  | Node(N_neg, _, [x]) -> - (eval x)
  | Node(N_number, [A_value,x], []) -> int_of_string x
  | _ -> invalid_arg "eval"
;;

let _ =
  let binary = Aurochs.load (`File "examples/arith.nog") in
  let program = Aurochs.program_of_binary binary in
  while true do
    let u = input_line stdin in
    try
      let x = Aurochs.parse program u in
      (*Peg.print_tree stdout t;*)
      let x = Peg.relativize u x in
      let x = eval x in
      Printf.printf ">>> %d\n%!" x
    with
    | x ->
        Printf.printf "Exception: %s\n%!" (Printexc.to_string x)
  done
;;
