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
  let module M = Arith in
  while true do
    let u = input_line stdin in
    try
      let t = M.parse u in
      M.print_tree stdout t;
      let x = eval t in
      Printf.printf ">>> %d\n%!" x
    with
    | x ->
        Printf.printf "Exception: %s\n%!" (Printexc.to_string x)
  done
;;
