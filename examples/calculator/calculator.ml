(* Test_arith *)

open Aurochs_pack;;
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
  while true do
    let u = input_line stdin in
    try
      let t = Aurochs.read ~grammar:(`Program Arith.program) ~text:(`String u) in
      let x = eval t in
      Printf.printf ">>> %d\n%!" x;
      Util.with_binary_file_output "foo.bin" (fun oc ->
        Marshal.to_channel oc t []);
    with
    | x ->
        Printf.printf "Exception: %s\n%!" (Printexc.to_string x)
  done
;;
