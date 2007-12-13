(* Test_arith *)

open Aurochs_pack
open Peg

let _ =
  let grammar ="
    int ::= <num>val:[0-9]+</num>;
    outfixing [ \\n\\t]* do
      start ::= sum EOF;
      sum ::= <add> term '+' sum </add> | term;
      term ::= <mul> simple '*' term </mul> | simple;
      simple ::= int | '(' sum ')';
    done;"
  in
  let rec eval = function
    | Node("root", _, [x]) -> eval x
    | Node("add", _, [x;y]) -> (eval x) + (eval y)
    | Node("mul", _, [x;y]) -> (eval x) * (eval y)
    | Node("sub", _, [x;y]) -> (eval x) - (eval y)
    | Node("neg", _, [x]) -> - (eval x)
    | Node("num", ["val",x], []) -> int_of_string x
    | _ -> assert false
  in
  while true do
    Printf.printf "> %!";
    let u = input_line stdin in
    try
      let t = Aurochs.see ~grammar:(`Source(`String grammar)) ~text:(`String u) in
      let x = eval t in
      Printf.printf "%d\n%!" x;
    with
    | x ->
        Printf.printf "Exception: %s\n%!" (Printexc.to_string x)
  done
