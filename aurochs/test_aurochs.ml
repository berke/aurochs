(* Test *)

open Pffpsf;;

let _ =
  let grammar = Sys.argv.(1)
  and file = Sys.argv.(2)
  in
  pf "Loading automaton from NOG file %s\n%!" grammar;
  let binary = Aurochs.load (`File grammar) in
  pf "Unpacking automata\n%!";
  let program = Aurochs.generic_program_of_binary binary in
  pf "Unpacked\n%!";
  let text = Aurochs.load (`File file) in
  let t = Aurochs.parse_generic program text in
  pf "Parsed:\n%!";
  pf "Tree:\n%a\n%!" (Peg.print_tree ()) t;
  match t with
  | Peg.Node _ -> pf "  Node\n%!";
  | Peg.Token _ -> pf "  Token\n%!"
;;
