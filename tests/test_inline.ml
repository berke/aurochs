(* Test_inline *)

open Util.Syntax;;

let foo () =
  let grammar = Sys.argv.(1) in
  let u = Sys.argv.(2) in
  let t = Aurochs.see ~grammar:(`Source(`String grammar)) ~text:(`String u) in
  Peg.print_tree () stdout t
;;

open Peg;;
open Pffpsf;;

let process fn =
  let pg = Aurochs.program_of_binary & Aurochs.compile "start ::= {[0-9]+} [ \\t]+ {[0-9]+} EOF;" in
  Util.with_file_input fn (fun ic -> Util.iter_over_lines ic (fun u ->
    match Aurochs.parse_generic pg u with
    | Node(_, _, [Token u; Token v]) -> pf "%S %S\n%!" u v
    | _ -> failwith "parse error"))
;;

let _ = process Sys.argv.(1);;
