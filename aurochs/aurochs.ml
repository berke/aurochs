(* Aurochs *)

type binary = string
type ('node, 'attribute) program
type generic_program = (string, string) program

exception Parse_error of int;;
exception Compile_error of string;;

external program_of_binary : binary -> ('node, 'attribute) program = "caml_aurochs_program_of_binary" "caml_aurochs_program_of_binary"
external generic_program_of_binary : binary -> generic_program = "caml_aurochs_program_of_binary" "caml_aurochs_program_of_binary"
external parse : ('node, 'attribute) program -> string -> ('node, 'attribute) Peg.poly_positioned_tree = "caml_aurochs_parse" "caml_aurochs_parse"
external parse_generic : generic_program -> string -> Peg.tree = "caml_aurochs_parse_generic" "caml_aurochs_parse_generic"

let compile u = failwith "Not implemented";;

type data = [`File of string | `String of string];;

let read_file fn =
  let ic = open_in fn in
  let m = in_channel_length ic in
  let u = String.create m in
  really_input ic u 0 m;
  close_in ic;
  u
;;

let load = function
  | `String u -> u
  | `File fn -> read_file fn
;;

let ( & ) f x = f x;;

let easy ~program ~text =
  let binary = load program in
  let program = generic_program_of_binary binary in
  parse_generic program (load text)
;;
