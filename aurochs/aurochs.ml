(* Aurochs *)

type binary = string
type ('node, 'attribute) program
type generic_program = (string, string) program

exception Parse_error of int;;
exception Compile_error of string;;

external get_constructor_count : ('node, 'attribute) program -> int = "caml_aurochs_get_constructor_count" "caml_aurochs_get_constructor_count"
external get_constructor_name : ('node, 'attribute) program -> int -> string = "caml_aurochs_get_constructor_name" "caml_aurochs_get_constructor_name"
external program_of_binary : binary -> ('node, 'attribute) program = "caml_aurochs_program_of_binary" "caml_aurochs_program_of_binary"
external generic_program_of_binary : binary -> generic_program = "caml_aurochs_program_of_binary" "caml_aurochs_program_of_binary"
external parse_internal : ('node, 'attribute) program -> string -> int ref -> ('node, 'attribute) Peg.poly_positioned_tree option =
   "caml_aurochs_parse" "caml_aurochs_parse"

let constructors pg =
  let m = get_constructor_count pg in
  Array.init m (get_constructor_name pg)
;;

let parse pg u =
  let error_pos = ref 0 in
  match parse_internal pg u error_pos with
  | None -> raise (Parse_error !error_pos)
  | Some t -> t
;;

let parse_generic gp u =
  let _t = parse gp u in
  failwith "Not implemented"
;;

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
  let binary =
    match program with
    | `Binary b -> load b
    | `Source s -> compile (load s)
  in
  let program = generic_program_of_binary binary in
  parse_generic program (load text)
;;
