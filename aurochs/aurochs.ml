(* Aurochs *)

open Peg
open Util.Syntax
open Pffpsf

type binary = string
type ('node, 'attribute) program
type generic_program = (int, int) program

exception Parse_error of int
exception Error of string
exception Compile_error of exn

external get_choice_count : ('node, 'attribute) program -> int = "caml_aurochs_get_choice_count" "caml_aurochs_get_choice_count"
external get_production_count : ('node, 'attribute) program -> int = "caml_aurochs_get_production_count" "caml_aurochs_get_production_count"
external get_constructor_count : ('node, 'attribute) program -> int = "caml_aurochs_get_constructor_count" "caml_aurochs_get_constructor_count"
external get_constructor_name : ('node, 'attribute) program -> int -> string = "caml_aurochs_get_constructor_name" "caml_aurochs_get_constructor_name"
external get_attribute_count : ('node, 'attribute) program -> int = "caml_aurochs_get_attribute_count" "caml_aurochs_get_attribute_count"
external get_attribute_name : ('node, 'attribute) program -> int -> string = "caml_aurochs_get_attribute_name" "caml_aurochs_get_attribute_name"
external program_of_binary : binary -> ('node, 'attribute) program = "caml_aurochs_program_of_binary" "caml_aurochs_program_of_binary"
external parse_internal : ('node, 'attribute) program -> string -> int ref -> ('node, 'attribute) Peg.poly_positioned_tree option =
   "caml_aurochs_parse" "caml_aurochs_parse"

let constructors pg =
  let m = get_constructor_count pg in
  Array.init m (get_constructor_name pg)

let attributes pg =
  let m = get_attribute_count pg in
  Array.init m (get_attribute_name pg)

let parse pg u =
  let error_pos = ref 0 in
  match parse_internal pg u error_pos with
  | None -> raise (Parse_error !error_pos)
  | Some t -> t

let convert_tree pg t =
  let cons = constructors pg
  and attrs = attributes pg 
  in
  let rec convert = function
  | P_Node(a, b, n, al, xl) ->
      P_Node(a, b, cons.(n),
        List.map (fun (v, a) -> (v, attrs.(a))) al,
        List.map convert xl)
  | P_Token(i, j) as t -> t
  in
  convert t

let parse_generic pg u = relativize u (convert_tree pg (parse pg u))

let compile_inner ?start ?base ?root ?check u = failwith "Compile not implemented"

let compiler : (?start:string -> ?base:string -> ?root:string -> ?check:bool -> string -> binary) ref = ref compile_inner

let compile ?start ?base ?root ?check u =
  try
    !compiler ?start ?base ?root ?check u
  with
  | x -> raise (Compile_error x)

type data = [`File of string | `String of string]

let read_file fn =
  let ic = open_in fn in
  let m = in_channel_length ic in
  let u = String.create m in
  really_input ic u 0 m;
  close_in ic;
  u

let load = function
  | `String u -> u
  | `File fn -> read_file fn

let ( & ) f x = f x

let get_program = function
  | `Binary b -> program_of_binary (load b)
  | `Source s -> program_of_binary (compile (load s))
  | `Program p -> Lazy.force p

let see ~grammar ~text = parse_generic (get_program grammar) (load text)

let read_positioned ~grammar ~text =
  let u = load text in
  parse (get_program grammar) u

let read ~grammar ~text =
  let u = load text in
  Peg.relativize u (parse (get_program grammar) u)
