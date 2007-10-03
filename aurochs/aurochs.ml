(* Aurochs *)

open Peg;;
open Util.Syntax;;
open Pffpsf;;

type binary = string
type ('node, 'attribute) program
type generic_program = (int, int) program

exception Parse_error of int;;
exception Compile_error of string;;

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
;;

let attributes pg =
  let m = get_attribute_count pg in
  Array.init m (get_attribute_name pg)
;;

let parse pg u =
  let error_pos = ref 0 in
  match parse_internal pg u error_pos with
  | None -> raise (Parse_error !error_pos)
  | Some t -> t
;;

let parse_generic pg u =
  let t = parse pg u in
  let cons = constructors pg
  and attrs = attributes pg 
  in
  let sub u i j = String.sub u i (j - i) in
  let rec convert = function
  | P_Node(_, _, n, al, xl) ->
      Node(cons.(n),
        List.map (fun (i, j, a) -> (attrs.(a), if j < i then string_of_int i else sub u i j)) al,
        List.map convert xl)
  | P_Token(i, j) -> Token(sub u i j)
  in
  convert t
;;

exception Compile_error of string

(*** compile *)
let compile ?(start="start") ?(base="") ?(root="root") ?(check=true) u =
  let peg = Convert_grammar.convert_grammar & Grammar.parse u in
  if check then
    begin
      let results = Check.check_grammar start peg in
      List.iter
        begin function
          | `Warning _|`Info _ -> ()
          | `Error u   -> raise(Compile_error u)
        end
        results;
    end;
  let peg_canonified = Canonify.canonify_grammar ~start peg in
  let pg = Noggie.generate_code ~start ~root peg_canonified in
  Bytes.with_buffer_sink (Noggie.put_program pg peg)
;;
(* ***)

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

let get_program = function
  | `Binary b -> program_of_binary (load b)
  | `Source s -> program_of_binary (compile (load s))
  | `Program p -> Lazy.force p
;;

let see ~grammar ~text = parse_generic (get_program grammar) (load text);;

let read_positioned ~grammar ~text =
  let u = load text in
  parse (get_program grammar) u
;;

let read ~grammar ~text =
  let u = load text in
  Peg.relativize u (parse (get_program grammar) u)
;;
