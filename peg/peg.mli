(* Peg *)

type char_set =
  | One of char
  | Many of char list
  | Range of char * char

type 'a pe =
  | Epsilon
  | Position
  | Tokenize of 'a pe
  | Ascribe of string * 'a pe
  | A of string (* Atom *)
  | C of char_set Boolean.t
  | N of 'a (* Nonterminal *)
  | S of 'a pe list (* Sequence *)
  | Build of string * 'a pe list
  | Or of 'a pe list (* Ordered alternative *)
  | And of 'a pe
  | Not of 'a pe
  | Opt of 'a pe (* Option *)
  | Star of 'a pe (* Star *)
  | Plus of 'a pe
  | BOF
  | EOF

type proto_tree =
  | Empty_
  | Node_ of string * proto_tree list
  | Attribute_ of string * string
  | Token_ of string
  | Pseudo_ of proto_tree list

type ('node, 'attribute) poly_positioned_tree =
  | P_Node of int * int * 'node * (int * int * 'attribute) list * ('node, 'attribute) poly_positioned_tree list (* Node (name, attributes_list, children *)
  | P_Token of int * int

type ('node, 'attribute) poly_tree =
  | Node of 'node * ('attribute * string) list * ('node, 'attribute) poly_tree list (* Node (name, attributes_list, children *)
  | Token of string

type tree = (string, string) poly_tree;;

type 'a result = Failure | Success of 'a | Busy

exception Fail
exception Not_implemented
exception Error of string

val relativize : string -> ('node, 'attribute) poly_positioned_tree -> ('node, 'attribute) poly_tree
val iter_over_n : ('a -> unit) -> 'a pe -> unit
val map_over_n : ('a -> 'b) -> 'a pe -> 'b pe
val iter_over_builds : (string -> unit) -> 'a pe -> unit
val iter_over_attributes : (string -> unit) -> 'a pe -> unit
val iter_over_poly_tree_nodes : ('node -> unit) -> ('node, 'attribute) poly_tree -> unit
val iter_over_poly_tree_attributes : ('attribute -> unit) -> ('node, 'attribute) poly_tree -> unit


val compute_active_terminals : (string * string pe) list -> Set.Make(String).t

val print_indent : out_channel -> int -> unit
val print_poly_tree :
  ?depth:int ->
  ?short:bool ->
  print_node:(out_channel -> 'node -> unit) ->
  print_attribute:(out_channel -> 'attribute -> unit) ->
  unit ->
  out_channel -> ('node, 'attribute) poly_tree -> unit
val print_poly_tree_list :
  ?depth:int ->
  ?short:bool ->
  print_node:(out_channel -> 'node -> unit) ->
  print_attribute:(out_channel -> 'attribute -> unit) ->
  unit ->
  out_channel -> ('node, 'attribute) poly_tree list -> unit
val print_tree :
  ?depth:int ->
  ?short:bool ->
  unit ->
  out_channel -> tree -> unit
val print_tree_list :
  ?depth:int ->
  ?short:bool ->
  unit ->
  out_channel -> tree list -> unit

val collect : proto_tree -> tree
val descent : ('a -> 'a pe) -> 'a pe -> string -> bool * int * tree
