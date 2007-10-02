(* Aurochs *)

(** The main module for all your parsing needs! *)

(** This type alias helps distinguish binary programs from other strings. *)
type binary = string;;

(** This exception is raised on parse error.  The integer is the character position in the input. *)
exception Parse_error of int;;

(** Aurochs programs are represented using this abstract type.
    Actually, the program is malloc()'d and represented using C structures. *)
type ('node, 'attribute) program

(** A generic program *)
type generic_program = (int, int) program

(** Get a table giving the string representation of node constructors *)
val constructors : ('node, 'attribute) program -> string array

(** Get a table giving the string representation of node attributes *)
val attributes : ('node, 'attribute) program -> string array

(** Unpack a binary string and create a program. *)
val program_of_binary : binary -> ('node, 'attribute) program

(** Parse a given string *)
val parse : ('node, 'attribute) program -> string -> ('node, 'attribute) Peg.poly_positioned_tree

(** Parse a given string *)
val parse_generic : generic_program -> string -> Peg.tree

(** Exception raised when one tries to compile erroneous programs. *)
exception Compile_error of string;;

(** Interface to the compiler.
    Takes PEG source code and returns a program string. *)
val compile : ?start:string -> ?base:string -> ?root:string -> ?check:bool -> string -> binary

(** Convenience function *)

type data = [`File of string | `String of string];;

val load : data -> string;;

val read :
   grammar:[`Source of data|`Program of ('n, 'a) program Lazy.t|`Binary of data] ->
   text:data ->
   ('n, 'a) Peg.poly_tree
;;

val see :
   grammar:[`Source of data|`Program of generic_program Lazy.t|`Binary of data] ->
   text:data ->
   Peg.tree
;;
