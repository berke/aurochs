(* Aurochs *)

(** The main module for all your parsing needs! *)

(** This type alias helps distinguish binary programs from other strings. *)
type binary = string;;

(** This exception is raised on parse error.  The integer is the character position in the input. *)
exception Parse_error of int;;

(** Aurochs programs are represented using this abstract type.
    Actually, the program is malloc()'d and represented using C structures. *)
type ('node, 'attribute) program

(** Get a table giving the string representation of node constructors *)
val constructors : ('node, 'attribute) program -> string array

(** A generic program is a program whose node and label attributes are
    actually strings. *)
type generic_program

(** Unpack a binary string and create a program. *)
val program_of_binary : binary -> ('node, 'attribute) program

(** Unpack a string and create a generic program *)
val generic_program_of_binary : binary -> generic_program

(** Parse a given string *)
val parse : ('node, 'attribute) program -> string -> ('node, 'attribute) Peg.poly_positioned_tree

(** Parse a given string *)
val parse_generic : generic_program -> string -> Peg.tree

(** Exception raised when one tries to compile erroneous programs. *)
exception Compile_error of string;;

(** Interface to the compiler.
    Takes PEG source code and returns a program string. *)
val compile : string -> binary

(** Convenience function *)

type data = [`File of string | `String of string];;

val load : data -> string;;

val easy : program:[`Source of data|`Binary of data] -> text:data -> Peg.tree;;
