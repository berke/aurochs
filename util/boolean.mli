(* Boolean *)

type 'a t =
  | False
  | True
  | Atom of 'a
  | And of 'a t list
  | Or of 'a t list
  | Not of 'a t

val eval : ('a -> bool) -> 'a t -> bool
