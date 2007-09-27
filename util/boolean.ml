(* Boolean *)

type 'a t =
  | False
  | True
  | Atom of 'a
  | And of 'a t list
  | Or of 'a t list
  | Not of 'a t
;;

let rec eval f = function
  | False -> false
  | True -> true
  | Atom a -> f a
  | And l -> List.for_all (eval f) l
  | Or l -> List.exists (eval f) l
  | Not x -> not (eval f x)
;;
