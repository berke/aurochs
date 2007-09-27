(*** Seq *)
type 'a t =
  | It of 'a
  | Lst of 'a list
  | Seq of 'a t list
;;

let empty = Seq[];;

let ( !! ) a = It a;;

let ( ^^^ ) a s = match s with
  | It _ as a' -> Seq[It a; a']
  | Seq l -> Seq((It a) :: l)
  | Lst l -> Lst(a :: l)
;;

let ( ** ) s1 s2 =
  match (s1,s2) with
  | (It a, _) -> a ^^^ s1
  | (_, _) -> Seq[s1;s2]
;;

let rec iter f = function
  | It a -> f a
  | Seq l -> List.iter (iter f) l
  | Lst l -> List.iter f l
;;

let flatten s =
  let result = ref [] in
  iter begin fun x -> result := x :: !result end s;
  List.rev !result
;;
