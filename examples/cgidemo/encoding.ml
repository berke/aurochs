(* Encoding *)
(* Copyright (C)2000-2006 Berke Durak                               *)
(* Released under the GNU Lesser General Public License version 2.1 *)

(* XXX : handle star *)
(* XXX : Sort results *)

exception Error of string;;

type spec = (string option * float) list;;

let collect s =
  let b = Buffer.create 16 in
  let rec loop = parser
    | [< 'c; s >] ->
        Buffer.add_char b c;
        loop s
    | [< >] -> Buffer.contents b
  in
  loop s
;;

let rec space_stripper = parser
  | [< ' (' '|'\t'); s >] -> space_stripper s
  | [< 'c; s >] -> [< 'c; space_stripper s >]
  | [< >] -> [< >]
;;

let quality spec encoding =
  try
    List.assoc (Some encoding) spec
  with
  | Not_found ->
      try
        List.assoc None spec
      with
      | Not_found -> 0.0 (* By default *)
;;

let accepted_encodings u =
  let b = Buffer.create 16 in
  let add = Buffer.add_char b in
  let res : (string option * float ref) list ref = ref [] in
  let get_name () =
    let name = Buffer.contents b in
    Buffer.clear b;
    Some name
  in
  let emit name q =
    try
      let q' = List.assoc name !res in
      if q > !q' then
        q' := min 1.0 (max 0.0 q)
      else
        ()
    with
    | Not_found ->
      res := (name, ref (min 1.0 (max 0.0 q))) :: !res
  in
  let rec loop0 comma star = parser
  | [< '(('a'..'z'|'A'..'Z'|'0'..'9'|'-') as c); s >] ->
      if star then
        raise (Error "Expecting something else after star")
      else
        begin
          add c;
          loop0 false false s
        end
  | [< ''*'; s >] ->
      loop0 false true s
  | [< '','; s >] ->
      emit (if star then None else get_name ()) 1.0;
      loop0 true false s
  | [< '';'; ''q'; ''='; s >] ->
      let name = if star then None else get_name () in
      loop1 false name s
  | [< >] ->
      if comma then
        raise (Error "Expecting encoding after comma")
      else
        if Buffer.length b > 0 then
          emit (if star then None else get_name ()) 1.0
        else
          ()
  and loop1 decimal name = parser
  | [< ' ('0'..'9' as c); s >] ->
      add c;
      loop1 decimal name s
  | [< ''.'; s >] ->
      if decimal then raise (Error "Bad floating-point number");
      add '.';
      loop1 true name s
  | [< '','; s >] ->
    let q = Buffer.contents b in
    let q = float_of_string q in
    Buffer.clear b;
    emit name q;
    loop0 true false s
  | [< '_; _ >] -> raise (Error ("Not expecting this after floating-point number"))
  | [< >] ->
    let q = Buffer.contents b in
    let q = float_of_string q in
    Buffer.clear b;
    emit name q
  in
  let s = Stream.of_string u in
  loop0 false false (space_stripper s);
  List.map (fun (name, q) -> (name, !q)) !res
;;
