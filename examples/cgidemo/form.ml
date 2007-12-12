(* Form *)
(* Copyright (C)2000-2006 Berke Durak                               *)
(* Released under the GNU Lesser General Public License version 2.1 *)

exception Duplicate

let sf = Printf.sprintf

let hexadecimal c =
  match c with
  | '0' .. '9' -> (Char.code c) - (Char.code '0')
  | 'a' .. 'f' -> (Char.code c) - (Char.code 'a') + 10
  | 'A' .. 'F' -> (Char.code c) - (Char.code 'A') + 10
  | _ -> raise (Invalid_argument (sf "Character %C is not hexadecimal" c))

let read_hex_encoded_char =
  parser
    | [< 'c1; 'c2 >] -> (Char.chr (16 * (hexadecimal c1) + (hexadecimal c2)))

let rec read_chunk s =
  let b = Buffer.create 16 in
  let rec loop =
  parser
    | [< 'c; s >] ->
	begin
	  match c with
	  | '%' ->
	      Buffer.add_char b (read_hex_encoded_char s);
	      loop s
	  | '+' ->
	      Buffer.add_char b ' ';
	      loop s
	  | '=' -> (Buffer.contents b, `Equal)
	  | '&' -> (Buffer.contents b, `Ampersand)
	  | _ ->
	      Buffer.add_char b c;
	      loop s
	end
    | [< >] ->
	(Buffer.contents b, `EOS)
  in
  loop s

module SM = Map.Make (struct type t = string let compare = compare end)

module SS = Set.Make (struct type t = string let compare = compare end)

type t = SS.t SM.t

let add f n v =
  if SM.mem n f then
    let x = SM.find n f in
    SM.add n (SS.add v x) f
  else
    SM.add n (SS.singleton v) f

let parse_form_from_stream t =
  let rec loop f =
    let (name,x) = read_chunk t in
    match x with
    |	`EOS ->
        if name <> "" then
          raise (Invalid_argument (sf "Bad form: EOS in name %S" name))
        else
          f
    |	`Ampersand ->
	raise (Invalid_argument (sf "Bad form: ampersand in name %S" name))
    |	`Equal -> 
	begin
	  if String.length name = 0 then
	    raise (Invalid_argument (sf "Bad form: empty name"));
	  let (value,x) = read_chunk t in
	  let f = add f name value in
	  match x with
	  | `EOS -> f
	  | `Ampersand -> loop f
	  | `Equal ->
	      raise (Invalid_argument (sf "Bad form: '=' in value"))
	end
  in
  loop SM.empty

let parse_form_from_string s =
  let t = Stream.of_string s in
  parse_form_from_stream t

let display_stringmapstring f =
  SM.iter (fun k d -> Printf.printf "\"%s\" -> \"%s\"\n" k d) f

let display_form f =
  SM.iter (fun k d ->
    Printf.printf "\"%s\" -> {" k;
    SS.iter (fun s -> Printf.printf " \"%s\"" s) d;
    Printf.printf " }\n") f

let encode_string b x =
  let hex = "0123456789ABCDEF" in
  for i = 0 to String.length x - 1 do
    let c = x.[i] in
    let d = Char.code c in
    if c = ' ' then Buffer.add_char b '+'
    else
      if d < 32 or d > 126 or
        match c with
        | 'a'..'z'|'A'..'Z'|'0'..'9'|'*'|'_'|'.' -> false
        | _ -> true
      then
        begin
          Buffer.add_char b '%';
          Buffer.add_char b hex.[d lsr 4];
          Buffer.add_char b hex.[d land 15];
        end
      else Buffer.add_char b c
  done

let encode_form f =
  let b = Buffer.create 16 in
  SM.iter
    (fun n s ->
      (SS.iter
	 (fun v ->
	   if Buffer.length b > 0 then Buffer.add_char b '&';
	   encode_string b n;
	   Buffer.add_char b '=';
	   encode_string b v) s)) f;
  Buffer.contents b

let encode_form_from_list ?buffer f =
  let b =
    match buffer with
    | None -> Buffer.create 16
    | Some b -> b
 in
 let pristine = ref true in
 List.iter
   (fun (n,s) ->
     (List.iter
        (fun v ->
          if !pristine then pristine := false else Buffer.add_char b '&';
          encode_string b n;
          Buffer.add_char b '=';
          encode_string b v) s)) f;
 Buffer.contents b

let get_list f k = SS.elements (SM.find k f)

let get_set f k = SM.find k f

let get f ?default k =
  let none () =
    match default with
    | None -> raise Not_found
    | Some w -> w
  in
  try
    match get_list f k with
    | [] -> none ()
    | [x] -> x
    | _::_ -> raise Duplicate
  with
  | Not_found -> none ()

let get_value f ?default g k =
  let none () =
    match default with
    | None -> raise Not_found
    | Some y -> y
  in
  try
    match get_list f k with
    | [] -> none ()
    | [x] -> g x
    | _::_ -> raise Duplicate
  with
  | Not_found -> none ()

let empty = SM.empty

let to_string (x : string) = x

let some x = Some x
