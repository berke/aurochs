(* Util *)
(* Copyright (C)2004-2006 Berke Durak                               *)
(* Released under the GNU Lesser General Public License version 2.1 *)

let sf = Printf.sprintf

exception At of string * exn

let first_line =
  let b = Buffer.create 256 in
  fun w ->
    Buffer.clear b;
    let rec loop i =
      if i = String.length w || w.[i] = '\n' then
        Buffer.contents b
      else
        begin
          Buffer.add_char b w.[i];
          loop (i + 1)
        end
    in
    loop 0

let limit m w =
  let n = String.length w in
  if n <= m then
    w
  else
    if m < 3 then
      String.make m '.'
    else
      (String.sub w 0 (min (m - 3) n))^"..."

let limit_left m w =
  let n = String.length w in
  if n <= m then
    w
  else
    if m < 3 then
      String.make m '.'
    else
      let m' = min (m - 3) n in
      "..."^(String.sub w (m - m') m')

let for_all_chars f w =
  let m = String.length w in
  let rec loop i =
    if i = m then
      true
    else
      f w.[i] && loop (i + 1)
  in
  loop 0

let split_once_at f s =
  let m = String.length s in
  let rec loop1 i =
    if i = m then
      raise Not_found
    else
      if f s.[i] then
        loop2 i (i + 1)
      else
        loop1 (i + 1)
  and loop2 i j =
    if j = m || not (f s.[j]) then
      (i,j)
    else
      loop2 i (j + 1)
  in
  try
    let (i,j) = loop1 0 in
    (String.sub s 0 i,
     String.sub s j (m - j))
  with
  | Not_found -> (s, "")

let is_alpha = function
	| 'a'..'z' -> true
	| 'A'..'Z' -> true
	| _ -> false

let is_digit = function
  | '0'..'9' -> true
  | _ -> false

let is_space = function
  | ' '|'\t'|'\n' -> true
  | _ -> false

let parse_strings u =
  let m = String.length u in
  let b = Buffer.create m in
  let rec loop0 r i =
    if i >= m then
      List.rev r
    else
      match u.[i] with
      | ' '|'\t'|'\n' -> loop0 r (i + 1)
      | '"' ->
          Buffer.clear b;
          loop2 r (i + 1)
      | _ -> loop1 r i
  and loop1 r i =
    if i = m || is_space u.[i] || u.[i] = '"' then
      begin
        let x = Buffer.contents b in
        Buffer.clear b;
        loop0 (x::r) i
      end
    else
      begin
        Buffer.add_char b u.[i];
        loop1 r (i + 1)
      end
  and loop2 r i =
    if i = m then
      invalid_arg "Unterminated double quote"
    else
      if u.[i] = '"' then
        begin
          let x = Buffer.contents b in
          Buffer.clear b;
          loop0 (x::r) (i + 1)
        end
      else
        if u.[i] = '\\' then
          if i + 1 < m then
            match u.[i + 1] with
            | '\\' -> Buffer.add_char b '\\'; loop2 r (i + 2)
            | 'n' -> Buffer.add_char b '\n'; loop2 r (i + 2)
            | 'r' -> Buffer.add_char b 'r'; loop2 r (i + 2)
            | '"' -> Buffer.add_char b '"'; loop2 r (i + 2)
            | 't' -> Buffer.add_char b 't'; loop2 r (i + 2)
            | '0'..'9' ->
                if i + 3 < m then
                  let x = int_of_string (String.sub u (i + 1) 3) in
                  if 0 < x && x < 256 then
                    begin
                      Buffer.add_char b (Char.chr x);
                      loop2 r (i + 4)
                    end
                  else
                    invalid_arg "Bad or null character code in backslash code"
                else
                  invalid_arg "Unterminated decimal backslash code"
            | _ -> invalid_arg "Unknown backslash code"
          else
            invalid_arg "Unterminated backslash sequence"
        else
          begin
            Buffer.add_char b u.[i];
            loop2 r (i + 1)
          end
  in
  loop0 [] 0

let split_at c u =
  let m = String.length u in
  let b = Buffer.create m in
  let rec loop0 r i =
    if i >= m then
      List.rev r
    else
      if u.[i] = c then
        loop0 r (i + 1)
      else
        loop1 r i
  and loop1 r i =
    if i = m || u.[i] = c then
      begin
        let x = Buffer.contents b in
        Buffer.clear b;
        loop0 (x::r) (i + 1)
      end
    else
      begin
        Buffer.add_char b u.[i];
        loop1 r (i + 1)
      end
  in
  loop0 [] 0

let list_intersect l1 l2 =
  let rec loop r = function
    | [] -> r
    | x::y -> loop (if List.mem x l2 then x::r else r) y
  in
  loop [] l1

let once f =
  let x = ref true in
  fun () ->
    if !x then
      begin
        x := false;
        f ()
      end
    else
      ()

let list_has_more_than_one_element = function
  | []|[_] -> false
  | _ -> true

let count_lines w =
  let m = String.length w in
  let rec loop x i =
    if i = m then
      x
    else
      loop (if w.[i] = '\n' then x + 1 else x) (i + 1)
  in
  loop 1 0

let first_matching_char_from i f w =
  let m = String.length w in
  let rec loop i =
    if i = m then
      raise Not_found
    else
      if f w.[i] then
        i
      else
        loop (i + 1)
  in
  loop i

let first_matching_char = first_matching_char_from 0
let longest_matching_prefix f w =
  try
    let i = first_matching_char (fun c -> not (f c)) w in
    String.sub w 0 i, String.sub w i (String.length w - i)
  with
  | Not_found -> (w,"")

let remove_leading_spaces w =
  try
    let i = first_matching_char (fun c -> not (is_space c)) w in
    String.sub w i (String.length w - i)
  with
  | Not_found -> w

let delete_first_chars n w =
  let m = String.length w in
  if m > n then
    String.sub w n (m - n)
  else
    ""

let hierarchical x y =
  let m = String.length x
  and n = String.length y
  in
  if m < n then
    -1
  else if m > n then
    1
  else
    compare x y

let wind f x g y =
  begin
    try
      let r = f x in
      g y;
      r
    with
    | z ->
        g y;
        raise z
  end

let rec list_change_nth l n z =
  match l,n with
  | [],_ -> raise Not_found
  | x::y,0 -> z::y
  | x::y,_ -> x::(list_change_nth y (n - 1) z)

let rec list_remove_nth l n =
  match l,n with
  | [],_ -> raise Not_found
  | x::y,0 -> y
  | x::y,_ -> x::(list_remove_nth y (n - 1))

let word_wrap oc ?(columns=75) u =
  let m = String.length u in
  let f c = output_char oc c
  and g u i m = output oc u i m
  in
  (* beginning of line space *)
  (* i: current index *)
  (* j: pending beginning-of-line spaces (i.e., indent) *)
  let rec loop0 i j =
    if i = m then
      if j > 0 then
        f '\n'
      else
        ()
    else match u.[i] with
    | ' ' -> loop0 (i + 1) (j + 1)
    | '\t' -> loop0 (i + 1) (j + (4 - j land 3))
    | '\n' ->
        f '\n';
        loop0 (i + 1) 0
    | _ ->
        if j < columns then
          loop2 i i 0 j
        else
          begin
            f '\n';
            loop2 i i 0 0
          end
  (* inter-word space *)
  (* i: current index *)
  (* j: actual column *)
  and loop1 i j =
    if i = m then
      if j > 0 then
        f '\n'
      else
        ()
    else match u.[i] with
    | ' '|'\t' -> loop1 (i + 1) j
    | '\n' ->
        f '\n';
        loop0 (i + 1) 0
    | _ -> loop2 i i j 1
  (* word *)
  (* i0: index of beginning of word *)
  (* i: current index *)
  (* j: actual cursor column *)
  (* k: number of pending spaces *)
  and loop2 i0 i j k =
    if i = m || u.[i] = ' ' || u.[i] = '\t' || u.[i] = '\n' then
      let l = i - i0 in
      if j + k + l >= columns then
        begin
          f '\n';
          g u i0 l;
          if i < m && u.[i] = '\n' then
            begin
              f '\n';
              loop0 (i + 1) 0
            end
          else
            if l >= columns then
              begin
                f '\n';
                loop1 (i + 1) 0
              end
            else
              loop1 (i + 1) l
        end
      else
        begin
          for h = 1 to k do
            f ' '
          done;
          g u i0 l;
          if u.[i] = '\n' then
            begin
              f '\n';
              loop0 (i + 1) 0
            end
          else
            loop1 (i + 1) (j + k + l)
        end
    else
      loop2 i0 (i + 1) j k
  in
  loop0 0 0

let reg_of_string w =
  let m = String.length w in
  let b = Buffer.create m in
  for i = 0 to m - 1 do
    match w.[i] with
    | ('.'|'+'|'?'|'['|']'|'^'|'$'|'('|')'|'{'|'}'|'\\') as c -> Buffer.add_char b '\\'; Buffer.add_char b c
    | '*' -> Buffer.add_string b ".*"
    | c -> Buffer.add_char b c
  done;
  Buffer.contents b

let flip_array a =
  let m = Array.length a in
  for i = 0 to m / 2 - 1 do
    let t = a.(i) in
    a.(i) <- a.(m - 1 - i);
    a.(m - 1 - i) <- t
  done

let substitute_variables env w =
  let b = Buffer.create (String.length w) in
  Buffer.add_substitute b (fun v -> List.assoc v env) w;
  Buffer.contents b

let list_sub_rev l start length =
  let rec loop r j = function
    | [] -> r (* shall we raise an exception ? *)
    | x::y -> loop (if j < start || j >= start + length then r else x::r) (j + 1) y
  in
  loop [] 0 l

let is_prefix u v =
  let m = String.length u
  and n = String.length v
  in
  m <= n &&
    let rec loop i = i = m || u.[i] = v.[i] && loop (i + 1) in
    loop 0

let remove_prefix u v =
  if is_prefix u v then
    let m = String.length u in
    String.sub v m (String.length v - m)
  else
    v

let is_suffix u v =
  let m = String.length u
  and n = String.length v
  in
  m <= n &&
    let rec loop i = i = m || u.[m - 1 - i] = v.[n - 1 - i] && loop (i + 1) in
    loop 0

let remove_suffix u v =
  if is_suffix u v then
    let m = String.length u in
    String.sub v 0 (String.length v - m)
  else
    v

let lowercase_compare u v =
  let m = String.length u
  and n = String.length v
  in
  if m <> n then
    false
  else
    let rec loop i = i = m || (Char.lowercase u.[i] = Char.lowercase v.[i] && loop (i + 1)) in
    loop 0

let call_if f x =
  match f with
  | None -> ()
  | Some g -> g x

let optional f x =
  match x with
  | None -> ()
  | Some y -> f y

let mandatory = function
  | None -> raise Not_found
  | Some x -> x

let wrap x g f =
  begin
    try
      let r = f x in
      g x;
      r
    with
    | z ->
        g x;
        raise z
  end

let binary_search compare a x =
  let m = Array.length a in
  let rec loop i0 m =
    if m = 0 then
      raise Not_found
    else
      begin
        if m < 8 then
          if compare a.(i0) x = 0 then
            i0
          else
            loop (i0 + 1) (m - 1)
        else
          let i = i0 + m / 2 in
          let y = a.(i) in
          let c = compare x y in
          if c = 0 then
            i
          else
            if c < 0 then
              loop i0 (m / 2)
            else
              loop (i + 1) (m - m / 2 - 1)
      end
  in
  loop 0 m

let randomize a =
  let m = Array.length a in
  let swap i j =
    let x = a.(i) in
    a.(i) <- a.(j);
    a.(j) <- x
  in
  for i = 0 to m - 2 do
    let j = i + 1 + Random.int (m - i - 1) in
    swap i j
  done

let array_mem_assoc x a =
  let m = Array.length a in
  let rec loop i =
    if i = m then false
    else
      let (y,_) = a.(i) in
      x = y || loop (i + 1)
  in
  loop 0

let array_assoc x a =
  let m = Array.length a in
  let rec loop i =
    if i = m then raise Not_found
    else
      let (y,z) = a.(i) in
      if x = y then
        z
      else
        loop (i + 1)
  in
  loop 0

let inside msg f x =
  try
    f x
  with
  | x -> raise (At(msg, x))

let rec display_exception = function
| At(location, x) ->
    Printf.eprintf "  At %s:\n" location;
    display_exception x
| x -> Printf.eprintf "  %s.\n" (Printexc.to_string x)

let catch f =
  try
    f ()
  with
  | x ->
      Printf.eprintf "Caught exception:\n";
      display_exception x;
      Printf.eprintf "%!";
      exit 1

let sanitize_filename
  ?(is_safe=
    (function
    | ('a'..'z'|'A'..'Z'|'0'..'9'|'-'|'_') -> true
    | _ -> false))
  ?(buffer=Buffer.create 256)
  ?(prefix="") fn =
  let b = buffer in
  Buffer.clear b;
  let m = String.length fn in
  Buffer.add_string b prefix;
  for i = 0 to m - 1 do
    let c = fn.[i] in
    if is_safe c then
      Buffer.add_char b c
    else
      Printf.bprintf b "%%%02x" (Char.code c)
  done;
  Buffer.contents b

let unsanitize_filename ?(buffer=Buffer.create 256) ?(prefix="") fn =
  if not (is_prefix prefix fn) then invalid_arg "unsanitize_filename: no prefix";
  let b = buffer in
  Buffer.clear b;
  let m = String.length fn in
  let rec loop i =
    if i = m then
      Buffer.contents b
    else
      begin
        match fn.[i] with
        | ('a'..'z'|'A'..'Z'|'0'..'9'|'-'|'_') as c ->
           Buffer.add_char b c;
           loop (i + 1)
        | '%' ->
            if i + 2 >= m then invalid_arg (sf "unsanitize_filename: short hex escape at %d" i);
            let f c =
              let x = Char.code c in
              match c with
              | 'a'..'f' -> x - 87
              | 'A'..'F' -> x - 55
              | '0'..'9' -> x - 48
              | _ -> invalid_arg (sf "unsanitize_filename: bad hex char %C" c)
            in
            let x = ((f fn.[i+1]) lsl 4) lor (f fn.[i+2]) in
            Buffer.add_char b (Char.chr x);
            loop (i + 3)
        | c -> invalid_arg (sf "unsanitize_filename: bad char %C" c)
      end
  in
  loop (String.length prefix)

let with_file_input fn f =
  let ic = open_in fn in
  try
    f ic
  with
  | x ->
      close_in ic;
      raise x

let with_file_output fn f =
  let oc = open_out fn in
  try
    let y = f oc in
    close_out oc;
    y
  with
  | x ->
      close_out oc;
      raise x

let with_binary_file_output fn f =
  let oc = open_out_bin fn in
  try
    let y = f oc in
    close_out oc;
    y
  with
  | x ->
      close_out oc;
      raise x

let with_binary_file_input fn f =
  let ic = open_in_bin fn in
  try
    f ic
  with
  | x ->
      close_in ic;
      raise x

let save fn x = with_binary_file_output fn (fun oc -> Marshal.to_channel oc x [])
let load fn = with_binary_file_input fn (fun ic -> Marshal.from_channel ic)
let iter_over_lines ic f =
  try
    while true do
      let u = input_line ic in
      f u
    done;
    assert false
  with
  | End_of_file -> ()

let unsigned_int64_of_decimal u =
  let m = String.length u in
  let rec loop q i =
    if i = m then
      q
    else
      let x = (Char.code u.[i]) land 15 in
      loop (Int64.add (Int64.mul q 10L) (Int64.of_int x)) (i + 1)
  in
  loop 0L 0

module Syntax =
  struct
    include Pffpsf

    let (&) f x = f x (* From Nicolas Pouillard *)
    let ( += ) l x = l := x :: !l
    let ( |> ) x f = f x
    let ( <? ) x f = f; x
  end

let read_file fn =
  let ic = open_in fn in
  let m = in_channel_length ic in
  let u = String.create m in
  really_input ic u 0 m;
  close_in ic;
  u
