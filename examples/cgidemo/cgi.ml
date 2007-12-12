(* Cgi *)

let sf = Printf.sprintf

let hex c =
  match c with
  | '0' .. '9' -> (Char.code c) - (Char.code '0')
  | 'a' .. 'f' -> (Char.code c) - (Char.code 'a') + 10
  | 'A' .. 'F' -> (Char.code c) - (Char.code 'A') + 10
  | _ -> invalid_arg "Non-hexadecimal character"

let read_hex_char =
  parser
    | [< 'c1; 'c2 >] -> (Char.chr (16 * (hex c1) + (hex c2)))

type separator =
  | EOS
  | Equal
  | Ampersand

let rec read_part s =
  let b = Buffer.create 16 in
  let rec loop =
  parser
    | [< 'c; s >] ->
	begin
	  match c with
	  | '%' ->
	      Buffer.add_char b (read_hex_char s);
	      loop s
	  | '+' ->
	      Buffer.add_char b ' ';
	      loop s
	  | '=' -> (Buffer.contents b, Equal)
	  | '&' -> (Buffer.contents b, Ampersand)
	  | _ ->
	      Buffer.add_char b c;
	      loop s
	end
    | [< >] ->
	(Buffer.contents b, EOS)
  in
  loop s

module StringMap = Map.Make (struct type t = string let compare = compare end)
module StringSet = Set.Make (struct type t = string let compare = compare end)

let parse_form_from_stream t =
  let add n v f =
    if StringMap.mem n f then
      let x = StringMap.find n f in
      StringMap.add n (StringSet.add v x) f
    else
      StringMap.add n (StringSet.singleton v) f
  in
  let rec loop f =
    let (name,x) = read_part t in
    match x with
    |	EOS ->
        failwith (sf "Bad form data: end of stream in name %S" name)
    |	Ampersand ->
        failwith (sf "Bad form data: ampersand in name %S" name)
    |	Equal -> 
	begin
	  if String.length name = 0 then
            failwith (sf "Bad form data: empty name");
	  let (value,x) = read_part t in
	  let f = add name value f in
	  match x with
	  | EOS -> f
	  | Ampersand -> loop f
	  | Equal ->
	      failwith "Bad form: '=' in value"
	end
  in
  loop StringMap.empty

let parse_form_from_string s =
  let t = Stream.of_string s in
  parse_form_from_stream t

let print_stringmapstring f =
  StringMap.iter (fun k d -> Printf.printf "\"%s\" -> \"%s\"\n" k d) f

let print_stringsetstringmap f =
  StringMap.iter (fun k d ->
    Printf.printf "\"%s\" -> {" k;
    StringSet.iter (fun s -> Printf.printf " \"%s\"" s) d;
    Printf.printf " }\n") f

let encode_string b x =
  let hex = "0123456789ABCDEF" in
  for i = 0 to String.length x - 1 do
    let c = x.[i] in
    let d = Char.code c in
    if c = ' ' then Buffer.add_char b '+'
    else if d < 32 or d > 126 or c = '&' or c = '=' or c = '"' or c = '%' then
      begin
	Buffer.add_char b '%';
	Buffer.add_char b hex.[d lsr 4];
	Buffer.add_char b hex.[d land 15];
      end
    else Buffer.add_char b c
  done

let encode_form f =
  let b = Buffer.create 16 in
  StringMap.iter
    (fun n s ->
      (StringSet.iter
	 (fun v ->
	   if Buffer.length b > 0 then Buffer.add_char b '&';
	   encode_string b n;
	   Buffer.add_char b '=';
	   encode_string b v) s)) f;
  Buffer.contents b

(* Functions processing environment vars *)

type invocation = POST | GET

let invocation_method () =
  try
    match Sys.getenv ("REQUEST_METHOD") with
    | "POST" -> POST
    | "GET" -> GET
    | m -> invalid_arg (sf "Unknown invocation method %s" m)
  with
    Not_found -> GET (* Default value *)

let remote_host =
  let g x =
    try
      Sys.getenv x
    with
      Not_found -> ""
  and f x y = match (x,y) with
    "",_ -> y
  | _,"" -> x
  | _,_ -> x^","^y
  in
  f (g "REMOTE_HOST")
    (f (g "REMOTE_USER")
       (g "REMOTE_ADDR"))
