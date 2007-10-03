(* Convert_grammar *)

open Peg;;
open Pffpsf;;
open Talk;;

module B = Boolean;;
open Grammar;;

exception Error of string;;
exception Internal_error;;
exception Inside of string * exn;;

(*** explode_string *)
let explode_string u =
  let m = String.length u in
  let rec loop i result =
    if i = 0 then
      result
    else
      loop (i - 1) (u.[i - 1] :: result)
  in
  loop m []
;;
(* ***)
(*** unescape_string *)
let unescape_string ?(quotes=['\'']) u =
  let m = String.length u in
  let b = Buffer.create m in
  let rec loop0 i =
    if i = m then
      Buffer.contents b
    else
      begin
        match u.[i] with
        | '\\' -> loop1 (i + 1)
        | c -> Buffer.add_char b c; loop0 (i + 1)
      end
  and loop1 i =
    if i = m then
      invalid_arg "Unterminated escape string"
    else
      match u.[i] with
      | 'n' -> Buffer.add_char b '\n'; loop0 (i + 1)
      | 'r' -> Buffer.add_char b '\r'; loop0 (i + 1)
      | 't' -> Buffer.add_char b '\t'; loop0 (i + 1)
      | 'b' -> Buffer.add_char b '\b'; loop0 (i + 1)
      | '\\' -> Buffer.add_char b '\\'; loop0 (i + 1)
      | '[' -> Buffer.add_char b '['; loop0 (i + 1)
      | ']' -> Buffer.add_char b ']'; loop0 (i + 1)
      | '0'..'9' as c -> loop2 (i + 1) 1 ((Char.code c) land 15)
      | 'x' -> loop3 (i + 1) 0 0
      | c when List.mem c quotes -> Buffer.add_char b c; loop0 (i + 1)
      | c -> invalid_arg (sf "Unknown escape character %C" c)
  and loop2 i j q =
    if j = 3 then
      begin
        Buffer.add_char b (Char.chr q);
        loop0 i
      end
    else
      if i = m then
        invalid_arg "Unterminated escape string"
      else
        match u.[i] with
        | '0'..'9' as c -> loop2 (i + 1) (j + 1) (10 * q + (Char.code c) land 15)
        | c -> invalid_arg (sf "Invalid digit %C in decimal escape" c)
  and loop3 i j q =
    if j = 2 then
      begin
        Buffer.add_char b (Char.chr q);
        loop0 i
      end
    else
      if i = m then
        invalid_arg "Unterminated hexadecimal escape string"
      else
        match u.[i] with
        | '0'..'9' as c -> loop3 (i + 1) (j + 1) (q lsl 4 + (Char.code c) land 15)
        | 'a'..'f' as c -> loop3 (i + 1) (j + 1) (q lsl 4 + (Char.code c - Char.code 'a' + 10))
        | 'A'..'' as c -> loop3 (i + 1) (j + 1) (q lsl 4 + (Char.code c - Char.code 'a' + 10))
        | c -> invalid_arg (sf "Invalid digit %C in decimal escape" c)
  in
  loop0 0
;;
(* ***)
(*** charify *)
let charify u =
  if String.length u = 1 then
    u.[0]
  else
    raise Internal_error
;;
(* ***)

(*** collect_sequence *)
let collect_sequence xl =
  let rec loop = function
    | Node(N_S, [], xl') :: rest -> (loop xl') @ (loop rest)
    | y :: rest -> y :: (loop rest)
    | [] -> []
  in
  loop xl
;;
(* ***)

type 'a modifier = {
  mod_prefix : 'a option;
  mod_prepend : 'a option;
  mod_append : 'a option;
  mod_suffix : 'a option;
};;

let modifier_empty = {
  mod_prefix = None;
  mod_prepend = None;
  mod_append = None;
  mod_suffix = None
};;

(*** prefix *)
let prefix (m : 'a Peg.pe modifier) p =
  { m with mod_prefix =
      begin
        match m.mod_prefix with
        | None -> Some p
        | Some p' -> Some(S[p';p])
      end }
;;
(* ***)
(*** prepend *)
let prepend (m : 'a Peg.pe modifier) i =
  { m with mod_prepend =
      begin
        match m.mod_prepend with
        | None -> Some i
        | Some i' -> Some(S[i';i])
      end }
;;
(* ***)
(*** append *)
let append (m : 'a Peg.pe modifier) i =
  { m with mod_append =
      begin
        match m.mod_append with
        | None -> Some i
        | Some i' -> Some(S[i';i])
      end }
;;
(* ***)
(*** suffix *)
let suffix (m : 'a Peg.pe modifier) i =
  { m with mod_suffix =
      begin
        match m.mod_suffix with
        | None -> Some i
        | Some i' -> Some(S[i';i])
      end }
;;
(* ***)

let rec convert_expression ~(modifier:'a Peg.pe modifier) = function
  | Node(N_Epsilon, [], []) -> Epsilon
  | Node(N_Position, [], []) -> Position
  | Node(N_EOF, [], []) -> EOF
  | Node(N_BOF, [], []) -> BOF
  | Node(N_Sigma, [], []) -> C(B.True)
  | Node(N_S, [], xl) ->
      let xl = collect_sequence xl in
      let yl = List.map (fun x -> convert_expression ~modifier x) xl in
      S begin
        match modifier.mod_prepend, modifier.mod_append with
        | None, None -> yl
        | Some p, None -> List.concat (List.map (fun x -> [p;x]) yl)
        | None, Some s -> List.concat (List.map (fun x -> [x;s]) yl)
        | Some p, Some s -> List.concat (List.map (fun x -> [p;x;s]) yl)
      end
  | Node(N_Tight, [], xl) -> S(List.map (fun x -> convert_expression ~modifier x) xl)
  | Node(N_Option, [], [x]) -> Opt(convert_expression ~modifier x)
  | Node(N_String, [A_value, v], []) -> A(unescape_string ~quotes:['"'] v)
  | Node(N_Char,   [A_value, v], []) -> A(unescape_string ~quotes:['\''] v)
  | Node(N_N, [A_name, name], []) -> N name
  | Node(N_Or, [], xl) -> Or(List.map (fun x -> convert_expression ~modifier x) xl)
  | Node(N_And, [], [x]) -> And(convert_expression ~modifier x)
  | Node(N_Not, [], [x]) -> Not(convert_expression ~modifier x)
  | Node(N_Plus, [], [x]) ->
      begin
        let y = convert_expression ~modifier x in
        match modifier.mod_prepend, modifier.mod_append with
        | None, None -> Plus y
        | Some p, None -> S[Star(S[y;p]);y]
        | None, Some s -> S[y;Star(S[s;y])]
        | Some p, Some s -> Or[y; S[y;Star(S[p;y;s]);y]]
      end
  | Node(N_Star, [], [x]) ->
      begin
        let y = convert_expression ~modifier (Node(N_Plus, [], [x])) in
        Opt y
      end
  | Node(N_TightPlus, [], [x]) -> Plus(convert_expression ~modifier x)
  | Node(N_TightStar, [], [x]) -> Star(convert_expression ~modifier x)
  | Node(N_Chars, [], l) -> C(B.Or(List.map convert_chars l))
  | Node(N_NotChars, [], l) -> C(B.Not(B.Or(List.map convert_chars l)))
  | Node(N_Tokenize, [], [x]) -> Tokenize(convert_expression ~modifier x)
  | Node(N_Ascribe, [A_name,name], [x]) ->
      let y = convert_expression ~modifier x in
      Ascribe(name, y)
  | Node(N_JustTag, attributes, []) ->
      let open_tag = List.assoc A_open attributes in
      Build(open_tag, [])
  | Node(N_Build, attributes, xl) ->
      let open_tag = List.assoc A_open attributes
      and close_tag = List.assoc A_close attributes
      in
      if open_tag <> close_tag then
        raise (Error(sf "Opening tag %S does not match closing tag %S" open_tag close_tag))
      else
        begin
          let yl = List.map (fun x -> convert_expression ~modifier x) xl in
          Build(open_tag, yl)
        end
  | t ->
      error "Unhandled internal tree: %a" Grammar.print_tree t;
      raise Internal_error
and convert_chars = function
  | Node(N_Range, attributes, []) ->
      B.Atom(
        Range(
          charify (unescape_string ~quotes:['-';']'] (List.assoc A_low attributes)),
          charify (unescape_string ~quotes:['-';']'] (List.assoc A_high attributes))))
  | Node(N_Char, [A_value, c], []) -> B.Atom(One(charify (unescape_string ~quotes:['-';']'] c)))
  | t ->
      error "Unhandled internal tree in convert_chars: %a" Grammar.print_tree t;
      raise Internal_error
;;

let rec convert_production_list ~modifier productions =
  List.concat (List.map (fun x -> convert_production ~modifier x) productions)
and convert_grammar ?(modifier = modifier_empty) = function
  | Node(N_Root, [], [Node(N_Grammar, [], productions)]) -> convert_production_list ~modifier productions
  | t ->
      error "Unhandled internal tree in convert_grammar: %a" Grammar.print_tree t;
      raise Internal_error
and convert_production ~modifier = function
  | Node(N_Modifying, [], [Node(N_Suffixing, _, _); Node(N_Modifier, [], [x1]); Node(N_Grammar, [] ,xl)]) ->
      let y1 = convert_expression ~modifier x1 in
      let modifier = suffix modifier y1 in
      convert_production_list ~modifier xl
  | Node(N_Modifying, [], [Node(N_Prefixing, _, _); Node(N_Modifier, [], [x1]); Node(N_Grammar, [] ,xl)]) ->
      let y1 = convert_expression ~modifier x1 in
      let modifier = prefix modifier y1 in
      convert_production_list ~modifier xl
  | Node(N_Modifying, [], [Node(N_Outfixing, _, _); Node(N_Modifier, [], [x1]); Node(N_Grammar, [] ,xl)]) ->
      let y1 = convert_expression ~modifier x1 in
      let modifier = prefix modifier y1 in
      let modifier = suffix modifier y1 in
      convert_production_list ~modifier xl
  | Node(N_Modifying, [], [Node(N_Prepending, [], []); Node(N_Modifier, [], [x1]); Node(N_Grammar, [] ,xl)]) ->
      let y1 = convert_expression ~modifier x1 in
      let modifier = prepend modifier y1 in
      convert_production_list ~modifier xl
  | Node(N_Modifying, [], [Node(N_Appending, [], []); Node(N_Modifier, [], [x1]); Node(N_Grammar, [] ,xl)]) ->
      let y1 = convert_expression ~modifier x1 in
      let modifier = append modifier y1 in
      convert_production_list ~modifier xl
  | Node(N_Modifying, [], [Node(N_Surrounding, [], []); Node(N_Modifier, [], [x1]); Node(N_Grammar, [] ,xl)]) ->
      let y1 = convert_expression ~modifier x1 in
      let modifier = append modifier y1 in
      let modifier = prepend modifier y1 in
      convert_production_list ~modifier xl
  | Node(N_Production, [A_name, name], [x]) ->
      begin
        try
          let y = convert_expression ~modifier x in
          let y =
            match modifier.mod_prefix with
            | None -> y
            | Some p -> S[p;y]
          in
          let y =
            match modifier.mod_suffix with
            | None -> y
            | Some s -> S[y;s]
          in
          [(name, y)]
        with
        | x ->
            error "Inside %S\n" name;
            raise x
            (*raise (Inside(name, x))*)
      end
  | t ->
      error "Unhandled internal tree in convert_production: %a" Grammar.print_tree t;
      raise Internal_error
;;
