(* Convert_grammar *)

open Peg;;
module B = Boolean;;

exception Error of string;;
exception Internal_error;;
exception Inside of string * exn;;

let sf = Printf.sprintf;;

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
      | c when List.mem c quotes -> Buffer.add_char b c; loop0 (i + 1)
      | c -> invalid_arg (sf "Unknown escape character %C" c)
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

let rec convert_expression = function
  | Node("Epsilon", [], []) -> Epsilon
  | Node("EOF", [], []) -> EOF
  | Node("Sigma", [], []) -> C(B.True)
  | Node("S", [], xl) -> S(List.map convert_expression xl)
  | Node("Option", [], [x]) -> Opt(convert_expression x)
  | Node("String", ["value", v], []) -> A(unescape_string ~quotes:['"'] v)
  | Node("Char",   ["value", v], []) -> A(unescape_string ~quotes:['\''] v)
  | Node("N", ["name", name], []) -> N name
  | Node("Or", [], xl) -> Or(List.map convert_expression xl)
  | Node("And", [], [x]) -> And(convert_expression x)
  | Node("Not", [], [x]) -> Not(convert_expression x)
  | Node("Plus", [], [x]) -> Plus(convert_expression x)
  | Node("Star", [], [x]) -> Star(convert_expression x)
  | Node("Opt", [], [x]) -> Opt(convert_expression x)
  | Node("Chars", [], l) -> C(B.Or(List.map convert_chars l))
  | Node("NotChars", [], l) -> C(B.Not(B.Or(List.map convert_chars l)))
  | Node("Tokenize", [], [x]) -> Tokenize(convert_expression x)
  | Node("Ascribe", ["name",name], [x]) ->
      let y = convert_expression x in
      Ascribe(name, y)
  | Node("Build", attributes, xl) ->
      let open_tag = List.assoc "open" attributes
      and close_tag = List.assoc "close" attributes
      in
      if open_tag <> close_tag then
        raise (Error(sf "Opening tag %S does not match closing tag %S" open_tag close_tag))
      else
        begin
          let yl = List.map convert_expression xl in
          Build(open_tag, yl)
        end
  | t ->
      Printf.eprintf "Unhandled internal tree:\n%a%!" (Peg.print_tree ()) t;
      raise Internal_error
and convert_chars = function
  | Node("Range", attributes, []) ->
      B.Atom(
        Range(
          charify (unescape_string ~quotes:['-';']'] (List.assoc "low" attributes)),
          charify (unescape_string ~quotes:['-';']'] (List.assoc "high" attributes))))
  | Node("Char", ["value", c], []) -> B.Atom(One(charify (unescape_string ~quotes:['-';']'] c)))
  | _ -> raise Internal_error
;;

let rec convert_grammar = function
  | Node("Grammar", [], productions) -> List.map convert_production productions
  | _ -> raise Internal_error
and convert_production = function
  | Node("Production", ["name", name], [expression]) ->
      begin
        try
          (name, convert_expression expression)
        with
        | x ->
            Printf.eprintf "Inside %S\n" name;
            raise x
            (*raise (Inside(name, x))*)
      end
  | _ -> raise Internal_error
;;

let check_grammar start g =
  let results = ref [] in
  let error x   = results := (`Error x)   :: !results in
  let warning x = results := (`Warning x) :: !results in
  let inform x  = results := (`Info x)    :: !results in
  let module SS = Set.Make(String) in
  let ns_seen = ref SS.empty in
  let ns_def  = ref SS.empty in
  List.iter
    begin fun (name, pe) ->
      ns_def := SS.add name !ns_def;
      Peg.iter_over_n
        begin fun n ->
          ns_seen := SS.add n !ns_seen
        end
        pe
    end
    g;
  inform (sf "Number of non-terminals referenced: %d" (SS.cardinal !ns_def));
  SS.iter
    begin fun name ->
      error (sf "Non-terminal %S used but not defined." name)
    end
    (SS.diff !ns_seen !ns_def);
  (* Reachability analysis *)
  let rec transitive past n =
    let pe =
      try
        List.assoc n g
      with
      | Not_found -> raise (Error(sf "Production %S not found" n))
    in
    let past' = ref (SS.add n past) in
    Peg.iter_over_n
      begin fun n' ->
        if SS.mem n' !past' then
          ()
        else
          begin
            past' := transitive !past' n'
          end
      end
      pe;
    !past'
  in
  let reachable = transitive SS.empty start in
  SS.iter
    begin fun name ->
      warning (sf "Non-terminal %S is not reachable." name)
    end
    (SS.diff !ns_seen reachable);
  List.rev !results
;;
