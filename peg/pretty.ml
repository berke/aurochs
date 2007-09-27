(* Pretty *)

open Peg;;
module B = Boolean;;

let fp = Printf.fprintf;;

let print_char oc = function
  | '\\' -> fp oc "\\\\"
  | '-' -> fp oc "\\-"
  | ']' -> fp oc "\\]"
  | '[' -> fp oc "\\["
  | '\n' -> fp oc "\\n"
  | '\r' -> fp oc "\\r"
  | '\t' -> fp oc "\\t"
  | '\b' -> fp oc "\\b"
  | c -> fp oc "%c" c
;;

let rec print_chars oc = function
  | B.Atom(Range(c1,c2)) -> fp oc "%a-%a" print_char c1 print_char c2
  | B.Atom(One c) -> print_char oc c
  | B.Atom(Many cl) -> List.iter (print_char oc) cl
  | _ -> fp oc ".UNHANDLED_CHAR_RANGE."
let rec print_production priority oc = function
  | EOF -> fp oc "EOF"
  | BOF -> fp oc "BOF"
  | Epsilon -> fp oc "epsilon"
  | Position -> fp oc "position"
  | Tokenize x -> fp oc "{ %a }" (print_production true) x
  | Ascribe(n, x) -> fp oc "%s:%a" n (print_production false) x
  | A x ->
      if String.length x = 1 then
        begin
          match x.[0] with
          | '\n' -> fp oc "'\\n'"
          | '\r' -> fp oc "'\\r'"
          | '\t' -> fp oc "'\\t'"
          | '\b' -> fp oc "'\\b'"
          | '\'' -> fp oc "'\\''"
          | c    -> fp oc "%C" c
        end
      else
        fp oc "%S" x
  | C B.True -> fp oc "sigma"
  | C(B.Or[B.Atom(One c)]) | C(B.Atom(One c)) -> fp oc "%C" c
  | C(B.Or((_::_) as cl)) ->
      fp oc "[";
      List.iter
        (print_chars oc)
        cl;
      fp oc "]"
  | C(B.Atom _ as a) ->
      fp oc "[";
      print_chars oc a;
      fp oc "]"
  | C(B.Not(B.Atom _ as a)) ->
      fp oc "[^";
      print_chars oc a;
      fp oc "]"
  | C(B.Not(B.Or cl)) ->
      fp oc "[^";
      List.iter
        (print_chars oc)
        cl;
      fp oc "]"
  | C _ -> fp oc "(* UNHANDLED CHARS *)"
  | N n -> fp oc "%s" n
  | (S xl) as x ->
      if not priority then
        fp oc "(%a)" (print_production true) x
      else
        begin
          let first = ref true in
          List.iter
            begin fun x ->
              if !first then first := false else fp oc " ";
              print_production false oc x;
            end
            xl;
        end
  | Build(n, xl) ->
      fp oc "<%s>" n;
      let first = ref true in
      List.iter
        begin fun x ->
          if !first then first := false else fp oc " ";
          print_production true oc x;
        end
        xl;
      fp oc "</%s>" n
  | Or xl ->
      if not priority then fp oc "(";
      let first = ref true in
      List.iter
        begin fun x ->
          if !first then first := false else fp oc " | ";
          print_production false oc x;
        end
        xl;
      if not priority then fp oc ")"
  | And x ->
      if not priority then fp oc "(";
      fp oc "& %a" (print_production false) x;
      if not priority then fp oc ")"
  | Not x ->
      if not priority then fp oc "(";
      fp oc "~ %a" (print_production false) x;
      if not priority then fp oc ")"
  | Opt x ->
      if not priority then fp oc "(";
      fp oc "%a" (print_production false) x;
      if not priority then fp oc ")";
      fp oc "?"
  | Star x ->
      if not priority then fp oc "(";
      fp oc "%a" (print_production false) x;
      if not priority then fp oc ")";
      fp oc "*"
  | Plus x ->
      if not priority then fp oc "(";
      fp oc "%a" (print_production false) x;
      if not priority then fp oc ")";
      fp oc "+"
;;

let print_grammar oc g =
  List.iter
    begin fun (name, production) -> 
      fp oc "%s ::= " name;
      print_production true oc production;
      fp oc ";\n"
    end
    g
;;
