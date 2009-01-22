(* Ritchie *)

open Peg;;
open Seq;;
open Machine;;
open Pffpsf;;
open Talk;;
open Talk_level;;

module B = Boolean;;

exception Inside of string * exn;;

(* u points to the byte after the END of the string *)
(* u[-1] is the last char *)
(* u[-m] is the first char, wher m is the length *)
(* failure is signaled by a 1 *)

let r_eof = 0
and r_unknown = 1
and r_fail = 2
and r_busy = 3

(* print_c_char *)
let print_c_char oc c =
  let k = Char.code c in
  fp oc "/* %d */ " k;
  match c with
  | '\000' -> fp oc "\\0"
  | '\'' -> fp oc "'\\''"
  | '\n'|'\t'|'\b' -> fp oc "%C" c
  | '\r' -> fp oc "'\\r'"
  | _ ->
      if 32 <= k && k < 128 then
        fp oc "%C" c
      else
        fp oc "'\\%03o'" k

(* generate *)
let generate fn ?(start="start") peg =
  let m = List.length peg in
  let num_alternatives = ref 0 in
  let num_productions = ref 0 in
  let productions = Hashtbl.create (2 * m) in
  begin
    List.iter
      begin function (name, pe) ->
        let number = !num_productions in
        incr num_productions;
        let choice_number =
          match pe with
          | Or _ ->
              let id = !num_alternatives in
              incr num_alternatives;
              Some id
          | _ -> None
        in
        Hashtbl.replace productions name (number, choice_number)
      end
      peg;
  end;
  let oc = open_out (fn^".c") in
  let och = open_out (fn^".h") in
  (* bexpr *)
  let rec bexpr ?choice_number = function
    | BOF -> fp oc "/* BOF */\n"
    | EOF -> fp oc "/* EOF */\n"
    | Position -> fp oc "/* Position */\n"
    | Epsilon -> fp oc "/* Epsilon */\n"
    | Tokenize x ->
        fp oc "/* Start token */\n";
        fp oc "{\n\
                  tree *tk;\n\
                  \n\
                  tk = create_token(i, r); i = r;\n\
                  add_children(nd, tk); }\n";
        fp oc "/* End token */\n"
    | Constant _ -> fp oc "/* Constant */\n"
    | Ascribe(n, Position) ->
        fp oc "attach_position_attribute(nd, %S, i);\n" n;
    | Ascribe(n, Constant u) ->
        fp oc "/* Constant attribute */\n";
    | Ascribe(n, x) ->
        fp oc "{ int saved_i;\n\
                 \n\
                 saved_i = i;\n";
        bexpr x;
        fp oc "attach_attribute(nd, %S, saved_i, i);\n" n;
        fp oc "}\n"
    | N n -> fp oc "i = %sbuild_%s(cx, nd, i);\n" !Opt.function_prefix n
    | S xl ->
        fp oc "/* Begin S */\n";
        List.iter
          begin fun x ->
            bexpr x;
          end
          xl;
        fp oc "/* End S */\n";
    | Or xl ->
        let choice_number' =
          match choice_number with
          | None -> invalid_arg "Unremoved inner disjunction in bexpr"
          | Some n -> n
        in
        fp oc "/* Begin Or */\n";
        fp oc "switch(cx->cx_alternatives[%d][i]) {\n" choice_number';
        let position = ref 0 in
        List.iter
          begin fun x ->
            fp oc "  case %d:\n" !position;
            bexpr x;
            fp oc "    break;\n";
            incr position
          end
          xl;
        fp oc "  default:\n\
                   abort();\n";
        fp oc "};\n";
        fp oc "/* End Or */\n"
    | Build(n, xl) ->
        fp oc "/* Build %s */\n" n;
        fp oc "{ tree *tr;\n\
                 node *old_nd;\n\
                 \n\
                 old_nd = nd;\n\
                 tr = create_node(%S);\n\
                 nd = &tr->t_element.t_node;\n" n;
        bexpr (S xl);
        fp oc "  nd = old_nd;\n\
                 add_children(nd, tr);\n\
               }";
        fp oc "/* End build %s */\n" n
    | And x -> fp oc "/* And */\n" (* XXX *)
    | Not x -> fp oc "/* Not */"   (* XXX *)
    | Opt _|Star _|Plus _ -> invalid_arg "Not supported"
    | Ax(v, _) | A v -> fp oc "i += %d;\n" (String.length v)
    | C _ -> fp oc "i ++;\n"
    (*| _ -> fp oc "i = c->c_result;\n"*)

  in
  (* gexpr *)
  let rec gexpr ?choice_number = function
    | EOF -> fp oc "if(i < 0) i = R_FAIL;\n"
    | BOF -> fp oc "if(i != - cx->cx_input_length) i = R_FAIL;\n"
    | Epsilon -> fp oc "/* Epsilon */\n"
    | Position -> fp oc "/* Position */\n"
    | Tokenize x ->
        fp oc "/* Start token */\n";
        gexpr x;
        fp oc "/* End token */\n"
    | Constant _ -> fp oc "/* Constant */\n"
    | Ascribe(n, x) ->
        fp oc "/* Ascribe %s */\n" n;
        gexpr x;
        fp oc "/* End ascribe %s */\n" n
    | Ax(v, Exact) | A v ->
        let m = String.length v in
        fp oc "if(i > %d) i = R_FAIL;\n" (-m);
        fp oc "else do {\n";
        for i = 0 to m - 1 do
          fp oc "if(u[i++] != %a) { i = R_FAIL; break; }\n" print_c_char v.[i]
        done;
        fp oc "} while(0);\n";
    | Ax(v, Case_insensitive) ->
        let m = String.length v in
        fp oc "if(i > %d) i = R_FAIL;\n" (-m);
        fp oc "else do {\n";
        for i = 0 to m - 1 do
          fp oc "if(tolower(u[i++]) != %a) { i = R_FAIL; break; }\n" print_c_char (Char.lowercase v.[i])
        done;
        fp oc "} while(0);\n";
    | C b ->
        fp oc "if(";
        gbool b;
        fp oc ") i++; else i = R_FAIL;\n"
    | N n -> fp oc "i = %sparse_%s(cx, i);\n" !Opt.function_prefix n
    | S xl ->
        fp oc "/* Begin S */\n";
        fp oc "do {\n";
        List.iter
          begin fun x ->
            gexpr x;
            fp oc "if(i > 0) break;\n"
          end
          xl;
        fp oc "} while(0);\n";
        fp oc "/* End S */\n";
    | Or xl ->
        let choice_number' =
          match choice_number with
          | None -> invalid_arg "Unremoved inner disjunction in gexpr"
          | Some n -> n
        in
        fp oc "/* Begin Or */\n";
        fp oc "do { int saved_i;\n\
                 \n\
                 saved_i = i;\n";
        let position = ref 0 in
        List.iter
          begin fun x ->
            if !position > 0 then fp oc "i = saved_i;\n";
            gexpr x;
            fp oc "if(i <= 0) {\n\
                      cx->cx_alternatives[%d][saved_i] = %d;\n\
                      break;\n\
                    }\n" choice_number' !position;
            incr position
          end
          xl;
        fp oc "} while(0);\n";
        fp oc "/* End Or */\n"
    | Build(n, xl) ->
        fp oc "/* Build %s */\n" n;
        gexpr (S xl);
        fp oc "/* End build %s */\n" n
    | And x ->
        fp oc "/* Begin And */\n";
        fp oc "{ int saved_i;\n\
                 \n\
                 saved_i = i;";
        gexpr x;
        fp oc "  if(i <= 0) i = saved_i; }\n";
        fp oc "/* End And */\n"
    | Not x ->
        fp oc "{ int saved_i;\n\
                 \n\
                 saved_i = i;\n";
        gexpr x;
        fp oc "  if(i <= 0)\n\
                   i = R_FAIL;\n\
                 else
                   i = saved_i; }\n"
    | Opt _ | Star _ | Plus _ -> invalid_arg "Uncanonical grammar (saw Opt, Star or Plus)"

  (* gatom *)
  and gatom = function
    | One c -> fp oc "(i < 0 && u[i] == %a)" print_c_char c
    | Range(c1,c2) -> fp oc "(i < 0 && %a <= u[i] && u[i] <= %a)" print_c_char c1 print_c_char c2
    | Many cl ->
        let first = ref true in
        List.iter
          begin fun c ->
            if !first then
              first := false
            else
              fp oc " | ";
              fp oc "(i < 0 && u[i] == %a)" print_c_char c
          end
          cl

  (* gbool *)
  and gbool = function
    | B.True -> fp oc "1"
    | B.False -> fp oc "0"
    | B.Atom a -> gatom a
    | B.Or bl ->
        let first = ref true in
        List.iter
          begin fun b ->
            if !first then
              first := false
            else
              fp oc " | ";
            fp oc "(";
            gbool b;
            fp oc ")"
          end
          bl
    | B.And bl ->
        let first = ref true in
        List.iter
          begin fun b ->
            if !first then
              first := false
            else
              fp oc " & ";
            fp oc "(";
            gbool b;
            fp oc ")"
          end
          bl
    | B.Not b ->
        fp oc "!(";
        gbool b;
        fp oc ")"
  in

  fp oc "#include <stdlib.h>\n";
  fp oc "#include \"peg_prelude.h\"\n";
  let is_static name = name <> start in
  fp oc "#include \"%s.h\"\n" fn;
  (* Function prototypes *)
  List.iter
    begin fun (name, expr) ->
      let st = is_static name in
      if not !Opt.build_only then fp oc "%sint %sparse_%s(context *, int);\n" (if st then "static " else "") !Opt.function_prefix name;
      if not st then fp och "int %sparse_%s(context *, int);\n" !Opt.function_prefix name;
      fp oc "%sint %sbuild_%s(context *, node *, int);\n" (if st then "static " else "") !Opt.function_prefix name;
      if not st then fp och "int %sbuild_%s(context *, node *, int);\n" !Opt.function_prefix name;
    end
    peg;

  (* create_context *)
  fp och "#define NUM_PRODUCTIONS %d\n" !num_productions;
  fp och "#define NUM_ALTERNATIVES %d\n" !num_alternatives;

  (* Generate parser function bodies *)
  info `Time "Generating parser functions";
  if not !Opt.build_only then
    List.iter
      begin fun (name, expr) ->
        info `Minor "  %s" name;
        let (number, choice_number) = Hashtbl.find productions name in
        fp oc "/* Non-terminal %s has number %d */\n" name number;
        fp oc "%sint %sparse_%s(context *cx, int i) {\n" (if is_static name then "static " else "") !Opt.function_prefix name;
        fp oc "letter *u;\n";
        fp oc "result *r;\n";
        fp oc "u = cx->cx_input;\n";
        fp oc "r = cx->cx_results[%d] + i;\n\
               if(*r <= R_EOF || *r == R_FAIL) return *r;\n\
               else {\n\
                 if(*r == R_BUSY) {\n\
                   *r = R_FAIL;\n\
                   return *r;\n\
                 } else {\n\
                   *r = R_BUSY;\n\
                 }\n\
               }\n" number;
        fp oc "do {\n";
        gexpr ?choice_number expr;
        fp oc "} while(0);\n";
        fp oc "*r = i;\n";
        fp oc "return i;\n";
        fp oc "}\n"
      end
      peg;

  (* Generate builder function bodies *)
  List.iter
    begin fun (name, expr) ->
      let (number, choice_number) = Hashtbl.find productions name in
      fp oc "%sint %sbuild_%s(context *cx, node *nd, int i) {\n" (if is_static name then "static " else "") !Opt.function_prefix name;
      fp oc "result r;\n";
      fp oc "r = cx->cx_results[%d][i];\n" number;
      fp oc "if(r > R_EOF) abort();\n";
      bexpr ?choice_number expr;
      fp oc "  return i; /* Should be r */\n";
      fp oc "}\n";
    end
    peg;

  fp oc "\n";
  fp och "\n";

