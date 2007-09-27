(* AST for Exalead *)

open Peg;;
open Seq;;
open Machine;;
module B = Boolean;;

exception Inside of string * exn;;

let fp = Printf.fprintf;;
let sf = Printf.sprintf;;

(* u points to the byte after the END of the string *)
(* u[-1] is the last char *)
(* u[-m] is the first char, wher m is the length *)
(* failure is signaled by a 1 *)

let r_eof = 0
and r_unknown = 1
and r_fail = 2
and r_busy = 3
;;

(*** generate *)
let generate fn ?(start="start") peg =
  let peg = Canonify.canonify_grammar peg in
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
  let oc = open_out (fn^".exa") in
  (*** bexpr *)
  let rec bexpr ?choice_number = function
    | EOF -> fp oc "/* EOF */\n"
    | Epsilon -> fp oc "/* Epsilon */\n"
    | Tokenize x ->
        fp oc "/* Start token */\n";
        fp oc "{\n\
                  tree *tk;\n\
                  \n\
                  tk = create_token(i, r); i = r;\n\
                  add_children(nd, tk); }\n";
        fp oc "/* End token */\n"
    | Ascribe(n, x) ->
        fp oc "{ int saved_i;\n\
                 \n\
                 saved_i = i;\n";
        bexpr x;
        fp oc "attach_attribute(nd, %S, saved_i, i);\n" n;
        fp oc "}\n"
    | N n -> fp oc "i = build_%s(cx, nd, i);\n" n
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
    | A v -> fp oc "i += %d;\n" (String.length v)
    | C _ -> fp oc "i ++;\n"
    (*| _ -> fp oc "i = c->c_result;\n"*)
  (* ***)
  in
  (*** gexpr *)
  let rec gexpr ?choice_number = function
    | EOF -> fp oc "if(i < 0) r = R_Failure();\n"
    | Epsilon -> fp oc "/* Epsilon */\n"
    | Tokenize x ->
        fp oc "/* Start token */\n";
        gexpr x;
        fp oc "/* End token */\n"
    | Ascribe(n, x) ->
        fp oc "/* Ascribe %s */\n" n;
        gexpr x;
        fp oc "/* End ascribe %s */\n" n
    | A v ->
        let m = String.length v in
        fp oc "if(cx.input.length - i < %d) r = R_Failure();\n" (-m);
        fp oc "else do {\n";
        for i = 0 to m - 1 do
          fp oc "if(u[i++] != %C) { r = R_Failure(); break; }\n" v.[i]
        done;
        fp oc "} while(false);\n";
    | C b ->
        fp oc "if(";
        gbool b;
        fp oc ") i++; else r = R_Failure();\n"
    | N n -> fp oc "i = parse_%s(cx, i);\n" n
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
    | Opt x ->
        fp oc "/* Opt */\n\
               { int saved_i;\n\
                 \n\
                 saved_i = i;";
        gexpr x;
        fp oc "  if(i > 0) i = saved_i;\n
               }\n"
    | Star x ->
        fp oc "/* Begin Star */\n\
               { int saved_i;\n\
                 \n\
                 for(;;) {\n\
                   saved_i = i;\n";
                   gexpr x;
        fp oc "    if(i > 0) {\n\
                     i = saved_i;\n\
                     break;\n\
                   }\n\
                 }\n\
               }\n\
               /* End Star */\n"
    | Plus x -> gexpr (S[x;Star x])
  (* ***)
  (*** gatom *)
  and gatom = function
    | One c -> fp oc "(i < 0 && u[i] == %C)" c
    | Range(c1,c2) -> fp oc "(i < 0 && %C <= u[i] && u[i] <= %C)" c1 c2
    | Many cl ->
        let first = ref true in
        List.iter
          begin fun c ->
            if !first then
              first := false
            else
              fp oc " | ";
              fp oc "(i < 0 && u[i] == %C)" c
          end
          cl
  (* ***)
  (*** gbool *)
  and gbool = function
    | B.True -> fp oc "true"
    | B.False -> fp oc "false"
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
  (* ***)
  fp oc "package %s;\n" fn;
  fp oc "import peg;\n";
  let is_static name = name <> start in
  (*** create_context *)
  fp oc "int NUM_PRODUCTIONS = %d;\n" !num_productions;
  fp oc "int NUM_ALTERNATIVES = %d;\n" !num_alternatives;
  (* ***)
  (*** Generate parser function bodies *)
  if not !Opt.build_only then
    List.iter
      begin fun (name, expr) ->
        let (number, choice_number) = Hashtbl.find productions name in
        fp oc "/* Non-terminal %s has number %d */\n" name number;
        fp oc "%sint parse_%s(Context cx, int i) {\n" (if is_static name then "static " else "") name;
        fp oc "Letter u;\n";
        fp oc "Result r, r_old;\n";
        fp oc "u = cx.input;\n";
        fp oc "r_old = cx.results[%d][i];\n" number;
        fp oc "switch(r_old)\n\
                 case @R_Jump:
                 case @R_Fail:
                   r = r_old;
                   break;
                 case @R_Busy:
                   r = R_Fail();
                   break;
                 case @R_Unknown:";
        fp oc "do {\n";
        gexpr ?choice_number expr;
        fp oc "} while(false);\n";
        fp oc "    break;\n";
        fp oc "}\n";
        fp oc "cx.results[%d][i] = r;\n" number;
        fp oc "return r;\n";
        fp oc "}\n"
      end
      peg;
  (* ***)
  (*** Generate builder function bodies *)
  (*List.iter
    begin fun (name, expr) ->
      let (number, choice_number) = Hashtbl.find productions name in
      fp oc "%sint build_%s(Context cx, Node nd, int i) {\n" (if is_static name then "static " else "") name;
      fp oc "result r;\n";
      fp oc "r = cx->cx_results[%d][i];\n" number;
      fp oc "if(r > R_EOF) abort();\n";
      bexpr ?choice_number expr;
      fp oc "  return i; /* Should be r */\n";
      fp oc "}\n";
    end
    peg;*)
  (* ***)
  fp oc "\n";
;;
(* ***)
