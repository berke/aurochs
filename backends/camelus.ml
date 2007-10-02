(* Camelus *)

open Peg;;
open Seq;;
open Machine;;
open Nog;;
open Pffpsf;;

module B = Boolean;;

module SM = Map.Make(String);;

(*** gensym *)
let gensym =
  let t = Hashtbl.create 1009 in
  fun prefix ->
    let n =
      try
        Hashtbl.find t prefix
      with
      | Not_found -> 0
    in
    Hashtbl.replace t prefix (n + 1);
    sf "_%s_%d" prefix n
;;
(* ***)
let print_node oc n = fp oc "%s%s" !Opt.node_prefix n;;
let print_attr oc n = fp oc "%s%s" !Opt.attribute_prefix n;;

(*** generate_type_defs *)
let generate_type_defs oc pg peg =
  let attributes, attribute_numbers = Nog.number_attributes pg peg
  and nodes, node_numbers = Nog.number_nodes pg peg
  in
  let sum_type print name a =
    fp oc "type %s =\n" name;
    Array.iteri (fun i u -> fp oc "| %a (* %d *)\n" print u i) a;
    fp oc ";;\n";
  in
  sum_type print_node "node_name" node_numbers;
  fp oc "\n";
  sum_type print_attr "attribute_name" attribute_numbers
;;
(* ***)
(*** generate_interface *)
let generate_interface fn pg peg =
  Util.with_file_output (fn^".mli") (fun oci ->
    fp oci "(* %s *)\n" (String.capitalize fn);
    fp oci "\n";
    generate_type_defs oci pg peg)
;;
(* ***)
(*** generate_classic *)
let generate_classic fn start peg (pg : (string, string) program) =
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
  let oc = open_out (fn^".ml") in
  let oci = open_out (fn^".mli") in

  (*** pri *)
  (*let pri oc d =
    for i = 1 to d do
      fp oc "  "
    done
  in*)
  (* ***)

  fp oc "(* %s *)\n" (String.capitalize fn);
  fp oc "\n";
  fp oci "(* %s *)\n" (String.capitalize fn);
  fp oci "\n";

  fp oc "open Peg;;\n";
  fp oc "open Nog;;\n";
  fp oc "open Machine;;\n";
  fp oc "\n";
  fp oc "exception Parse_error of int * string;;\n";
  fp oc "\n";
  (* Collect builds *)
  let root_node = !Opt.root_node in

  (*** build_names *)
  let build_names prefix iterator =
    let map = ref SM.empty in
    let number = ref 0 in
    List.iter
      begin fun (_, p) ->
        iterator
          begin fun n ->
            if not (SM.mem n !map) then
              begin
                map := SM.add n !number !map;
                fp oc "| %s%s\n" prefix n;
                fp oci "| %s%s\n" prefix n;
                incr number
              end
          end
          p
      end
      peg;
    !map
  in

  let build_printer name prefix map =
    fp oci "val print_%s : out_channel -> %s -> unit\n" name name;
    fp oc "let print_%s oc = function\n" name;
    SM.iter
      begin fun n _ ->
        fp oc "| %s%s -> output_string oc %S\n" prefix n n
      end
      map;
    fp oc ";;\n";
    fp oc "\n"
  in

  (* ***)
  let print_node oc n = fp oc "%s%s" !Opt.node_prefix n in
  let print_attr oc n = fp oc "%s%s" !Opt.attribute_prefix n in
  fp oc "type node_name = \n";
  fp oci "type node_name = \n";
  let node_map = build_names !Opt.node_prefix (fun f p -> Peg.iter_over_builds f p; f root_node) in
  fp oci "\n";
  fp oc "type attribute_name = \n";
  fp oci "type attribute_name = \n";
  let attribute_map = build_names !Opt.attribute_prefix Peg.iter_over_attributes in
  fp oci "\n";
  fp oci "type tree = (node_name, attribute_name) Peg.poly_tree\n\n";
  fp oc "type tree = (node_name, attribute_name) Peg.poly_tree\n\n";
  fp oci "type positioned_tree = (node_name, attribute_name) Peg.poly_positioned_tree\n\n";
  fp oc "type positioned_tree = (node_name, attribute_name) Peg.poly_positioned_tree\n\n";

  build_printer "node_name" !Opt.node_prefix node_map;
  build_printer "attribute_name" !Opt.attribute_prefix attribute_map;

  fp oci "val print_tree : out_channel -> tree -> unit\n";
  fp oci "val program : (node_name, attribute_name) Nog.program\n";
  fp oc "let print_tree oc t = Peg.print_poly_tree ~print_node:print_node_name ~print_attribute:print_attribute_name () oc t;;\n";

  (*
  (*** bexpr *)
  fp oc "let build c u =\n";

  let rec bexpr ?(indent=0) ?choice_number = function
    | BOF -> fp oc "%a(* BOF *)\n" pri indent
    | EOF -> fp oc "%a(* EOF *)\n" pri indent
    | Epsilon -> fp oc "%a(* Epsilon *)\n" pri indent
    | Tokenize x ->
        fp oc "%achildren := (Token(String.sub u _i (r - _i))) :: !_children;" pri indent;
    | Ascribe(n, x) ->
        fp oc "%alet saved_i = _i in\n" pri indent;
        bexpr ~indent x;
        fp oc "%a_attributes := (%s%s, String.sub u saved_i (_i - saved_i)) :: !_attributes;\n" pri indent !Opt.attribute_prefix n
    | N n -> fp oc "%alet _i = _build_%s _node _i in\n" pri indent n
    | S xl ->
        fp oc "%a(* Begin S *)\n" pri indent;
        List.iter
          begin fun x ->
            bexpr ~indent:(indent + 1) x;
          end
          xl;
        fp oc "%a(* End S *)\n" pri indent;
    | Or xl ->
        let choice_number' =
          match choice_number with
          | None -> invalid_arg "Unremoved inner disjunction in bexpr"
          | Some n -> n
        in
        fp oc "%a(* Begin Or *)\n" pri indent;
        fp oc "%aignore begin match c.choices.(%d).(_i) with\n" pri indent choice_number';
        fp oc "%a| None -> invalid_arg \"Bad choice table\"\n" pri indent;
        fp oc "%a| Some x ->\n" pri indent;
        let indent = indent + 1 in
        fp oc "%amatch x with\n" pri indent;
        let position = ref 0 in
        List.iter
          begin fun x ->
            fp oc "%a| %d -> begin\n" pri indent !position;
            bexpr ~indent:(indent + 1) x;
            fp oc "%a_i\n" pri (indent + 1);
            fp oc "%aend\n" pri indent;
            incr position
          end
          xl;
        fp oc "%a| _ -> invalid_arg \"Bad memo table\"\n" pri indent;
        fp oc "%aend; (* End Or *)\n" pri (indent - 1)
    | Build(n, xl) ->
        fp oc "%a(* Build %s *)\n" pri indent n;
        let old_node = gensym "old_node" in
        fp oc "%alet %s = _node in\n" pri indent old_node;
        fp oc "%alet (_attributes, _children) as _node = (ref [], ref []) in\n" pri indent;
        bexpr ~indent (S xl);
        fp oc "%alet new_node = Node(%s%s, List.rev !_attributes, List.rev !_children) in\n" pri indent !Opt.node_prefix n;
        fp oc "%alet (_attributes, _children) as _node = %s in\n" pri indent old_node;
        fp oc "%a_children := new_node :: !_children;\n" pri indent;
        fp oc "%a(* End build %s *)\n" pri indent n
    | And x -> fp oc "%a(* And *)\n" pri indent
    | Not x -> fp oc "%a(* Not *)\n" pri indent
    | Opt _|Star _|Plus _ -> invalid_arg "Not supported"
    | A v -> fp oc "%alet _i = _i + %d in\n" pri indent (String.length v)
    | C _ -> fp oc "%alet _i = _i + 1 in\n" pri indent
    (*| _ -> fp oc "_i = c->c_result;\n"*)
  (* ***)
  in
  (*** Generate builder function bodies *)
  let first = ref true in
  List.iter
    begin fun (name, expr) ->
      let (number, choice_number) = Hashtbl.find productions name in
      if !first then
        begin
          first := false;
          fp oc "let rec "
        end
      else
        fp oc "and ";
      fp oc "_build_%s ((_attributes, _children) as _node) _i =\n" name;
      fp oc "  match c.memo.(%d).(_i) with\n" number;
      fp oc "  | Jump r -> begin\n";
      bexpr ~indent:3 ?choice_number expr;
      fp oc "      r\n";
      fp oc "    end\n";
      fp oc "  | _ -> raise (Parse_error(_i, %S))\n" name
    end
    peg;
  (* ***)
  fp oc "  in\n";
  fp oc "  let ((_attributes, _children) as _node) = (ref [], ref []) in\n";
  fp oc "  ignore (_build_%s _node 0);\n" start;
  fp oc "  (Node(%s%s, List.rev !_attributes, List.rev !_children))\n" !Opt.node_prefix root_node;
  fp oc ";;\n";
  fp oc "\n";
  *)


  (*
  let pgm = Marshal.to_string pg [] in
  let m = String.length pgm in
  fp oc "let program : program =\n";
  fp oc "  let u = String.create %d in\n" m;
  fp oc "  let d = \n";
  let rec loop i j =
    if i + j = m then
      begin
        fp oc "\"\n";
        fp oc "  in\n"
      end
    else
      if j = 40 then
        begin
          fp oc "\\\n";
          loop (i + j) 0
        end
      else
        begin
          if j = 0 then
            if i = 0 then
              fp oc "   \""
            else
              fp oc "    "
          else
            ();
          let c = Char.code pgm.[i + j] in
          let ch = c lsr 4
          and cl = c land 15
          in
          fp oc "%c%c" (Char.chr (97 + ch)) (Char.chr (97 + cl));
          loop i (j + 1)
        end
  in
  loop 0 0;
  fp oc "  for i = 0 to %d do\n" (m - 1);
  fp oc "    u.[i] <- Char.chr ((((Char.code d.[2 * i]) - 97) lsl 4) + (((Char.code d.[2 * i + 1] - 97))))\n";
  fp oc "  done;\n";
  fp oc "  Marshal.from_string u 0\n";
  fp oc ";;\n";*)

  fp oc "let program = {\n";
  fp oc "  pg_start = %S;\n" pg.pg_start;
  fp oc "  pg_start_pc = %d;\n" pg.pg_start_pc;
  fp oc "  pg_build_pc = %d;\n" pg.pg_build_pc;
  fp oc "  pg_root = %a;\n" print_node pg.pg_root;
  fp oc "  pg_labels = List.fold_left (fun sm (k, v) -> SM.add k v sm) SM.empty [\n";
  SM.iter (fun k v -> fp oc "    (%S, %d);\n" k v) pg.pg_labels;
  fp oc "    ];\n";
  fp oc "  pg_productions = [|";
  Array.iter (fun s -> fp oc "%S;" s) pg.pg_productions;
  fp oc "    |];\n";
  fp oc "  pg_choices = [|";
  Array.iter (fun s -> fp oc "%S;" s) pg.pg_productions;
  fp oc "    |];\n";
  fp oc "  pg_code = [|";
  Array.iter (fun x ->
     fp oc "    %a;\n" (Machine.dump_instruction ~print_node ~print_attr) x) pg.pg_code;
  fp oc "    |];\n";
  fp oc "  };;\n\n";

  fp oc "let parse_positioned u =\n";
  fp oc "  Nog.execute_positioned program ~print_node:print_node_name ~print_attr:print_attribute_name ~root:%a u\n" print_node root_node;
  fp oc ";;\n";
  fp oc "\n";

  fp oc "let parse u = relativize u (parse_positioned u);;\n";

  fp oci "val parse_positioned : string -> positioned_tree\n";
  fp oci "val parse : string -> tree\n";
;;
(* ***)
(*** generate_implementation *)
let generate_implementation fn start peg (pg : (string, string) program) =
  Util.with_file_output (fn^".ml") (fun oc ->
    Util.with_file_output (fn^".mli") (fun oci ->
      fp oc "(* %s *)\n\n" (String.capitalize fn);
      fp oci "(* %s *)\n\n" (String.capitalize fn);
      generate_type_defs oc pg peg;
      generate_type_defs oci pg peg;
      fp oci "\n";
      fp oci "val binary : Aurochs.binary\n";
      fp oci "val program : (node_name, attribute_name) Aurochs.program Lazy.t\n";

      let u = Bytes.with_buffer_sink (Noggie.put_program pg peg) in
      fp oc "\n";
      fp oc "let binary =\n";
      Stringifier.print_ocaml_string ~indent:4 () oc u;
      fp oc ";;\n";
      fp oc "\n";
      fp oc "let program = lazy (Aurochs.program_of_binary binary);;\n"
    )
  )
;;
(* ***)
