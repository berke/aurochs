(* Camelus *)

open Peg
open Seq
open Machine
open Nog
open Pffpsf

module B = Boolean

module SM = Map.Make(String)

let print_node oc n = fp oc "%s%s" !Opt.node_prefix n
let print_attr oc n = fp oc "%s%s" !Opt.attribute_prefix n

(* generate_type_defs *)
let generate_type_defs oc pg peg =
  let attributes, attribute_numbers = Nog.number_attributes pg peg
  and nodes, node_numbers = Nog.number_nodes pg peg
  in
  let sum_type print name a =
    fp oc "type %s =\n" name;
    Array.iteri (fun i u -> fp oc "| %a (* %d *)\n" print u i) a;
    fp oc "\n";
  in
  sum_type print_node "node_name" node_numbers;
  fp oc "\n";
  sum_type print_attr "attribute_name" attribute_numbers

(* generate_interface *)
let generate_interface ?(pack=true) fn pg peg =
  Util.with_file_output (fn^".mli") (fun oci ->
    fp oci "(* %s *)\n" (String.capitalize fn);
    fp oci "\n";
    if pack then fp oci "\nopen Aurochs_pack;\n";
    generate_type_defs oci pg peg)

(* generate_classic *)
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

  fp oc "(* %s *)\n" (String.capitalize fn);
  fp oc "\n";
  fp oci "(* %s *)\n" (String.capitalize fn);
  fp oci "\n";

  fp oc "open Aurochs_pack\n";
  fp oc "open Peg\n";
  fp oc "open Nog\n";
  fp oc "open Machine\n";
  fp oc "\n";
  fp oc "exception Parse_error of int * string\n";
  fp oc "\n";

  (* Collect builds *)
  let root_node = !Opt.root_node in

  (* build_names *)
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
    fp oc "\n";
    fp oc "\n"
  in


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
  fp oc "let print_tree oc t = Peg.print_poly_tree ~print_node:print_node_name ~print_attribute:print_attribute_name () oc t\n";

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
  fp oc "  }\n\n";

  fp oc "let parse_positioned u =\n";
  fp oc "  Nog.execute_positioned program ~print_node:print_node_name ~print_attr:print_attribute_name ~root:%a u\n" print_node root_node;
  fp oc "\n";
  fp oc "\n";

  fp oc "let parse u = relativize u (parse_positioned u)\n";

  fp oci "val parse_positioned : string -> positioned_tree\n";
  fp oci "val parse : string -> tree\n"

(* generate_implementation *)
let generate_implementation ?(pack=true) fn start peg (pg : (string, string) program) =
  Util.with_file_output (fn^".ml") (fun oc ->
    Util.with_file_output (fn^".mli") (fun oci ->
      let aurochs = "Aurochs" in
      let _, attribute_numbers = Nog.number_attributes pg peg
      and _, node_numbers = Nog.number_nodes pg peg
      in

      let build_printer name prefix a =
        fp oci "\n";
        fp oci "val print_%s : out_channel -> %s -> unit\n" name name;

        fp oc "\n";
        fp oc "let print_%s oc = function\n" name;
        Array.iter
          begin fun n ->
            fp oc "| %s%s -> output_string oc %S\n" prefix n n
          end
          a;
        fp oc "\n";
        fp oc "\n"
      in

      fp oc "(* %s *)\n" (String.capitalize fn);
      fp oci "(* %s *)\n" (String.capitalize fn);
      if pack then
        begin
          fp oc "\nopen Aurochs_pack\n\n";
          fp oci "\nopen Aurochs_pack\n\n";
        end;

      generate_type_defs oc pg peg;
      generate_type_defs oci pg peg;

      fp oci "\n";
      fp oci "type tree = (node_name, attribute_name) Peg.poly_tree\n\n";
      fp oci "type positioned_tree = (node_name, attribute_name) Peg.poly_positioned_tree\n\n";

      fp oc "\n";
      fp oc "type tree = (node_name, attribute_name) Peg.poly_tree\n";
      fp oc "type positioned_tree = (node_name, attribute_name) Peg.poly_positioned_tree\n";

      build_printer "node_name" !Opt.node_prefix node_numbers;
      build_printer "attribute_name" !Opt.attribute_prefix attribute_numbers;

      let u = Bytes_.with_buffer_sink (Noggie.put_program pg peg) in
      fp oc "\n";
      fp oc "let binary =\n";
      Stringifier.print_ocaml_string ~indent:4 () oc u;
      fp oc "\n";
      fp oc "\n";
      fp oc "let program = lazy (%s.program_of_binary binary)\n" aurochs;

      fp oc "\n";
      fp oc "let parse_positioned u = %s.read_positioned ~grammar:(`Program program) ~text:(`String u)\n" aurochs;
      fp oc "let parse u = Peg.relativize u (parse_positioned u)\n";
      fp oc "let print_tree oc t = Peg.print_poly_tree ~print_node:print_node_name ~print_attribute:print_attribute_name () oc t\n";

      fp oci "\n";
      fp oci "val binary : %s.binary\n" aurochs;
      fp oci "val program : (node_name, attribute_name) %s.program Lazy.t\n" aurochs;
      fp oci "val parse : string -> tree\n";
      fp oci "val parse_positioned : string -> positioned_tree\n";
      fp oci "val print_tree : out_channel -> tree -> unit\n";
    )
  )

