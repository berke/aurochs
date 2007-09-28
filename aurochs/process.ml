(* Process *)

open Peg;;
open Pffpsf;;
open Util;;
open Talk;;

exception Error of string;;

type 'a name =
| Resolved of string * 'a
| Unresolved of string
;;

(*** load_grammar *)
let load_grammar fn =
  if !Opt.bootstrap then
    begin
      let u = Driver.read_file fn in
      let m = String.length u in
      let peg = Grammar_original.peg in
      with_file_output "internal.peg" (fun oc -> Pretty.print_grammar oc peg);
      let (q, i, t) =
        Peg.descent
          (fun x -> List.assoc x peg)
           (List.assoc "start" peg) u in
      try
        if q && i = m then
          begin
            begin
              match !Opt.dump_xml with
              | None -> ()
              | Some fn ->
                  info `Normal "Dumping XML grammar to file %s" fn;
                  with_file_output fn (fun oc -> Peg.print_tree () oc t)
            end;
            Convert_grammar_original.convert_grammar t
          end
        else
          raise (Error(sf "Error in grammar file %S at character %d" fn (i + 1)))
      with
      | Peg.Fail -> raise (Error(sf "Cannot parse grammar file %S" fn))
    end
  else
    begin
      let u = Driver.read_file fn in
      let t = Grammar.parse u in
      begin
        match !Opt.dump_xml with
        | None -> ()
        | Some fn ->
            info `Normal "Dumping XML grammar to file %s" fn;
            with_file_output fn (fun oc -> Grammar.print_tree oc t)
      end;
      Convert_grammar.convert_grammar t
     (*assert false*)
    end
;;
(* ***)
module SM = Map.Make(String);;
(*** build_color_table *)
let build_color_table ?(min_color=1) ?(max_color=7) t =
  let color = ref min_color in
  let builds_table = ref SM.empty in
  let attributes_table = ref SM.empty in
  let add_color table name =
    if not (SM.mem name !table) then
      begin
        table := SM.add name !color !table;
        color := !color + 1;
        if !color = max_color then color := min_color
      end
  in
  Peg.iter_over_poly_tree_nodes (fun b -> add_color builds_table b) t;
  Peg.iter_over_poly_tree_attributes (fun a -> add_color attributes_table a) t;
  (!builds_table, !attributes_table)
;;
(* ***)
(*** colorize *)
let colorize ?(default_acolor=Ansi.black) ?(default_bcolor=Ansi.white) (builds_table, attributes_table) u pt =
  let m = String.length u in
  let ac = Array.make (m + 1) default_acolor in
  let bc = Array.make (m + 1) default_bcolor in
  let get_build_color b = SM.find b builds_table
  and get_attribute_color b = SM.find b attributes_table
  in
  let rec loop_build = function
    | P_Node(i, j, b, al, bl) ->
        let c = get_build_color b in
        for k = i to j - 1 do
          bc.(k) <- c;
        done;
        List.iter loop_attributes al;
        List.iter loop_build bl
    | P_Token(_, _) -> ()
  and loop_attributes (i, j, an) =
    let c = get_attribute_color an in
    for k = i to j - 1 do
      ac.(k) <- c
    done
  in
  loop_build pt;
  (ac, bc)
;;
(* ***)
(*** dump_colorized *)
let dump_colorized oc (ac, bc) u =
  let m = String.length u in
  let last_fg_color = ref (-1) in
  let last_bg_color = ref (-1) in
  for i = 0 to m - 1 do
    let (a, b) = (ac.(i), bc.(i)) in
    let c = u.[i] in
    let (a, b) =
      match c with
      | ' '|'\n'|'\r'|'\t' -> (Ansi.white, Ansi.black)
      | _ -> (a, b)
    in
    let (fg, bg) =
      if !Opt.colorize_background then
        (b, a)
      else
        (b, Ansi.black)
    in
    if fg <> !last_fg_color then
      begin
        last_fg_color := b;
        fp oc "%s" Ansi.foreground.(fg)
      end;
    if b <> !last_bg_color then
      begin
        last_bg_color := bg;
        fp oc "%s" Ansi.background.(bg)
      end;
    output_char oc u.[i]
  done;
  fp oc "%s%!" Ansi.none
;;
(* ***)
(*** parse_file_with_nog *)
let parse_file_with_nog pg fn =
  let with_dump_oc = 
    match !Opt.dump_colorized with
    | None -> ignore
    | Some fn ->
        let oc = open_out fn in
        fun f -> f oc
  in
  let line = ref 0 in
  let treat u =
    try
      let profiler =
        match !Opt.profile with
        | None -> ignore
        | Some fn ->
            fun profile ->
              with_file_output fn
                begin fun oc ->
                  Noggie.print_code
                    oc
                    ~annotator:(fun oc pc -> fp oc "%5d %5d -- " profile.(pc) pc)
                    pg.Nog.pg_code
                end
      in
      let pt =
        let (trace, interactive) = (!Opt.trace or !Opt.debug, !Opt.debug) in
        Nog.execute_positioned
          ~quick:!Opt.quick
          ?log_calls:!Opt.log_calls
          ?record:!Opt.record
          ~show_memo:!Opt.show_memo
          ~root:!Opt.root_node
          ~trace
          ~interactive
          ~profiler
          ~print_node:output_string
          ~print_attr:output_string
          pg
          u
      in
      let t = Peg.relativize u pt in
      with_dump_oc
        begin fun oc ->
          let bat = build_color_table t in
          let ct = colorize bat u pt in
          dump_colorized oc ct u;
          if !Opt.line then fp oc "%!\n"
        end;
      if !Opt.tree then
        begin
          info `Important "RESULT";
          Peg.print_tree () stdout t;
          flush stdout
        end
      else
        info `Important "RESULT OK"
    with
    | Nog.Parse_error i ->
        if !Opt.line then
          info `Important "PARSE ERROR AT CHARACTER %d IN LINE %d OF FILE %s" i !line fn
        else
          begin
            info `Important "PARSE ERROR IN FILE %s AT CHARACTER %d" fn i;
            exit 1
          end
  in
  if !Opt.line then
    with_file_input fn
      begin fun ic ->
        try
          while true do
            let u = input_line ic in
            incr line;
            treat u
          done
        with
        | End_of_file -> ()
      end
  else
    treat (Driver.read_file fn)
;;
(* ***)
(*** parse_file *)
let parse_file peg fn =
  (*let peg' =
    List.map
      begin fun (name, pe) ->
        (name,
          Peg.map_over_n
            begin fun name' ->
              ref (Unresolved name')
            end
            pe)
      end
      peg
  in
  let resolve r =
    match !r with
    | Resolved(_, x) -> x
    | Unresolved name -> raise (Error(sf "Unresolved non-terminal %S" name))
  in
  List.iter
    begin fun (_, pe') ->
      Peg.iter_over_n
        begin fun r ->
          match !r with
          | Resolved(_, _) -> ()
          | Unresolved name' -> r := Resolved(name', List.assoc name' peg')
        end
        pe'
    end
    peg';*)
  let start = List.assoc !Opt.start peg in
  let count = ref 0 in
  let treat v =
    try
      let (q, i, t) = Peg.descent (fun x -> List.assoc x peg) start v in
      if q then
        begin
          info `Normal "%05d RESULT OK" !count;
          Peg.print_tree () stdout (Peg.Node(!Opt.root_node,[],[t]))
        end
      else
        info `Normal "%05d RESULT PREFIX %d" !count i
    with
    | Peg.Fail -> info `Normal "%05d RESULT NOPREFIX" !count
  in
  if !Opt.line then
    with_file_input fn
      begin fun ic ->
          begin
            try
              while true do
                incr count;
                flush stdout;
                let v = input_line ic in
                treat v
              done
            with
            | End_of_file -> info `Normal "EOF"
          end
      end
  else
    treat (Driver.read_file fn)
;;
(* ***)
(*** process *)
let process fno =
  banner "Aurochs 0.5";

  let peg =
    lazy begin
      match fno with
      | None -> raise (Error "No grammar file specified")
      | Some fn ->
          let peg = load_grammar fn in
          info `Normal "Grammar loaded from file %s" fn;
          peg
    end
  in

  let base_name =
    lazy begin
      match !Opt.base with
      | None ->
          begin
            match fno with
            | None -> raise (Error "Specify base name with -base <name>")
            | Some fn -> Filename.chop_extension fn
          end
      | Some fn' -> fn'
    end
  in

  begin
    match !Opt.dump_grammar with
    | None -> ()
    | Some fn ->
        info `Normal "Dumping grammar to file %s" fn;
        with_file_output fn (fun oc -> Pretty.print_grammar oc (Lazy.force peg))
  end;

  let peg_canonified =
    lazy begin
      info `Minor "Checking grammar";
      let results = Check.check_grammar !Opt.start (Lazy.force peg) in
      let fail = ref false in
      List.iter
        begin function
          | `Warning u -> warn `Normal "Peg: %s" u
          | `Error u   -> error "Peg: %s" u; fail := true
          | `Info u    -> info `Minor "Peg: %s" u
        end
        results;

      if !fail then raise (Error "Invalid grammar");

      info `Minor "Canonifying grammar";
      let peg = Canonify.canonify_grammar ~start:!Opt.start (Lazy.force peg) in
      begin
        match !Opt.dump_canonified with
        | None -> ()
        | Some fn ->
            info `Normal "Dumping canonified grammar to file %s" fn;
            with_file_output fn (fun oc -> Pretty.print_grammar oc peg)
      end;
      peg
    end
  in

  let nog =
    lazy begin
      match !Opt.load_nog with
      | None ->
          info `Minor "Generating NOG code";
          Noggie.generate (Lazy.force base_name) ~start:!Opt.start (Lazy.force peg_canonified)
      | Some fn -> with_file_input fn (fun ic -> Marshal.from_channel ic)
    end
  in

  begin
    match !Opt.dump_nog with
    | None -> ()
    | Some fn ->
        info `Normal "Dumping NOG code to %s" fn;
        with_file_output fn (fun oc -> Noggie.print_code oc (Lazy.force nog).Nog.pg_code)
  end;

  begin
    match !Opt.save_nog with
    | None -> ()
    | Some fn ->
        info `Normal "Saving NOG program to %s" fn;
        with_file_output fn (fun oc -> Marshal.to_channel oc (Lazy.force nog) [])
  end;

  begin
    match !Opt.parse with
    | [] -> info `Normal "No file to parse."
    | something ->
        List.iter
          begin fun fn' ->
            if !Opt.parse_with_nog then
              begin
                info `Normal "Parsing file %s using Nog interpreter" fn';
                let pg = Lazy.force nog in
                parse_file_with_nog pg fn'
              end
            else
              begin
                info `Normal "Parsing file %s using expression interpreter" fn';
                parse_file (Lazy.force peg) fn'
              end
          end
          (List.rev something)
  end;

  if !Opt.generate then
    begin
      info `Minor "Generating parser";
      let fn' = Lazy.force base_name in
      match !Opt.target with
      | `ml ->
          let pg = Lazy.force nog in
          Camelus.generate fn' !Opt.start (Lazy.force peg) pg
      | `c ->
          Ritchie.generate fn' ~start:!Opt.start (Lazy.force peg_canonified)
      | `nog -> Noggie.save_program (fn'^".nog") (Lazy.force nog) (Lazy.force peg)
      | `amd64 ->
          let pg = Lazy.force nog in
          with_file_output (fn'^".s")
            begin fun oc ->
              Amd64.emit oc pg
            end
    end
;;
(* ***)
