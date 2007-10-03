(* Aurochs *)
(* A PEG parser generator *)

open Pffpsf;;
open Util.Syntax;;
open Talk;;

module Spec =
  struct
    open Arg;;
    open Opt;;

    let aor x y = x := Some y;;

    let append x y = x := y :: !x;;

    let level x =
      Symbol(Talk_level.levels,
      begin fun u ->
        x :=
          try
            Talk_level.level_of_string u
          with
          | Not_found -> raise (Bad "Bad level")
      end)
    ;;

    let specs =
      Arg.align [
        "-target",
          Symbol(["c";"amd64";"nog";"ml";"mli";"ml_classic"],
                 begin function
                   | "c"             -> targets += `c
                   | "nog"           -> targets += `nog
                   | "amd64"         -> targets += `amd64
                   | "ml"            -> targets += `ml
                   | "ml_classic"    -> targets += `ml_classic
                   | "mli"           -> targets += `mli
                   | _               -> raise (Bad "Invalid target")
                 end),
          " target language (default c)";

        "-start",
          Set_string start,
          "<string> Set start symbol (default \"start\")";

        "-save-nog",
          String(aor save_nog),
          "<file> Save NOG into file";

        "-load-nog",
          String(aor load_nog),
          "<file> Load NOG from file";

        "-dump-grammar",
          String(aor dump_grammar),
          "<file> Dump grammar into file";

        "-dump-canonified",
          String(aor dump_canonified),
          "<file> Dump canonified form of grammar into file";

        "-dump-xml",
          String(aor dump_xml),
          "<file> Dump XML parse tree for grammar into file";

        "-dump-colorized",
          String(aor dump_colorized),
          "<file> Dump ANSI-colorized parse tree into file";

        "-colorize-background",
          Set colorize_background,
          " Colorize background in addition to foreground";

        "-no-tree",
          Clear Opt.tree,
          " Don't dump the syntax tree after parsing";

        "-bootstrap",
          Set bootstrap,
          " Bootstrap grammar";

        "-dump-nog",
          String(aor dump_nog),
          "<file> Dump NOG bytecode into file";

        "-build-only",
          Set build_only,
          " Don't generate parsing code, only building code";

        "-show-memo",
          Set show_memo,
          " Show memo table after parsing";

        "-nog",
          Set parse_with_nog,
          " Parse by interpreting Nog code (default)";

        "-expi",
          Clear parse_with_nog,
          " Parse by direct interpretation";

        "-parse",
          String(append parse),
          "<file> Parse the given file using the grammar and dump the XML parse tree on stdout";

        "-parse-list",
          String(fun fn -> Util.with_file_input fn (fun ic -> Util.iter_over_lines ic (fun u -> append parse u))),
          "<file> Add a -parse <fn> option for each line in given file";

        "-trace",
          Set trace,
          " Trace execution";

        "-debug",
          Set debug,
          " Interactively debug execution";

        "-log-calls",
          String(aor log_calls),
          "<file> When running Nog code, log encountered calls and labels to given file";

        "-record",
          String(aor record),
          "<file> When running Nog code, record execution trace to given file";

        "-profile",
          String(aor profile),
          "<file> Dump Nog profiling information to file";

        "-quick",
          Set quick,
          " Execute Nog code as quickly as possible without checks";

        "-root-node",
          Set_string root_node,
          "<name> ML constructor names for the root node";

        "-base",
          String(aor base),
          "<name> Set the base for output files";

        "-function-prefix",
          Set_string function_prefix,
          "<prefix> Prepend C and assembler function names with this string";

        "-node-prefix",
          Set_string node_prefix,
          "<prefix> Prepend ML node constructor names with this string";

        "-attribute-prefix",
          Set_string attribute_prefix,
          "<prefix> Prepend ML attribute constructor names with this string";

        "-line",
          Set line,
          " Parse line-by-line instead of whole file";

        "-generate",
          Set generate,
          " Generate a parser";

        "-w",
        level warning,
        " Start reporting warnings with this severity level";

        "-i",
        level info,
        " Start reporting information with this severity level";

        "-quiet",
        Unit(fun () -> warning := `None; info := `None; error := `None),
        " Suppress all text output";

        "-version",
        Unit(fun () ->
          pf "Aurochs version V%d\n" Version.version;
          exit 0),
        " Show Aurochs version.";
      ]
    ;;
  end
;;

let _ =
  let did_something = ref false in
  Arg.parse
    Spec.specs
    (fun x -> did_something := true; Process.process (Some x))
    (sf "Usage: %s [options] <grammar-file>" (Filename.basename Sys.argv.(0)));
  if not !did_something then Process.process None
;;
