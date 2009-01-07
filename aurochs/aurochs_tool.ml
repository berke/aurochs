(* Aurochs *)
(* A PEG parser generator *)

open Pffpsf
open Util.Syntax
open Talk

module Spec =
  struct
    open Arg
    open Opt

    let aor x y = x := Some y

    let append x y = x := y :: !x

    let level x =
      Symbol(Talk_level.levels,
      begin fun u ->
        x :=
          try
            Talk_level.level_of_string u
          with
          | Not_found -> raise (Bad "Bad level")
      end)

    let specs =
      Arg.align [
        "-target",
        Symbol(["nog";"ml";"mli";"ml_classic";"c"],
                 begin function
                   | "nog"           -> targets += `nog
                   | "ml"            -> targets += `ml
                   | "mli"           -> targets += `mli
                   | "ml_classic"    -> targets += `ml_classic
                   | "c"             -> targets += `c
                   | _               -> raise (Bad "Invalid target")
                 end),
          " target language";

        "-start",
          Set_string start,
          "<string> Set start symbol (default \"start\")";

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

        "-no-tree",
          Clear Opt.tree,
          " Don't dump the syntax tree after parsing";

        "-dont-indent",
          Clear Opt.indent,
          " Don't indent the dumper tree";

        "-terse",
          Set terse,
          " Dump tree in terse format";

        "-parse",
          String(append parse),
          "<file> Parse the given file using the grammar and dump the XML parse tree on stdout";

        "-parse-list",
          String(fun fn -> Util.with_file_input fn (fun ic -> Util.iter_over_lines ic (fun u -> append parse u))),
          "<file> Add a -parse <fn> option for each line in given file";

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


        "-dump-nog",
          String(aor dump_nog),
          "<file> Dump NOG bytecode into file";

        "-build-only",
          Set build_only,
          " Don't generate parsing code, only building code";

        "-bootstrap",
          Set bootstrap,
          " Bootstrap grammar";

        "-show-memo",
          Set show_memo,
          " Show memo table after parsing";

        "-using",
        Symbol(["nog";"mlnog";"exp"],
                 begin function
                   | "nog"           -> interpreter := `nog
                   | "mlnog"         -> interpreter := `mlnog
                   | "exp"           -> interpreter := `exp
                   | _               -> raise (Bad "Invalid interpreter")
                 end),
          " interpreter to use (default nog)";

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

        "-line",
          Set line,
          " Parse line-by-line instead of whole file";

        "-quiet",
          Unit(fun () -> warning := `None; info := `None; error := `None),
          " Suppress all text output";

        "-w",
          level warning,
          " Start reporting warnings with this severity level";

        "-i",
          level info,
          " Start reporting information with this severity level";

        "-version",
          Unit(fun () ->
            let (v1,v2,v3) = Version.version in
            pf "Aurochs version %d.%d.%d\n" v1 v2 v3;
            exit 0),
          " Show Aurochs version.";
      ]
    
  end

let _ =
  let did_something = ref false in
  Arg.parse
    Spec.specs
    (fun x -> did_something := true; Process.process (Some x))
    (sf "Usage: %s [options] <grammar-file>" (Filename.basename Sys.argv.(0)));
  if not !did_something then Process.process None
