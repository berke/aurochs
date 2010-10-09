(* Ocamlbuild plugin *)

open Ocamlbuild_pack
open Ocamlbuild_plugin
open Command
open Ocaml_specific
open Outcome

let ( & ) f x = f x

let ocaml_local_dir =
  lazy begin
    try
      Sys.getenv "OCAMLDIR"
    with
    | Not_found ->
        let cmd = "ocamlc -where" in
        My_unix.run_and_open cmd (fun ic ->
          Log.dprintf 5 "Getting Ocaml directory from command %s" cmd;
          input_line ic)
  end

let cflags =
  lazy begin
    try
      let fl = Sys.getenv "CFLAGS" in
      let flags = Lexers.comma_or_blank_sep_strings (Lexing.from_string fl) in
      S(List.concat (List.map (fun fl -> [A"-ccopt"; A fl]) flags))
    with
    | Not_found -> S[]
  end


let local dir = Filename.concat (Lazy.force ocaml_local_dir) dir

type libdep_description = {
  ld_name : string;
  ld_have_lib : string option;
  ld_lib : string;
  ld_dir : string;
  ld_include : string;
  ld_static : bool;
  ld_c_headers : string list
}

type ocaml_lib_description = {
  od_path : string;
  od_name : string;
  od_headers : string list;
  od_incdirs : string list
}

let system_lib_dir = "/usr/lib"

let zlib_description = {
  ld_name = "zlib";
  ld_have_lib = Some"-DHAVE_ZLIB";
  ld_lib = "-lz";
  ld_dir = "-L"^system_lib_dir;
  ld_include = "-L/usr/include";
  ld_c_headers = [];
  ld_static = true
}

let aurochslib_description = {
  ld_name = "aurochslib";
  ld_have_lib = Some"-DHAVE_AUROCHSLIB";
  ld_lib = "-l";
  ld_dir = "-L"^system_lib_dir;
  ld_include = "-L/usr/include";
  ld_c_headers = [];
  ld_static = true
}

let aurochs_lib_description = {
  od_path = ""; (* (Lazy.force ocaml_local_dir)^"/aurochs_lib/";*)
  od_name = "aurochs";
  od_headers = [ "include/nog.h";
                 "include/peg.h";
                 "include/peg_lib.h";
                 "include/parse_tree.h";
                 "include/pack.h";
                 "include/staloc.h";
                 "include/pushdown.h";
                 "include/alloc.h";
                 "include/parse_tree.h";
                 "include/base_types.h";
                 "include/aurochs.h" ];
  od_incdirs = [ "include"; "_build/include" ];
}

(*** ocamllib *)
let ocamllib old =
   let u = old.od_name in
   (* X is an ocaml library.
      This will declare use_X and include_X *)
   ocaml_lib u;

   flag ["compile"; "c"; "cstuff"] &
     S[A"-ccopt";A"-Wall";
       Lazy.force cflags;
       A"-verbose";
       S(List.map(fun x -> S[A"-ccopt";A("-I"^x)]) old.od_incdirs);
     ];

   flag ["compile"; "c"; "cstuff"; "debug"] & S[A"-ccopt";A"-g"];
   flag ["compile"; "c"; "cstuff"; "optimize"] & S[A"-ccopt";A"-O3"];

   flag ["link"; "library"; "ocaml"; "byte"; "use_"^u]
   (S[A"-dllpath";A(old.od_path);A"-dllib";A("-l"^u); (*A"-cclib";A("-L"^old.od_path);*)A"-cclib";A("-l"^u)]);

   flag ["link"; "ocaml"; "byte"; "use_"^u]
   (S[A"-dllpath";A(old.od_path);A"-dllib";A("-l"^u); (*A"-cclib";A("-L"^old.od_path);*)A"-cclib";A("-l"^u)]);

   flag ["link"; "library"; "ocaml"; "native"; "use_lib"^u]
        (S[(*A"-cclib";A("-L"^old.od_path);*)A"-cclib"; A("-l"^u)]);

   dep  ["link"; "ocaml"; "use_lib"^u] [old.od_path^"lib"^u^".a"];
   dep  ["compile"; "c"] old.od_headers
;;

(* ***)

dispatch
  begin function
  | After_options ->
      begin
        Options.hygiene := true;
        Options.sanitize := true;
        Options.ocamlopt := A"ocamlopt";
        Options.ocamlc := A"ocamlc";
        Options.ocamldep := A"ocamldep";
        Options.ocamlyacc := A"menhir";
        Options.include_dirs :=
          ["front";
           "backends";
           "aurochs";
           "syntax";
           "util";
           "pack";
           "peg";
           "examples";
           "nog"] @ !Options.include_dirs
      end
  | After_rules ->
      begin
        List.iter
          begin fun dir ->
            flag ["ocaml"; "link"]    (S[A"-I"; A dir]);
            flag ["ocaml"; "compile"] (S[A"-I"; A dir]);
            flag ["ocaml"; "doc"]     (S[A"-I"; A dir])
          end
          [(* local "java";
              local "compiler" *)];

        Log.dprintf 5 "Ready";

        ocamllib aurochs_lib_description;

        rule "Generation"
          ~prods:["c/nog_unpack.c";"include/nog_unpack.h";"backends/nog_packer.ml"]
          ~deps:["genmachine.byte";"nog/machine.ml"]
          begin fun env _build ->
            Seq[
              Cmd(S[A"mkdir"; A"-p"; A"include"]);
              Cmd(S[A"./genmachine.byte"])
            ]
          end;
          
        rule "Program"
          ~prods:["c/test/test_nog"]
          ~deps:["c/test/test_nog.o"; "libaurochs.a"]
          begin fun env _build ->
            Cmd(S[A"gcc"; A"c/test/test_nog.o"; A"-L."; A"-laurochs"; A"-o"; A"c/test/test_nog"])
          end;

        rule "Bootstrap"
          ~prods:["syntax/grammar.ml";"syntax/grammar.mli"]
          ~deps:["aurochs/bootstrap.native";"syntax/grammar.peg"]
          begin fun env _build ->
            Cmd(S[A"aurochs/bootstrap.native"])
          end;

        (*rule "aurochs: .peg -> .ml,.mli"
          ~prods:["%.ml";"%.mli"]
          ~deps:["%.peg"; "aurochs/aurochs_tool.native"]
            begin fun env _build ->
              let peg = env "%.peg" and ml = env "%.ml" in
              let tags = tags_of_pathname ml++"aurochs" in
              Cmd(S[A"aurochs/aurochs_tool.native"; T tags; P peg])
            end*)
      end
  | _ -> ()
  end
