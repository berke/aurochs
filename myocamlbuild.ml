(* Ocamlbuild plugin *)

open Ocamlbuild_pack;;
open Ocamlbuild_plugin;;
open Command;;
open Ocaml_specific;;
open Outcome;;

let ( & ) f x = f x;;

module Config_common =
  struct
    let ocamldir_default = "/udir/durak/lib/ocaml";;
  end
;;

module Config32 =
  struct
    include Config_common
    let bits64 = false;;
  end
;;

module Config64 =
  struct
    include Config_common
    let bits64 = true;;
  end
;;

open Config64;;

let ocaml_local_dir =
  try
    Sys.getenv "OCAMLDIR"
  with
  | Not_found -> ocamldir_default
;;

let local dir = Filename.concat ocaml_local_dir dir;;

type libdep_description = {
  ld_name : string;
  ld_have_lib : string option;
  ld_lib : string;
  ld_dir : string;
  ld_include : string;
  ld_static : bool;
  ld_c_headers : string list
};;

type ocaml_lib_description = {
  od_path : string;
  od_name : string;
  od_headers : string list;
  od_incdirs : string list
};;

let system_lib_dir =
  if bits64 then
    "/usr/lib64"
  else
    "/usr/lib"
;;

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

(*** clibdep *)
let clibdep ld =
   (* When one makes a C library that use the zlib with ocamlmklib,
      then issue these flags. *)
   flag ["ocamlmklib"; "c"; "use_"^ld.ld_name]
        (S[A ld.ld_dir; A ld.ld_lib]);

   (* When one compiles C code using the zlib *)
   flag ["c"; "compile"; "include_"^ld.ld_name]
        (S[A"-ccopt"; A ld.ld_include;
           begin match ld.ld_have_lib with
             | None -> N
             | Some x -> S[A"-ccopt"; A x]
           end]);

   flag ["link"; "ocaml"; "library"; "use_"^ld.ld_name]
        (S[A"-ccopt"; A ld.ld_dir; A"-cclib"; A ld.ld_lib]);

   (* If `static' is true then every ocaml link in bytecode will add -custom *)
   if ld.ld_static then flag ["link"; "ocaml"; "byte"] (A"-custom");
;;
(* ***)

let aurochs_lib_description = {
  od_path = "";
  od_name = "aurochs";
  od_headers = [ "cnog/cnog.h"; "cnog/peg.h"; "cnog/peg_tree.h"; "cpack/pack.h"; "cpeglib/parse_tree.h"; "include/base_types.h" ];
  od_incdirs = [ "cnog"; "cpack"; "cpeglib"; "include" ];
}

(*** ocamllib *)
let ocamllib old =
   let u = old.od_name in
   (* X is an ocaml library.
      This will declare use_X and include_X *)
   ocaml_lib u;

   flag ["compile"; "c"; "cstuff"] &
     S[A"-ccopt";A"-Wall";
       S(List.map(fun x -> S[A"-ccopt";A("-I"^x)]) old.od_incdirs);
     ];

   flag ["compile"; "c"; "cstuff"; "debug"] & S[A"-ccopt";A"-g"];
   flag ["compile"; "c"; "cstuff"] & S[A"-ccopt";A"-O3"];

   flag ["link"; "library"; "ocaml"; "byte"; "use_"^u]
   (S[A"-dllpath";A(old.od_path);A"-dllib";A("-l"^u); A"-cclib";A("-L"^old.od_path);A"-cclib";A("-l"^u)]);

   flag ["link"; "ocaml"; "byte"; "use_"^u]
   (S[A"-dllpath";A(old.od_path);A"-dllib";A("-l"^u); A"-cclib";A("-L"^old.od_path);A"-cclib";A("-l"^u)]);

   flag ["link"; "library"; "ocaml"; "native"; "use_lib"^u]
        (S[A"-cclib";A("-L"^old.od_path);A"-cclib"; A("-l"^u)]);

   (* When ocaml link something that use the libX
      then one need that file to be up to date. *)
   dep  ["link"; "ocaml"; "use_lib"^u] [old.od_path^"lib"^u^".a"];

   (* As an approximation all our C files use the headers.
      Note: This will import headers in the build directory. *)
   dep  ["compile"; "c"] old.od_headers;
;;
(* ***)

dispatch
  begin function
  | After_options ->
      begin
        Options.hygiene := true;
        Options.sanitize := true;
        Options.ocamlopt := A"ocamlopt.opt";
        Options.ocamlc := A"ocamlc.opt";
        Options.ocamldep := A"ocamldep.opt";
        Options.ocamlyacc := A "menhir";
        Options.include_dirs :=
          ["front";
           "backends";
           "aurochs";
           "syntax";
           "util";
           "peg";
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

        (*ocaml_lib "java";
        ocaml_lib "compiler";*)
        (*ocaml_lib "float32";*)

        Log.dprintf 5 "Ready";

        ocamllib aurochs_lib_description;
        (*flag ["ocaml"; "byte"; "library"; "float32"] (S[A"-Lfloat32";A"-lfloat32"]);*)
        (*flag ["ocaml"; "native"; "program"; "float32"] (S[A"-cclib"; A"float32/dllfloat32.so"]); WORKS *)
        (*flag ["ocaml"; "native"; "program"; "float32"] (S[A"-verbose";A"-ccopt";A"-Lfloat32";A"-cclib"; A"float32/dllfloat32.so"]);*)
        (*flag ["ocaml"; "link"; "native"; "use_float32"] (S[A"-cclib";A"-Lfloat32";A"-cclib"; A"-lfloat32"]);
        flag ["ocaml"; "link"; "byte"; "use_float32"] (S[A"-dllpath";A"float32";A"-dllib"; A"-lfloat32"]);*)
        (*dep ["use_float32"] ["float32/libfloat32.so"];*)
        (*dep ["file:astivore/astivore.native"] ["float32/dllfloat32.so"];*)


        rule "LaTeX to PDF conversion rule"
          ~prods:["cnog/cnog_unpack.c";"cnog/cnog_unpack.h";"nog/nog_packer.ml"]
          ~deps:["genmachine.byte";"nog/machine.ml"]
          begin fun env _build ->
            Cmd(S[A"./genmachine.byte"])
          end;
      end
  | _ -> ()
  end
;;
