(* Ocamlbuild plugin *)

open Ocamlbuild_pack;;
open Ocamlbuild_plugin;;
open Command;;
open Ocaml_specific;;
open Outcome;;

let aurochs = ref (S[A"aurochs";A"-target";A"ml";A"-generate"]);;
let system_lib_dir = "/usr/lib";;

dispatch
  begin function
  | After_rules ->
      begin
        List.iter
          begin fun dir ->
            flag ["ocaml"; "link"]    (S[A"-I"; A dir]);
            flag ["ocaml"; "compile"] (S[A"-I"; A dir]);
            flag ["ocaml"; "doc"]     (S[A"-I"; A dir])
          end
          ["+aurochs_lib"];

        ocaml_lib ~extern:true "aurochs_lib";

        flag ["link"; "ocaml"; "byte"; "use_libaurochs"]
             (S[A"-dllib";A("-laurochs"); A"-cclib";A("-laurochs")]);

        flag ["link"; "ocaml"; "use_libaurochs"]
             (S[A"-ccopt"; A("-L"^system_lib_dir); A"-cclib"; A"-laurochs"]);

        rule "aurochs: .peg -> .ml,.mli"

        ~prods:["%.ml";"%.mli"]

        ~dep:"%.peg"
          begin fun env _build ->
            let peg = env "%.peg" and ml = env "%.ml" in
            let tags = tags_of_pathname ml++"aurochs" in
            Cmd(S[!aurochs; T tags; P peg])
          end
      end
  | _ -> ()
  end
;;
