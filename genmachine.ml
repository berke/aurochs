(* Genmachine *)

open Util.Syntax;;
open Pffpsf;;

type arg =
| Int
| Attribute
| Node
| Char
| Label
;;

let arg_of_char = function
| 'i' -> Int
| 'a' -> Attribute
| 'n' -> Node
| 'c' -> Char
| 'l' -> Label
| c -> invalid_arg (sf "Unknown attribute %C" c)
;;

let load_opcodes =
  let rex1 = Str.regexp "^.*%opcode{\\([0-9]+\\)\\([a-z]*\\)}.*| *\\([A-Z_]+\\) .*$" in
  fun () ->
  let result = ref [] in
  Util.with_file_input "nog/machine.ml" (fun ic -> Util.iter_over_lines ic (fun u ->
    if Str.string_match rex1 u 0 then
      begin
        let f i = Str.matched_group i u in
        let opcode = int_of_string & f 1
        and flags = f 2
        and name = f 3
        in
        let m = String.length flags in
        let flags' = Array.init m (fun i -> arg_of_char flags.[i]) in
        result += (name, opcode, flags');
        Printf.printf ">> 0x%02x %S %S\n" opcode flags name
      end
  ));
  !result
;;

let gen_ocaml_packer ops fn =
  let string_of_arg i = sf "a%d" i in
  Util.with_file_output fn (fun oc ->
    fp oc "(* %s *)\n" fn;
    fp oc "(* Auto-generated; do not edit. *)\n";
    fp oc "\n";
    fp oc "open Machine;\n";
    fp oc "\n";
    fp oc "let pack_instruction ~resolve pk = function\n";
    List.iter
      begin fun (name, opcode, args) ->
        fp oc "  | %s" name;
        let args = Array.to_list args in
        begin match args with
          | [] -> fp oc " "
          | _ ->
              let i = ref 0 in
              fp oc "(%s) " (String.concat ", " (List.map (fun _ -> incr i; string_of_arg !i) args))
        end;
        fp oc "->\n";
        fp oc "      Pack.write_uint pk 0x%02x;\n" opcode;
        let i = ref 0 in
        List.iter
          begin fun x ->
            incr i;
            match x with
            | Int ->            fp oc "      Pack.write_uint pk %s;\n" (string_of_arg !i)
            | Char ->           fp oc "      Pack.write_uint pk (Char.code %s);\n" (string_of_arg !i)
            | Node|Attribute -> fp oc "      Pack.write_string pk %s;\n" (string_of_arg !i)
            | Label ->          fp oc "      Pack.write_uint pk (resolve %s);\n" (string_of_arg !i)
          end
          args;
        fp oc "      ()\n"
      end
      ops;
    fp oc "  | _ -> ()\n";
    fp oc ";;\n")
;;

let _ =
  let ops = load_opcodes() in
  gen_ocaml_packer ops "packer.ml"
;;
