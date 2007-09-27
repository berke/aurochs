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

let _ = load_opcodes();;
