(* Compiler *)

open Aurochs;;

(*** compile *)
let compile ?(start="start") ?(base="") ?(root="root") ?(check=true) u =
  let peg = Convert_grammar.convert_grammar (Grammar.parse u) in
  if check then
    begin
      let results = Check.check_grammar start peg in
      List.iter
        begin function
          | `Warning _|`Info _ -> ()
          | `Error u   -> raise(Compile_error u)
        end
        results;
    end;
  let peg_canonified = Canonify.canonify_grammar ~start peg in
  let pg = Noggie.generate_code ~start ~root peg_canonified in
  Bytes.with_buffer_sink (Noggie.put_program pg peg)
;;
(* ***)

let _ = compiler := compile;;
