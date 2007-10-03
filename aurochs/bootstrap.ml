(* Bootstrap *)

open Pffpsf;;
open Peg;;

(*** load_grammar *)
let load_grammar fn =
  let u = Driver.read_file fn in
  let m = String.length u in
  let peg = Grammar_original.peg in
  (*with_file_output "internal.peg" (fun oc -> Pretty.print_grammar oc peg);*)
  let (q, i, t) =
    Peg.descent
      (fun x -> List.assoc x peg)
       (List.assoc "start" peg) u in
  try
    if q && i = m then
      Convert_grammar_original.convert_grammar t
    else
      raise (Error(sf "Error in grammar file %S at character %d" fn (i + 1)))
  with
  | Peg.Fail -> raise (Error(sf "Cannot parse grammar file %S" fn))
;;
(* ***)

let _ =
  let fn = "syntax/grammar.peg" in
  let peg = load_grammar fn in
  let peg = Canonify.canonify_grammar ~start:!Opt.start peg in
  let nog = Noggie.generate "grammar" ~root:"Root" ~start:"start" peg in
  Camelus.generate_implementation ~pack:false "syntax/grammar" "start" peg nog
;;
