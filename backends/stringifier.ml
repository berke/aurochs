(* Stringifier *)

open Pffpsf

let print_indent oc n =
  for i = 0 to n - 1 do
    output_char oc ' '
  done

let print_ocaml_string ?(indent=4) ?(cols=32) () oc u =
  let m = String.length u in
  print_indent oc (indent - 1);
  fp oc "\"";
  let rec loop i =
    if i = m then
      fp oc "\"\n"
    else
      begin
        if i > 0 && i mod cols = 0 then
          begin
            fp oc "\\\n";
            print_indent oc indent;
          end;
        let c = Char.code u.[i] in
        fp oc "\\%03d" c;
        loop (i + 1)
      end
  in
  loop 0
