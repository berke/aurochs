(* Cgi-bin *)

open Aurochs_pack;;

let _ =
  let peg = Sys.argv.(1) in
  for i = 2 to Array.length Sys.argv - 1 do
    begin
      let fn = Sys.argv.(i) in
      Printf.printf "%s\n%!" fn;
      try
        let t = Aurochs.read_positioned ~grammar:(`Binary(`File peg)) ~text:(`File fn) in
        let fn' = (Filename.chop_extension (Filename.basename fn))^".bin" in
        Util.with_binary_file_output fn' (fun oc -> Marshal.to_channel oc t []);
      with
      | Aurochs.Parse_error n -> Printf.printf "%s: parse error at %d\n%!" fn n
    end;
    Gc.compact ()
  done
;;
