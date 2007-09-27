(* Talk *)

open Pffpsf;;
open Talk_level;;

exception Error of string;;

(*** formatter *)
let formatter condition oc prefix severity fmt =
  if is_sublevel !condition severity then
    begin
      fp oc "%s" prefix;
      kf (fun oc -> fp oc "\n%!") oc fmt
    end
  else
    ipf oc fmt
;;
(* ***)

let warn  level  fmt = formatter Opt.warning stdout "Warning: " level   fmt;;
let info  level  fmt = formatter Opt.info    stdout "  - "      level   fmt;;
let error        fmt = formatter Opt.error   stderr "ERROR: "   `Vital  fmt;;
let banner       fmt = formatter Opt.info    stdout ""          `Normal fmt;;

let die fmt = ksf (fun u -> raise (Error u)) fmt;;

(*** plural *)
let plural oc n = if n = 1 then () else fp oc "s";;
(* ***)
