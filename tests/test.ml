(* Test *)

open Seq;;
open Machine;;

let pf = Printf.printf;;
let fp = Printf.fprintf;;

(*let _ =
  let fn = Sys.argv.(1)
  in
  let peg = Process.load_grammar fn in
  let (pg, lb) = Noggie.generate fn peg in
  let start_pc = Nog.SM.find start lb in
  try
    while true do
      let u = input_line stdin in
      let r =
        if Array.length Sys.argv > 2 && Sys.argv.(2) = "trace" then
            Nog.execute ~pc:start_pc ~trace:true ~interactive:true pg u
          else
            Nog.execute ~pc:start_pc pg u
      in
      Printf.printf "RESULT: %b\n%!" r
    done
  with
  | End_of_file -> ()
;;*)
