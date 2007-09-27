(* Driver *)

open Peg;;

open Pffpsf;;

(*let process (start : peg) u =
  try
    let (q, i, t) = Peg.descent (fun x -> x) start u in
    fp stdout "%b %d\n%a\n%!" q i (Peg.print_tree ~depth:0) t
  with
  | Peg.Fail ->
      fp stdout "Does not parse.\n%!"
;;

let line_driver start =
  try
    while true do
      (*fp stdout "Query? %!";*)
      let l = input_line stdin in
      fp stdout "%s\n" l;
      process start l
    done
  with
  | End_of_file ->
      ()
      (*fp stdout "\nBye.\n%!"*)
;;*)

let read_file fn =
  let ic = open_in fn in
  let m = in_channel_length ic in
  let u = String.create m in
  really_input ic u 0 m;
  close_in ic;
  u
;;

(*let file_driver start fn =
  let u = read_file fn in
  process start u
;;*)
