(* Util *)

(*** split_at *)
let split_at c u =
  let m = String.length u in
  let b = Buffer.create m in
  let rec loop0 r i =
    if i >= m then
      List.rev r
    else
      if u.[i] = c then
        loop0 r (i + 1)
      else
        loop1 r i
  and loop1 r i =
    if i = m or u.[i] = c then
      begin
        let x = Buffer.contents b in
        Buffer.clear b;
        loop0 (x::r) (i + 1)
      end
    else
      begin
        Buffer.add_char b u.[i];
        loop1 r (i + 1)
      end
  in
  loop0 [] 0
;;
(* ***)
(*** with_file_input *)
let with_file_input fn f =
  let ic = open_in fn in
  try
    f ic
  with
  | x ->
      close_in ic;
      raise x
;;
(* ***)
(*** with_file_output *)
let with_file_output fn f =
  let oc = open_out fn in
  try
    let y = f oc in
    close_out oc;
    y
  with
  | x ->
      close_out oc;
      raise x
;;
(* ***)
(*** iter_over_lines *)
let iter_over_lines fn f =
  with_file_input fn
    begin fun ic ->
      try
        while true do
          let u = input_line ic in
          f u
        done;
        assert false
      with
      | End_of_file -> ()
    end
;;
(* ***)
