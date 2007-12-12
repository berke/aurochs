(* Entity *)

let char_array_to_char_table a =
  let b = Array.init 256 (fun i -> String.make 1 (Char.chr i)) in
  for i = 0 to Array.length a - 1 do
    let (c,s) = a.(i) in
    b.(Char.code c) <- "&"^s^";"
  done;
  b
;;
    
(*** character_array *)
let character_array =
  [|
    '\038', "amp";
    '\062', "gt";
    '\060', "lt";
    '\034', "quot";
  |];;
(* ***)

let character_table = char_array_to_char_table character_array
;;

let character_table_nl_to_br =
  let a = Array.copy character_table in
  a.(Char.code '\n') <- "<BR>\n";
  a
;;

let to_buffer table b u =
  for i = 0 to String.length u - 1 do
    Buffer.add_string b table.(Char.code u.[i])
  done
;;
