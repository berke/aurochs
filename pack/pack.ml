(* Pack *)

type packer = {
  p_oc : out_channel;
}

let ( &&& ) = Int64.logand;;
let ( ||| ) = Int64.logor;;
let ( >>> ) = Int64.shift_right_logical;;
let ( <<< ) = Int64.shift_left;;
let ( !!! ) = Int64.of_int;;
let ( ??? ) = Int64.to_int;;
let ( --- ) = Int64.sub;;
let ( +++ ) = Int64.add;;

let from_out_channel oc = { p_oc = oc };;

(*** write_byte *)
let write_byte pk x = output_byte pk.p_oc x;;
(* ***)
(*** write_int64 *)
let write_int64 pk x =
  let (negative, x) =
    if x < 0L then
      (true, 0L --- (x +++ 1L))
    else
      (false, x)
  in
  let a = Array.make 10 0 in
  let i = ref 0 in
  let emit x = a.(!i) <- x; incr i in
  let rec loop x =
    if x <= 0x3fL then
      (* Head byte *)
      emit ((if negative then 0x40 else 0x00) lor (??? x))
    else
      begin
        (* Inner byte *)
        emit (??? (x &&& 0x7fL));
        loop (x >>> 7)
      end
  in
  loop x;
  for j = !i - 1 downto 0 do
    if j = 0 then
      write_byte pk (0x80 lor a.(j))
    else
      write_byte pk a.(j)
  done
;;
(* ***)
(*** write_uint64 *)
let write_uint64 pk x =
  let a = Array.make 10 0 in
  let i = ref 0 in
  let emit x = a.(!i) <- x; incr i in
  let rec loop x =
    if 0L <= x && x <= 0x7fL then
      (* Head byte *)
      emit (??? x)
    else
      begin
        (* Inner byte *)
        emit (??? (x &&& 0x7fL));
        loop (x >>> 7)
      end
  in
  loop x;
  for j = !i - 1 downto 0 do
    if j = 0 then
      write_byte pk (0x80 lor a.(j))
    else
      write_byte pk a.(j)
  done
;;
(* ***)
(*** write_string *)
let write_string pk u =
  let m = String.length u in
  write_uint64 pk (!!! m);
  for i = 0 to m - 1 do
    write_byte pk (Char.code u.[i])
  done
;;
(* ***)
let write_uint pk x = write_uint64 pk (!!! x);;
(*** test *)
let test fn =
  let oc = open_out_bin fn in
  let pk = from_out_channel oc in
  let rec loop i x =
    let y = x in
    Printf.printf ">>> 0x%08Lx %Ld\n%!" y y;
    if i = 100 then
      ()
    else
      begin
        write_uint64 pk y;
        write_string pk (Printf.sprintf "0x%08Lx" y);
        loop (i + 1) ((x <<< 4) ||| (!!! (i land 0xf)))
      end
  in
  loop 0 0L;
  close_out oc
;;
(* ***)
