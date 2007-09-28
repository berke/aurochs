(* Pack *)

open Bytes;;
open Int_operators;;

type packer = Bytes.sink;;
(*** write_byte *)
let write_byte sk x = Bytes.put_byte sk x;;
(* ***)
(*** write_int64 *)
let write_int64 sk x =
  let (negative, x) =
    if x < 0L then
      (true, 0L ---- (x ++++ 1L))
    else
      (false, x)
  in
  let a = Array.make 10 0 in
  let i = ref 0 in
  let emit x = a.(!i) <- x; incr i in
  let rec loop x =
    if x <= 0x3fL then
      (* Head byte *)
      emit ((if negative then 0x40 else 0x00) lor (???? x))
    else
      begin
        (* Inner byte *)
        emit (???? (x &&&& 0x7fL));
        loop (x >>>> 7)
      end
  in
  loop x;
  for j = !i - 1 downto 0 do
    if j = 0 then
      write_byte sk (0x80 lor a.(j))
    else
      write_byte sk a.(j)
  done
;;
(* ***)
(*** write_uint64 *)
let write_uint64 sk x =
  let a = Array.make 10 0 in
  let i = ref 0 in
  let emit x = a.(!i) <- x; incr i in
  let rec loop x =
    if 0L <= x && x <= 0x7fL then
      (* Head byte *)
      emit (???? x)
    else
      begin
        (* Inner byte *)
        emit (???? (x &&&& 0x7fL));
        loop (x >>>> 7)
      end
  in
  loop x;
  for j = !i - 1 downto 0 do
    if j = 0 then
      write_byte sk (0x80 lor a.(j))
    else
      write_byte sk a.(j)
  done
;;
(* ***)
(*** write_string *)
let write_string sk u =
  let m = String.length u in
  write_uint64 sk (!!!! m);
  for i = 0 to m - 1 do
    write_byte sk (Char.code u.[i])
  done
;;
(* ***)
let write_uint sk x = write_uint64 sk (!!!! x);;
(*** test *)
let test fn =
  let oc = open_out_bin fn in
  let sk = Bytes.sink_of_out_channel oc in
  let rec loop i x =
    let y = x in
    Printf.printf ">>> 0x%08Lx %Ld\n%!" y y;
    if i = 100 then
      ()
    else
      begin
        write_uint64 sk y;
        write_string sk (Printf.sprintf "0x%08Lx" y);
        loop (i + 1) ((x <<<< 4) |||| (!!!! (i land 0xf)))
      end
  in
  loop 0 0L;
  close_out oc
;;
(* ***)
