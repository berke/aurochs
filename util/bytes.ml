(* Bytes *)

open Int_operators;;

type sink = {
  sk_put_byte : int -> unit;
  sk_put_string : string -> unit
};;

let put_byte sk b = sk.sk_put_byte b;;
let put_string sk b = sk.sk_put_string b;;

let put_int16 sk x =
  let f x = put_byte sk x in
  f ((x lsr 8) land 255);
  f (x land 255)
;;

let put_int sk x =
  let f = put_byte sk in
  f ((x asr 24) land 255);
  f ((x asr 16) land 255);
  f ((x asr 8) land 255);
  f (x land 255)
;;

let put_int32 sk x =
  let f x = put_byte sk (!!? x) in
  f ((x >>> 24) &&& 255l);
  f ((x >>> 16) &&& 255l);
  f ((x >>> 8) &&& 255l);
  f (x &&& 255l)
;;

let put_int64 sk x =
  let f x = put_byte sk (!!!? x) in
  f ((x >>>> 56) &&&& 255L);
  f ((x >>>> 48) &&&& 255L);
  f ((x >>>> 40) &&&& 255L);
  f ((x >>>> 32) &&&& 255L);
  f ((x >>>> 24) &&&& 255L);
  f ((x >>>> 16) &&&& 255L);
  f ((x >>>> 8) &&&& 255L);
  f (x &&&& 255L)
;;

let sink_of_out_channel oc =
  { sk_put_byte = output_byte oc;
    sk_put_string = output_string oc }
;;

let null_sink = { sk_put_byte = (fun _ -> ()); sk_put_string = (fun _ -> ()) };;

let sink_of_buffer b =
  { sk_put_byte = (fun x -> Buffer.add_char b (Char.chr x));
    sk_put_string = Buffer.add_string b }
;;

let with_buffer_sink f =
  let b = Buffer.create 64 in
  let sk = sink_of_buffer b in
  f sk;
  Buffer.contents b
;;

let logger oc sk =
  let fp = Printf.fprintf in
  { sk_put_byte = (fun b ->
      fp oc "  Byte %d\n%!" b;
      put_byte sk b);
    sk_put_string = (fun u ->
      fp oc "  String %S\n%!" u;
      put_string sk u) }
;;

let counter c sk =
  { sk_put_byte = (fun b ->
      incr c;
      put_byte sk b);
    sk_put_string = (fun u ->
      c := !c + String.length u;
      put_string sk u) }
;;

let measure f =
  let c = ref 0 in
  let sk = counter c null_sink in
  f sk;
  !c
;;

type source = {
  sr_get_byte : unit -> int;
  sr_input_string : int -> string
};;

let source_of_in_channel ic = {
  sr_get_byte = (fun () -> input_byte ic);
  sr_input_string = (fun m ->
    let u = String.create m in
    really_input ic u 0 m;
    u)
};;

let source_of_string u =
  let x = ref 0 in
  {
    sr_get_byte = (fun () ->
      let i = !x in
      if i = String.length u then
        raise End_of_file
      else
        begin
          x := i + 1;
          Char.code u.[i]
        end);
    sr_input_string = (fun m -> String.sub u !x m)
  }
;;

let get_byte sr = sr.sr_get_byte ();;

let input_string sr m = sr.sr_input_string m;;

let get_int sr =
  let f () = get_byte sr in
  let b3 = f () in
  let b2 = f () in
  let b1 = f () in
  let b0 = f () in
  (b3 lsl 24) lor (b2 lsl 16) lor (b1 lsl 8) lor b0
;;

let get_int32 sr =
  let f () = !!! (get_byte sr) in
  let b3 = f () in
  let b2 = f () in
  let b1 = f () in
  let b0 = f () in
  (b3 <<< 24) ||| (b2 <<< 16) ||| (b1 <<< 8) ||| b0
;;

let get_int64 sr =
  let f () = !!!! (get_byte sr) in
  let b7 = f () in
  let b6 = f () in
  let b5 = f () in
  let b4 = f () in
  let b3 = f () in
  let b2 = f () in
  let b1 = f () in
  let b0 = f () in
  (b7 <<<< 56) |||| (b6 <<<< 48) |||| (b5 <<<< 40) |||| (b4 <<<< 32) ||||
  (b3 <<<< 24) |||| (b2 <<<< 16) |||| (b1 <<<< 8) |||| b0
;;

let with_string_source u f =
  let sr = source_of_string u in
  f sr
;;
