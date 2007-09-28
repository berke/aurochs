(* Pack *)

type packer = Bytes.sink;;

val write_byte : packer -> int -> unit
val write_int64 : packer -> int64 -> unit
val write_uint64 : packer -> int64 -> unit
val write_string : packer -> string -> unit
val write_int : packer -> int -> unit
val write_uint : packer -> int -> unit

val test : string -> unit
