(* Unix_util *)

val string_of_process_status : string -> Unix.process_status -> string option
val unix_really_read : Unix.file_descr -> string -> int -> int -> unit
val string_of_sockaddr : Unix.sockaddr -> string
val sockaddr_of_string : string -> Unix.sockaddr
val int64_of_inet_addr : Unix.inet_addr -> int64
val proc_get_rsz_vsz : unit -> int * int
val proc_get_free_mem : unit -> int
val string_of_iso_8601 : (int * int * int) -> string
val seconds_of_iso_8601 : (int * int * int) -> float
val iso_8601_of_string : string -> (int * int * int)
val mkdirhier : string -> unit
val today : unit -> (int * int * int)
