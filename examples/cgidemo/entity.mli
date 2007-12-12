(* Entity *)

val char_array_to_char_table : (char * string) array -> string array
val character_array : (char * string) array
val character_table : string array
val character_table_nl_to_br : string array
val to_buffer : string array -> Buffer.t -> string -> unit
