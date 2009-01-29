(* Util *)
(* Copyright (C)2004-2006 Berke Durak                               *)
(* Released under the GNU Lesser General Public License version 2.1 *)

val sf : ('a, unit, string) format -> 'a
val first_line : string -> string
val limit : int -> string -> string
val limit_left : int -> string -> string
val for_all_chars : (char -> bool) -> string -> bool
val split_once_at : (char -> bool) -> string -> string * string
val is_alpha : char -> bool
val is_digit : char -> bool
val is_space : char -> bool
val parse_strings : string -> string list
val split_at : char -> string -> string list
val list_intersect : 'a list -> 'a list -> 'a list
val once : (unit -> unit) -> unit -> unit
val list_has_more_than_one_element : 'a list -> bool
val count_lines : string -> int
val first_matching_char_from : int -> (char -> bool) -> string -> int
val first_matching_char : (char -> bool) -> string -> int
val longest_matching_prefix : (char -> bool) -> string -> string * string
val remove_leading_spaces : string -> string
val delete_first_chars : int -> string -> string
val hierarchical : string -> string -> int
val wind : ('a -> 'b) -> 'a -> ('c -> 'd) -> 'c -> 'b
val list_change_nth : 'a list -> int -> 'a -> 'a list
val list_remove_nth : 'a list -> int -> 'a list
val word_wrap : out_channel -> ?columns:int -> string -> unit
val reg_of_string : string -> string
val flip_array : 'a array -> unit
val substitute_variables : (string * string) list -> string -> string
val list_sub_rev : 'a list -> int -> int -> 'a list
val is_prefix : string -> string -> bool
val remove_prefix : string -> string -> string
val is_suffix : string -> string -> bool
val remove_suffix : string -> string -> string
val lowercase_compare : string -> string -> bool
val call_if : ('a -> unit) option -> 'a -> unit
val optional : ('a -> unit) -> 'a option -> unit
val mandatory : 'a option -> 'a
val wrap : 'a -> ('a -> 'b) -> ('a -> 'c) -> 'c
val binary_search : ('a -> 'a -> int) -> 'a array -> 'a -> int
val randomize : 'a array -> unit
val array_mem_assoc : 'a -> ('a * 'b) array -> bool
val array_assoc : 'a -> ('a * 'b) array -> 'b
val inside : string -> ('a -> 'b) -> 'a -> 'b
val catch : (unit -> 'a) -> 'a
val sanitize_filename : ?is_safe:(char -> bool) -> ?buffer:Buffer.t -> ?prefix:string -> string -> string
val unsanitize_filename : ?buffer:Buffer.t -> ?prefix:string -> string -> string
val with_file_input : string -> (in_channel -> 'a) -> 'a
val with_binary_file_input : string -> (in_channel -> 'a) -> 'a
val with_file_output : string -> (out_channel -> 'a) -> 'a
val with_binary_file_output : string -> (out_channel -> 'a) -> 'a
val save : string -> 'a -> unit
val load : string -> 'a
val iter_over_lines : in_channel -> (string -> unit) -> unit
val unsigned_int64_of_decimal : string -> int64
val read_file : string -> string

module Syntax :
  sig
    val pf : ('a, out_channel, unit) format -> 'a
    val fp : out_channel -> ('a, out_channel, unit) format -> 'a
    val sf : ('a, unit, string) format -> 'a
    val bf : Buffer.t -> ('a, Buffer.t, unit) format -> 'a
    val ef : ('a, out_channel, unit) format -> 'a
    val kf : (out_channel -> 'a) -> out_channel -> ('b, out_channel, unit, 'a) format4 -> 'b
    val ksf : (string -> 'a) -> ('b, unit, string, 'a) format4 -> 'b
    val ipf : 'a -> ('b, 'a, unit) format -> 'b

    val (&) : ('a -> 'b) -> 'a -> 'b
    val (+=) : 'a list ref -> 'a -> unit
    val (|>) : 'a -> ('a -> 'b) -> 'b
    val (<?) : 'a -> 'b -> 'a
  end
