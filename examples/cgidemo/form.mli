(* Form *)
(* Copyright (C)2000-2006 Berke Durak                               *)
(* Released under the GNU Lesser General Public License version 2.1 *)

module SM :
  sig
    type key = string
    type +'a t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val find : key -> 'a t -> 'a
    val remove : key -> 'a t -> 'a t
    val mem : key -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  end
module SS :
  sig
    type elt = string
    type t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val split : elt -> t -> t * bool * t
  end
type t = SS.t SM.t
val sf : ('a, unit, string) format -> 'a
val hexadecimal : char -> int
val read_hex_encoded_char : char Stream.t -> char
val read_chunk : char Stream.t -> string * [> `Ampersand | `EOS | `Equal ]
val parse_form_from_stream : char Stream.t -> t
val parse_form_from_string : string -> t
val display_stringmapstring : string SM.t -> unit
val display_form : t -> unit
val encode_string : Buffer.t -> string -> unit
val encode_form : t -> string
val encode_form_from_list : ?buffer:Buffer.t -> (string * string list) list -> string
val get_set : t -> string -> SS.t
val get_list : t -> string -> string list
val get : t -> ?default:string -> string -> string
val get_value : t -> ?default:'b -> (string -> 'b) -> string -> 'b
val add : t -> string -> string -> t
val empty : t
