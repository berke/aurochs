(* Bytes_ *)

(** Module for byte sinks and sources. *)

(** The type of a byte sink *)
type sink

(** Add a byte to the sink *)
val put_byte : sink -> int -> unit

(** Add a string to the sink *)
val put_string : sink -> string -> unit

(** Add a 16-bit integer to the sink *)
val put_int16 : sink -> int -> unit

(** Add an integer to the sink as a 32-bit integer *)
val put_int : sink -> int -> unit

(** Add an int32 to the sink *)
val put_int32 : sink -> int32 -> unit

(** Add an int64 to the sink *)
val put_int64 : sink -> int64 -> unit

(** Create a sink from an [out_channel], which should better be a binary output channel *)
val sink_of_out_channel : out_channel -> sink

(** Create a sink that gobbles everything *)
val null_sink : sink

(** Create a sink from a [Buffer.t] *)
val sink_of_buffer : Buffer.t -> sink

(** [with_buffer_sink f] executes the function [f] on a freshly created buffer sink and returns
    the contents of the buffer. *)
val with_buffer_sink : (sink -> unit) -> string

(** [counter r s] takes an integer reference [r] and a sink [s] and creates a new sink [t]; writes to [t]
    are passed to [s], but [r] is increased by the number of bytes written to [r]. *)
val counter : int ref -> sink -> sink

(** [logger oc s] creates a sink [t]; writes to [t] are passed to [s] and logged in human-readable form on the output
    channel [oc] *)
val logger : out_channel -> sink -> sink

(** [checksum64 sum sk] creates a sink [t] that adds the bytes it receives to [sum] and passes them to [sk] *)
val checksum64 : int64 ref -> sink -> sink

(** [measure f] runs [f] on a fresh sink and returns the number of bytes [f] writes to the sink *)
val measure : (sink -> unit) -> int

(** The type of a byte source *)
type source

(** Create a source from an [in_channel], which should better be a binary input channel *)
val source_of_in_channel : in_channel -> source

(** Create a source from a string *)
val source_of_string : string -> source

(** Return the next byte from the source *)
val get_byte : source -> int

(** [input_string s n] reads [n] bytes from the source [s] and returns them as a string *)
val input_string : source -> int -> string

(** Read a 4-byte integer *)
val get_int : source -> int

(** Read a 4-byte integer as an [int32] *)
val get_int32 : source -> int32

(** Read an 8-byte integer as an [int64] *)
val get_int64 : source -> int64

(** [with_string_source u f] runs [f] on a source created from the string [u] *)
val with_string_source : string -> (source -> 'a) -> 'a
