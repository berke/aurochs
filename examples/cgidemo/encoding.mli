(* Encoding *)
(* Copyright (C)2000-2006 Berke Durak                               *)
(* Released under the GNU Lesser General Public License version 2.1 *)

exception Error of string
type spec = (string option * float) list
val accepted_encodings : string -> spec
val quality : spec -> string -> float
