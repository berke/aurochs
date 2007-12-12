(* Rgb *)
(* Copyright (C)2000-2006 Berke Durak                               *)
(* Released under the GNU Lesser General Public License version 2.1 *)

type t
val mix : float -> t -> t -> t
val add : t -> t -> t
val white : t
val red : t
val green : t
val blue : t
val yellow : t
val cyan : t
val magenta : t
val black : t
val to_string : t -> string
