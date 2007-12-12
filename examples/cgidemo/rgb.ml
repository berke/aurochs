(* Rgb *)
(* Copyright (C)2000-2006 Berke Durak                               *)
(* Released under the GNU Lesser General Public License version 2.1 *)

type t = float * float * float

let mix alpha (r1,g1,b1) (r2,g2,b2) =
  let f x y = (1.0 -. alpha) *. x +. alpha *. y
  in
  (f r1 r2, f g1 g2, f b1 b2)

let add (r1,g1,b1) (r2,g2,b2) =
  let g x = if x > 1.0 then 1.0 else x in
  let f x y = g (x +. y)
  in
  (f r1 r2, f g1 g2, f b1 b2)

let white = (1.0,1.0,1.0)
let red   = (1.0,0.0,0.0)
let green = (0.0,1.0,0.0)
let blue  = (0.0,0.0,1.0)
let yellow = add red green
let cyan = add green blue
let magenta = add red blue
let black = (0.0,0.0,0.0)

let to_string (r,g,b) =
  let f x = int_of_float (255.0 *. x)
  in
  Printf.sprintf "#%02x%02x%02x" (f r) (f g) (f b)
