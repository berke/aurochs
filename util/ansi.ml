(* Ansi *)

open Pffpsf;;

let black   = 0;;
let red     = 1;;
let green   = 2;;
let yellow  = 3;;
let blue    = 4;;
let magenta = 5;;
let cyan    = 6;;
let white   = 7;;

let foreground = [|
  "\027[30m";
  "\027[31m";
  "\027[32m";
  "\027[33m";
  "\027[34m";
  "\027[35m";
  "\027[36m";
  "\027[37m";
|];;

let background = [|
  "\027[40m";
  "\027[41m";
  "\027[42m";
  "\027[43m";
  "\027[44m";
  "\027[45m";
  "\027[46m";
  "\027[47m";
|];;

let move_to oc x = fp oc "\r\027[%dC\027[K" x
let up = "\027[1A"
let ceol = "\027[K"
let home = "\r"
let none = "\027[0m"
;;
