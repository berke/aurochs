(* Int_operators *)

open Int32;;

let ( >>> ) = shift_right_logical;;
let ( <<< ) = shift_left;;
let ( ||| ) = logor;;
let ( ^^^ ) = logxor;;
let ( &&& ) = logand;;
let ( !!! ) = of_int;;
let ( !!? ) = to_int;; (* Deprecated *)
let ( ??? ) = to_int;;
let ( --- ) = sub;;
let ( +++ ) = add;;
let ( >>> ) = shift_right_logical;;

open Int64;;

let ( >>>> ) = shift_right_logical;;
let ( <<<< ) = shift_left;;
let ( |||| ) = logor;;
let ( ^^^^ ) = logxor;;
let ( &&&& ) = logand;;
let ( !!!! ) = of_int;;
let ( !!!? ) = to_int;; (* Deprecated *)
let ( ???? ) = to_int;;
let ( ---- ) = sub;;
let ( ++++ ) = add;;
let ( >>>> ) = shift_right_logical;;
