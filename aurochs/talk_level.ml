(* Talk_level *)

type level = [`All|`Debug|`Time|`Minor|`Normal|`Important|`Vital|`None];;

let int_of_level = function
  | `All       -> 0
  | `Debug     -> 1
  | `Minor     -> 2
  | `Time      -> 3
  | `Normal    -> 4
  | `Important -> 5
  | `Vital     -> 6
  | `None      -> 7
;;

let is_sublevel l1 l2 = int_of_level l1 <= int_of_level l2;;

let level_to_string_map =
  [ "all",       `All;
    "debug",     `Debug;
    "minor",     `Minor;
    "time",      `Time;
    "normal",    `Normal;
    "important", `Important;
    "vital",     `Vital;
    "none",      `None ]
;;

let levels = List.map fst level_to_string_map;;

let level_of_string u = List.assoc u level_to_string_map;;
