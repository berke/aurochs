(* Check *)

open Peg;;
open Pffpsf;;

module B = Boolean;;

exception Error of string;;

let check_grammar start g =
  let results = ref [] in
  let error x   = results := (`Error x)   :: !results in
  let warning x = results := (`Warning x) :: !results in
  let inform x  = results := (`Info x)    :: !results in
  let module SS = Set.Make(String) in
  let ns_seen = ref SS.empty in
  let ns_def  = ref SS.empty in
  List.iter
    begin fun (name, pe) ->
      ns_def := SS.add name !ns_def;
      Peg.iter_over_n
        begin fun n ->
          ns_seen := SS.add n !ns_seen
        end
        pe
    end
    g;
  inform (sf "Number of non-terminals referenced: %d" (SS.cardinal !ns_def));
  SS.iter
    begin fun name ->
      error (sf "Non-terminal %S used but not defined." name)
    end
    (SS.diff !ns_seen !ns_def);
  (* Reachability analysis *)
  let rec transitive past n =
    let pe =
      try
        List.assoc n g
      with
      | Not_found -> raise (Error(sf "Production %S not found" n))
    in
    let past' = ref (SS.add n past) in
    Peg.iter_over_n
      begin fun n' ->
        if SS.mem n' !past' then
          ()
        else
          begin
            past' := transitive !past' n'
          end
      end
      pe;
    !past'
  in
  let reachable = transitive SS.empty start in
  SS.iter
    begin fun name ->
      warning (sf "Non-terminal %S is not reachable." name)
    end
    (SS.diff !ns_seen reachable);
  List.rev !results
;;
