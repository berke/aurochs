(* Canonify *)
(* For ease of recording choices made in alternative productions,
 * we limit the number of disjunctive groups in a non-terminal to one.
 * Hence, a non-terminal is either disjunctive or it contains no disjunctions.
 * The choice made for a disjunctive non-terminal at a particular position can
 * thus be recorder by an integer.
 *)

open Peg;;
open Pffpsf;;

exception Error of string;;

(*** remove_actions *)
let rec remove_actions = function
  | (N _|A _|Ax _|C _|Epsilon|Position|EOF|BOF) as t -> t
  | Tokenize x -> remove_actions x
  | Ascribe(_, x) -> remove_actions x
  | And x -> And(remove_actions x)
  | Not x -> Not(remove_actions x)
  | Opt x -> Opt(remove_actions x)
  | Star x -> Star(remove_actions x)
  | Plus x -> Plus(remove_actions x)
  | S xl -> S(List.map (remove_actions) xl)
  | Build(_, xl) -> S(List.map (remove_actions) xl)
  | Or xl -> Or(List.map (remove_actions) xl)
;;
(* ***)
(*** canonify_grammar *)
let canonify_grammar ?start g =
  let result = ref [] in
  let prefix = ref "top" in
  let gensym =
    let counter = ref 0 in
    fun () ->
      incr counter;
      sf "%s__%d" !prefix !counter
  in
  let rec collect_or result = function
    | [] -> result
    | (Or xl) :: rest ->
        let result' = collect_or result xl in
        collect_or result' rest
    | a :: rest ->
        collect_or (a :: result) rest
  in
  let table = Hashtbl.create 1009 in
  let symbols = Hashtbl.create 1009 in
  let bind suggested_name x =
    try
      Hashtbl.find table x
    with
    | Not_found ->
        Hashtbl.add table x suggested_name;
        Hashtbl.add symbols suggested_name x;
        result := (suggested_name, x) :: !result;
        suggested_name
  in
  let rec transform top = function
    | (N _|A _|Ax _|C _|Epsilon|Position|EOF|BOF) as x -> x
    | Tokenize x ->
        if top then
          Tokenize(transform false x)
        else
          begin
            let x' = transform true (Tokenize x) in
            let nt = gensym () in
            let nt' = bind nt x' in
            N nt'
          end
    | Ascribe(n, x) ->
        if top then
          Ascribe(n, transform false x)
        else
          begin
            let x' = transform true (Ascribe(n, x)) in
            let nt = gensym () in
            let nt' = bind nt x' in
            N nt'
          end
    | And x -> And(transform false x)
    | Not x -> Not(transform false x)
    | Opt x -> transform false (Or[transform false x; Epsilon])
    | Star x ->
        (* x* -> temp ; temp ::= x temp | epsilon ; *)
        let x' = transform false x in
        let temp = gensym () in
        let temp' = bind temp (transform top (Or[S[x'; N temp]; Epsilon])) in
        N temp'
        (* Star(transform false x)*) (* XXX *)
    | Plus x ->
        (* x+ -> temp ; temp ::= x temp | x ; *)
        let x' = transform false x in
        let temp = gensym () in
        let temp' = bind temp (transform top (Or[S[x'; N temp]; x'])) in
        N temp'
        (*Plus(transform false x)*) (* XXX *)
    | S xl -> S(List.map (transform false) xl)
    | Build(n, xl) -> Build(n, List.map (transform false) xl)
    | Or xl ->
        begin
          match List.rev(collect_or [] xl) with
          | [x] -> transform top x
          | xl ->
              (* Top ? *)
              if top then
                begin
                  Or(List.map (transform false) xl)
                end
              else
                begin
                  (* Not at top, create a new non-terminal *)
                  let x' = Or(List.map (transform true) xl) in
                  let nt = gensym () in
                  let nt' = bind nt x' in
                  N nt'
                end
        end
  in
  List.iter
    begin fun (name, pe) ->
      prefix := name;
      let pe' = transform true pe in
      result := (name, pe') :: !result
    end
    g;
  let module SS = Set.Make(String) in
  let resolution = Hashtbl.create 1009 in
  let rec resolve busy name =
    if SS.mem name busy then
      raise (Error(sf "Symbol %S has invalid recursion" name))
    else
      begin
        try
          Hashtbl.find resolution name
        with
        | Not_found ->
          try
            match List.assoc name !result with
            | N name' ->
                let name'' = resolve (SS.add name busy) name' in
                Hashtbl.add resolution name name'';
                name''
            | _ -> name
          with
          | Not_found -> raise (Error(sf "Cannot find symbol %S" name))
      end
  in
  let result' =
    List.fold_left
      begin fun acc (name, pe) ->
        let name' = resolve SS.empty name in
        if name = name' then
          (name, pe) :: acc
        else
          acc
      end
      []
      !result
  in
  let result'' =
    List.map
      begin fun (name, pe) ->
        (name, Peg.map_over_n
          (fun n ->
            try
              Hashtbl.find resolution n
            with
            | Not_found -> n) pe)
      end
      result'
  in
  begin
    match start with
    | None -> result''
    | Some n ->
        try
          let n' = resolve SS.empty n in
          if n <> n' then
            (n, N n') :: result''
          else
            result''
        with
        | Not_found -> result''
  end
;;
(* ***)
