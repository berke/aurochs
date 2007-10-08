(* Peg *)

open Pffpsf;;

(*** types *)
type char_set =
  | One of char
  | Many of char list
  | Range of char * char
;;

type match_options =
  | Exact
  | Case_insensitive
;;

type 'a pe =
  | Epsilon
  | Position
  | Tokenize of 'a pe
  | Ascribe of string * 'a pe
  | A of string (* Atom *)
  | Ax of string * match_options
  | C of char_set Boolean.t
  | N of 'a (* Nonterminal *)
  | S of 'a pe list (* Sequence *)
  | Build of string * 'a pe list
  | Or of 'a pe list (* Ordered alternative *)
  | And of 'a pe
  | Not of 'a pe
  | Opt of 'a pe (* Option *)
  | Star of 'a pe (* Star *)
  | Plus of 'a pe
  | BOF
  | EOF
;;

type proto_tree =
  | Empty_
  | Node_ of string * proto_tree list
  | Attribute_ of string * string
  | Token_ of string
  | Pseudo_ of proto_tree list
;;

type ('node, 'attribute) poly_positioned_tree =
  | P_Node of int * int * 'node * (int * int * 'attribute) list * ('node, 'attribute) poly_positioned_tree list (* Node (name, attributes_list, children *)
  | P_Token of int * int
;;

type ('node, 'attribute) poly_tree =
  | Node of 'node * ('attribute * string) list * ('node, 'attribute) poly_tree list (* Node (name, attributes_list, children *)
  | Token of string
;;

let rec iter_over_poly_tree_nodes f = function
  | Node(nd, _, xl) -> f nd; List.iter (iter_over_poly_tree_nodes f) xl
  | Token _ -> ()
;;

let rec iter_over_poly_tree_attributes f = function
  | Node(_, al, xl) -> List.iter (fun (a, _) -> f a) al; List.iter (iter_over_poly_tree_attributes f) xl
  | Token _ -> ()
;;

type tree = (string, string) poly_tree;;

type 'a result =
  | Failure
  | Success of 'a
  | Busy
;;

exception Fail;;
exception Not_implemented;;
exception Error of string;;
(* ***)
let sub u i j = String.sub u i (j - i);;

(*** rec *)
let rec relativize u = function
  | P_Node(_, _, n, al, xl) ->
      Node(n,
        List.map (fun (i, j, a) -> (a, if j < i then string_of_int i else sub u i j)) al,
        List.map (relativize u) xl)
  | P_Token(i, j) -> Token(sub u i j)
;;
(* ***)
(*** iter_over_n *)
let rec iter_over_n f = function
  | N x -> f x
  | Epsilon|EOF|BOF|Position|A _|Ax _|C _ -> ()
  | Tokenize x|Ascribe(_,x)|And x|Not x|Opt x|Star x|Plus x -> iter_over_n f x
  | S xl|Build(_, xl)|Or xl -> List.iter (iter_over_n f) xl
;;
(* ***)
(*** map_over_n *)
let rec map_over_n f = function
  | N x -> N(f x)
  | Epsilon -> Epsilon
  | Position -> Position
  | EOF -> EOF
  | BOF -> BOF
  | A u -> A u
  | Ax(u, o) -> Ax(u, o)
  | C c -> C c
  | Tokenize x -> Tokenize(map_over_n f x)
  | Ascribe(n, x) -> Ascribe(n, map_over_n f x)
  | And x -> And(map_over_n f x)
  | Not x -> Not(map_over_n f x)
  | Opt x -> Opt(map_over_n f x)
  | Star x -> Star(map_over_n f x)
  | Plus x -> Plus(map_over_n f x)
  | S xl -> S(List.map (map_over_n f) xl)
  | Build(n, xl) -> Build(n, List.map (map_over_n f) xl)
  | Or xl -> Or(List.map (map_over_n f) xl)
;;
(* ***)
(*** iter_over_builds *)
let rec iter_over_builds f = function
  | N _ | Epsilon | Position | EOF | BOF | A _ | Ax _ | C _ -> ()
  | And x | Not x | Opt x | Star x | Plus x | Tokenize x | Ascribe(_, x) -> iter_over_builds f x
  | Build(n, xl) -> f n; List.iter (iter_over_builds f) xl
  | S xl | Or xl -> List.iter (iter_over_builds f) xl
;;
(* ***)
(*** iter_over_attributes *)
let rec iter_over_attributes f = function
  | N _ | Epsilon | Position | EOF | BOF | A _ | Ax _ | C _ -> ()
  | And x | Not x | Opt x | Star x | Plus x | Tokenize x -> iter_over_attributes f x
  | Ascribe(a, x) -> f a; iter_over_attributes f x
  | Build(_, xl) | S xl | Or xl -> List.iter (iter_over_attributes f) xl
;;
(* ***)

module SS = Set.Make(String);;
module SM = Map.Make(String);;

type trool =
| Unknown
| True
| False
;;

let ( ||| ) a b = match (a, b) with
  | (Unknown, x)|(x, Unknown) -> x
  | (True,_)|(_,True) -> True
  | (_,_) -> False
;;

let print_trool oc = function
| True -> fp oc "true"
| False -> fp oc "false"
| Unknown -> fp oc "unknown"
;;
 
(*** rec *)
(* MUST BE CANONIFIED *)
let compute_active_terminals peg =
  (* Compute produceability graph *)
  let genitors = ref SM.empty in

  (* x may generate y *)
  let is_genitor y x =
    let children =
      try
        SM.find x !genitors
      with
      | Not_found -> SS.empty
    in
    genitors := SM.add x (SS.add y children) !genitors
  in

  List.iter
    begin fun (n, x) ->
      let rec loop = function
        | N n' -> is_genitor n n'
        | Epsilon | Position | EOF | BOF | A _ | Ax _ | C _ -> ()
        | And x | Not x | Opt x | Star x | Plus x | Tokenize x | Ascribe(_, x) -> loop x
        | Build(_, xl) | S xl | Or xl -> List.iter loop xl
      in
      loop x
    end
    peg;

  (* Compute reachability *)
  let actives = ref SS.empty in

  let rec mark busy n =
    if SS.mem n busy then
      () (* Already being marked *)
    else
      begin
        actives := SS.add n !actives;
        let g = 
          try
            SM.find n !genitors
          with
          | Not_found -> SS.empty
        in
        let busy' = SS.add n busy in
        SS.iter
          begin fun n' ->
            if not (SS.mem n' busy) && not (SS.mem n' !actives) then
              mark busy' n'
          end
          g
      end
  in
  
  let rec active_root = function
    | N n' -> false
    | Epsilon | Position | EOF | BOF | A _ | Ax _ | C _ -> false
    | And x | Not x | Opt x | Star x | Plus x -> active_root x
    | Tokenize _ | Ascribe(_, _) | Build(_, _)-> true
    | S xl | Or xl -> List.exists active_root xl
  in

  List.iter (fun (n, x) -> if active_root x then mark SS.empty n) peg;

  !actives
;;
(* ***)

let fp = Printf.fprintf;;
let sf = Printf.sprintf;;

(*** print_indent *)
let print_indent oc d =
  for i = 1 to d do
    fp oc "  "
  done
;;
(* ***)
(*** print_tree, print_tree_list *)
let print_string oc x = fp oc "%s" x;;
let rec print_poly_tree ?(depth=0) ?(short=false) ~print_node ~print_attribute () oc t =
  match t with
  | Node(n, al, (_::_ as l)) ->
      if short then fp oc "<%a" print_node n else fp oc "%a<%a" print_indent depth print_node n;
      List.iter
        begin fun (name, value) ->
          fp oc " %a=%S" print_attribute name value
        end
        al;
      if short then fp oc ">" else fp oc ">\n";
      print_poly_tree_list ~depth:(depth + 1) ~short ~print_node ~print_attribute () oc l;
      if short then
        fp oc "</%a>" print_node n
      else
        fp oc "%a</%a>\n" print_indent depth print_node n
  | Node(n, al, []) ->
      if short then fp oc "<%a" print_node n else fp oc "%a<%a" print_indent depth print_node n;
      List.iter
        begin fun (name, value) ->
          fp oc " %a=%S" print_attribute name value
        end
        al;
      if short then fp oc "/>" else fp oc "/>\n"
  | Token u ->
      if short then
        fp oc "%s" u
      else
        fp oc "%a%s\n" print_indent depth u
and print_poly_tree_list ?(depth=0) ?(short=false) ~print_node ~print_attribute () oc l =
  List.iter
    begin fun t ->
      print_poly_tree ~depth ~short ~print_node ~print_attribute () oc t
    end
    l
;;
let print_tree ?depth ?short () oc t = print_poly_tree ?depth ?short ~print_node:print_string ~print_attribute:print_string () oc t;;
let print_tree_list ?depth ?short () oc l = print_poly_tree_list ?depth ?short ~print_node:print_string ~print_attribute:print_string () oc l;;
(* ***)
(*** collect *)
exception Bad_tree;;

let collect t =
  let rec do_collector n attributes children = function
    | [] -> (attributes, children)
    | x :: rest ->
        match x with
        | Node_(n', l') ->
            let (attributes', children') = do_collector n' [] [] l' in
            do_collector n attributes (Node(n', List.rev attributes', List.rev children') :: children) rest
        | Token_ u -> do_collector n attributes ((Token u) :: children) rest
        | Empty_ -> do_collector n attributes children rest
        | Attribute_(name, value) -> do_collector n ((name, value) :: attributes) children rest
        | Pseudo_ l ->
            let (attributes', children') = do_collector n attributes children l in
            do_collector n attributes' children' rest
  and do_tree = function
  | Empty_ -> Node("*Empty", [], [])
  | Attribute_(n, v) -> Node("*Attribute", [n,v], [])
  | Pseudo_ l -> Node("*Pseudo", [], List.map do_tree l)
  | Node_(n, children) ->
      let (attributes, children') = do_collector n [] [] children in
      Node(n, List.rev attributes, List.rev children')
  | Token_ u -> Token u
  in
  do_tree t
;;
(* ***)
(*** is_factor *)
let is_factor v u i =
  let m = String.length u
  and n = String.length v
  in
  0 <= i && i < m && n <= m - i &&
  let rec loop j =
    j = n or
    u.[i + j] = v.[j] && loop (j + 1)
  in
  loop 0
;;
(* ***)
(*** extract_token, extract_token_from_list *)
let rec extract_token = function
  | (Node_(_, l)|Pseudo_ l) -> extract_token_from_list l
  | Token_ u -> u
  | _ -> raise Fail
and extract_token_from_list = function
  | [] -> raise Fail
  | x :: rest ->
      match x with
      | Token_ u -> u
      | (Pseudo_ l|Node_(_, l)) ->
          begin
            try
              extract_token_from_list l
            with
            | Fail -> extract_token_from_list rest
          end
      | _ -> extract_token_from_list rest
;;
(* ***)
(*** process *)
let process resolve peg u =
  let m = String.length u in
  let cache = Hashtbl.create 1009 in
  let rec loop i pe =
    if Hashtbl.mem cache (i, pe) then
      begin
        match Hashtbl.find cache (i, pe) with
        | Failure -> raise Fail
        | Busy -> raise Fail
        | Success x -> x
      end
    else
      begin
        Hashtbl.replace cache (i, pe) Busy;
        let y =
          try
            Success(
              match pe with
              | Epsilon | Position -> (i, Empty_)
              | BOF ->
                  if i = 0 then
                    (i, Empty_)
                  else
                    raise Fail
              | EOF ->
                  if i = m then
                    (i, Empty_)
                  else
                    raise Fail
              | Tokenize pe ->
                  let (j, s) = loop i pe in
                  (j, Token_(String.sub u i (j - i)))
              | Ascribe(n, Position) ->
                  (i, Attribute_(n, Printf.sprintf "%d" i))
              | Ascribe(n, pe) ->
                  if false then
                    begin
                      let (j, s) = loop i pe in
                      let v = extract_token s in
                      (j, Attribute_(n, v))
                    end
                  else
                    begin
                      let (j, _) = loop i pe in
                      (j, Attribute_(n, String.sub u i (j - i)))
                    end
              | C f ->
                  if i < m &&
                    begin
                      let c = u.[i] in
                      Boolean.eval
                        begin function
                          | One d -> c = d
                          | Many dl -> List.mem c dl
                          | Range(d1,d2) -> d1 <= c && c <= d2
                        end
                        f
                    end
                  then
                    (i + 1, Empty_)
                  else
                    raise Fail
              | Ax(v, Case_insensitive) ->
                  let n = String.length v in
                  if is_factor (String.lowercase v) (String.lowercase u) i then
                    (i + n, Empty_)
                  else
                    raise Fail
              | Ax(v, Exact) | A v ->
                  let n = String.length v in
                  if is_factor v u i then
                    (i + n, Empty_)
                  else
                    raise Fail
              | N n -> loop i (resolve n)
              | S l ->
                  let (i, sl) =
                    List.fold_left
                      begin fun (i, sl) pe ->
                        let (i, s) = loop i pe in
                        (i, s :: sl)
                      end
                      (i, [])
                      l
                  in
                  (i, Pseudo_(List.rev sl))
              | Build(n, l) ->
                  let (i, sl) =
                    List.fold_left
                      begin fun (i, sl) pe ->
                        let (i, s) = loop i pe in
                        (i, s :: sl)
                      end
                      (i, [])
                      l
                  in
                  (i, Node_(n, List.rev sl))
              | Or l ->
                  begin
                    let rec scan = function
                      | [] -> raise Fail
                      | pe :: rest ->
                          try
                            loop i pe
                          with
                          | Fail -> scan rest
                    in
                    scan l
                  end
              | And pe ->
                  let _ = loop i pe in
                  (i, Empty_)
              | Not pe ->
                  begin
                    if
                      try
                        let _ = loop i pe in
                        true
                      with
                      | Fail -> false
                    then
                      raise Fail
                    else
                      (i, Empty_)
                  end
              | Opt pe ->
                  begin
                    try
                      loop i pe
                    with
                    | Fail -> (i, Empty_)
                  end
              | Star pe ->
                  begin
                    let rec scan i sl =
                      try
                        let (i', s) = loop i pe in
                        if i = i' then
                          (i, sl)
                        else
                          scan i' (s :: sl)
                      with
                      | Fail -> (i, sl)
                    in
                    let (i, sl) = scan i [] in
                    (i, Pseudo_(List.rev sl))
                  end
              | Plus pe ->
                  begin
                    let rec scan i sl =
                      try
                        let (i', s) = loop i pe in
                        if i = i' then
                          (i, sl)
                        else
                          scan i' (s :: sl)
                      with
                      | Fail -> (i, sl)
                    in
                    let (i, s) = loop i pe in
                    let (i, sl) = scan i [s] in
                    (i, Pseudo_(List.rev sl))
                  end
            )
          with
          | Fail -> Failure
        in
        Hashtbl.add cache (i, pe) y;
        match y with
        | Failure|Busy -> raise Fail
        | Success x -> x
      end
  in
  loop 0 peg, cache
;;
(* ***)
(*** descent *)
let descent resolve peg u =
  let ((i, s), cache) = process resolve peg u in
  let success = i = String.length u in
  if success then
    (true, i, collect s)
  else
    begin
      let max_i = ref 0 in
      Hashtbl.iter
        begin fun (i, _) y ->
          match y with
          | Success(i, _) -> max_i := max i !max_i
          | _ -> ()
        end
        cache;
      Printf.eprintf "Parse error at character %d.\n%!" !max_i;
      (false, i, collect s)
    end
;;
(* ***)
