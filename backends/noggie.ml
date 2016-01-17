(* Noggie *)

open Peg
open Seq
open Machine
open Nog
open Pffpsf
open Talk
open Util.Syntax

module B = Boolean
module SS = Set.Make(String)

exception Inside of string * exn

(*** register_complexity *)
let rec register_complexity = function
  | B.False|B.True|B.Atom _ -> 1
  | B.Not x -> register_complexity x
  | B.And l|B.Or l ->
      let (n', arity) =
        List.fold_left
          begin fun (n, arity) t ->
            let n' = register_complexity t in
            (max n n', arity + 1)
          end
          (0, 0)
          l
      in
      n' + arity - 1

(* ***)
(*** generate_code *)
let generate_code ~root ~start peg =
  (*let peg = Canonify.canonify_grammar peg in*)
  let m = List.length peg in
  let num_alternatives = ref 0 in
  let num_productions = ref 0 in
  let build_name n = "__build_"^n in
  (*** Number productions and alternatives *)
  let productions = Hashtbl.create (2 * m) in
  let gensym =
    let gs = ref 0 in
    fun prefix ->
      incr gs;
      sf "__%s_%d" prefix !gs
  in
  let productions_array = ref []
  and choices_array = ref []
  in
  let tables, tables_list = Hashtbl.create 100, ref [] in
  let register_table t =
    try
      Hashtbl.find tables t
    with
    | Not_found ->
        tables_list += t;
        let id = Hashtbl.length tables in
        Hashtbl.add tables t id;
        id
  in

  let lowercase_table_id = lazy (register_table lowercase_table) in

  begin
    List.iter
      begin function (name, pe) ->
        let number = !num_productions in
        incr num_productions;
        let choice_number =
          match pe with
          | Or _ ->
              let id = !num_alternatives in
              incr num_alternatives;
              choices_array := name :: !choices_array;
              Some id
          | _ -> None
        in
        productions_array := name :: !productions_array;
        Hashtbl.replace productions name (number, choice_number)
      end
      peg;
  end;
  (* ***)
  (*** compile_boolean *)
  let compile_boolean be =
    let rec boolean = function
      | B.Atom cs -> char_set cs
      | B.True|B.And[] -> It(U(BTRUE))
      | B.False|B.Or[] -> It(U(BFALSE))
      | B.And[b]|B.Or[b] -> boolean b
      | B.Not b -> Seq[boolean b; It(U(BNOT))]
      | B.And([b1;b2]) ->
         let n1 = register_complexity b1
         and n2 = register_complexity b2
         in
         let (b1,b2) = if n1 < n2 then (b2,b1) else (b1,b2) in
         Seq[
           boolean b1;
           boolean b2;
           It(U(BAND))]
      | B.And(b1 :: b2 :: b3 :: brest) ->
          let b' =
            List.fold_left
              begin fun bx by ->
                B.And[bx; by]
              end
              (B.And[b1; b2])
              (b3 :: brest)
          in
          boolean b'
      | B.Or([b1;b2]) ->
         let n1 = register_complexity b1
         and n2 = register_complexity b2
         in
         let (b1,b2) = if n1 < n2 then (b2,b1) else (b1,b2) in
         Seq[
           boolean b1;
           boolean b2;
           It(U(BOR))]
      | B.Or(b1 :: b2 :: b3 :: brest) ->
          let b' =
            List.fold_left
              begin fun bx by ->
                B.Or[bx; by]
              end
              (B.Or[b1; b2])
              (b3 :: brest)
          in
          boolean b'
    and char_set = function
    | One c -> It(U(SSEQ c))
    | Many cl -> boolean (B.Or(List.map (fun c -> B.Atom(One c)) cl))
    | Range(c1,c2) -> It(U(SSIR(c1, c2)))
    in
    boolean be
  in
  (* ***)
  (*** compile_pexpr *)
  let rec compile_pexpr ?choice_number ~fail x =
    match x with
    | Epsilon | Position | Constant _ -> Seq[]
    | EOF -> It(L(fail, BNEOF))
    | BOF -> It(L(fail, BNBOF))
    | Tokenize x|Ascribe(_, x) -> compile_pexpr ?choice_number ~fail x (* Ignore tokenization/ascriptions *)
    | Build(_, xl) -> compile_pexpr ?choice_number ~fail (S xl)
    | Ax(v, Case_insensitive) ->
        begin
          let m = String.length v in
          let l = ref [] in
          l := (L(fail, BSLLT m)) :: !l;
          for i = 0 to m - 1 do
            l += U(TSSEQ(Lazy.force lowercase_table_id, Char.code (Char.lowercase v.[i])));
            l += L(fail, BBRC);
            l += U(RIGHT 1)
          done;
          Lst(List.rev !l)
        end
    | Ax(v, Exact) | A v ->
        begin
          let m = String.length v in
          let l = ref [] in
          l := (L(fail, BSLLT m)) :: !l;
          for i = 0 to m - 1 do
            l := (L(fail, SBNS(v.[i]))) :: !l
          done;
          Lst(List.rev !l)
        end
    | C c ->
      (*let sq = compile_boolean c in*)
      let sq = compile_boolean c in
      Seq[It(L(fail, BSLLT 1));
          sq;
          It(U (RIGHT 1));
          It(L(fail, BBRC))]
    | N n ->
      Lst[L(n, JSR);
          L(fail, BFS)]
    | S xl ->
        Seq(
          List.map
            begin fun x ->
              compile_pexpr ~fail x
            end
            xl)
    | Not x ->
       let my_accept = gensym "negate" in
       Seq[
         It(U PUSHP);
         compile_pexpr ~fail:my_accept x;
         It(U DROPP);
         It(L(fail, BRA));
         It(L(my_accept, LABEL));
         It(U POPP);
         It(U CLRF)]
    | Or xl ->
        let choice_number' =
          match choice_number with
          | None -> invalid_arg "Unremoved inner disjunction in gexpr"
          | Some n -> n
        in
        let my_accept : string = gensym "accept" in
        (*let finally = gensym "finally" in*)
        Seq[
          (*It(U PUSHP);*) (* SAVED BY FUNCTION PRELUDE *)
          Seq(
            let n = List.length xl in
            let position = ref 0 in
            (* List.fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b *)
            begin
              let (sq, _) =
                List.fold_right
                  begin fun x (sq, next) ->
                    let label = gensym "next" in
                    (Seq[
                          It(L(label, LABEL));
                          if !position <> n then It(U RESTP) else empty;
                          (let _ = incr position in
                          compile_pexpr ~fail:next x);
                          (*It(L(fail, BFS));*)

                          It(U(TOPSTCH(choice_number', n - 1 - !position)));
                          It(L(my_accept, BRA)) ] :: sq,
                     label)
                  end
                  xl
                  ([], fail (* finally *))
              in
              sq
            end);
          (*It(L(finally, LABEL));
          It(U DROPP);
          It(L(fail, BRA));*)

          It(L(my_accept, LABEL));
          It(U CLRF);
          (*It(U DROPP);*)
        ]
    | And x ->
       let my_fail = gensym "and_reject" in
       let my_accept = gensym "and_accept" in
       Seq[
         It(U PUSHP);
         compile_pexpr ~fail:my_fail x;
         It(U POPP);
         It(L(my_accept, BRA));

         It(L(my_fail, LABEL));
         It(U DROPP);
         It(L(fail, BRA));

         It(L(my_accept, LABEL));
       ]
    | _ -> invalid_arg "Something not implemented"
  in
  (* ***)
  (*** Generate parser function bodies *)
  let parse_code =
    List.map
      begin fun (name, expr) ->
        try
          let (number, choice_number) = Hashtbl.find productions name in
          let fail_label = name^"__fail" in
          let popping_fail_label = name^"__pop_and_fail" in
          let recompute_label = name^"__recompute" in
          (name, 
            Seq[ Lst[
                   L(name, LABEL);

                   (* Check memo... *)
                   U(LDMEM number);
                   (*L(fail_label, BMF);
                   L(fail_label, BB);*)
                   L(fail_label, BMBF);
                   L(recompute_label, BMUK);
                   U JMEM;
                   U CLRF;
                   U RTS];

                 Lst[
                   L(recompute_label, LABEL);
                   U(STMEMB number);
                   U PUSHP]; (* Save position *)

                 compile_pexpr ?choice_number ~fail:popping_fail_label expr;

                 Lst[U(POPSTMEMJ number);
                     U CLRF;
                     U RTS];

                 (* Could be factorized *)
                 Lst[L(popping_fail_label, LABEL);
                     U POPP;
                     L(fail_label, LABEL);
                     U(STMEMF number);
                     U SETF;
                     U RTS] ])
         with
         | x -> raise (Inside(name, x))
      end
      peg
  in
  (* ***)
  (*** Generate builder function bodies *)
  (* Compute the set of non-terminals that build, ascribe or tokenize something. *)
  let have_action_set = Peg.compute_active_terminals peg in
  (*** build_names *)
  let rec bexpr ?number ?choice_number = function
    | BOF | EOF | Position | Epsilon | And _ | Not _ | Constant _ -> empty
    | Opt _ | Star _ | Plus _ -> invalid_arg "Unremoved internal Opt, Star or Plus"
    | Ax(u, _) | A u -> It(U(RIGHT (String.length u)))
    | C _ -> It(U(RIGHT 1))
    | N n ->
        (*if SS.mem n have_action_set then*)
          It(L(build_name n, JSR))
        (*else
          empty*)
    | S xl -> Seq(List.map bexpr xl)
    | Build(n, xl) ->
        Seq[
          It(U (SNODE n));
          Seq(List.map bexpr xl);
          It(U FNODE)
        ]
    | Ascribe(n, Constant u) -> It(U(STRATTR(n, u)))
    | Ascribe(n, Position) -> It(U(POSATTR n))
    | Ascribe(n, x) ->
        let number' =
          match number with
          | None -> invalid_arg "Unremoved inner ascription"
          | Some k -> k
        in
        Lst[U(LDMEM number');
            U(ATTR n)]
    | Tokenize x ->
        let number' =
          match number with
          | None -> invalid_arg "Unremoved inner token"
          | Some k -> k
        in
        Lst[U(LDMEM number');
            U TOKEN]
    | Or xl ->
        begin
          let choice_number' =
            match choice_number with
            | None -> invalid_arg "Unremoved inner disjunction in gexpr"
            | Some n -> n
          in
          let xa = Array.of_list xl in
          let a = Array.init (Array.length xa) (fun i -> gensym (sf "choice_%d" i)) in
          let label_continue = gensym "continue" in
          Seq[
            It(U(LDCH choice_number'));
            It(M(a, SWCH));
            Seq(
              Array.to_list
                (Array.mapi
                  begin fun i x ->
                    Seq[It(L(a.(i), LABEL));
                    bexpr x;
                    if i = Array.length xa - 1 then
                      empty
                    else
                      It(L(label_continue, BRA))]
                    end
                    xa));
            It(L(label_continue, LABEL));
          ]
        end
  in
  (*pf "Active symbols:";*)
  let build_code =
    List.fold_left
      begin fun code (name, expr) ->
        let active = SS.mem name have_action_set in
        (*if active then pf " %s" name;*)

        let b_name = build_name name in
        let (number, choice_number) = Hashtbl.find productions name in
        code ** (Seq[It(L(b_name, LABEL));
          if active then
            Seq[It(U PUSHP);
                bexpr ~number ?choice_number expr;
                It(U POPP);
                It(U(LDMEM number));
                It(U JMEM);
                It(U RTS)]
          else
            Seq[It(U(LDMEM number));
                It(U JMEM);
                It(U RTS)]])
      end
      empty
      peg
  in
  (*pf "\n%!";*)
  (* ***)
  (* ***)
  let code = Seq[Seq(List.map (fun (_, x) -> x) parse_code); build_code] in
  let cd = Array.of_list (Seq.flatten code) in
  let (cd, lb) = Nog.resolve cd in
  let start_pc = Nog.SM.find start lb in
  let build_pc = Nog.SM.find (build_name start) lb in
  { pg_start = start;
    pg_start_pc = start_pc;
    pg_build_pc = build_pc;
    pg_labels = lb;
    pg_tables = Array.of_list (List.rev !tables_list);
    pg_productions = Array.of_list (List.rev !productions_array);
    pg_choices = Array.of_list (List.rev !choices_array);
    pg_root = root;
    pg_code = cd }

(* ***)
(*** print_compiled *)
let print_compiled oc =
  List.iter
    begin fun (name, bb) ->
      fp oc "; %s\n\n%a\n" name print_instruction_sequence bb
    end

(* ***)
(*** print_code *)
let print_code oc ?(annotator = fun _ _ -> ()) pg =
  Array.iteri
    begin fun pc instr ->
      match instr with
      | L((pc,l), LABEL) -> fp oc "%a%s: ; %04d\n" annotator pc l pc
      | _ -> fp oc "%a      %a\n" annotator pc Machine.print_instruction instr
    end
    pg

(* ***)
(*** put_program *)
let put_program pg peg sk =
  let attributes, attribute_numbers = Nog.number_attributes pg peg
  and nodes, node_numbers = Nog.number_nodes pg peg
  in

  let dump_array a =
    Pack.write_uint sk & Array.length a;
    Array.iteri
      begin fun i u ->
        info `Debug "  #%d: %s" i u;
        Pack.write_string sk u
      end
      a
  in

  let resolve_label (x, _) = x in
  let resolve_node x =
    let id = Hashtbl.find nodes x in
    info `Debug "Resolving %s as %d" x id;
    id
  in

  Pack.write_uint64 sk nog_signature;
  Pack.write_uint64 sk nog_version;
  Pack.write_uint sk pg.pg_start_pc;
  Pack.write_uint sk pg.pg_build_pc;
  Pack.write_uint sk (resolve_node pg.pg_root);
  Pack.write_uint sk & Array.length pg.pg_productions;
  Pack.write_uint sk & Array.length pg.pg_choices;

  info `Debug "Nodes:";
  dump_array node_numbers;

  info `Debug "Attributes:";
  dump_array attribute_numbers;

  info `Debug "Tables";
  Pack.write_uint sk & Array.length pg.pg_tables;
  Array.iter
    begin fun a ->
      let top = 1 + Array.fold_left max 0 a in
      Pack.write_uint sk top;
      Array.iter
        begin fun x ->
          Pack.write_uint sk x
        end
        a
    end
    pg.pg_tables;

  Pack.write_uint sk & Array.length pg.pg_code;
  let resolve_attribute = Hashtbl.find attributes in
  Array.iter (Nog_packer.pack_instruction ~resolve_label ~resolve_node ~resolve_attribute sk) pg.pg_code

let put_program pg peg sk =
  let sum64 = ref 0L in
  let sk_sum64 = Bytes_.checksum64 sum64 sk in
  put_program pg peg sk_sum64;
  Pack.write_uint64 sk !sum64
(* ***)
(*** save_program *)
let save_program_ascii ~prologue ~epilogue fn pg peg =
  Util.with_file_output fn (fun oc ->
    let u = Bytes_.with_buffer_sink (put_program pg peg) in
    let m = String.length u in
    output_string oc prologue;
    for i = 0 to m - 1 do
      let c = Char.code u.[i] in
      if i mod 32 = 0 then
        fp oc "\n  ";
      fp oc "%3d," c
    done;
    output_string oc epilogue)
(* ***)
(*** save_program *)
let save_program fn pg peg = Util.with_binary_file_output fn (fun oc -> put_program pg peg (Bytes_.sink_of_out_channel oc))
(* ***)
(*** generate *)
let generate fn ?(start="start") peg =
  let peg' = Canonify.canonify_grammar ~start peg in
  generate_code ~start peg'
(* ***)
