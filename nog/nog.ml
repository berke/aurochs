(* Nog *)

open Machine;;
open Pffpsf;;
open Util.Syntax;;

exception Parse_error of int;;

type 'pc stack_symbol =
  | PC of 'pc
  | Pos of int
  | Accept
;;

let print_stack_symbol oc = function
  | PC(p, l) -> fp oc "%s=%d" l p
  | Pos h -> fp oc "%d" h
  | Accept -> fp oc "#"
;;

(*** print_stack *)
let print_stack printer oc s =
  let first = ref true in
  Stack.iter
    begin fun x ->
      if !first then first := false else fp oc " ";
      fp oc "%a" printer x
    end
    s
;;
(* ***)

type ('nd,'at) code = ('nd, 'at, (int * string)) instruction array;;

module SM = Map.Make(String);;

type ('nd, 'at) program = {
  pg_start       : string; (** Top-level production *)
  pg_start_pc    : int;    (** Entry point for parsing *)
  pg_build_pc    : int;    (** Entry point for building *)
  pg_labels      : int SM.t;
  pg_productions : string array;
  pg_choices     : string array;
  pg_tables      : int array array;
  pg_root        : 'nd;    (** Name of the root node *)
  pg_code        : ('nd, 'at) code
};;

let lowercase_table = Array.init 256 (fun k -> Char.code & Char.lowercase & Char.chr k);;

type memo =
| Unknown
| Jump of int
| Busy
| Failure
;;

module IS = Set.Make(struct type t = int let compare = compare end);;

type ('nd, 'at) construction = {
  cons_name : 'nd option;
  cons_start : int;
  mutable cons_attributes : (int * int * 'at) list;
  mutable cons_children : ('nd, 'at) Peg.poly_positioned_tree list;
};;

(*** Memo *)
module Memo =
  struct
    type t = {
      m_m : int;
      m_n : int;
      m_table : (int, memo) Hashtbl.t
    };;

    let create m n = {
      m_m = m;
      m_n = n;
      m_table = Hashtbl.create m
    }

    let key m i j = (i * m.m_n) + j;;

    let get m i j =
      try
        Hashtbl.find m.m_table (key m i j)
      with
      | Not_found -> Unknown
    ;;

    let set m i j x =
      match x with
      | Unknown -> Hashtbl.remove m.m_table (key m i j)
      | _ -> Hashtbl.replace m.m_table (key m i j) x
    ;;
  end
;;
(* ***)
(*** Choices *)
module Choices =
  struct
    type t = {
      m_m : int;
      m_n : int;
      m_table : (int, int) Hashtbl.t
    };;

    let create m n = {
      m_m = m;
      m_n = n;
      m_table = Hashtbl.create m
    }

    let key m i j = (i * m.m_n) + j;;

    let get m i j =
      try
        Some(Hashtbl.find m.m_table (key m i j))
      with
      | Not_found -> None
    ;;

    let set m i j x = Hashtbl.replace m.m_table (key m i j) x
    ;;
  end
;;
(* ***)
(*** configuration *)
type ('nd, 'at) configuration = {
  mutable pc : int;
  mutable head : int;
  mutable fail : bool;
  boolean : bool Stack.t; (* Boolean stack *)
  stack : (int * string) stack_symbol Stack.t;
  mutable label : string; (* Last symbol seen *)
  mutable memo_register : memo;
  mutable choice_register : int option;
  mutable step_mode : [`step of int|`finish|`run|`stop|`trace];
  mutable breakpoints : IS.t;
  trace_instructions : ('nd, 'at, (int * string)) instruction list;
  saved_cons : ('nd, 'at) construction Stack.t;
  mutable cons : ('nd, 'at) construction;
  memo : Memo.t;
  choices : Choices.t; (* int option array array; *)
};;
(* ***)

exception Finish;;
exception Quit;;
exception Error of string;;

let ansi_red = "\027[31m"
let ansi_green = "\027[32m"
let ansi_yellow = "\027[33m"
let ansi_none = "\027[0m"
let move_to oc x = fp oc "\r\027[%dC\027[K" x
let up = "\027[1A"
let ceol = "\027[K"
let home = "\r"
;;

(*** print_memo *)
let print_memo oc = function
| Jump j -> fp oc "J(%d)" j
| Failure -> fp oc "F"
| Busy -> fp oc "B"
| Unknown -> fp oc "U"
;;
(* ***)

let print_boolean oc b = fp oc "%b" b;;

(*** print_boolean_array *)
let print_boolean_array oc a =
  Array.iter 
    begin fun b ->
      fp oc "%d" (if b then 1 else 0)
    end
    a
;;
(* ***)
(*** execute_positioned_quick *)
let execute_positioned_quick program
  ~print_node
  ~print_attr
  ~root
  u
  =
  let m = String.length u in
  let num_productions = Array.length program.pg_productions in
  let num_choices = Array.length program.pg_choices in
  let c =
    { pc = 0;
      head = 0;
      stack = Stack.create ();
      fail = false;
      boolean = Stack.create ();
      memo = Memo.create num_productions (m + 1);
      choices = Choices.create num_choices (m + 1);
      memo_register = Unknown;
      choice_register = None;
      step_mode = `run;
      breakpoints = IS.empty;
      trace_instructions = [];
      saved_cons = Stack.create ();
      cons = { cons_name = None; cons_start = 0; cons_attributes = []; cons_children = [] };
      label = program.pg_start }
  in
  let rec run pc =
    let instr = program.pg_code.(pc) in
    let pc = pc + 1 in
    match instr with
    | L((pc', _), li) ->
      begin
        match li with
        | LABEL     -> run pc
        | BRA       -> run pc'
        | BFS       -> if c.fail then run pc' else run pc
        | BFC       -> if c.fail then run pc else run pc'
        | BEOF      -> if c.head = m then run pc' else run pc
        | BNEOF     -> if c.head < m then run pc' else run pc
        | BNBOF     -> if c.head > 0 then run pc' else run pc
        | BMB       -> if c.memo_register = Busy then run pc' else run pc
        | BMBF      -> if c.memo_register = Busy or c.memo_register = Failure then run pc' else run pc
        | BBRS      -> if Stack.pop c.boolean then run pc' else run pc
        | BBRC      -> if not (Stack.pop c.boolean) then run pc' else run pc
        | BMF       -> if c.memo_register = Failure then run pc' else run pc
        | BMUK      -> if c.memo_register = Unknown then run pc' else run pc
        | BMK       -> if c.memo_register <> Unknown then run pc' else run pc
        | JSR       -> run pc'; run pc
        | SBNS ch   ->
            if c.head = m or u.[c.head] <> ch then
              run pc'
            else
              begin
                c.head <- c.head + 1;
                run pc
              end
        | BSLLT n   -> if m - c.head < n then run pc' else run pc
      end
    | U ui ->
       begin
         match ui with
         | RTS -> ()
         | _ ->
             begin
               match ui with
               | BAND ->
                   let br1 = Stack.pop c.boolean in
                   let br2 = Stack.pop c.boolean in
                   Stack.push (br1 && br2) c.boolean
               | BOR ->
                   let br1 = Stack.pop c.boolean in
                   let br2 = Stack.pop c.boolean in
                   Stack.push (br1 or br2) c.boolean
               | BNOT -> Stack.push (not (Stack.pop c.boolean)) c.boolean
               | BTRUE -> Stack.push true c.boolean
               | BFALSE -> Stack.push false c.boolean
               | SSIR(c1, c2) -> Stack.push (c.head < m && c1 <= u.[c.head] && u.[c.head] <= c2) c.boolean
               | SSEQ ch -> Stack.push (c.head < m && ch = u.[c.head]) c.boolean
               | TSSEQ(ti, k) ->
                   Stack.push (c.head < m &&
                     begin
                       let c = Char.code u.[c.head] in
                       program.pg_tables.(ti).(c) = k
                     end)
                     c.boolean
               | RIGHT n -> c.head <- n + c.head
               | JMEM ->
                 begin
                   match c.memo_register with
                   | Jump h -> c.head <- h
                   | _ -> invalid_arg "JUMP without position in memo register"
                 end
               | SETF -> c.fail <- true
               | CLRF -> c.fail <- false
               | PUSHP -> Stack.push (Pos c.head) c.stack
               | DROPP -> ignore (Stack.pop c.stack)
               | POPP ->
                   begin
                     match Stack.pop c.stack with
                     | Pos head -> c.head <- head
                     | _ -> raise (Error "POPP without position on stack")
                   end
               | RESTP ->
                   begin
                     match Stack.top c.stack with
                     | Pos head -> c.head <- head
                     | _ -> raise (Error "POPP without position on stack")
                   end
               | LDMEM k -> c.memo_register <- Memo.get c.memo k c.head
               | LDCH k -> c.choice_register <- Choices.get c.choices k c.head
               | POPSTMEMJ k ->
                   begin
                     match Stack.pop c.stack with
                     | Pos head -> Memo.set c.memo k head (Jump c.head)
                     | _ -> raise (Error "POPSTMEMJ without position on stack")
                   end
               | STMEMB k -> Memo.set c.memo k c.head Busy
               | STMEMF k -> Memo.set c.memo k c.head Failure
               | TOPSTCH(j,k) ->
                   begin
                     match Stack.top c.stack with
                     | Pos head -> Choices.set c.choices j head k
                     | _ -> raise (Error "TOPSTCH without position on stack")
                   end
               | SNODE n ->
                   Stack.push c.cons c.saved_cons;
                   c.cons <- { cons_name = Some n; cons_start = c.head; cons_attributes = []; cons_children = [] }
               | FNODE ->
                   let nd = Peg.P_Node(c.cons.cons_start, c.head, Util.mandatory c.cons.cons_name, List.rev c.cons.cons_attributes, List.rev c.cons.cons_children) in
                   c.cons <- Stack.pop c.saved_cons;
                   c.cons.cons_children <- nd :: c.cons.cons_children
               | ATTR n ->
                   begin
                     match c.memo_register with
                     | Jump i -> c.cons.cons_attributes <- (c.head, i, n) :: c.cons.cons_attributes
                     | _ -> raise (Error "ATTR without valid value in memo register")
                   end
               | POSATTR n -> c.cons.cons_attributes <- (c.head, c.head - 1, n) :: c.cons.cons_attributes
               | TOKEN ->
                   begin
                     match c.memo_register with
                     | Jump i ->
                       c.cons.cons_children <- (Peg.P_Token(c.head, i)) :: c.cons.cons_children
                     | _ -> raise (Error "TOKEN without valid value in memo register")
                   end
               | _ -> assert false
             end;
             run pc
       end
   | M(a, SWCH) ->
       match c.choice_register with
       | None -> raise (Error "SWCH with empty choice register")
       | Some i ->
           let (pc', _) = a.(i) in
           run pc'
  in

  let error_position ()=
    let max_j = ref 0 in
    for i = 0 to num_productions - 1 do
      for j = 0 to m do
        match Memo.get c.memo i j with
        | Jump j' -> max_j := max j' !max_j
        | _ -> ()
      done;
    done;
    !max_j
  in

  let root () =
    Peg.P_Node(0, m, root, List.rev c.cons.cons_attributes, List.rev c.cons.cons_children)
  in

  run program.pg_start_pc;

  if c.fail then raise (Parse_error(error_position ()))
  else
    begin
      c.head <- 0;
      run program.pg_build_pc;
      root ()
    end
;;
(* ***)
(*** execute_positioned *)
let execute_positioned program
  ?(quick=false)
  ?(show_memo=false)
  ?(trace=false)
  ?log_calls
  ?record
  ?(interactive=false)
  ?(profiler=ignore)
  ~print_node
  ~print_attr
  ~root
  u
  =
  if quick then
    execute_positioned_quick program ~print_node ~print_attr ~root u
  else
    begin
      let m = String.length u in
      let n = Array.length program.pg_code in
      let num_productions = Array.length program.pg_productions in
      let num_choices = Array.length program.pg_choices in
      let c =
        { pc = 0;
          head = 0;
          stack = Stack.create ();
          fail = false;
          boolean = Stack.create ();
          memo = Memo.create num_productions (m + 1);
          choices = Choices.create num_choices (m + 1);
          memo_register = Unknown;
          choice_register = None;
          step_mode = if interactive then `stop else `run;
          breakpoints = IS.empty;
          trace_instructions = [];
          saved_cons = Stack.create ();
          cons = { cons_name = None; cons_start = 0; cons_attributes = []; cons_children = [] };
          label = program.pg_start }
      in
      let log_call =
        match log_calls with
        | None -> ignore
        | Some fn ->
            let oc = open_out fn in
            let last_label = ref None in
            fun x ->
              if !last_label <> Some x then
                begin
                  last_label := Some x;
                  fp oc "%s\n" x
                end
      in
      let record =
        match record with
        | None -> ignore
        | Some fn ->
            let oc = open_out fn in
            fun x -> fp oc "%d %d %d\n" c.pc c.head (if c.fail then 1 else 0)
      in

      let profile = Array.make (Array.length program.pg_code) 0 in
      let prompt () =
        pf "(ndb) %!";
        let cmd = input_line stdin in
        pf "%s%s%s%!" up home ceol;
        cmd
      in
      let show_state () =
        let instr = program.pg_code.(c.pc) in
        pf "PC=%04d %s%-16s%s " c.pc ansi_green c.label ansi_none;
        pf "%s%a%s " ansi_red (print_poly_instruction ~print_node ~print_attr) instr ansi_none;
        pf "%a" move_to 60;
        pf "H=%d " c.head;
        pf "M=%a " print_memo c.memo_register;
        pf "F=%d " (if c.fail then 1 else 0);
        pf "%a" move_to 80;
        pf "B=%a " (print_stack print_boolean) c.boolean;
        pf "S=[%a]\n" (print_stack print_stack_symbol) c.stack;
      in
      let iterate () =
        if c.pc < 0 or c.pc >= n then
          raise (Error "PC out of bounds")
        else
          begin
            record program.pg_start;
            let instr = program.pg_code.(c.pc) in
            profile.(c.pc) <- 1 + profile.(c.pc);
            c.pc <- 1 + c.pc;
            match instr with
            | L((pc, l), li) ->
              begin
                match li with
                | LABEL     -> log_call l; c.label <- l (* Pseudo-instruction *)
                | BRA       -> c.pc <- pc
                | BFS       -> if c.fail then c.pc <- pc
                | BFC       -> if not c.fail then c.pc <- pc
                | BEOF      -> if c.head = m then c.pc <- pc
                | BNEOF     -> if c.head < m then c.pc <- pc
                | BNBOF     -> if c.head > 0 then c.pc <- pc
                | BMB       -> if c.memo_register = Busy then c.pc <- pc
                | BMBF      -> if c.memo_register = Busy or c.memo_register = Failure then c.pc <- pc
                | BBRS      -> if Stack.pop c.boolean then c.pc <- pc
                | BBRC      -> if not (Stack.pop c.boolean) then c.pc <- pc
                | BMF       -> if c.memo_register = Failure then c.pc <- pc
                | BMUK      -> if c.memo_register = Unknown then c.pc <- pc
                | BMK       -> if c.memo_register <> Unknown then c.pc <- pc
                | JSR       ->
                    Stack.push (PC(c.pc, c.label)) c.stack;
                    c.label <- l;
                    (*log_call program.pg_start;*)
                    c.pc <- pc
                | SBNS ch   ->
                    if c.head = m or u.[c.head] <> ch then
                      c.pc <- pc
                    else
                      c.head <- c.head + 1
                | BSLLT n   -> if m - c.head < n then c.pc <- pc
              end
            | U ui ->
               begin
                 match ui with
                 | BAND ->
                     let br1 = Stack.pop c.boolean in
                     let br2 = Stack.pop c.boolean in
                     Stack.push (br1 && br2) c.boolean
                 | BOR ->
                     let br1 = Stack.pop c.boolean in
                     let br2 = Stack.pop c.boolean in
                     Stack.push (br1 or br2) c.boolean
                 | BNOT -> Stack.push (not (Stack.pop c.boolean)) c.boolean
                 | BTRUE -> Stack.push true c.boolean
                 | BFALSE -> Stack.push false c.boolean
                 | SSIR(c1, c2) -> Stack.push (c.head < m && c1 <= u.[c.head] && u.[c.head] <= c2) c.boolean
                 | SSEQ(ch) -> Stack.push (c.head < m && ch = u.[c.head]) c.boolean
                 | TSSEQ(ti, k) ->
                     Stack.push (c.head < m &&
                       begin
                         let cc = Char.code u.[c.head] in
                         program.pg_tables.(ti).(cc) = k
                       end)
                       c.boolean
                 | RIGHT n -> if c.head + n - 1 <= m then c.head <- n + c.head else invalid_arg (sf "RIGHT %d at %d" n c.head)
                 | JMEM ->
                   begin
                     match c.memo_register with
                     | Jump h -> c.head <- h
                     | _ -> invalid_arg "JUMP without position in memo register"
                   end
                 | SETF -> c.fail <- true
                 | CLRF -> c.fail <- false
                 | PUSHP -> Stack.push (Pos c.head) c.stack
                 | DROPP -> ignore (Stack.pop c.stack)
                 | POPP ->
                     begin
                       match Stack.pop c.stack with
                       | Pos head -> c.head <- head
                       | _ -> raise (Error "POPP without position on stack")
                     end
                 | RESTP ->
                     begin
                       match Stack.top c.stack with
                       | Pos head -> c.head <- head
                       | _ -> raise (Error "POPP without position on stack")
                     end
                 | LDMEM k -> c.memo_register <- Memo.get c.memo k c.head
                 | LDCH k -> c.choice_register <- Choices.get c.choices k c.head
                 | POPSTMEMJ k ->
                     begin
                       match Stack.pop c.stack with
                       | Pos head -> Memo.set c.memo k head (Jump c.head)
                       | _ -> raise (Error "POPSTMEMJ without position on stack")
                     end
                 | STMEMB k -> Memo.set c.memo k c.head Busy
                 | STMEMF k -> Memo.set c.memo k c.head Failure
                 | TOPSTCH(j,k) ->
                     begin
                       match Stack.top c.stack with
                       | Pos head -> Choices.set c.choices j head k
                       | _ -> raise (Error "TOPSTCH without position on stack")
                     end
                 | RTS ->
                    begin
                      match Stack.pop c.stack with
                      | PC(pc, l) -> c.pc <- pc; log_call program.pg_start; c.label <- l
                      | Accept -> raise Finish
                      | _ -> raise (Error "RET without PC on stack")
                    end
                 | SNODE n ->
                     Stack.push c.cons c.saved_cons;
                     c.cons <- { cons_name = Some n; cons_start = c.head; cons_attributes = []; cons_children = [] }
                 | FNODE ->
                     let nd = Peg.P_Node(c.cons.cons_start, c.head, Util.mandatory c.cons.cons_name, List.rev c.cons.cons_attributes, List.rev c.cons.cons_children) in
                     c.cons <- Stack.pop c.saved_cons;
                     c.cons.cons_children <- nd :: c.cons.cons_children
                 | ATTR n ->
                     begin
                       match c.memo_register with
                       | Jump i ->
                         c.cons.cons_attributes <- (c.head, i, n) :: c.cons.cons_attributes
                       | _ -> raise (Error "ATTR without valid value in memo register")
                     end
                 | POSATTR n -> c.cons.cons_attributes <- (c.head, c.head - 1, n) :: c.cons.cons_attributes
                 | TOKEN ->
                     begin
                       match c.memo_register with
                       | Jump i ->
                         c.cons.cons_children <- (Peg.P_Token(c.head, i)) :: c.cons.cons_children
                       | _ -> raise (Error "TOKEN without valid value in memo register")
                     end
               end
           | M(a, SWCH) ->
               match c.choice_register with
               | None -> raise (Error "SWCH with empty choice register")
               | Some i ->
                   let (pc, l) = a.(i) in
                   c.pc <- pc;
                   c.label <- l
          end
      in

      let breakpoint_reached = ref (-1) in
      let step () =
        if trace then
          begin
            if c.pc <> !breakpoint_reached && IS.mem c.pc c.breakpoints then
              begin
                pf "Breakpoint reached.\n%!";
                breakpoint_reached := c.pc;
                c.step_mode <- `stop
              end
            else
              if c.pc <> !breakpoint_reached && List.mem program.pg_code.(c.pc) c.trace_instructions then
                begin
                  pf "Traced instruction reached.\n%!";
                  breakpoint_reached := c.pc;
                  c.step_mode <- `stop
                end
              else
                begin
                  try
                    iterate ();
                    breakpoint_reached := -1
                  with
                  | Finish -> raise Finish
                  | x ->
                      pf "EXCEPTION %s\n%!" (Printexc.to_string x);
                      breakpoint_reached := -1;
                      c.step_mode <- `stop
                end
          end
        else
          iterate ();
      in
      let breakpoint op point =
        let pc =
          try
            int_of_string point
          with
          | _ -> SM.find point program.pg_labels
        in
        c.breakpoints <- op pc c.breakpoints;
        pc
      in

      let error_position ()=
        let max_j = ref 0 in
        for i = 0 to num_productions - 1 do
          for j = 0 to m do
            match Memo.get c.memo i j with
            | Jump j' -> max_j := max j' !max_j
            | _ -> ()
          done;
        done;
        !max_j
      in

      let print_memo_table ?position oc =
        fp oc "Memo table:\n";
        for i = 0 to num_productions - 1 do
          fp oc " Line %-4d (%-16s):" i program.pg_productions.(i);
          let l j = Memo.get c.memo i j in
          begin
            match position with
            | None ->
              for j = 0 to m do
                fp oc " %d:%a" j print_memo (l j)
              done;
            | Some j -> fp oc " %a" print_memo (l j)
          end;
          fp oc "\n"
        done
      in

      let finish () =
        if show_memo then print_memo_table stdout;
      in

      let root () =
        Peg.P_Node(0, m, root, List.rev c.cons.cons_attributes, List.rev c.cons.cons_children)
      in

      let show_choices i =
        pf "Choices:\n";
        for k = 0 to num_choices - 1 do
          pf "%16s (%d) -> " program.pg_choices.(k) k;
          match Choices.get c.choices k i with
          | None -> pf "No choice\n"
          | Some j -> pf "Choice %d\n" j
        done
      in

      let run pc =
        Stack.push Accept c.stack;
        c.pc <- pc;
        let last_displayed_pc = ref (pc + 1) in
        try
          while true do
            match c.step_mode with
            | `trace -> step ()
            | `run -> step ()
            | `stop ->
                begin
                  if c.pc <> !last_displayed_pc then
                    begin
                      show_state ();
                      last_displayed_pc := c.pc
                    end;
                  try
                    let cmd = prompt () in
                    match Util.split_at ' ' cmd with
                    | ["s"]|[] -> c.step_mode <- `step 1
                    | ["bt"] ->
                        pf "Stack:\n";
                        Stack.iter (fun s -> pf "  %a\n" print_stack_symbol s) c.stack
                    | ["f"] -> c.step_mode <- `finish
                    | ["r"] -> c.step_mode <- `run
                    | ["t"] -> c.step_mode <- `trace
                    | ["s"; n'] -> let n = int_of_string n' in c.step_mode <- `step n
                    | ["q"] -> raise Quit
                    | ["m"] -> print_memo_table ~position:c.head stdout
                    | ["u";i'] -> let i = int_of_string i' in pf "u[%d] = %C\n" i u.[i]
                    | ["m";x'] -> let x = int_of_string x' in print_memo_table ~position:x stdout
                    | ["chr"] ->
                        begin
                          match c.choice_register with
                          | None -> pf "None\n"
                          | Some i -> pf "Some %d\n" i
                        end
                    (*| ["cons"] ->
                        let t = root () in
                        Peg.print_tree () stdout t*)
                    | ["ch"] -> show_choices c.head
                    | ["ch"; k'] ->
                        begin
                          let k = int_of_string k' in
                          show_choices k
                        end
                    | ["b"; point] ->
                        let pc = breakpoint IS.add point in
                        pf "Breakpoint added at %d\n" pc
                    | ["bd"; point] ->
                        let pc = breakpoint IS.add point in
                        pf "Breakpoint deleted at %d\n" pc
                    | _ -> pf "Invalid command.\n"
                  with
                  | (Finish|Quit) as x -> raise x
                  | x ->
                      pf "Exception: %s\n%!" (Printexc.to_string x)
                end
            | `finish ->
                let instr = program.pg_code.(c.pc) in
                if instr = U RTS then c.step_mode <- `step 1;
                step ()
            | `step 0 -> c.step_mode <- `stop
            | `step n ->
                c.step_mode <- `step(n - 1);
                step ()
          done;
          assert false
        with
        | Finish -> finish ()
        | x -> raise x
      in
      run program.pg_start_pc;
      if c.fail then
        begin
          profiler profile;
          raise (Parse_error(error_position ()))
        end
      else
        begin
          c.head <- 0;
          run program.pg_build_pc;
          profiler profile;
          root ()
        end
    end
;;
(* ***)
(*** execute *)
let execute program ?quick ?show_memo ?trace ?log_calls ?interactive ?profiler ~print_node ~print_attr ~root u =
  let t = execute_positioned program ?quick ?show_memo ?trace ?log_calls ?interactive ?profiler ~print_node ~print_attr ~root u in
  Peg.relativize u t
;;
(* ***)

let sf = Printf.sprintf;;

(*** resolve *)
let resolve p =
  let labels = ref SM.empty in
  (* Collect *)
  Array.iteri
    begin fun i instr ->
      match instr with
      | L(n, LABEL) ->
          if SM.mem n !labels then
            raise (Error(sf "Label %S defined twice, once at %d, once at %d" n
                         (SM.find n !labels) i))
          else
            labels := SM.add n i !labels
      | _ -> ()
    end
    p;
  (* Resolve *)
  let resolve n =
    try
      (SM.find n !labels, n)
    with
    | Not_found -> raise (Error(sf "Undefined label %S" n))
  in
  Array.map
    begin fun instr ->
      match instr with
      | M(a, x) -> M(Array.map resolve a, x)
      | L(n, x) -> L(resolve n, x)
      | U i -> U i
    end
    p, !labels
;;
(* ***)
(*** is_space *)
let is_space = ("is_space", (=) ' ');;
(* ***)
(*** is_not_space *)
let is_not_space = ("is_not_space", (<>) ' ');;
(* ***)
(*** is_alpha *)
let is_alpha = ("is_alpha", (fun c -> ('a' <= c && c <= 'z') or ('A' <= c && c <= 'Z')));;
(* ***)
(*** number *)
let number ?root pg iterator =
  let module SS = Set.Make(String) in
  let set = ref
    (match root with
      | None -> SS.empty
      | Some r -> SS.singleton r)
  in
  iterator (fun u -> set := SS.add u !set);
  let ua = Array.of_list (SS.elements !set) in
  let h = Hashtbl.create (Array.length ua) in
  Array.iteri (fun i u -> Hashtbl.add h u i) ua;
  h, ua
;;
(* ***)
(*** number_attributes *)
let number_attributes pg peg =
  number pg (fun f -> List.iter (fun (_, pe) -> Peg.iter_over_attributes f pe) peg)
;;
(* ***)
(*** number_nodes *)
let number_nodes pg peg =
  number ~root:pg.pg_root pg (fun f -> List.iter (fun (_, pe) -> Peg.iter_over_builds f pe) peg)
;;
(* ***)
