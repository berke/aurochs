(* AMD64 backend *)

(* TODO: Spill on overflow for boolean expressions... *)

open Machine;;
open Nog;;
open Pffpsf;;

let r_a  = 0;;
let r_b  = 1;;
let r_c  = 2;;
let r_d  = 3;;
let r_si = 4;;
let r_di = 5;;
let r_bp = 6;;
let r_sp = 7;;
let r_8  = 8;;
let r_9  = 9;;
let r_10 = 10;;
let r_11 = 11;;
let r_12 = 12;;
let r_13 = 13;;
let r_14 = 14;;
let r_15 = 15;;

let registers =
  [|
     "rax",  "eax",  "ax",   "al";
     "rbx",  "ebx",  "bx",   "bl";
     "rcx",  "ecx",  "cx",   "cl";
     "rdx",  "edx",  "dx",   "dl";
     "rsi",  "esi",  "si",   "sil";
     "rdi",  "edi",  "di",   "dil";
     "rbp",  "ebp",  "bp",   "bpl";
     "rsp",  "esp",  "sp",   "spl";
     "r8",   "r8d",  "r8w",  "r8b";
     "r9",   "r9d",  "r9w",  "r9b";
     "r10", "r10d", "r10w", "r10b";
     "r11", "r11d", "r11w", "r11b";
     "r12", "r12d", "r12w", "r12b";
     "r13", "r13d", "r13w", "r13b";
     "r14", "r14d", "r14w", "r14b";
     "r15", "r15d", "r15w", "r15b";
  |]
;;

let callee_saved =
  Array.map
    (Array.get registers)
    [|
      r_b;
      r_bp;
      r_10;
      r_13;
      r_14;
      r_15;
      (* To be sure... *)
      r_12;
      r_11
    |]
;;


let pr64 oc (x,_,_,_) = fp oc "%%%s" x;;
let pr32 oc (_,x,_,_) = fp oc "%%%s" x;;
let pr16 oc (_,_,x,_) = fp oc "%%%s" x;;
let pr8  oc (_,_,_,x) = fp oc "%%%s" x;;

(* Register allocation
 *
 *   RAX - head position
 *   RDI - result table
 *
 *)

let r_stack = registers.(r_sp);;
let r_head  = registers.(r_a);;
let r_input = registers.(r_b);;
let r_fail = registers.(r_c);;
let r_tmp1 = registers.(r_si);;
let r_tmp2 = registers.(r_di);;
let r_memo = registers.(r_bp);;
let r_results = registers.(r_8);;
let r_alternatives = registers.(r_9);;
let r_input_char = registers.(r_10);;
let r_boolean = [|
  registers.(r_d);
  registers.(r_11);
  registers.(r_12);
  registers.(r_13);
  registers.(r_14);
  registers.(r_15)
|];;

let letter_size = 1;;
let choice_size = 1;;
let result_size = 4;;
let pointer_size = 8;;
let register_size = 8;;

let bitmask64 i = Int64.shift_left 1L i;;
let not64 = Int64.lognot;;

(*
    typedef struct {
      letter *cx_input;               /* Change to wchar for Unicode */
      result **cx_results;
      choice **cx_alternatives;
      int cx_input_length;
      int cx_num_productions;  /* Number of productions */
      int cx_num_alternatives; /* Number of productions for which choices may need to be recorded */
    } context;
*)

let cx_input_offset        = 0 * pointer_size;;
let cx_results_offset      = 1 * pointer_size;;
let cx_alternatives_offset = 2 * pointer_size;;
let cx_input_length        = cx_alternatives_offset + 1 * result_size;;

let r_EOF = 0
and r_UNKNOWN = 1
and r_FAIL = 2
and r_BUSY = 3
;;

let emit oc pg =
  let m = Array.length pg.pg_code in
  let input_loaded = ref true in
  let _eof_checked = ref true in
  let boolean_sp = ref (-1) in
  let _nops n =
    for i = 1 to n do
      fp oc "\tnop\n"
    done
  in
  (* Entry point and prelude *)
  let load_input () =
    if !input_loaded then
      fp oc "\t\t\t\t/* Input already loaded */\n"
    else
      begin
        fp oc "\tmovb (%a, %a), %a\n" pr64 r_input pr64 r_head pr8 r_input_char;
        input_loaded := true
      end
  in
  let invalidate_input () =
    input_loaded := false
  in
  let prelude () =
    fp oc "/* Prelude */\n";
    fp oc ".text\n";
    fp oc "\t.globl parse_%s\n" pg.pg_start;
    fp oc ".type parse_%s, @function\n" pg.pg_start;
    fp oc "parse_%s:\n" pg.pg_start;
    let m = Array.length callee_saved in
    fp oc "\tsub $%d, %a\n" (register_size * m) pr64 r_stack;
    for i = 0 to m - 1 do
      fp oc "\tmov %a, %d(%a)\n" pr64 callee_saved.(i) (register_size * i) pr64 r_stack
    done;
    (* Load registers... *)
    fp oc "\tmov %a, %a\n" pr64 registers.(r_si) pr64 r_head;
    assert (r_head == registers.(r_a));
    fp oc "\tcdqe\n";
    fp oc "\tmov %d(%a), %a\n" cx_input_offset pr64 registers.(r_di) pr64 r_input;
    fp oc "\tmov %d(%a), %a\n" cx_results_offset pr64 registers.(r_di) pr64 r_results;
    fp oc "\tmov %d(%a), %a\n" cx_alternatives_offset pr64 registers.(r_di) pr64 r_alternatives;
    fp oc "\txor %a, %a\n" pr64 r_memo pr64 r_memo;
    fp oc "\txor %a, %a\n" pr64 r_fail pr64 r_fail;
  in
  let epilog () =
    fp oc "/* Epilog */\n";
    let m = Array.length callee_saved in

    fp oc "\tmov $%d, %a\n" r_FAIL pr64 r_tmp1;
    fp oc "\ttest %a, %a\n" pr8 r_fail pr8 r_fail;
    fp oc "\tcmovne %a, %a\n" pr64 r_tmp1 pr64 r_head;
    assert (r_head == registers.(r_a));

    for i = 0 to m - 1 do
      fp oc "\tmov %d(%a), %a\n" (register_size * i) pr64 r_stack pr64 callee_saved.(i)
    done;
    fp oc "\tadd $%d, %a\n" (register_size * m) pr64 r_stack;
    fp oc "\tret\n"
  in
  prelude ();
  fp oc "\tcall %s\n" pg.pg_start;
  epilog ();
  for i = 0 to m - 1 do
    let instr = pg.pg_code.(i) in
    fp oc "\t\t\t\t/* %a */\n" print_instruction instr;
    (*nops 3;*)
    match instr with
    | M(_, _) -> fp oc "\t/* (Build match) */\n"
    | L((_, l), li) ->
        begin
          match li with
          | LABEL ->
              fp oc "%s:\n" l
          | BMB ->
              fp oc "\tcmp $%d, %a\n" r_BUSY pr64 r_memo;
              fp oc "\tje %s\n" l
          | BMBF ->
              assert (r_FAIL = 2);
              assert (r_BUSY = 3);
              fp oc "\tcmp $%d, %a\n" r_FAIL pr64 r_memo;
              fp oc "\tjge %s\n" l
          | BMK ->
              fp oc "\tcmp $%d, %a\n" r_UNKNOWN pr64 r_memo;
              fp oc "\tjne %s\n" l
          | BMUK ->
              fp oc "\tcmp $%d, %a\n" r_UNKNOWN pr64 r_memo;
              fp oc "\tje %s\n" l
          | BMF ->
              fp oc "\tcmp $%d, %a\n" r_FAIL pr64 r_memo;
              fp oc "\tje %s\n" l
          | BBRC ->
              fp oc "\ttest %a, %a\n" pr8 r_boolean.(!boolean_sp) pr8 r_boolean.(!boolean_sp);
              decr boolean_sp;
              fp oc "\tje %s\n" l
          | BFC ->
              fp oc "\ttest %a, %a\n" pr8 r_fail pr8 r_fail;
              fp oc "\tjne %s\n" l
          | BFS ->
              fp oc "\ttest %a, %a\n" pr8 r_fail pr8 r_fail;
              fp oc "\tjne %s\n" l
          | BEOF ->
              fp oc "\ttestq %a, %a\n" pr64 r_head pr64 r_head;
              fp oc "\tje %s\n" l
          | BNEOF ->
              fp oc "\ttestq %a, %a\n" pr64 r_head pr64 r_head;
              fp oc "\tjne %s\n" l
          | BRA -> fp oc "\tjmp %s\n" l
          | JSR -> fp oc "\tcall %s\n" l
          | BSLLT n ->
              if n = -1 then
                begin
                  fp oc "\ttest %a, %a\n" pr64 r_head pr64 r_head;
                  fp oc "\tjg %s\n" l
                end
              else
                begin
                  fp oc "\tcmp $%d, %a\n" (-n) pr64 r_head;
                  fp oc "\tjg %s\n" l
                end
          | SBNS c ->
              fp oc "\ttestq %a, %a\n" pr64 r_head pr64 r_head;
              fp oc "\tje %s\n" l;
              load_input ();
              fp oc "\tcmp $%d, %a\n" (Char.code c) pr8 r_input_char;
              fp oc "\tjne %s\n" l;
              invalidate_input ();
              fp oc "\tadd $1, %a\n" pr64 r_head (* Use add instead ? *)
          | _ -> failwith "Missing instructions" (* fp oc "\t /* XXX %a */\n" print_instruction instr *)
        end
    | U ui ->
        begin
          match ui with
          | POPSTMEMJ k ->
              fp oc "\tpop %a\n" pr64 r_tmp1; (* Target *)
              fp oc "\tmov %d(%a), %a\n" (pointer_size * k) pr64 r_results pr64 r_tmp2;
              fp oc "\tmov %a, (%a, %a, %d)\n"
                    pr32 r_head
                    pr64 r_tmp2
                    pr64 r_tmp1
                    result_size
          | STMEMF k ->
              fp oc "\tmov %d(%a), %a\n" (pointer_size * k) pr64 r_results pr64 r_tmp2;
              fp oc "\tmov $%d, %a\n" r_FAIL pr64 r_tmp1;
              fp oc "\tmov %a, (%a, %a, %d)\n"
                    pr32 r_tmp1
                    pr64 r_tmp2
                    pr64 r_head
                    result_size
          | STMEMB k ->
              fp oc "\tmov %d(%a), %a\n" (pointer_size * k) pr64 r_results pr64 r_tmp2;
              fp oc "\tmov $%d, %a\n" r_BUSY pr64 r_tmp1;
              fp oc "\tmov %a, (%a, %a, %d)\n"
                    pr32 r_tmp1
                    pr64 r_tmp2
                    pr64 r_head
                    result_size
          | LDMEM k ->
              fp oc "\tmov %d(%a), %a\n" (pointer_size * k) pr64 r_results pr64 r_tmp2;
              fp oc "\tmov (%a, %a, %d), %a\n"
                    pr64 r_tmp2
                    pr64 r_head
                    result_size
                    pr32 r_memo;
              fp oc "\tmovsx %a, %a\n"
                    pr32 r_memo
                    pr64 r_memo;
          | JMEM ->
              invalidate_input ();
              fp oc "\tmov %a, %a\n" pr64 r_memo pr64 r_head
          | RTS   ->
              invalidate_input ();
              fp oc "\tret\n"
          | POPP  ->
              invalidate_input ();
              fp oc "\tpop %a\n" pr64 r_head
          | DROPP -> fp oc "\tadd %d,%a\n" register_size pr64 r_stack
          | RESTP ->
              invalidate_input ();
              fp oc "\tmov (%a),%a\n" pr64 r_stack pr64 r_head
          | PUSHP -> fp oc "\tpush %a\n" pr64 r_head
          | RIGHT n ->
              invalidate_input ();
              fp oc "\tadd $%d,%a\n" (n * letter_size) pr64 r_head
          | SETF  -> fp oc "\tmov $1, %a\n" pr8 r_fail
          | CLRF  -> fp oc "\txor %a, %a\n" pr8 r_fail pr8 r_fail
          | SSEQ(c) ->
              incr boolean_sp;
              (*fp oc "\ttestq %a, %a\n" pr64 r_head pr64 r_head;
              fp oc "\tsetne %a\n" pr8 r_boolean.(!boolean_sp);*)
              load_input ();
              fp oc "\tcmpb $%d, %a\n" (Char.code c) pr8 r_input_char;
              fp oc "\tsete %a\n" pr8 r_boolean.(!boolean_sp);
              (*fp oc "\tand %a, %a\n" pr8 r_boolean.(!boolean_sp + 1) pr8 r_boolean.(!boolean_sp)*)
          | SSIR(c1, c2) ->
              incr boolean_sp;
              (*fp oc "\ttestq %a, %a\n" pr64 r_head pr64 r_head;
              fp oc "\tsetne %a\n" pr8 r_boolean.(!boolean_sp);*)
              load_input ();
              fp oc "\tcmpb $%d, %a\n" (Char.code c1) pr8 r_input_char;
              fp oc "\tsetae %a\n" pr8 r_boolean.(!boolean_sp);
              (*fp oc "\tand %a, %a\n" pr8 r_boolean.(!boolean_sp + 1) pr8 r_boolean.(!boolean_sp);*)
              fp oc "\tcmpb $%d, %a\n" (Char.code c2) pr8 r_input_char;
              fp oc "\tsetbe %a\n" pr8 r_boolean.(!boolean_sp + 1);
              fp oc "\tand %a, %a\n" pr8 r_boolean.(!boolean_sp + 1) pr8 r_boolean.(!boolean_sp)
          | BAND ->
              decr boolean_sp;
              fp oc "\tand %a, %a\n" pr8 r_boolean.(!boolean_sp + 1) pr8 r_boolean.(!boolean_sp)
          | BOR ->
              decr boolean_sp;
              fp oc "\tor %a, %a\n" pr8 r_boolean.(!boolean_sp + 1) pr8 r_boolean.(!boolean_sp)
          | BNOT ->
              fp oc "\txor $1, %a\n" pr8 r_boolean.(!boolean_sp)
          | BTRUE ->
              incr boolean_sp;
              fp oc "\tmov $1, %a\n" pr8 r_boolean.(!boolean_sp)
          | BFALSE ->
              incr boolean_sp;
              fp oc "\txor %a, %a\n" pr8 r_boolean.(!boolean_sp) pr8 r_boolean.(!boolean_sp)
          | TOPSTCH(k,l) ->
              (* Store choice l in choice line k *)
              fp oc "\tmov (%a), %a\n" pr64 r_stack pr64 r_tmp1;
              fp oc "\tmov %d(%a), %a\n" (pointer_size * k) pr64 r_alternatives pr64 r_tmp2;
              fp oc "\tmovb $%d, (%a, %a, %d)\n"
                    l
                    pr64 r_tmp2
                    pr64 r_tmp1
                    choice_size
          | LDCH _ | SNODE _ | FNODE | ATTR _ | POSATTR _ | TOKEN -> fp oc "\t/* Unimplemented construction opcode */\n"
        end
  done;
;;
