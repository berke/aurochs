(* Machine *)

let fp = Printf.fprintf;;

type boolean_register = int;;

type 'br multilabelable_instruction =
  | SWCH         (** Switch on choice register *)
;;

type 'br labelable_instruction =
                     | LABEL        (** Label definition *)
  (* %opcode{00l} *) | BRA          (** Unconditional branch *)
  (* %opcode{01l} *) | BEOF         (** Branch if EOF *)
  (* %opcode{02l} *) | BNEOF        (** Branch if not EOF *)
  (* %opcode{03l} *) | BFC          (** Branch if fail clear *)
  (* %opcode{04l} *) | BFS          (** Branch if fail set *)
  (* %opcode{05l} *) | BMB          (** Branch if memo busy *)
  (* %opcode{06l} *) | BMBF         (** Branch if memo busy or failed *)
  (* %opcode{07l} *) | BMK          (** Branch if memo known *)
  (* %opcode{08l} *) | BMUK         (** Branch if memo unknown *)
  (* %opcode{09l} *) | BMF          (** Branch if memo failed *)
  (* %opcode{10l} *) | BBRC         (** Branch if boolean register is clear *)
  (* %opcode{11l} *) | BBRS         (** Branch if boolean register is set *)
  (* %opcode{12l} *) | JSR          (** Subroutine call *)
  (* %opcode{13l} *) | SBNS of char (** Scan and branch to label if character does not match *)
  (* %opcode{14l} *) | BSLLT of int (** Branch if suffix length is less than given integer *)
  (* %opcode{15l} *) | BNBOF        (** Branch if not BOF *)
;;

type ('br,'nd,'at) unlabelable_instruction =
  (* %opcode{16c} *)  | SSEQ of char               (** Scan and push true if not equal, false otherwise *)
  (* %opcode{17cc} *) | SSIR of char * char        (** Scan and push true if char in range, false otherwise *)
  (* %opcode{18} *)   | BTRUE                      (** Push true on boolean stack *)
  (* %opcode{19} *)   | BFALSE                     (** Push false on boolean stack *)
  (* %opcode{20} *)   | BAND                       (** Push conjunction of top two boolean values *)
  (* %opcode{21} *)   | BOR                        (** Push disjunction of top two boolean values *)
  (* %opcode{22} *)   | BNOT                       (** Push negation of the top boolean value *)
  (* %opcode{23} *)   | SETF                       (** Set the fail flag *)
  (* %opcode{24} *)   | CLRF                       (** Clear the fail flag *)
  (* %opcode{25} *)   | RIGHT of int               (** Advance the head *)
  (* %opcode{26} *)   | PUSHP                      (** Save the head position *)
  (* %opcode{27} *)   | POPP                       (** Restore the head position and pop it from the stack*)
  (* %opcode{28} *)   | RESTP                      (** Restore the head position *)
  (* %opcode{29} *)   | DROPP                      (** Drop head position *)
  (* %opcode{30i} *)  | LDMEM of int               (** Get the memo entry for the given line *)
  (* %opcode{31i} *)  | LDCH of int                (** Get the choice entry for the given line *)
  (* %opcode{32i} *)  | POPSTMEMJ of int           (** Store the position in the memo entry at the position popped from the stack *)
  (* %opcode{33i} *)  | STMEMB of int              (** Mark the memo entry as busy *)
  (* %opcode{34i} *)  | STMEMF of int              (** Mark the memo entry as failed *)
  (* %opcode{35ii} *) | TOPSTCH of int * int       (** Store the value into the choice register *)
  (* %opcode{36} *)   | JMEM                       (** Set head position to memo *)
  (* %opcode{37} *)   | RTS                        (** Return from subroutine *)
  (* %opcode{38} *)   | PCN                        (** Push current construction and create a new one *)
  (* %opcode{39n} *)  | NODE of 'nd                (** Build a new node using the current construction and append it to the previous
                                                       construction, making the previous construction current. *)
  (* %opcode{39a} *)  | ATTR of 'at                (** Add an attribute of the given name whose value is the input between the saved position
                                                       and the head position to the current construction *)
  (* %opcode{40a} *)  | POSATTR of 'at             (** Add an attribute of the given name whose value is the current input position *)
  (* %opcode{41} *)   | TOKEN                      (** Build a token between the memo register and the head position and add it to the current construction *)
;;

type ('br,'nd,'at,'label) instruction =
  | L of 'label * 'br labelable_instruction
  | U of ('br,'nd,'at) unlabelable_instruction
  | M of 'label array * 'br multilabelable_instruction
;;

type ('br,'nd,'at,'label) basic_block = {
    bb_body : ('br,'nd,'at,'label) instruction list;
    bb_next : 'label;
    bb_fail : 'label;
};;

let print_multilabelable oc = function
  | SWCH -> fp oc "SWCH"
;;

let print_labelable oc = function
  | LABEL -> fp oc "LABEL"
  | BRA -> fp oc "BRA"
  | BFS -> fp oc "BFS"
  | BFC -> fp oc "BFC"
  | BEOF -> fp oc "BEOF"
  | BNEOF -> fp oc "BNEOF"
  | BNBOF -> fp oc "BNBOF"
  | BMB -> fp oc "BMB"
  | BMF -> fp oc "BMF"
  | BMBF -> fp oc "BMBF"
  | BMUK -> fp oc "BMUK"
  | BMK -> fp oc "BMK"
  | BBRC -> fp oc "BBRC"
  | BBRS -> fp oc "BBRS"
  | JSR -> fp oc "JSR"
  | SBNS c -> fp oc "SBNS %C" c
  | BSLLT n -> fp oc "BSLLT %d" n
;;

let print_unlabelable ~print_node ~print_attr oc = function
  | SSEQ c -> fp oc "SSEQ %C" c
  | SSIR(c1, c2) -> fp oc "SSIR(%C, %C)" c1 c2
  | BFALSE -> fp oc "BFALSE"
  | BTRUE -> fp oc "BTRUE"
  | BAND -> fp oc "BAND"
  | BOR -> fp oc "BOR"
  | BNOT -> fp oc "BNOT"
  | SETF -> fp oc "SETF"
  | JMEM -> fp oc "JMEM"
  | CLRF -> fp oc "CLRF"
  | RIGHT n -> fp oc "RIGHT %d" n
  | PUSHP -> fp oc "PUSHP"
  | POPP -> fp oc "POPP"
  | RESTP -> fp oc "RESTP"
  | DROPP -> fp oc "DROPP"
  | LDMEM k -> fp oc "LDMEM %d" k
  | LDCH k -> fp oc "LDCH %d" k
  | POPSTMEMJ k -> fp oc "POPSTMEMJ %d" k
  | STMEMF k -> fp oc "STMEMF %d" k
  | STMEMB k -> fp oc "STMEMB %d" k
  | TOPSTCH(k,l) -> fp oc "TOPSTCH(%d, %d)" k l
  | RTS -> fp oc "RTS"
  | PCN -> fp oc "PCN"
  | NODE n -> fp oc "NODE %a" print_node n
  | ATTR a -> fp oc "ATTR %a" print_attr a
  | POSATTR a -> fp oc "POSATTR %a" print_attr a
  | TOKEN -> fp oc "TOKEN"
;;

let print_poly_instruction ~print_node ~print_attr oc = function
  | L((pc, l), instr) -> fp oc "%a %s (%d)" print_labelable instr l pc
  | U instr -> fp oc "%a" (print_unlabelable ~print_node ~print_attr) instr
  | M(a, instr) ->
      fp oc "%a\n" print_multilabelable instr;
      Array.iteri
        begin fun i (pc,l) ->
          fp oc "        #%d -> %s (%d)\n" i l pc
        end
        a
;;

let print_node, print_attr = output_string, output_string;;

let print_instruction oc x = print_poly_instruction ~print_node ~print_attr oc x;;

let print_instruction_sequence oc sq =
  Seq.iter
    begin fun instr ->
      fp oc "\t%a\n" print_instruction instr
    end
    sq
;;

let dump_instruction ~print_node ~print_attr oc = function
  | L((pc, l), instr) -> fp oc "L((%d, %S), %a)" pc l print_labelable instr
  | U instr -> fp oc "U(%a)" (print_unlabelable ~print_node ~print_attr) instr
  | M(a, instr) ->
      fp oc "M([|\n";
      Array.iteri
        begin fun i (pc,l) ->
          fp oc " (* %d *) (%d, %S);\n" i pc l
        end
        a;
      fp oc "|], %a)\n" print_multilabelable instr;
;;

let dump_instruction_sequence ~print_node ~print_attr oc sq =
  fp oc "[|\n";
  Seq.iter
    begin fun instr ->
      fp oc "\t%a;\n" (dump_instruction ~print_node ~print_attr) instr
    end
    sq
;;
