(* Machine *)

let fp = Printf.fprintf;;

type boolean_register = int;;

type 'br multilabelable_instruction =
  | SWCH         (** Switch on choice register *)
;;

type 'br labelable_instruction =
  | LABEL        (** Label definition *)
  | BRA          (** Unconditional branch *)
  | BEOF         (** Branch if EOF *)
  | BNEOF        (** Branch if not EOF *)
  | BFC          (** Branch if fail clear *)
  | BFS          (** Branch if fail set *)
  | BMB          (** Branch if memo busy *)
  | BMBF         (** Branch if memo busy or failed *)
  | BMK          (** Branch if memo known *)
  | BMUK         (** Branch if memo unknown *)
  | BMF          (** Branch if memo failed *)
  | BBRC         (** Branch if boolean register is clear *)
  | BBRS         (** Branch if boolean register is set *)
  | JSR          (** Subroutine call *)
  | SBNS of char (** Scan and branch to label if character does not match *)
  | BSLLT of int (** Branch if suffix length is less than given integer *)
  | BNBOF        (** Branch if not BOF *)
;;

type ('br,'nd,'at) unlabelable_instruction =
  | SSEQ of char               (** Scan and push true if not equal, false otherwise *)
  | SSIR of char * char        (** Scan and push true if char in range, false otherwise *)
  | BTRUE                      (** Push true on boolean stack *)
  | BFALSE                     (** Push false on boolean stack *)
  | BAND                       (** Push conjunction of top two boolean values *)
  | BOR                        (** Push disjunction of top two boolean values *)
  | BNOT                       (** Push negation of the top boolean value *)
  | SETF                       (** Set the fail flag *)
  | CLRF                       (** Clear the fail flag *)
  | RIGHT of int               (** Advance the head *)
  | PUSHP                      (** Save the head position *)
  | POPP                       (** Restore the head position and pop it from the stack*)
  | RESTP                      (** Restore the head position *)
  | DROPP                      (** Drop head position *)
  | LDMEM of int               (** Get the memo entry for the given line *)
  | LDCH of int                (** Get the choice entry for the given line *)
  | POPSTMEMJ of int           (** Store the position in the memo entry at the position popped from the stack *)
  | STMEMB of int              (** Mark the memo entry as busy *)
  | STMEMF of int              (** Mark the memo entry as failed *)
  | TOPSTCH of int * int       (** Store the value into the choice register *)
  | JMEM                       (** Set head position to memo *)
  | RTS                        (** Return from subroutine *)
  | PCN                        (** Push current construction and create a new one *)
  | NODE of 'nd                (** Build a new node using the current construction and append it to the previous
                                   construction, making the previous construction current. *)
  | ATTR of 'at                (** Add an attribute of the given name whose value is the input between the saved position
                                   and the head position to the current construction *)
  | POSATTR of 'at             (** Add an attribute of the given name whose value is the current input position *)
  | TOKEN                      (** Build a token between the memo register and the head position and add it to the current construction *)
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
