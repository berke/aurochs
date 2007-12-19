(* Machine *)

let nog_signature = 0xABBE55E5L;;
let nog_version   = 0x00010002L;;

let fp = Printf.fprintf;;

type boolean_register = int;;

type multilabelable_instruction =
  (* %opcode{M43}  *) | SWCH         (** Switch on choice register *)
;;

type labelable_instruction =
  (* %opcode{L44}  *) | LABEL        (** Label definition *)
  (* %opcode{L00}  *) | BRA          (** Unconditional branch *)
  (* %opcode{L01}  *) | BEOF         (** Branch if EOF *)
  (* %opcode{L02}  *) | BNEOF        (** Branch if not EOF *)
  (* %opcode{L03}  *) | BFC          (** Branch if fail clear *)
  (* %opcode{L04}  *) | BFS          (** Branch if fail set *)
  (* %opcode{L05}  *) | BMB          (** Branch if memo busy *)
  (* %opcode{L06}  *) | BMBF         (** Branch if memo busy or failed *)
  (* %opcode{L07}  *) | BMK          (** Branch if memo known *)
  (* %opcode{L08}  *) | BMUK         (** Branch if memo unknown *)
  (* %opcode{L09}  *) | BMF          (** Branch if memo failed *)
  (* %opcode{L10}  *) | BBRC         (** Branch if boolean register is clear *)
  (* %opcode{L11}  *) | BBRS         (** Branch if boolean register is set *)
  (* %opcode{L12}  *) | JSR          (** Subroutine call *)
  (* %opcode{L13c} *) | SBNS of char (** Scan and branch to label if character does not match *)
  (* %opcode{L14i} *) | BSLLT of int (** Branch if suffix length is less than given integer *)
  (* %opcode{L15}  *) | BNBOF        (** Branch if not BOF *)
;;

type ('nd,'at) unlabelable_instruction =
  (* %opcode{U16c} *)  | SSEQ of char               (** Scan and push true if equal, false otherwise *)
  (* %opcode{U17cc} *) | SSIR of char * char        (** Scan and push true if char in range, false otherwise *)
  (* %opcode{U18} *)   | BTRUE                      (** Push true on boolean stack *)
  (* %opcode{U19} *)   | BFALSE                     (** Push false on boolean stack *)
  (* %opcode{U20} *)   | BAND                       (** Push conjunction of top two boolean values *)
  (* %opcode{U21} *)   | BOR                        (** Push disjunction of top two boolean values *)
  (* %opcode{U22} *)   | BNOT                       (** Push negation of the top boolean value *)
  (* %opcode{U23} *)   | SETF                       (** Set the fail flag *)
  (* %opcode{U24} *)   | CLRF                       (** Clear the fail flag *)
  (* %opcode{U25i} *)  | RIGHT of int               (** Advance the head *)
  (* %opcode{U26} *)   | PUSHP                      (** Save the head position *)
  (* %opcode{U27} *)   | POPP                       (** Restore the head position and pop it from the stack*)
  (* %opcode{U28} *)   | RESTP                      (** Restore the head position *)
  (* %opcode{U29} *)   | DROPP                      (** Drop head position *)
  (* %opcode{U30i} *)  | LDMEM of int               (** Get the memo entry for the given line *)
  (* %opcode{U31i} *)  | LDCH of int                (** Get the choice entry for the given line *)
  (* %opcode{U32i} *)  | POPSTMEMJ of int           (** Store the position in the memo entry at the position popped from the stack *)
  (* %opcode{U33i} *)  | STMEMB of int              (** Mark the memo entry as busy *)
  (* %opcode{U34i} *)  | STMEMF of int              (** Mark the memo entry as failed *)
  (* %opcode{U35ii} *) | TOPSTCH of int * int       (** Store the value into the choice register *)
  (* %opcode{U36} *)   | JMEM                       (** Set head position to memo *)
  (* %opcode{U37} *)   | RTS                        (** Return from subroutine *)
  (* %opcode{U38n} *)  | SNODE of 'nd               (** Start a new node *)
  (* %opcode{U39} *)   | FNODE                      (** Finish the node using and append it to the previous
                                                       construction, making the previous construction current. *)
  (* %opcode{U40a} *)  | ATTR of 'at                (** Add an attribute of the given name whose value is the input between the saved position
                                                       and the head position to the current construction *)
  (* %opcode{U41a} *)  | POSATTR of 'at             (** Add an attribute of the given name whose value is the current input position *)
  (* %opcode{U42} *)   | TOKEN                      (** Build a token between the memo register and the head position and add it to the current construction *)
  (* %opcode{U45ii} *) | TSSEQ of int * int         (** Scan, translate and push true if result in equal *)
  (* %opcode{U46as} *) | STRATTR of 'at * string    (** Add an attribute of the given name whose value is the given constant string *)
;;

type ('nd,'at,'label) instruction =
  | L of 'label * labelable_instruction
  | U of ('nd,'at) unlabelable_instruction
  | M of 'label array * multilabelable_instruction
;;

type ('nd,'at,'label) basic_block = {
    bb_body : ('nd,'at,'label) instruction list;
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
  | TSSEQ(ti, k) -> fp oc "TSSEQ(%d, %d)" ti k
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
  | FNODE -> fp oc "FNODE"
  | SNODE n -> fp oc "SNODE %a" print_node n
  | ATTR a -> fp oc "ATTR %a" print_attr a
  | POSATTR a -> fp oc "POSATTR %a" print_attr a
  | TOKEN -> fp oc "TOKEN"
  | STRATTR(a, u) -> fp oc "STRATTR %a %S" print_attr a u
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
