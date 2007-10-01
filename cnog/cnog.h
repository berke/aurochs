/* cnog.h
 *
 */

#ifndef CNOG_H
#define CNOG_H

#include <base_types.h>
#include <peg.h>
#include <pack.h>

typedef enum {
  NOG_LABEL,        /* Label (ignored, just for debugging) */
  NOG_BRA,          /* Unconditional branch */
  NOG_BEOF,         /* Branch if EOF */
  NOG_BNEOF,        /* Branch if not EOF */
  NOG_BFC,          /* Branch if fail clear */
  NOG_BFS,          /* Branch if fail set */
  NOG_BMB,          /* Branch if memo busy */
  NOG_BMBF,         /* Branch if memo busy or failed */
  NOG_BMK,          /* Branch if memo known */
  NOG_BMUK,         /* Branch if memo unknown */
  NOG_BMF,          /* Branch if memo failed */
  NOG_BBRC,         /* Branch if boolean register is clear */
  NOG_BBRS,         /* Branch if boolean register is set */
  NOG_JSR,          /* Subroutine call */
  NOG_SBNS,         /* Scan and branch to label if character does not match */
  NOG_BSLLT,        /* Branch if suffix length is less than given integer */
  NOG_BNBOF,        /* Branch if not BOF */
  NOG_SSEQ,         /* Scan and push true if not equal, false otherwise */
  NOG_SSIR,         /* Scan and push true if char in range, false otherwise */
  NOG_BTRUE,        /* Push true on boolean stack */
  NOG_BFALSE,       /* Push false on boolean stack */
  NOG_BAND,         /* Push conjunction of top two boolean values */
  NOG_BOR,          /* Push disjunction of top two boolean values */
  NOG_BNOT,         /* Push negation of the top boolean value */
  NOG_SETF,         /* Set the fail flag */
  NOG_CLRF,         /* Clear the fail flag */
  NOG_RIGHT,        /* Advance the head */
  NOG_PUSHP,        /* Save the head position */
  NOG_POPP,         /* Restore the head position and pop it from the stack*/
  NOG_RESTP,        /* Restore the head position */
  NOG_DROPP,        /* Drop head position */
  NOG_LDMEM,        /* Get the memo entry for the given line */
  NOG_LDCH,         /* Get the choice entry for the given line */
  NOG_POPSTMEMJ,    /* Store the position in the memo entry at the position popped from the stack */
  NOG_STMEMB,       /* Mark the memo entry as busy */
  NOG_STMEMF,       /* Mark the memo entry as failed */
  NOG_TOPSTCH,      /* Store the value into the choice register */
  NOG_JMEM,         /* Set head position to memo */
  NOG_RTS,          /* Return from subroutine */
  NOG_SNODE,        /* Push current construction and create a new one */
  NOG_FNODE,        /* Build a new node using the current construction and append it to the previous construction, */
  NOG_ATTR,         /* Add an attribute of the given name whose value is the input between the saved position and, */
  NOG_POSATTR,      /* Add an attribute of the given name whose value is the current input position */
  NOG_TOKEN,        /* Build a token between the memo register and the head position and add it to the current construction */
  NOG_SWCH,
} nog_opcode_t;

typedef struct {
  unsigned char *ns_chars;
  int ns_length;
} nog_string_t;

typedef union {
  int na_int;
  nog_string_t na_string;
  struct {
    unsigned int *nt_elements;
    int nt_length;
  } na_table;
} nog_arg_t;

#define NI_MAX_ARGS 2

typedef struct {
  nog_opcode_t ni_opcode;
  nog_arg_t ni_arg[NI_MAX_ARGS];
} nog_instruction_t;

typedef struct {
  unsigned int np_count;
  unsigned int np_start_pc;
  unsigned int np_build_pc;
  unsigned int np_num_productions;
  unsigned int np_num_choices;
  unsigned int np_num_constructors;
  nog_string_t *np_constructors;
  unsigned int np_num_attributes;
  nog_string_t *np_attributes;
  nog_instruction_t *np_program;
} nog_program_t;

bool cnog_execute(peg_context_t *cx, nog_program_t *pg, tree **build_result);
int cnog_error_position(peg_context_t *cx, nog_program_t *pg);
nog_program_t *cnog_unpack_program(alloc_t *alloc, packer_t *pk);
void cnog_free_program(alloc_t *alloc, nog_program_t *pg);

#define NOG_SIGNATURE 0xABBE55E5

#endif 
