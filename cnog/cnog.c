/* cnog.c
 *
 */

#include <cnog.h>
#include <peg.h>

int cnog_error_position(peg_context_t *cx, nog_program_t *pg)/*{{{*/
{
  int i, j, k;
  int m;
  int max_j;

  max_j = 0;
  m = cx->cx_input_length;

  for(j = 0; j < m; j ++) {
    for(k = 0; k < pg->np_num_productions; k ++) {
      i = cx->cx_results[k][j - m];
      if(i <= R_EOF) {
        i += m;
        if(i > max_j)
          max_j = i;
      }
    }
  }

  return max_j;
}/*}}}*/
bool cnog_execute(peg_context_t *cx, nog_program_t *pg, bool build)/*{{{*/
{
  bool fail;                /* Failure register */
  unsigned int boolean;     /* Small stack for evaluating boolean formulas */
  memo_t memo;              /* Register for accessing the memo table */
  choice_t choice;          /* Register for accessing the choice table */
  symbol_t *sp;             /* Symbol stack pointer (PC stack pointer is host machine stack) */
  letter_t *head, *bof, *eof; /* Pointers to current position, beginning and end. */

  /* Initialize to defined values */
  boolean = 0;
  fail = false;
  choice = 0;
  memo = 0;
  head = cx->cx_input;
  bof = head - 1;
  eof = cx->cx_input + cx->cx_input_length;

  /* Boolean stack manipulation */
  void boolean_push(bool x) {/*{{{*/
    boolean <<= 1;
    boolean |= x ? 1 : 0;
  }/*}}}*/
  bool boolean_pop(void) {/*{{{*/
    bool result;

    result = boolean & 1;

    boolean >>= 1;
    return result;
  }/*}}}*/

  /* Regular stack manipulation */
  void stack_push(symbol_t x) {/*{{{*/
    *(sp ++) = x;
  }/*}}}*/
  symbol_t stack_pop(void) {/*{{{*/
    return *(-- sp);
  }/*}}}*/
  symbol_t stack_top(void) {/*{{{*/
    return *sp;
  }/*}}}*/

  /* Execution loop */
  void run(nog_instruction_t *ip) {/*{{{*/
    nog_instruction_t *ip_next;

    int arg0() {
      return ip->ni_arg[0].na_int;
    }

    int arg1() {
      return ip->ni_arg[1].na_int;
    }

    void jump(void) {
      ip_next = pg->np_program + arg0();
    }

    for(;;) {
      ip = ip_next;
      ip_next = ip + 1;

      switch(ip->ni_opcode) {
        case NOG_BRA:
          jump();
          break;

        case NOG_BEOF:
          if(head == eof) jump();
          break;

        case NOG_BNEOF:
          if(head != eof) jump();
          break;

        case NOG_BFC:
          if(!fail) jump();
          break;

        case NOG_BFS:
          if(fail) jump();
          break;

        case NOG_BMB:
          if(memo == R_BUSY) jump();
          break;

        case NOG_BMBF:
          if(memo == R_BUSY || memo == R_FAIL) jump();
          break;

        case NOG_BMK:
          if(memo != R_UNKNOWN) jump();
          break;

        case NOG_BMUK:
          if(memo == R_UNKNOWN) jump();
          break;

        case NOG_BMF:
          if(memo == R_FAIL) jump();
          break;

        case NOG_BBRC:
          if(!boolean_pop()) jump();
          break;
          
        case NOG_BBRS:
          if(boolean_pop()) jump();
          break;

        case NOG_JSR:
          run(pg->np_program + arg0());
          break;

        case NOG_SBNS:
          if(bof < head && head < eof && *head == arg0()) {
            head ++;
            jump();
          }
          break;

        case NOG_BSLLT:
          if(eof - head < ip->ni_arg[0].na_int) jump(); /* XXX */
          break;

        case NOG_BNBOF:
          if(head != bof) jump();
          break;

        case NOG_SSEQ:
          stack_push((bof < head && head < eof && *head == arg0()) ? true : false);
          break;

        case NOG_SSIR:
          stack_push((bof < head && head < eof && arg0() <= *head && *head <= arg0()) ? true : false);
          break;

        case NOG_BTRUE:
          boolean_push(true);
          break;

        case NOG_BFALSE:
          boolean_push(false);
          break;

        case NOG_BAND:
          {
            bool b1, b2;

            b1 = boolean_pop();
            b2 = boolean_pop();
            boolean_push(b1 && b2);
          }
          break;

        case NOG_BOR:
          {
            bool b1, b2;

            b1 = boolean_pop();
            b2 = boolean_pop();
            boolean_push(b1 || b2);
          }
          break;

        case NOG_BNOT:
          boolean_push(!boolean_pop());
          break;

        case NOG_SETF:
          fail = true;
          break;

        case NOG_CLRF:
          fail = false;
          break;

        case NOG_RIGHT:
          head += arg0();
          break;

        case NOG_PUSHP:
          stack_push(head - bof);
          break;

        case NOG_POPP:
          head = bof + stack_pop();
          break;

        case NOG_RESTP:
          head = bof + stack_top();
          break;

        case NOG_DROPP:
          (void) stack_pop();
          break;

        case NOG_LDMEM:
          memo = cx->cx_results[arg0()][head - bof];
          break;

        case NOG_LDCH:
          choice = cx->cx_alternatives[arg0()][head - bof];
          break;

        case NOG_POPSTMEMJ:
          {
            int position;

            position = stack_pop();
            cx->cx_results[arg0()][position] = head - bof;
          }
          break;

        case NOG_STMEMB:
          cx->cx_results[arg0()][head - bof] = R_BUSY;
          break;

        case NOG_STMEMF:
          cx->cx_results[arg0()][head - bof] = R_FAIL;
          break;

        case NOG_TOPSTCH:
          {
            int position;

            position = stack_top();
            cx->cx_alternatives[arg0()][position] = arg1();
          }

        case NOG_JMEM:
          head = bof + memo;
          break;

        case NOG_RTS:
          return;

        /* Construction */
        case NOG_PCN:
        case NOG_NODE:
        case NOG_ATTR:
        case NOG_POSATTR:
        case NOG_TOKEN:
          break;
      }
    }
  }/*}}}*/

  sp = cx->cx_stack;
  run(pg->np_program + (build ? pg->np_build_pc : pg->np_start_pc));

  return fail;
}/*}}}*/
