/* cnog.c
 *
 */

#include <cnog.h>
#include <cnog_unpack.h>
#include <peg.h>
#include <pack.h>
#include <assert.h>
#include <alloc.h>

#if 0
#define DEBUGF(x, y...) printf(x, ##y);
#else
#define DEBUGF(x,...)
#endif

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
      if(i >= R_EOF) {
        i += m;
        if(i > max_j)
          max_j = i;
      }
    }
  }

  return max_j;
}/*}}}*/
bool cnog_execute(peg_context_t *cx, nog_program_t *pg, tree *result)/*{{{*/
{
  bool fail;                /* Failure register */
  unsigned int boolean;     /* Small stack for evaluating boolean formulas */
  memo_t memo;              /* Register for accessing the memo table */
  choice_t choice;          /* Register for accessing the choice table */
  symbol_t *sp;             /* Symbol stack pointer (PC stack pointer is host machine stack) */
  letter_t *head, *bof, *eof; /* Pointers to current position, beginning and end. */
  peg_builder_t *bd;
  info bi;

  /* Initialize to defined values */
  void init(void) {/*{{{*/
    boolean = 0;
    fail = false;
    choice = 0;
    memo = R_UNKNOWN;
    head = cx->cx_input;
    bof = head;
    eof = cx->cx_input + cx->cx_input_length;
    bd = cx->cx_builder;
    bi = cx->cx_builder_info;
    sp = cx->cx_stack;
  }/*}}}*/

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
    assert(sp - cx->cx_stack < cx->cx_stack_size);
    *(sp ++) = x;
  }/*}}}*/
  symbol_t stack_pop(void) {/*{{{*/
    assert(sp > cx->cx_stack);
    return *(-- sp);
  }/*}}}*/
  symbol_t stack_top(void) {/*{{{*/
    assert(sp > cx->cx_stack);
    return sp[-1];
  }/*}}}*/

  /* Execution loop */
  nog_instruction_t *run(construction current, nog_instruction_t *ip_next, tree *result_tree) {/*{{{*/
    nog_instruction_t *ip;

    int arg0() { return ip->ni_arg[0].na_int; }
    int arg1() { return ip->ni_arg[1].na_int; }
    void jump_to(int pc) { ip_next = pg->np_program + pc; }
    void jump(void) { jump_to(arg0()); } 

    if(!ip_next) return 0;

    for(;;) {
      ip = ip_next;

      assert(pg->np_program <= ip && ip < pg->np_program + pg->np_count);
      /*printf("pc=%ld i=%ld sp=%ld fail=%d memo=%d\n", ip - pg->np_program, head - bof, sp - cx->cx_stack, fail, memo);*/
      DEBUGF("%ld %ld %d\n", ip - pg->np_program, head - bof, fail);

      ip_next = ip + 1;

      switch(ip->ni_opcode) {
        case NOG_BRA:
          jump();
          break;

        case NOG_BEOF:
          if(head == eof) jump();
          break;

        case NOG_BNEOF:
          if(head < eof) jump();
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
          (void) run(current, pg->np_program + arg0(), result_tree);
          break;

        case NOG_SBNS:
          if(head < eof && *head == arg1()) {
            head ++;
          } else {
            jump();
          }
          break;

        case NOG_BSLLT:
          if(eof - head < arg1()) jump(); /* XXX */
          break;

        case NOG_BNBOF:
          if(head != bof) jump();
          break;

        case NOG_SSEQ:
          boolean_push(head < eof && *head == arg0());
          break;

        case NOG_SSIR:
          boolean_push(head < eof && arg0() <= *head && *head <= arg1());
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
          break;

        case NOG_JMEM:
          head = bof + memo;
          break;

        case NOG_RTS:
          return ip_next;

        case NOG_SWCH:
          assert(choice < ip->ni_arg[0].na_table.nt_length);
          jump_to(ip->ni_arg[0].na_table.nt_elements[choice]);
          break;

        case NOG_LABEL:
          break;

        /* Construction */
        case NOG_SNODE:
          {
            int id;
            unsigned char *name;
            construction new_cons;
            tree new_tree;

            id = arg0();
            name = pg->np_constructors[id].ns_chars;
            new_cons = bd->pb_start_construction(bi, id, name, head - bof);
            ip_next = run(new_cons, ip_next, &new_tree);
            if(!ip_next || !new_tree) {
              return 0; /* XXX */
            }
            /* new_tree = bd->pb_finish_construction(bi, new_cons); */
            if(!bd->pb_add_children(bi, current, new_tree)) return 0;
          }
          break;

        case NOG_FNODE:
          if(result_tree) {
            *result_tree = bd->pb_finish_construction(bi, current, head - bof);
          } else {
            printf("no result tree\n");
          }
          return ip_next;

        case NOG_ATTR:
          {
            int id;
            unsigned char *name;

            id = arg0();
            name = pg->np_attributes[id].ns_chars;

            if(!bd->pb_add_attribute(bi, current, id, name, head - bof, memo)) return 0;
          }
          break;

        case NOG_POSATTR:
          {
            int id;
            unsigned char *name;

            id = arg0();
            name = pg->np_attributes[id].ns_chars;

            if(!bd->pb_add_attribute(bi, current, id, name, head - bof, head - bof - 1)) return 0;
          }
          break;

        case NOG_TOKEN:
          if(!bd->pb_add_token(bi, current, head - bof, memo)) return 0;
          break;

      }
    }
  }/*}}}*/

  init();
  if(run(0, pg->np_program + pg->np_start_pc, 0)) {
    if(!fail) {
      /* Input parses.  Now construct a tree. */
      construction root;

      if(result) {
        init();
        root = bd->pb_start_construction(bi, pg->np_root_constructor, pg->np_constructors[pg->np_root_constructor].ns_chars, 0);
        (void) run(root, pg->np_program + pg->np_build_pc, 0);
        *result = bd->pb_finish_construction(bi, root, head - bof);
      }
      return true; /* Can't fail (?) XXX */
    }
  }
  return false;
}/*}}}*/
nog_program_t *cnog_unpack_program(alloc_t *alloc, packer_t *pk) {/*{{{*/
  nog_program_t *pg, *result;
  uint64_t signature, version; 
  uint64_t size;
  int i;

  nog_program_t *fail(void) { return 0; }

  result = 0;
  
  pg = alloc_malloc(alloc, sizeof(nog_program_t));

  /* Welcome to C allocation hell! */
  if(!pg) return fail();
  DEBUGF("Allocated program\n");

  if(!pack_read_uint64(pk, &signature)) return fail();
  DEBUGF("Read signature %lx\n", signature);

  if(signature != NOG_SIGNATURE) return fail();
  DEBUGF("Signature OK\n");

  if(!pack_read_uint64(pk, &version)) return fail();
  DEBUGF("Version OK\n");

  if(!pack_read_uint(pk, &pg->np_start_pc)) return fail();
  DEBUGF("Start pc is %d\n", pg->np_start_pc);

  if(!pack_read_uint(pk, &pg->np_build_pc)) return fail();
  DEBUGF("Build pc is %d\n", pg->np_build_pc);

  if(!pack_read_uint(pk, &pg->np_root_constructor)) return fail();
  DEBUGF("Root constructor is %d\n", pg->np_root_constructor);

  if(!pack_read_uint(pk, &pg->np_num_productions)) return fail();
  DEBUGF("Num_productions is %d\n", pg->np_num_productions);

  if(!pack_read_uint(pk, &pg->np_num_choices)) return fail();
  DEBUGF("Num_choices is %d\n", pg->np_num_choices);

  if(!pack_read_uint(pk, &pg->np_num_constructors)) return fail();
  DEBUGF("Num_constructors is %d\n", pg->np_num_constructors);

  pg->np_constructors = alloc_malloc(alloc, sizeof(nog_string_t) * pg->np_num_constructors);
  if(!pg->np_constructors) return fail();

  for(i = 0; i < pg->np_num_constructors; i ++) {
    if(!pack_read_string(pk, &pg->np_constructors[i].ns_chars, &size, alloc)) return fail();
    pg->np_constructors[i].ns_length = size;
    DEBUGF("  Constructor #%d: %s\n", i, pg->np_constructors[i].ns_chars);
  }

  if(!pack_read_uint(pk, &pg->np_num_attributes)) return fail();

  DEBUGF("Num_attributes is %d\n", pg->np_num_attributes);
  pg->np_attributes = alloc_malloc(alloc, sizeof(nog_string_t) * pg->np_num_attributes);
  if(!pg->np_attributes) return fail();

  for(i = 0; i < pg->np_num_attributes; i ++) {
    if(!pack_read_string(pk, &pg->np_attributes[i].ns_chars, &size, alloc)) return fail();
    pg->np_attributes[i].ns_length = size;
    DEBUGF("  Attribute #%d: %s\n", i, pg->np_attributes[i].ns_chars);
  }
  
  if(!pack_read_uint(pk, &pg->np_count)) return fail();
  DEBUGF("Program size is %d\n", pg->np_count);

  pg->np_program = alloc_malloc(alloc, sizeof(nog_instruction_t) * pg->np_count);
  if(!pg->np_program) return fail();

  for(i = 0; i < pg->np_count; i ++) {
    if(!cnog_unpack_instruction(alloc, pk, pg->np_program + i)) {
      fprintf(stderr, "Unpack error at instruction %d\n", i);
      return fail();
    }
  }

  result = pg;

  return result;
}/*}}}*/
void cnog_free_program(alloc_t *alloc, nog_program_t *pg)/*{{{*/
{
  int i;

  if(pg) {
    if(pg->np_program) {
      for(i = 0; i < pg->np_count; i ++) {
        cnog_free_instruction(alloc, pg->np_program + i);
      }
      alloc_free(alloc, pg->np_program);
    }
    alloc_free(alloc, pg);
  }
}/*}}}*/
