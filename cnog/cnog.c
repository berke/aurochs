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
      i = cx->cx_results[k][j];
      if(i >= R_EOF) {
        if(i > max_j)
          max_j = i;
      }
    }
  }

  return max_j;
}/*}}}*/

typedef struct {
  peg_context_t *cx;
  nog_program_t *pg;
  tree *result;
  bool fail;                /* Failure register */
  unsigned int boolean;     /* Small stack for evaluating boolean formulas */
  memo_t memo;              /* Register for accessing the memo table */
  choice_t choice;          /* Register for accessing the choice table */
  symbol_t *sp;             /* Symbol stack pointer (PC stack pointer is host machine stack) */
  letter_t *head, *bof, *eof; /* Pointers to current position, beginning and end. */
  peg_builder_t *bd;
  info bi;
} cnog_closure_t;

/* Initialize to defined values */
static void init(cnog_closure_t *c, peg_context_t *cx, nog_program_t *pg, tree *result) {/*{{{*/
  c->cx = cx;
  c->pg = pg;
  c->result = result;
  c->boolean = 0;
  c->fail = false;
  c->choice = 0;
  c->memo = R_UNKNOWN;
  c->head = cx->cx_input;
  c->bof = c->head;
  c->eof = cx->cx_input + cx->cx_input_length;
  c->bd = cx->cx_builder;
  c->bi = cx->cx_builder_info;
  c->sp = cx->cx_stack;
}/*}}}*/
/* Boolean stack manipulation */
static void boolean_push(cnog_closure_t *c, bool x) {/*{{{*/
  c->boolean <<= 1;
  c->boolean |= x ? 1 : 0;
}/*}}}*/
static bool boolean_pop(cnog_closure_t *c) {/*{{{*/
  bool result;

  result = c->boolean & 1;
  c->boolean >>= 1;
  return result;
}/*}}}*/
/* Regular stack manipulation */
static void stack_push(cnog_closure_t *c, symbol_t x) {/*{{{*/
  assert(c->sp - c->cx->cx_stack < c->cx->cx_stack_size);
  *(c->sp ++) = x;
}/*}}}*/
static symbol_t stack_pop(cnog_closure_t *c) {/*{{{*/
  assert(c->sp > c->cx->cx_stack);
  return *(-- c->sp);
}/*}}}*/
static symbol_t stack_top(cnog_closure_t *c) {/*{{{*/
  assert(c->sp > c->cx->cx_stack);
  return c->sp[-1];
}/*}}}*/
/* Execution loop */
#define arg0() (ip->ni_arg[0].na_int)
#define arg1() (ip->ni_arg[1].na_int)
#define jump_to(pc) do { ip_next = c->pg->np_program + pc; } while(0)
#define jump() do { jump_to(arg0()); } while(0)

static nog_instruction_t *run(cnog_closure_t *c, construction current, nog_instruction_t *ip_next, tree *result_tree) {/*{{{*/
  nog_instruction_t *ip;

  /*printf("run pc=%ld i=%ld c->sp=%ld c->fail=%d c->memo=%d\n", ip_next - pg->np_program, c->head - c->bof, c->sp - c->cx->cx_stack, c->fail, c->memo);*/
  if(!ip_next) return 0;

  for(;;) {
    ip = ip_next;

    assert(c->pg->np_program <= ip && ip < c->pg->np_program + c->pg->np_count);
    assert(c->bof <= c->head && c->head <= c->eof);
    /*printf("pc=%ld i=%ld c->sp=%ld c->fail=%d c->memo=%d\n", ip - c->pg->np_program, c->head - c->bof, c->sp - c->cx->cx_stack, c->fail, c->memo);*/
    DEBUGF("%ld %ld %d\n", ip - c->pg->np_program, c->head - c->bof, c->fail);

    ip_next = ip + 1;

    switch(ip->ni_opcode) {
      case NOG_BRA:
        jump();
        break;

      case NOG_BEOF:
        if(c->head == c->eof) jump();
        break;

      case NOG_BNEOF:
        if(c->head < c->eof) jump();
        break;

      case NOG_BFC:
        if(!c->fail) jump();
        break;

      case NOG_BFS:
        if(c->fail) jump();
        break;

      case NOG_BMB:
        if(c->memo == R_BUSY) jump();
        break;

      case NOG_BMBF:
        if(c->memo == R_BUSY || c->memo == R_FAIL) jump();
        break;

      case NOG_BMK:
        if(c->memo != R_UNKNOWN) jump();
        break;

      case NOG_BMUK:
        if(c->memo == R_UNKNOWN) jump();
        break;

      case NOG_BMF:
        if(c->memo == R_FAIL) jump();
        break;

      case NOG_BBRC:
        if(!boolean_pop(c)) jump();
        break;
        
      case NOG_BBRS:
        if(boolean_pop(c)) jump();
        break;

      case NOG_JSR:
        (void) run(c, current, c->pg->np_program + arg0(), result_tree);
        break;

      case NOG_SBNS:
        if(c->head < c->eof && *c->head == arg1()) {
          c->head ++;
        } else {
          jump();
        }
        break;

      case NOG_BSLLT:
        if(c->eof - c->head < arg1()) jump(); /* XXX */
        break;

      case NOG_BNBOF:
        if(c->head != c->bof) jump();
        break;

      case NOG_SSEQ:
        boolean_push(c, c->head < c->eof && *c->head == arg0());
        break;

      case NOG_TSSEQ:
        boolean_push(c, c->head < c->eof && c->pg->np_tables[arg0()].nt_entries[*c->head] == arg1());
        break;

      case NOG_SSIR:
        boolean_push(c, c->head < c->eof && arg0() <= *c->head && *c->head <= arg1());
        break;

      case NOG_BTRUE:
        boolean_push(c, true);
        break;

      case NOG_BFALSE:
        boolean_push(c, false);
        break;

      case NOG_BAND:
        {
          bool b1, b2;

          b1 = boolean_pop(c);
          b2 = boolean_pop(c);
          boolean_push(c, b1 && b2);
        }
        break;

      case NOG_BOR:
        {
          bool b1, b2;

          b1 = boolean_pop(c);
          b2 = boolean_pop(c);
          boolean_push(c, b1 || b2);
        }
        break;

      case NOG_BNOT:
        boolean_push(c, !boolean_pop(c));
        break;

      case NOG_SETF:
        c->fail = true;
        break;

      case NOG_CLRF:
        c->fail = false;
        break;

      case NOG_RIGHT:
        c->head += arg0();
        break;

      case NOG_PUSHP:
        stack_push(c, c->head - c->bof);
        break;

      case NOG_POPP:
        c->head = c->bof + stack_pop(c);
        break;

      case NOG_RESTP:
        c->head = c->bof + stack_top(c);
        break;

      case NOG_DROPP:
        (void) stack_pop(c);
        break;

      case NOG_LDMEM:
        assert(0 <= arg0() && arg0() < c->cx->cx_num_productions);
        c->memo = c->cx->cx_results[arg0()][c->head - c->bof];
        break;

      case NOG_LDCH:
        assert(0 <= arg0() && arg0() < c->cx->cx_num_alternatives);
        c->choice = c->cx->cx_alternatives[arg0()][c->head - c->bof];
        break;

      case NOG_POPSTMEMJ:
        {
          int position;

          position = stack_pop(c);
          assert(0 <= arg0() && arg0() < c->cx->cx_num_productions);
          c->cx->cx_results[arg0()][position] = c->head - c->bof;
        }
        break;

      case NOG_STMEMB:
        assert(0 <= arg0() && arg0() < c->cx->cx_num_productions);
        c->cx->cx_results[arg0()][c->head - c->bof] = R_BUSY;
        break;

      case NOG_STMEMF:
        assert(0 <= arg0() && arg0() < c->cx->cx_num_productions);
        c->cx->cx_results[arg0()][c->head - c->bof] = R_FAIL;
        break;

      case NOG_TOPSTCH:
        {
          int position;

          position = stack_top(c);
          assert(0 <= arg0() && arg0() < c->cx->cx_num_alternatives);
          c->cx->cx_alternatives[arg0()][position] = arg1();
        }
        break;

      case NOG_JMEM:
        c->head = c->bof + c->memo;
        break;

      case NOG_RTS:
        return ip_next;

      case NOG_SWCH:
        assert(c->choice < ip->ni_arg[0].na_table.nt_length);
        jump_to(ip->ni_arg[0].na_table.nt_elements[c->choice]);
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
          name = c->pg->np_constructors[id].ns_chars;
          new_cons = c->bd->pb_start_construction(c->bi, id, name, c->head - c->bof);
          ip_next = run(c, new_cons, ip_next, &new_tree);
          if(!ip_next || !new_tree) {
            return 0; /* XXX */
          }
          /* new_tree = c->bd->pb_finish_construction(c->bi, new_cons); */
          if(!c->bd->pb_add_children(c->bi, current, new_tree)) return 0;
        }
        break;

      case NOG_FNODE:
        if(result_tree) {
          *result_tree = c->bd->pb_finish_construction(c->bi, current, c->head - c->bof);
        } else {
          printf("no c->result tree\n");
        }
        return ip_next;

      case NOG_ATTR:
        {
          int id;
          unsigned char *name;

          id = arg0();
          name = c->pg->np_attributes[id].ns_chars;

          if(!c->bd->pb_add_attribute(c->bi, current, id, name, c->head - c->bof, c->memo)) return 0;
        }
        break;

      case NOG_POSATTR:
        {
          int id;
          unsigned char *name;

          id = arg0();
          name = c->pg->np_attributes[id].ns_chars;

          if(!c->bd->pb_add_attribute(c->bi, current, id, name, c->head - c->bof, c->head - c->bof - 1)) return 0;
        }
        break;

      case NOG_TOKEN:
        if(!c->bd->pb_add_token(c->bi, current, c->head - c->bof, c->memo)) return 0;
        break;

    }
  }
}/*}}}*/

#undef arg0
#undef arg1
#undef jump_to
#undef jump

#define CNOG_VERSION 0x00010001

bool cnog_execute(peg_context_t *cx, nog_program_t *pg, tree *result)/*{{{*/
{
  cnog_closure_t c;

  init(&c, cx, pg, result);
  if(run(&c, 0, pg->np_program + pg->np_start_pc, 0)) {
    if(!c.fail) {
      /* Input parses.  Now construct a tree. */
      construction root;

      if(c.result) {
        init(&c, cx, pg, result);
        root = c.bd->pb_start_construction(c.bi, pg->np_root_constructor, pg->np_constructors[pg->np_root_constructor].ns_chars, 0);
        (void) run(&c, root, pg->np_program + pg->np_build_pc, 0);
        *result = c.bd->pb_finish_construction(c.bi, root, c.head - c.bof);
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
  int i, j;

  DEBUGF("Unpacking\n");
  result = 0;
  
  pg = alloc_malloc(alloc, sizeof(nog_program_t));

  /* Welcome to C allocation hell! */
  if(!pg) goto finish;
  DEBUGF("Allocated program\n");

  if(!pack_read_uint64(pk, &signature)) goto finish;
  DEBUGF("Read signature %lx\n", signature);

  if(signature != NOG_SIGNATURE) goto finish;
  DEBUGF("Signature OK\n");

  if(!pack_read_uint64(pk, &version)) goto finish;
  if(version <= CNOG_VERSION) {
    DEBUGF("Version too recent\n");
    goto finish;
  }
  DEBUGF("Version OK\n");

  if(!pack_read_uint(pk, &pg->np_start_pc)) goto finish;
  DEBUGF("Start pc is %d\n", pg->np_start_pc);

  if(!pack_read_uint(pk, &pg->np_build_pc)) goto finish;
  DEBUGF("Build pc is %d\n", pg->np_build_pc);

  if(!pack_read_uint(pk, &pg->np_root_constructor)) goto finish;
  DEBUGF("Root constructor is %d\n", pg->np_root_constructor);

  if(!pack_read_uint(pk, &pg->np_num_productions)) goto finish;
  DEBUGF("Num_productions is %d\n", pg->np_num_productions);

  if(!pack_read_uint(pk, &pg->np_num_choices)) goto finish;
  DEBUGF("Num_choices is %d\n", pg->np_num_choices);

  if(!pack_read_uint(pk, &pg->np_num_constructors)) goto finish;
  DEBUGF("Num_constructors is %d\n", pg->np_num_constructors);

  pg->np_constructors = alloc_malloc(alloc, sizeof(nog_string_t) * pg->np_num_constructors);
  if(!pg->np_constructors) goto finish;

  for(i = 0; i < pg->np_num_constructors; i ++) {
    if(!pack_read_string(pk, &pg->np_constructors[i].ns_chars, &size, alloc)) goto finish;
    pg->np_constructors[i].ns_length = size;
    DEBUGF("  Constructor #%d: %s\n", i, pg->np_constructors[i].ns_chars);
  }

  if(!pack_read_uint(pk, &pg->np_num_attributes)) goto finish;

  DEBUGF("Num_attributes is %d\n", pg->np_num_attributes);
  pg->np_attributes = alloc_malloc(alloc, sizeof(nog_string_t) * pg->np_num_attributes);
  if(!pg->np_attributes) goto finish;

  for(i = 0; i < pg->np_num_attributes; i ++) {
    if(!pack_read_string(pk, &pg->np_attributes[i].ns_chars, &size, alloc)) goto finish;
    pg->np_attributes[i].ns_length = size;
    DEBUGF("  Attribute #%d: %s\n", i, pg->np_attributes[i].ns_chars);
  }

  if(!pack_read_uint(pk, &pg->np_num_tables)) goto finish;

  DEBUGF("Num_tables is %d\n", pg->np_num_tables);
  pg->np_tables = alloc_malloc(alloc, sizeof(nog_table_t) * pg->np_num_tables);
  if(!pg->np_tables) goto finish;

  for(i = 0; i < pg->np_num_tables; i ++) {
    if(!pack_read_uint(pk, &pg->np_tables[i].nt_classes)) goto finish;

    for(j = 0; j < NOG_TABLE_ENTRIES; j ++) {
      if(!pack_read_uint(pk, pg->np_tables[i].nt_entries + j)) goto finish;
    }
  }
  
  if(!pack_read_uint(pk, &pg->np_count)) goto finish;
  DEBUGF("Program size is %d\n", pg->np_count);

  pg->np_program = alloc_malloc(alloc, sizeof(nog_instruction_t) * pg->np_count);
  if(!pg->np_program) goto finish;

  for(i = 0; i < pg->np_count; i ++) {
    if(!cnog_unpack_instruction(alloc, pk, pg->np_program + i)) {
      fprintf(stderr, "Unpack error at instruction %d\n", i);
      goto finish;
    }
  }

  result = pg;

finish:
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
