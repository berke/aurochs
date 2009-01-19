/* cnog.c
 *
 */

#include <cnog.h>
#include <cnog_unpack.h>
#include <peg.h>
#include <pack.h>
#include <assert.h>
#include <alloc.h>

#define CNOG_DEBUG 0

static inline void set_comemo_non_compact(alloc_t *alloc, comemo_t *table, int position, u32 key, u32 value)
{
  comemo_t r;

  r = table[position];
  big_comemo_t *head, *current;

  current = (big_comemo_t *) r;

  /* Check if there already is an entry. */
  while(current) {
    if(current->key == key) {
      current->value = value;
      return;
    }
    current = current->next;
  }

  /* Not already present.  Append it to the beginning of the list. */
  head = (big_comemo_t *) r;

  current = alloc_malloc(alloc, sizeof(big_comemo_t));
  current->next = head;
  current->key = key;
  current->value = value;
  table[position] = (comemo_t) current;
}

static inline void set_comemo(alloc_t *alloc, comemo_t *table, int position, u32 key, u32 value)
{
  comemo_t r;
  int n;
  bool small;
  bool compact;
  bool present;
  int index;
  
  r = table[position];
  n = r & MASK(COMEMO_TAG_BITS);

  compact = !r || n;

  small = key < COMEMO_MAX_KEY && value < COMEMO_MAX_VALUE;

  /* r = 0          -> no entries
   * r <> 0 & n = 0 -> extended
   * n <> 0         -> from 1 to COMEMO_MAX_SHORTS entries */

  index = -1;
  if(n) {
    /* We are in compact mode and there is at least one entry.  Check if the entry is already present. */

    comemo_t r_copy;
    int n_copy;
    int r_key;

    r_copy = r;
    n_copy = n;
    r_copy >>= COMEMO_TAG_BITS;

    while(n_copy --) {
      r_copy >>= COMEMO_VALUE_BITS;
      r_key = r_copy & MASK(COMEMO_KEY_BITS);
      r_copy >>= COMEMO_KEY_BITS;

      if(r_key == key) {
        index = n - n_copy - 1;
        break;
      }
    }
  }
  present = index >= 0;

  /* */
  if(!compact || !small || (!present && n == COMEMO_MAX_SHORTS)) {
    /* Cannot add short entry. */
    if(n) { 
      int r_key;
      int r_val;

      /* Entries are in compact form.  We'll need to convert them to a table. */
      table[position] = 0;

      r >>= COMEMO_TAG_BITS;
      while(n--) {
        r_val = r & MASK(COMEMO_VALUE_BITS); r >>= COMEMO_VALUE_BITS;
        r_key = r & MASK(COMEMO_KEY_BITS); r >>= COMEMO_KEY_BITS;
        if(r_key != key) {
          set_comemo_non_compact(alloc, table, position, r_key, r_val);
        }
      }
      r = 0;
    }
    
    /* Add entry to long table. */
    set_comemo_non_compact(alloc, table, position, key, value);
  } else {
    int offset;

    /* Add or replace short entry */
    if(index < 0) index = n;
    offset = index * (COMEMO_KEY_BITS + COMEMO_VALUE_BITS) + COMEMO_TAG_BITS;
    r &= ~((MASK(COMEMO_KEY_BITS + COMEMO_VALUE_BITS)) << offset);
    r |= ((u64) key) << (COMEMO_VALUE_BITS + offset);
    r |= ((u64) value) << offset;

    if(!present) r++; /* Won't overflow, by hypothesis */
    /* assert((r & MASK(COMEMO_TAG_BITS))); */
    table[position] = r;
  }

  return;
}

static inline int get_comemo(comemo_t *table, int position, int key, unsigned int default_value)
{
  comemo_t r;
  int n;
  
  r = table[position];
  n = r & MASK(COMEMO_TAG_BITS);

  if(n) {
    int r_key;
    int r_val;
    r >>= COMEMO_TAG_BITS;

    while(n--) {
      r_val = r & MASK(COMEMO_VALUE_BITS); r >>= COMEMO_VALUE_BITS;
      r_key = r & MASK(COMEMO_KEY_BITS); r >>= COMEMO_KEY_BITS;
      if(r_key == key) {
        return r_val;
      }
    }
  } else if(r) {
    big_comemo_t *br;
    big_comemo_t *head;

    head = (big_comemo_t *) r;
    br = head;

    while(br) {
      if(br->key == key) {
#if 0
        int head_val, head_key;

        /* Move to head */
        if(br != head) {
          head_key = head->key;
          head_val = head->value;

          head->key = key;
          head->value = br->value;

          br->key = head_key;
          br->value = head_val;
        }
        return head->value;
#else
        return br->value;
#endif

      } else {
        br = br->next;
      }
    }
  }
  return default_value;
}

static inline int get_choice(peg_context_t *cx, int position, int alternative)
{
  return get_comemo(cx->cx_choices, position, alternative, 0);
}

static inline void set_choice(peg_context_t *cx, int position, int alternative, int choice)
{
  return set_comemo(&cx->cx_table_stack->s_alloc, cx->cx_choices, position, alternative, choice);
}

static inline int get_result(peg_context_t *cx, int position, int production)
{
  int v;

  v = (int) get_comemo(cx->cx_results, position, production, R_UNKNOWN - R_MIN);

  if(v < - R_MIN) {
    v += R_MIN;
  } else {
    v += R_MIN + position;
  }
  return v;
}

static inline void set_result(peg_context_t *cx, int position, int production, int result)
{
  if(result < 0) {
    result = result - R_MIN; /* -4 -> 0 ; -3 -> 1 ; -2 -> 2 ; -1 -> 3 */
  } else {
    result = result - position - R_MIN; /* position+0 -> 4 ; position+1 -> 5 ... */
  }
  set_comemo(&cx->cx_table_stack->s_alloc, cx->cx_results, position, production, result);
}

int cnog_error_position(peg_context_t *cx, nog_program_t *pg)
{
  int i, j, k;
  int m;
  int max_j;

  max_j = 0;
  m = cx->cx_input_length;

  for(j = 0; j < m; j ++) {
    for(k = 0; k < (int) pg->np_num_productions; k ++) {
      i = get_result(cx, j, k);
      if(i >= R_EOF) {
        if(i > max_j)
          max_j = i;
      }
    }
  }

  return max_j;
}


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
static void init(cnog_closure_t *c, peg_context_t *cx, nog_program_t *pg, tree *result) {
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
}

/* Boolean stack manipulation */
static inline void boolean_push(cnog_closure_t *c, bool x) {
  c->boolean <<= 1;
  c->boolean |= x ? 1 : 0;
}

static inline bool boolean_pop(cnog_closure_t *c) {
  bool result;

  result = c->boolean & 1;
  c->boolean >>= 1;
  return result;
}

/* Regular stack manipulation */
static inline void stack_push(cnog_closure_t *c, symbol_t x) {
  assert(c->sp - c->cx->cx_stack < c->cx->cx_stack_size);
  *(c->sp ++) = x;
}

static inline symbol_t stack_pop(cnog_closure_t *c) {
  assert(c->sp > c->cx->cx_stack);
  return *(-- c->sp);
}

static inline symbol_t stack_top(cnog_closure_t *c) {
  assert(c->sp > c->cx->cx_stack);
  return c->sp[-1];
}

/* Execution loop */
#define arg0() (ip->ni_arg[0].na_int)
#define arg1() (ip->ni_arg[1].na_int)
#define jump_to(pc) do { ip_next = c->pg->np_program + pc; } while(0)
#define jump() do { jump_to(arg0()); } while(0)

static nog_instruction_t *run(cnog_closure_t *c, construction current, nog_instruction_t *ip_next, tree *result_tree) {
  nog_instruction_t *ip;

  /*printf("run pc=%ld i=%ld c->sp=%ld c->fail=%d c->memo=%d\n", ip_next - pg->np_program, c->head - c->bof, c->sp - c->cx->cx_stack, c->fail, c->memo);*/
  if(!ip_next) return 0;

  for(;;) {
    ip = ip_next;

    assert(c->pg->np_program <= ip && ip < c->pg->np_program + c->pg->np_count);
    assert(c->bof <= c->head && c->head <= c->eof);
    /*printf("pc=%ld i=%ld c->sp=%ld c->fail=%d c->memo=%d\n", ip - c->pg->np_program, c->head - c->bof, c->sp - c->cx->cx_stack, c->fail, c->memo);*/
    DEBUGIF(CNOG_DEBUG,"%ld %ld %d\n", ip - c->pg->np_program, c->head - c->bof, c->fail);

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
        boolean_push(c, c->head < c->eof &&
              c->pg->np_tables[arg0()].nt_entries[*c->head] == (letter_t) arg1());
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
        c->memo = get_result(c->cx, c->head - c->bof, arg0());
        break;

      case NOG_LDCH:
        assert(0 <= arg0() && arg0() < c->cx->cx_num_alternatives);
        c->choice = get_choice(c->cx, c->head - c->bof, arg0());
        break;

      case NOG_POPSTMEMJ:
        {
          int position;

          position = stack_pop(c);
          assert(0 <= arg0() && arg0() < c->cx->cx_num_productions);
          set_result(c->cx, position, arg0(), c->head - c->bof);
        }
        break;

      case NOG_STMEMB:
        assert(0 <= arg0() && arg0() < c->cx->cx_num_productions);
        set_result(c->cx, c->head - c->bof, arg0(), R_BUSY);
        break;

      case NOG_STMEMF:
        assert(0 <= arg0() && arg0() < c->cx->cx_num_productions);
        set_result(c->cx, c->head - c->bof, arg0(), R_FAIL);
        break;

      case NOG_TOPSTCH:
        {
          int position;

          position = stack_top(c);
          assert(0 <= arg0() && arg0() < c->cx->cx_num_alternatives);
          set_choice(c->cx, position, arg0(), arg1());
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
          if(!ip_next) {
            return 0;
          }
          /* new_tree = c->bd->pb_finish_construction(c->bi, new_cons); */
          if(!c->bd->pb_add_children(c->bi, current, new_tree)) return 0;
        }
        break;

      case NOG_FNODE:
        if(result_tree) {
          *result_tree = c->bd->pb_finish_construction(c->bi, current, c->head - c->bof);
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

      case NOG_STRATTR:
        {
          int id;
          unsigned char *name;

          id = arg0();
          name = c->pg->np_attributes[id].ns_chars;

          if(!c->bd->pb_add_constant_attribute(c->bi, current, id, name, ip->ni_arg[1].na_string.ns_chars, ip->ni_arg[1].na_string.ns_length)) return 0;
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
}


#undef arg0
#undef arg1
#undef jump_to
#undef jump

#define CNOG_VERSION 0x00010001

bool cnog_execute(peg_context_t *cx, nog_program_t *pg, tree *result)
{
  cnog_closure_t c;

  init(&c, cx, pg, result);
  if(run(&c, 0, pg->np_program + pg->np_start_pc, 0)) {
    if(!c.fail) {
      /* Input parses.  Now construct a tree. */
      construction root;
      /*printf("Run: %ld bytes\n", alloc_stdlib_total());*/

      if(c.result) {
        init(&c, cx, pg, result);
        root = c.bd->pb_start_construction(c.bi, pg->np_root_constructor, pg->np_constructors[pg->np_root_constructor].ns_chars, 0);
        (void) run(&c, root, pg->np_program + pg->np_build_pc, 0);
        *result = c.bd->pb_finish_construction(c.bi, root, c.head - c.bof);
      }
      /*printf("And construct: %ld bytes\n", alloc_stdlib_total());*/
      return true; /* Can't fail (?) XXX */
    }
  }
  return false;
}

static inline void cnog_add_to_checksum(void *info, u8 *data, size_t size)
{
  u64 sum;

  sum = *((u64 *) info);
  while(size > 0) {
    sum += *(data ++);
    size --;
  }
  *((u64 *) info) = sum;
}

nog_program_t *cnog_unpack_program(alloc_t *alloc, packer_t *pk) {
  nog_program_t *pg, *result;
  u64 signature, version; 
  size_t size;
  unsigned int i, j;
  u64 checksum, checksum2;

  DEBUGIF(CNOG_DEBUG,"Unpacking\n");
  result = 0;

  checksum = 0;
  pack_set_observer(pk, &checksum, cnog_add_to_checksum);
  
  pg = alloc_malloc(alloc, sizeof(nog_program_t));

  /* Welcome to C allocation hell! */
  if(!pg) goto finish;
  DEBUGIF(CNOG_DEBUG,"Allocated program\n");

  if(!pack_read_uint64(pk, &signature)) goto finish;
  DEBUGIF(CNOG_DEBUG,"Read signature %lx\n", signature);

  if(signature != NOG_SIGNATURE) goto finish;
  DEBUGIF(CNOG_DEBUG,"Signature OK\n");

  if(!pack_read_uint64(pk, &version)) goto finish;
  if(version <= CNOG_VERSION) {
    DEBUGIF(CNOG_DEBUG,"Version too recent\n");
    goto finish;
  }
  DEBUGIF(CNOG_DEBUG,"Version OK\n");

  if(!pack_read_uint(pk, &pg->np_start_pc)) goto finish;
  DEBUGIF(CNOG_DEBUG,"Start pc is %d\n", pg->np_start_pc);

  if(!pack_read_uint(pk, &pg->np_build_pc)) goto finish;
  DEBUGIF(CNOG_DEBUG,"Build pc is %d\n", pg->np_build_pc);

  if(!pack_read_uint(pk, &pg->np_root_constructor)) goto finish;
  DEBUGIF(CNOG_DEBUG,"Root constructor is %d\n", pg->np_root_constructor);

  if(!pack_read_uint(pk, &pg->np_num_productions)) goto finish;
  DEBUGIF(CNOG_DEBUG,"Num_productions is %d\n", pg->np_num_productions);

  if(!pack_read_uint(pk, &pg->np_num_choices)) goto finish;
  DEBUGIF(CNOG_DEBUG,"Num_choices is %d\n", pg->np_num_choices);

  if(!pack_read_uint(pk, &pg->np_num_constructors)) goto finish;
  DEBUGIF(CNOG_DEBUG,"Num_constructors is %d\n", pg->np_num_constructors);

  pg->np_constructors = alloc_malloc(alloc, sizeof(nog_string_t) * pg->np_num_constructors);
  if(!pg->np_constructors) goto finish;

  for(i = 0; i < pg->np_num_constructors; i ++) {
    if(!pack_read_string(pk, &pg->np_constructors[i].ns_chars, &size, alloc)) goto finish;
    pg->np_constructors[i].ns_length = size;
    DEBUGIF(CNOG_DEBUG,"  Constructor #%d: %s\n", i, pg->np_constructors[i].ns_chars);
  }

  if(!pack_read_uint(pk, &pg->np_num_attributes)) goto finish;

  DEBUGIF(CNOG_DEBUG,"Num_attributes is %d\n", pg->np_num_attributes);
  pg->np_attributes = alloc_malloc(alloc, sizeof(nog_string_t) * pg->np_num_attributes);
  if(!pg->np_attributes) goto finish;

  for(i = 0; i < pg->np_num_attributes; i ++) {
    if(!pack_read_string(pk, &pg->np_attributes[i].ns_chars, &size, alloc)) goto finish;
    pg->np_attributes[i].ns_length = size;
    DEBUGIF(CNOG_DEBUG,"  Attribute #%d: %s\n", i, pg->np_attributes[i].ns_chars);
  }

  if(!pack_read_uint(pk, &pg->np_num_tables)) goto finish;

  DEBUGIF(CNOG_DEBUG,"Num_tables is %d\n", pg->np_num_tables);
  pg->np_tables = alloc_malloc(alloc, sizeof(nog_table_t) * pg->np_num_tables);
  if(!pg->np_tables) goto finish;

  for(i = 0; i < pg->np_num_tables; i ++) {
    if(!pack_read_uint(pk, &pg->np_tables[i].nt_classes)) goto finish;

    for(j = 0; j < NOG_TABLE_ENTRIES; j ++) {
      if(!pack_read_uint(pk, pg->np_tables[i].nt_entries + j)) goto finish;
    }
  }
  
  if(!pack_read_uint(pk, &pg->np_count)) goto finish;
  DEBUGIF(CNOG_DEBUG,"Program size is %d\n", pg->np_count);

  pg->np_program = alloc_malloc(alloc, sizeof(nog_instruction_t) * pg->np_count);
  if(!pg->np_program) goto finish;

  for(i = 0; i < pg->np_count; i ++) {
    if(!cnog_unpack_instruction(alloc, pk, pg->np_program + i)) {
      DEBUGIF(CNOG_DEBUG, "Unpack error at instruction %d\n", i);
      goto finish;
    }
  }
  
  if(!pack_finish_observing(pk)) goto finish;

  if(!pack_read_uint64(pk, &checksum2)) goto finish;
  if(checksum != checksum2) {
    DEBUGIF(CNOG_DEBUG, "Bad checksum, residual 0x%lx recorded 0x%lx\n", checksum, checksum2);
    goto finish;
  } else {
    DEBUGIF(CNOG_DEBUG, "Checksum OK 0x%lx\n", checksum2);
  }
  result = pg;

finish:
  return result;
}

void cnog_free_program(alloc_t *alloc, nog_program_t *pg)
{
  unsigned int i;

  if(pg) {
    if(pg->np_program) {
      for(i = 0; i < pg->np_count; i ++) {
        cnog_free_instruction(alloc, pg->np_program + i);
      }
      alloc_free(alloc, pg->np_program);
    }
    alloc_free(alloc, pg);
  }
}
