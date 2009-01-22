/* peg.c
 *
 * Copyright (C)2007-2009 Exalead
 */

#include <stdlib.h>
#include <alloc.h>
#include <peg_lib.h>
#include <peg.h>
#include <assert.h>

peg_context_t *peg_create_context(alloc_t *alloc, nog_program_t *pg, peg_builder_t *pb, info bi, letter_t *input, int input_length)
{
  int i;
  peg_context_t *cx;
  int num_alternatives;
  int num_productions;
  alloc_t *salloc;

  cx = alloc_malloc(alloc, sizeof(peg_context_t));

  cx->cx_alloc = alloc;
  cx->cx_table_staloc = staloc_create(alloc);
  salloc = &cx->cx_table_staloc->s_alloc;

  cx->cx_input = input;
  cx->cx_input_length = input_length;

  num_alternatives = pg->np_num_choices;
  num_productions = pg->np_num_productions;

  cx->cx_num_alternatives = num_alternatives;
  cx->cx_num_productions = num_productions;

  cx->cx_choices = alloc_malloc(salloc, sizeof(memo_block_t *) * (input_length + 1));

  for(i = 0; i <= input_length; i ++) {
    cx->cx_choices[i] = 0;
  }

  cx->cx_results = alloc_malloc(alloc, sizeof(memo_block_t *) * (input_length + 1));
  for(i = 0; i <= input_length; i ++) {
    cx->cx_results[i] = 0;
  }

  cx->cx_builder = pb;
  cx->cx_builder_info = bi;

  cx->cx_stack = pushdown_create(alloc);

  return cx;
}

void peg_delete_context(peg_context_t *cx)
{
  if(cx) {
    staloc_dispose(cx->cx_table_staloc);
    pushdown_dispose(cx->cx_stack);
    alloc_free(cx->cx_alloc, (cx));
  }
}
