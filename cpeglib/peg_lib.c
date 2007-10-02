/* Peg_lib */

#include <stdlib.h>
#include <stdio.h>

#include <peg.h>
#include <cnog.h>

static void *xmalloc(size_t p)/*{{{*/
{
  void *r;
  r = malloc(p);
  if(!r) abort();
  return r;
}/*}}}*/
void peg_dump_context(FILE *f, peg_context_t *cx)/*{{{*/
{
  int i,j;
  int m;

  m = cx->cx_input_length;
  fprintf(f, "Context:\n");
  fprintf(f, "  Input length: %d\n", m);
  fprintf(f, "  Number of productions: %d\n", cx->cx_num_productions);
  fprintf(f, "  Number of alternatives: %d\n", cx->cx_num_alternatives);
  fprintf(f, "  Results:\n");
  for(i = 0; i < cx->cx_num_productions; i ++) {
    fprintf(f, "    Result %d:\n", i);
    for(j = -cx->cx_input_length; j <= 0; j++) {
      fprintf(f, "      %d (%3d) : ", j + m, j);
      switch(cx->cx_results[i][j]) {
        case R_FAIL:
          fprintf(f, "FAIL\n");
          break;
        case R_BUSY:
          fprintf(f, "BUSY\n");
          break;
        case R_UNKNOWN:
          fprintf(f, "UNKNOWN\n");
          break;
        case R_EOF:
          fprintf(f, "EOF\n");
          break;
        default:
          fprintf(f, "%d\n", cx->cx_results[i][j] + m);
          break;
      }
    }
  }
  fprintf(f, "  Alternatives:\n");
  for(i = 0; i < cx->cx_num_alternatives; i ++) {
    fprintf(f, "    Alternative %d:\n", i);
    for(j = -cx->cx_input_length; j <= 0; j++) {
      fprintf(f, "      %d (%3d) : ", j + m, j);
      if(cx->cx_alternatives[i][j] == A_UNDEFINED)
        fprintf(f, "UNDEFINED\n");
      else
        fprintf(f, "%d\n", cx->cx_alternatives[i][j]);
    }
  }
}/*}}}*/
peg_context_t *peg_create_context(nog_program_t *pg, peg_builder_t *pb, info bi, letter_t *input, int input_length)/*{{{*/
{
  int i;
  peg_context_t *cx;
  choice_t *alternatives;
  result_t *results;
  int num_alternatives;
  int num_productions;

  cx = xmalloc(sizeof(peg_context_t));

  cx->cx_input = input;
  cx->cx_input_length = input_length;

  num_alternatives = pg->np_num_choices;
  num_productions = pg->np_num_productions;

  cx->cx_num_alternatives = num_alternatives;
  cx->cx_num_productions = num_productions;

  cx->cx_alternatives = xmalloc(sizeof(choice_t *) * num_alternatives);
  alternatives = xmalloc(sizeof(choice_t) * (input_length + 1) * num_alternatives);
  for(i = 0; i < num_alternatives * (input_length + 1); i ++) {
    alternatives[i] = A_UNDEFINED;
  }
  for(i = 0; i < num_alternatives; i ++) {
    cx->cx_alternatives[i] = alternatives + i * (input_length + 1);
  }

  cx->cx_results = xmalloc(sizeof(result_t *) * num_productions);
  results = xmalloc(sizeof(result_t) * (input_length + 1) * num_productions);
  for(i = 0; i < num_productions * (input_length + 1); i ++) {
    results[i] = R_UNKNOWN;
  }
  for(i = 0; i < num_productions; i ++) {
    cx->cx_results[i] = results + i * (input_length + 1);
  }

  cx->cx_builder = pb;
  cx->cx_builder_info = bi;

  /* XXX: Give a reasonable upper bound on the stack size */
  cx->cx_stack_size = (input_length + 1) * num_productions;
  cx->cx_stack = xmalloc(sizeof(symbol_t) * cx->cx_stack_size);

  return cx;
}/*}}}*/
void peg_delete_context(peg_context_t *cx)/*{{{*/
{
  if(cx->cx_num_alternatives) free(*cx->cx_alternatives);
  free(*cx->cx_results);
  free(cx->cx_alternatives);
  free(cx->cx_results);
  free(cx->cx_stack);
  free(cx);
}/*}}}*/
