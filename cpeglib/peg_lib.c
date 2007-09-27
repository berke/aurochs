/* Peg_lib */

#include <stdlib.h>
#include <stdio.h>

#include "peg_prelude.h"

void *xmalloc(size_t p)/*{{{*/
{
  void *r;
  r = malloc(p);
  if(!r) abort();
  return r;
}/*}}}*/
void peg_dump_context(FILE *f, context *cx)/*{{{*/
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
context *peg_create_context(letter *input, int input_length, int num_productions, int num_alternatives)/*{{{*/
{
  int i;
  context *cx;
  cx = xmalloc(sizeof(context));
  choice *alternatives;
  result *results;

  cx->cx_input = input + input_length;
  cx->cx_input_length = input_length;
  cx->cx_num_productions = num_productions;
  cx->cx_num_alternatives = num_alternatives;

  cx->cx_alternatives = xmalloc(sizeof(choice *) * num_alternatives);
  alternatives = xmalloc(sizeof(choice) * (input_length + 1) * num_alternatives);
  for(i = 0; i < num_alternatives * (input_length + 1); i ++) {
    alternatives[i] = A_UNDEFINED;
  }
  for(i = 0; i < num_alternatives; i ++) {
    cx->cx_alternatives[i] = alternatives + i * (input_length + 1) + input_length;
  }

  cx->cx_results = xmalloc(sizeof(result *) * num_productions);
  results = xmalloc(sizeof(result) * (input_length + 1) * num_productions);
  for(i = 0; i < num_productions * (input_length + 1); i ++) {
    results[i] = R_UNKNOWN;
  }
  for(i = 0; i < num_productions; i ++) {
    cx->cx_results[i] = results + i * (input_length + 1) + input_length;
  }

  return cx;
}/*}}}*/
void peg_delete_context(context *cx)/*{{{*/
{
  if(cx->cx_num_alternatives) free(*cx->cx_alternatives - cx->cx_input_length);
  free(*cx->cx_results - cx->cx_input_length);
  free(cx->cx_alternatives);
  free(cx->cx_results);
  free(cx);
}/*}}}*/
