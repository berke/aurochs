/* peg_lib.h
 *
 */

#ifndef PEG_LIB_H
#define PEG_LIB_H

#include <peg.h>
#include <cnog.h>

peg_context_t *peg_create_context(nog_program_t *pg, peg_builder_t *pb, info bi, letter_t *input, int input_length);
void peg_delete_context(peg_context_t *cx);
void peg_dump_context(FILE *f, peg_context_t *cx);
int peg_error_position(peg_context_t *cx);

#endif
