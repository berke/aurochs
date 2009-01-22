/* peg_lib.h
 *
 */

#ifndef PEG_LIB_H
#define PEG_LIB_H

#include <peg.h>
#include <nog.h>

EXPORT peg_context_t *peg_create_context(alloc_t *alloc, nog_program_t *pg, peg_builder_t *pb, info bi, letter_t *input, int input_length);
EXPORT void peg_delete_context(peg_context_t *cx);
/*EXPORT void peg_dump_context(FILE *f, peg_context_t *cx);
EXPORT int peg_error_position(peg_context_t *cx);*/

#endif
