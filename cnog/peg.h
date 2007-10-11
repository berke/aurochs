/* peg.h
 *
 * Copyright (C)2007 Exalead
 */

#ifndef PEG_H
#define PEG_H

#include <stdio.h>
#include <base_types.h>
#include <alloc.h>

typedef enum {
  R_EOF = -1,
  R_UNKNOWN = -2,
  R_FAIL = -3,
  R_BUSY = -4
} memo_t;

#define A_UNDEFINED 0xff

typedef int result_t;
#if LONG_LETTERS
typedef unsigned int letter_t;
#else
typedef unsigned char letter_t;
#endif
typedef unsigned char choice_t;

typedef int symbol_t;

/* Builder callbacks */
#if BUILDER_TYPES_DEFINED
#warning "Builder types already defined"
#else
typedef struct { }* info;
typedef struct { }* tree;
typedef struct { }* construction;
typedef struct { }* attribute;
#endif

typedef struct {
  info pb_info;
  construction (*pb_start_construction)(info a, int id, unsigned char *name, int n_begin);
  bool (*pb_add_children)(info a, construction c, tree tr2);
  bool (*pb_add_token)(info a, construction c, int t_begin, int t_end);
  bool (*pb_add_attribute)(info a, construction c, int id, unsigned char *name, int v_begin, int v_end);
  tree (*pb_finish_construction)(info a, construction t, int n_end);
} peg_builder_t;

#if 0
/* Specific */
void (*pb_delete_attribute)(info *a, attribute *at);
void (*pb_delete_tree)(info *a, tree *tr);
void (*pb_reverse_sibling)(info *a, tree *tr);
void (*pb_reverse_tree)(info *a, tree *tr);
void (*pb_dump_tree)(info *a, FILE *f, unsigned char *input, tree *tr, int indent);
#endif

/* Execution context */
typedef struct {
  alloc_t *cx_alloc;
  letter_t *cx_input;        /* Change to wchar for Unicode */
  result_t **cx_results;
  choice_t **cx_alternatives;
  symbol_t *cx_stack;
  int cx_stack_size;
  int cx_input_length;
  int cx_num_productions;
  int cx_num_alternatives;
  peg_builder_t *cx_builder;
  info cx_builder_info;
} peg_context_t;

#endif
