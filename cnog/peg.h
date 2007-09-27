/* peg.h
 *
 * Copyright (C)2007 Exalead
 */

#ifndef PEG_H
#define PEG_H

#include <stdio.h>
#include <base_types.h>

typedef enum {
  R_EOF = 0,
  R_UNKNOWN = 1,
  R_FAIL = 2,
  R_BUSY = 3
} memo_t;

#define A_UNDEFINED 0xff

typedef int result_t;
#if LONG_LETTERS
typedef unsigned int letter_t;
#else
typedef char letter_t;
#endif
typedef unsigned char choice_t;

typedef int symbol_t;

/* Builder callbacks */
#ifndef BUILDER_TYPES_DEFINED
typedef void info;
typedef void tree;
typedef void node;
typedef void attribute;
#endif

typedef struct {
  info *pb_info;
  tree *(*pb_create_token)(info *a, int t_begin, int t_end);
  tree *(*pb_create_node)(info *a, char *name);
  void (*pb_delete_attribute)(info *a, attribute *at);
  void (*pb_delete_tree)(info *a, tree *tr);
  void (*pb_attach_attribute)(info *a, node *nd, char *name, int v_begin, int v_end);
  void (*pb_add_children)(info *a, node *nd, tree *tr);
  void (*pb_reverse_sibling)(info *a, node *nd);
  void (*pb_reverse_tree)(info *a, tree *tr);
  void (*pb_dump_tree)(info *a, FILE *f, char *input, tree *tr, int indent);
} peg_builder_t;


/* Execution context */
typedef struct {
  letter_t *cx_input;        /* Change to wchar for Unicode */
  result_t **cx_results;
  choice_t **cx_alternatives;
  symbol_t *cx_stack;
  int cx_stack_size;
  int cx_input_length;
  int cx_num_productions;
  int cx_num_alternatives;
  peg_builder_t *cx_builder;
} peg_context_t;

#endif
