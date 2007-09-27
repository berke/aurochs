/* peg.h
 *
 * Copyright (C)2007 Exalead
 */

#ifndef PEG_H
#define PEG_H

#include <stdio.h>
#include <base_types.h>

typedef int result_t;
#if LONG_LETTERS
typedef unsigned int letter_t;
#else
typedef char letter_t;
#endif
typedef unsigned char choice_t;

typedef int symbol_t;

/* Builder callbacks */
typedef void info;
typedef void tree;
typedef void node;
typedef void attribute;

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
  peg_builder_t cx_builder;
} peg_context_t;

peg_context_t *peg_create_context(letter_t *input, int input_length, int num_productions, int num_alternatives);
void peg_delete_context(peg_context_t *cx);
void peg_dump_context(FILE *f, peg_context_t *cx);
int peg_error_position(peg_context_t *cx);

#endif
