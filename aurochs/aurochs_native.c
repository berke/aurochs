/* aurochs_native.c */

#include <sys/mman.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <caml/mlvalues.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>

#include <base_types.h>
#include <alloc.h>
#include <stack.h>

typedef value tree;
typedef value attribute;
typedef struct {
  value cons_value;
} construction_t;
typedef alloc_t *info;
typedef construction_t *construction;

#define BUILDER_TYPES_DEFINED 1

#include <peg.h>
#include <cnog.h>
#include <peg_lib.h>

typedef struct {
  nog_program_t *p_nog;
  stack_t *p_stack;
} program_t;

#define program_val(v) (*((program_t *) Data_custom_val(v)))

static void program_finalize(value pgv);

static struct custom_operations program_ops = {
  "fr.aurochs.caml.aurochs.program",
  program_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

static void program_finalize(value pgv)
{
  stack_dispose(program_val(pgv).p_stack);
}

static value program_alloc(nog_program_t *pg, stack_t *s)
{
  value v = alloc_custom(&program_ops, sizeof(program_t), pg->np_count, 1000000);
  program_val(v).p_nog = pg;
  program_val(v).p_stack = s;
  return v;
}

static value cons(value v1, value v2)
{
  CAMLparam2(v1, v2);
  CAMLlocal1(consv);

  consv = caml_alloc(2, 0);
  Store_field(consv, 0, v1);
  Store_field(consv, 1, v2);

  CAMLreturn(consv);
}

static value list_rev_append(value l1v, value l2v)
{
  CAMLparam2(l1v, l2v);
  CAMLlocal2(headv, restv);

  if(Is_long(l1v)) {
    /* The empty list */
    CAMLreturn(l2v);
  } else {
    headv = Field(l1v, 0);
    restv = Field(l1v, 1);
    CAMLreturn(list_rev_append(restv, cons(headv, l2v)));
  }
}

value caml_aurochs_program_of_binary(value binaryv)
{
  CAMLparam1(binaryv);
  CAMLlocal1(programv);
  uint8_t *binary;
  size_t length;
  packer_t pk;
  stack_t *s;
  nog_program_t *pg;
  const char *error;

  binary = (uint8_t *) String_val(binaryv);
  length = string_length(binaryv);
  error = 0;

  if(pack_init_from_string(&pk, binary, length)) {
    s = stack_create(&alloc_stdlib);
    if(s) {
      pg = cnog_unpack_program(&s->s_alloc, &pk);
      if(pg) {
        programv = program_alloc(pg, s);
        CAMLreturn(programv);
      } else error = "Can't initialize program";
      stack_dispose(s);
    } else error = "Can't initialize stack";
    pack_shutdown(&pk);
  } else error = "Can't initialize packer";

  caml_failwith(error);
}

#define AUROCHS_P_NODE_TAG 0
#define AUROCHS_P_TOKEN_TAG 1

enum {
  AUROCHS_P_NODE_START_FIELD,
  AUROCHS_P_NODE_END_FIELD,
  AUROCHS_P_NODE_NAME_FIELD,
  AUROCHS_P_NODE_ATTRS_FIELD,
  AUROCHS_P_NODE_CHILD_FIELD,
  AUROCHS_P_NODE_COUNT
};

enum {
  AUROCHS_P_TOKEN_START_FIELD,
  AUROCHS_P_TOKEN_END_FIELD,
  AUROCHS_P_TOKEN_COUNT
};

#define AUROCHS_ATTRIBUTE_TAG 0

enum {
  AUROCHS_ATTRIBUTE_START_FIELD,
  AUROCHS_ATTRIBUTE_END_FIELD,
  AUROCHS_ATTRIBUTE_NAME_FIELD,
  AUROCHS_ATTRIBUTE_COUNT
};

static value create_token(int begin, int end)
{
  CAMLparam0();
  CAMLlocal1(treev);

  treev = caml_alloc(AUROCHS_P_TOKEN_COUNT, AUROCHS_P_TOKEN_TAG);
  Store_field(treev, AUROCHS_P_NODE_START_FIELD, Val_int(begin));
  Store_field(treev, AUROCHS_P_NODE_END_FIELD, Val_int(end));
  CAMLreturn(treev);
}

static value create_attribute(int id, int begin, int end)
{
  CAMLparam0();
  CAMLlocal1(attrv);

  attrv = caml_alloc(AUROCHS_ATTRIBUTE_COUNT, AUROCHS_ATTRIBUTE_TAG);
  Store_field(attrv, AUROCHS_ATTRIBUTE_START_FIELD, Val_int(begin));
  Store_field(attrv, AUROCHS_ATTRIBUTE_END_FIELD, Val_int(end));
  Store_field(attrv, AUROCHS_ATTRIBUTE_NAME_FIELD, Val_int(id));
  CAMLreturn(attrv);
}

static value create_node(int id, int begin)
{
  CAMLparam0();
  CAMLlocal1(treev);

  treev = caml_alloc(AUROCHS_P_NODE_COUNT, AUROCHS_P_NODE_TAG);
  Store_field(treev, AUROCHS_P_NODE_START_FIELD, Val_int(begin));
  Store_field(treev, AUROCHS_P_NODE_END_FIELD, Val_int(0));
  Store_field(treev, AUROCHS_P_NODE_NAME_FIELD, Val_int(id));
  Store_field(treev, AUROCHS_P_NODE_ATTRS_FIELD, Val_int(0));
  Store_field(treev, AUROCHS_P_NODE_CHILD_FIELD, Val_int(0));

  CAMLreturn(treev);
}

static construction (start_construction)(info in, int id, unsigned char *name, int begin)
{
  construction_t *cons;

  cons = alloc_malloc(in, sizeof(construction_t));
  if(!cons) caml_failwith("Cannot start construction");
  caml_register_global_root(&cons->cons_value);
  cons->cons_value = create_node(id, begin);
  return cons;
}

static tree finish_construction(info in, construction cons, int end)
{
  CAMLparam0();
  CAMLlocal1(nodev);
  nodev = cons->cons_value;
  Store_field(nodev, AUROCHS_P_NODE_END_FIELD, Val_int(end));
  Store_field(nodev, AUROCHS_P_NODE_CHILD_FIELD, list_rev_append(Field(nodev, AUROCHS_P_NODE_CHILD_FIELD), Val_int(0)));
  Store_field(nodev, AUROCHS_P_NODE_ATTRS_FIELD, list_rev_append(Field(nodev, AUROCHS_P_NODE_ATTRS_FIELD), Val_int(0)));
  caml_remove_global_root(&cons->cons_value);
  CAMLreturn(nodev);
}

static bool add_children(info a, construction c, tree tr2)
{
  Store_field(c->cons_value, AUROCHS_P_NODE_CHILD_FIELD, cons(tr2, Field(c->cons_value, AUROCHS_P_NODE_CHILD_FIELD)));
  return true;
}

static bool add_token(info a, construction c, int t_begin, int t_end)
{
  Store_field(c->cons_value, AUROCHS_P_NODE_CHILD_FIELD,
    cons(
      create_token(t_begin, t_end),
      Field(c->cons_value, AUROCHS_P_NODE_CHILD_FIELD)));
  return true;
}

static bool add_attribute(info a, construction c, int id, unsigned char *name, int v_begin, int v_end)
{
  Store_field(c->cons_value, AUROCHS_P_NODE_ATTRS_FIELD,
    cons(
      create_attribute(id, v_begin, v_end),
      Field(c->cons_value, AUROCHS_P_NODE_ATTRS_FIELD)));
  return true;
}

#define ROOT_NODE_ID 0
#define ROOT_NODE_NAME "Foo"

static value some(value x)/*{{{*/
{
  CAMLparam1(x);
  CAMLlocal1(somev);

  somev = caml_alloc(1, 0);
  Store_field(somev, 0, x);

  CAMLreturn(somev);
}/*}}}*/

#define none (Val_int(0))

value caml_aurochs_get_constructor_count(value programv)/*{{{*/
{
  CAMLparam1(programv);
  nog_program_t *pg;

  pg = program_val(programv).p_nog;
  CAMLreturn(Val_int(pg->np_num_constructors));
}/*}}}*/
value caml_aurochs_get_constructor_name(value programv, value iv)/*{{{*/
{
  CAMLparam2(programv, iv);
  nog_program_t *pg;
  int i;

  i = Int_val(iv);
  pg = program_val(programv).p_nog;
  if(i < 0 || i >= pg->np_num_constructors) caml_invalid_argument("Constructor index out of range");
  CAMLreturn(caml_copy_string((char *) pg->np_constructors[i].ns_chars));
}/*}}}*/
value caml_aurochs_get_attribute_count(value programv)/*{{{*/
{
  CAMLparam1(programv);
  nog_program_t *pg;

  pg = program_val(programv).p_nog;
  CAMLreturn(Val_int(pg->np_num_attributes));
}/*}}}*/
value caml_aurochs_get_attribute_name(value programv, value iv)/*{{{*/
{
  CAMLparam2(programv, iv);
  nog_program_t *pg;
  int i;

  i = Int_val(iv);
  pg = program_val(programv).p_nog;
  if(i < 0 || i >= pg->np_num_attributes) caml_invalid_argument("attribute index out of range");
  CAMLreturn(caml_copy_string((char *) pg->np_attributes[i].ns_chars));
}/*}}}*/
value caml_aurochs_parse(value programv, value uv, value errorv)/*{{{*/
{
  CAMLparam3(programv, uv, errorv);
  CAMLlocal1(treev);
  uint8_t *input;
  size_t input_length;
  nog_program_t *pg;
  peg_context_t *cx;
  stack_t *s;
  peg_builder_t builder;
  info builder_info;

  pg = program_val(programv).p_nog;
  input = (uint8_t *) String_val(uv);
  input_length = string_length(uv);

  s = stack_create(&alloc_stdlib);
  if(!s) caml_failwith("Can't allocate stack");
  builder_info = &s->s_alloc;

  builder.pb_info = builder_info;
  builder.pb_start_construction = start_construction;
  builder.pb_add_children = add_children;
  builder.pb_add_attribute = add_attribute;
  builder.pb_add_token = add_token;
  builder.pb_finish_construction = finish_construction;

  cx = peg_create_context(pg, &builder, builder_info, input, input_length);
  if(!cx) caml_failwith("Can't allocate context");

  if(cnog_execute(cx, pg, &treev)) {
    CAMLreturn(some(treev));
  } else {
    /* We've got a parse error, compute its position. */
    int pos;

    pos = cnog_error_position(cx, pg);
    Store_field(errorv, 0, Int_val(pos));
    CAMLreturn(none); /* None */
  }
}/*}}}*/
