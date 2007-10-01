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

typedef struct {
  
  
} builder_t;

#define BUILDER_TYPES_DEFINED 1

typedef struct { }* info;
typedef struct { }* tree;
typedef struct { }* construction;
typedef struct { }* attribute;

#include <peg.h>
#include <cnog.h>

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

value caml_aurochs_parse(value programv, value uv)
{
  CAMLparam2(programv, uv);

  caml_failwith("Parse not implemented");

  CAMLreturn(Val_unit);
}

#if 0
static construction *(start_construction)(info *a, int id, unsigned char *name)
{

}

static bool add_children(info *a, construction *c, tree *tr2)
{

}

static bool add_token(info *a, construction *c, int t_begin, int t_end)
{

}

static bool add_attribute(info *a, construction *c, int id, unsigned char *name, int v_begin, int v_end)
{

}

static tree *finish_construction(info *a, construction *t)
{

}
#endif

#define AUROCHS_P_NODE_TAG 0
#define AUROCHS_P_NODE_NAME_FIELD 2

#define AUROCHS_NODE_TAG 0
#define AUROCHS_NODE_NAME_FIELD 0
#define AUROCHS_NODE_ATTRS_FIELD 1
#define AUROCHS_NODE_CHILD_FIELD 2

static value create_node(char *name)
{
  CAMLparam0();
  CAMLlocal1(treev);

  treev = caml_alloc(3, AUROCHS_NODE_TAG);
  Store_field(treev, AUROCHS_NODE_NAME_FIELD, caml_copy_string(name));
  Store_field(treev, AUROCHS_NODE_ATTRS_FIELD, Val_int(0));
  Store_field(treev, AUROCHS_NODE_CHILD_FIELD, Val_int(0));

  CAMLreturn(treev);
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

value caml_aurochs_parse_generic(value programv, value uv)
{
  CAMLparam2(programv, uv);
  CAMLlocal4(tree1v, tree2v, tree3v, tree4v);

#if 0
  uint8_t *input;
  size_t input_length;
  nog_program_t *pg;
  peg_context_t *cx;
  stack_t *s;

  pg = program_val(v).p_nog;
  input = (uint8_t)* String_val(uv);
  input_length = (uint8_t)* string_length(uv);

  cx = peg_create_context(pg, builder, builder_info, input, input_length);
  if(!cx) caml_failwith("Can't allocate context");
#endif

  tree1v = create_node("Foo");
  tree2v = create_node("Bar");
  tree3v = create_node("Baz");
  tree4v = create_node("Gogol");
  Store_field(tree1v, AUROCHS_NODE_CHILD_FIELD,
    cons(tree4v, cons(tree3v, cons(tree2v, Val_int(0)))));

  CAMLreturn(tree1v);
}
