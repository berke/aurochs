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

#if 0
#define DEBUGF(x, y...) printf("DEBUG " __FILE__ ":" x "\n", ##y);
#else
#define DEBUGF(x,...)
#endif

#include <alloc.h>

typedef value tree;
typedef value attribute;
typedef struct {
  value cons_value;
} construction_t;
typedef alloc_t *info;
typedef construction_t *construction;

#define BUILDER_TYPES_DEFINED 1

#include <aurochs.h>

typedef struct {
  nog_program_t *p_nog;
  staloc_t *p_staloc;
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
  staloc_dispose(program_val(pgv).p_staloc);
}

static value program_alloc(nog_program_t *pg, staloc_t *s)
{
  value v = alloc_custom(&program_ops, sizeof(program_t), pg->np_count, 1000000);
  program_val(v).p_nog = pg;
  program_val(v).p_staloc = s;
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
  staloc_t *s;
  nog_program_t *pg;
  const char *error;

  binary = (uint8_t *) String_val(binaryv);
  length = string_length(binaryv);
  error = 0;

  if(pack_init_from_string(&pk, binary, length)) {
    s = staloc_create(&alloc_stdlib);
    if(s) {
      pg = nog_unpack_program(&s->s_alloc, &pk);
      if(pg) {
        programv = program_alloc(pg, s);
        CAMLreturn(programv);
      } else error = "Can't initialize program";
      staloc_dispose(s);
    } else error = "Can't initialize staloc";
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
  AUROCHS_ATTRIBUTE_VALUE_FIELD,
  AUROCHS_ATTRIBUTE_NAME_FIELD,
  AUROCHS_ATTRIBUTE_COUNT
};

enum {
  AUROCHS_VALUE_CONSTANT_STRING_FIELD,
  AUROCHS_VALUE_CONSTANT_COUNT
};

enum {
  AUROCHS_VALUE_SUBSTRING_START_FIELD,
  AUROCHS_VALUE_SUBSTRING_END_FIELD,
  AUROCHS_VALUE_SUBSTRING_COUNT
};

#define AUROCHS_VALUE_SUBSTRING_TAG 0
#define AUROCHS_VALUE_CONSTANT_TAG 1

static value create_token(int begin, int end)
{
  CAMLparam0();
  CAMLlocal1(treev);

  treev = caml_alloc(AUROCHS_P_TOKEN_COUNT, AUROCHS_P_TOKEN_TAG);
  Store_field(treev, AUROCHS_P_NODE_START_FIELD, Val_int(begin));
  Store_field(treev, AUROCHS_P_NODE_END_FIELD, Val_int(end));

  CAMLreturn(treev);
}

static value create_string_attribute(int id, unsigned char *u, int length)
{
  CAMLparam0();
  CAMLlocal3(stringv, valuev, attrv);

  stringv = caml_alloc_string(length);
  memcpy(String_val(stringv), u, length);

  valuev = caml_alloc(AUROCHS_VALUE_CONSTANT_COUNT, AUROCHS_VALUE_CONSTANT_TAG);
  Store_field(valuev, AUROCHS_VALUE_CONSTANT_STRING_FIELD, stringv);

  attrv = caml_alloc(AUROCHS_ATTRIBUTE_COUNT, AUROCHS_ATTRIBUTE_TAG);
  Store_field(attrv, AUROCHS_ATTRIBUTE_VALUE_FIELD, valuev);
  Store_field(attrv, AUROCHS_ATTRIBUTE_NAME_FIELD, Val_int(id));

  CAMLreturn(attrv);
}

static value create_substring_attribute(int id, int begin, int end)
{
  CAMLparam0();
  CAMLlocal2(valuev, attrv);

  valuev = caml_alloc(AUROCHS_VALUE_SUBSTRING_COUNT, AUROCHS_VALUE_SUBSTRING_TAG);
  Store_field(valuev, AUROCHS_VALUE_SUBSTRING_START_FIELD, Val_int(begin));
  Store_field(valuev, AUROCHS_VALUE_SUBSTRING_END_FIELD, Val_int(end));

  attrv = caml_alloc(AUROCHS_ATTRIBUTE_COUNT, AUROCHS_ATTRIBUTE_TAG);
  Store_field(attrv, AUROCHS_ATTRIBUTE_VALUE_FIELD, valuev);
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

static construction start_construction(info in, int id, unsigned char *name, int begin)
{
  construction_t *cons;

  cons = alloc_malloc(in, sizeof(construction_t));
  DEBUGF("Start construction %p", cons);
  if(!cons) caml_failwith("Cannot start construction");
  cons->cons_value = Val_int(0);
  caml_register_global_root(&cons->cons_value);
  cons->cons_value = create_node(id, begin);
  return cons;
}

static tree finish_construction(info in, construction cons, int end)
{
  CAMLparam0();
  CAMLlocal1(nodev);

  DEBUGF("Finish construction %p", cons);
  nodev = cons->cons_value;
  Store_field(nodev, AUROCHS_P_NODE_END_FIELD, Val_int(end));
  Store_field(nodev, AUROCHS_P_NODE_CHILD_FIELD, list_rev_append(Field(nodev, AUROCHS_P_NODE_CHILD_FIELD), Val_int(0)));
  Store_field(nodev, AUROCHS_P_NODE_ATTRS_FIELD, list_rev_append(Field(nodev, AUROCHS_P_NODE_ATTRS_FIELD), Val_int(0)));
  caml_remove_global_root(&cons->cons_value);
  alloc_free(in, cons);

  CAMLreturn(nodev);
}

static bool add_children(info a, construction c, tree tr2)
{
  CAMLparam1(tr2);
  CAMLlocal1(consv);

  consv = c->cons_value;
  Store_field(consv, AUROCHS_P_NODE_CHILD_FIELD, cons(tr2, Field(c->cons_value, AUROCHS_P_NODE_CHILD_FIELD)));

  CAMLreturnT(bool, true);
}

static bool add_token(info a, construction c, int t_begin, int t_end)
{
  CAMLparam0();
  CAMLlocal2(tokv, childv);

  tokv = create_token(t_begin, t_end);
  childv = cons(tokv, Field(c->cons_value, AUROCHS_P_NODE_CHILD_FIELD));
  Store_field(c->cons_value, AUROCHS_P_NODE_CHILD_FIELD, childv);

  CAMLreturnT(bool, true);
}

static bool add_constant_attribute(info a, construction c, int id, unsigned char *name, unsigned char *u, int length)
{
  CAMLparam0();
  CAMLlocal2(attrv, attrsv);

  attrv = create_string_attribute(id, u, length);
  attrsv = cons(attrv, Field(c->cons_value, AUROCHS_P_NODE_ATTRS_FIELD));
  Store_field(c->cons_value, AUROCHS_P_NODE_ATTRS_FIELD, attrsv);

  CAMLreturnT(bool, true);
}

static bool add_attribute(info a, construction c, int id, unsigned char *name, int v_begin, int v_end)
{
  CAMLparam0();
  CAMLlocal2(attrv, attrsv);

  attrv = create_substring_attribute(id, v_begin, v_end);
  attrsv = cons(attrv, Field(c->cons_value, AUROCHS_P_NODE_ATTRS_FIELD));
  Store_field(c->cons_value, AUROCHS_P_NODE_ATTRS_FIELD, attrsv);

  CAMLreturnT(bool, true);
}

#define ROOT_NODE_ID 0
#define ROOT_NODE_NAME "Foo"

static value some(value x)
{
  CAMLparam1(x);
  CAMLlocal1(somev);

  somev = caml_alloc(1, 0);
  Store_field(somev, 0, x);

  CAMLreturn(somev);
}


#define none (Val_int(0))

value caml_aurochs_get_production_count(value programv)
{
  CAMLparam1(programv);
  nog_program_t *pg;

  pg = program_val(programv).p_nog;
  CAMLreturn(Val_int(pg->np_num_productions));
}

value caml_aurochs_get_choice_count(value programv)
{
  CAMLparam1(programv);
  nog_program_t *pg;

  pg = program_val(programv).p_nog;
  CAMLreturn(Val_int(pg->np_num_choices));
}

value caml_aurochs_get_constructor_count(value programv)
{
  CAMLparam1(programv);
  nog_program_t *pg;

  pg = program_val(programv).p_nog;
  CAMLreturn(Val_int(pg->np_num_constructors));
}

value caml_aurochs_get_constructor_name(value programv, value iv)
{
  CAMLparam2(programv, iv);
  nog_program_t *pg;
  int i;

  i = Int_val(iv);
  pg = program_val(programv).p_nog;
  if(i < 0 || i >= pg->np_num_constructors) caml_invalid_argument("Constructor index out of range");
  CAMLreturn(caml_copy_string((char *) pg->np_constructors[i].ns_chars));
}

value caml_aurochs_get_attribute_count(value programv)
{
  CAMLparam1(programv);
  nog_program_t *pg;

  pg = program_val(programv).p_nog;
  CAMLreturn(Val_int(pg->np_num_attributes));
}

value caml_aurochs_get_attribute_name(value programv, value iv)
{
  CAMLparam2(programv, iv);
  nog_program_t *pg;
  int i;

  i = Int_val(iv);
  pg = program_val(programv).p_nog;
  if(i < 0 || i >= pg->np_num_attributes) caml_invalid_argument("attribute index out of range");
  CAMLreturn(caml_copy_string((char *) pg->np_attributes[i].ns_chars));
}

value caml_aurochs_parse(value programv, value uv, value errorv)
{
  CAMLparam3(programv, uv, errorv);
  CAMLlocal1(treev);
  uint8_t *input;
  size_t input_length;
  nog_program_t *pg;
  peg_context_t *cx;
  staloc_t *s;
  peg_builder_t builder;
  info builder_info;

  DEBUGF("Parsing");
  pg = program_val(programv).p_nog;
  input = (uint8_t *) String_val(uv);
  input_length = string_length(uv);

  s = staloc_create(&alloc_stdlib);
  if(!s) caml_failwith("Can't allocate staloc");
  builder_info = &s->s_alloc;

  builder.pb_info = builder_info;
  builder.pb_start_construction = start_construction;
  builder.pb_add_children = add_children;
  builder.pb_add_attribute = add_attribute;
  builder.pb_add_constant_attribute = add_constant_attribute;
  builder.pb_add_token = add_token;
  builder.pb_finish_construction = finish_construction;

  cx = peg_create_context(&alloc_stdlib, pg, &builder, builder_info, input, input_length);
  DEBUGF("Created context of input length %ld", input_length);
  if(!cx) {
    staloc_dispose(s);
    caml_failwith("Can't allocate context");
  }

  if(nog_execute(cx, pg, &treev)) {
    peg_delete_context(cx);
    staloc_dispose(s);
    CAMLreturn(some(treev));
  } else {
    /* We've got a parse error, compute its position. */
    int pos;

    pos = nog_error_position(cx, pg);
    Store_field(errorv, 0, Val_int(pos));
    peg_delete_context(cx);
    staloc_dispose(s);
    CAMLreturn(none); /* None */
  }
}

