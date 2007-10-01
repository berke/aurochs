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

#include <cnog.h>
#include <stack.h>

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

value caml_aurochs_parse_generic(value programv, value uv)
{
  caml_failwith("Parse not generic implemented");
}

value caml_aurochs_parse(value programv, value uv)
{
  caml_failwith("Parse not implemented");
}
