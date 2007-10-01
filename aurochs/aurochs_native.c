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

#include <nog.h>
#include <stack.h>

typedef struct {
  nog_program_t *p_nog;
  stack_t *p_stack;
} program_t;

#define program_val(v) (*((progran_t *) Data_custom_val(v)))

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
  program_t *pg;

  pg = program_val(pgv);
  stack_dispose(pg->p_stack);
}

static value program_alloc(nog_program_t *pg, stack_t *s)
{
  value v = alloc_custom(&program_ops, sizeof(program_t), length, 1048576);
  program_val(v).p_nog = pg;
  program_val(v).p_stack = s;
  return v;
}

value caml_aurochs_program_of_binary(value binaryv)
{
  CAMLparam1(programv);
  CAMLlocal1(programv);

  uint8_t *binary;
  size_t length;
  void *start;
  value ecbv;
  packer_t pk;
  stack_t *st;
  nog_program_t *pg;
  const char *error;

  binary = (uint8_t *) String_val(uv);
  length = string_length(uv);
  error = 0;

  if(pack_init_from_string(&pk, binary, length)) {
    st = stack_create(&alloc_stdlib);
    if(st) {
      pg = cnog_unpack_program(&st->s_alloc, &pk);
      if(pg) {
        programv = program_alloc(pg, s);
        return programv;
      } else error = "Can't initialize program";
      stack_dispose(st);
    } else error = "Can't initialize stack");
    pack_shutdown(&pk);
  } else error = "Can't initialize packer");

  caml_failwith(error);
}
