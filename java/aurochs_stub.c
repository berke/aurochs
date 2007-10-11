/* fr_aurochs_Parser */

#include <stdio.h>
#include <jni.h>

#include <base_types.h>
#include <stack.h>
#include <alloc.h>

typedef jint *tree;
typedef jint *attribute;
typedef jint *construction;
typedef alloc_t *info;

#define BUILDER_TYPES_DEFINED 1

#include <peg.h>
#include <cnog.h>
#include <peg_lib.h>

#include "fr_aurochs_Parser.h"

static void find_and_fail(JNIEnv *env, const char *msg, const char *name)
{
  jclass cls;

  cls = (*env)->FindClass(env, name);
  if(cls) (*env)->ThrowNew(env, cls, msg);
  (*env)->DeleteLocalRef(env, cls);
}


static void fail(JNIEnv *env, const char *msg)
{
  find_and_fail(env, msg, "Exception");
}

static construction start_construction(info in, int id, unsigned char *name, int begin)
{
  printf("Start construction %s @ %d\n", name, begin);
  return 0;
}
static tree finish_construction(info in, construction cons, int end)
{
  printf("Finish construction %d, info = %p\n", end, in);
  return 0;
}
static bool add_children(info a, construction c, tree tr2)
{
  printf("Add children\n");
  return true;
}
static bool add_token(info a, construction c, int t_begin, int t_end)
{
  printf("Add token %d-%d\n", t_begin, t_end);
  return true;
}
static bool add_attribute(info a, construction c, int id, unsigned char *name, int v_begin, int v_end)
{
  printf("Add attribute %s %d-%d\n", name, v_begin, v_end);
  return true;
}

JNIEXPORT jlong JNICALL Java_fr_aurochs_Parser_unpack (JNIEnv *env, jobject obj, jbyteArray nog)
{
  uint8_t *binary;
  size_t length;
  packer_t pk;
  stack_t *s;
  nog_program_t *pg;

  binary = (uint8_t *) (*env)->GetByteArrayElements(env, nog, 0);
  length = (*env)->GetArrayLength(env, nog);
  printf("Length is %ld\n", length);
  if(pack_init_from_string(&pk, binary, length)) {
    s = stack_create(&alloc_stdlib);
    if(s) {
      pg = cnog_unpack_program(&s->s_alloc, &pk);
      (*env)->ReleaseByteArrayElements(env, nog, (jbyte *) binary, JNI_ABORT);
      printf("pg = %p\n", pg);
      return (jlong) pg;
    }
    //stack_dispose(s);
  }
  pack_shutdown(&pk);

  (*env)->ReleaseByteArrayElements(env, nog, (jbyte *) binary, JNI_ABORT);
  return (jlong) 0;
}

JNIEXPORT jobject JNICALL Java_fr_aurochs_Parser_parse (JNIEnv *env, jobject obj, jbyteArray ub)
{
  jclass cl;
  uint8_t *input;
  size_t input_length;
  nog_program_t *pg;
  peg_context_t *cx;
  stack_t *s;
  peg_builder_t builder;
  info builder_info;
  tree tree;
  jobject result;

  result = 0;

  /* Extract program */
  cl = (*env)->FindClass(env, "fr/aurochs/Parser");
  if(!cl) {
    fail(env, "Can't find class");
    goto bye;
  }

  jfieldID fid = (*env)->GetFieldID(env, cl, "program", "J");
  if(!fid) {
    fail(env, "Can't get field ID");
    goto bye;
  }

  pg = (nog_program_t *) (*env)->GetLongField(env, obj, fid);
  if(!pg) {
    fail(env, "Program is null");
    goto bye;
  }
  printf("Got pg at %p\n", pg);

  /* Get byte pointer to the input */
  input = (uint8_t *) (*env)->GetByteArrayElements(env, ub, 0);
  input_length = (*env)->GetArrayLength(env, ub);
  printf("Length is %ld\n", input_length);

  /* Do stuff */
  s = stack_create(&alloc_stdlib);
  if(!s) {
    fail(env, "Can't allocate stack");
    goto bye;
  }

  builder_info = &s->s_alloc;
  printf("Info = %p\n", builder_info);

  builder.pb_info = builder_info;
  builder.pb_start_construction = start_construction;
  builder.pb_add_children = add_children;
  builder.pb_add_attribute = add_attribute;
  builder.pb_add_token = add_token;
  builder.pb_finish_construction = finish_construction;

  cx = peg_create_context(&alloc_stdlib, pg, &builder, builder_info, input, input_length);
  if(!cx) {
    //stack_dispose(s);
    fail(env, "Can't allocate context");
    goto bye;
  }

  {
    int i;
    for(i = 0; i < input_length; i ++) {
      printf("  input[%d] = %c\n", i, input[i]);
    }
  }
  printf("Execute\n");
  if(cnog_execute(cx, pg, &tree)) {

  } else {
    jclass cls;
    jint pos;
    jmethodID mid;
    jobject exc;

    printf("Failed!\n");
    pos = cnog_error_position(cx, pg);
    printf("Position =%d\n", pos);
    peg_delete_context(cx);
    //stack_dispose(s);

    printf("Disposed\n");
    cls = (*env)->FindClass(env, "fr/aurochs/ParseError");
    printf("Class %p\n", cls);
    if(!cls) goto bye;
    mid = (*env)->GetMethodID(env, cls, "<init>", "(I)V");
    printf("Method %p\n", mid);
    if(!mid) goto bye;
    exc = (*env)->NewObjectA(env, cls, mid, (jvalue *) &pos);
    printf("Exception %p\n", exc);
    if(!exc) goto bye;
    if(!exc){
      goto bye;
    }
    (*env)->Throw(env, exc);
  }

bye:
  (*env)->ReleaseByteArrayElements(env, ub, (jbyte *) input, JNI_ABORT);
  return result;
}
