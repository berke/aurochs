/* fr_aurochs_Parser */

#include <stdio.h>
#include <jni.h>

#include <base_types.h>
#include <stack.h>
#include <alloc.h>

typedef jobject tree;
typedef jobject construction;

typedef struct {
  alloc_t *i_alloc;
  JNIEnv *i_jnienv;

  jclass i_node_class;

  jmethodID i_constructor_mid;
  jmethodID i_add_token_mid;
  jmethodID i_add_attribute_mid;
  jmethodID i_add_children_mid;

  jobject exc;

  jbyteArray exp;
} info_t;

typedef info_t *info;

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
  find_and_fail(env, msg, "java/lang/Exception");
}

static construction start_construction(info in, int id, unsigned char *name, int begin)
{
  jobject node;
  jstring jname;
  JNIEnv *env;

  env = in->i_jnienv;

  jname = (*env)->NewStringUTF(env, (char *) name);
  if(!jname) return 0;

  node = (*env)->NewObjectA(env, in->i_node_class, in->i_constructor_mid,
                            (jvalue *) &jname);
  return node;
}
static tree finish_construction(info in, construction cons, int end)
{
  return cons;
}
static bool add_children(info in, construction c, tree tr2)
{
  JNIEnv *env;

  env = in->i_jnienv;
  (*env)->CallVoidMethod(env, c, in->i_add_children_mid, tr2, in->exp);
  return true;
}
static bool add_token(info in, construction c, int t_begin, int t_end)
{
  JNIEnv *env;

  env = in->i_jnienv;
  (*env)->CallVoidMethod(env, c, in->i_add_token_mid, t_begin, t_end, in->exp);
  return true;
}
static bool add_attribute(info in, construction c, int id, unsigned char *name, int v_begin, int v_end)
{
  JNIEnv *env;
  jstring jname;

  env = in->i_jnienv;
  jname = (*env)->NewStringUTF(env, (char *) name);
  if(!jname) return 0;
  (*env)->CallVoidMethod(env, c, in->i_add_attribute_mid, jname, v_begin, v_end, in->exp);
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
  if(pack_init_from_string(&pk, binary, length)) {
    s = stack_create(&alloc_stdlib);
    if(s) {
      pg = cnog_unpack_program(&s->s_alloc, &pk);
      (*env)->ReleaseByteArrayElements(env, nog, (jbyte *) binary, JNI_ABORT);
      return (jlong) pg;
    }
    stack_dispose(s);
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
  info_t builder_info;
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

  /* Get byte pointer to the input */
  input = (uint8_t *) (*env)->GetByteArrayElements(env, ub, 0);
  input_length = (*env)->GetArrayLength(env, ub);

  /* Do stuff */
  s = stack_create(&alloc_stdlib);
  if(!s) {
    fail(env, "Can't allocate stack");
    goto bye;
  }

  builder_info.i_alloc = &s->s_alloc;
  builder_info.i_jnienv = env;
  builder_info.exp = ub;

  builder_info.i_node_class = (*env)->FindClass(env, "fr/aurochs/Node");
  if(!builder_info.i_node_class) {
    fail(env, "Can't find Node class");
    goto bye;
  }

#define F(x) x
#define JFUN(x,y) "(" F(x) ")" F(y)
#define JCLS(x) "L" F(x) ";"
#define JVOID "V"
#define JINT "I"
#define JSTR JCLS("java/lang/String")
#define JNODE JCLS("fr/aurochs/Node")
#define JTREE JCLS("fr/aurochs/Tree")
#define JBARR "[B"

  builder_info.i_constructor_mid   = (*env)->GetMethodID(env, builder_info.i_node_class, "<init>",       JFUN(JSTR, JVOID));
  if(!builder_info.i_constructor_mid) { fail(env, "Can't find constructor"); goto bye; }

  builder_info.i_add_token_mid     = (*env)->GetMethodID(env, builder_info.i_node_class, "addToken",     JFUN(JINT JINT JBARR, JVOID));
  if(!builder_info.i_add_token_mid) { fail(env, "Can't find addToken method"); goto bye; }

  builder_info.i_add_attribute_mid = (*env)->GetMethodID(env, builder_info.i_node_class, "addAttribute", JFUN(JSTR JINT JINT JBARR, JVOID));
  if(!builder_info.i_add_attribute_mid) { fail(env, "Can't find addAttribute method"); goto bye; }

  builder_info.i_add_children_mid  = (*env)->GetMethodID(env, builder_info.i_node_class, "addChildren",  JFUN(JTREE, JVOID));
  if(!builder_info.i_add_children_mid) { fail(env, "Can't find addChildren method"); goto bye; }

  builder.pb_info = &builder_info;
  builder.pb_start_construction = start_construction;
  builder.pb_add_children = add_children;
  builder.pb_add_attribute = add_attribute;
  builder.pb_add_token = add_token;
  builder.pb_finish_construction = finish_construction;

  cx = peg_create_context(&alloc_stdlib, pg, &builder, &builder_info, input, input_length);
  if(!cx) {
    stack_dispose(s);
    fail(env, "Can't allocate context");
    goto bye;
  }

  if(cnog_execute(cx, pg, &tree)) {
    peg_delete_context(cx);
    stack_dispose(s);
    return tree;
  } else {
    jclass cls;
    jint pos;
    jmethodID mid;
    jobject exc;

    pos = cnog_error_position(cx, pg);
    peg_delete_context(cx);
    stack_dispose(s);

    cls = (*env)->FindClass(env, "fr/aurochs/ParseError");
    if(!cls) goto bye;
    mid = (*env)->GetMethodID(env, cls, "<init>", "(I)V");
    if(!mid) goto bye;
    exc = (*env)->NewObjectA(env, cls, mid, (jvalue *) &pos);
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
