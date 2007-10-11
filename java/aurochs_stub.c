/* fr_aurochs_Parser */

#include <stdio.h>
#include <jni.h>
#include <base_types.h>
#include <cnog.h>
#include <stack.h>
#include <alloc.h>
#include "fr_aurochs_Parser.h"

void fail(JNIEnv *env, const char *msg)
{
  jclass cls;

  cls = (*env)->FindClass(env, "Exception");
  if(cls) (*env)->ThrowNew(env, cls, msg);
  (*env)->DeleteLocalRef(env, cls);
}

JNIEXPORT jlong JNICALL Java_fr_aurochs_Parser_unpack (JNIEnv *env, jobject obj, jbyteArray nog)
{
  uint8_t *binary;
  size_t length;
  packer_t pk;
  stack_t *s;
  nog_program_t *pg;

  binary = (uint8_t *) (*env)->GetByteArrayElements(env, nog, 0);
  if(pack_init_from_string(&pk, binary, length)) {
    s = stack_create(&alloc_stdlib);
    if(s) {
      pg = cnog_unpack_program(&s->s_alloc, &pk);
      (*env)->ReleaseByteArrayElements(env, nog, (jbyte *) binary, JNI_COMMIT);
      return (jlong) pg;
    }
    stack_dispose(s);
  }
  pack_shutdown(&pk);

  (*env)->ReleaseByteArrayElements(env, nog, (jbyte *) binary, JNI_COMMIT);
  return (jlong) 0;
}

JNIEXPORT jobject JNICALL Java_fr_aurochs_Parser_parse (JNIEnv *env, jobject obj, jstring u)
{
  return 0;
}
