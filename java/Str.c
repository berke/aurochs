#include <stdio.h>

#include <jni.h>

#include "Str.h"


JNIEXPORT void JNICALL Java_Str_fuzzy(JNIEnv* env, jobject obj, jobject js)
{
	char* str;

	str = (*env)->GetDirectBufferAddress(env, js);

	printf("In C #1 : %s\n", str);
	printf("In C #2 : %d\n", (*env)->GetDirectBufferCapacity(env, js));

	for (size_t i = 0; i < 10; ++i)
		printf("str[%d] = %c (%d)\n", i, str[i], str[i]);

	str[1] = 'X';

	printf("\n");
}
