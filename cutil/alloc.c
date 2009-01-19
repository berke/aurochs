/* alloc.c
 *
 */

#include <stdlib.h>
#include <alloc.h>

void *alloc_malloc(alloc_t *a, size_t size)/*{{{*/
{
  return a->a_malloc(a->a_info, size);
}/*}}}*/
void alloc_free(alloc_t *a, void *ptr)/*{{{*/
{
  a->a_free(a->a_info, ptr);
}/*}}}*/
static void *do_malloc(void *info, size_t bytes) {/*{{{*/
  void *result;
  result = malloc(bytes);
  if(!result) abort();
  return result;
}/*}}}*/
static void do_free(void *info, void *ptr) {/*{{{*/
  if(ptr) free(ptr);
}/*}}}*/
alloc_t alloc_stdlib = {/*{{{*/
  0,
  do_malloc,
  do_free
};/*}}}*/
