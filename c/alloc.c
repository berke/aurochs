/* alloc.c
 *
 */

#include <stdlib.h>
#include <alloc.h>

static size_t total = 0;

size_t alloc_stdlib_total() {
  return total;
}

void *alloc_malloc(alloc_t *a, size_t size)
{
  return a->a_malloc(a->a_info, size);
}

void alloc_free(alloc_t *a, void *ptr)
{
  a->a_free(a->a_info, ptr);
}

static void *do_malloc(void *info, size_t bytes) {
  void *result;
  result = malloc(bytes);
  if(!result) abort();
  total += bytes;
  return result;
}

static void do_free(void *info, void *ptr) {
  free(ptr);
}

alloc_t alloc_stdlib = {
  0,
  do_malloc,
  do_free
};
