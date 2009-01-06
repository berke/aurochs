/* alloc.h
 *
 */

#ifndef ALLOC_H
#define ALLOC_H

#include <base_types.h>

/* An allocator and exception structure to be used by the various library routines.
 * This structure is an attempt to remedy the lack of exceptions and garbage collection in C.
 *
 * Rule 1) All functions allocate using the functions given in their allocation context.
 * Rule 2) The free function can be called a null pointer - it should not do anything.
 */

typedef struct {
  void *a_info;
  void *(*a_malloc)(void *info, size_t bytes);
  void (*a_free)(void *info, void *free);
} alloc_t;

void *alloc_malloc(alloc_t *, size_t);
void alloc_free(alloc_t *, void *);
extern alloc_t alloc_stdlib;

#endif
