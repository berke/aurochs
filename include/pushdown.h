/* pushdown.h
 *
 */

#ifndef PUSHDOWN_H
#define PUSHDOWN_H

#include <base_types.h>
#include <alloc.h>

#define PUSHDOWN_ELEMENTS_PER_BLOCK 1000

#define STALOC_ALIGN (sizeof(u64))

typedef int pushdown_element_t;

typedef struct pushdown_block {
  struct pushdown_block *next;
  pushdown_element_t elements[PUSHDOWN_ELEMENTS_PER_BLOCK];
} pushdown_block_t;

typedef struct {
  alloc_t *alloc;
  pushdown_block_t *head;
  int index;
} pushdown_t;

EXPORT pushdown_t *pushdown_create(alloc_t *);
EXPORT void pushdown_dispose(pushdown_t *);
EXPORT void pushdown_push(pushdown_t *, pushdown_element_t);
EXPORT bool pushdown_pop(pushdown_t *, pushdown_element_t *);
EXPORT bool pushdown_is_empty(pushdown_t *);

#endif
