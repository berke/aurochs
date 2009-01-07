/* stack.h
 *
 */

#ifndef STACK_H
#define STACK_H

#include <base_types.h>
#include <alloc.h>

#define STACK_DEFAULT_CHUNK_SIZE 900

#define STACK_ALIGN (sizeof(uint64_t))

typedef struct stack_chunk {
  struct stack_chunk *sc_next;  /* Next chunk */
  size_t sc_size;               /* Size of this chunk */
  /* Used as pointer for end of structure */
  u8 sc_data[1];
} aurochs_stack_chunk_t;

typedef struct {
  alloc_t *s_chunk_alloc; /* Where to allocate chunks FOR this stack */
  aurochs_stack_chunk_t *s_head;
  aurochs_stack_chunk_t *s_tail;
  off_t s_index;
  alloc_t s_alloc; /* Allocator for allocating IN this stack */
} aurochs_stack_t;

EXPORT aurochs_stack_t *stack_create(alloc_t *);
EXPORT void stack_dispose(aurochs_stack_t *);
EXPORT void *stack_alloc(aurochs_stack_t *, size_t);

#endif
