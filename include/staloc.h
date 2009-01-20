/* staloc.h
 *
 */

#ifndef STALOC_H
#define STALOC_H

#include <base_types.h>
#include <alloc.h>

#define STALOC_DEFAULT_CHUNK_SIZE 900

#define STALOC_ALIGN (sizeof(u64))

typedef struct staloc_chunk {
  struct staloc_chunk *sc_next;  /* Next chunk */
  size_t sc_size;               /* Size of this chunk */
  /* Used as pointer for end of structure */
  u8 sc_data[1];
} staloc_chunk_t;

typedef struct {
  alloc_t *s_chunk_alloc; /* Where to allocate chunks FOR this stack */
  staloc_chunk_t *s_head;
  staloc_chunk_t *s_tail;
  off_t s_index;
  alloc_t s_alloc; /* Allocator for allocating IN this stack */
  size_t s_total; /* Total allocated bytes */
} staloc_t;

EXPORT staloc_t *staloc_create(alloc_t *);
EXPORT void staloc_dispose(staloc_t *);
EXPORT void *staloc_alloc(staloc_t *, size_t);
EXPORT size_t staloc_total(staloc_t *);

#endif
