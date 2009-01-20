/* staloc.c
 *
 */

#include <staloc.h>

static bool add_chunk(staloc_t *s, size_t size)
{
  staloc_chunk_t *sc;

  sc = alloc_malloc(s->s_chunk_alloc, sizeof(staloc_chunk_t) + size);
  if(!sc) return false;
  sc->sc_size = size;

  if(s->s_tail) {
    s->s_tail->sc_next = sc;
  } else {
    s->s_head = sc;
  }
  sc->sc_next = 0;
  s->s_tail = sc;
  s->s_index = 0;

  return true;
}

static void dispose_chunk(staloc_t *s, staloc_chunk_t *sc)
{
  if(sc) {
    dispose_chunk(s, sc->sc_next);
    alloc_free(s->s_chunk_alloc, sc);
  }
}

static void free_ignore(void *s, void *x)
{
}

staloc_t *staloc_create(alloc_t *a)
{
  staloc_t *s;

  s = alloc_malloc(a, sizeof(staloc_t)); if(!s) return 0;
  s->s_chunk_alloc = a;
  s->s_head = 0;
  s->s_tail = 0;
#if 0
  if(!add_chunk(s, staloc_DEFAULT_CHUNK_SIZE)) {
    alloc_free(a, s);
    return 0;
  }
#endif

  s->s_alloc.a_info = s;
  s->s_alloc.a_malloc = (void *(*)(void *, size_t)) staloc_alloc;
  s->s_alloc.a_free = free_ignore;
  s->s_total = 0;
  return s;
}

void staloc_dispose(staloc_t *s)
{
  dispose_chunk(s, s->s_head);
  alloc_free(s->s_chunk_alloc, s);
}

size_t staloc_total(staloc_t *s)
{
  return s->s_total;
}

void *staloc_alloc(staloc_t *s, size_t size)
{
  staloc_chunk_t *sc;
  void *result;

  size = (size + STALOC_ALIGN - 1) & ~(STALOC_ALIGN - 1);

  if(!s->s_tail || s->s_index + size > s->s_tail->sc_size) {
    if(!add_chunk(s, size > STALOC_DEFAULT_CHUNK_SIZE ? size : STALOC_DEFAULT_CHUNK_SIZE)) return 0;
  }

  sc = s->s_tail;
  result = sc->sc_data + s->s_index;
  s->s_index += size;
  s->s_total += size;
  return result;
}
