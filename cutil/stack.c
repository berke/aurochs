/* stack.c
 *
 */

#include <stack.h>

static bool add_chunk(stack_t *s, size_t size)/*{{{*/
{
  stack_chunk_t *sc;

  sc = alloc_malloc(s->s_chunk_alloc, sizeof(stack_chunk_t) + size);
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
}/*}}}*/
static void dispose_chunk(stack_t *s, stack_chunk_t *sc)/*{{{*/
{
  if(sc) {
    dispose_chunk(s, sc->sc_next);
    alloc_free(s->s_chunk_alloc, sc);
  }
}/*}}}*/
static void free_ignore(void *s, void *x)/*{{{*/
{
}/*}}}*/
stack_t *stack_create(alloc_t *a)/*{{{*/
{
  stack_t *s;

  s = alloc_malloc(a, sizeof(stack_t)); if(!s) return 0;
  s->s_chunk_alloc = a;
  s->s_head = 0;
  s->s_tail = 0;
#if 0
  if(!add_chunk(s, STACK_DEFAULT_CHUNK_SIZE)) {
    alloc_free(a, s);
    return 0;
  }
#endif

  s->s_alloc.a_info = s;
  s->s_alloc.a_malloc = (void *(*)(void *, size_t)) stack_alloc;
  s->s_alloc.a_free = free_ignore;
  return s;
}/*}}}*/
void stack_dispose(stack_t *s)/*{{{*/
{
  dispose_chunk(s, s->s_head);
  alloc_free(s->s_chunk_alloc, s);
}/*}}}*/
void *stack_alloc(stack_t *s, size_t size)/*{{{*/
{
  stack_chunk_t *sc;
  void *result;

  size = (size + STACK_ALIGN - 1) & ~(STACK_ALIGN - 1);

  if(!s->s_tail || s->s_index + size > s->s_tail->sc_size) {
    add_chunk(s, size > STACK_DEFAULT_CHUNK_SIZE ? size : STACK_DEFAULT_CHUNK_SIZE);
  }

  sc = s->s_tail;
  result = sc->sc_data + s->s_index;
  s->s_index += size;
  return result;
}/*}}}*/
