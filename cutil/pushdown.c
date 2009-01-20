/* pushdown.c
 *
 */

#include <pushdown.h>
#include <assert.h>

static void add_block(pushdown_t *p)
{
  pushdown_block_t *pb;

  pb = alloc_malloc(p->alloc, sizeof(pushdown_block_t));
  pb->next = p->head;
  p->head = pb;
  p->index = 0;
}

static void dispose_block(pushdown_t *p, pushdown_block_t *pb)
{
  if(pb) {
    dispose_block(p, pb->next);
    alloc_free(p->alloc, pb);
  }
}

pushdown_t *pushdown_create(alloc_t *a)
{
  pushdown_t *p;

  p = alloc_malloc(a, sizeof(pushdown_t));
  p->alloc = a;
  p->head = 0;
  return p;
}

void pushdown_dispose(pushdown_t *p)
{
  dispose_block(p, p->head);
  alloc_free(p->alloc, p);
}

void pushdown_push(pushdown_t *p, pushdown_element_t e)
{
  if(p->index == PUSHDOWN_ELEMENTS_PER_BLOCK) add_block(p);
  p->head->elements[p->index ++] = e;
}

bool pushdown_is_empty(pushdown_t *p)
{
  return !p->head;
}

bool pushdown_pop(pushdown_t *p, pushdown_element_t *e)
{
  if(!p->head) return false;
  assert(p->index);
  *e = p->head->elements[-- p->index];
  if(!p->index) {
    pushdown_block_t *next;

    next = p->head->next;
    alloc_free(p->alloc, p->head);
    p->head = next;
    if(next) {
      p->index = PUSHDOWN_ELEMENTS_PER_BLOCK - 1;
    }
  }
  return true;
}
