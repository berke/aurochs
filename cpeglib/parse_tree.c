/* parse_tree.c
 *
 */

#include <stdlib.h>
#include <parse_tree.h>

static tree ptree_create_token(info pti, int t_begin, int t_end)/*{{{*/
{
  tree tr;
  tr = alloc_malloc((alloc_t *) pti, sizeof(tree));
  if(tr) {
    tr->t_kind = TREE_TOKEN;
    tr->t_sibling = 0;
    tr->t_parent = 0;
    tr->t_element.t_token.s_begin = t_begin;
    tr->t_element.t_token.s_end = t_end;
  }
  return tr;
}/*}}}*/
bool ptree_add_token(info pti, construction tr1, int t_begin, int t_end)/*{{{*/
{
  tree tr2;

  tr2 = ptree_create_token(pti, t_begin, t_end);
  if(tr2) return ptree_add_children(pti, tr1, tr2);
  return false;
}/*}}}*/
construction ptree_start_construction(info pti, int id, unsigned char *name)/*{{{*/
{
  tree tr;
  tr = alloc_malloc((alloc_t *) pti, sizeof(tree));
  tr->t_kind = TREE_NODE;
  tr->t_sibling = 0;
  tr->t_parent = 0;
  tr->t_element.t_node.n_name = name;
  tr->t_element.t_node.n_attributes = 0;
  tr->t_element.t_node.n_children = 0;

  return tr;
}/*}}}*/
tree ptree_finish_construction(info pti, construction c)/*{{{*/
{
  return c;
}/*}}}*/

node ptree_get_node(tree tr)/*{{{*/
{
  if(tr->t_kind == TREE_NODE) return &tr->t_element.t_node;
  else abort();
}/*}}}*/
bool ptree_add_attribute(info pti, construction tr, int id, unsigned char *name, int v_begin, int v_end)/*{{{*/
{
  node nd;
  attribute at;

  nd = ptree_get_node(tr);
  at = alloc_malloc((alloc_t *) pti, sizeof(attribute));
  if(at) {
    at->a_name = name;
    at->a_value.s_begin = v_begin;
    at->a_value.s_end = v_end;
    at->a_sibling = nd->n_attributes;
    nd->n_attributes = at;
    return true;
  } else return false;
}/*}}}*/
bool ptree_add_position_attribute(info pti, construction tr, int id, unsigned char *name, int v_begin, int v_end)/*{{{*/
{
  return ptree_add_attribute(pti, tr, id, name, v_begin, v_begin);
}/*}}}*/
bool ptree_add_children(info pti, tree tr1, tree tr2)/*{{{*/
{
  node nd;
  
  nd = ptree_get_node(tr1);
  tr2->t_sibling = nd->n_children;
  tr2->t_parent = tr1;
  nd->n_children = tr2;

  return true;
}/*}}}*/
void ptree_reverse_sibling(info pti, tree tr) {/*{{{*/
  node nd;

  nd = ptree_get_node(tr);

  tree loop(tree tr_accu, tree tr) {/*{{{*/
    if(tr) {
      tree tr_rest;
      tr_rest = tr->t_sibling;
      tr->t_sibling = tr_accu;
      return loop(tr, tr_rest);
    } else {
      return tr_accu;
    }
  }/*}}}*/
  nd->n_children = loop(0, nd->n_children);
}/*}}}*/
void ptree_reverse_tree(info pti, tree tr) {/*{{{*/
  switch(tr->t_kind) {
    case TREE_TOKEN:
      break;
    case TREE_NODE:
      ptree_reverse_sibling(pti, tr);
      {
        tree tr2;

        tr2 = tr->t_element.t_node.n_children;
        while(tr2) {
          ptree_reverse_tree(pti, tr2);
          tr2 = tr2->t_sibling;
        }
      }
      break;
  }
}/*}}}*/
static void put_indent(info pti, FILE *f, int indent)/*{{{*/
{
  while(indent --) {
    fprintf(f, "  ");
  }
}/*}}}*/
static void put_substring(info pti, FILE *f, unsigned char *input, substring *s)/*{{{*/
{
  if(s->s_begin > s->s_end) {
    fprintf(f,"%d", s->s_begin);
  } else {
    fwrite(input + s->s_begin, s->s_end - s->s_begin, 1, f);
  }
}/*}}}*/
void ptree_dump_tree(info pti, FILE *f, unsigned char *input, tree tr, int indent)/*{{{*/
{
  node nd;
  token tk;

  switch(tr->t_kind) {
    case TREE_NODE:
      nd = &tr->t_element.t_node;
      put_indent(pti, f, indent);
      fprintf(f, "<%s", nd->n_name);
      {
        attribute at;

        at = nd->n_attributes;
        while(at) {
          fprintf(f, " %s=\"", at->a_name);
          put_substring(pti, f, input, &at->a_value);
          fprintf(f, "\"");
          at = at->a_sibling;
        }
      }
      if(nd->n_children) {
        fprintf(f, ">\n");
        {
          tree tr;
          tr = nd->n_children;
          while(tr) {
            ptree_dump_tree(pti, f, input, tr, indent + 1);
            tr = tr->t_sibling;
          }
        }
        put_indent(pti, f, indent);
        fprintf(f, "</%s>\n", nd->n_name);
      } else {
        fprintf(f, "/>\n");
      }
      break;
    case TREE_TOKEN:
      tk = &tr->t_element.t_token;
      put_indent(pti, f, indent);
      put_substring(pti, f, input, tk);
      fprintf(f, "\n");
  }
}/*}}}*/
void ptree_delete_attribute(info pti, attribute at)/*{{{*/
{
  if(!at) return;
  ptree_delete_attribute(pti, at->a_sibling);
  free(at);
}/*}}}*/
void ptree_delete_tree(info pti, tree tr)/*{{{*/
{
  node nd;

  if(!tr) return;
  ptree_delete_tree(pti, tr->t_sibling);

  switch(tr->t_kind) {
    case TREE_NODE:
      nd = &tr->t_element.t_node;
      ptree_delete_tree(pti, nd->n_children);
      ptree_delete_attribute(pti, nd->n_attributes);
      break;
    case TREE_TOKEN:
      free(tr); /* XXX */
      break;
  }
}/*}}}*/
tree ptree_get_parent(info a, tree t)/*{{{*/
{
  return t->t_parent;
}/*}}}*/
void ptree_init(peg_builder_t *pb, alloc_t *alloc)/*{{{*/
{
  pb->pb_info = alloc;
  pb->pb_add_token = ptree_add_token;
  pb->pb_finish_construction = ptree_finish_construction;
  pb->pb_start_construction = ptree_start_construction;
  pb->pb_add_attribute = ptree_add_attribute;
  pb->pb_add_children = ptree_add_children;
}/*}}}*/
