/* parse_tree.c
 *
 */

#include <stdlib.h>
#include <peg.h>

peg_builter_t parse_tree_builder = {
  .pb_info = 0;
  .pb_create_token = ptree_create_token,
  .pb_create_node = ptree_create_node,
  .pb_delete_attribute = ptree_delete_attribute,
  .pb_delete_tree = ptree_delete_tree,
  .pb_attach_attribute = ptree_attach_attribute,
  .pb_add_children = ptree_add_children,
  .pb_reverse_sibling = ptree_reverse_sibling,
  .pb_reverse_tree = ptree_reverse_tree,
  .pb_dump_tree = ptree_dump_tree,
};

void *xmalloc(size_t p)/*{{{*/
{
  void *r;
  r = malloc(p);
  if(!r) abort();
  return r;
}/*}}}*/
tree *ptree_create_token(int t_begin, int t_end)/*{{{*/
{
  tree *tr;
  tr = xmalloc(sizeof(tree));
  tr->t_kind = TREE_TOKEN;
  tr->t_sibling = 0;
  tr->t_element.t_token.s_begin = t_begin;
  tr->t_element.t_token.s_end = t_end;

  return tr;
}/*}}}*/
tree *ptree_create_node(char *name)/*{{{*/
{
  tree *tr;
  tr = xmalloc(sizeof(tree));
  tr->t_kind = TREE_NODE;
  tr->t_sibling = 0;
  tr->t_element.t_node.n_name = name;
  tr->t_element.t_node.n_attributes = 0;
  tr->t_element.t_node.n_children = 0;

  return tr;
}/*}}}*/
void ptree_attach_attribute(node *nd, char *name, int v_begin, int v_end)/*{{{*/
{
  attribute *at;

  at = xmalloc(sizeof(attribute));
  at->a_name = name;
  at->a_value.s_begin = v_begin;
  at->a_value.s_end = v_end;
  at->a_sibling = nd->n_attributes;
  nd->n_attributes = at;
}/*}}}*/
void ptree_attach_position_attribute(node *nd, char *name, int v_begin, int v_end)/*{{{*/
{
  ptree_attach_attribute(nd, name, v_begin, v_begin);
}/*}}}*/
void ptree_add_children(node *nd, tree *tr)/*{{{*/
{
  tr->t_sibling = nd->n_children;
  nd->n_children = tr;
}/*}}}*/
void ptree_reverse_sibling(node *nd) {/*{{{*/
  tree *loop(tree *tr_accu, tree *tr) {/*{{{*/
    if(tr) {
      tree *tr_rest;
      tr_rest = tr->t_sibling;
      tr->t_sibling = tr_accu;
      return loop(tr, tr_rest);
    } else {
      return tr_accu;
    }
  }/*}}}*/
  nd->n_children = loop(0, nd->n_children);
}/*}}}*/
void ptree_reverse_tree(tree *tr) {/*{{{*/
  switch(tr->t_kind) {
    case TREE_TOKEN:
      break;
    case TREE_NODE:
      ptree_reverse_sibling(&tr->t_element.t_node);
      {
        tree *tr2;

        tr2 = tr->t_element.t_node.n_children;
        while(tr2) {
          ptree_reverse_tree(tr2);
          tr2 = tr2->t_sibling;
        }
      }
      break;
  }
}/*}}}*/
void ptree_put_indent(FILE *f, int indent)/*{{{*/
{
  while(indent --) {
    fprintf(f, "  ");
  }
}/*}}}*/
void ptree_put_substring(FILE *f, char *input, substring *s)/*{{{*/
{
  if(s->s_begin > s->s_end) {
    fprintf(f,"***ILLEGAL(%d,%d)***", s->s_begin, s->s_end);
  } else {
    fwrite(input + s->s_begin, s->s_end - s->s_begin, 1, f);
  }
}/*}}}*/
void ptree_dump_tree(FILE *f, char *input, tree *tr, int indent)/*{{{*/
{
  node *nd;
  token *tk;

  switch(tr->t_kind) {
    case TREE_NODE:
      nd = &tr->t_element.t_node;
      put_indent(f, indent);
      fprintf(f, "<%s", nd->n_name);
      {
        attribute *at;

        at = nd->n_attributes;
        while(at) {
          fprintf(f, " %s=\"", at->a_name);
          put_substring(f, input, &at->a_value);
          fprintf(f, "\"");
          at = at->a_sibling;
        }
      }
      if(nd->n_children) {
        fprintf(f, ">\n");
        {
          tree *tr;
          tr = nd->n_children;
          while(tr) {
            dump_tree(f, input, tr, indent + 1);
            tr = tr->t_sibling;
          }
        }
        put_indent(f, indent);
        fprintf(f, "</%s>\n", nd->n_name);
      } else {
        fprintf(f, "/>\n");
      }
      break;
    case TREE_TOKEN:
      tk = &tr->t_element.t_token;
      put_indent(f, indent);
      put_substring(f, input, tk);
      fprintf(f, "\n");
  }
}/*}}}*/
void ptree_delete_attribute(attribute *at)/*{{{*/
{
  if(!at) return;
  ptree_delete_attribute(at->a_sibling);
  free(at);
}/*}}}*/
void ptree_delete_tree(tree *tr)/*{{{*/
{
  node *nd;

  if(!tr) return;
  ptree_delete_tree(tr->t_sibling);

  switch(tr->t_kind) {
    case TREE_NODE:
      nd = &tr->t_element.t_node;
      ptree_delete_tree(nd->n_children);
      ptree_delete_attribute(nd->n_attributes);
      break;
    case TREE_TOKEN:
      free(tr); /* XXX */
      break;
  }
}/*}}}*/
