typedef enum {
  TREE_NODE,
  TREE_TOKEN,
} tree_kind;

typedef char *string;

struct _tree;

typedef struct {
  int s_begin;
  int s_end;
} substring;

struct _attribute;

typedef struct _attribute {
  struct _attribute *a_sibling;
  string a_name;
  substring a_value;
} attribute;

typedef struct {
  string n_name;
  attribute *n_attributes;
  struct _tree *n_children;
} node;

typedef substring token;

typedef struct _tree {
  tree_kind t_kind;
  struct _tree *t_sibling;
  union {
    node t_node;
    token t_token;
  } t_element;
} tree;

tree *create_token(int t_begin, int t_end);
tree *create_node(char *name);
void delete_attribute(attribute *at);
void delete_tree(tree *tr);
void attach_attribute(node *nd, char *name, int v_begin, int v_end);
void add_children(node *nd, tree *tr);
void reverse_sibling(node *nd);
void reverse_tree(tree *tr);
void dump_tree(FILE *f, char *input, tree *tr, int indent);
