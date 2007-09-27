/* Check */

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include "peg_prelude.h"
#include "pegfile.h"

int main(int argc, char **argv)
{
  char *buf;
  int count;

#if 0
  { context *cx;
    cx = 0;

    printf("cx_input @ %p\n", (char *)(&cx->cx_input));
    printf("cx_input_length @ %p\n", (char *)(&cx->cx_input_length));
    printf("cx_alternatives @ %p\n", (char *)(&cx->cx_alternatives));
    printf("cx_results @ %p\n", (char *)(&cx->cx_results));
  }
#endif

#if 1
  {
    context *cx;
    int m;
    int i;
    int error_pos;
    char *fn;
    struct stat st;
    FILE *f;
    int rc;

    rc = 0;

    for(count = 1; count < argc; count ++) {
      fn = argv[count];

      if(stat(fn, &st) < 0) {
        perror("stat");
        exit(EXIT_FAILURE);
      }
      if(!S_ISREG(st.st_mode)) {
        fprintf(stderr, "%s is not a file\n", fn);
        exit(EXIT_FAILURE);
      }

      m = st.st_size;
      buf = malloc(m);
      if(!buf) abort();

      f = fopen(fn, "rb");
      if(!f) {
        perror("fopen");
        exit(EXIT_FAILURE);
      }
      if(1 != fread(buf, m, 1, f)) {
        perror("fread");
        exit(EXIT_FAILURE);
      }
      cx = create_context(buf, m, NUM_PRODUCTIONS, NUM_ALTERNATIVES);
      i = foobar_parse_start(cx, - m);
      if(getenv("DUMP_CONTEXT")) dump_context(stdout, cx);

      if(!i) {
        printf("%05d RESULT OK\n", count);
        tree *tr0;

        if(getenv("DUMP_TREE"))
        {
          tr0 = create_node("Root");
          (void) foobar_build_start(cx, &tr0->t_element.t_node, -m);
          reverse_tree(tr0);
          dump_tree(stdout, cx->cx_input, tr0, 0);
        }
      } else {
        error_pos = error_position(cx);
        if(i > 0) {
          printf("%05d RESULT NOPREFIX; ERROR AT %d\n", count, error_pos);
          rc = 1;
        } else {
          printf("%05d RESULT PREFIX %d; ERROR AT %d\n", count, m + i, error_pos);
        }
      }
      fflush(stdout);
      delete_context(cx);
      fclose(f);
    }
    return rc;
  }
#else
  for(count = 1;; count ++) {
    context *cx;
    int m;
    int i;
    int error_pos;

    if(!fgets(buf, sizeof(buf), stdin)) break;
    m = strlen(buf) - 1;
    buf[m]=0;
    cx = create_context(buf, m, NUM_PRODUCTIONS, NUM_ALTERNATIVES);
    i = foobar_parse_start(cx, - m);
    if(getenv("DUMP_CONTEXT")) dump_context(stdout, cx);

    if(!i) {
      printf("%05d RESULT OK\n", count);
      tree *tr0;

      if(getenv("DUMP_TREE"))
      {
        tr0 = create_node("Root");
        (void) foobar_build_start(cx, &tr0->t_element.t_node, -m);
        reverse_tree(tr0);
        dump_tree(stdout, cx->cx_input, tr0, 0);
      }
    } else {
      error_pos = error_position(cx);
      if(i > 0) {
        printf("%05d RESULT NOPREFIX; ERROR AT %d\n", count, error_pos);
      } else {
        printf("%05d RESULT PREFIX %d; ERROR AT %d\n", count, m + i, error_pos);
      }
    }
    fflush(stdout);
    delete_context(cx);
  }
#endif
  return 0;
}
