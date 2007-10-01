/* Check */

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include <parse_tree.h>
#include <cnog.h>
#include <peg_lib.h>
#include <pack.h>
#include <alloc.h>
#include <stack.h>

static unsigned char *load_file(char *name, size_t *size)/*{{{*/
{
  size_t m;
  struct stat st;
  unsigned char *data;
  FILE *f;
  unsigned char *retval;

  retval = 0;
  
  if(!stat(name, &st)) {
    m = st.st_size;
    *size = m;
    data = malloc(m);
    if(data) {
      f = fopen(name, "rb");
      if(f) {
        if(1 == fread(data, m, 1, f)) {
          retval = data;
          data = 0;
        }
        fclose(f);
      }
      if(data) free(data);
    }
  }

  return retval;
}/*}}}*/
int main(int argc, char **argv)
{
  unsigned char *peg_data;
  char *peg_fn;
  size_t peg_data_size;
  nog_program_t *pg;
  packer_t pk;
  stack_t *st;
  int rc;

  rc = 0;

  argv ++; argc --;

  if(!argc) {
    printf("No PEG data\n");
    exit(EXIT_FAILURE);
  }

  peg_fn = *(argv ++); argc --;
  printf("Loading peg_data from file %s\n", peg_fn);

  peg_data = load_file(peg_fn, &peg_data_size);
  if(!peg_data) {
    printf("Can't load peg data.\n");
    exit(EXIT_FAILURE);
  }

  /* Create a stack allocator */

  st = stack_create(&alloc_stdlib);

  if(pack_init_from_string(&pk, peg_data, peg_data_size)) {
    printf("peg_data[0] = %d\n", peg_data[0]);
    pg = cnog_unpack_program(&st->s_alloc, &pk);
    printf("Unpacked to %p\n", pg);
    if(pg) {
      peg_context_t *cx;
      size_t m;
      int i;
      int error_pos;
      char *fn;
      unsigned char *buf;
      int rc;

      rc = 0;

      for(i = 0; i < argc; i ++) {
        fn = argv[i];

        buf = load_file(fn, &m);
        printf("Loaded file %s to %p\n", fn, buf);
        if(buf) {
          cx = peg_create_context(pg, &parse_tree_builder, buf, m);
          printf("Created context %p\n", cx);
          if(cx) {
            if(cnog_execute(cx, pg, 0)) {
              tree *tr;

              printf("Does parse.\n");
              if(cnog_execute(cx, pg, &tr)) {
                printf("Built.\n");
                cx->cx_builder->pb_dump_tree(cx->cx_builder_info, stdout, buf, tr, 0);
              } else {
                printf("Can't build.\n");
              }
            } else {
              printf("Doesn't parse.\n");
              error_pos = cnog_error_position(cx, pg);
              printf("Error at %d\n", error_pos);
            }

            peg_delete_context(cx);
          }
        }
        free(buf);
      }
#if 0
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
#endif

      cnog_free_program(&st->s_alloc, pg);
    }
  }

  return rc;

#if 0
  { peg_context_t *cx;
    cx = 0;

    printf("cx_input @ %p\n", (char *)(&cx->cx_input));
    printf("cx_input_length @ %p\n", (char *)(&cx->cx_input_length));
    printf("cx_alternatives @ %p\n", (char *)(&cx->cx_alternatives));
    printf("cx_results @ %p\n", (char *)(&cx->cx_results));
  }
#endif

#if 0
  {
    peg_context_t *cx;
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
      cx = peg_create_context(buf, m, NUM_PRODUCTIONS, NUM_ALTERNATIVES);
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
#if 0
  for(count = 1;; count ++) {
    peg_context_t *cx;
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
#endif
  return 0;
}
