#include <stdio.h>
#include <stdlib.h>
#include <pack.h>

size_t file_resplenish(void *f, uint8_t *target, size_t count)
{
  return fread(target, 1, count, (FILE *) f);
}

int main(int argc, char **argv)
{
  char *fn;
  FILE *f;
  packer_t pk;
  uint64_t x;
  size_t string_length;
  uint8_t *string;

  fn = argv[1];
  f = fopen(fn, "rb");
  if(!f) abort();

  if(!pack_init(&pk, 1024, f, file_resplenish, malloc, free)) abort();
  while(pack_read_uint64(&pk, &x)) {
    if(!pack_read_string(&pk, &string, &string_length)) {
      break;
    } else {
      printf("0x%08lx : %s\n", x, string);
      free(string);
    }
  }
  return 0;
}
