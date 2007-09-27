/* pack.c
 *
 * Byte packer
 *
 * Copyright(C) 2007 Exalead SA
 */

#include <string.h>
#include <stdio.h>

#include <pack.h>

bool pack_init(packer_t *pk, size_t block_size, void *extra, pack_resplenisher_t resplenish, void *(*malloc)(size_t), void (*free)(void *))/*{{{*/
{
  pk->p_block_size = block_size;
  pk->p_index = 0;
  pk->p_length = 0;
  pk->p_extra = extra;
  pk->p_resplenish = resplenish;
  pk->p_malloc = malloc;
  pk->p_free = free;
  pk->p_data = malloc(block_size);
  if(!pk->p_data) return false;
  return true;
}/*}}}*/
void pack_shutdown(packer_t *pk)/*{{{*/
{
  pk->p_free(pk->p_data);
}/*}}}*/
bool pack_resplenish(packer_t *pk)/*{{{*/
{
  size_t m;
  m = pk->p_resplenish(pk->p_extra, pk->p_data, pk->p_block_size);
  if(m <= 0)
    return false;
  else {
    pk->p_length = m;
    pk->p_index = 0;
    return true;
  }
}/*}}}*/
bool pack_read_uint8(packer_t *pk, uint8_t *result)/*{{{*/
{
  if(!(pk->p_index < pk->p_length || pack_resplenish(pk))) return false;

  *result = pk->p_data[pk->p_index ++];

  return true;
}/*}}}*/
bool pack_read_bytes(packer_t *pk, uint8_t *result, size_t count)/*{{{*/
{
  size_t chunk;

  while(count > 0) {
    if(!(pk->p_index < pk->p_length || pack_resplenish(pk))) return false;
    chunk = pk->p_length - pk->p_index;
    if(count < chunk) chunk = count;
    memcpy(result, pk->p_data + pk->p_index, chunk);
    pk->p_index += chunk;
    count -= chunk;
    result += chunk;
  }
  return true;
}/*}}}*/
bool pack_read_string(packer_t *pk, uint8_t **result, size_t *length)/*{{{*/
{
  size_t m;
  
  if(!pack_read_uint64(pk, &m)) return false;
  if(m < 0) return false;
  *length = m;
  if(!m) {
    *result = 0;
    return true;
  }
  *result = pk->p_malloc(m + 1);
  if(!*result) return false;
  if(pack_read_bytes(pk, *result, m)) {
    (*result)[m] = 0;
    return true;
  } else {
    pk->p_free(*result);
    return false;
  }
}/*}}}*/
bool pack_read_int64(packer_t *pk, int64_t *result)/*{{{*/
{
  int i;
  int64_t x;
  uint8_t y;
  bool positive;

  x = 0;

  if(!pack_read_uint8(pk, &y)) return false;

  positive = !(y & PACK_NEGATIVE);
  x = y & (PACK_NEGATIVE - 1);
  if(!(y & PACK_STOP)) {
    for(i = 0; i < 10; i++) {
      if(!pack_read_uint8(pk, &y)) return false;
      x <<= 7;
      x |= y & (PACK_STOP - 1);
      if(y & PACK_STOP) break;
    }
  }
  *result = positive ? x : -x-1;
  return true;
}/*}}}*/
bool pack_read_uint64(packer_t *pk, uint64_t *result)/*{{{*/
{
  int i;
  uint64_t x;
  uint8_t y;

  x = 0;

  for(i = 0; i < 10; i++) {
    if(!pack_read_uint8(pk, &y)) return false;
    x <<= 7;
    x |= y & (PACK_STOP - 1);
    if(y & PACK_STOP) break;
  }
  *result = x;
  return true;
}/*}}}*/
bool pack_read_int(packer_t *pk, int *result) {/*{{{*/
  int64_t x;

  if(pack_read_int64(pk, &x)) {
    *result = x;
    return true;
  } else {
    return false;
  }
}/*}}}*/
