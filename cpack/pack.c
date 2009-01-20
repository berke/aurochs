/* pack.c
 *
 * Byte packer
 *
 * Copyright(C) 2007 Exalead SA
 */

#include <string.h>
#include <stdio.h>

#include <pack.h>

bool pack_init_from_string(packer_t *pk, u8 *data, size_t size)
{
  pk->p_block_size = size;
  pk->p_index = 0;
  pk->p_length = size;
  pk->p_extra = 0;
  pk->p_observer = 0;
  pk->p_resplenish = 0;
  pk->p_data = data;
  return true;
}

bool pack_init(packer_t *pk, u8 *block_buffer, size_t block_size, void *extra, pack_resplenisher_t resplenish)
{
  pk->p_block_size = block_size;
  pk->p_index = 0;
  pk->p_length = 0;
  pk->p_extra = extra;
  pk->p_observer = 0;
  pk->p_observer_extra = 0;
  pk->p_resplenish = resplenish;
  pk->p_data = block_buffer;
  return true;
}

void pack_shutdown(packer_t *pk)
{
}

static INLINE void pack_observe(packer_t *pk) {
  if(pk->p_observer && pk->p_index) pk->p_observer(pk->p_observer_extra, pk->p_data, pk->p_index);
}

bool pack_finish_observing(packer_t *pk) {
  pack_observe(pk);
  pk->p_observer = 0;
  return true;
}

bool pack_set_observer(packer_t *pk, void *observer_extra, pack_observer_t observe)
{
  pk->p_observer = observe;
  pk->p_observer_extra = observer_extra;
  return true;
}

bool pack_resplenish(packer_t *pk)
{
  size_t m;

  pack_observe(pk);
  m = pk->p_resplenish ? pk->p_resplenish(pk->p_extra, pk->p_data, pk->p_block_size) : 0;
  if(m <= 0)
    return false;
  else {
    pk->p_length = m;
    pk->p_index = 0;
    pack_observe(pk);
    return true;
  }
}

bool pack_read_uint8(packer_t *pk, u8 *result)
{
  if(!(pk->p_index < pk->p_length || pack_resplenish(pk))) return false;

  *result = pk->p_data[pk->p_index ++];

  return true;
}

bool pack_read_bytes(packer_t *pk, u8 *result, size_t count)
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
}

bool pack_read_string(packer_t *pk, u8 **result, size_t *length, alloc_t *alloc)
{
  u64 m;
  
  *result = 0;

  if(!pack_read_uint64(pk, &m)) return false;
  *length = m;
  if(!m) return true;
  *result = alloc_malloc(alloc, m + 1);
  if(!*result) return false;
  if(pack_read_bytes(pk, *result, m)) {
    (*result)[m] = 0;
    return true;
  } else {
    alloc_free(alloc, *result);
    return false;
  }
}

bool pack_read_int64(packer_t *pk, s64 *result)
{
  int i;
  s64 x;
  u8 y;
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
}

bool pack_read_uint64(packer_t *pk, u64 *result)
{
  int i;
  u64 x;
  u8 y;

  x = 0;

  for(i = 0; i < 10; i++) {
    if(!pack_read_uint8(pk, &y)) return false;
    x <<= 7;
    x |= y & (PACK_STOP - 1);
    if(y & PACK_STOP) break;
  }
  *result = x;
  return true;
}

bool pack_read_int(packer_t *pk, int *result) {
  s64 x;

  if(pack_read_int64(pk, &x)) {
    *result = x;
    return true;
  } else {
    return false;
  }
}

bool pack_read_uint(packer_t *pk, unsigned int *result) {
  u64 x;

  if(pack_read_uint64(pk, &x)) {
    *result = x;
    return true;
  } else {
    return false;
  }
}

