/* pack.h
 *
 * Byte packer
 *
 * Copyright(C) 2007 Exalead SA
 */

#ifndef PACK_H
#define PACK_H

#include <base_types.h>

typedef size_t (*pack_resplenisher_t)(void *, uint8_t *, size_t);
  
typedef struct {
  size_t p_block_size;
  size_t p_index;
  size_t p_length;
  void *p_extra;
  pack_resplenisher_t p_resplenish;
  void *(*p_malloc)(size_t);
  void (*p_free)(void *);
  uint8_t *p_data;
} packer_t;

#define PACK_STOP 0x80
#define PACK_NEGATIVE 0x40

bool pack_init(packer_t *pk, size_t block_size, void *extra, pack_resplenisher_t resplenish, void *(*malloc)(size_t), void (*free)(void *));
bool pack_init_from_string(packer_t *pk, u8 *data, size_t size, void *(*malloc)(size_t), void (*free)(void *));
void pack_shutdown(packer_t *pk);
bool pack_resplenish(packer_t *pk);
bool pack_read_uint8(packer_t *pk, uint8_t *result);
bool pack_read_bytes(packer_t *pk, uint8_t *result, size_t count);
bool pack_read_string(packer_t *pk, uint8_t **result, size_t *length);
bool pack_read_int64(packer_t *pk, int64_t *result);
bool pack_read_uint64(packer_t *pk, uint64_t *result);
bool pack_read_int(packer_t *pk, int *result);
bool pack_read_uint(packer_t *pk, unsigned int *result);

#endif
