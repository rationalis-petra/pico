#ifndef __DATA_META_AMAP_HEADER_H
#define __DATA_META_AMAP_HEADER_H

#include <stddef.h>
#include <stdbool.h>
#include "memory/allocator.h"

#define AMAP_HEADER(key_t, val_t, prefix) \
  typedef struct prefix ## _cell {  \
    key_t key; \
    val_t val; \
  } prefix ## _cell; \
  \
  typedef struct prefix ## _amap { \
    size_t capacity; \
    size_t len; \
    void (*delete_key)(key_t key, allocator a); \
    void (*delete_val)(val_t val, allocator a); \
    prefix ## _cell* data; \
  } prefix ## _amap; \
  \
  prefix##_amap mk_##prefix##_amap(size_t capacity, allocator a); \
  prefix##_amap copy_##prefix##_amap(prefix##_amap map, key_t (*copy_key)(key_t key, allocator a), val_t (*copy_val)(val_t val, allocator a), allocator a); \
  void delete_##prefix##_amap(prefix##_amap map, void (*delete_key)(key_t key, allocator a), void (*delete_val)(val_t val, allocator a), allocator a); \
  void sdelete_##prefix##_amap(prefix##_amap map, allocator a); \
  \
  val_t* prefix##_lookup(key_t key, prefix ## _amap map); \
  bool prefix##_find(size_t* idx, key_t key, prefix ## _amap map); \
  void prefix##_insert(key_t key, val_t val, prefix ## _amap* map, allocator a); \

#endif
