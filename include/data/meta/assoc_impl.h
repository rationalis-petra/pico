#ifndef __DATA_META_ASSOC_IMPL_H
#define __DATA_META_ASSOC_IMPL_H

#include <stddef.h>
#include "memory/allocator.h"

#define ASSOC_IMPL(key_t, val_t, prefix) \
  prefix ## _assoc mk_ ## prefix ## _assoc(size_t capacity, allocator a) {\
    prefix##_assoc out; \
    out.capacity = capacity; \
    out.len = 0; \
    out.data = mem_alloc(capacity * sizeof(prefix##_cell), a); \
    return out; \
  } \
  \
  void delete_##prefix##_assoc(prefix##_assoc map, void (*delete_key)(key_t key, allocator a), void (*delete_val)(val_t val, allocator a), allocator a) { \
    for (size_t i = 0; i < map.len; i++) { \
      delete_key(map.data[i].key, a);\
      delete_val(map.data[i].val, a);\
    }\
    mem_free(map.data, a);\
  } \
  \
  void sdelete_##prefix##_assoc(prefix##_assoc map, allocator a) { \
    mem_free(map.data, a);\
  } \
  \
  val_t* prefix##_alookup(key_t key, prefix ## _assoc map) { \
    for (size_t i = map.len; i > 0; i--) { \
        if (key == map.data[i - 1].key) { \
            return &(map.data[i - 1].val); \
        } \
    } \
    return NULL; \
  } \
  \
  void prefix##_bind(key_t key, val_t val, prefix ## _assoc* map, allocator a) { \
    if (map->len >= map->capacity) { \
      map->capacity *= 2; \
      map->data = mem_realloc(map->data, sizeof(prefix ## _cell) * map->capacity, a);\
    } \
    map->data[map->len].key = key; \
    map->data[map->len].val = val; \
    map->len++; \
  } \
  \
  void prefix##_unbind(prefix ## _assoc* map) { \
      map->len --; \
  } \
  \
  void prefix##_unbindn(size_t n, prefix ## _assoc* map) { \
      map->len -= n; \
  }\

#endif
