#ifndef __DATA_META_AMAP_IMPL_H
#define __DATA_META_AMAP_IMPL_H

#include <stddef.h>
#include <stdbool.h>
#include "memory/allocator.h"

#define AMAP_IMPL(key_t, val_t, prefix) \
  prefix ## _amap mk_ ## prefix ## _amap(size_t capacity, allocator a) {\
    prefix##_amap out; \
    out.capacity = capacity; \
    out.len = 0; \
    out.data = mem_alloc(capacity * sizeof(prefix##_cell), a); \
    return out; \
  } \
  \
  prefix##_amap copy_##prefix##_amap(prefix##_amap map, key_t (*copy_key)(key_t key, allocator a), val_t (*copy_val)(val_t val, allocator a), allocator a) { \
    prefix##_amap out; \
    out.capacity = map.capacity; \
    out.len = map.len; \
    out.data = mem_alloc(map.capacity * sizeof(prefix##_cell), a); \
    for (size_t i = 0; i < map.len; i++) { \
      out.data[i].key = copy_key(map.data[i].key, a); \
      out.data[i].val = copy_val(map.data[i].val, a); \
    } \
    return out; \
  } \
  void delete_##prefix##_amap(prefix##_amap map, void (*delete_key)(key_t key, allocator a), void (*delete_val)(val_t val, allocator a), allocator a) { \
    for (size_t i = 0; i < map.len; i++) { \
      delete_key(map.data[i].key, a);\
      delete_val(map.data[i].val, a);\
    }\
    mem_free(map.data, a);\
  } \
  \
  void sdelete_##prefix##_amap(prefix##_amap map, allocator a) { \
    mem_free(map.data, a);\
  } \
  \
  val_t* prefix##_lookup(key_t key, prefix ## _amap map) { \
    for (size_t i = 0; i < map.len; i++) { \
        if (key == map.data[i].key) { \
            return &(map.data[i].val); \
        } \
    } \
    return NULL; \
  } \
  bool prefix##_find(size_t* idx, key_t key, prefix ## _amap map) {   \
    for (size_t i = 0; i < map.len; i++) { \
        if (key == map.data[i].key) { \
            *idx = i; \
            return true; \
        } \
    } \
    return false; \
  } \
  \
  void prefix##_insert(key_t key, val_t val, prefix ## _amap* map, allocator a) { \
    bool append = true;\
    size_t i = 0; \
    while (i < map->len && append) { \
      if (key == map->data[i].key) { \
        map->data[i].val = val; \
        append = false; \
      } \
      i++; \
    } \
    if (append) { \
      if (map->len >= map->capacity) { \
        map->capacity *= 2; \
        map->data = mem_realloc(map->data, sizeof(prefix ## _cell) * map->capacity, a);\
      } \
      map->data[i].key = key; \
      map->data[i].val = val; \
      map->len++; \
    } \
  }

#define AMAP_CMP_IMPL(key_t, val_t, cmpfun, prefix) \
  prefix ## _amap mk_ ## prefix ## _amap(size_t capacity, allocator a) {\
    prefix##_amap out; \
    out.capacity = capacity; \
    out.len = 0; \
    out.data = mem_alloc(capacity * sizeof(prefix##_cell), a); \
    return out; \
  } \
  \
  void delete_##prefix##_amap(prefix##_amap map, void (*delete_key)(key_t key, allocator a), void (*delete_val)(val_t val, allocator a), allocator a) { \
    for (size_t i = 0; i < map.len; i++) { \
      delete_key(map.data[i].key, a);\
      delete_val(map.data[i].val, a);\
    }\
  } \
  \
  void sdelete_##prefix##_amap(prefix##_amap map, allocator a) { \
    mem_free(map.data, a);\
  } \
  \
  val_t* prefix##_lookup(key_t key, prefix ## _amap map) { \
    for (size_t i = 0; i < map.len; i++) { \
        if (cmpfun(key, map.data[i].key) == 0) { \
            return &(map.data[i].val); \
        } \
    } \
    return NULL; \
  } \
  bool prefix##_find(size_t* idx, key_t key, prefix ## _amap map) {   \
    for (size_t i = 0; i < map.len; i++) { \
        if (cmpfun(key, map.data[i].key) == 0) { \
            *idx = i; \
            return true; \
        } \
    } \
    return false; \
  } \
  \
  void prefix##_insert(key_t key, val_t val, prefix ## _amap* map, allocator a) { \
    bool append = true;\
    size_t i = 0; \
    while (i < map->len && append) { \
      if (cmpfun(key, map->data[i].key) == 0) { \
        map->data[i].val = val; \
        append = false; \
      } \
      i++; \
    } \
    if (append) { \
      if (map->len >= map->capacity) { \
        map->capacity *= 2; \
        map->data = mem_realloc(map->data, sizeof(prefix ## _cell) * map->capacity, a);\
      } \
      map->data[i].key = key; \
      map->data[i].val = val; \
      map->len++; \
    } \
  }


#endif
