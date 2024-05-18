#ifndef __DATA_META_ASSOC_HEADER_H
#define __DATA_META_ASSOC_HEADER_H

#include <stddef.h>
#include "memory/allocator.h"

#define ASSOC_HEADER(key_t, val_t, prefix) \
  typedef struct prefix ## _acell {  \
    key_t key; \
    val_t val; \
  } prefix ## _acell; \
  \
  typedef struct prefix ## _assoc { \
    size_t capacity; \
    size_t len; \
    void (*delete_key)(key_t key, allocator a); \
    void (*delete_val)(val_t val, allocator a); \
    prefix ## _cell* data; \
  } prefix ## _assoc; \
  \
  prefix##_assoc mk_##prefix##_assoc(size_t capacity, allocator a);       \
  void delete_##prefix##_assoc(prefix##_assoc map, void (*delete_key)(key_t key, allocator a), void (*delete_val)(val_t val, allocator a), allocator a); \
  void sdelete_##prefix##_assoc(prefix##_assoc map, allocator a); \
  \
  val_t* prefix##_alookup(key_t key, prefix ## _assoc map); \
  void prefix##_bind(key_t key, val_t val, prefix ## _assoc* map, allocator a); \
  void prefix##_unbind(prefix ## _assoc* map);    \
  void prefix##_unbindn(size_t n, prefix ## _assoc* map);    \

#endif
