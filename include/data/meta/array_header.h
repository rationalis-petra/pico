#ifndef __DATA_META_ARRAY_HEADER_H
#define __DATA_META_ARRAY_HEADER_H

#include <stddef.h>
#include "memory/allocator.h"

#define ARRAY_HEADER(type, prefix) \
  typedef struct prefix ## _array { \
    size_t size; \
    size_t len; \
    type* data; \
  } prefix ## _array; \
  \
  prefix ## _array mk_ ## prefix ## _array (const size_t size, allocator a); \
  prefix ## _array copy_ ## prefix ## _array(const prefix ## _array source, allocator a); \
  \
  void delete_ ## prefix ## _array(prefix ## _array arr, void (*delete_elem)(type elem, allocator allocator), allocator a); \
  void sdelete_ ## prefix ## _array(prefix ## _array arr, allocator a); \
  \
  void push_ ## prefix(type val, prefix ## _array* arr, allocator a); \
  type pop_ ## prefix(prefix ## _array* arr); \
  \
  static inline void aset_ ## prefix(const size_t index, type val, const prefix ## _array arr) { \
        arr.data[index] = val;\
  } \
  static inline type aref_ ## prefix(const size_t index, const prefix ## _array arr) { \
        return arr.data[index];\
  } \

#endif
