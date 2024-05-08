#ifndef __DATA_META_ARRAY_IMPL_H
#define __DATA_META_ARRAY_IMPL_H

#include <stddef.h>

#define ARRAY_IMPL(type, prefix) \
  prefix ## _array mk_ ## prefix ## _array(const size_t size, allocator a) { \
    prefix ## _array out; \
    out.data = (type*)mem_alloc(sizeof(type) * size, a); \
    out.size = size;\
    out.len = 0; \
    return out; \
  } \
  \
  prefix ## _array copy_ ## prefix ## _array(const prefix ## _array source, allocator a) { \
    size_t memsize = sizeof(type) * source.size; \
    prefix ## _array out; \
    out.data = mem_alloc(memsize, a); \
    out.len = source.len; \
    out.size = source.size; \
    memcpy(out.data, source.data, memsize); \
    return out;\
  } \
  \
  void delete_ ## prefix ## _array(prefix ## _array arr, void (*delete_elem)(type elem, allocator a), allocator a) { \
      for (size_t i = 0; i < arr.len; i++) { \
          delete_elem(arr.data[i], a); \
      } \
      mem_free(arr.data, a); \
  } \
  \
  void sdelete_ ## prefix ## _array(prefix ## _array arr, allocator a) { \
      mem_free(arr.data, a); \
  } \
  \
  void push_ ## prefix (type val, prefix ## _array* arr, allocator a) { \
    if (arr->len < arr->size) { \
        arr->data[arr->len] = val; \
        arr->len++; \
    } \
    else { \
        arr->size = arr->size * 2; \
        arr->data = mem_realloc(arr->data, arr->size * sizeof(type), a); \
        arr->data[arr->len] = val; \
        arr->len++; \
    } \
  } \
  type pop_ ## prefix(prefix ## _array* arr) { \
    arr->len--;\
    return arr->data[arr->len]; \
  } 

#endif
