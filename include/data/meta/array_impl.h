#ifndef __DATA_META_ARRAY_IMPL_H
#define __DATA_META_ARRAY_IMPL_H

#include <string.h>

#define ARRAY_IMPL(type, prefix)                                        \
    prefix##Array mk_ ## prefix ## _array(const size_t size, Allocator* a) { \
        return (prefix##Array){                                         \
            .data = (type*)mem_alloc(sizeof(type) * size, a),           \
            .size = size,                                               \
            .len = 0,                                                   \
            .gpa = a,                                                   \
        };                                                              \
    }                                                                   \
    prefix##Array copy_##prefix##_array(const prefix##Array source, type (*copy_elt)(type, Allocator*), Allocator* a) { \
        size_t memsize = sizeof(type) * source.size;                    \
        prefix ## _array out;                                           \
        out.data = mem_alloc(memsize, a);                               \
        out.len = source.len;                                           \
        out.size = source.size;                                         \
        out.gpa = a;                                                    \
        for (size_t i = 0; i < out.size; i++) {                         \
            out.data[i] = copy_elt(source.data[i], a);                  \
        }                                                               \
        return out;                                                     \
    }                                                                   \
                                                                        \
    prefix##Array scopy_ ## prefix ## _array(const prefix##Array source, Allocator* a) { \
        size_t memsize = sizeof(type) * source.size;                    \
        prefix ## _array out;                                           \
        out.data = mem_alloc(memsize, a);                               \
        out.len = source.len;                                           \
        out.size = source.size;                                         \
        out.gpa = a;                                                    \
        memcpy(out.data, source.data, memsize);                         \
        return out;                                                     \
    }                                                                   \
                                                                        \
    void delete_ ## prefix ## _array(prefix ## _array arr, void (*delete_elem)(type elem)) { \
        for (size_t i = 0; i < arr.len; i++) {                          \
            delete_elem(arr.data[i], a);                                \
        }                                                               \
        mem_free(arr.data, arr.gpa);                                    \
    }                                                                   \
                                                                        \
    void sdelete_ ## prefix ## _array(prefix ## _array arr) {           \
        mem_free(arr.data, arr.gpa);                                    \
    }                                                                   \
                                                                        \
    void push_ ## prefix (type val, prefix ## _array* arr) {            \
        if (arr->len < arr->size) {                                     \
            arr->data[arr->len] = val;                                  \
            arr->len++;                                                 \
        }                                                               \
        else {                                                          \
            arr->size = arr->size * 2;                                  \
            arr->data = mem_realloc(arr->data, arr->size * sizeof(type), arr->gpa); \
            arr->data[arr->len] = val;                                  \
            arr->len++;                                                 \
        }                                                               \
    }                                                                   \
    type pop_ ## prefix(prefix ## _array* arr) {                        \
        arr->len--;                                                     \
        return arr->data[arr->len];                                     \
    } 

#endif
