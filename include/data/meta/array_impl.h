#ifndef __DATA_META_ARRAY_IMPL_H
#define __DATA_META_ARRAY_IMPL_H

#include <string.h>

#define ARRAY_COMMON_IMPL(type, fprefix, tprefix)                              \
    tprefix##Array mk_ ## fprefix ## _array(const size_t size, Allocator* a) { \
        return (tprefix##Array){                                        \
            .data = (type*)mem_alloc(sizeof(type) * size, a),           \
            .size = size,                                               \
            .len = 0,                                                   \
            .gpa = a,                                                   \
        };                                                              \
    }                                                                   \
    tprefix##Array copy_##fprefix##_array(const tprefix##Array source, type (*copy_elt)(type, Allocator*), Allocator* a) { \
        size_t memsize = sizeof(type) * source.size;                    \
        tprefix##Array out = (tprefix##Array) {                         \
            .data = mem_alloc(memsize, a),                              \
            .len = source.len,                                          \
            .size = source.size,                                        \
            .gpa = a,                                                   \
        };                                                              \
        for (size_t i = 0; i < out.len; i++) {                         \
            out.data[i] = copy_elt(source.data[i], a);                  \
        }                                                               \
        return out;                                                     \
    }                                                                   \
                                                                        \
    tprefix##Array scopy_ ## fprefix ## _array(const tprefix##Array source, Allocator* a) { \
        size_t memsize = sizeof(type) * source.size;                    \
        tprefix ## Array out;                                           \
        out.data = mem_alloc(memsize, a);                               \
        out.len = source.len;                                           \
        out.size = source.size;                                         \
        out.gpa = a;                                                    \
        memcpy(out.data, source.data, memsize);                         \
        return out;                                                     \
    }                                                                   \
                                                                        \
    void delete_ ## fprefix ## _array(tprefix##Array arr, void (*delete_elem)(type elem)) { \
        for (size_t i = 0; i < arr.len; i++) {                          \
            delete_elem(arr.data[i]);                                   \
        }                                                               \
        mem_free(arr.data, arr.gpa);                                    \
    }                                                                   \
                                                                        \
    void sdelete_ ## fprefix ## _array(tprefix ## Array arr) {          \
        mem_free(arr.data, arr.gpa);                                    \
    }                                                                   \
                                                                        \
    void push_ ## fprefix (type val, tprefix ## Array* arr) {           \
        if (arr->len < arr->size) {                                     \
            arr->data[arr->len] = val;                                  \
            arr->len++;                                                 \
        }                                                               \
        else {                                                          \
            arr->size = arr->size == 0 ? 8 : arr->size * 2;             \
            arr->data = mem_realloc(arr->data, arr->size * sizeof(type), arr->gpa); \
            arr->data[arr->len] = val;                                  \
            arr->len++;                                                 \
        }                                                               \
    }                                                                   \
    type pop_ ## fprefix(tprefix ## Array* arr) {                       \
        arr->len--;                                                     \
        return arr->data[arr->len];                                     \
    }
                                                                        

#define ARRAY_IMPL(type, fprefix, tprefix)                      \
    ARRAY_COMMON_IMPL(type, fprefix, tprefix)                          \
    size_t find_ ## fprefix(type val, tprefix##Array* arr) {    \
        for (size_t i = 0; i < arr->len; i++) {                 \
            if (arr->data[i] == val) return i;                  \
        }                                                       \
        return arr->len;                                        \
    }                                                           \

#define ARRAY_CMP_IMPL(type, cmpfun, fprefix, tprefix)          \
    ARRAY_COMMON_IMPL(type, fprefix, tprefix)                   \
    size_t find_ ## fprefix(type val, tprefix##Array* arr) {    \
        for (size_t i = 0; i < arr->len; i++) {                 \
            if (cmpfun(arr->data[i], val)) return i;            \
        }                                                       \
        return arr->len;                                        \
    }                                                           \


#endif
