#ifndef __PICO_DATA_META_ARRAY_IMPL_H
#define __PICO_DATA_META_ARRAY_IMPL_H

#include <string.h>

#ifdef VALIDATE_INPUTS
#include "platform/signals.h"
#include "data/string.h"
#define POP_CHECK(arr) if (arr -> len == 0) panic(mv_string("popped past end!"));
#else
#define POP_CHECK(arr)
#endif

#define PICO_ARRAY_COMMON_IMPL(type, fprefix, tprefix)                  \
    tprefix##PiArray mk_ ## fprefix ## _array(const size_t size, PiAllocator a) { \
        return (tprefix##PiArray){                                      \
            .data = (type*)call_alloc(sizeof(type) * size, a),          \
            .size = size,                                               \
            .len = 0,                                                   \
            .gpa = a,                                                   \
        };                                                              \
    }                                                                   \
    tprefix##PiArray copy_##fprefix##_array(const tprefix##PiArray source, type (*copy_elt)(type, PiAllocator), PiAllocator a) { \
        size_t memsize = sizeof(type) * source.size;                    \
        tprefix##PiArray out = (tprefix##PiArray) {                     \
            .data = call_alloc(memsize, a),                             \
            .len = source.len,                                          \
            .size = source.size,                                        \
            .gpa = a,                                                   \
        };                                                              \
        for (size_t i = 0; i < out.len; i++) {                          \
            out.data[i] = copy_elt(source.data[i], a);                  \
        }                                                               \
        return out;                                                     \
    }                                                                   \
                                                                        \
    tprefix##PiArray scopy_ ## fprefix ## _array(const tprefix##PiArray source, PiAllocator a) { \
        size_t memsize = sizeof(type) * source.size;                    \
        tprefix ## PiArray out;                                         \
        out.data = call_alloc(memsize, a);                              \
        out.len = source.len;                                           \
        out.size = source.size;                                         \
        out.gpa = a;                                                    \
        memcpy(out.data, source.data, memsize);                         \
        return out;                                                     \
    }                                                                   \
                                                                        \
    void delete_ ## fprefix ## _array(tprefix##PiArray arr, void (*delete_elem)(type elem)) { \
        for (size_t i = 0; i < arr.len; i++) {                          \
            delete_elem(arr.data[i]);                                   \
        }                                                               \
        call_free(arr.data, &arr.gpa);                                  \
    }                                                                   \
                                                                        \
    void sdelete_ ## fprefix ## _array(tprefix ## PiArray arr) {        \
        call_free(arr.data, &arr.gpa);                                  \
    }                                                                   \
    void reverse_ ## fprefix ##_array(tprefix##PiArray arr) {           \
        for (size_t i = 0; i < arr.len / 2; i++) {                      \
            type tmp = arr.data[i];                                     \
            arr.data[i] = arr.data[arr.len - (i + 1)];                  \
            arr.data[arr.len - (i + 1)] = tmp;                          \
        }                                                               \
    }                                                                   \
                                                                        \
    void push_ ## fprefix (type val, tprefix ## PiArray* arr) {         \
        if (arr->len < arr->size) {                                     \
            arr->data[arr->len] = val;                                  \
            arr->len++;                                                 \
        }                                                               \
        else {                                                          \
            arr->size = arr->size == 0 ? 8 : arr->size * 2;             \
            arr->data = call_realloc(arr->data, arr->size * sizeof(type), &arr->gpa); \
            arr->data[arr->len] = val;                                  \
            arr->len++;                                                 \
        }                                                               \
    }                                                                   \
    type pop_ ## fprefix(tprefix ## PiArray* arr) {                     \
        POP_CHECK(arr)                                                  \
            arr->len--;                                                 \
        return arr->data[arr->len];                                     \
    }
                                                                        

#define PICO_ARRAY_IMPL(type, fprefix, tprefix)                     \
    PICO_ARRAY_COMMON_IMPL(type, fprefix, tprefix)                  \
        size_t find_ ## fprefix(type val, tprefix##PiArray arr) {   \
        for (size_t i = 0; i < arr.len; i++) {                      \
            if (arr.data[i] == val) return i;                       \
        }                                                           \
        return arr.len;                                             \
    }                                                               \

#define PICO_ARRAY_CMP_IMPL(type, cmpfun, fprefix, tprefix)         \
    PICO_ARRAY_COMMON_IMPL(type, fprefix, tprefix)                  \
        size_t find_ ## fprefix(type val, tprefix##PiArray arr) {   \
        for (size_t i = 0; i < arr.len; i++) {                      \
            if (cmpfun(arr.data[i], val) == 0) return i;            \
        }                                                           \
        return arr.len;                                             \
    }                                                               \


#endif
