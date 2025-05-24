#ifndef __DATA_META_ASSOC_IMPL_H
#define __DATA_META_ASSOC_IMPL_H

#include <stddef.h>
#include "platform/memory/allocator.h"

#define ASSOC_COMMON_IMPL(key_t, val_t, fprefix, tprefix)               \
    tprefix##Assoc mk_ ## fprefix ## _assoc(size_t capacity, Allocator* a) { \
        return (tprefix##Assoc) {                                       \
            .capacity = capacity,                                       \
            .len = 0,                                                   \
            .data = mem_alloc(capacity * sizeof(tprefix##ACell), a),    \
            .gpa = a,                                                   \
        };                                                              \
    }                                                                   \
                                                                        \
    void delete_##fprefix##_assoc(tprefix##Assoc map, void (*delete_key)(key_t key), void (*delete_val)(val_t val)) { \
        for (size_t i = 0; i < map.len; i++) {                          \
            delete_key(map.data[i].key);                                \
            delete_val(map.data[i].val);                                \
        }                                                               \
        mem_free(map.data, map.gpa);                                    \
    }                                                                   \
                                                                        \
    void sdelete_##fprefix##_assoc(tprefix##Assoc map) {                 \
        mem_free(map.data, map.gpa);                                    \
    }                                                                   \
                                                                        \
    tprefix##Assoc scopy_##fprefix##_assoc(tprefix##Assoc map, Allocator* a) { \
        return (tprefix##Assoc) {                                       \
            .capacity = map.len,                                        \
            .len = map.len,                                             \
            .data = mem_alloc(map.len * sizeof(tprefix##ACell), a),      \
            .gpa = a,                                                   \
        };                                                              \
    }                                                                   \
                                                                        \
    void fprefix##_bind(key_t key, val_t val, tprefix##Assoc* map) {    \
        if (map->len >= map->capacity) {                                \
            map->capacity = map->capacity == 0 ? 8 : map->capacity * 2; \
            map->data = mem_realloc(map->data, sizeof(tprefix##ACell) * map->capacity, map->gpa); \
        }                                                               \
        map->data[map->len].key = key;                                  \
        map->data[map->len].val = val;                                  \
        map->len++;                                                     \
    }                                                                   \
                                                                        \
    void fprefix##_unbind(tprefix##Assoc* map) {                        \
        map->len --;                                                    \
    }                                                                   \
                                                                        \
    void fprefix##_unbindn(size_t n, tprefix##Assoc* map) {             \
        map->len -= n;                                                  \
    }                                                                   \

#define ASSOC_IMPL(key_t, val_t, fprefix, tprefix)                      \
    ASSOC_COMMON_IMPL(key_t, val_t, fprefix, tprefix)                   \
    val_t* fprefix##_alookup(key_t key, tprefix##Assoc map) {           \
        for (size_t i = map.len; i > 0; i--) {                          \
            if (key == map.data[i - 1].key) {                           \
                return &(map.data[i - 1].val);                          \
            }                                                           \
        }                                                               \
        return NULL;                                                    \
    }                                                                   \

#define ASSOC_CMP_IMPL(key_t, val_t, cmpfun, fprefix, tprefix)  \
    ASSOC_COMMON_IMPL(key_t, val_t, fprefix, tprefix)                   \
    val_t* fprefix##_alookup(key_t key, tprefix##Assoc map) {           \
        for (size_t i = map.len; i > 0; i--) {                          \
            if (cmpfun(key, map.data[i - 1].key) == 0) {                \
                return &(map.data[i - 1].val);                          \
            }                                                           \
        }                                                               \
        return NULL;                                                    \
    }                                                                   \
                                                                        \

#endif
