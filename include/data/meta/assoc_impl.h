#ifndef __DATA_META_ASSOC_IMPL_H
#define __DATA_META_ASSOC_IMPL_H

#include <stddef.h>
#include "memory/allocator.h"

#define ASSOC_IMPL(key_t, val_t, prefix)                                \
    prefix##Assoc mk_ ## prefix ## _assoc(size_t capacity, Allocator* a) { \
        return (prefix##_assoc) {                                       \
            .capacity = capacity,                                       \
            .len = 0,                                                   \
            .data = mem_alloc(capacity * sizeof(prefix##_acell), a),    \
            .gpa = a,                                                   \
        };                                                              \
    }                                                                   \
                                                                        \
    void delete_##prefix##_assoc(prefix##Assoc map, void (*delete_key)(key_t key), void (*delete_val)(val_t val)) { \
        for (size_t i = 0; i < map.len; i++) {                          \
            delete_key(map.data[i].key);                                \
            delete_val(map.data[i].val);                                \
        }                                                               \
        mem_free(map.data, map.gpa);                                    \
    }                                                                   \
                                                                        \
    void sdelete_##prefix##_assoc(prefix##Assoc map) {                  \
        mem_free(map.data, map.gpa);                                    \
    }                                                                   \
                                                                        \
    val_t* prefix##_alookup(key_t key, prefix ## Assoc map) {           \
        for (size_t i = map.len; i > 0; i--) {                          \
            if (key == map.data[i - 1].key) {                           \
                return &(map.data[i - 1].val);                          \
            }                                                           \
        }                                                               \
        return NULL;                                                    \
    }                                                                   \
                                                                        \
    void prefix##_bind(key_t key, val_t val, prefix ## Assoc* map) {    \
        if (map->len >= map->capacity) {                                \
            map->capacity *= 2;                                         \
            map->data = mem_realloc(map->data, sizeof(prefix ## _acell) * map->capacity, map->gpa); \
        }                                                               \
        map->data[map->len].key = key;                                  \
        map->data[map->len].val = val;                                  \
        map->len++;                                                     \
    }                                                                   \
                                                                        \
    void prefix##_unbind(prefix ## Assoc* map) {                        \
        map->len --;                                                    \
    }                                                                   \
                                                                        \
    void prefix##_unbindn(size_t n, prefix ## Assoc* map) {             \
        map->len -= n;                                                  \
    }                                                                   \

#endif
