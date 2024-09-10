#ifndef __DATA_META_AMAP_IMPL_H
#define __DATA_META_AMAP_IMPL_H

#include <stddef.h>
#include <stdbool.h>
#include "memory/allocator.h"

#define AMAP_IMPL(key_t, val_t, fprefix, tprefix)                       \
    tprefix##AMap mk_ ## fprefix ## _amap(size_t capacity, Allocator* a) { \
        return (tprefix##AMap) {                                        \
            .capacity = capacity,                                       \
            .len = 0,                                                   \
            .data = mem_alloc(capacity * sizeof(tprefix##Cell), a),     \
            .gpa= a,                                                    \
        };                                                              \
    }                                                                   \
                                                                        \
    tprefix##AMap copy_##fprefix##_amap(tprefix##AMap map, key_t (*copy_key)(key_t key, Allocator* a), val_t (*copy_val)(val_t val, Allocator* a), Allocator* a) { \
        tprefix##AMap out;                                              \
        out.capacity = map.capacity;                                    \
        out.len = map.len;                                              \
        out.data = mem_alloc(map.capacity * sizeof(tprefix##Cell), a);  \
        for (size_t i = 0; i < map.len; i++) {                          \
            out.data[i].key = copy_key(map.data[i].key, a);             \
            out.data[i].val = copy_val(map.data[i].val, a);             \
        }                                                               \
        return out;                                                     \
    }                                                                   \
    void delete_##fprefix##_amap(tprefix##AMap map, void (*delete_key)(key_t key), void (*delete_val)(val_t val)) { \
        for (size_t i = 0; i < map.len; i++) {                          \
            delete_key(map.data[i].key);                                \
            delete_val(map.data[i].val);                                \
        }                                                               \
        mem_free(map.data, map.gpa);                                    \
    }                                                                   \
                                                                        \
    void sdelete_##fprefix##_amap(tprefix##AMap map) {                  \
        mem_free(map.data, map.gpa);                                    \
    }                                                                   \
                                                                        \
    val_t* fprefix##_lookup(key_t key, tprefix##AMap map) {             \
        for (size_t i = 0; i < map.len; i++) {                          \
            if (key == map.data[i].key) {                               \
                return &(map.data[i].val);                              \
            }                                                           \
        }                                                               \
        return NULL;                                                    \
    }                                                                   \
    bool fprefix##_find(size_t* idx, key_t key, tprefix##AMap map) {    \
        for (size_t i = 0; i < map.len; i++) {                          \
            if (key == map.data[i].key) {                               \
                *idx = i;                                               \
                return true;                                            \
            }                                                           \
        }                                                               \
        return false;                                                   \
    }                                                                   \
                                                                        \
    void fprefix##_insert(key_t key, val_t val, tprefix ## AMap* map) { \
        bool append = true;                                             \
        size_t i = 0;                                                   \
        while (i < map->len && append) {                                \
            if (key == map->data[i].key) {                              \
                map->data[i].val = val;                                 \
                append = false;                                         \
            }                                                           \
            i++;                                                        \
        }                                                               \
        if (append) {                                                   \
            if (map->len >= map->capacity) {                            \
                map->capacity *= 2;                                     \
                map->data = mem_realloc(map->data, sizeof(tprefix ## Cell) * map->capacity, map->gpa); \
            }                                                           \
            map->data[i].key = key;                                     \
            map->data[i].val = val;                                     \
            map->len++;                                                 \
        }                                                               \
    }

#define AMAP_CMP_IMPL(key_t, val_t, cmpfun, fprefix, tprefix)           \
    tprefix##AMap mk_ ## fprefix ## _amap(size_t capacity, Allocator* a) { \
        return (tprefix##AMap) {                                    \
            .capacity = capacity,                                       \
            .len = 0,                                                   \
            .data = mem_alloc(capacity * sizeof(tprefix##Cell), a),     \
            .gpa = a,                                                   \
        };                                                              \
    }                                                                   \
                                                                        \
    void delete_##fprefix##_amap(tprefix##AMap map, void (*delete_key)(key_t key), void (*delete_val)(val_t val)) { \
        for (size_t i = 0; i < map.len; i++) {                          \
            delete_key(map.data[i].key);                                \
            delete_val(map.data[i].val);                                \
        }                                                               \
        mem_free(map.data, map.gpa);                                    \
    }                                                                   \
                                                                        \
    void sdelete_##fprefix##_amap(tprefix##AMap map) {                  \
        mem_free(map.data, map.gpa);                                    \
    }                                                                   \
                                                                        \
    val_t* fprefix##_lookup(key_t key, tprefix##AMap map) {             \
        for (size_t i = 0; i < map.len; i++) {                          \
            if (cmpfun(key, map.data[i].key) == 0) {                    \
                return &(map.data[i].val);                              \
            }                                                           \
        }                                                               \
        return NULL;                                                    \
    }                                                                   \
    bool fprefix##_find(size_t* idx, key_t key, tprefix##AMap map) {    \
        for (size_t i = 0; i < map.len; i++) {                          \
            if (cmpfun(key, map.data[i].key) == 0) {                    \
                *idx = i;                                               \
                return true;                                            \
            }                                                           \
        }                                                               \
        return false;                                                   \
    }                                                                   \
                                                                        \
    void fprefix##_insert(key_t key, val_t val, tprefix##AMap* map) {   \
        bool append = true;                                             \
        size_t i = 0;                                                   \
        while (i < map->len && append) {                                \
            if (cmpfun(key, map->data[i].key) == 0) {                   \
                map->data[i].val = val;                                 \
                append = false;                                         \
            }                                                           \
            i++;                                                        \
        }                                                               \
        if (append) {                                                   \
            if (map->len >= map->capacity) {                            \
                map->capacity *= 2;                                     \
                map->data = mem_realloc(map->data, sizeof(tprefix##Cell) * map->capacity, map->gpa); \
            }                                                           \
            map->data[i].key = key;                                     \
            map->data[i].val = val;                                     \
            map->len++;                                                 \
        }                                                               \
    }


#endif
