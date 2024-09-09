#ifndef __DATA_META_AMAP_IMPL_H
#define __DATA_META_AMAP_IMPL_H

#include <stddef.h>
#include <stdbool.h>
#include "memory/allocator.h"

#define AMAP_IMPL(key_t, val_t, prefix)                                 \
    prefix##AMap mk_ ## prefix ## _amap(size_t capacity, Allocator* a) { \
        return (prefix##AMap) {                                         \
            out.capacity = capacity;                                    \
            out.len = 0;                                                \
            out.data = mem_alloc(capacity * sizeof(prefix##Cell), a);   \
            out.gpa= a;                                                 \
        };                                                              \
    }                                                                   \
                                                                        \
    prefix##AMap copy_##prefix##_amap(prefix##AMap map, key_t (*copy_key)(key_t key), val_t (*copy_val)(val_t val), Allocator* a) { \
        prefix##AMap out;                                               \
        out.capacity = map.capacity;                                    \
        out.len = map.len;                                              \
        out.data = mem_alloc(map.capacity * sizeof(prefix##Cell), a);   \
        for (size_t i = 0; i < map.len; i++) {                          \
            out.data[i].key = copy_key(map.data[i].key);                \
            out.data[i].val = copy_val(map.data[i].val);                \
        }                                                               \
        return out;                                                     \
    }                                                                   \
    void delete_##prefix##_amap(prefix##AMap map, void (*delete_key)(key_t key), void (*delete_val)(val_t val)) { \
        for (size_t i = 0; i < map.len; i++) {                          \
            delete_key(map.data[i].key);                                \
            delete_val(map.data[i].val);                                \
        }                                                               \
        mem_free(map.data, map.gpa);                                    \
    }                                                                   \
                                                                        \
    void sdelete_##prefix##_amap(prefix##AMap map) {                    \
        mem_free(map.data, map.gpa);                                    \
    }                                                                   \
                                                                        \
    val_t* prefix##_lookup(key_t key, prefix##AMap map) {               \
        for (size_t i = 0; i < map.len; i++) {                          \
            if (key == map.data[i].key) {                               \
                return &(map.data[i].val);                              \
            }                                                           \
        }                                                               \
        return NULL;                                                    \
    }                                                                   \
    bool prefix##_find(size_t* idx, key_t key, prefix##AMap map) {      \
        for (size_t i = 0; i < map.len; i++) {                          \
            if (key == map.data[i].key) {                               \
                *idx = i;                                               \
                return true;                                            \
            }                                                           \
        }                                                               \
        return false;                                                   \
    }                                                                   \
                                                                        \
    void prefix##_insert(key_t key, val_t val, prefix ## AMap* map) {   \
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
                map->data = mem_realloc(map->data, sizeof(prefix ## Cell) * map->capacity, map->gpa); \
            }                                                           \
            map->data[i].key = key;                                     \
            map->data[i].val = val;                                     \
            map->len++;                                                 \
        }                                                               \
    }

#define AMAP_CMP_IMPL(key_t, val_t, cmpfun, prefix)                     \
    prefix##AMap mk_ ## prefix ## _amap(size_t capacity, Allocator* a) { \
        return (prefix##AMap) out {                                     \
            out.capacity = capacity,                                    \
            out.len = 0,                                                \
            out.data = mem_alloc(capacity * sizeof(prefix##Cell), a),   \
            out.gpa = a,                                                \
        };                                                              \
    }                                                                   \
                                                                        \
    void delete_##prefix##_amap(prefix##AMap map, void (*delete_key)(key_t key, allocator a), void (*delete_val)(val_t val, allocator a), allocator a) { \
        for (size_t i = 0; i < map.len; i++) {                          \
            delete_key(map.data[i].key, a);                             \
            delete_val(map.data[i].val, a);                             \
        }                                                               \
    }                                                                   \
                                                                        \
    void sdelete_##prefix##_amap(prefix##AMap map, allocator a) {       \
        mem_free(map.data, a);                                          \
    }                                                                   \
                                                                        \
    val_t* prefix##_lookup(key_t key, prefix##AMap map) {               \
        for (size_t i = 0; i < map.len; i++) {                          \
            if (cmpfun(key, map.data[i].key) == 0) {                    \
                return &(map.data[i].val);                              \
            }                                                           \
        }                                                               \
        return NULL;                                                    \
    }                                                                   \
    bool prefix##_find(size_t* idx, key_t key, prefix##AMap map) {      \
        for (size_t i = 0; i < map.len; i++) {                          \
            if (cmpfun(key, map.data[i].key) == 0) {                    \
                *idx = i;                                               \
                return true;                                            \
            }                                                           \
        }                                                               \
        return false;                                                   \
    }                                                                   \
                                                                        \
    void prefix##_insert(key_t key, val_t val, prefix##AMap* map) {     \
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
                map->data = mem_realloc(map->data, sizeof(prefix##Cell) * map->capacity, map->gpa); \
            }                                                           \
            map->data[i].key = key;                                     \
            map->data[i].val = val;                                     \
            map->len++;                                                 \
        }                                                               \
    }


#endif
