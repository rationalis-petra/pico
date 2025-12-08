#ifndef __PICO_DATA_META_AMAP_IMPL_H
#define __PICO_DATA_META_AMAP_IMPL_H

#include <stddef.h>
#include <stdbool.h>
#include "pico/data/client/allocator.h"

#define PICO_AMAP_IMPL(key_t, val_t, fprefix, tprefix)                  \
    tprefix##PiAMap mk_ ## fprefix ## _piamap(size_t capacity, PiAllocator* a) { \
        return (tprefix##PiAMap) {                                      \
            .capacity = capacity,                                       \
            .len = 0,                                                   \
            .data = call_alloc(capacity * sizeof(tprefix##PiCell), a),  \
            .gpa= *a,                                                   \
        };                                                              \
    }                                                                   \
                                                                        \
    tprefix##PiAMap copy_##fprefix##_piamap(tprefix##PiAMap map, key_t (*copy_key)(key_t key, PiAllocator* a), val_t (*copy_val)(val_t val, PiAllocator* a), PiAllocator* a) { \
        tprefix##PiAMap out = (tprefix##PiAMap) {                       \
            .capacity = map.capacity,                                   \
            .len = map.len,                                             \
            .data = call_alloc(map.capacity * sizeof(tprefix##PiCell), a), \
            .gpa = *a,                                                  \
        };                                                              \
        for (size_t i = 0; i < map.len; i++) {                          \
            out.data[i].key = copy_key(map.data[i].key, a);             \
            out.data[i].val = copy_val(map.data[i].val, a);             \
        }                                                               \
        return out;                                                     \
    }                                                                   \
                                                                        \
    tprefix##PiAMap scopy_##fprefix##_piamap(tprefix##PiAMap map, PiAllocator* a) { \
        return (tprefix##PiAMap) {                                      \
            .capacity = map.capacity,                                   \
            .len = map.len,                                             \
            .data = call_alloc(map.capacity * sizeof(tprefix##PiCell), a), \
            .gpa = *a,                                                  \
        };                                                              \
    }                                                                   \
                                                                        \
    void delete_##fprefix##_piamap(tprefix##PiAMap map, void (*delete_key)(key_t key), void (*delete_val)(val_t val)) { \
        for (size_t i = 0; i < map.len; i++) {                          \
            delete_key(map.data[i].key);                                \
            delete_val(map.data[i].val);                                \
        }                                                               \
        call_free(map.data, &map.gpa);                                  \
    }                                                                   \
                                                                        \
    void sdelete_##fprefix##_piamap(tprefix##PiAMap map) {                \
        call_free(map.data, &map.gpa);                                  \
    }                                                                   \
                                                                        \
    val_t* fprefix##_lookup(key_t key, tprefix##PiAMap map) {           \
        for (size_t i = 0; i < map.len; i++) {                          \
            if (key == map.data[i].key) {                               \
                return &(map.data[i].val);                              \
            }                                                           \
        }                                                               \
        return NULL;                                                    \
    }                                                                   \
    bool fprefix##_find(size_t* idx, key_t key, tprefix##PiAMap map) {  \
        for (size_t i = 0; i < map.len; i++) {                          \
            if (key == map.data[i].key) {                               \
                *idx = i;                                               \
                return true;                                            \
            }                                                           \
        }                                                               \
        return false;                                                   \
    }                                                                   \
                                                                        \
    void fprefix##_insert(key_t key, val_t val, tprefix ## PiAMap* map) { \
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
                map->data = call_realloc(map->data, sizeof(tprefix ## PiCell) * map->capacity, &map->gpa); \
            }                                                           \
            map->data[i].key = key;                                     \
            map->data[i].val = val;                                     \
            map->len++;                                                 \
        }                                                               \
    }

#define PICO_AMAP_CMP_IMPL(key_t, val_t, cmpfun, fprefix, tprefix)       \
    tprefix##PiAMap mk_ ## fprefix ## _piamap(size_t capacity, PiAllocator* a) { \
        return (tprefix##PiAMap) {                                      \
            .capacity = capacity,                                       \
            .len = 0,                                                   \
            .data = call_alloc(capacity * sizeof(tprefix##PiCell), a),  \
            .gpa = *a,                                                  \
        };                                                              \
    }                                                                   \
                                                                        \
    tprefix##PiAMap copy_##fprefix##_piamap(tprefix##PiAMap map, key_t (*copy_key)(key_t key, PiAllocator* a), val_t (*copy_val)(val_t val, PiAllocator* a), PiAllocator* a) { \
        tprefix##PiAMap out = (tprefix##PiAMap) {                       \
            .capacity = map.capacity,                                   \
            .len = map.len,                                             \
            .data = call_alloc(map.capacity * sizeof(tprefix##PiCell), a), \
            .gpa = *a,                                                  \
        };                                                              \
        for (size_t i = 0; i < map.len; i++) {                          \
            out.data[i].key = copy_key(map.data[i].key, a);             \
            out.data[i].val = copy_val(map.data[i].val, a);             \
        }                                                               \
        return out;                                                     \
    }                                                                   \
                                                                        \
    tprefix##PiAMap scopy_##fprefix##_piamap(tprefix##PiAMap map, PiAllocator* a) { \
        return (tprefix##PiAMap) {                                      \
            .capacity = map.capacity,                                   \
            .len = map.len,                                             \
            .data = call_alloc(map.capacity * sizeof(tprefix##PiCell), a), \
            .gpa = *a,                                                  \
        };                                                              \
    }                                                                   \
                                                                        \
                                                                        \
    void delete_##fprefix##_piamap(tprefix##PiAMap map, void (*delete_key)(key_t key), void (*delete_val)(val_t val)) { \
        for (size_t i = 0; i < map.len; i++) {                          \
            delete_key(map.data[i].key);                                \
            delete_val(map.data[i].val);                                \
        }                                                               \
        call_free(map.data, &map.gpa);                                  \
    }                                                                   \
                                                                        \
    void sdelete_##fprefix##_piamap(tprefix##PiAMap map) {                \
        call_free(map.data, &map.gpa);                                  \
    }                                                                   \
                                                                        \
    val_t* fprefix##_lookup(key_t key, tprefix##PiAMap map) {           \
        for (size_t i = 0; i < map.len; i++) {                          \
            if (cmpfun(key, map.data[i].key) == 0) {                    \
                return &(map.data[i].val);                              \
            }                                                           \
        }                                                               \
        return NULL;                                                    \
    }                                                                   \
    bool fprefix##_find(size_t* idx, key_t key, tprefix##PiAMap map) {  \
        for (size_t i = 0; i < map.len; i++) {                          \
            if (cmpfun(key, map.data[i].key) == 0) {                    \
                *idx = i;                                               \
                return true;                                            \
            }                                                           \
        }                                                               \
        return false;                                                   \
    }                                                                   \
                                                                        \
    void fprefix##_insert(key_t key, val_t val, tprefix##PiAMap* map) { \
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
                map->data = call_realloc(map->data, sizeof(tprefix##PiCell) * map->capacity, &map->gpa); \
            }                                                           \
            map->data[i].key = key;                                     \
            map->data[i].val = val;                                     \
            map->len++;                                                 \
        }                                                               \
    }

#endif
