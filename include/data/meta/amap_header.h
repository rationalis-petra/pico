#ifndef __DATA_META_AMAP_HEADER_H
#define __DATA_META_AMAP_HEADER_H

#include <stddef.h>
#include <stdbool.h>
#include "memory/allocator.h"

#define AMAP_HEADER(key_t, val_t, prefix)                               \
    typedef struct prefix ## Cell {                                     \
        key_t key;                                                      \
        val_t val;                                                      \
    } prefix ## Cell;                                                  \
                                                                        \
    typedef struct prefix ## AMap {                                     \
        size_t capacity;                                                \
        size_t len;                                                     \
        Allocator* alloc;                                               \
        prefix ## Cell* data;                                           \
    } prefix ## AMap;                                                   \
                                                                        \
    prefix##AMap mk_##prefix##_amap(size_t capacity, Allocator* a);    \
    prefix##AMap copy_##prefix##_amap(prefix##AMap map, key_t (*copy_key)(key_t key, Allocator* a), val_t (*copy_val)(val_t val, Allocator* a), Allocator* a); \
    void delete_##prefix##_amap(prefix##AMap map, void (*delete_key)(key_t key), void (*delete_val)(val_t val)); \
    void sdelete_##prefix##_amap(prefix##AMap map);                     \
                                                                        \
    val_t* prefix##_lookup(key_t key, prefix##AMap map);                \
    bool prefix##_find(size_t* idx, key_t key, prefix##AMap map);       \
    void prefix##_insert(key_t key, val_t val, prefix##AMap* map);      \

#endif
