#ifndef __DATA_META_AMAP_HEADER_H
#define __DATA_META_AMAP_HEADER_H

#include <stddef.h>
#include <stdbool.h>
#include "platform/memory/allocator.h"

#define AMAP_HEADER(key_t, val_t, fprefix, tprefix)                     \
    typedef struct tprefix ## Cell {                                    \
        key_t key;                                                      \
        val_t val;                                                      \
    } tprefix ## Cell;                                                  \
                                                                        \
    typedef struct tprefix ## AMap {                                    \
        size_t capacity;                                                \
        size_t len;                                                     \
        Allocator* gpa;                                                 \
        tprefix ## Cell* data;                                           \
    } tprefix ## AMap;                                                  \
                                                                        \
    tprefix##AMap mk_##fprefix##_amap(size_t capacity, Allocator* a);   \
    tprefix##AMap scopy_##fprefix##_amap(tprefix##AMap map, Allocator* a); \
    tprefix##AMap copy_##fprefix##_amap(tprefix##AMap map, key_t (*copy_key)(key_t key, Allocator* a), val_t (*copy_val)(val_t val, Allocator* a), Allocator* a); \
    void delete_##fprefix##_amap(tprefix##AMap map, void (*delete_key)(key_t key), void (*delete_val)(val_t val)); \
    void sdelete_##fprefix##_amap(tprefix##AMap map);                   \
                                                                        \
    val_t* fprefix##_lookup(key_t key, tprefix##AMap map);              \
    bool fprefix##_find(size_t* idx, key_t key, tprefix##AMap map);     \
    void fprefix##_insert(key_t key, val_t val, tprefix##AMap* map);    \

#endif
