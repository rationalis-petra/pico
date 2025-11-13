#ifndef __PICO_DATA_META_AMAP_HEADER_H
#define __PICO_DATA_META_AMAP_HEADER_H

#include <stddef.h>
#include <stdbool.h>
#include "pico/data/client/allocator.h"

#define PICO_AMAP_HEADER_CELL(key_t, val_t, fprefix, tprefix)   \
    typedef struct tprefix ## PiCell {                          \
        key_t key;                                              \
        val_t val;                                              \
    } tprefix ## PiCell;                                        \

#define PICO_AMAP_HEADER_NOCELL(key_t, val_t, fprefix, tprefix)         \
    struct tprefix ## PiCell;                                           \
                                                                        \
    typedef struct tprefix ## PiAMap {                                  \
        size_t capacity;                                                \
        size_t len;                                                     \
        PiAllocator gpa;                                                \
        struct tprefix ## PiCell* data;                                 \
    } tprefix ## PiAMap;                                                \
                                                                        \
    tprefix##PiAMap mk_##fprefix##_piamap(size_t capacity, PiAllocator* a); \
    tprefix##PiAMap scopy_##fprefix##_piamap(tprefix##PiAMap map, PiAllocator* a); \
    tprefix##PiAMap copy_##fprefix##_piamap(tprefix##PiAMap map, key_t (*copy_key)(key_t key, PiAllocator* a), val_t (*copy_val)(val_t val, PiAllocator* a), PiAllocator* a); \
    void delete_##fprefix##_piamap(tprefix##PiAMap map, void (*delete_key)(key_t key), void (*delete_val)(val_t val)); \
    void sdelete_##fprefix##_piamap(tprefix##PiAMap map);               \
                                                                        \
    val_t* fprefix##_lookup(key_t key, tprefix##PiAMap map);            \
    bool fprefix##_find(size_t* idx, key_t key, tprefix##PiAMap map);   \
    void fprefix##_insert(key_t key, val_t val, tprefix##PiAMap* map);  \

#define PICO_AMAP_HEADER(key_t, val_t, fprefix, tprefix)        \
    PICO_AMAP_HEADER_NOCELL(key_t, val_t, fprefix, tprefix);    \
    PICO_AMAP_HEADER_CELL(key_t, val_t, fprefix, tprefix);      \

#endif
