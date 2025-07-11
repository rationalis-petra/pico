#ifndef __DATA_META_ASSOC_HEADER_H
#define __DATA_META_ASSOC_HEADER_H

#include <stddef.h>
#include "platform/memory/allocator.h"

#define ASSOC_HEADER_CELL(key_t, val_t, fprefix, tprefix)              \
    typedef struct tprefix##ACell {                                    \
        key_t key;                                                     \
        val_t val;                                                     \
    } tprefix##ACell;                                                  \

#define ASSOC_HEADER_NOCELL(key_t, val_t, fprefix, tprefix)            \
    struct tprefix##ACell;                                             \
                                                                       \
    typedef struct tprefix##Assoc {                                    \
        struct tprefix ## ACell* data;                                 \
        size_t len;                                                    \
        size_t capacity;                                               \
        Allocator gpa;                                                 \
    } tprefix##Assoc;                                                  \
                                                                       \
    tprefix##Assoc mk_##fprefix##_assoc(size_t capacity, Allocator* a); \
    void delete_##fprefix##_assoc(tprefix##Assoc map, void (*delete_key)(key_t key), void (*delete_val)(val_t val)); \
    void sdelete_##fprefix##_assoc(tprefix##Assoc map);                 \
                                                                       \
    tprefix##Assoc scopy_##fprefix##_assoc(tprefix##Assoc map, Allocator* a); \
                                                                       \
    val_t* fprefix##_alookup(key_t key, tprefix##Assoc map);            \
    void fprefix##_bind(key_t key, val_t val, tprefix##Assoc* map);     \
    void fprefix##_unbind(tprefix##Assoc* map);                         \
    void fprefix##_unbindn(size_t n, tprefix##Assoc* map);              \

#define ASSOC_HEADER(key_t, val_t, fprefix, tprefix)    \
    ASSOC_HEADER_CELL(key_t, val_t, fprefix, tprefix)   \
    ASSOC_HEADER_NOCELL(key_t, val_t, fprefix, tprefix)

#endif
