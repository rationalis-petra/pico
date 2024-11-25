#ifndef __DATA_META_ASSOC_HEADER_H
#define __DATA_META_ASSOC_HEADER_H

#include <stddef.h>
#include "platform/memory/allocator.h"

#define ASSOC_HEADER(key_t, val_t, fprefix, tprefix)                    \
    typedef struct tprefix##ACell {                                     \
        key_t key;                                                      \
        val_t val;                                                      \
    } tprefix##ACell;                                                   \
                                                                        \
    typedef struct tprefix##Assoc {                                     \
        size_t capacity;                                                \
        size_t len;                                                     \
        void (*delete_key)(key_t key, Allocator* a);                    \
        void (*delete_val)(val_t val, Allocator* a);                    \
        tprefix ## ACell* data;                                         \
        Allocator* gpa;                                                 \
    } tprefix##Assoc;                                                   \
                                                                        \
    tprefix##Assoc mk_##fprefix##_assoc(size_t capacity, Allocator* a); \
    void delete_##fprefix##_assoc(tprefix##Assoc map, void (*delete_key)(key_t key), void (*delete_val)(val_t val)); \
    void sdelete_##fprefix##_assoc(tprefix##Assoc map);   \
                                                                        \
    val_t* fprefix##_alookup(key_t key, tprefix##Assoc map);            \
    void fprefix##_bind(key_t key, val_t val, tprefix##Assoc* map);     \
    void fprefix##_unbind(tprefix##Assoc* map);                         \
    void fprefix##_unbindn(size_t n, tprefix##Assoc* map);              \

#endif
