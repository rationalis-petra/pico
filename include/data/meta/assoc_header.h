#ifndef __DATA_META_ASSOC_HEADER_H
#define __DATA_META_ASSOC_HEADER_H

#include <stddef.h>
#include "memory/allocator.h"

#define ASSOC_HEADER(key_t, val_t, prefix)                              \
    typedef struct prefix##ACell {                                      \
        key_t key;                                                      \
        val_t val;                                                      \
    } prefix##ACell;                                                    \
                                                                        \
    typedef struct prefix##Assoc {                                      \
        size_t capacity;                                                \
        size_t len;                                                     \
        void (*delete_key)(key_t key, Allocator* a);                    \
        void (*delete_val)(val_t val, Allocator* a);                    \
        prefix ## ACell* data;                                          \
    } prefix ## Assoc;                                                  \
                                                                        \
    prefix##Assoc mk_##prefix##_assoc(size_t capacity, Allocator* a);   \
    void delete_##prefix##_assoc(prefix##Assoc map, void (*delete_key)(key_t key), void (*delete_val)(val_t val)); \
    void sdelete_##prefix##_assoc(prefix##Assoc map, Allocator* a);     \
                                                                        \
    val_t* prefix##_alookup(key_t key, prefix##Assoc map);              \
    void prefix##_bind(key_t key, val_t val, prefix##Assoc* map);       \
    void prefix##_unbind(prefix##Assoc* map);                           \
    void prefix##_unbindn(size_t n, prefix##Assoc* map);                \

#endif
