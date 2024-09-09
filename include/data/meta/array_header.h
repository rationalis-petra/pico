#ifndef __DATA_META_ARRAY_HEADER_H
#define __DATA_META_ARRAY_HEADER_H

#include <stddef.h>
#include "memory/allocator.h"

#define ARRAY_HEADER(type, prefix)                                      \
    typedef struct prefix ## Array {                                    \
        size_t size;                                                    \
        size_t len;                                                     \
        type* data;                                                     \
        Allocator* gpa;                                                 \
    } prefix##Array;                                                    \
                                                                        \
    prefix##Array mk_ ## prefix ## _array (const size_t size, Allocator* a); \
    prefix##Array scopy_ ## prefix ## _array(const prefix##Array source, Allocator* a); \
    prefix##Array copy_ ## prefix ## _array(const prefix##Array source, type (*copy_elt)(type val, Allocator* a), Allocator* a); \
                                                                        \
    void delete_ ## prefix ## _array(prefix##Array arr, void (*delete_elem)(type elem)); \
    void sdelete_ ## prefix ## _array(prefix##Array arr);               \
                                                                        \
    void push_ ## prefix(type val, prefix##Array* arr);              \
    type pop_ ## prefix(prefix##Array* arr);                         \
                                                                        \
    static inline void aset_ ## prefix(const size_t index, type val, const prefix##Array arr) { \
        arr.data[index] = val;                                          \
    }                                                                   \
    static inline type aref_ ## prefix(const size_t index, const prefix##Array arr) { \
        return arr.data[index];                                         \
    }                                                                   \

#endif
