#ifndef __DATA_META_ARRAY_HEADER_H
#define __DATA_META_ARRAY_HEADER_H

#include <stddef.h>
#include "memory/allocator.h"

#define ARRAY_HEADER(type, fprefix, tprefix)                            \
    typedef struct tprefix##Array {                                     \
        size_t size;                                                    \
        size_t len;                                                     \
        type* data;                                                     \
        Allocator* gpa;                                                 \
    } tprefix##Array;                                                   \
                                                                        \
    tprefix##Array mk_ ## fprefix ## _array (const size_t size, Allocator* a); \
    tprefix##Array scopy_ ## fprefix ## _array(const tprefix##Array source, Allocator* a); \
    tprefix##Array copy_ ## fprefix ## _array(const tprefix##Array source, type (*copy_elt)(type val, Allocator* a), Allocator* a); \
                                                                        \
    void delete_ ## fprefix ## _array(tprefix##Array arr, void (*delete_elem)(type elem)); \
    void sdelete_ ## fprefix ## _array(tprefix##Array arr);             \
                                                                        \
    void push_ ## fprefix(type val, tprefix##Array* arr);               \
    type pop_ ## fprefix(tprefix##Array* arr);                          \
                                                                        \
    static inline void aset_ ## fprefix(const size_t index, type val, const tprefix##Array arr) { \
        arr.data[index] = val;                                          \
    }                                                                   \
    static inline type aref_ ## fprefix(const size_t index, const tprefix##Array arr) { \
        return arr.data[index];                                         \
    }                                                                   \

#endif
