#ifndef __DATA_META_ARRAY_HEADER_H
#define __DATA_META_ARRAY_HEADER_H

#include <stddef.h>
#include "platform/memory/allocator.h"

#define ARRAY_HEADER(type, fprefix, tprefix)                            \
    typedef struct tprefix##Array {                                     \
        type* data;                                                     \
        size_t len;                                                     \
        size_t size;                                                    \
        Allocator* gpa;                                                 \
    } tprefix##Array;                                                   \
                                                                        \
    tprefix##Array mk_ ## fprefix ## _array (const size_t size, Allocator* a); \
    tprefix##Array scopy_ ## fprefix ## _array(const tprefix##Array source, Allocator* a); \
    tprefix##Array copy_ ## fprefix ## _array(const tprefix##Array source, type (*copy_elt)(type val, Allocator* a), Allocator* a); \
                                                                        \
    void delete_ ## fprefix ## _array(tprefix##Array arr, void (*delete_elem)(type elem)); \
    void sdelete_ ## fprefix ## _array(tprefix##Array arr);             \
    void reverse_ ## fprefix ## _array(tprefix##Array arr);             \
                                                                        \
    void push_ ## fprefix(type val, tprefix##Array* arr);               \
    type pop_ ## fprefix(tprefix##Array* arr);                          \
                                                                        \
    size_t find_ ## fprefix(type val, tprefix##Array arr);              \

#endif
