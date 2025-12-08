#ifndef __PICO_DATA_META_LIST_HEADER_H
#define __PICO_DATA_META_LIST_HEADER_H

#include <stddef.h>
#include "pico/data/client/allocator.h"

// define the type only
#define PICO_LIST_HEADER_TYPE(type, tprefix)    \
    typedef struct tprefix##PiList {            \
        type* data;                             \
        size_t len;                             \
        size_t size;                            \
        PiAllocator gpa;                        \
    } tprefix##PiList;                          \

#define PICO_LIST_HEADER(type, fprefix, tprefix)                        \
    PICO_LIST_HEADER_TYPE(type, tprefix);                               \
                                                                        \
    tprefix##PiList mk_ ## fprefix ## _list (const size_t size, PiAllocator* a); \
    tprefix##PiList scopy_ ## fprefix ## _list(const tprefix##PiList source, PiAllocator* a); \
    tprefix##PiList copy_ ## fprefix ## _list(const tprefix##PiList source, type (*copy_elt)(type val, PiAllocator* a), PiAllocator* a); \
                                                                        \
    void delete_ ## fprefix ## _list(tprefix##PiList arr, void (*delete_elem)(type elem)); \
    void sdelete_ ## fprefix ## _list(tprefix##PiList arr);            \
    void reverse_ ## fprefix ## _list(tprefix##PiList arr);            \
                                                                        \
    void push_ ## fprefix(type val, tprefix##PiList* arr);              \
    type pop_ ## fprefix(tprefix##PiList* arr);                         \
                                                                        \
    size_t find_ ## fprefix(type val, tprefix##PiList arr);             \

#endif
