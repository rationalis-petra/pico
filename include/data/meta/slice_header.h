#ifndef __DATA_SLICE_HEADER_H
#define __DATA_SLICE_HEADER_H

#include <stddef.h>

// define the type only
#define SLICE_TYPE(type, tprefix)                   \
    typedef struct {                                \
        type* data;                                 \
        size_t len;                                 \
    } tprefix##Slice;                               \

#endif
