#ifndef __DATA_SLICE_HEADER_H
#define __DATA_SLICE_HEADER_H

#include <stddef.h>

// define the type only
#define SLICE_TYPE(type, tprefix)                   \
    typedef struct {                                \
        type* data;                                 \
        size_t len;                                 \
    } tprefix##Slice;                               \

#define SLICE_MAP_TYPE(type1, type2, tprefix)   \
    typedef struct {                            \
        type1 key;                              \
        type2 val;                              \
    } tprefix##SCell;                           \
    typedef struct {                            \
        tprefix##SCell* data;                   \
        size_t len;                             \
    } tprefix##Slice;

#endif
