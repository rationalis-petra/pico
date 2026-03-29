#ifndef __DATA_META_CIRCLE_BUFFER_HEADER_H
#define __DATA_META_CIRCLE_BUFFER_HEADER_H

#define CIRCLE_BUFFER_HEADER_TYPE(type, tprefix)    \
    typedef struct tprefix##CBuffer {               \
        type *data;                                 \
        size_t start;                               \
        size_t end;                                 \
        size_t size;                                \
        Allocator gpa;                              \
    } tprefix##CBuffer;


#define CIRCLE_BUFFER_HEADER(type, fprefix, tprefix)                    \
    CIRCLE_BUFFER_HEADER_TYPE(type, tprefix);                           \
                                                                        \
    tprefix##CBuffer mk_ ## fprefix ## _cbuffer(const size_t size, Allocator *a); \
    tprefix##CBuffer scopy_ ## fprefix ## _cbuffer(const tprefix##CBuffer size, Allocator *a); \
                                                                        \
    tprefix##CBuffer sdelete_ ## fprefix ## _cbuffer(const tprefix##CBuffer size); \
                                                                        \
    void push_ ## fprefix ## _cbuffer(type val, tprefix##cbuffer cbuffer); \
    type pop_ ## fprefix ## _cbuffer(tprefix##cbuffer cbuffer);         \


#endif
