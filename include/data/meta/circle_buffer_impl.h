#ifndef __DATA_META_CIRCLE_BUFFER_IMPL_H
#define __DATA_META_CIRCLE_BUFFER_IMPL_H

#include <string.h>

#ifdef VALIDATE_INPUTS
#include "platform/signals.h"
#include "data/string.h"
#define POP_CHECK(buf) if (buf -> start == buf -> end) panic(mv_string("popped past end!"));
#else
#define POP_CHECK(buf)
#endif

#define CIRCLE_BUFFER_IMPL(type, fprefix, tprefix)                      \
    tprefix##CBuffer mk_##fprefix##_cbuffer(const size_t size, Allocator *a) { \
        return (tprefix##CBuffer) {                                     \
            .data = (type *)mem_alloc(sizeof(type) * size, a),          \
            .size = size,                                               \
            .start = 0,                                                 \
            .end = 0,                                                   \
            .gpa = *a                                                   \
        };                                                              \
    }                                                                   \
                                                                        \
    tprefix##CBuffer scopy_##fprefix##_cbuffer(const tprefix##CBuffer old, \
                                               Allocator *a) {          \
        tprefix##CBuffer new_buf =                                      \
            (tprefix##CBuffer){.data = (type *)mem_alloc(sizeof(type) * old.size, a), \
                               .size = old.size,                        \
                               .start = old.start,                      \
                               .end = old.end,                          \
                               .gpa = *a};                              \
                                                                        \
        size_t start = old.start < old.end ? old.start : old.end;       \
        size_t end = old.start < old.end ? old.end : old.start;         \
        memcpy(old.data + start, old.data + end, (end - start) * sizeof(type)); \
        return new_buf;                                                 \
    }                                                                   \
                                                                        \
    void sdelete_##fprefix##_cbuffer(tprefix##CBuffer buf) { \
        mem_free(buf.data, &buf.gpa);                                    \
    };                                                                  \
                                                                        \
    void push_##fprefix##_cbuffer(type val, tprefix##CBuffer * buf) {   \
        /* Check if we need to grow cbuffer */                          \
        if ((buf->end + 1) % buf->size == buf->start) {                 \
            size_t old_size = buf->size;                                \
            buf->size *= 2;                                             \
            type *new_data = mem_alloc(buf->size, &buf->gpa);           \
            if (buf->start < buf->end) {                                \
                /* only true in cases where buf->start == 0 */          \
                memcpy(new_data, buf->data, old_size * sizeof(type));   \
            } else {                                                    \
                memcpy(new_data, buf->data + buf->start,                \
                       (old_size - buf->start) * sizeof(type));         \
                memcpy(new_data + buf->end, buf->data, buf->start * sizeof(type)); \
            }                                                           \
            mem_free(buf->data, &buf->gpa);                             \
            buf->data = new_data;                                       \
            buf->start = 0;                                             \
            buf->end = old_size;                                        \
        } else {                                                        \
            buf->data[buf->end] = val;                                  \
            buf->end = (buf->end + 1) % buf->size;                      \
        }                                                               \
    }                                                                   \
                                                                        \
    type pop_##fprefix##_cbuffer(tprefix##CBuffer * buf) {              \
        POP_CHECK(buf);                                                 \
                                                                        \
        type val = buf->data[buf->start];                               \
        buf->start++;                                                   \
        return val;                                                     \
    }                                                                   \
                                                                        \
    bool fprefix##_not_empty(tprefix##CBuffer buf) {                    \
        return buf.start == buf.end;                                    \
    }                                                                   \
                                                                        \
    size_t fprefix##_len(tprefix##CBuffer buf) {                           \
        return buf.start < buf.end ? buf.end - buf.start : buf.start - buf.end; \
    }                                                                   \

#endif
