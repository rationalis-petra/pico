#ifndef __DATA_STRINGBUFFER_H
#define __DATA_STRINGBUFFER_H

#include "data/string.h"
#include "data/array.h"

typedef struct {
    size_t cursor_pos;
    U8Array bytes;
} StringBuffer;

StringBuffer mk_string_buffer(size_t capacity, Allocator* a);

void insert_char(uint32_t codepoint, StringBuffer* buffer);
void delete_nchars(size_t n, StringBuffer* buffer);

String get_contents(StringBuffer buffer, Allocator* a);

#endif
