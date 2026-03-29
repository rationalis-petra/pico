#include <string.h>

#include "data/stringbuffer.h"

#include "components/encodings/utf8.h"

StringBuffer mk_string_buffer(size_t capacity, Allocator *a) {
    StringBuffer out = {
        .cursor_pos = 0,
        .bytes = mk_u8_array(capacity, a),
    };
    push_u8(0, &out.bytes);
    return out;
}

void insert_char(uint32_t codepoint, StringBuffer *buffer) {
    uint8_t chars[4];
    uint8_t size;
    encode_point_utf8(chars, &size, codepoint);

    // TODO: handle if cursor is not at end. 
    pop_u8(&buffer->bytes);
    for (size_t i = 0; i < size; i++) {
        push_u8(chars[i], &buffer->bytes);
    }
    push_u8(0, &buffer->bytes);
    buffer->cursor_pos += size;
}

void delete_nchars(size_t n, StringBuffer *buffer) {
    if (n >= buffer->bytes.len) {
        buffer->bytes.len = 1;
        buffer->cursor_pos = 0;
        buffer->bytes.data[0] = 0;
    } else {
        buffer->bytes.len -= n;
        buffer->cursor_pos -= n;
        buffer->bytes.data[buffer->bytes.len - 1] = 0;
    }
}

String get_contents(StringBuffer buffer, Allocator* a) {
    const size_t memsize = sizeof(uint8_t) * buffer.bytes.len;
    uint8_t *mem_out = mem_alloc(memsize, a);
    memcpy(mem_out, buffer.bytes.data, memsize);

    return (String) {
        .bytes = mem_out,
        .memsize = memsize,
    };
}
