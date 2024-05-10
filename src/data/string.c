#include <string.h>
#include <stdio.h>

#include "memory/allocator.h"
#include "data/string.h"
#include "encodings/utf8.h"

string mk_string(const char* str, allocator a) {
    size_t slen = strlen(str);
    string out;
    out.memsize = (slen + 1) * sizeof(uint8_t);
    out.bytes = (uint8_t*)mem_alloc(out.memsize, a);
    memcpy(out.bytes, str, out.memsize);
    return out;
}

string mv_string(const char* str) {
    size_t slen = strlen(str);
    string out;
    out.memsize = (slen + 1) * sizeof(uint8_t);
    out.bytes = (uint8_t*)str;
    return out;
}

void delete_string(string str, allocator a) {
    mem_free(str.bytes, a);
}

string copy_string(const string str, allocator a) {
    string out;
    out.memsize = str.memsize;
    out.bytes = (uint8_t*)mem_alloc(out.memsize, a);
    memcpy(out.bytes, str.bytes, str.memsize);
    return out;
}

string string_from_UTF_32(u32_array arr, allocator a) {
    // Step1: calcluate byte length (initialize to 1 for NULL-terminator)
    size_t numbytes = 1;
    for (size_t i = 0; i < arr.len; i++) {
        uint32_t codepoint = aref_u32(i, arr);
        numbytes += point_size_utf8(codepoint);
    }
    string out;
    out.memsize = numbytes * sizeof(uint8_t);
    out.bytes = mem_alloc(out.memsize, a);
    for (size_t i = 0; i < arr.len; ) {
        uint8_t nbytes;
        uint32_t codepoint = aref_u32(i, arr);
        encode_point_utf8(out.bytes + i, &nbytes, codepoint);
        i += nbytes;
    }
    out.bytes[out.memsize - 1] = 0;
    return out;
}

int string_cmp(const string lhs, const string rhs) {
    return strcmp((char*)lhs.bytes, (char*)rhs.bytes);
}

string string_cat(const string lhs, const string rhs, allocator a) {
    string out;
    out.memsize = lhs.memsize + (rhs.memsize - 1);
    out.bytes = (uint8_t*)mem_alloc(out.memsize, a);
    memcpy(out.bytes, lhs.bytes, lhs.memsize);
    memcpy(out.bytes + (lhs.memsize - 1), rhs.bytes, rhs.memsize);
    return out;
}
