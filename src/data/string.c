#include <string.h>
#include <stdarg.h>

#include "platform/memory/allocator.h"
#include "data/string.h"
#include "encodings/utf8.h"

String mk_string(const char* str, Allocator* a) {
    // TODO (FEATURE): panic if str == NULL and in debug
    size_t slen = strlen(str);
    String out;
    out.memsize = (slen + 1) * sizeof(uint8_t);
    out.bytes = (uint8_t*)mem_alloc(out.memsize, a);
    memcpy(out.bytes, str, out.memsize);
    return out;
}

String mv_string(const char* str) {
    // TODO (FEATURE): panic if str == NULL and in debug
    size_t slen = strlen(str);
    String out;
    out.memsize = (slen + 1) * sizeof(uint8_t);
    out.bytes = (uint8_t*)str;
    return out;
}

void delete_string(String str, Allocator* a) {
    mem_free(str.bytes, a);
}

String copy_string(const String str, Allocator* a) {
    String out;
    out.memsize = str.memsize;
    out.bytes = (uint8_t*)mem_alloc(out.memsize, a);
    memcpy(out.bytes, str.bytes, str.memsize);
    return out;
}

String string_from_UTF_32(U32Array arr, Allocator* a) {
    // Step1: calcluate byte length (initialize to 1 for NULL-terminator)
    size_t numbytes = 1;
    for (size_t i = 0; i < arr.len; i++) {
        uint32_t codepoint = arr.data[i];
        numbytes += point_size_utf8(codepoint);
    }
    String out;
    out.memsize = numbytes * sizeof(uint8_t);
    out.bytes = mem_alloc(out.memsize, a);
    for (size_t i = 0; i < arr.len; ) {
        uint8_t nbytes;
        uint32_t codepoint = arr.data[i];
        encode_point_utf8(out.bytes + i, &nbytes, codepoint);
        i += nbytes;
    }
    out.bytes[out.memsize - 1] = 0;
    return out;
}

String string_from_ASCII(U8Array arr, Allocator* a) {
    // Step1: calcluate byte length (initialize to 1 for NULL-terminator)
    size_t numbytes = 1 + arr.len;
    String out;
    out.memsize = numbytes * sizeof(uint8_t);
    out.bytes = mem_alloc(out.memsize, a);

    memcpy(out.bytes, arr.data, out.memsize - sizeof(uint8_t));
    out.bytes[out.memsize - 1] = 0;
    return out;
}

int string_cmp(const String lhs, const String rhs) {
    // TODO: this assumed char == uint8_t. Implement thine own
    //       method, instead of relying on strcmp!
    return strcmp((char*)lhs.bytes, (char*)rhs.bytes);
}

String string_cat(const String lhs, const String rhs, Allocator* a) {
    String out;
    out.memsize = lhs.memsize + (rhs.memsize - 1);
    out.bytes = (uint8_t*)mem_alloc(out.memsize, a);
    memcpy(out.bytes, lhs.bytes, lhs.memsize);
    memcpy(out.bytes + (lhs.memsize - 1), rhs.bytes, rhs.memsize);
    out.bytes[out.memsize - 1] = '\0';
    return out;
}

String string_ncat(Allocator* a, size_t n, ...) {
    String out = (String) {.memsize = 0};
    va_list args;
    va_start(args, n);
    for (size_t i = 0; i < n; i++) {
        String sn = va_arg(args, String);
        out.memsize += sn.memsize - 1;
    }
    out.memsize += 1;
    va_end(args);

    out.bytes = (uint8_t*)mem_alloc(out.memsize, a);
    out.bytes[out.memsize - 1] = '\0';

    ptrdiff_t index = 0;
    va_start(args, n);
    for (size_t i = 0; i < n; i++) {
        String sn = va_arg(args, String);
        memcpy(out.bytes + index, sn.bytes, sn.memsize);
        index += sn.memsize - 1;
    }
    va_end(args);
    return out;
}
