#include <string.h>
#include <stdarg.h>

#include "platform/memory/allocator.h"

#include "data/string.h"
#include "components/encodings/utf8.h"

String mk_string(const char* str, Allocator* a) {
    // TODO (FEATURE): panic if str == NULL and in debug
    size_t slen = strlen(str);
    String out;
    out.memsize = slen * sizeof(uint8_t);
    out.bytes = (uint8_t*)mem_alloc(out.memsize, a);
    memcpy(out.bytes, str, out.memsize);
    return out;
}

String mv_string(const char* str) {
    // TODO (FEATURE): panic if str == NULL and in debug
    size_t slen = strlen(str);
    String out;
    out.memsize = slen * sizeof(uint8_t);
    out.bytes = (uint8_t*)str;
    return out;
}

String string_from_codepoint(uint32_t codepoint, Allocator *a) {
    U32Array arr = mk_u32_array(1, a);
    push_u32(codepoint, &arr);
    String out = string_from_UTF_32(arr, a);
    sdelete_u32_array(arr);
    return out;
}

char* to_c_string(String str, Allocator* a) {
    char* out = mem_alloc(str.memsize + 1, a);
    memcpy(out, str.bytes, str.memsize);
    out[str.memsize] = '\0';
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
    // Step1: calcluate byte length
    size_t numbytes = 0;
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
    return out;
}

String string_from_ASCII(U8Array arr, Allocator* a) {
    // Step1: calcluate byte length (initialize to 1 for NULL-terminator)
    size_t numbytes = arr.len;
    String out;
    out.memsize = numbytes * sizeof(uint8_t);
    out.bytes = mem_alloc(out.memsize, a);

    memcpy(out.bytes, arr.data, out.memsize - sizeof(uint8_t));
    out.bytes[out.memsize] = 0;
    return out;
}

bool string_eq(const String lhs, const String rhs) {
    return string_cmp(lhs, rhs) == 0;
}
int string_cmp(const String lhs, const String rhs) {
    // TODO: use vector instructions to make this more efficient?
    size_t min_memsize = (lhs.memsize < rhs.memsize)
        ? lhs.memsize
        : rhs.memsize;
    for (size_t i = 0; i < min_memsize; i++) {
        int diff = lhs.bytes[i] - rhs.bytes[i];
        if (diff != 0) return diff;
    }
    if (lhs.memsize < rhs.memsize) return -1;
    if (lhs.memsize > rhs.memsize) return 1;
    return 0;
}

String string_cat(const String lhs, const String rhs, Allocator* a) {
    String out;
    out.memsize = lhs.memsize + rhs.memsize;
    out.bytes = (uint8_t*)mem_alloc(out.memsize, a);
    memcpy(out.bytes, lhs.bytes, lhs.memsize);
    memcpy(out.bytes + lhs.memsize, rhs.bytes, rhs.memsize);
    return out;
}

String string_ncat(Allocator* a, size_t n, ...) {
    String out = (String) {.memsize = 0};
    va_list args;
    va_start(args, n);
    for (size_t i = 0; i < n; i++) {
        String sn = va_arg(args, String);
        out.memsize += sn.memsize;
    }
    va_end(args);

    out.bytes = (uint8_t*)mem_alloc(out.memsize, a);

    ptrdiff_t index = 0;
    va_start(args, n);
    for (size_t i = 0; i < n; i++) {
        String sn = va_arg(args, String);
        memcpy(out.bytes + index, sn.bytes, sn.memsize);
        index += sn.memsize;
    }
    va_end(args);
    return out;
}

String substring(size_t start, size_t end, const String source, Allocator *a) {
#ifdef VALIDATE_INPUTS
    if (start > end) {
        panic(mv_string("substring: start > end"));
    }
    if (end > source.memsize) {
        panic(mv_string("substring: end > source.memsize"));
    }
#endif
    String out = (String) {.memsize = (end - start)};
    out.bytes = mem_alloc(out.memsize, a);
    memcpy(out.bytes, source.bytes + start, out.memsize);
    return out;
}

bool begins_with(const String source, const String substr) {
    if (substr.memsize == 0) return true;
    if (substr.memsize > source.memsize) return false;

    for (size_t i = 0; i < substr.memsize; i++) {
        if (source.bytes[i] != substr.bytes[i]) return false;
    }
    return true;
}


bool is_substring(const String source, const String substr) {
    // Degenerate cases: the empty string is a subset of all strings; and 
    //   if the substring is larger than the source, then it cannot be a substring 
    if (substr.memsize == 0) return true;
    if (substr.memsize > source.memsize) return false;

    for (size_t i = 0; i < source.memsize - substr.memsize; i++) {
        size_t j = 0;
        while (source.bytes[i + j] == substr.bytes[j]) {
            j++;
            if (j + 1 == substr.memsize) return true;
        }
    }
    return false;
}
