#ifndef __DATA_STRING_H
#define __DATA_STRING_H

#include <stdint.h>
#include "data/array.h"

// A UFT-8 Encoded String
typedef struct String {
    size_t memsize;
    uint8_t* bytes;
} String;

String mk_string(const char* str, Allocator* a);
String mv_string(const char* str);
String string_from_codepoint(uint32_t codepoint, Allocator* a);

void delete_string(String str, Allocator* a);
String copy_string(const String str, Allocator* a);

String string_from_UTF_32(U32Array arr, Allocator* a);
String string_from_ASCII(U8Array arr, Allocator* a);

int string_cmp(const String lhs, const String rhs);
String string_cat(const String lhs, const String rhs, Allocator* a);
String string_ncat(Allocator* a, size_t n, ...);

String substring(size_t start, size_t end, const String source, Allocator* a);

#endif
