#ifndef __DATA_STRING_H
#define __DATA_STRING_H

#include <stdint.h>
#include "data/array.h"

// A UFT-8 Encoded String
typedef struct string {
    size_t memsize;
    uint8_t* bytes;
} string;

string mk_string(const char* str, allocator a);
string mv_string(const char* str);
void delete_string(string str, allocator a);
string copy_string(const string str, allocator a);

string string_from_UTF_32(u32_array arr, allocator a);

int string_cmp(const string lhs, const string rhs);
string string_cat(const string lhs, const string rhs, allocator a);

#endif
