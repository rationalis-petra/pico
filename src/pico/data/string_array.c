#include "pico/data/string_array.h"

#include "data/meta/array_impl.h"

ARRAY_CMP_IMPL(String, string_cmp, string, String);

bool string_array_eq(StringArray lhs, StringArray rhs) {
    if (lhs.len != rhs.len) return false;
    for (size_t i = 0; i < lhs.len; i++) {
        if (!string_eq(lhs.data[i], rhs.data[i])) return false;
    }
    return true;
}
