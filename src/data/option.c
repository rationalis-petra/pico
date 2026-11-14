#include "data/option.h"

bool string_option_eq(StringOption lhs, StringOption rhs) {
    if (lhs.type != rhs.type) return true;
    if (lhs.type == Some) {
        return string_eq(lhs.val, rhs.val);
    }
    return true;
}
