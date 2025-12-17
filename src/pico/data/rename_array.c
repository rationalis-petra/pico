#include "pico/data/rename_array.h"

#include "data/meta/array_impl.h"

int64_t cmp_rename(Rename n1, Rename n2) {
    int64_t r1 = symbol_cmp(n1.l_name, n2.l_name);
    if (r1 != 0) return r1;
    return symbol_cmp(n1.r_name, n2.r_name);
}

ARRAY_CMP_IMPL(Rename, cmp_rename, rename, Rename)
