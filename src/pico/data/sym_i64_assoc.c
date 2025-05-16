#include "pico/data/sym_i64_assoc.h"
#include "data/meta/assoc_impl.h"

ASSOC_CMP_IMPL(Symbol, int64_t, cmp_symbol, sym_i64, SymI64)
