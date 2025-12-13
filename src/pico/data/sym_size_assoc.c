#include "pico/data/sym_size_assoc.h"
#include "data/meta/assoc_impl.h"

ASSOC_CMP_IMPL(Symbol, size_t, symbol_cmp, sym_size, SymSize)
