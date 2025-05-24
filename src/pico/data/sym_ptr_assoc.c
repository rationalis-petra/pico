#include "pico/data/sym_ptr_assoc.h"
#include "data/meta/assoc_impl.h"

ASSOC_CMP_IMPL(Symbol, void*, cmp_symbol, sym_ptr, SymPtr)
