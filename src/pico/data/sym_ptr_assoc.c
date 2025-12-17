#include "pico/data/sym_ptr_assoc.h"
#include "data/meta/assoc_impl.h"

ASSOC_CMP_IMPL(Symbol, void*, symbol_cmp, sym_ptr, SymPtr)
