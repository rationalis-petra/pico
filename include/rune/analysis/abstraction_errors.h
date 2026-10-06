#ifndef __RUNE_ANALYSIS_ABSTRACTION_ERRORS_H
#define __RUNE_ANALYSIS_ABSTRACTION_ERRORS_H

#include "platform/memory/region.h"

#include "pico/data/error.h"
#include "pico/syntax/concrete.h"
#include "rune/syntax/expression.h"

_Noreturn void record_bad_fdesc_type(RawTree raw, PiErrorPoint* point, RegionAllocator* a);
_Noreturn void record_bad_fdesc_len(RawTree raw, PiErrorPoint* point, RegionAllocator* a);
_Noreturn void record_bad_fdesc_fieldname(RawTree raw, PiErrorPoint* point, RegionAllocator* a);
_Noreturn void record_duplicate_fieldname(RawTree raw, Symbol fname, PiErrorPoint* point, RegionAllocator* a);

#endif
