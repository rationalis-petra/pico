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

// Match
_Noreturn void case_not_enough_terms(RawTree raw, RegionAllocator* a, PiErrorPoint* point);
_Noreturn void case_clause_not_enough_terms(RawTree raw, RegionAllocator* a, PiErrorPoint* point);
_Noreturn void case_pattern_is_atom(RawTree raw, RegionAllocator* a, PiErrorPoint* point);
_Noreturn void case_pattern_head_malformed(RawTree raw, RegionAllocator* a, PiErrorPoint* point);
_Noreturn void case_pattern_no_tagname(RawTree raw, RegionAllocator* a, PiErrorPoint* point);
_Noreturn void case_pattern_bad_binding(RawTree raw, RegionAllocator* a, PiErrorPoint* point);

#endif
