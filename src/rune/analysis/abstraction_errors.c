#include "pico/abstraction/helpers.h"
#include "rune/analysis/abstraction.h"
#include "rune/analysis/abstraction_errors.h"

_Noreturn void record_bad_fdesc_type(RawTree raw, PiErrorPoint* point, RegionAllocator* a) {
    Allocator ra = ra_to_gpa(a);
    PicoError err = {
        .range = raw.range,
        .message = mv_cstr_doc("Invalid Field Descriptor. Field descriptors have the format [.fieldname value]", &ra),
    };
    throw_pi_error(point, err);
}

_Noreturn void record_bad_fdesc_len(RawTree raw, PiErrorPoint* point, RegionAllocator* a) {
    Allocator ra = ra_to_gpa(a);
    PicoError err = {
        .range = raw.range,
        .message = mv_cstr_doc("Invaild field descriptor (contains too many elements). Field descriptos have the format\n"
                               "[.fieldname value].", &ra),
    };
    throw_pi_error(point, err);
}

_Noreturn void record_bad_fdesc_fieldname(RawTree raw, PiErrorPoint* point, RegionAllocator* a) {
    Allocator ra = ra_to_gpa(a);
    PicoError err = {
        .range = raw.branch.nodes.data[0].range,
        .message = mv_cstr_doc(
                               "Structure has malformed fieldname, fieldnames are "
                               "symbols and must therefore use symbol rules.\n"
                               "Symbol rules: must start with a letter, and not contain spaces "
                               "or special characters, i.e. any paren/bracket '{([])}', dots, \n"
                               "colons or semicolons.", &ra),
    };
    throw_pi_error(point, err);
}
