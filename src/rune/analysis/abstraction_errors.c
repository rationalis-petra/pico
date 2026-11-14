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

_Noreturn void case_not_enough_terms(RawTree raw, RegionAllocator* region, PiErrorPoint* point) {
    Allocator a = ra_to_gpa(region);
    PicoError err = {
        .range = raw.range,
        .message = mv_cstr_doc("Case expects at least 1 argument.", &a),
    };
    throw_pi_error(point, err);
}

_Noreturn void case_clause_not_enough_terms(RawTree raw, RegionAllocator* region, PiErrorPoint* point) {
    Allocator a = ra_to_gpa(region);
    PicoError err = {
        .range = raw.range,
        .message = mv_cstr_doc("Case clause expects at least 2 arguments: a pattern an a body.", &a),
    };
    throw_pi_error(point, err);
}

_Noreturn void case_pattern_is_atom(RawTree raw_clause, RegionAllocator* region, PiErrorPoint* point) {
    Allocator a = ra_to_gpa(region);
    PicoError err = {
        .range = raw_clause.range,
        .message = mv_cstr_doc("Expecting pattern but got an atom instead. This is not yet supported.", &a),
    };
    throw_pi_error(point, err);
}

_Noreturn void case_pattern_head_malformed(RawTree raw, RegionAllocator* region, PiErrorPoint* point) {
    Allocator a = ra_to_gpa(region);
    PicoError err = {
        .range = raw.range,
        .message = mv_cstr_doc("For patterns not in square brackets, they are expected to be be :<symbol>, e.g. :none", &a),
    };
    throw_pi_error(point, err);
}

_Noreturn void case_pattern_no_tagname(RawTree clause, RegionAllocator* region, PiErrorPoint* point) {
    Allocator a = ra_to_gpa(region);
    RawTree pattern = clause.branch.nodes.data[0];
    PtrArray nodes = mk_ptr_array(2, &a);
    push_ptr(mv_str_doc(mv_string("Unable to get tagname in pattern:"), &a), &nodes);
    push_ptr(pretty_rawtree(pattern.branch.nodes.data[0], &a), &nodes);
    Document* doc = mv_sep_doc(nodes, &a);
    PicoError err = {
        .range = clause.range,
        .message = doc,
    };
    throw_pi_error(point, err);
}

_Noreturn void case_pattern_bad_binding(RawTree raw, RegionAllocator* region, PiErrorPoint* point) {
    Allocator a = ra_to_gpa(region);
    PicoError err = {
        .range = raw.range,
        .message = mv_cstr_doc("Pattern binding was not a symbol!", &a),
    };
    throw_pi_error(point, err);
}
