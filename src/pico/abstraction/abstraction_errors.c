#include "components/pretty/standard_types.h"

#include "pico/abstraction/abstraction_errors.h"


// ------------------------------------------------------------
//   Expression/Value Formers
// ------------------------------------------------------------

_Noreturn void array_incorrect_numterms(RawTree raw, size_t expected, AbstractionICtx ctx) {
    if (raw.branch.nodes.len < expected) {
        PtrArray nodes = mk_ptr_array(4, ctx.gpa);
        push_ptr(mv_cstr_doc("Not enough terms provided to 'array' term former. Correct usage is either", ctx.gpa), &nodes);
        push_ptr(mv_cstr_doc("(array {n m ...} [a b c ...]) or (array [a b c ...])", ctx.gpa), &nodes);
        push_ptr(mv_cstr_doc("where n, m, ... represent the array dimensions, and a, b, c, ... are either values", ctx.gpa), &nodes);
        push_ptr(mv_cstr_doc("in the case of a one-dimensional array, or subarrays in the case of a multidimensional array.", ctx.gpa), &nodes);
        PicoError err = {
            .range = raw.range,
            .message = mv_sep_doc(nodes, ctx.gpa),
        };
        throw_pi_error(ctx.point, err);
    } else {

        // More values than expected
        Range extra_values = {
            .start = raw.branch.nodes.data[expected + 1].range.start,
            .end = raw.branch.nodes.data[raw.branch.nodes.len - 1].range.end,
        };
        PtrArray nodes = mk_ptr_array(4, ctx.gpa);
        push_ptr(mv_cstr_doc("Too many  terms provided to 'array' term former. Correct usage is either", ctx.gpa), &nodes);
        push_ptr(mv_cstr_doc("(array {n m ...} [a b c ...]) or (array [a b c ...])", ctx.gpa), &nodes);
        push_ptr(mv_cstr_doc("where n, m, ... represent the array dimensions, and a, b, c, ... are either values", ctx.gpa), &nodes);
        push_ptr(mv_cstr_doc("in the case of a one-dimensional array, or subarrays in the case of a multidimensional array.", ctx.gpa), &nodes);
        PicoError err = {
            .range = extra_values,
            .message = mv_sep_doc(nodes, ctx.gpa),
        };
        throw_pi_error(ctx.point, err);
    }
}

_Noreturn void array_incorrect_dimtype(RawTree raw, AbstractionICtx ctx) {
    PtrArray nodes = mk_ptr_array(2, ctx.gpa);
    push_ptr(mv_cstr_doc("When providing array dimensions, all values shold be numeric constants, for example:", ctx.gpa), &nodes);
    push_ptr(mv_cstr_doc("(array {2 3} [[1 2 3] [4 5 6]])", ctx.gpa), &nodes);
    PicoError err = {
        .range = raw.range,
        .message = mv_sep_doc(nodes, ctx.gpa),
    };
    throw_pi_error(ctx.point, err);
}

_Noreturn void array_incorrect_format(RawTree raw, AbstractionICtx ctx) {
    PtrArray nodes = mk_ptr_array(3, ctx.gpa);
    push_ptr(mv_cstr_doc("When providing arrays, both arrays and subarrays should be lists of values enclosed with", ctx.gpa), &nodes);
    push_ptr(mv_cstr_doc("square brackets. An array or subarray was expected, but the value here does not have the", ctx.gpa), &nodes);
    push_ptr(mv_cstr_doc("correct formatting.", ctx.gpa), &nodes);
    PicoError err = {
        .range = raw.range,
        .message = mv_hsep_doc(nodes, ctx.gpa),
    };
    throw_pi_error(ctx.point, err);
}

_Noreturn void array_incorrect_size(RawTree raw, uint64_t expected, AbstractionICtx ctx) {
    PtrArray nodes = mk_ptr_array(4, ctx.gpa);
    push_ptr(mv_cstr_doc("The expected length for this array or subarray is", ctx.gpa), &nodes);
    push_ptr(pretty_u64(expected, ctx.gpa), &nodes);
    push_ptr(mv_cstr_doc(", but, the actual length was ", ctx.gpa), &nodes);
    push_ptr(pretty_u64(raw.branch.nodes.len, ctx.gpa), &nodes);
    PicoError err = {
        .range = raw.range,
        .message = mv_hsep_doc(nodes, ctx.gpa),
    };
    throw_pi_error(ctx.point, err);
}

_Noreturn void array_elt_incorrect_numterms(RawTree raw, AbstractionICtx ctx) {
    Document* message;
    if (raw.branch.nodes.len > 3) {
        message = mv_cstr_doc("aelt type has received too many terms - expect to receive only an index and an array.", ctx.gpa);
    } else if (raw.branch.nodes.len == 2) {
        message = mv_cstr_doc("aelt is missing an array.", ctx.gpa);
    } else {
        message = mv_cstr_doc("aelt is missing both an index and an array.", ctx.gpa);
    }
    
    PicoError err = {
        .range = raw.range,
        .message = message,
    };
    throw_pi_error(ctx.point, err);
}


// Structure
// ----------
_Noreturn void struct_bad_fdesc_type(RawTree fdesc, AbstractionICtx ctx) {
    PicoError err = {
        .range = fdesc.range,
        .message = mv_cstr_doc("Invalid Field Descriptor. Field descriptors have the format [.fieldname value]", ctx.gpa),
    };
    throw_pi_error(ctx.point, err);
}

_Noreturn void struct_bad_fdesc_len(RawTree fdesc, AbstractionICtx ctx) {
    // TODO: detect the specific error '. fieldname value' and point out the issue.
    PicoError err = {
        .range = fdesc.range,
        .message = mv_cstr_doc("Invaild field descriptor (contains too many elements). Field descriptos have the format\n"
                               "[.fieldname value].", ctx.gpa),
    };
    throw_pi_error(ctx.point, err);
}

_Noreturn void struct_bad_fdesc_fieldname(RawTree fdesc, AbstractionICtx ctx) {
  PicoError err = {
    .range = fdesc.branch.nodes.data[0].range,
    .message = mv_cstr_doc(
        "Structure has malformed fieldname, fieldnames are "
        "symbols and must therefore use symbol rules.\n"
        "Symbol rules: must start with a letter, and not contain spaces "
        "or special characters, i.e. any paren/bracket '{([])}', dots, \n"
        "colons or semicolons.", ctx.gpa),
    };
    throw_pi_error(ctx.point, err);
}

_Noreturn void struct_duplicate_fieldname(RawTree fdesc, Symbol fname, AbstractionICtx ctx) {
    PicoError err = {
        .range = fdesc.range,
        .message = mv_cstr_doc("Duplicate field in structure.", ctx.gpa),
    };
    throw_pi_error(ctx.point, err);
}

// ------------------------------------------------------------
//   Type Formers
// ------------------------------------------------------------
_Noreturn void proc_tyformer_incorrect_numterms(RawTree raw, AbstractionICtx ctx) {
    Document* message;
    if (raw.branch.nodes.len > 3) {
        message = mv_cstr_doc("Proc type has received too many terms - expect to receive only an argument list and return type.", ctx.gpa);
    } else if (raw.branch.nodes.len == 2) {
        message = mv_cstr_doc("Proc type missing return argument.", ctx.gpa);
    } else {
        message = mv_cstr_doc("Proc type missing both argument list and return argument.", ctx.gpa);
    }
    
    PicoError err = {
        .range = raw.range,
        .message = message,
    };
    throw_pi_error(ctx.point, err);
}

_Noreturn void array_tyformer_incorrect_numterms(RawTree raw, AbstractionICtx ctx) {
    size_t expected = 3;
    if (raw.branch.nodes.len < expected) {
        PtrArray nodes = mk_ptr_array(4, ctx.gpa);
        push_ptr(mv_cstr_doc("Not enough terms provided to 'Array' type former. Correct usage is", ctx.gpa), &nodes);
        push_ptr(mv_cstr_doc("(Array [n m ...] T), where n, m, ... represent the array dimensions, and T", ctx.gpa), &nodes);
        push_ptr(mv_cstr_doc("is the element type.", ctx.gpa), &nodes);
        PicoError err = {
            .range = raw.range,
            .message = mv_sep_doc(nodes, ctx.gpa),
        };
        throw_pi_error(ctx.point, err);
    } else {

        // More values than expected
        Range extra_values = {
            .start = raw.branch.nodes.data[expected + 1].range.start,
            .end = raw.branch.nodes.data[raw.branch.nodes.len - 1].range.end,
        };
        PtrArray nodes = mk_ptr_array(4, ctx.gpa);
        push_ptr(mv_cstr_doc("Not enough terms provided to 'Array' type former. Correct usage is", ctx.gpa), &nodes);
        push_ptr(mv_cstr_doc("(Array [n m ...] T), where n, m, ... represent the array dimensions, and T", ctx.gpa), &nodes);
        push_ptr(mv_cstr_doc("is the element type.", ctx.gpa), &nodes);
        PicoError err = {
            .range = extra_values,
            .message = mv_sep_doc(nodes, ctx.gpa),
        };
        throw_pi_error(ctx.point, err);
    }
}

_Noreturn void array_tyformer_incorrect_dimformat(RawTree raw, AbstractionICtx ctx) {
    PtrArray nodes = mk_ptr_array(4, ctx.gpa);
    push_ptr(mv_cstr_doc("The 'dimension' argument to the 'Array' type former is malformed. Correct usage is", ctx.gpa), &nodes);
    push_ptr(mv_cstr_doc("(Array [n m ...] T), where n, m, ... represent the array dimensions, and T", ctx.gpa), &nodes);
    push_ptr(mv_cstr_doc("is the element type.", ctx.gpa), &nodes);
    PicoError err = {
        .range = raw.range,
        .message = mv_sep_doc(nodes, ctx.gpa),
    };
    throw_pi_error(ctx.point, err);
}

_Noreturn void array_tyformer_dim_not_number(RawTree raw, AbstractionICtx ctx) {
    PicoError err = {
        .range = raw.range,
        .message = mv_cstr_doc("The dimensions of an Array type should all be numeric literals.", ctx.gpa),
    };
    throw_pi_error(ctx.point, err);
}
