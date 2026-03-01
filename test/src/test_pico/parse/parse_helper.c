#include <stdarg.h>
#include "platform/memory/std_allocator.h"
#include "platform/memory/region.h"
#include "platform/memory/executable.h"
#include "platform/memory/arena.h"
#include "platform/signals.h"
#include "platform/error.h"
#include "data/stream.h"

#include "components/pretty/string_printer.h"

#include "pico/parse/parse.h"
#include "pico/syntax/concrete.h"
#include "pico/binding/environment.h"

#include "test/test_log.h"
#include "test_pico/helper.h"
#include "test_pico/parse/parse_helper.h"


bool rawtree_eql(RawTree lhs, RawTree rhs) {
    if (lhs.type != rhs.type) return false;

    if (lhs.type == RawAtom) {
        Atom la = lhs.atom; 
        Atom ra = rhs.atom; 
        if (la.type != ra.type) return false;

        switch (la.type) {
        case ABool:
            return la.boolean == ra.boolean;
            break;
        case AIntegral:
            return la.int_64 == ra.int_64;
            break;
        case AFloating:
            return la.float_64 == ra.float_64;
            break;
        case ASymbol:
            return symbol_eq(la.symbol, ra.symbol);
            break;
        case AString:
            return string_cmp(la.string, ra.string) == 0;
            break;
        case ACapture:
            panic(mv_string("Cannot currently"));
            break;
        }
    } else {
        if (lhs.branch.hint != rhs.branch.hint) return false ;
        if (lhs.branch.nodes.len != rhs.branch.nodes.len) return false;
        RawTreePiList ln = lhs.branch.nodes;
        RawTreePiList rn = rhs.branch.nodes;

        for (size_t i = 0; i < ln.len; i++) {
            if (!rawtree_eql(ln.data[i], rn.data[i])) return false;
        }
        return true;
    }
    panic(mv_string("Invalid rawtrees provided to rawtree_eql"));
}

void parse_on_pi_error(MultiError err, IStream* cin, TestLog* log) {
    ArenaAllocator* arena = make_arena_allocator(4096, get_std_allocator());
    Allocator gpa = aa_to_gpa(arena);
    display_error(err, *get_captured_buffer(cin), get_fstream(log), mv_string("test-suite"), &gpa);
    test_log_error(log, mv_string("Test failure - message logged"));
    test_fail(log);
    delete_arena_allocator(arena);
}

void parse_on_error(Document* doc, TestLog* log, Allocator* ra) {
    test_log_error(log, doc_to_str(doc, 120, ra));
    test_fail(log);
}

//void test_typecheck_internal(const char *string, Environment* env, TypeCallbacks callbacks, void* data, TestLog* log, RegionAllocator* region) {
void test_parse_eq_internal(const char *string, Environment *env, RawTree expected, TestLog* log, RegionAllocator* region) {
    Allocator ra = ra_to_gpa(region);
    PiAllocator pico_region = convert_to_pallocator(&ra);
    PiAllocator* pia = &pico_region;

    IStream* sin = mk_string_istream(mv_string(string), &ra);
    Allocator exalloc = mk_executable_allocator(&ra);
    // Note: we need to be aware of the arena and error point, as both are used
    // by code in the 'true' branches of the nonlocal exits, and may be stored
    // in registers, so they cannotbe changed (unless marked volatile).
    IStream* cin = mk_capturing_istream(sin, &ra);

    ErrorPoint point;
    if (catch_error(point)) goto on_error;

    PiErrorPoint pi_point;
    if (catch_error(pi_point)) goto on_pi_error;

    ParseResult res = parse_rawtree(cin, pia, &ra);
    if (res.type == ParseNone) {
        throw_error(&point, mv_cstr_doc("Parse Returned None!", &ra));
    }
    if (res.type == ParseFail) {
        throw_pi_error(&pi_point, res.error);
    }
    if (res.type != ParseSuccess) {
        // If parse is invalid, means internal bug, so better exit soon!
        throw_error(&point, mv_cstr_doc("Parse Returned Invalid Result!\n", &ra));
    }

    if (rawtree_eql(res.result, expected)) {
        test_pass(log);
    } else {
        PtrArray nodes = mk_ptr_array(8, &ra);
        push_ptr(mv_cstr_doc("Parsed Rawtree differed from expected rawtree. Expected:", &ra), &nodes);
        push_ptr(pretty_rawtree(expected, &ra), &nodes);
        push_ptr(mv_cstr_doc("But got:", &ra), &nodes);
        push_ptr(pretty_rawtree(res.result, &ra), &nodes);

        // String doc_to_str(Document* doc, uint16_t width, Allocator* a);
        test_log_error(log, doc_to_str(mv_sep_doc(nodes, &ra), 120, &ra));
        test_fail(log);
    }

    release_executable_allocator(exalloc);
    delete_istream(sin, &ra);
    return;

 on_pi_error:
    parse_on_pi_error(pi_point.multi, cin, log);
    release_executable_allocator(exalloc);
    delete_istream(sin, &ra);
    return;

 on_error:
    parse_on_error(point.error_message, log, &ra);
    release_executable_allocator(exalloc);
    delete_istream(sin, &ra);
    return;
}

void test_parse_eq(const char *string, RawTree expected, TestContext context) {
    RegionAllocator* subregion = make_subregion(context.region);
    test_parse_eq_internal(string, context.env, expected, context.log, subregion);
    release_subregion(subregion);
}


RawTree int_atom(int64_t val) {
  return (RawTree) {
    .type = RawAtom,
    .atom.type = AIntegral,
    .atom.int_64 = val,
  };
}

RawTree bool_atom(bool val) {
  return (RawTree) {
    .type = RawAtom,
    .atom.type = ABool,
    .atom.boolean = val,
  };
}

RawTree symbol_atom(const char* str) {
  return (RawTree) {
    .type = RawAtom,
    .atom.type = ASymbol,
    .atom.symbol = string_to_symbol(mv_string(str)),
  };
}


RawTree mk_branch(PiAllocator *pia, uint64_t len, SyntaxHint hint, va_list args) {
    RawTreePiList nodes = mk_rawtree_list(len, pia);
    for (size_t i = 0; i < len; i++) {
        push_rawtree(va_arg(args, RawTree), &nodes);
    }
    return (RawTree) {
        .type = RawBranch,
        .branch.hint = hint,
        .branch.nodes = nodes,
    };
}

RawTree expr_branch(PiAllocator *a, uint64_t len, ...) {
    va_list args;
    va_start(args, len);
    RawTree out = mk_branch(a, len, HExpression, args);
    va_end(args);
    return out;
}
