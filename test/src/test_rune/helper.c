#include <stdarg.h>
#include <string.h>

#include "platform/memory/region.h"
#include "platform/error.h"

#include "data/stream.h"

#include "components/pretty/stream_printer.h"
#include "components/pretty/string_printer.h"

#include "pico/parse/parse.h"

#include "rune/analysis/abstraction.h"
#include "rune/eval/eval.h"

#include "test/test_log.h"
#include "test_rune/helper.h"

static void log_rune_eval_error(TestLog* log, Document* doc, Allocator* a) {
    String msg = doc_to_str(doc, 120, a);
    test_log_error(log, msg);
    test_fail(log);
}

static void report_rune_mismatch(TestLog* log, ValRef actual, ValRef expected, RuneEnv* env, Allocator* a) {
    FormattedOStream* os = get_fstream(log);
    PtrArray nodes = mk_ptr_array(4, a);
    push_ptr(mv_cstr_doc("Expected:", a), &nodes);
    push_ptr(pretty_rune_value(expected, env, a), &nodes);
    push_ptr(mv_cstr_doc("but got:", a), &nodes);
    push_ptr(pretty_rune_value(actual, env, a), &nodes);
    write_doc_formatted(mv_sep_doc(nodes, a), 120, os);
    test_fail(log);
}

void test_rune_toplevel_eq(const char *string, ValRef expected, TestContext context) {
    RegionAllocator* subregion = make_subregion(context.region);
    Allocator ra = ra_to_gpa(subregion);
    PiAllocator pia = convert_to_pallocator(&ra);
    IStream* sin = mk_string_istream(mv_string(string), &ra);
    IStream* volatile cin = mk_capturing_istream(sin, &ra);

    PiErrorPoint point;
    if (catch_error(point)) {
        test_log_error(context.log, mv_string("Rune error while evaluating test."));
        display_error(point.multi, *get_captured_buffer(cin), get_fstream(context.log), mv_string("test-suite"), &ra);
        test_fail(context.log);
        delete_istream(cin, &ra);
        release_subregion(subregion);
        return;
    }

    ParseResult parse_res = parse_rune_rawtree(cin, &pia, &ra);
    if (parse_res.type == ParseNone) {
        test_log_error(context.log, mv_string("Rune parse returned none."));
        test_fail(context.log);
        delete_istream(cin, &ra);
        release_subregion(subregion);
        return;
    }
    if (parse_res.type == ParseFail) {
        MultiError multi = (MultiError) { .has_many = false, .error = parse_res.error };
        display_error(multi, *get_captured_buffer(cin), get_fstream(context.log), mv_string("rune-test"), &ra);
        test_fail(context.log);
        delete_istream(cin, &ra);
        release_subregion(subregion);
        return;
    }

    RuneEnv* env = context.env ? context.env : mk_rune_env(&ra);
    Pools pools = get_pools(env);
    HostCallbackData host_data = {0};
    ExprRef expr = abstract_rune_expr(parse_res.result, host_data, pools.expr, subregion, &point);
    RuneEvalResult result = eval_rune(expr, env, subregion);

    if (result.type == AError) {
        log_rune_eval_error(context.log, result.error_message, &ra);
        delete_istream(cin, &ra);
        if (context.env == NULL) {
            delete_rune_env(env);
        }
        release_subregion(subregion);
        return;
    }

    if (!rune_value_eql(result.value, expected, pools.value, &ra)) {
        report_rune_mismatch(context.log, result.value, expected, env, &ra);
        delete_istream(cin, &ra);
        if (context.env == NULL) {
            delete_rune_env(env);
        }
        release_subregion(subregion);
        return;
    }

    test_pass(context.log);
    delete_istream(cin, &ra);
    if (context.env == NULL) {
        delete_rune_env(env);
    }
    release_subregion(subregion);
}

void assert_rune_toplevel_eq(const char *string, ValRef expected, TestContext context) {
    RegionAllocator* subregion = make_subregion(context.region);
    Allocator ra = ra_to_gpa(subregion);
    PiAllocator pia = convert_to_pallocator(&ra);
    IStream* sin = mk_string_istream(mv_string(string), &ra);

    PiErrorPoint point;
    if (catch_error(point)) {
        test_log_error(context.log, mv_string("Rune error while evaluating test."));
        test_fail(context.log);
        delete_istream(sin, &ra);
        release_subregion(subregion);
        return;
    }

    ParseResult parse_res = parse_rune_rawtree(sin, &pia, &ra);
    if (parse_res.type == ParseNone) {
        test_log_error(context.log, mv_string("Rune parse returned none."));
        test_fail(context.log);
        delete_istream(sin, &ra);
        release_subregion(subregion);
        return;
    }
    if (parse_res.type == ParseFail) {
        MultiError multi = (MultiError) { .has_many = false, .error = parse_res.error };
        display_error(multi, *get_captured_buffer(sin), get_fstream(context.log), mv_string("rune-test"), &ra);
        test_fail(context.log);
        delete_istream(sin, &ra);
        release_subregion(subregion);
        return;
    }

    RuneEnv* env = mk_rune_env(&ra);
    Pools pools = get_pools(env);
    HostCallbackData host_data = {0};
    ExprRef expr = abstract_rune_expr(parse_res.result, host_data, pools.expr, subregion, &point);
    RuneEvalResult result = eval_rune(expr, env, subregion);

    if (result.type == AError) {
        log_rune_eval_error(context.log, result.error_message, &ra);
        delete_istream(sin, &ra);
        delete_rune_env(env);
        release_subregion(subregion);
        return;
    }

    if (!rune_value_eql(result.value, expected, pools.value, &ra)) {
        report_rune_mismatch(context.log, result.value, expected, env, &ra);
        delete_istream(sin, &ra);
        delete_rune_env(env);
        release_subregion(subregion);
        return;
    }

    delete_istream(sin, &ra);
    delete_rune_env(env);
    release_subregion(subregion);
}

typedef struct {
    size_t len;
    void* data;
} List;

static size_t align_to(size_t size, size_t align) {
    size_t rem = size % align;
    size_t pad = rem == 0 ? 0 : align - rem;
    return size + pad;
}

static bool val_eq(Bridge bridge, void* data, void* expected) {
    switch (bridge.type) {
    case BPassthrough:
        return (*(ValRef*)data).ref == (*(ValRef*)expected).ref;
    case BPtrWrap:
        return val_eq(*bridge.wrapped, *(void**)data, *(void**)expected);
    case BData: {
        // Check tags for equality
        if ((*(uint64_t*)data) != (*(uint64_t*)expected)) return false;
        uint64_t tag = *(uint64_t*)data;
        if (tag >= bridge.data.len) return false;
        BridgeVariant variant = bridge.data.variants[tag];
        size_t offset = sizeof(uint64_t);
        for (size_t i = 0; i < variant.len; i++) {
            offset = align_to(offset, bridge_host_align(variant.types[i]));
            if (!val_eq(variant.types[i], data + offset, expected + offset)) return false;
            offset += bridge_host_size(variant.types[i]);
        }
        return true;
    }
    case BRecord: {
        size_t offset = 0;
        for (size_t i = 0; i < bridge.record.len; i++) {
            Bridge field = bridge.record.types[i].val;
            offset = align_to(offset, bridge_host_align(field));
            if (!val_eq(field, data + offset, expected + offset)) return false;
            offset += bridge_host_size(field);
        }
        return true;
    }
    case BInt:
        return (*(int64_t*)data) == (*(int64_t*)expected);
    case BString: {
        return string_eq(*(String*)data, *(String*)expected);
    }
    case BList: {
        List* lhs = data;
        List* rhs = expected;
        if (lhs->len != rhs->len) return false;
        size_t elt_size = bridge_host_size(*bridge.element);
        for (size_t i = 0; i < lhs->len; i++) {
            if (!val_eq(*bridge.element, lhs->data + (i * elt_size), rhs->data + (i * elt_size))) {
                return false;
            }
        }
        return true;
    }
    }
    return false;
}

void test_rune_bridge(const char *string, Bridge bridge, void* expected, size_t expected_sz, TestContext context) {
    RegionAllocator* subregion = make_subregion(context.region);
    Allocator ra = ra_to_gpa(subregion);
    PiAllocator pia = convert_to_pallocator(&ra);
    IStream* sin = mk_string_istream(mv_string(string), &ra);
    IStream* volatile cin = mk_capturing_istream(sin, &ra);

    PiErrorPoint point;
    if (catch_error(point)) {
        test_log_error(context.log, mv_string("Rune error while evaluating test."));
        display_error(point.multi, *get_captured_buffer(cin), get_fstream(context.log), mv_string("test-suite"), &ra);
        test_fail(context.log);
        delete_istream(cin, &ra);
        release_subregion(subregion);
        return;
    }

    ParseResult parse_res = parse_rune_rawtree(cin, &pia, &ra);
    if (parse_res.type == ParseNone) {
        test_log_error(context.log, mv_string("Rune parse returned none."));
        test_fail(context.log);
        delete_istream(cin, &ra);
        release_subregion(subregion);
        return;
    }
    if (parse_res.type == ParseFail) {
        MultiError multi = (MultiError) { .has_many = false, .error = parse_res.error };
        display_error(multi, *get_captured_buffer(cin), get_fstream(context.log), mv_string("rune-test"), &ra);
        test_fail(context.log);
        delete_istream(cin, &ra);
        release_subregion(subregion);
        return;
    }

    RuneEnv* env = context.env ? context.env : mk_rune_env(&ra);
    Pools pools = get_pools(env);
    HostCallbackData host_data = {0};
    ExprRef expr = abstract_rune_expr(parse_res.result, host_data, pools.expr, subregion, &point);
    RuneEvalResult result = eval_rune(expr, env, subregion);

    if (result.type == AError) {
        log_rune_eval_error(context.log, result.error_message, &ra);
        delete_istream(cin, &ra);
        if (context.env == NULL) {
            delete_rune_env(env);
        }
        release_subregion(subregion);
        return;
    }

    size_t data_sz = bridge_host_size(bridge);
    void* data = region_alloc(expected_sz, subregion);
    if (data_sz != expected_sz) {
        test_log_error(context.log, mv_string("Bridge gave incorrect size"));
        test_fail(context.log);
        delete_istream(cin, &ra);
        if (context.env == NULL) {
            delete_rune_env(env);
        }
        release_subregion(subregion);
        return;
    }

    if (!get_as_host_val(result.value, bridge, data, &ra)) {
        test_log_error(context.log, mv_string("Failed to get host val"));
        test_fail(context.log);
        delete_istream(cin, &ra);
        if (context.env == NULL) {
            delete_rune_env(env);
        }
        release_subregion(subregion);
        return;
    }

    if (!val_eq(bridge, data, expected)) {
        test_log_error(context.log, mv_string("Incorrect val returned by bridge"));
        test_fail(context.log);
        delete_istream(cin, &ra);
        if (context.env == NULL) {
            delete_rune_env(env);
        }
        release_subregion(subregion);
        return;
    }
     

    test_pass(context.log);
    delete_istream(cin, &ra);
    if (context.env == NULL) {
        delete_rune_env(env);
    }
    release_subregion(subregion);
}
