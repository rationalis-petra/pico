#include <stdarg.h>

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
