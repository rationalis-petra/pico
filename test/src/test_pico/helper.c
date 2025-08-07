#include <stdarg.h>

#include "platform/memory/arena.h"
#include "platform/memory/executable.h"
#include "platform/memory/std_allocator.h"
#include "platform/jump.h"
#include "platform/error.h"

#include "data/stream.h"

#include "components/pretty/stream_printer.h"

#include "pico/parse/parse.h"
#include "pico/stdlib/extra.h"
#include "pico/values/array.h"
#include "pico/binding/environment.h"
#include "pico/analysis/abstraction.h"
#include "pico/analysis/typecheck.h"
#include "pico/codegen/codegen.h"
#include "pico/eval/call.h"

#include "test/test_log.h"

typedef struct {
    void (* on_expr)(PiType* type, void* val, void* data, TestLog* log);
    void (* on_top)(void* data, TestLog* log);
    void (* on_pi_error)(MultiError err, IStream* sin, TestLog* log);
    void (* on_error)(String msg, TestLog* log);
    void (* on_exit)(void* data, TestLog* log);
} Callbacks;

void run_toplevel_internal(const char *string, Module *module, Environment* env, Callbacks callbacks, void* data, TestLog* log, Allocator *a) {
    IStream* sin = mk_string_istream(mv_string(string), a);
    Allocator exalloc = mk_executable_allocator(a);
    Allocator* exec = &exalloc;
    // Note: we need to be aware of the arena and error point, as both are used
    // by code in the 'true' branches of the nonlocal exits, and may be stored
    // in registers, so they cannotbe changed (unless marked volatile).
    Allocator arena = mk_arena_allocator(4096, a);
    IStream* cin = mk_capturing_istream(sin, &arena);

    Target gen_target = {
        .target = mk_assembler(current_cpu_feature_flags(), exec),
        .code_aux = mk_assembler(current_cpu_feature_flags(), exec),
        .data_aux = mem_alloc(sizeof(U8Array), &arena)
    };
    *gen_target.data_aux = mk_u8_array(128, &arena);

    jump_buf exit_point;
    if (set_jump(exit_point)) goto on_exit;
    set_exit_callback(&exit_point);

    ErrorPoint point;
    if (catch_error(point)) goto on_error;

    PiErrorPoint pi_point;
    if (catch_error(pi_point)) goto on_pi_error;

    ParseResult res = parse_rawtree(cin, &arena);
    if (res.type == ParseNone) {
        throw_error(&point, mv_string("Parse Returned None!"));
    }
    if (res.type == ParseFail) {
        throw_pi_error(&pi_point, res.error);
    }
    if (res.type != ParseSuccess) {
        // If parse is invalid, means internal bug, so better exit soon!
        throw_error(&point, mv_string("Parse Returned Invalid Result!\n"));
    }

    // -------------------------------------------------------------------------
    // Resolution
    // -------------------------------------------------------------------------

    TopLevel abs = abstract(res.result, env, &arena, &pi_point);
    type_check(&abs, env, &arena, &pi_point);
    LinkData links = generate_toplevel(abs, env, gen_target, &arena, &point);
    EvalResult evres = pico_run_toplevel(abs, gen_target, links, module, &arena, &point);

    if (evres.type == ERValue) {
        if (callbacks.on_expr) {
            callbacks.on_expr(evres.val.type, evres.val.val, data, log);
        }
        if (evres.val.type->sort == TArray)
            free_array(evres.val.val);
    } else {
        if (callbacks.on_top) {
            callbacks.on_top(data, log);
        }
    }
    // TODO: check evres == expected_val 

    delete_assembler(gen_target.target);
    delete_assembler(gen_target.code_aux);
    release_arena_allocator(arena);
    release_executable_allocator(exalloc);
    delete_istream(sin, a);
    return;

 on_pi_error:
    if (callbacks.on_pi_error) {
        callbacks.on_pi_error(pi_point.multi, cin, log);
    }
    delete_assembler(gen_target.target);
    delete_assembler(gen_target.code_aux);
    release_arena_allocator(arena);
    release_executable_allocator(exalloc);
    delete_istream(sin, a);
    return;

 on_error:
    if (callbacks.on_error) {
        callbacks.on_error(point.error_message, log);
    }
    delete_assembler(gen_target.target);
    delete_assembler(gen_target.code_aux);
    release_arena_allocator(arena);
    release_executable_allocator(exalloc);
    delete_istream(sin, a);
    return;

 on_exit:
    if (callbacks.on_exit) {
        callbacks.on_exit(data, log);
    }
    delete_assembler(gen_target.target);
    delete_assembler(gen_target.code_aux);
    release_arena_allocator(arena);
    release_executable_allocator(exalloc);
    return;
}


void fail_exit(void* data, TestLog* log) {
    test_log_error(log, mv_string("(exit) called unexpectedly during test"));
    test_fail(log);
}

void fail_error(String err, TestLog* log) {
    test_log_error(log, err);
    test_fail(log);
}

void fail_pi_error(MultiError err, IStream* cin, TestLog* log) {
    Allocator arena = mk_arena_allocator(4096, get_std_allocator());
    display_error(err, cin, get_fstream(log), &arena);
    test_log_error(log, mv_string("Test failure - message logged"));
    test_fail(log);
    release_arena_allocator(arena);
}

void expr_eql(PiType* type, void* val, void* data, TestLog* log) {
    Allocator* std = get_std_allocator();
    if (!pi_value_eql(type, val, data, std)) {
        Allocator arena = mk_arena_allocator(4096, std);
        Allocator* a = &arena;
        FormattedOStream* os = get_fstream(log);
        write_fstring(mv_string("Expected: "), os);
        Document* doc = pretty_pi_value(data, type, a);
        write_doc_formatted(doc, 120, os);
        delete_doc(doc, a);
        write_fstring(mv_string("\nGot: "), os);
        doc = pretty_pi_value(val, type, a);
        write_doc_formatted(doc, 120, os);
        delete_doc(doc, a);
        write_fstring(mv_string("\n"), os);
        test_fail(log);
        release_arena_allocator(arena);
    } else {
        test_pass(log);
    }
}

void top_eql(void *data, TestLog *log) {
    FormattedOStream* os = get_fstream(log);
    write_fstring(mv_string("Did not evaluate to a value."), os);
    test_fail(log);
}

void test_toplevel_eq(const char *string, void *expected_val, Module *module, Environment* env, TestLog* log, Allocator *a) {
    Callbacks callbacks = (Callbacks) {
        .on_expr = expr_eql,
        .on_top = top_eql,
        .on_pi_error = fail_pi_error,
        .on_error = fail_error,
        .on_exit = fail_exit,
    };
    run_toplevel_internal(string, module, env, callbacks, expected_val, log, a);
}

typedef struct {
    String expected;
    OStream* stream;
} StdoutData;

void expr_stdout(PiType* type, void* val, void* data, TestLog* log) {
    if (type->sort != TPrim || type->prim != Unit) {
        test_log_error(log, mv_string("stdout tests expecte expressions to have unit type"));
        test_fail(log);
    } else {
        Allocator arena = mk_arena_allocator(4096, get_std_allocator());
        Allocator* a = &arena;
        StdoutData vals = *(StdoutData*)data;
        String actual = *current_string(vals.stream, a);
        if (string_cmp(actual, vals.expected) != 0) {
            FormattedOStream* os = get_fstream(log);
            write_fstring(mv_string("Expected: "), os);
            write_fstring(vals.expected, os);
            write_fstring(mv_string("\nGot: "), os);
            write_fstring(actual, os);
            write_fstring(mv_string("\n"), os);
            test_fail(log);
        } else {
            test_pass(log);
        }
        release_arena_allocator(arena);
    }
}

void top_stdout(void *data, TestLog *log) {
    FormattedOStream* os = get_fstream(log);
    write_fstring(mv_string("Did not evaluate to a value."), os);
    test_fail(log);
}

void test_toplevel_stdout(const char *string, const char *expected_stdout, Module *module, Environment* env, TestLog* log, Allocator *a) {
    Callbacks callbacks = (Callbacks) {
        .on_expr = expr_stdout,
        .on_top = top_stdout,
        .on_pi_error = fail_pi_error,
        .on_error = fail_error,
        .on_exit = fail_exit,
    };
    OStream* out = mk_string_ostream(a);
    StdoutData data = (StdoutData) {
        .stream = out,
        .expected = mv_string(expected_stdout),
    };

    OStream* old = set_std_ostream(out);
    run_toplevel_internal(string, module, env, callbacks, &data, log, a);
    set_std_ostream(old);

    delete_ostream(out, a);
}

void log_exit(void* data, TestLog* log) {
    test_log_error(log, mv_string("Test failure - (exit) called unexpectedly during test"));
}

void log_error(String err, TestLog* log) {
    test_log_error(log, err);
}

void log_pi_error(MultiError err, IStream* cin, TestLog* log) {
    // TODO: improve the test log error to take in a document 
    Allocator arena = mk_arena_allocator(4096, get_std_allocator());
    display_error(err, cin, get_fstream(log), &arena);
    test_log_error(log, mv_string("Test failure - message logged"));
    release_arena_allocator(arena);
}

void run_toplevel(const char *string, Module *module, Environment* env, TestLog* log, Allocator *a) {
    Callbacks callbacks = (Callbacks) {
        .on_pi_error = log_pi_error,
        .on_error = log_error,
        .on_exit = log_exit,
    };
    run_toplevel_internal(string, module, env, callbacks, NULL, log, a);
}

typedef struct {
    void (* on_expr)(PiType* actual, void* data, TestLog* log);
    void (* on_top)(void* data, TestLog* log);
    void (* on_pi_error)(MultiError err, IStream* sin, TestLog* log);
    void (* on_error)(String msg, TestLog* log);
    void (* on_exit)(void* data, TestLog* log);
} TypeCallbacks;

void test_typecheck_internal(const char *string, Environment* env, TypeCallbacks callbacks, void* data, TestLog* log, Allocator* a) {
    IStream* sin = mk_string_istream(mv_string(string), a);
    Allocator exalloc = mk_executable_allocator(a);
    Allocator* exec = &exalloc;
    // Note: we need to be aware of the arena and error point, as both are used
    // by code in the 'true' branches of the nonlocal exits, and may be stored
    // in registers, so they cannotbe changed (unless marked volatile).
    Allocator arena = mk_arena_allocator(4096, a);
    IStream* cin = mk_capturing_istream(sin, &arena);

    Target gen_target = {
        .target = mk_assembler(current_cpu_feature_flags(), exec),
        .code_aux = mk_assembler(current_cpu_feature_flags(), exec),
        .data_aux = mem_alloc(sizeof(U8Array), &arena)
    };
    *gen_target.data_aux = mk_u8_array(128, &arena);

    jump_buf exit_point;
    if (set_jump(exit_point)) goto on_exit;
    set_exit_callback(&exit_point);

    ErrorPoint point;
    if (catch_error(point)) goto on_error;

    PiErrorPoint pi_point;
    if (catch_error(pi_point)) goto on_pi_error;

    ParseResult res = parse_rawtree(cin, &arena);
    if (res.type == ParseNone) {
        throw_error(&point, mv_string("Parse Returned None!"));
    }
    if (res.type == ParseFail) {
        throw_pi_error(&pi_point, res.error);
    }
    if (res.type != ParseSuccess) {
        // If parse is invalid, means internal bug, so better exit soon!
        throw_error(&point, mv_string("Parse Returned Invalid Result!\n"));
    }

    // -------------------------------------------------------------------------
    // Resolution
    // -------------------------------------------------------------------------

    TopLevel abs = abstract(res.result, env, &arena, &pi_point);

    type_check(&abs, env, &arena, &pi_point);

    if (abs.type == TLExpr) {
        if (callbacks.on_expr) {
            callbacks.on_expr(abs.expr->ptype, data, log);
        }
    } else {
        if (callbacks.on_top) {
            callbacks.on_top(data, log);
        }
    }
    // TODO: check evres == expected_val 

    delete_assembler(gen_target.target);
    delete_assembler(gen_target.code_aux);
    release_arena_allocator(arena);
    release_executable_allocator(exalloc);
    delete_istream(sin, a);
    return;

 on_pi_error:
    if (callbacks.on_pi_error) {
        callbacks.on_pi_error(pi_point.multi, cin, log);
    }
    delete_assembler(gen_target.target);
    delete_assembler(gen_target.code_aux);
    release_arena_allocator(arena);
    release_executable_allocator(exalloc);
    delete_istream(sin, a);
    return;

 on_error:
    if (callbacks.on_error) {
        callbacks.on_error(point.error_message, log);
    }
    delete_assembler(gen_target.target);
    delete_assembler(gen_target.code_aux);
    release_arena_allocator(arena);
    release_executable_allocator(exalloc);
    delete_istream(sin, a);
    return;

 on_exit:
    if (callbacks.on_exit) {
        callbacks.on_exit(data, log);
    }
    delete_assembler(gen_target.target);
    delete_assembler(gen_target.code_aux);
    release_arena_allocator(arena);
    release_executable_allocator(exalloc);
    return;
}

void type_eql(PiType* type, void* data, TestLog* log) {
    Allocator* std = get_std_allocator();
    if (!pi_type_eql(type, data, std)) {
        Allocator arena = mk_arena_allocator(4096, std);
        Allocator* a = &arena;
        FormattedOStream* os = get_fstream(log);
        write_fstring(mv_string("Expected: "), os);
        Document* doc = pretty_type(data, a);
        write_doc_formatted(doc, 120, os);
        delete_doc(doc, a);
        write_fstring(mv_string("\nGot: "), os);
        doc = pretty_type(type, a);
        write_doc_formatted(doc, 120, os);
        delete_doc(doc, a);
        write_fstring(mv_string("\n"), os);
        test_fail(log);
        release_arena_allocator(arena);
    } else {
        test_pass(log);
    }
}

void top_type(void *data, TestLog *log) {
    FormattedOStream* os = get_fstream(log);
    write_fstring(mv_string("Typecheck tests only typecheck expressions."), os);
    test_fail(log);
}

void test_typecheck_eq(const char *string, PiType* expected, Environment* env, TestLog* log, Allocator* a) {
    TypeCallbacks callbacks = (TypeCallbacks) {
        .on_expr = type_eql,
        .on_top = top_type,
        .on_pi_error = fail_pi_error,
        .on_error = fail_error,
        .on_exit = fail_exit,
    };
    test_typecheck_internal(string, env, callbacks, expected, log, a);
}
