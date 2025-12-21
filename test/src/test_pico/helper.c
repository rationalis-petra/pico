#include <stdarg.h>
#include <string.h>

#include "platform/memory/arena.h"
#include "platform/memory/region.h"
#include "platform/memory/executable.h"
#include "platform/memory/std_allocator.h"
#include "platform/jump.h"
#include "platform/error.h"

#include "data/stream.h"
#include "data/stringify.h"

#include "components/pretty/stream_printer.h"
#include "components/pretty/string_printer.h"

#include "pico/parse/parse.h"
#include "pico/stdlib/extra.h"
#include "pico/stdlib/platform/submodules.h"
#include "pico/values/array.h"
#include "pico/binding/environment.h"
#include "pico/analysis/abstraction.h"
#include "pico/analysis/typecheck.h"
#include "pico/codegen/codegen.h"
#include "pico/eval/call.h"

#include "test/test_log.h"
#include "test_pico/helper.h"

typedef struct {
    void (*on_expr)(PiType* type, void* val, void* data, TestLog* log);
    void (*on_top)(void* data, TestLog* log);
    void (*on_pi_error)(MultiError err, IStream* sin, TestLog* log);
    void (*on_error)(String msg, TestLog* log);
    void (*on_exit)(void* data, TestLog* log);
} Callbacks;

typedef struct {
    jump_buf point;
} NotImplementedCtx;

void skip_hook(void *ctx) {
    jump_buf* jmp = ctx;
    long_jump(*jmp, 1);
}

void run_toplevel_internal(const char *string, Module *module, Environment* env, Callbacks callbacks, void* data, TestLog* log, Target target, RegionAllocator *region) {
    // Note: we need to be aware of the arena and error point, as both are used
    // by code in the 'true' branches of the nonlocal exits, and may be stored
    // in registers, so they cannotbe changed (unless marked volatile).
    Allocator ra = ra_to_gpa(region);
    PiAllocator pico_region = convert_to_pallocator(&ra);
    PiAllocator* pia = &pico_region;

    IStream* sin = mk_string_istream(mv_string(string), &ra);
    IStream* cin = mk_capturing_istream(sin, &ra);

    clear_assembler(target.target);
    clear_assembler(target.code_aux);
    target.data_aux->len = 0;

    jump_buf exit_point;
    if (set_jump(exit_point)) goto on_exit;
    set_exit_callback(&exit_point);

    jump_buf on_not_implemend;
    if (set_jump(on_not_implemend)) goto on_not_implemented;
    Hook hook = (Hook) {.fn = skip_hook, .ctx = &on_not_implemend};
    set_not_implemented_hook(hook);

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

    // -------------------------------------------------------------------------
    // Resolution
    // -------------------------------------------------------------------------
    TopLevel abs = abstract(res.result, env, &ra, &pi_point);

    Logger* logger = get_structured_logger(log);
    TypeCheckContext ctx = (TypeCheckContext) {
        .a = &ra, .pia = pia, .point = &pi_point, .target = target, .logger = logger,
    };
    type_check(&abs, env, ctx);

    clear_target(target);
    LinkData links = generate_toplevel(abs, env, target, &ra, &point);
    EvalResult evres = pico_run_toplevel(abs, target, links, module, &ra, &point);

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

    delete_istream(sin, &ra);
    return;

 on_pi_error:
    if (callbacks.on_pi_error) {
        callbacks.on_pi_error(pi_point.multi, cin, log);
    }
    delete_istream(sin, &ra);
    return;

 on_error:
    if (callbacks.on_error) {
        callbacks.on_error(doc_to_str(point.error_message, 120, &ra), log);
    }
    delete_istream(sin, &ra);
    return;

 on_exit:
    if (callbacks.on_exit) {
        callbacks.on_exit(data, log);
    }
    delete_istream(sin, &ra);
    return;

 on_not_implemented:
    delete_istream(sin, &ra);
    test_skip(log);
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
    ArenaAllocator* arena = make_arena_allocator(4096, get_std_allocator());
    Allocator gpa = aa_to_gpa(arena);
    display_error(err, *get_captured_buffer(cin), get_fstream(log), mv_string("test-suite"), &gpa);
    test_log_error(log, mv_string("Test failure - message logged"));
    test_fail(log);
    delete_arena_allocator(arena);
}

void expr_eql(PiType* type, void* val, void* data, TestLog* log) {
    Allocator* std = get_std_allocator();
    if (!pi_value_eql(type, val, data, std)) {
        ArenaAllocator* arena = make_arena_allocator(4096, std);
        Allocator gpa = aa_to_gpa(arena);
        FormattedOStream* os = get_fstream(log);
        write_fstring(mv_string("Expected: "), os);
        Document* doc = pretty_pi_value(data, type, &gpa);
        write_doc_formatted(doc, 120, os);
        delete_doc(doc, &gpa);
        write_fstring(mv_string("\nGot: "), os);
        doc = pretty_pi_value(val, type, &gpa);
        write_doc_formatted(doc, 120, os);
        delete_doc(doc, &gpa);
        write_fstring(mv_string("\n"), os);
        test_fail(log);
        delete_arena_allocator(arena);
    } else {
        test_pass(log);
    }
}
void expr_assert_eql(PiType* type, void* val, void* data, TestLog* log) {
    Allocator* std = get_std_allocator();
    if (!pi_value_eql(type, val, data, std)) {
        ArenaAllocator* arena = make_arena_allocator(4096, std);
        Allocator gpa = aa_to_gpa(arena);
        FormattedOStream* os = get_fstream(log);
        write_fstring(mv_string("Expected: "), os);
        Document* doc = pretty_pi_value(data, type, &gpa);
        write_doc_formatted(doc, 120, os);
        delete_doc(doc, &gpa);
        write_fstring(mv_string("\nGot: "), os);
        doc = pretty_pi_value(val, type, &gpa);
        write_doc_formatted(doc, 120, os);
        delete_doc(doc, &gpa);
        write_fstring(mv_string("\n"), os);
        test_fail(log);
        delete_arena_allocator(arena);
    }
}

void top_eql(void *data, TestLog *log) {
    FormattedOStream* os = get_fstream(log);
    write_fstring(mv_string("Did not evaluate to a value."), os);
    test_fail(log);
}

void test_toplevel_eq(const char *string, void *expected_val, Module *module, TestContext context) {
    Callbacks callbacks = (Callbacks) {
        .on_expr = expr_eql,
        .on_top = top_eql,
        .on_pi_error = fail_pi_error,
        .on_error = fail_error,
        .on_exit = fail_exit,
    };
    RegionAllocator* subregion = make_subregion(context.region);
    run_toplevel_internal(string, module, context.env, callbacks, expected_val,
                          context.log, context.target, subregion);
    release_subregion(subregion);
    clear_not_implemented_hook();
}

void assert_toplevel_eq(const char *string, void *expected_val, Module *module, TestContext context) {
    Callbacks callbacks = (Callbacks) {
        .on_expr = expr_assert_eql,
        .on_top = top_eql,
        .on_pi_error = fail_pi_error,
        .on_error = fail_error,
        .on_exit = fail_exit,
    };
    RegionAllocator* subregion = make_subregion(context.region);
    run_toplevel_internal(string, module, context.env, callbacks, expected_val,
                          context.log, context.target, subregion);
    release_subregion(subregion);
    clear_not_implemented_hook();
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
        ArenaAllocator* arena = make_arena_allocator(4096, get_std_allocator());
        Allocator gpa = aa_to_gpa(arena);
        StdoutData vals = *(StdoutData*)data;
        String actual = *current_string(vals.stream, &gpa);
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
        delete_arena_allocator(arena);
    }
}

void expr_assert_stdout(PiType* type, void* val, void* data, TestLog* log) {
    if (type->sort != TPrim || type->prim != Unit) {
        test_log_error(log, mv_string("stdout tests expecte expressions to have unit type"));
        test_fail(log);
    } else {
        ArenaAllocator* arena = make_arena_allocator(512, get_std_allocator());
        Allocator gpa = aa_to_gpa(arena);
        StdoutData vals = *(StdoutData*)data;
        String actual = *current_string(vals.stream, &gpa);
        if (string_cmp(actual, vals.expected) != 0) {
            FormattedOStream* os = get_fstream(log);
            write_fstring(mv_string("Expected: "), os);
            write_fstring(vals.expected, os);
            write_fstring(mv_string("\nGot: "), os);
            write_fstring(actual, os);
            write_fstring(mv_string("\n"), os);
            test_fail(log);
        }
        delete_arena_allocator(arena);
    }
}

void top_stdout(void *data, TestLog *log) {
    FormattedOStream* os = get_fstream(log);
    write_fstring(mv_string("Did not evaluate to a value."), os);
    test_fail(log);
}

void test_toplevel_stdout(const char *string, const char *expected_stdout, Module *module, TestContext context) {
    Callbacks callbacks = (Callbacks) {
        .on_expr = expr_stdout,
        .on_top = top_stdout,
        .on_pi_error = fail_pi_error,
        .on_error = fail_error,
        .on_exit = fail_exit,
    };
    RegionAllocator* subregion = make_subregion(context.region);
    Allocator gpa = ra_to_gpa(subregion);
    OStream* out = mk_string_ostream(&gpa);
    StdoutData data = (StdoutData) {
        .stream = out,
        .expected = mv_string(expected_stdout),
    };

    OStream* old = set_std_ostream(out);
    Hook hook = (Hook) {.fn = skip_hook, .ctx = context.log};
    set_not_implemented_hook(hook);
    run_toplevel_internal(string, module, context.env, callbacks, &data, context.log, context.target, subregion);
    clear_not_implemented_hook();
    set_std_ostream(old);
    release_subregion(subregion);
}

void assert_toplevel_stdout(const char *string, const char *expected_stdout, Module *module, TestContext context) {
    Callbacks callbacks = (Callbacks) {
        .on_expr = expr_assert_stdout,
        .on_top = top_stdout,
        .on_pi_error = fail_pi_error,
        .on_error = fail_error,
        .on_exit = fail_exit,
    };
    RegionAllocator* subregion = make_subregion(context.region);
    Allocator gpa = ra_to_gpa(subregion);
    OStream* out = mk_string_ostream(&gpa);
    StdoutData data = (StdoutData) {
        .stream = out,
        .expected = mv_string(expected_stdout),
    };

    OStream* old = set_std_ostream(out);
    Hook hook = (Hook) {.fn = skip_hook, .ctx = context.log};
    set_not_implemented_hook(hook);
    run_toplevel_internal(string, module, context.env, callbacks, &data, context.log, context.target, subregion);
    clear_not_implemented_hook();
    set_std_ostream(old);
    release_subregion(subregion);
}

typedef struct {
    const void* expected;
    const void* actual;
    size_t memsize;
} MemData;

void expr_mem(PiType* type, void* val, void* data, TestLog* log) {
    if (type->sort != TPrim || type->prim != Unit) {
        test_log_error(log, mv_string("stdout tests expecte expressions to have unit type"));
        test_fail(log);
    } else {
        ArenaAllocator* arena = make_arena_allocator(4096, get_std_allocator());
        Allocator gpa = aa_to_gpa(arena);
        MemData vals = *(MemData*)data;
        if (memcmp(vals.actual, vals.expected, vals.memsize) != 0) {
            FormattedOStream* os = get_fstream(log);
            write_fstring(mv_string("Expected: "), os);
            write_fstring(string_hex_mem(vals.expected, vals.memsize, &gpa), os);
            write_fstring(mv_string("\nGot: "), os);
            write_fstring(string_hex_mem(vals.actual, vals.memsize, &gpa), os);
            write_fstring(mv_string("\n"), os);
            test_fail(log);
        } else {
            test_pass(log);
        }
        delete_arena_allocator(arena);
    }
}

void assert_expr_mem(PiType* type, void* val, void* data, TestLog* log) {
    if (type->sort != TPrim || type->prim != Unit) {
        test_log_error(log, mv_string("stdout tests expecte expressions to have unit type"));
        test_fail(log);
    } else {
        ArenaAllocator* arena = make_arena_allocator(1024, get_std_allocator());
        Allocator gpa = aa_to_gpa(arena);
        MemData vals = *(MemData*)data;
        if (memcmp(vals.actual, vals.expected, vals.memsize) != 0) {
            FormattedOStream* os = get_fstream(log);
            write_fstring(mv_string("Expected: "), os);
            write_fstring(string_hex_mem(vals.expected, vals.memsize, &gpa), os);
            write_fstring(mv_string("\nGot: "), os);
            write_fstring(string_hex_mem(vals.actual, vals.memsize, &gpa), os);
            write_fstring(mv_string("\n"), os);
            test_fail(log);
        } else {
            test_pass(log);
        }
        delete_arena_allocator(arena);
    }
}

void top_mem(void *data, TestLog *log) {
    FormattedOStream* os = get_fstream(log);
    write_fstring(mv_string("Did not evaluate to a value."), os);
    test_fail(log);
}

void test_toplevel_mem(const char *string, const void *expected, const void* actual, size_t memsize, Module *module, TestContext context) {
    Callbacks callbacks = (Callbacks) {
        .on_expr = expr_mem,
        .on_top = top_mem,
        .on_pi_error = fail_pi_error,
        .on_error = fail_error,
        .on_exit = fail_exit,
    };
    RegionAllocator* subregion = make_subregion(context.region);
    Allocator gpa = ra_to_gpa(subregion);
    OStream* out = mk_string_ostream(&gpa);
    MemData data = (MemData) {
        .expected = expected,
        .actual = actual,
        .memsize = memsize,
    };

    OStream* old = set_std_ostream(out);
    Hook hook = (Hook) {.fn = skip_hook, .ctx = context.log};
    set_not_implemented_hook(hook);
    run_toplevel_internal(string, module, context.env, callbacks, &data, context.log, context.target, subregion);
    clear_not_implemented_hook();
    set_std_ostream(old);
    release_subregion(subregion);
}

void assert_toplevel_mem(const char *string, const void *expected, const void* actual, size_t memsize, Module *module, TestContext context) {
    Callbacks callbacks = (Callbacks) {
        .on_expr = assert_expr_mem,
        .on_top = top_mem,
        .on_pi_error = fail_pi_error,
        .on_error = fail_error,
        .on_exit = fail_exit,
    };
    RegionAllocator* subregion = make_subregion(context.region);
    Allocator gpa = ra_to_gpa(subregion);
    OStream* out = mk_string_ostream(&gpa);
    MemData data = (MemData) {
        .expected = expected,
        .actual = actual,
        .memsize = memsize,
    };

    OStream* old = set_std_ostream(out);
    Hook hook = (Hook) {.fn = skip_hook, .ctx = context.log};
    set_not_implemented_hook(hook);
    run_toplevel_internal(string, module, context.env, callbacks, &data, context.log, context.target, subregion);
    clear_not_implemented_hook();
    set_std_ostream(old);
    release_subregion(subregion);
}

void log_exit(void* data, TestLog* log) {
    test_log_error(log, mv_string("Test failure - (exit) called unexpectedly during test"));
}

void log_error(String err, TestLog* log) {
    test_log_error(log, err);
}

void log_pi_error(MultiError err, IStream* cin, TestLog* log) {
    // TODO: improve the 'test_log_error' so it can take in a document 
    ArenaAllocator* arena = make_arena_allocator(256, get_std_allocator());
    Allocator gpa = aa_to_gpa(arena);
    display_error(err, *get_captured_buffer(cin), get_fstream(log), mv_string("test-suite"), &gpa);
    test_log_error(log, mv_string("Test failure - message logged"));
    delete_arena_allocator(arena);
}

void run_toplevel(const char *string, Module *module, TestContext context) {
    Callbacks callbacks = (Callbacks) {
        .on_pi_error = log_pi_error,
        .on_error = log_error,
        .on_exit = log_exit,
    };
    RegionAllocator* subregion = make_subregion(context.region);
    run_toplevel_internal(string, module, context.env, callbacks, NULL,
                          context.log, context.target, subregion);
    release_subregion(subregion);
}

typedef struct {
    void (* on_expr)(PiType* actual, void* data, TestLog* log);
    void (* on_top)(void* data, TestLog* log);
    void (* on_pi_error)(MultiError err, IStream* sin, TestLog* log);
    void (* on_error)(String msg, TestLog* log);
    void (* on_exit)(void* data, TestLog* log);
} TypeCallbacks;

void test_typecheck_internal(const char *string, Environment* env, TypeCallbacks callbacks, void* data, TestLog* log, RegionAllocator* region) {
    Allocator ra = ra_to_gpa(region);
    PiAllocator pico_region = convert_to_pallocator(&ra);
    PiAllocator* pia = &pico_region;

    IStream* sin = mk_string_istream(mv_string(string), &ra);
    Allocator exalloc = mk_executable_allocator(&ra);
    Allocator* exec = &exalloc;
    // Note: we need to be aware of the arena and error point, as both are used
    // by code in the 'true' branches of the nonlocal exits, and may be stored
    // in registers, so they cannotbe changed (unless marked volatile).
    IStream* cin = mk_capturing_istream(sin, &ra);

    Target gen_target = {
        .target = mk_assembler(current_cpu_feature_flags(), exec),
        .code_aux = mk_assembler(current_cpu_feature_flags(), exec),
        .data_aux = mem_alloc(sizeof(U8Array), &ra)
    };
    *gen_target.data_aux = mk_u8_array(128, &ra);

    jump_buf exit_point;
    if (set_jump(exit_point)) goto on_exit;
    set_exit_callback(&exit_point);

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

    // -------------------------------------------------------------------------
    // Resolution
    // -------------------------------------------------------------------------

    TopLevel abs = abstract(res.result, env, &ra, &pi_point);

    Logger* logger = get_structured_logger(log);
    TypeCheckContext ctx = (TypeCheckContext) {
        .a = &ra, .pia = pia, .point = &pi_point, .target = gen_target, .logger = logger,
    };
    type_check(&abs, env, ctx);

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
    release_executable_allocator(exalloc);
    delete_istream(sin, &ra);
    return;

 on_pi_error:
    if (callbacks.on_pi_error) {
        callbacks.on_pi_error(pi_point.multi, cin, log);
    }
    delete_assembler(gen_target.target);
    delete_assembler(gen_target.code_aux);
    release_executable_allocator(exalloc);
    delete_istream(sin, &ra);
    return;

 on_error:
    if (callbacks.on_error) {
        callbacks.on_error(doc_to_str(point.error_message, 120, &ra), log);
    }
    delete_assembler(gen_target.target);
    delete_assembler(gen_target.code_aux);
    release_executable_allocator(exalloc);
    delete_istream(sin, &ra);
    return;

 on_exit:
    if (callbacks.on_exit) {
        callbacks.on_exit(data, log);
    }
    delete_assembler(gen_target.target);
    delete_assembler(gen_target.code_aux);
    release_executable_allocator(exalloc);
    delete_istream(sin, &ra);
    return;
}

void type_eql(PiType* type, void* data, TestLog* log) {
    Allocator* std = get_std_allocator();
    if (!pi_type_eql(type, data, std)) {
        ArenaAllocator* arena = make_arena_allocator(4096, std);
        Allocator gpa = aa_to_gpa(arena);
        FormattedOStream* os = get_fstream(log);
        write_fstring(mv_string("Expected: "), os);
        Document* doc = pretty_type(data, &gpa);
        write_doc_formatted(doc, 120, os);
        delete_doc(doc, &gpa);
        write_fstring(mv_string("\nGot: "), os);
        doc = pretty_type(type, &gpa);
        write_doc_formatted(doc, 120, os);
        delete_doc(doc, &gpa);
        write_fstring(mv_string("\n"), os);
        test_fail(log);
        delete_arena_allocator(arena);
    } else {
        test_pass(log);
    }
}

void top_type(void *data, TestLog *log) {
    FormattedOStream* os = get_fstream(log);
    write_fstring(mv_string("Typecheck tests only typecheck expressions."), os);
    test_fail(log);
}

void test_typecheck_eq(const char *string, PiType* expected, Environment* env, TestContext context) {
    TypeCallbacks callbacks = (TypeCallbacks) {
        .on_expr = type_eql,
        .on_top = top_type,
        .on_pi_error = fail_pi_error,
        .on_error = fail_error,
        .on_exit = fail_exit,
    };
    Hook hook = (Hook) {.fn = skip_hook, .ctx = context.log};
    set_not_implemented_hook(hook);
    RegionAllocator* subregion = make_subregion(context.region);
    test_typecheck_internal(string, env, callbacks, expected, context.log, subregion);
    release_subregion(subregion);
    clear_not_implemented_hook();
}
