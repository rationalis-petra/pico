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
#include "pico/stdlib/meta/meta.h"
#include "pico/stdlib/platform/submodules.h"
#include "pico/binding/environment.h"
#include "pico/abstraction/abstraction.h"
#include "pico/typecheck/typecheck.h"
#include "pico/codegen/codegen.h"
#include "pico/eval/call.h"

#include "test/test_log.h"
#include "test_pico/helper.h"

typedef enum {
    EPParse,
    EPAbstract,
    EPTypeCheck,
} ErrorPhase;

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
    // Analysis
    // -------------------------------------------------------------------------
    SynTape tape = mk_syn_tape(&ra, 128);
    AbstractionCtx ab_ctx = {
        .tape = tape, .env = env, .a = &ra, .point = &pi_point,
    };
    TopLevel abs = abstract(res.result, ab_ctx);

    Logger* logger = get_structured_logger(log);
    TypeCheckContext tc_ctx = {
        .tape = tape, .a = &ra, .pia = pia, .point = &pi_point, .target = target, .logger = logger,
    };
    type_check(&abs, env, tc_ctx);

    clear_target(target);
    CodegenContext cg_ctx = {
        .tape = tape, .a = &ra, .pia = pia, .point = &point, .target = target, .logger = logger,
    };
    LinkData links = generate_toplevel(abs, env, cg_ctx);


    // -------------------------------------------------------------------------
    // Evaluation
    // -------------------------------------------------------------------------
    EvalCtx ev_ctx = {
        .tape = tape, .target = target, .links = links, .module = module, .a = &ra, .point = &point
    };
    EvalResult evres = pico_run_toplevel(abs, ev_ctx);

    if (evres.type == ERValue) {
        if (callbacks.on_expr) {
            callbacks.on_expr(evres.val.type, evres.val.val, data, log);
        }
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
        Document* doc = pretty_pi_value(data, type, default_pvp, &gpa);
        write_doc_formatted(doc, 120, os);
        delete_doc(doc, &gpa);
        write_fstring(mv_string("\n     Got: "), os);
        doc = pretty_pi_value(val, type, default_pvp, &gpa);
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
        Document* doc = pretty_pi_value(data, type, default_pvp, &gpa);
        write_doc_formatted(doc, 120, os);
        delete_doc(doc, &gpa);
        write_fstring(mv_string("\n     Got: "), os);
        doc = pretty_pi_value(val, type, default_pvp, &gpa);
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
            write_fstring(mv_string("\n     Got: "), os);
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
            write_fstring(mv_string("\n     Got: "), os);
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
            write_fstring(mv_string("\n     Got: "), os);
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
            write_fstring(mv_string("\n     Got: "), os);
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

void build_module_internal(const char *string, Module *parent, Callbacks callbacks, TestContext context) {
    Package* package = get_package(parent);
    TestLog* log = context.log; 

    Allocator ra = ra_to_gpa(context.region);
    IStream* in = mv_string_istream(mv_string(string), &ra);
    RegionAllocator* iter_region = make_subregion(context.region);
    Allocator itera = ra_to_gpa(iter_region);
    Allocator exec = mk_executable_allocator(&ra);
    Logger* logger = NULL;

    PiAllocator pico_itera = convert_to_pallocator(&itera);

    Target target = (Target) {
        .target = mk_assembler(current_cpu_feature_flags(), &exec),
        .code_aux = mk_assembler(current_cpu_feature_flags(), &exec),
        .data_aux = mem_alloc(sizeof(U8Array), &ra),
    };
    *target.data_aux = mk_u8_array(256, &ra);

    ModuleHeader* volatile header = NULL;
    Module* volatile module = NULL;
    Module* volatile old_module = NULL;

    IStream* cin = mk_capturing_istream(in, &ra);
    reset_bytecount(cin);

    // Step 2:
    // Setup error reporting
    ErrorPoint point;
    if (catch_error(point)) goto on_error;

    PiErrorPoint pi_point;
    if (catch_error(pi_point)) goto on_pi_error;

    // Step 3: Parse Module header, get the result (ph_res)
    ParseResult ph_res = parse_rawtree(cin, &pico_itera, &itera);
    if (ph_res.type == ParseNone) goto on_noparse;

    if (ph_res.type == ParseFail) {
        throw_pi_error(&pi_point, ph_res.error);
        goto on_noparse;
    }

    // Step 3: check / abstract module header
    // • module_header header = parse_module_header
    // Note: volatile is to protect from clobbering by longjmp
    header = abstract_header(ph_res.result, &itera, &pi_point);

    // Step 4:
    //  • Create new module
    //  • Update module based on imports
    // Note: volatile is to protect from clobbering by longjmp
    module = mk_module(*header, package, parent);

    old_module = get_std_current_module();
    set_std_current_module(module);

    // Step 5:
    //  • Using the environment, parse and run each expression/definition in the module
    bool next_iter = true;
    Environment* env = env_from_module(module, &point, &ra);
    while (next_iter) {
        reset_subregion(iter_region);
        refresh_env(env);

        ParseResult res = parse_rawtree(cin, &pico_itera, &itera);
        if (res.type == ParseNone) goto on_exit;

        if (res.type == ParseFail) {
            throw_pi_error(&pi_point, res.error);
        }
        if (res.type != ParseSuccess) {
            PicoError err = {
                .message = mv_cstr_doc("Parse Returned Invalid Result!\n", &ra),
            };
            throw_pi_error(&pi_point, err);
        }

        // -------------------------------------------------------------------------
        // Resolution
        // -------------------------------------------------------------------------

        SynTape tape = mk_syn_tape(&ra, 128);
        AbstractionCtx ab_ctx = {
            .tape = tape, .env = env, .a = &itera, .point = &pi_point,
        };
        TopLevel abs = abstract(res.result, ab_ctx);

        // -------------------------------------------------------------------------
        // Type Checking
        // -------------------------------------------------------------------------

        // Note: typechecking annotates the syntax tree with types, but doesn't have
        // an output.
        TypeCheckContext tc_ctx = {
            .tape = tape, .a = &itera, .pia = &pico_itera, .point = &pi_point, .target = target, .logger = logger
        };
        type_check(&abs, env, tc_ctx);

        // -------------------------------------------------------------------------
        // Code Generation
        // -------------------------------------------------------------------------

        // Ensure the target is 'fresh' for code-gen
        clear_target(target);
        CodegenContext cg_ctx = {
            .tape = tape, .a = &itera, .pia = &pico_itera, .point = &point, .target = target, .logger = logger
        };
        LinkData links = generate_toplevel(abs, env, cg_ctx);

        // -------------------------------------------------------------------------
        // Evaluation
        // -------------------------------------------------------------------------
        EvalCtx ev_ctx = {
            .tape = tape, .target = target, .links = links, .module = module, .a = &ra, .point = &point
        };
        pico_run_toplevel(abs, ev_ctx);
    }
    return;

 on_exit:
    goto cleanup;

 on_noparse:
 cleanup:
    if (old_module) set_std_current_module(old_module);
    uncapture_istream(cin);
    release_subregion(iter_region);
    release_executable_allocator(exec);
    return;

 on_pi_error:
    if (callbacks.on_pi_error) {
        callbacks.on_pi_error(pi_point.multi, cin, log);
    }
    goto on_error_generic;

 on_error:
    if (callbacks.on_error) {
        callbacks.on_error(doc_to_str(point.error_message, 120, &ra), log);
    }
    goto on_error_generic;
    
 on_error_generic:
    if (old_module) set_std_current_module(old_module);
    release_subregion(iter_region);
    release_executable_allocator(exec);
    return;
}


void module_from_string(const char *string, Module *parent, TestContext context) {
    Callbacks callbacks = (Callbacks) {
        .on_pi_error = log_pi_error,
        .on_error = log_error,
        .on_exit = log_exit,
    };

    build_module_internal(string, parent, callbacks, context);
}

typedef struct {
    void (* on_expr)(PiType* actual, void* data, TestLog* log);
    void (* on_top)(void* data, TestLog* log);
    void (* on_pi_error)(MultiError err, IStream* sin, ErrorPhase phase, TestLog* log);
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

    volatile ErrorPhase error_phase = EPParse;

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

    error_phase = EPAbstract;
    SynTape tape = mk_syn_tape(&ra, 128);
    AbstractionCtx ab_ctx = {
        .tape = tape, .env = env, .a = &ra, .point = &pi_point,
    };
    TopLevel abs = abstract(res.result, ab_ctx);

    Logger* logger = get_structured_logger(log);
    TypeCheckContext ctx = (TypeCheckContext) {
        .tape = tape, .a = &ra, .pia = pia, .point = &pi_point, .target = gen_target, .logger = logger,
    };
    error_phase = EPTypeCheck;
    type_check(&abs, env, ctx);

    if (abs.type == TLExpr) {
        if (callbacks.on_expr) {
            callbacks.on_expr(get_type(abs.expr, tape), data, log);
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
        callbacks.on_pi_error(pi_point.multi, cin, error_phase, log);
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
        Document* doc = pretty_type(data, default_ptp, &gpa);
        write_doc_formatted(doc, 120, os);
        delete_doc(doc, &gpa);
        write_fstring(mv_string("\n     Got: "), os);
        doc = pretty_type(type, default_ptp, &gpa);
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

void typecheck_fail_pi_error(MultiError err, IStream* cin, ErrorPhase phase, TestLog* log) {
    ArenaAllocator* arena = make_arena_allocator(4096, get_std_allocator());
    Allocator gpa = aa_to_gpa(arena);
    display_error(err, *get_captured_buffer(cin), get_fstream(log), mv_string("test-suite"), &gpa);
    test_log_error(log, mv_string("Test failure - message logged"));
    test_fail(log);
    delete_arena_allocator(arena);
}

void test_typecheck_eq(const char *string, PiType* expected, Environment* env, TestContext context) {
    TypeCallbacks callbacks = (TypeCallbacks) {
        .on_expr = type_eql,
        .on_top = top_type,
        .on_pi_error = typecheck_fail_pi_error,
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

void succeed_if_type_error(MultiError err, IStream* cin, ErrorPhase phase, TestLog* log) {
    if (phase == EPTypeCheck) {
        test_pass(log);
    } else {
        ArenaAllocator* arena = make_arena_allocator(4096, get_std_allocator());
        Allocator gpa = aa_to_gpa(arena);
        display_error(err, *get_captured_buffer(cin), get_fstream(log), mv_string("test-suite"), &gpa);
        test_log_error(log, mv_string("Test failure - message logged"));
        test_fail(log);
        delete_arena_allocator(arena);
    }
}

void type_fail(PiType* type, void* data, TestLog* log) {
    // TODO (BUG): move error reporting for failure into a separate method!
    test_log_error(log, mv_string("Expecting this term to fail typechecking, but it succeeded!"));
    test_fail(log);
}

void test_typecheck_fail(const char *string, Environment* env, TestContext context) {
    TypeCallbacks callbacks = (TypeCallbacks) {
        .on_expr = type_fail,
        .on_top = top_type,
        .on_pi_error = succeed_if_type_error,
        .on_error = fail_error,
        .on_exit = fail_exit,
    };
    Hook hook = (Hook) {.fn = skip_hook, .ctx = context.log};
    set_not_implemented_hook(hook);
    RegionAllocator* subregion = make_subregion(context.region);
    test_typecheck_internal(string, env, callbacks, NULL, context.log, subregion);
    release_subregion(subregion);
    clear_not_implemented_hook();
}

void succeed_if_abstract_error(MultiError err, IStream* cin, ErrorPhase phase, TestLog* log) {
    if (phase == EPAbstract) {
        test_pass(log);
    } else {
        ArenaAllocator* arena = make_arena_allocator(4096, get_std_allocator());
        Allocator gpa = aa_to_gpa(arena);
        display_error(err, *get_captured_buffer(cin), get_fstream(log), mv_string("test-suite"), &gpa);
        test_log_error(log, mv_string("Test failure - message logged"));
        test_fail(log);
        delete_arena_allocator(arena);
    }
}

void test_abstract_fail(const char *string, Environment* env, TestContext context) {
    TypeCallbacks callbacks = (TypeCallbacks) {
        .on_expr = type_fail,
        .on_top = top_type,
        .on_pi_error = succeed_if_abstract_error,
        .on_error = fail_error,
        .on_exit = fail_exit,
    };
    Hook hook = (Hook) {.fn = skip_hook, .ctx = context.log};
    set_not_implemented_hook(hook);
    RegionAllocator* subregion = make_subregion(context.region);
    test_typecheck_internal(string, env, callbacks, NULL, context.log, subregion);
    release_subregion(subregion);
    clear_not_implemented_hook();
}
