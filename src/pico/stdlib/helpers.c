#include <stdarg.h>

#include "platform/jump.h"
#include "platform/error.h"

#include "data/stream.h"

#include "pico/parse/parse.h"
#include "pico/stdlib/extra.h"
#include "pico/analysis/abstraction.h"
#include "pico/analysis/typecheck.h"
#include "pico/codegen/codegen.h"
#include "pico/eval/call.h"

#include "pico/stdlib/helpers.h"

void compile_toplevel(const char *string, Module *module, Target target, ErrorPoint *final_point, PiErrorPoint *final_pi_point, RegionAllocator* region) {
    Allocator ra = ra_to_gpa(region);
    clear_target(target);

    IStream* sin = mk_string_istream(mv_string(string), &ra);
    // Note: we need to be aware of the arena and error point, as both are used
    // by code in the 'true' branches of the nonlocal exits, and may be stored
    // in registers, so they cannotbe changed (unless marked volatile).
    IStream* cin = mk_capturing_istream(sin, &ra);
    Logger* logger = NULL;

    jump_buf exit_point;
    if (set_jump(exit_point)) goto on_exit;
    set_exit_callback(&exit_point);

    ErrorPoint point;
    if (catch_error(point)) goto on_error;

    PiErrorPoint pi_point;
    if (catch_error(pi_point)) goto on_pi_error;

    Environment* env = env_from_module(module, &point, &ra);

    PiAllocator pia = convert_to_pallocator(&ra);
    ParseResult res = parse_rawtree(cin, &pia, &ra);
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
    //  Process Term: abstract, typecheck, & evaluate
    // -------------------------------------------------------------------------

    TopLevel abs = abstract(res.result, env, &ra, &pi_point);

#ifdef DEBUG
    logger = make_logger(&ra);
#endif
    TypeCheckContext ctx = (TypeCheckContext) {
        .a = &ra, .pia = &pia, .point = &pi_point, .target = target, .logger = logger,
    };
    type_check(&abs, env, ctx);

    clear_target(target);
    LinkData links = generate_toplevel(abs, env, target, &ra, &point);
    pico_run_toplevel(abs, target, links, module, &ra, &point);

    delete_istream(sin, &ra);
    return;

 on_pi_error:
    display_error(pi_point.multi, *get_captured_buffer(cin), get_formatted_stdout(), mv_string("C Sources"), &ra);
#ifdef DEBUG
    if (logger) {
        write_fstring(mv_string(" Writing Structured Log\n"), get_formatted_stdout());
        write_fstring(mv_string("--------------------------\n"), get_formatted_stdout());
        log_to_formatted_ostream(logger, 120, get_formatted_stdout());
    }
#endif
    delete_istream(sin, &ra);
    throw_error(final_point, mv_cstr_doc("Compile-time failure - message written to stdout", &ra));

 on_error:
    delete_istream(sin, &ra);
    throw_error(final_point, point.error_message);

 on_exit:
    delete_istream(sin, &ra);
    throw_error(final_point, mv_cstr_doc("Startup compiled definition not exepcted to exit!", &ra));
}

void add_import(ImportClauseArray* arr, Allocator* a, size_t len, ...) {
    SymbolArray path = mk_symbol_array(len, a);
    va_list args;
    va_start(args, len);
    for (size_t i = 0; i < len; i++) {
        const char* name = va_arg(args, const char*);
        push_symbol(string_to_symbol(mv_string(name)), &path);
    }
    push_import_clause((ImportClause) {
            .type = Import,
            .path = path,
        },
        arr);
}

void add_import_all(ImportClauseArray* arr, Allocator* a, size_t len, ...) {
    SymbolArray path = mk_symbol_array(len, a);
    va_list args;
    va_start(args, len);
    for (size_t i = 0; i < len; i++) {
        const char* name = va_arg(args, const char*);
        push_symbol(string_to_symbol(mv_string(name)), &path);
    }
    va_end(args);
    push_import_clause((ImportClause) {
            .type = ImportAll,
            .path = path,
        },
        arr);
}
