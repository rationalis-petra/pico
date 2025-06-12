#include <stdarg.h>

#include "platform/memory/arena.h"
#include "platform/memory/executable.h"
#include "platform/jump.h"
#include "platform/error.h"

#include "data/stream.h"

#include "pretty/stream_printer.h"

#include "pico/parse/parse.h"
#include "pico/stdlib/extra.h"
#include "pico/analysis/abstraction.h"
#include "pico/analysis/typecheck.h"
#include "pico/codegen/codegen.h"
#include "pico/eval/call.h"

#include "test/test_log.h"

void test_toplevel(const char *string, void *expected_val, Module *module, TestLog* log, Allocator *a) {
    IStream* sin = mk_string_istream(mv_string(string), a);
    Allocator exalloc = mk_executable_allocator(a);
    Allocator* exec = &exalloc;
    // Note: we need to be aware of the arena and error point, as both are used
    // by code in the 'true' branches of the nonlocal exits, and may be stored
    // in registers, so they cannotbe changed (unless marked volatile).
    Allocator arena = mk_arena_allocator(4096, a);
    IStream* cin = mk_capturing_istream(sin, &arena);

    Target gen_target = {
        .target = mk_assembler(exec),
        .code_aux = mk_assembler(exec),
        .data_aux = mem_alloc(sizeof(U8Array), &arena)
    };
    *gen_target.data_aux = mk_u8_array(128, &arena);

    Environment* env = env_from_module(module, &arena);

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
      if (!pi_value_eql(evres.val.type, evres.val.val, expected_val)) {
          test_fail(log);
          FormattedOStream* os = get_fstream(log);
          write_fstring(mv_string("Expected: "), os);
          Document* doc = pretty_pi_value(expected_val, evres.val.type, a);
          write_doc_formatted(doc, 120, os);
          delete_doc(doc, a);
          write_fstring(mv_string("\nGot: "), os);
          doc = pretty_pi_value(evres.val.val, evres.val.type, a);
          write_doc_formatted(doc, 120, os);
          delete_doc(doc, a);
          write_fstring(mv_string("\n"), os);
      } else {
          test_pass(log);
      }
    } else {
        FormattedOStream* os = get_fstream(log);
        write_fstring(mv_string("Did not evaluate to a value."), os);
        test_fail(log);
    }

    delete_assembler(gen_target.target);
    delete_assembler(gen_target.code_aux);
    release_arena_allocator(arena);
    release_executable_allocator(exalloc);
    delete_istream(sin, a);
    return;

 on_pi_error:
    display_error(pi_point.multi, cin, get_fstream(log), &arena);
    delete_assembler(gen_target.target);
    delete_assembler(gen_target.code_aux);
    release_arena_allocator(arena);
    release_executable_allocator(exalloc);
    delete_istream(sin, a);
    test_fail(log);
    return;

 on_error:
    delete_assembler(gen_target.target);
    delete_assembler(gen_target.code_aux);
    release_arena_allocator(arena);
    release_executable_allocator(exalloc);
    delete_istream(sin, a);
    test_fail(log);
    return;

 on_exit:
    delete_assembler(gen_target.target);
    delete_assembler(gen_target.code_aux);
    release_arena_allocator(arena);
    release_executable_allocator(exalloc);
    delete_istream(sin, a);
    test_fail(log);
    return;
}

void run_toplevel(const char *string, Module *module, TestLog* log, Allocator *a) {
    IStream* sin = mk_string_istream(mv_string(string), a);
    Allocator exalloc = mk_executable_allocator(a);
    Allocator* exec = &exalloc;
    // Note: we need to be aware of the arena and error point, as both are used
    // by code in the 'true' branches of the nonlocal exits, and may be stored
    // in registers, so they cannotbe changed (unless marked volatile).
    Allocator arena = mk_arena_allocator(4096, a);
    IStream* cin = mk_capturing_istream(sin, &arena);

    Target gen_target = {
        .target = mk_assembler(exec),
        .code_aux = mk_assembler(exec),
        .data_aux = mem_alloc(sizeof(U8Array), &arena)
    };
    *gen_target.data_aux = mk_u8_array(128, &arena);

    Environment* env = env_from_module(module, &arena);

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
    pico_run_toplevel(abs, gen_target, links, module, &arena, &point);

    // TODO: check evres == expected_val 

    delete_assembler(gen_target.target);
    delete_assembler(gen_target.code_aux);
    release_arena_allocator(arena);
    release_executable_allocator(exalloc);
    delete_istream(sin, a);
    return;

 on_pi_error:
    display_error(pi_point.multi, cin, get_fstream(log), &arena);
    delete_assembler(gen_target.target);
    delete_assembler(gen_target.code_aux);
    release_arena_allocator(arena);
    release_executable_allocator(exalloc);
    delete_istream(sin, a);
    test_log_error(log, mv_string("Test failure - message logged"));
    return;

 on_error:
    delete_assembler(gen_target.target);
    delete_assembler(gen_target.code_aux);
    release_arena_allocator(arena);
    release_executable_allocator(exalloc);
    delete_istream(sin, a);
    test_log_error(log, mv_string("Test failure - message logged?"));
    return;

 on_exit:
    delete_assembler(gen_target.target);
    delete_assembler(gen_target.code_aux);
    release_arena_allocator(arena);
    release_executable_allocator(exalloc);
    delete_istream(sin, a);
    test_log_error(log, mv_string("Test failure - (exit) called unexpectedly during test"));
    return;
}
