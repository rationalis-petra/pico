#include "platform/memory/arena.h"
#include "platform/memory/executable.h"
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

void compile_toplevel(const char *string, Module *module, ErrorPoint *final_point, PiErrorPoint *final_pi_point, Allocator *a) {
    IStream* sin = mk_string_istream(mv_string(string), a);
    Allocator exalloc = mk_executable_allocator(a);
    Allocator* exec = &exalloc;
    // Note: we need to be aware of the arena and error point, as both are used
    // by code in the 'true' branches of the nonlocal exits, and may be stored
    // in registers, so they cannotbe changed (unless marked volatile).
    Allocator arena = mk_arena_allocator(4096, a);

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

    ParseResult res = parse_rawtree(sin, &arena);
    if (res.type == ParseNone) {
        throw_error(&point, mv_string("Parse Returned None!"));
    }
    if (res.type == ParseFail) {
        throw_error(&point, mv_string("Parse Failed :(\n"));
    }
    if (res.type != ParseSuccess) {
        // If parse is invalid, means internal bug, so better exit soon!
        throw_error(&point, mv_string("Parse Returned Invalid Result!\n"));
    }

    // -------------------------------------------------------------------------
    // Resolution
    // -------------------------------------------------------------------------

    TopLevel abs = abstract(res.result, env, &arena, &pi_point);
    type_check(&abs, env, &arena, &point);
    LinkData links = generate_toplevel(abs, env, gen_target, &arena, &point);
    pico_run_toplevel(abs, gen_target, links, module, &arena, &point);

    delete_assembler(gen_target.target);
    delete_assembler(gen_target.code_aux);
    release_arena_allocator(arena);
    return;

 on_pi_error:
    delete_assembler(gen_target.target);
    delete_assembler(gen_target.code_aux);
    release_arena_allocator(arena);
    throw_pi_error(final_pi_point, pi_point.error);

 on_error:
    delete_assembler(gen_target.target);
    delete_assembler(gen_target.code_aux);
    release_arena_allocator(arena);
    throw_error(final_point, point.error_message);

 on_exit:
    delete_assembler(gen_target.target);
    delete_assembler(gen_target.code_aux);
    release_arena_allocator(arena);
    throw_error(final_point, mv_string("Startup compiled definition not exepcted to exit!"));
}
