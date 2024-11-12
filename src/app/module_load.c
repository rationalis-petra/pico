#include "app/module_load.h"

#include "memory/arena.h"
#include "memory/executable.h"
#include "assembler/assembler.h"

#include "pico/binding/environment.h"
#include "pico/parse/parse.h"
#include "pico/analysis/abstraction.h"
#include "pico/analysis/typecheck.h"
#include "pico/codegen/codegen.h"
#include "pico/eval/call.h"


void load_module_from_istream(IStream* in, OStream* serr, Package* pkg_parent, Module* parent, Allocator* a) {
    Allocator arena = mk_arena_allocator(4096, a);
    Allocator exec = mk_executable_allocator(a);

    Assembler* ass = mk_assembler(&exec);

    ErrorPoint point;
    if (catch_error(point)) goto on_error;

    // Step 1: Parse Module header, get the result (ph_res)
    ParseResult ph_res = parse_rawtree(in, &arena);
    if (ph_res.type == ParseNone) goto on_exit;

    if (ph_res.type == ParseFail) {
        write_string(mv_string("Parse Failed :(\n"), serr);
        release_arena_allocator(arena);
        return;
    }

    // Step 2: Check / abstract module header
    // • module_header header = parse_module_header
    ModuleHeader* header = abstract_header(ph_res.data.result, &arena, &point);

    // Step 3:
    //  • Create new module
    //  • Update module based on imports
    Module* module = mk_module(*header, pkg_parent, parent, a);

    // Step 4:
    //  • Using the environment, parse each expression in the 
    //  • Create new module
    bool next_iter = true;
    while (next_iter) {
        // Prep the arena for another round
        reset_arena_allocator(arena);
        Environment* env = env_from_module(module, &arena);

        ParseResult res = parse_rawtree(in, &arena);
        if (res.type == ParseNone) goto on_exit;

        if (res.type == ParseFail) {
            write_string(mv_string("Parse Failed :(\n"), serr);
            release_arena_allocator(arena);
            return;
        }
        if (res.type != ParseSuccess) {
            write_string(mv_string("Parse Returned Invalid Result!\n"), serr);
            release_arena_allocator(arena);
            return;
        }

        // -------------------------------------------------------------------------
        // Resolution
        // -------------------------------------------------------------------------

        TopLevel abs = abstract(res.data.result, env, &arena, &point);

        // -------------------------------------------------------------------------
        // Type Checking
        // -------------------------------------------------------------------------

        // Note: typechecking annotates the syntax tree with types, but doesn't have
        // an output.
        type_check(&abs, env, &arena, &point);

        // -------------------------------------------------------------------------
        // Code Generation
        // -------------------------------------------------------------------------

        GenResult gen_res = generate_toplevel(abs, env, ass, &arena, &point);

        // -------------------------------------------------------------------------
        // Evaluation
        // -------------------------------------------------------------------------

        pico_run_toplevel(abs, ass, &(gen_res.backlinks), module, &arena, &point);

        release_arena_allocator(arena);
    }

    return;

 on_exit:
    release_arena_allocator(arena);
    release_executable_allocator(exec);
    return;

 on_error:
    write_string(point.error_message, serr);
    write_string(mv_string("\n"), serr);
    release_arena_allocator(arena);
    release_executable_allocator(exec);
    return;
        
}
