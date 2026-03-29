#include "data/stream.h"

#include "platform/memory/executable.h"

#include "pico/parse/parse.h"
#include "pico/abstraction/abstraction.h"
#include "pico/typecheck/typecheck.h"

#include "app/dev/process_term.h"

ProcessResult process_term(ProcessInput input, RegionAllocator* region) {
    Allocator ra = ra_to_gpa(region);
    PiAllocator pi_ra = convert_to_pallocator(&ra);
    Allocator exec = mk_executable_allocator(&ra);

    IStream* strin = mv_string_istream(input.input, &ra);
    IStream* cin = mk_capturing_istream(strin, &ra);

    ProcessResult result = {};

    ParseResult res = parse_rawtree(cin, &pi_ra, &ra);

    if (res.type == ParseNone) return result;
    if (res.type == ParseFail) {
        if (input.options & POErrors) {
            PtrArray* errors = mem_alloc(sizeof(PtrArray), &ra);
            *errors = mk_ptr_array(1, &ra);

            PicoError* error = mem_alloc(sizeof(PicoError), &ra);
            *error = res.error;
            push_ptr(error, errors);
            result.errors = errors;
        }
        return result;
    }

    PiErrorPoint pi_point;
    if (catch_error(pi_point)) goto on_pi_error;

    ErrorPoint point;
    if (catch_error(point)) goto on_error;

    Environment* env = env_from_module(input.current, &point, &ra);

    // -------------------------------------------------------------------------
    // Resolution
    // -------------------------------------------------------------------------
    TopLevel abs = abstract(res.result, env, &ra, &pi_point);

    // -------------------------------------------------------------------------
    // Type Checking
    // -------------------------------------------------------------------------
    Target target = (Target) {
        .target = mk_assembler(current_cpu_feature_flags(), &exec),
        .code_aux = mk_assembler(current_cpu_feature_flags(), &exec),
        .data_aux = mem_alloc(sizeof(U8Array), &ra),
    };
    *target.data_aux = mk_u8_array(256, &ra);
    TypeCheckContext tc_ctx = {
        .a = &ra, .pia = &pi_ra, .point = &pi_point, .target = target, 
    };
    type_check(&abs, env, tc_ctx);

    release_executable_allocator(exec);
    result.has_term = Some;
    result.checked_term = abs;
    return result;

 on_pi_error:

    if (input.options & POErrors) {
        PtrArray* errors = mem_alloc(sizeof(PtrArray), &ra);
        if (pi_point.multi.has_many) {
            *errors = pi_point.multi.errors;
        } else {
            *errors = mk_ptr_array(1, &ra);

            PicoError* error = mem_alloc(sizeof(PicoError), &ra);
            *error = pi_point.multi.error;
            push_ptr(error, errors);
        }

        result.errors = errors;
    }
    release_executable_allocator(exec);
    return result;

 on_error:
    if (input.options & POErrors) {
        PtrArray* errors = mem_alloc(sizeof(PtrArray), &ra);
        *errors = mk_ptr_array(1, &ra);

        PicoError* error = mem_alloc(sizeof(PicoError), &ra);
        *error = (PicoError) {
            .message = point.error_message,
        };
        push_ptr(error, errors);

        result.errors = errors;
    }
    release_executable_allocator(exec);
    return result;
}
