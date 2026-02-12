#include "pico/typecheck/type_errors.h"

_Noreturn void type_error_unexpected_module(TypeCheckContext ctx, Syntax* syn, Module* module) {
    PicoError err = {
        .message = mv_cstr_doc("Unexpected module.", ctx.a),
        .range = syn->range,
    };
    throw_pi_error(ctx.point, err);
}
