#include "pico/abstraction/abstraction_errors.h"

_Noreturn void proc_tyformer_incorrect_numterms(RawTree raw, AbstractionCtx ctx) {
    Document* message;
    if (raw.branch.nodes.len > 3) {
        message = mv_cstr_doc("Proc type has received too many terms - expect to receive only an argument list and return type.", ctx.gpa);
    } else if (raw.branch.nodes.len == 2) {
        message = mv_cstr_doc("Proc type missing return argument.", ctx.gpa);
    } else {
        message = mv_cstr_doc("Proc type missing both argument list and return argument.", ctx.gpa);
    }

    
    PicoError err = {
        .range = raw.range,
        .message = message,
    };
    throw_pi_error(ctx.point, err);
}
