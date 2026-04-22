#include "platform/machine_info.h"
#if ARCH == AARCH64

#include "platform/signals.h"

#include "pico/codegen/backend-direct/generate.h"

LinkData bd_generate_toplevel(TopLevel top, Environment* env, CodegenContext ctx) {
    panic(mv_string("Not implemented: bd_generate_toplevel"));
}

LinkData bd_generate_expr(SynRef syn, Environment* env, CodegenContext ctx) {
    panic(mv_string("Not implemented: bd_generate_expr"));
}

void bd_generate_type_expr(SynRef syn, TypeEnv* env, CodegenContext ctx) {
    panic(mv_string("Not implemented: bd_generate_type_expr"));
}

#endif
