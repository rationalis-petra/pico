#include "platform/signals.h"

#include "pico/data/error.h"
#include "pico/binding/address_env.h"
#include "pico/codegen/codegen.h"
#include "pico/codegen/backend-direct/generate.h"
#include "pico/codegen/backend-direct/foreign_adapters.h"
#include "pico/codegen/backend-pvm/generate.h"
#include "pico/codegen/backend-direct/foreign_adapters.h"

static CodegenBackend global_backend = CodegenDirect;

void init_codegen(CodegenBackend backend, Allocator *alloc) {
    global_backend = backend;
}

void teardown_codegen() {
}

LinkData generate_toplevel(TopLevel top, Environment* env, CodegenContext ctx) {
    switch (global_backend) {
    case CodegenDirect:
        return bd_generate_toplevel(top, env, ctx);
    case CodegenPVM:
        return pvm_generate_toplevel(top, env, ctx);
    }
    panic(mv_string("Invalid codegen backend selected."));
}

LinkData generate_expr(Syntax* syn, Environment* env, CodegenContext ctx) {
    switch (global_backend) {
    case CodegenDirect:
        return bd_generate_expr(syn, env, ctx);
    case CodegenPVM:
        return pvm_generate_expr(syn, env, ctx);
    }
    panic(mv_string("Invalid codegen backend selected."));
}

void generate_type_expr(Syntax* syn, TypeEnv* env, CodegenContext ctx) {
    switch (global_backend) {
    case CodegenDirect:
        return bd_generate_type_expr(syn, env, ctx);
    case CodegenPVM:
        return pvm_generate_type_expr(syn, env, ctx);
    }
    panic(mv_string("Invalid codegen backend selected."));
}

void clear_target(Target target) {
    clear_assembler(target.target);
    clear_assembler(target.code_aux);
    target.data_aux->len = 0;
}

bool can_convert(CType *ctype, PiType *ptype) {
    switch (global_backend) {
    case CodegenDirect:
        return bd_can_convert(ctype, ptype);
    case CodegenPVM:
        not_implemented(mv_string("Unimplemented: can_convert for Codegen PVM"));
    }
    panic(mv_string("Invalid codegen backend selected."));
}

void convert_c_fn(void *cfn, CType *ctype, PiType *ptype, Assembler *ass, Allocator *a, ErrorPoint *point) {
    switch (global_backend) {
    case CodegenDirect:
        return bd_convert_c_fn(cfn, ctype, ptype, ass, a, point);
    case CodegenPVM:
        return bd_convert_c_fn(cfn, ctype, ptype, ass, a, point);
    }
    panic(mv_string("Invalid codegen backend selected."));
}

bool can_reinterpret(CType *ctype, PiType *ptype) {
    switch (global_backend) {
    case CodegenDirect:
        return bd_can_reinterpret(ctype, ptype);
    case CodegenPVM:
        not_implemented(mv_string("Unimplemented: can_reinterpret for Codegen PVM"));
    }
    panic(mv_string("Invalid codegen backend selected."));
}
