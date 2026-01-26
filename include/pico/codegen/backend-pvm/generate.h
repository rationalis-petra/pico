#ifndef __PICO_CODEGEN_BACKEND_PVM_CODEGEN_H
#define __PICO_CODEGEN_BACKEND_PVM_CODEGEN_H

#include "pico/codegen/codegen.h"

LinkData pvm_generate_toplevel(TopLevel top, Environment* env, CodegenContext ctx);

LinkData pvm_generate_expr(Syntax* syn, Environment* env, CodegenContext ctx);

void pvm_generate_type_expr(Syntax* syn, TypeEnv* env, CodegenContext ctx);

#endif
