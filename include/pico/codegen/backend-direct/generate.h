#ifndef __PICO_CODEGEN_BACKEND_DIRECT_CODEGEN_H
#define __PICO_CODEGEN_BACKEND_DIRECT_CODEGEN_H

#include "pico/codegen/codegen.h"

LinkData bd_generate_toplevel(TopLevel top, Environment* env, CodegenContext ctx);

LinkData bd_generate_expr(Syntax* syn, Environment* env, CodegenContext ctx);

void bd_generate_type_expr(Syntax* syn, TypeEnv* env, CodegenContext ctx);

#endif
