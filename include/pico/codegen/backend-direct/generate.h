#ifndef __PICO_CODEGEN_BACKEND_DIRECT_CODEGEN_H
#define __PICO_CODEGEN_BACKEND_DIRECT_CODEGEN_H

#include "pico/codegen/codegen.h"

LinkData bd_generate_toplevel(TopLevel top, Environment* env, Target target, Allocator* a, ErrorPoint* point);

LinkData bd_generate_expr(Syntax* syn, Environment* env, Target target, Allocator* a, ErrorPoint* point);

void bd_generate_type_expr(Syntax* syn, TypeEnv* env, Target target, Allocator* a, ErrorPoint* point);

#endif
