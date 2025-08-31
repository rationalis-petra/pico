#ifndef __PICO_CODEGEN_BACKEND_PVM_CODEGEN_H
#define __PICO_CODEGEN_BACKEND_PVM_CODEGEN_H

#include "pico/codegen/codegen.h"

LinkData pvm_generate_toplevel(TopLevel top, Environment* env, Target target, Allocator* a, ErrorPoint* point);

LinkData pvm_generate_expr(Syntax* syn, Environment* env, Target target, Allocator* a, ErrorPoint* point);

void pvm_generate_type_expr(Syntax* syn, TypeEnv* env, Target target, Allocator* a, ErrorPoint* point);

#endif
