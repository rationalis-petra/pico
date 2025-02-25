#ifndef __PICO_CODEGEN_CODEGEN_H
#define __PICO_CODEGEN_CODEGEN_H

#include "pico/binding/environment.h"
#include "pico/binding/type_env.h"
#include "pico/syntax/syntax.h"

typedef struct {
    U8Array* data_aux;
    Assembler* code_aux;
    Assembler* target;
} Target;

LinkData generate_toplevel(TopLevel top, Environment* env, Target target, Allocator* a, ErrorPoint* point);

LinkData generate_expr(Syntax* syn, Environment* env, Target target, Allocator* a, ErrorPoint* point);

void generate_type_expr(Syntax* syn, TypeEnv* env, Target target, Allocator* a, ErrorPoint* point);

#endif
