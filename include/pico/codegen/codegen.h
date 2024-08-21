#ifndef __PICO_CODEGEN_CODEGEN_H
#define __PICO_CODEGEN_CODEGEN_H

#include "data/result.h"

#include "assembler/assembler.h"

#include "pico/binding/environment.h"
#include "pico/syntax/syntax.h"
#include "pico/data/sym_sarr_amap.h"

typedef struct gen_result {
    Result_t type;
    string error_message;
    sym_sarr_amap backlinks;
} gen_result;

gen_result generate_toplevel(toplevel top, environment* env, assembler* ass, allocator a);

gen_result generate_expr(syntax syn, environment* env, assembler* ass, allocator a);

#endif
