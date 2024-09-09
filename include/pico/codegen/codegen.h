#ifndef __PICO_CODEGEN_CODEGEN_H
#define __PICO_CODEGEN_CODEGEN_H

#include "data/result.h"

#include "assembler/assembler.h"

#include "pico/binding/environment.h"
#include "pico/syntax/syntax.h"
#include "pico/data/sym_sarr_amap.h"

typedef struct GenResult {
    Result_t type;
    String error_message;
    SymSArrAMap backlinks;
} GenResult;

GenResult generate_toplevel(TopLevel top, Environment* env, Assembler* ass, Allocator* a);

GenResult generate_expr(Syntax syn, Environment* env, Assembler* ass, Allocator* a);

#endif
