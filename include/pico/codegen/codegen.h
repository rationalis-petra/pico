#ifndef __PICO_CODEGEN_CODEGEN_H
#define __PICO_CODEGEN_CODEGEN_H

#include "data/result.h"
#include "assembler/assembler.h"
#include "pico/binding/environment.h"
#include "pico/syntax/syntax.h"


// Generate some assembly that can be called into
// This code:
// + 
result generate_toplevel(syntax syn, environment* env, assembler* ass, allocator a);

//result generate_expr(syntax syn, environment* env, assembler* ass, allocator a);


#endif
