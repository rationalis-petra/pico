#ifndef __PICO_CODEGEN_CODEGEN_H
#define __PICO_CODEGEN_CODEGEN_H

#include "assembler/assembler.h"
#include "pico/binding/environment.h"
#include "pico/syntax/syntax.h"

// For Now, Generate Assembly directly from syntax!

// generate some assembly that can be called into
result generate_toplevel(syntax syn, environment* env, assembler* ass, allocator a);

//result generate_expr(syntax syn, environment* env, assembler* ass, allocator a);


#endif
