#ifndef __PICO_CODEGEN_CODEGEN_H
#define __PICO_CODEGEN_CODEGEN_H

#include "assembler/assembler.h"
#include "pico/binding/environment.h"
#include "pico/syntax/syntax.h"

// For Now, Generate Assembly directly from syntax!

result generate(syntax syn, environment* env, assembler* ass, allocator a);


#endif
