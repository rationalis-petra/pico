#ifndef __PICO_CODEGEN_CODEGEN_H
#define __PICO_CODEGEN_CODEGEN_H

#include "assembler/assembler.h"
#include "pico/syntax/syntax.h"

// For Now, Generate Assembly directly from syntax!

asm_result generate(syntax syn, assembler* ass, allocator a);


#endif
