#ifndef __COMPONENTS_PVM_GENERATE_H
#define __COMPONENTS_PVM_GENERATE_H

#include "components/assembler/assembler.h"
#include "components/pvm/pvm.h"

// Generate assembly code from term

void generate_term(Term* term, Assembler* ass, ErrorPoint* on_error, Allocator* err_alloc);

#endif
