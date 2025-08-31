#ifndef __COMPONENTS_PVM_GENERATE_H
#define __COMPONENTS_PVM_GENERATE_H

#include "data/array.h"

#include "components/assembler/assembler.h"
#include "components/assembler/link_data.h"
#include "components/pvm/pvm.h"

/* Polymorphic Virutal Machine: Code Generation
 * ----------------------------
 * Code generation for PVM needs to be able to account for:
 * - Producing linkage information (what functions link to what)
 *   addresses.
 * - Emitting to an executable, text and data segment
 * 
 * Potential errors e.g. from malformed data, or something that the target does
 * not support. 
 */

typedef struct {
    PVMContext *context;
    ErrorPoint *on_error;
    Allocator* err_alloc;
} PVMGenContext;

typedef struct {
    // Code generation target
    U8Array* data_segment;
    Assembler* code_segment;
    Assembler* exec_segment;
} PVMTarget;

void generate_term(Term* term, PVMTarget* target, LinkData* links, PVMGenContext context);

void generate_proc(PVMProc* proc, PVMTarget* target, LinkData* links, PVMGenContext context);

#endif
