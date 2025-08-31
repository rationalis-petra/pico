#include "platform/signals.h"

#include "components/pvm/generate.h"

// Polymorphic Virtual Machine codegen:
// Strategy:
//   1. Apply optimisation passes  
//   2. Allocate registers 
//   3. 
// Expressions: 
//

// The register allocation algorithm's job is to create a map
//  which assignes names (variables) to either registers OR 
//  offsets (from RBP)


// The register allocation algorithm keeps RAX and RDX free for temporary variables.
//  (note: both are needed for some arithmetic operations like division)

// Internal codegen
static void generate_instruction(PVMInstruction instruction, PVMTarget *target, LinkData *links, PVMGenContext context);

void generate_term(Term *term, PVMTarget *target, LinkData *links, PVMGenContext context) {
}

void generate_proc(PVMProc *proc, PVMTarget *target, LinkData *links, PVMGenContext context) {
}

static void generate_instruction(PVMInstruction instruction, PVMTarget *target,
                                 LinkData *links, PVMGenContext context) {
    switch (instruction.sort) {
    case PArith: {
        panic(mv_string("PVM: generate_instruction does not support arithmetic instructions (yet)"));
    }
    case PCmp: {
        panic(mv_string("PVM: generate_instruction does not support comparison instructions (yet)"));
    }
    case PLiteral:
        panic(mv_string("PVM: generate_instruction does not support literals (yet)"));
    }

    // default
    panic(mv_string("PVM: generate_instruction received a bad instruction (invalid sort)"));
}
