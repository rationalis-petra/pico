#ifndef __COMPONENTS_PVM_PVM_H
#define __COMPONENTS_PVM_PVM_H

#include <stdint.h>
#include <stdbool.h>

#include "data/meta/array_header.h"
#include "platform/memory/allocator.h"


/* Polymorphic Virutal Machine
 * ----------------------------
 * This header contains primarily the types for PVM
 * 
 * Builder
 * ------
 *
 * Generate
 * ------
 */

typedef enum {
    Reg
} RegisterAllocAlg;

typedef struct {
    RegisterAllocAlg raa;
} PassSet;

typedef struct {
    Allocator* alloc;
    PassSet set;
    void* tape;
} PVMContext;

PVMContext* mk_pvm_context(PassSet set, Allocator* a);

/* PVM Type
 */
typedef enum { PKind, PVar, PProc, PExists, PInt, PFloat, PPtr, POpaque } PVMSort;

typedef struct {
    uint32_t type_start;
    uint32_t type_end;
    uint32_t arg_start;
    uint32_t arg_end;
    uint32_t body;
} PVMProc;

typedef struct {
    uint32_t type_start;
    uint32_t type_end;
    uint32_t arg_start;
    uint32_t arg_end;
    uint32_t body;
} PVMExists;

typedef struct {
    bool is_packed;
    uint32_t type_start;
    uint32_t type_end;
} PVMStruct;

typedef struct {
    PVMSort sort;
    union {
        PVMProc proc;
        PVMProc exists;
        PVMStruct structure;
        uint16_t prim_size;
    };
} PVMTypeEntry;

typedef struct {
    Allocator* alloc;
    PVMTypeEntry* tape;
    size_t len;
    size_t capacity;
} PVMType;

/* Term: 
 * A term represents an expression. In particular, when compiled, a term will
 * emit to a target:
 *   - The code to generate, which will leave the value of an expression on the stack
 *   - Any functions, which are captured in the 'code' segment
 *   - nt data, which is captured in a 'data' segment
 */
typedef struct {
} PVMTerm;

/* Function:
 *  A collection of basic-blocks, with an entry-point.
 *  
 */
typedef struct {
    uint32_t entry_point;
} Function;


typedef struct {
    
} PVMVariable;

typedef enum { Constant, Variable } PVMOperandType;
typedef struct {
    PVMType type; 
    PVMOperandType sort;
    union {
        uint64_t const_bytes;
        PVMVariable var;
    };
} PVMOperand; 

typedef enum { PArith, PCmp, PLiteral } PVMInstrSort;
typedef enum {
    PVMAdd,
    PVMSub,
    PVMMul,
    PVMDiv,
    PVMShiftL,
    PVMShiftR,
} ArithmeticOperator;

typedef enum {
    PVMEq,
    PVMLess,
    PVMGreater,
    PVMLessOrEq,
    PVMGreaterOrEq,
} ComparisonOperator;

typedef struct {
    PVMInstrSort sort;
    union {
        ArithmeticOperator arith;
        ComparisonOperator cmp;
    };
    union {
        struct {
            PVMOperand arg1;
            PVMOperand arg2;
        };
        // TODO: operations like GEP, which have a type/projection set/array
    };
    PVMVariable asssignee; 
} PVMInstruction; 
ARRAY_HEADER_TYPE(PVMInstruction, Instr)


// Block
// A block is a linear sequence of instructions. 
//
typedef struct {
    uint16_t id;
    InstrArray instructions;
    // Analysis information
    // U32Array preds;
} Block; 

typedef struct {
    PVMProc type; 
    void* address;
} PVMFuncHandle;

typedef enum {
    CallPVM,
    CallSysV,
    CallWin64,
} CallingConvention;

// TODO: consider how the PVM would integrate with a JIT?
//   - Should PVM have modules?
//   - Matching PVM code to module definitions?
//     - Inlining.
//     - Constant propagation.
//     - Propagating definitions forward.
//   - Structures
//     - alloca, or otherwise?
// 



#endif
