#ifndef __ASSEMBLER_ASSEMBLER_H
#define __ASSEMBLER_ASSEMBLER_H


#include "data/result.h"
#include "data/array.h"
#include "pretty/document.h"

/* The assembler writes encoded instructions directly to a byte-array. */

typedef struct Assembler Assembler; 
struct Assembler* mk_assembler(Allocator* a);
void delete_assembler(Assembler* assembler);
void clear_assembler(Assembler* assembler);

U8Array get_instructions(Assembler* assembler);
size_t get_pos(Assembler* assembler);

Document* pretty_assembler(Assembler* assembler, Allocator a);

// Integral operations
typedef enum BinaryOp {
    // ------------------
    //  Arithmetic
    // ------------------
    Add,
    Sub,
    Cmp, // p289. Compare two operands

    // ------------------
    //  Logic
    // ------------------
    And,
    Or,

    // ------------------
    //  Bit Manipulation
    // ------------------
    LShift,
    RShift,

    // ------------------
    //  Memory
    // ------------------
    Mov,   // p 769.
    Binary_Op_Count,
} BinaryOp;

typedef enum UnaryOp {
    // ------------------
    //  Functions
    // ------------------
    Call,
    Push, // p 1250.
    Pop,  // 

    // ------------------
    //  Jumps
    // ------------------
    JE,  // p658, jump if equal


    JNE, // p658, jump if equal
    JMP, // p663, unconditional jump

    // ------------------------
    //  Set Byte based on flag 
    // ------------------------
    SetE,
    SetL,
    SetG,
} UnaryOp;

typedef enum NullaryOp {
    Ret,
} NullaryOp;

typedef enum Regname {
    RAX = 0b0000,
    RBX = 0b0011,
    RCX = 0b0001,
    RDX = 0b0010,
    RSP = 0b0100,
    RBP = 0b0101,
    RSI = 0b0110,
    RDI = 0b0111,
    R8  = 0b1000,
    R9  = 0b1001,
    R10 = 0b1010,
    R11 = 0b1011,
    R12 = 0b1100,
    R13 = 0b1101,
    R14 = 0b1110,
    R15 = 0b1111
} Regname;

typedef enum Dest_t {
    Register,
    Deref,
    Immediate,
} Dest_t;

typedef enum LocationSize {
    sz_8,
    sz_16,
    sz_32,
    sz_64
} LocationSize;

// Location: Can be one of
// + Register
// + [Register + Offset]
// + Immediate
typedef struct Location {
    Dest_t type;
    LocationSize sz;
    Regname reg;
    union {
        int8_t immediate_8;
        int16_t immediate_16;
        int32_t immediate_32;
        int64_t immediate_64;

        int8_t immediate_bytes[8];
    };
    uint8_t disp_sz;  
    union {
        int8_t disp_8;
        int32_t disp_32;
        int8_t disp_bytes[4];
    };

} Location;

// Location Constructors 
Location reg(Regname name);
Location rref(Regname name, int8_t offset);
Location imm8(int8_t immediate);
Location imm16(int16_t immediate);
Location imm32(int32_t immediate);
Location imm64(int64_t immediate);

// Result 
typedef struct asm_result {
    Result_t type;
    union {
        size_t backlink; // backlink to immediate (if it exists)
        String error_message;
    }; 
} asm_result;

asm_result build_binary_op(Assembler* ass, BinaryOp op, Location dest, Location src, Allocator* err_allocator);
asm_result build_unary_op(Assembler* assembler, UnaryOp op, Location loc, Allocator* err_allocator);
asm_result build_nullary_op(Assembler* assembler, NullaryOp op, Allocator err_allocator);

void asm_init();

#endif
