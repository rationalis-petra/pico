#ifndef __COMPONENTS_ASSEMBLER_ASSEMBLER_H
#define __COMPONENTS_ASSEMBLER_ASSEMBLER_H

#include "platform/error.h"

#include "data/array.h"
#include "components/pretty/document.h"

/* The assembler writes encoded instructions directly to a byte-array. */

typedef enum {
    SSE = 0b1,
} CPUFeatureFlags;

CPUFeatureFlags current_cpu_feature_flags();

typedef struct Assembler Assembler; 
struct Assembler* mk_assembler(CPUFeatureFlags flags, Allocator* a);
void delete_assembler(Assembler* assembler);
void clear_assembler(Assembler* assembler);

U8Array get_instructions(Assembler* assembler);

// Get and set the current write head of the assembler.
size_t get_pos(Assembler* assembler);
void set_pos(Assembler* assembler, size_t pos);

// Backlink setting: 
void set_ptr_backlink(Assembler* assembler, size_t backlink, void* val);
void set_i32_backlink(Assembler* assembler, size_t backlink, int32_t val);
void set_u32_backlink(Assembler* assembler, size_t backlink, uint32_t val);

Document* pretty_assembler(Assembler* assembler, Allocator* a);

// Integral operations
typedef enum {
    // ------------------
    //  Arithmetic
    // ------------------
    Add,
    Sub,
    Cmp,

    AddSS,
    AddSD,

    SubSS,
    SubSD,

    MulSS,
    MulSD,

    DivSS,
    DivSD,

    // ------------------
    //  Logic
    // ------------------
    And,
    Or,
    Xor,

    // ------------------
    //  Bit Manipulation
    // ------------------
    SHL, // Left shift
    SHR, // Right shift
    XCHG, // Exchange 2 bytes of a 2-byte register.

    // ------------------
    //  Memory & Registers
    // ------------------
    Mov,   // p 769.
    MovSS, // p 857.
    MovSD, // p 848.
    LEA,

    // ------------------
    //  Conditional Moves
    // ------------------
    CMovE, 
    CMovB, // Move if below (unsigned)
    CMovA, // Move if above (unsigned)
    CMovL, // Move if lesser (signed)
    CMovG, // Move if greater (signed)

    // Conversions
    CvtSD2SS, // p 403.

    // ------------------
    //  Meta
    // ------------------
    Binary_Op_Count,
} BinaryOp;

typedef enum {
    // ------------------
    //  Functions
    // ------------------
    Call,
    Push, // p 1250.
    Pop,  //

    // ------------------
    //  Jumps
    // ------------------
    JE, // p658, jump if equal


    JNE, // p658, jump if equal
    JMP, // p663, unconditional jump

    // ------------------------
    //  Set Byte based on flag & Logic
    // ------------------------
    Not,
    SetE,
    SetNE,
    SetB,  // Set below   (unsigned)
    SetBE, // Set below or equal
    SetA,  // Set above   (unsigned)
    SetAE, // Set above or equal
    SetL,  // Set lesser  (signed)
    SetLE, // Set lesser or equal
    SetG,  // Set greater (signed)
    SetGE, // Set greater or equal

    // ------------------
    //  Arithmetic
    // ------------------
    Neg,
    Mul,
    Div,
    IMul,
    IDiv,

    // Bit manipulation
    BSwap, // Reverse bytes in a 4 or 8 byte register. 
    SHLCL, // Shift target register left by amount in CL
    SHRCL, // Shift target register right by amount in CL

    // ------------------
    //  Meta
    // ------------------
    Unary_Op_Count,
} UnaryOp;

typedef enum {
    Ret,
    CQO,
    Nullary_Op_Count,
} NullaryOp;

typedef enum {
    RAX = 0b0000,
    RCX = 0b0001,
    RDX = 0b0010,
    RBX = 0b0011,
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
    R15 = 0b1111,

    XMM0  = 0b10000,
    XMM1  = 0b10001,
    XMM2  = 0b10010,
    XMM3  = 0b10011,
    XMM4  = 0b10100,
    XMM5  = 0b10101,
    XMM6  = 0b10110,
    XMM7  = 0b10111,
    XMM8  = 0b11000,
    XMM9  = 0b11001,
    XMM10 = 0b11010,
    XMM11 = 0b11011,
    XMM12 = 0b11100,
    XMM13 = 0b11101,
    XMM14 = 0b11110,
    XMM15 = 0b11111,

    // Special! See RIP-Relative addressing, p50 of the Intel Manual Vol. 2
    RIP = 0b100101,
} Regname;

typedef enum {
    Dest_Register,
    Dest_Deref,
    Dest_Immediate,
} Dest_t;

typedef enum {
    sz_8,
    sz_16,
    sz_32,
    sz_64
} LocationSize;

// Location: Can be one of
// + Register
// + [Register + Offset]
// + [Register + Index * Size + Offset ]
// + Immediate
typedef struct {
    Dest_t type;
    LocationSize sz;
    Regname reg;
    union {
        int8_t immediate_8;
        int16_t immediate_16;
        int32_t immediate_32;
        int64_t immediate_64;

        uint8_t immediate_bytes[8];
    };

    Regname index;
    bool is_scale;  
    uint8_t scale;

    uint8_t disp_sz;  
    union {
        int8_t disp_8;
        int32_t disp_32;
        uint8_t disp_bytes[4];
    };

} Location;

// Location Constructors 
Location reg(Regname name, LocationSize sz);
Location rref8(Regname name, int64_t offset, LocationSize sz);
Location rref32(Regname name, int32_t offset, LocationSize sz);
Location sib(Regname base, Regname index, int64_t scale, LocationSize sz);
Location sib8(Regname base, Regname index, int64_t scale, int64_t displacement, LocationSize sz);
Location sib32(Regname base, Regname index, int64_t scale, int32_t displacement, LocationSize sz);
Location imm8(int64_t immediate);
Location imm16(int16_t immediate);
Location imm32(int32_t immediate);
Location imm64(int64_t immediate);

// Result 
typedef struct AsmResult {
    size_t backlink; // backlink to immediate (if it exists)
} AsmResult;

AsmResult build_binary_op(BinaryOp op, Location dest, Location src, Assembler* assembeler, Allocator* err_allocator, ErrorPoint* point);
AsmResult build_unary_op(UnaryOp op, Location loc, Assembler* assembler,Allocator* err_allocator, ErrorPoint* point);
AsmResult build_nullary_op(NullaryOp op, Assembler* assembler, Allocator* err_allocator, ErrorPoint* point);

void init_asm();

// Utility & Pretty
Document* pretty_register(Regname reg, LocationSize sz, Allocator* a);
Document* pretty_location(Location loc, Allocator* a);
Document* pretty_binary_op(BinaryOp op, Allocator* a);
Document* pretty_unary_op(UnaryOp op, Allocator* a);
Document* pretty_nullary_op(NullaryOp op, Allocator* a);

Document* pretty_binary_instruction(BinaryOp op, Location dest, Location src, Allocator* a);
Document* pretty_unary_instruction(UnaryOp op, Location loc, Allocator* a);

#endif
