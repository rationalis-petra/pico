#ifndef __PICO_ABSTRACTION_ERRORS_H
#define __PICO_ABSTRACTION_ERRORS_H

#include "pico/data/error.h"
#include "pico/binding/shadow_env.h"
#include "pico/syntax/concrete.h"
#include "pico/syntax/syntax.h"

typedef struct {
    Allocator* gpa;
    SynTape tape;
    PiAllocator* pia;
    ShadowEnv* env;
    PiErrorPoint* point;
    void* vstack_memory_ptr;
    void* dynamic_memory_ptr;
} AbstractionICtx;


// ------------------------------------------------------------
//   Expression/Value Formers
// ------------------------------------------------------------
 
// Arrays
_Noreturn void array_incorrect_numterms(RawTree raw, size_t expected, AbstractionICtx ctx);
_Noreturn void array_incorrect_dimtype(RawTree raw, AbstractionICtx ctx);
_Noreturn void array_incorrect_format(RawTree raw, AbstractionICtx ctx);
_Noreturn void array_incorrect_size(RawTree raw, uint64_t expected, AbstractionICtx ctx);

_Noreturn void array_elt_incorrect_numterms(RawTree raw, AbstractionICtx ctx);

// Structures
_Noreturn void struct_bad_fdesc_type(RawTree raw, AbstractionICtx ctx);
_Noreturn void struct_bad_fdesc_len(RawTree raw, AbstractionICtx ctx);
_Noreturn void struct_bad_fdesc_fieldname(RawTree raw, AbstractionICtx ctx);
_Noreturn void struct_duplicate_fieldname(RawTree raw, Symbol fname, AbstractionICtx ctx);

// ------------------------------------------------------------
//   Type Formers
// ------------------------------------------------------------

_Noreturn void proc_tyformer_incorrect_numterms(RawTree raw, AbstractionICtx ctx);

_Noreturn void array_tyformer_incorrect_numterms(RawTree raw, AbstractionICtx ctx);
_Noreturn void array_tyformer_incorrect_dimformat(RawTree raw, AbstractionICtx ctx);
_Noreturn void array_tyformer_dim_not_number(RawTree raw, AbstractionICtx ctx);



#endif
