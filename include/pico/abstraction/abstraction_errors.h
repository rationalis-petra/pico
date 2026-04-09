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

_Noreturn void proc_tyformer_incorrect_numterms(RawTree raw, AbstractionICtx ctx);



#endif
