#ifndef __PICO_DATA_BULID_ERROR_H
#define __PICO_DATA_BULID_ERROR_H

#include "platform/jump.h"
#include "components/pretty/document.h"

#include "pico/values/modular.h"

typedef struct {
    Document* message;
    Module* module;
    Symbol definition;
} BuildError;

typedef struct {
    bool has_many;
    union {
        BuildError error;
        PtrArray errors;
    }; 
} MultiBuildError;

typedef struct {
    volatile MultiBuildError multi;
    jump_buf buf; 
} BuildErrorPoint;

void display_build_error(MultiBuildError error, FormattedOStream* fos, Allocator* a);

_Noreturn void throw_build_error(BuildErrorPoint* point, BuildError err); 
_Noreturn void throw_build_errors(BuildErrorPoint* point, PtrArray errors); 


#endif
