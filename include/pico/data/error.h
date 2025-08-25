#ifndef __PICO_DATA_ERROR_H
#define __PICO_DATA_ERROR_H

#include "platform/jump.h"
#include "data/stream.h"
#include "components/pretty/document.h"

#include "pico/data/range.h"

typedef struct {
    Document* message;
    Range range;
} PicoError;

typedef struct {
    bool has_many;
    union {
        PicoError error;
        PtrArray errors;
    }; 
} MultiError;

typedef struct {
    volatile MultiError multi;
    jump_buf buf; 
} PiErrorPoint;

_Noreturn void throw_pi_error(PiErrorPoint* point, PicoError err); 
_Noreturn void throw_pi_errors(PiErrorPoint* point, PtrArray errors); 

void display_error(MultiError error, IStream *is, FormattedOStream* cout, const char* filename, Allocator* a);
void display_code_region(String buffer, Range range, const size_t lines_prior, FormattedOStream* os, Allocator* a);

typedef struct {
    void(*fn)(void* ctx);
    void* ctx;
} Hook;

void not_implemented(String message);
void set_not_implemented_hook(Hook hook);
void clear_not_implemented_hook();

#endif
