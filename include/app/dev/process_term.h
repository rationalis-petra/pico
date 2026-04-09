#ifndef __APP_DEV_PROCESS_TERM
#define __APP_DEV_PROCESS_TERM

#include "data/string.h"
#include "data/option.h"
#include "data/colour.h"
#include "platform/memory/region.h"

#include "pico/values/modular.h"
#include "pico/syntax/syntax.h"

typedef enum {
    PONone       = 0x0, 
    POHighilight = 0x1,
    POErrors     = 0x2,
    POCompletion = 0x4,
} ProcessOptions;

typedef struct {
    Module* current;
    String input;
    size_t cursor_pos;
    ProcessOptions options;
} ProcessInput;

typedef struct {
    PtrArray* completions;
    PtrArray* errors;
    
    Option_t has_term;
    SynTape tape;
    TopLevel checked_term;
} ProcessResult;

ProcessResult process_term(ProcessInput input, RegionAllocator* region);

#endif
