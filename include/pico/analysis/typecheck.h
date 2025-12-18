#ifndef __PICO_ANALYSIS_TYPECHECK_H
#define __PICO_ANALYSIS_TYPECHECK_H

#include "components/logging/structured_logging.h"

#include "pico/data/error.h"
#include "pico/binding/environment.h"
#include "pico/codegen/codegen.h"
#include "pico/syntax/syntax.h"

typedef struct {
    Allocator* a;
    PiAllocator* pia;
    PiErrorPoint* point;
    Target target;
    Logger* logger;
} TypeCheckContext;

void type_check(TopLevel* untyped, Environment* env, TypeCheckContext ctx);

#endif
