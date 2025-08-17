#ifndef __PICO_ANALYSIS_TYPECHECK_H
#define __PICO_ANALYSIS_TYPECHECK_H

#include "pico/data/error.h"
#include "pico/binding/environment.h"
#include "pico/codegen/codegen.h"
#include "pico/syntax/syntax.h"

typedef struct {
    Allocator* a;
    PiErrorPoint* point;
    Target target;
} TypeCheckContext;

void type_check(TopLevel* untyped, Environment* env, TypeCheckContext ctx);

#endif
