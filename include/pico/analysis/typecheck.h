#ifndef __PICO_ANALYSIS_TYPECHECK_H
#define __PICO_ANALYSIS_TYPECHECK_H

#include "platform/error.h"
#include "pico/binding/environment.h"
#include "pico/syntax/syntax.h"

void type_check(TopLevel* untyped, Environment* env, Allocator* a, ErrorPoint* point);

#endif
