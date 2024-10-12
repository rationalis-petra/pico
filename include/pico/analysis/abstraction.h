#ifndef __PICO_ANALYSIS_ABSTRACTION_H
#define __PICO_ANALYSIS_ABSTRACTION_H

#include "platform/error.h"
#include "pico/binding/environment.h"
#include "pico/syntax/concrete.h"
#include "pico/syntax/syntax.h"

TopLevel abstract(RawTree raw, Environment* env, Allocator* a, ErrorPoint* point);
Syntax* abstract_expr(RawTree raw, Environment* env, Allocator* a, ErrorPoint* point);

#endif
