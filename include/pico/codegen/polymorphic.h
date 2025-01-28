#ifndef __PICO_CODEGEN_POLYMORPHIC_H
#define __PICO_CODEGEN_POLYMORPHIC_H

#include "pico/binding/address_env.h"
#include "pico/syntax/syntax.h"
#include "pico/codegen/codegen.h"
#include "pico/codegen/internal.h"

void generate_polymorphic(SymbolArray arr, Syntax syn, AddressEnv* env, Target target, InternalLinkData* links, Allocator* a, ErrorPoint* point);

#endif
