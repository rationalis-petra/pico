#ifndef __PICO_CODEGEN_POLYMORPHIC_H
#define __PICO_CODEGEN_POLYMORPHIC_H

#include "data/result.h"

#include "assembler/assembler.h"

#include "pico/binding/address_env.h"
#include "pico/syntax/syntax.h"
#include "pico/data/sym_sarr_amap.h"

void generate_polymorphic(SymbolArray arr, Syntax syn, AddressEnv* env, Assembler* ass, SymSArrAMap* links, Allocator* a, ErrorPoint* point);

#endif
