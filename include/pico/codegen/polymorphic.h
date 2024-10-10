#ifndef __PICO_CODEGEN_POLYMORPHIC_H
#define __PICO_CODEGEN_POLYMORPHIC_H

#include "data/result.h"

#include "assembler/assembler.h"

#include "pico/binding/address_env.h"
#include "pico/syntax/syntax.h"
#include "pico/data/sym_sarr_amap.h"

AsmResult generate_polymorphic(Syntax syn, AddressEnv* env, Assembler* ass, SymSArrAMap* links, Allocator* a);

#endif
