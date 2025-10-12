#ifndef __PICO_CODEGEN_POLYMORPHIC_H
#define __PICO_CODEGEN_POLYMORPHIC_H

#include "pico/binding/address_env.h"
#include "pico/syntax/syntax.h"
#include "pico/codegen/codegen.h"
#include "pico/codegen/backend-direct/internal.h"

void generate_polymorphic(SymbolArray arr, Syntax syn, AddressEnv* env, Target target, InternalLinkData* links, Allocator* a, ErrorPoint* point);

// Type Info
void generate_pi_type(PiType* type, AddressEnv* env, Assembler* ass, Allocator* a, ErrorPoint* point);
void generate_align_to(Regname sz, Regname align, Assembler* ass, Allocator* a, ErrorPoint* point);
void generate_stack_size_of(Regname dest, PiType* type, AddressEnv* env, Assembler* ass, Allocator* a, ErrorPoint* point);
void generate_offset_of(Regname dest, Symbol field, SymPtrAMap fields, AddressEnv* env, Assembler* ass, Allocator* a, ErrorPoint* point);
void generate_variant_size_of(Regname dest, PtrArray* types, AddressEnv* env, Assembler* ass, Allocator* a, ErrorPoint* point);
void generate_variant_align_of(Regname dest, PtrArray* types, AddressEnv* env, Assembler* ass, Allocator* a, ErrorPoint* point);
void generate_variant_stack_size_of(Regname dest, PtrArray* types, AddressEnv* env, Assembler* ass, Allocator* a, ErrorPoint* point);
U8Array free_registers(U8Array inputs, Allocator* a);

// Movement
void generate_poly_move(Location dest, Location src, Location size, Assembler* ass, Allocator* a, ErrorPoint* point);
void generate_poly_stack_move(Location dest_offset, Location src_offset, Location size, Assembler* ass, Allocator* a, ErrorPoint* point);
void generate_poly_copy_from_base(size_t dest, size_t src, Location size, Assembler* ass, Allocator* a, ErrorPoint* point);

#endif
