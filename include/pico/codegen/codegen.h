#ifndef __PICO_CODEGEN_CODEGEN_H
#define __PICO_CODEGEN_CODEGEN_H

#include "components/logging/structured_logging.h"

#include "pico/binding/environment.h"
#include "pico/binding/type_env.h"
#include "pico/syntax/syntax.h"

typedef struct {
    U8Array* data_aux;
    Assembler* code_aux;
    Assembler* target;
} Target;

typedef enum {
    CodegenDirect,
    CodegenPVM,
} CodegenBackend;

typedef struct {
    SynTape tape;
    Target target;
    Allocator *a;
    PiAllocator* pia;
    ErrorPoint* point;
    Logger* logger;
} CodegenContext;

void init_codegen(CodegenBackend backend, Allocator* alloc);
void teardown_codegen();

/**
 * Generate a toplevel expression:   
 * • In the executable segment, 
 * • In the code segment 
 * • In the data segment 
 */
LinkData generate_toplevel(TopLevel top, Environment* env, CodegenContext ctx);

LinkData generate_expr(SynRef syn, Environment* env, CodegenContext ctx);

void generate_type_expr(SynRef syn, TypeEnv* env, CodegenContext ctx);

/**
 * Generate a set of closures for an instance. Given both a set of closure data
 * and a set of closure targets, together with the types and implicits provided
 * to the instance, generate:
 * • A code segment where all generated code lives
 * • An array of offsets, indicating the start of each closure (function)
 * 
 */
typedef struct {
  ClosureLinkArray links;
  U8Array code_segment;
  SymPtrAssoc type_binds;
  U64Array type_encodings;
  PtrArray implicits;
} ClosureGenData;

typedef struct {
    U64Array closure_starts;
} InstanceClosures;

InstanceClosures generate_instance_closures(Assembler* target, ClosureGenData data, Allocator* a);

/**
 * Clear all data from target segments, resetting them to 0. No memory
 * allocation or deallocation is done.
 */
void clear_target(Target target);

// Foreign Function
bool can_convert(CType* ctype, PiType* ptype); 

void convert_c_fn(void* cfn, CType* ctype, PiType* ptype, Assembler* ass, Allocator* a, ErrorPoint* point); 

bool can_reinterpret(CType* ctype, PiType* ptype); 

#endif
