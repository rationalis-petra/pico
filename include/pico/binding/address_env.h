#ifndef __PICO_BINDING_ADDRESS_ENV_H
#define __PICO_BINDING_ADDRESS_ENV_H

#include "platform/memory/allocator.h"
#include "pico/data/sym_size_assoc.h"
#include "pico/binding/environment.h"

typedef struct AddressEnv AddressEnv;


/* Address Entry 
 * -------------
 * This is the type returned by address environment lookups. It is an ADT with
 * the following outcomes:
 * • Local Direct: The address entry contains a int which denotes the offset
 *   (from RBP) of the local stack variable. Hence, the variable is located at
 *   [RBP + offset]
 * • Local Indirect: The address entry contains an int which denotes the offset
 *   (from RBP) of another offset. This is used in polymorphic functions.
 *   [RBP + [RBP + offset]]
 * • Global: The address entry contains a pointer to a fixed location, which
 *   holds the value of the variable.
 */
typedef enum {
    ALocalDirect,
    ALocalIndirect,
    AGlobal,
    ATypeVar,
    ANotFound,
    ATooManyLocals,
} AddressEntry_t;

typedef struct {
    AddressEntry_t type;
    union {
        void* value;
        int8_t stack_offset;
    };
} AddressEntry;

typedef struct {
    Result_t type;
    uint32_t stack_offset;
} LabelEntry;

AddressEnv* mk_address_env(Environment* env, Symbol* sym, Allocator* a);
void delete_address_env(AddressEnv* env, Allocator* a);

/* Address environment interface
 * Lookups return either:
 * • An error
 * • A local variable (returned as an offset from RBP)
 * • A global variable (returned as a pointer)
 *
 *
 * To calculate this, the environment needs:
 * • To know when a value is pushed to the stack (and of what size).
 * • To know when a value is popped from the stack (and of what size).
 * • To know when a new value of RBP is saved (currently only upon entering a function) .
 * • To know when any local variables are bound.
 */

AddressEntry address_env_lookup(Symbol s, AddressEnv* env);
LabelEntry label_env_lookup(Symbol s, AddressEnv* env);

// Push and pop a new local environment to deal with procedure
void address_start_proc(SymSizeAssoc implicits, SymSizeAssoc vars, AddressEnv* env, Allocator* a);
void address_end_proc(AddressEnv* env, Allocator* a);

void address_start_poly(SymbolArray types, SymbolArray args, AddressEnv* env, Allocator* a);
void address_end_poly(AddressEnv* env, Allocator* a);


//------------------------------------------------------------------------------
// Manipulate an (extant) local environment: push and pop ariables, or 
//  bind values associated with specific forms/values (e.g. enums/structs)
//------------------------------------------------------------------------------

void address_bind_type(Symbol s, AddressEnv* env);

void address_bind_relative(Symbol s, size_t offset, AddressEnv* env);
void address_pop_n(size_t n, AddressEnv* env);
void address_pop(AddressEnv* env);

// Bind and unbind enum vars: 
// Binding assumes that an enum sits on top of the stack (i.e. at stack_head). 
//   it establishes bindings for the members of the enum, but does not 
// Unbind removes these bindings. Like bind, it does not adjust the stack head.
void address_bind_enum_vars(SymSizeAssoc vars, AddressEnv* env);
void address_unbind_enum_vars(AddressEnv* env);

// Start/end 
void address_start_labels(SymbolArray labels, AddressEnv* env);
void address_end_labels(AddressEnv* env);

// Inform the environment that values have been pushed/popped from the stack.
void address_stack_grow(AddressEnv* env, size_t amount);
void address_stack_shrink(AddressEnv* env, size_t amount);

#endif
