#ifndef __PICO_BINDING_ADDRESS_ENV_H
#define __PICO_BINDING_ADDRESS_ENV_H

#include "memory/allocator.h"
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

// Push and pop a new local environment to deal with procedure
void address_start_proc(SymbolArray types, SymSizeAssoc vars, AddressEnv* env, Allocator* a);
void address_end_proc(AddressEnv* env, Allocator* a);

// Bind and unbind enum vars: 
// Binding assumes that an enum sits on top of the stack (i.e. at stack_head). 
//   it establishes bindings for the members of the enum, but does not 
// Unbind removes these bindings. Like bind, it does not adjust the stack head.
void address_bind_enum_vars(SymSizeAssoc vars, AddressEnv* env, Allocator* a);
void address_unbind_enum_vars(AddressEnv* env);

// Inform the environment that values have been pushed/popped from the stack.
void address_stack_grow(AddressEnv* env, size_t amount);
void address_stack_shrink(AddressEnv* env, size_t amount);

#endif
