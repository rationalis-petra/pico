#ifndef __PICO_VALUES_TYPES_H
#define __PICO_VALUES_TYPES_H

#include "data/array.h"
#include "pretty/document.h"

/* Basic types in pico typesystem
 */

typedef struct pi_type pi_type;
typedef struct uvar_generator uvar_generator;

typedef enum prim_type {
    Int_64,
    TFormer,
} prim_type;


typedef enum pi_type_t {
    TPrim,
    TProc,

    // Special sort: unification variable
    TUVar
} pi_type_t;

typedef struct proc_type {
    ptr_array args;
    pi_type* ret;
} proc_type;

typedef struct uvar_type {
    uint64_t id;
    pi_type* subst;
} uvar_type;

typedef struct pi_type {
    pi_type_t sort; 
    union {
        prim_type prim;
        proc_type proc;
        uvar_type* uvar;
    };
} pi_type;


document* pretty_type(pi_type* type, allocator a);
void delete_pi_type(pi_type t, allocator a);
size_t pi_size_of(pi_type t);

pi_type* mk_uvar(uvar_generator* gen, allocator a);
uvar_generator* mk_gen(allocator a);
void delete_gen(uvar_generator* gen, allocator a);

#endif
