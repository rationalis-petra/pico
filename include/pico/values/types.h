#ifndef __PICO_VALUES_TYPES_H
#define __PICO_VALUES_TYPES_H

#include "data/array.h"
#include "pico/values/values.h"

/* Basic types in pico typesystem
 */

typedef struct pi_type pi_type;

typedef enum prim_type {
    Int_64,
    
} prim_type;


typedef enum pi_type_t {
    TFormer,
    TPrim,
    TProc,
} pi_type_t;

typedef struct proc_type {
    ptr_array args;
    pi_type* ret;
} proc_type;

typedef struct pi_type {
    pi_type_t sort; 
    union {
        pi_term_former_t former; 
        prim_type prim;
        proc_type proc;
    };
} pi_type;


void delete_pi_type(pi_type t, allocator a);

#endif
