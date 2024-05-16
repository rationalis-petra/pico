#include "pico/values/types.h"

void delete_pi_ptr(void* t, allocator a) {
    delete_pi_type(*(pi_type*)t, a);
    mem_free(t, a);
}

void delete_pi_type(pi_type t, allocator a) {
    switch(t.sort) {
    case TProc:
        delete_pi_ptr(t.proc.ret, a);
        delete_ptr_array(t.proc.args, &delete_pi_ptr, a);

    case TFormer:
    case TPrim:
        // Do nothing!
        break;
            
    }
}
