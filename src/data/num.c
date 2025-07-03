#include <stddef.h>
#include "data/num.h"

void set_unaligned_ptr(void **dest, void *src) {
    char* cdest = (char*)dest;
    char* csrc = (char*)&src;
    for (size_t i = 0; i < sizeof(void*); i++) {
        cdest[i] = csrc[i];
    }
}

void set_unaligned_u32(uint32_t* dest, uint32_t src) {
    char* cdest = (char*)dest;
    char* csrc = (char*)&src;
    for (size_t i = 0; i < sizeof(int32_t); i++) {
        cdest[i] = csrc[i];
    }
}

void set_unaligned_i32(int32_t* dest, int32_t src) {
    char* cdest = (char*)dest;
    char* csrc = (char*)&src;
    for (size_t i = 0; i < sizeof(int32_t); i++) {
        cdest[i] = csrc[i];
    }
}
