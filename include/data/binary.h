#ifndef __DATA_BINARY_H
#define __DATA_BINARY_H

// Provide tools for manipulating bit-obejcts 

#include <stddef.h>

static inline void set_bit(uint8_t* value, uint8_t index) {
    *value = *value | (1 << index);
}

static inline void unset_bit(uint8_t* value, uint8_t index) {
    *value = *value & (1 << index);
}

#endif
