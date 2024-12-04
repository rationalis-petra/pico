#ifndef __DATA_BLOCK_H
#define __DATA_BLOCK_H

#include <stddef.h>

typedef struct {
    void* memory;
    size_t size;
} Block;

#endif
