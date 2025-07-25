#ifndef __PICO_VALUES_ARRAY_H
#define __PICO_VALUES_ARRAY_H

#include <stdint.h>

typedef struct {
    uint64_t len;
    uint64_t* data;
} Shape;

typedef struct {
    Shape shape;
    void* data;
} Array;

void free_array(Array* a);

#endif
