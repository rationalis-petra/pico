#ifndef __PICO_VALUES_ARRAY_H
#define __PICO_VALUES_ARRAY_H

#include <stdint.h>

typedef struct {
    uint64_t len;
    uint64_t* data;
} Shape;

typedef struct {
    Shape shape;
    uint64_t refcount;
    void* data;
} Array;

void inc_refcount(Array* a);
void dec_refcount(Array* a);

#endif
