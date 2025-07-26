#ifndef __PICO_VALUES_ARRAY_H
#define __PICO_VALUES_ARRAY_H

#include <stdint.h>
#include "pretty/document.h"

typedef struct {
    uint64_t len;
    uint64_t* data;
} Shape;

typedef struct {
    Shape shape;
    void* data;
} Array;

void free_array(Array* a);

typedef Document* (*print_element)(void *data, void *context, Allocator* a);

typedef struct {
    print_element print_elem;
    void* context;
} PrettyElem;

Document* pretty_array(Array arr, uint64_t elem_size, PrettyElem pelem, Allocator* a);

#endif
