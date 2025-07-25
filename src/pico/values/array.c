#include "pico/stdlib/extra.h"
#include "pico/values/array.h"

void free_array(Array* arr) {
    Allocator perm = get_std_perm_allocator();
    mem_free(arr->data, &perm);
    mem_free(arr->shape.data, &perm);
}

