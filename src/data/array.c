#include <string.h>

#include "data/meta/array_impl.h"
#include "data/array.h"

ARRAY_IMPL(void*, ptr, Ptr)

ARRAY_IMPL(uint8_t, u8, U8)

void add_u8_chunk(uint8_t* chunk, size_t memsize, U8Array* array) {
    // TODO (BUG) assumes sizeof(uint8_t) == 1.
    if (array->len + memsize < array->size) {
        memcpy(array->data + array->len, chunk, memsize);
        array->len += memsize;
    } else {
        // TODO (BUG): realloc - must account for failure (return NULL)
        size_t new_size = memsize + array->size < 2 * memsize 
            ? 2 * memsize 
            : memsize + array->size;
        array->data = mem_realloc(array->data, new_size, &array->gpa);
        array->size = new_size;

        memcpy(array->data + array->len, chunk, memsize);
        array->len += memsize;
    }
}

ARRAY_IMPL(uint32_t, u32, U32)

ARRAY_IMPL(uint64_t, u64, U64)

ARRAY_IMPL(size_t, size, Size)
