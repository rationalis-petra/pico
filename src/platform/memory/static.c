#include <stdlib.h>
#include <stdbool.h>

#include "platform/memory/allocator.h"
#include "platform/memory/static.h"

typedef struct {
    void* head;
    size_t remainder;
} StaticContext;

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-parameter"

void* static_malloc(size_t memsize, void* ctx) {
    StaticContext *context = ctx;
    if (memsize <= context->remainder) {
        void* out = context->head;
        context->remainder -= memsize;
        return out;
    } else {
        return NULL;
    }
}

void* static_realloc(void* location, size_t memsize, void* ctx) {
    return NULL;
}

void static_free(void* location, void* ctx) { }

#pragma GCC diagnostic pop

Allocator mk_static_allocator(void* memory, size_t size) {
    StaticContext *context = memory;
    *context = (StaticContext) {
        .head = memory + sizeof(StaticContext),
        .remainder = size - sizeof(StaticContext),
    };

    return (Allocator) {
        .malloc = static_malloc,
        .realloc = static_realloc,
        .free = static_free,
        .ctx = context,
    };
}
