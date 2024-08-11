#include "memory/arena.h"
#include "data/meta/array_header.h"
#include "data/meta/array_impl.h"

typedef struct arena_block {
    void* data;
    size_t bmp;
} arena_block;

ARRAY_HEADER(arena_block, block)
ARRAY_IMPL(arena_block, block)

typedef struct arena_context {
    size_t blocksize;
    block_array memory_blocks;
    allocator internal_allocator;
} arena_context;


void* arena_malloc(size_t memsize, void* vctx) {
    arena_context* ctx = (arena_context*)vctx;
    size_t alloc_size = memsize + sizeof(size_t);

    // if attempting to allocate more than a block of memory
    // allocate a larger than usual block
    if (memsize > ctx->blocksize) {
        // allocate new block
        arena_block new_block;
        new_block.data = mem_alloc(alloc_size, ctx->internal_allocator);
        push_block(new_block, &ctx->memory_blocks, ctx->internal_allocator);
        size_t* sz = new_block.data;
        *sz = memsize;
        return new_block.data + sizeof(size_t);
    } else {
        // attempt to allocate in the currently free arena.
        // Note: the len-1 is safe as there is always at least 1 element (and thus len - 1 > 0)
        arena_block* current_block =
            ctx->memory_blocks.data + ctx->memory_blocks.len - 1;

        if (alloc_size < ctx->blocksize - current_block->bmp) {
            void* data = current_block->data + current_block->bmp;
            current_block->bmp += alloc_size;
            *(size_t*)data = memsize;
            return data + sizeof(size_t);
        } else {
            // allocate new block
            arena_block new_block;
            new_block.data = mem_alloc(ctx->blocksize, ctx->internal_allocator);
            new_block.bmp = alloc_size;
            push_block(new_block, &ctx->memory_blocks, ctx->internal_allocator);
            *(size_t*)new_block.data = memsize;
            return new_block.data + sizeof(size_t);
        }
    }
}

void* arena_realloc(void* ptr, size_t memsize, void* ctx) {
    // Arenas don't reallocate; just get a new block and discard the old one
    size_t old_size = *(size_t*) (ptr - sizeof(size_t));
    void* new_data = arena_malloc(memsize, ctx);
    memcpy(new_data, ptr, old_size);
    return new_data;
}

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-parameter"
void arena_free(void* ptr, void* ctx) { }
#pragma GCC diagnostic pop

allocator mk_arena_allocator(size_t blocksize, allocator a) {
    arena_context* ctx = mem_alloc(sizeof(arena_context), a);
    ctx->memory_blocks = mk_block_array(10, a);
    ctx->blocksize = blocksize;
    ctx->internal_allocator = a;

    arena_block initial_block;
    initial_block.data = mem_alloc(ctx->blocksize, ctx->internal_allocator);
    initial_block.bmp = 0;
    push_block(initial_block, &ctx->memory_blocks, a);
    
    allocator arena;
    arena.malloc = &arena_malloc;
    arena.realloc = &arena_realloc;
    arena.free = &arena_free;
    arena.ctx = ctx;
    return arena;
}

void delete_block(arena_block block, allocator a) {
    mem_free(block.data, a);
}

void release_arena_allocator(allocator a) {
    arena_context* ctx = (arena_context*)a.ctx;
    allocator ialloc = ctx->internal_allocator;

    delete_block_array(ctx->memory_blocks, &delete_block, ialloc);
    mem_free(ctx, ialloc);
}
