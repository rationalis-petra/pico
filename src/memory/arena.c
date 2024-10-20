#include "memory/arena.h"
#include "data/meta/array_header.h"
#include "data/meta/array_impl.h"

typedef struct {
    void* data;
    size_t bmp;
} ArenaBlock;

ARRAY_HEADER(ArenaBlock, block, Block)
ARRAY_IMPL(ArenaBlock, block,Block)

typedef struct {
    size_t blocksize;
    BlockArray memory_blocks;
    Allocator* internal_allocator;
} ArenaContext;


void* arena_malloc(size_t memsize, void* vctx) {
    ArenaContext* ctx = (ArenaContext*)vctx;
    size_t alloc_size = memsize + sizeof(size_t);
    // TODO (TAGS: BUG FEAT): ensure allocations are aligned! 

    // if attempting to allocate more than a block of memory
    // allocate a larger than usual block
    if (memsize > ctx->blocksize) {
        // allocate new block
        ArenaBlock new_block;
        new_block.bmp = 0;
        new_block.data = mem_alloc(alloc_size, ctx->internal_allocator);
        push_block(new_block, &ctx->memory_blocks);
        size_t* sz = new_block.data;
        *sz = memsize;
        return new_block.data + sizeof(size_t);
    } else {
        // attempt to allocate in the currently free arena.
        // Note: the len-1 is safe as there is always at least 1 element (and thus len - 1 > 0)
        ArenaBlock* current_block =
            ctx->memory_blocks.data + ctx->memory_blocks.len - 1;

        if (alloc_size < ctx->blocksize - current_block->bmp) {
            void* data = current_block->data + current_block->bmp;
            current_block->bmp += alloc_size;
            *(size_t*)data = memsize;
            return data + sizeof(size_t);
        } else {
            // allocate new block
            ArenaBlock new_block;
            new_block.data = mem_alloc(ctx->blocksize, ctx->internal_allocator);
            new_block.bmp = alloc_size;
            push_block(new_block, &ctx->memory_blocks);
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

Allocator mk_arena_allocator(size_t blocksize, Allocator* a) {
    ArenaContext* ctx = mem_alloc(sizeof(ArenaContext), a);
    ctx->memory_blocks = mk_block_array(10, a);
    ctx->blocksize = blocksize;
    ctx->internal_allocator = a;

    ArenaBlock initial_block;
    initial_block.data = mem_alloc(ctx->blocksize, ctx->internal_allocator);
    initial_block.bmp = 0;
    push_block(initial_block, &ctx->memory_blocks);
    
    Allocator arena;
    arena.malloc = &arena_malloc;
    arena.realloc = &arena_realloc;
    arena.free = &arena_free;
    arena.ctx = ctx;
    return arena;
}

void delete_block(ArenaBlock block, Allocator* a) {
    mem_free(block.data, a);
}

void release_arena_allocator(Allocator a) {
    ArenaContext* ctx = (ArenaContext*)a.ctx;
    Allocator* ialloc = ctx->internal_allocator;

    for (size_t i = 0; i < ctx->memory_blocks.len; i++)
        delete_block(ctx->memory_blocks.data[i], ialloc);

    sdelete_block_array(ctx->memory_blocks);
    mem_free(ctx, ialloc);
}
