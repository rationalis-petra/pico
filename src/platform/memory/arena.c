#include "platform/memory/arena.h"
#include "data/meta/array_header.h"
#include "data/meta/array_impl.h"

typedef struct {
    void* data;
    size_t bmp;
} ArenaBlock;

ARRAY_HEADER(ArenaBlock, block, Block)
ARRAY_COMMON_IMPL(ArenaBlock, block, Block)

struct ArenaAllocator{
    size_t blocksize;
    BlockArray memory_blocks;
    Allocator* internal_allocator;
};

static size_t align_padding(size_t size, size_t align) {
    size_t rem = size % align;
    size_t pad = rem == 0 ? 0 : align - rem;
    return pad;
}

void* arena_malloc(ArenaAllocator* arena, size_t memsize) {
    size_t alloc_size = memsize + sizeof(size_t);
    // If attempting to allocate more than a block of memory
    // allocate a larger than usual block
    // the '+ sizeof(size_t)' accounts for the fact that all memory has a short
    // prefix that denotes it's storage location

    if (alloc_size + sizeof(size_t) > arena->blocksize) {
        // Allocate new (full) block for the large memory
        ArenaBlock data_block;
        data_block.bmp = alloc_size + sizeof(size_t);
        data_block.data = mem_alloc(alloc_size + sizeof(size_t), arena->internal_allocator);
        push_block(data_block, &arena->memory_blocks);
        *(size_t*)data_block.data = alloc_size; 

        // Allocate a new (empty) block for future (small) allocations.
        ArenaBlock new_block;
        new_block.bmp = 0;
        new_block.data = mem_alloc(arena->blocksize, arena->internal_allocator);
        push_block(new_block, &arena->memory_blocks);

        return data_block.data + sizeof(size_t);
    } else {
        // Attempt to allocate in the currently free arena.
        // Note: the len-1 is safe as there is always at least 1 block (and thus len - 1 > 0)
        ArenaBlock* current_block =
            arena->memory_blocks.data + arena->memory_blocks.len - 1;

        size_t pad = align_padding(current_block->bmp, 16);
        if (alloc_size + pad < arena->blocksize - current_block->bmp) {
            void* data = current_block->data + current_block->bmp + pad;
            current_block->bmp += alloc_size + pad;
            *(size_t*)data = memsize;
            return data + sizeof(size_t);
        } else {
            // allocate new block
            ArenaBlock new_block;
            new_block.data = mem_alloc(arena->blocksize, arena->internal_allocator);
            new_block.bmp = alloc_size;
            push_block(new_block, &arena->memory_blocks);
            *(size_t*)new_block.data = memsize;
            return new_block.data + sizeof(size_t);
        }
    }
}

void* arena_malloc_adapter(size_t memsize, void* arena) {
    return arena_malloc((ArenaAllocator*)arena, memsize);
}

void* arena_realloc(void* ptr, size_t memsize, void* ctx) {
    // Arenas don't reallocate; just get a new block and discard the old one
    size_t old_size = *(size_t*) (ptr - sizeof(size_t));
    void* new_data = arena_malloc(ctx, memsize);
    memcpy(new_data, ptr, old_size);
    return new_data;
}

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-parameter"
void arena_free(void* ptr, void* ctx) { }
#pragma GCC diagnostic pop

ArenaAllocator* make_arena_allocator(size_t blocksize, Allocator* a) {
    ArenaAllocator* arena = mem_alloc(sizeof(ArenaAllocator), a);
    *arena = (ArenaAllocator) {
        .memory_blocks = mk_block_array(10, a),
        .blocksize = blocksize,
        .internal_allocator = a,
    };

    ArenaBlock initial_block = {
        .data = mem_alloc(arena->blocksize, arena->internal_allocator),
        .bmp = 0,
    };
    push_block(initial_block, &arena->memory_blocks);

    return arena;
}

void delete_block(ArenaBlock block, Allocator* a) {
    mem_free(block.data, a);
}

void reset_arena_allocator(ArenaAllocator* arena) {
    Allocator* ialloc = arena->internal_allocator;

    for (size_t i = 1; i < arena->memory_blocks.len; i++)
        delete_block(arena->memory_blocks.data[i], ialloc);

    arena->memory_blocks.len = 1;
    ArenaBlock* initial_block = &arena->memory_blocks.data[0];
    initial_block->bmp = 0;
}

void delete_arena_allocator(ArenaAllocator* arena) {
    Allocator* ialloc = arena->internal_allocator;

    for (size_t i = 0; i < arena->memory_blocks.len; i++)
        delete_block(arena->memory_blocks.data[i], ialloc);

    sdelete_block_array(arena->memory_blocks);
    mem_free(arena, ialloc);
}

Allocator aa_to_gpa(ArenaAllocator* arena) {
    static AllocatorVTable arena_vtable = {
        .malloc = &arena_malloc_adapter,
        .realloc = &arena_realloc,
        .free = &arena_free,
    };
    
    return (Allocator) {.vtable = &arena_vtable, .ctx =  arena};
}
