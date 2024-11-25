// Relevant OS includes 
#include "platform/machine_info.h"

#if OS_FAMILY == WINDOWS
  #include <windows.h>
  #include <memoryapi.h>
#elif OS_FAMILY == UNIX
  #include <sys/mman.h>
  #include <unistd.h>
#else 
  #error "platform/memory/executable.c: Only support unix/windows families"
#endif

#include "platform/memory/executable.h"
#include "data/array.h"

typedef struct ex_mem {
    void* data;
    size_t size;
} ex_mem;


size_t platform_pagesize () {
#if OS_FAMILY == UNIX
    size_t pagesize = sysconf(_SC_PAGESIZE);
#elif OS_FAMILY == WINDOWS
    size_t pagesize = 1024;
#endif
    return pagesize;
}

ex_mem alloc_ex_mem(size_t min_size) {
    ex_mem out;

    // calculate out.size as smallest multiple of pagesize satisfying > min_size
    size_t pagesize = platform_pagesize();
    out.size = pagesize * ((min_size + pagesize - 1) / pagesize);
    
#if OS_FAMILY == UNIX
    void* memory = mmap(NULL, out.size, PROT_EXEC | PROT_READ | PROT_WRITE, MAP_ANONYMOUS | MAP_FILE | MAP_PRIVATE, -1, 0);
#elif OS_FAMILY == WINDOWS
    void* memory = VirtualAlloc(NULL, out.size, MEM_COMMIT | MEM_RESERVE, PAGE_EXECUTE_READWRITE);
#endif
    out.data = memory;

    return out;
}

void free_ex_mem(ex_mem mem) {
#if OS_FAMILY == UNIX
    munmap(mem.data, mem.size);
#elif OS_FAMILY == WINDOWS
    VirtualFree(mem.data, mem.size, MEM_DECOMMIT);
#endif
}


// -----------------------------------------------------------------------------
//  Executable Allocator
// -----------------------------------------------------------------------------

// Medium blocks
typedef struct small_block {
    ex_mem block_memory;
    size_t num_chunks;
    U8Array free_slots;
} small_block;
static const size_t small_chunk_size = 8;

typedef struct medium_block {
    ex_mem block_memory;
    struct medium_block* next;
} medium_block;

typedef struct large_block {
    ex_mem block_memory;
    struct large_block* next;
} large_block;

typedef struct exec_context {
    // Blocks
    PtrArray small_blocks;

    medium_block* medium_blocks;
    medium_block* medium_blocks_end;

    large_block* large_blocks;
    large_block* large_blocks_end;

    size_t blocksize;
    
    // To simplify the allocator, all auxiliary datastructures (such as the free
    // list and block metadata) are stored in memory allocated by a separate
    // allocator.
    Allocator* metadata_allocator;
} exec_context;

void* alloc_small(exec_context* ctx) {
    for (size_t block_idx = 0; block_idx < ctx->small_blocks.len; block_idx++) {
        small_block b = *(small_block*)ctx->small_blocks.data[block_idx];
        for (size_t i = 0; i < b.num_chunks; i++) {
            if (b.free_slots.data[i]) {
                b.free_slots.data[i] = false;
                return b.block_memory.data + (small_chunk_size * i);
            }
        }
    }
    // there was no free slot
    small_block* sb = (small_block*)mem_alloc(sizeof(small_block), ctx->metadata_allocator);
    sb->block_memory = alloc_ex_mem(ctx->blocksize);
    sb->block_memory.size = ctx->blocksize / 8;
    sb->free_slots = mk_u8_array(sb->num_chunks, ctx->metadata_allocator);
    for (size_t i = 0; i < sb->num_chunks; i++) {
        push_u8(0, &sb->free_slots);
    }
    sb->free_slots.data[0] = true;
    push_ptr(sb, &ctx->small_blocks);
    return sb->block_memory.data;
}

void* alloc_medium(size_t size, exec_context* ctx) {
    medium_block* new = mem_alloc(sizeof(medium_block), ctx->metadata_allocator);
    new->block_memory = alloc_ex_mem(size);
    new->next = NULL;

    medium_block* blk = ctx->medium_blocks_end;
    if (blk) blk->next = new;
    ctx->medium_blocks_end = new;

    if (!ctx->medium_blocks) {
        ctx->medium_blocks = new;
    }

    return new->block_memory.data;
}

void* alloc_large(size_t size, exec_context* ctx) {
    large_block* new = mem_alloc(sizeof(medium_block), ctx->metadata_allocator);
    new->block_memory = alloc_ex_mem(size);
    new->next = NULL;
    ctx->large_blocks_end = new;

    large_block* blk = ctx->large_blocks_end;
    if (blk) blk->next = new;

    return new->block_memory.data;
}


void* exec_alloc(size_t size, void* ctx) {
    exec_context* ectx = (exec_context*) ctx;
    if (size < 8) {
        return alloc_small(ectx);
    } else if (size < ectx->blocksize) {
        return alloc_medium(size, ectx);
    } else {
        return alloc_large(size, ectx);
    }
}

void* exec_realloc(void* ptr, size_t new_size, void* ctx) {
    // TODO (FEAT BUG): Implement ME!
    return NULL;
}

bool free_small(void* ptr, exec_context* ctx) {
    for (size_t i = 0; i < ctx->small_blocks.len; i++) {
        small_block block = *(small_block*)(ctx->small_blocks.data[i]);
        const void* data = block.block_memory.data;
        size_t offset = 0;
        // check for each element of to_free
        while (offset < block.num_chunks) {
            if (ptr == data + offset) {
                block.free_slots.data[offset / small_chunk_size] = 1; // set it as free
                return true;
            }
            offset += small_chunk_size;
        }
    }
    return false;
}
bool free_medium(void* ptr, exec_context* ctx) {
    medium_block* blk_prev = NULL;
    medium_block** blk_ptr = &ctx->medium_blocks;
    medium_block* blk = ctx->medium_blocks;
    while (blk != NULL) {
        if (ptr == blk) {
            *blk_ptr = blk->next;
            free_ex_mem(blk->block_memory);
            mem_free(blk, ctx->metadata_allocator);
            if (blk == ctx->medium_blocks_end) {
                ctx->medium_blocks_end = blk_prev;
            }
            return true;
        } else {
            blk_prev = blk;
            blk_ptr = &blk->next; 
            blk = blk->next;
        }
    }
    return false;
}

bool free_large(void* ptr, exec_context* ctx) {
    large_block* blk_prev = NULL;
    large_block** blk_ptr = &ctx->large_blocks;
    large_block* blk = ctx->large_blocks;
    while (blk != NULL) {
        if (ptr == blk) {
            *blk_ptr = blk->next;
            free_ex_mem(blk->block_memory);
            mem_free(blk, ctx->metadata_allocator);
            if (blk == ctx->large_blocks_end) {
                ctx->large_blocks_end = blk_prev;
            }
            return true;
        } else {
            blk_prev = blk;
            blk_ptr = &blk->next; 
            blk = blk->next;
        }
    }
    return false;
}

void exec_free(void* ptr, void* ctx) {
    exec_context* ectx = (exec_context*) ctx;
    if (free_small(ptr, ectx)) return;
    if (free_medium(ptr, ectx)) return;
    if (free_large(ptr, ectx)) return;
    // free failed
    // possibly log error & crash??
}

Allocator mk_executable_allocator(Allocator* a) {
    exec_context* ctx = mem_alloc(sizeof(exec_context), a); 
    ctx->blocksize = platform_pagesize();
    ctx->small_blocks = mk_ptr_array(10, a);
    ctx->medium_blocks = NULL;
    ctx->medium_blocks_end = NULL;
    ctx->large_blocks = NULL;
    ctx->large_blocks_end = NULL;
    ctx->metadata_allocator = a;

    Allocator out =
        { .ctx = ctx
        , .malloc = &exec_alloc
        , .free = &exec_free
        , .realloc = &exec_realloc
        };
    return out;
}

void release_executable_allocator(Allocator a) {
    exec_context* ctx = (exec_context*) a.ctx;
    Allocator* mda = ctx->metadata_allocator;

    for (size_t i = 0; i < ctx->small_blocks.len; i++) {
        small_block* b = (small_block*) ctx->small_blocks.data[i];
        free_ex_mem(b->block_memory);
        mem_free(b, mda);
    }
    sdelete_ptr_array(ctx->small_blocks);

    medium_block* mb = ctx->medium_blocks;
    while (mb) {
        medium_block* next = mb->next;
        mem_free(mb, mda);
        mb = next;
    }

    large_block* lb = ctx->large_blocks;
    while (lb) {
        large_block* next = lb->next;
        mem_free(lb, mda);
        lb = next;
    }

    mem_free(ctx, mda);
}


