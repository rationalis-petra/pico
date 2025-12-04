#include "platform/signals.h"
#include "platform/memory/region.h"

#include "data/array.h"
#include "data/meta/array_header.h"
#include "data/meta/array_impl.h"

typedef struct {
    void* data;
    size_t bmp;
} RegionBlock;

ARRAY_HEADER(RegionBlock, rblock, RBlock)
ARRAY_COMMON_IMPL(RegionBlock, rblock, RBlock)

struct RegionAllocator {
    RegionAllocator* parent_region;
    PtrArray child_regions;

    size_t memory_allocated;
    size_t blocksize;
    RBlockArray blocks;
    Allocator* gpa;

    bool dynamic_regionsize;
    bool in_use;
};

RegionAllocator* make_region_allocator(size_t initial_regionsize, bool dynamic_regionsize, Allocator* a) {
    RegionAllocator* region = mem_alloc(sizeof(RegionAllocator), a);

    *region = (RegionAllocator) {
        .parent_region = NULL,
        .child_regions = mk_ptr_array(8, a),

        .memory_allocated = 0,
        .blocksize = initial_regionsize,
        .blocks = mk_rblock_array(1, a),
        .gpa = a,

        .dynamic_regionsize = dynamic_regionsize,
        .in_use = true,
    };

    RegionBlock rblock = {
        .data = mem_alloc(initial_regionsize, a),
        .bmp = 0,
    };
    push_rblock(rblock, &region->blocks);

    return region;
}

static size_t align_padding(size_t size, size_t align) {
    size_t rem = size % align;
    size_t pad = rem == 0 ? 0 : align - rem;
    return pad;
}

void* region_alloc(RegionAllocator* region, size_t memsize) {
    // Allocation happens 

    size_t alloc_size = memsize + sizeof(size_t);
    // If attempting to allocate more than a block of memory
    // allocate a larger than usual block
    // the '+ sizeof(size_t)' accounts for the fact that all memory has a short
    // prefix that denotes it's storage location

    if (alloc_size + sizeof(size_t) > region->blocksize) {
        // Allocate new (full) block for the large memory
        RegionBlock data_block;
        data_block.bmp = alloc_size + sizeof(size_t);
        data_block.data = mem_alloc(alloc_size + sizeof(size_t), region->gpa);
        push_rblock(data_block, &region->blocks);
        *(size_t*)data_block.data = alloc_size; 

        // Allocate a new (empty) block for future (small) allocations.
        RegionBlock new_block;
        new_block.bmp = 0;
        new_block.data = mem_alloc(region->blocksize, region->gpa);
        push_rblock(new_block, &region->blocks);

        // Update the amount of memory allocated in this region
        region->memory_allocated += alloc_size + sizeof(size_t);

        return data_block.data + sizeof(size_t);
    } else {
        // Attempt to allocate in the currently free arena.
        // Note: the len-1 is safe as there is always at least 1 block (and thus len - 1 > 0)
        RegionBlock* current_block =
            region->blocks.data + region->blocks.len - 1;

        size_t pad = align_padding(current_block->bmp, 16);
        if (alloc_size + pad < region->blocksize - current_block->bmp) {
            void* data = current_block->data + current_block->bmp + pad;
            current_block->bmp += alloc_size + pad;
            *(size_t*)data = memsize;

            // Update the amount of memory allocated in this region
            region->memory_allocated += pad + alloc_size;
            return data + sizeof(size_t);
        } else {
            // allocate new block
            RegionBlock new_block;
            new_block.data = mem_alloc(region->blocksize, region->gpa);
            new_block.bmp = alloc_size;
            push_rblock(new_block, &region->blocks);
            *(size_t*)new_block.data = memsize;

            // Update the amount of memory allocated in this region
            region->memory_allocated += alloc_size;
            return new_block.data + sizeof(size_t);
        }
    }
}

void* region_malloc_adapter(size_t memsize, void* arena) {
    return region_alloc((RegionAllocator*)arena, memsize);
}

void* region_realloc(void* ptr, size_t memsize, void* ctx) {
    // Arenas don't reallocate; just get a new block and discard the old one
    size_t old_size = *(size_t*) (ptr - sizeof(size_t));
    void* new_data = region_alloc(ctx, memsize);
    memcpy(new_data, ptr, old_size);
    return new_data;
}

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-parameter"
void region_free(void* ptr, void* ctx) { }
#pragma GCC diagnostic pop

RegionAllocator* make_subregion(RegionAllocator* region) {
    if (region->parent_region) {
        return make_subregion(region->parent_region);
    } else {
        for (size_t i = 0; i < region->child_regions.len; i++) {
            RegionAllocator* subregion = region->child_regions.data[i];
            if (!subregion->in_use) {
                subregion->in_use = true;
                return subregion;
            }
        }

        // There are no freely available subregions, therefore we must make a
        // new subregion.
        RegionAllocator* subregion = mem_alloc(sizeof(RegionAllocator), region->gpa);

        *subregion = (RegionAllocator) {
            .parent_region = region,
            .memory_allocated = 0,
            .blocksize = region->blocksize,
            .blocks = mk_rblock_array(1, region->gpa),
            .gpa = region->gpa,
            .dynamic_regionsize = region->dynamic_regionsize,
            .in_use = true,
        };

        RegionBlock rblock = {
            .data = mem_alloc(region->blocksize, region->gpa),
            .bmp = 0,
        };
        push_rblock(rblock, &subregion->blocks);

        push_ptr(subregion, &region->child_regions);
        return subregion;
    }
}

void release_subregion(RegionAllocator* subregion) {
    subregion->in_use = false;
    reset_subregion(subregion);
}

void reset_subregion(RegionAllocator* subregion) {
    if (subregion->memory_allocated > subregion->blocksize) {
        // TODO: only reset regionsize if dynamic regionsize is true.
        subregion->blocksize = subregion->memory_allocated;
        if (subregion->parent_region) {
            subregion->parent_region->blocksize = subregion->memory_allocated;
        }
        subregion->memory_allocated = 0;
        for (size_t i = 0; i < subregion->blocks.len; i++) {
            RegionBlock block = subregion->blocks.data[i];
            mem_free(block.data, subregion->gpa);
        }
        RegionBlock first_block = (RegionBlock) {
            .data = mem_alloc(subregion->blocksize, subregion->gpa),
            .bmp = 0,
        };
        subregion->blocks.data[0] = first_block;
        subregion->blocks.len = 1;
    } else {
        // No resizing needed, simply delete excess blocks and reset current block
        for (size_t i = 1; i < subregion->blocks.len; i++) {
            RegionBlock block = subregion->blocks.data[i];
            mem_free(block.data, subregion->gpa);
        }
        subregion->blocks.len = 1;
        subregion->blocks.data[0].bmp = 0;
    }
}

void delete_subregion(RegionAllocator* subregion) {
    for (size_t i = 1; i < subregion->blocks.len; i++) {
        RegionBlock block = subregion->blocks.data[i];
        mem_free(block.data, subregion->gpa);
    }
}

void delete_region_allocator(RegionAllocator* region) {
    if (region->parent_region) {
        panic(mv_string("Attempted to delete a subregion. Only root (parent) regions can be delted!"));
    }

    for (size_t i = 0; i < region->child_regions.len; i++) {
        RegionAllocator* subregion = region->child_regions.data[i];
        delete_subregion(subregion);
        mem_free(subregion, region->gpa);
    }

    mem_free(region, region->gpa);
}

/* Adapt the region allocator to a regular allocator interface,  
 * for usage in containers etc.
 */
Allocator ra_to_gpa(RegionAllocator* arena) {
    static AllocatorVTable region_vtable = {
        .malloc = &region_malloc_adapter,
        .realloc = &region_realloc,
        .free = &region_free,
    };
    
    return (Allocator) {.vtable = &region_vtable, .ctx =  arena};
}
