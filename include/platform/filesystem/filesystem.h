#ifndef __PLATFORM_FILESYSTEM_FILESYSTEM_H
#define __PLATFORM_FILESYSTEM_FILESYSTEM_H

#include "platform/memory/allocator.h"
#include "data/string.h"
#include <stdbool.h>
#include <stdint.h>

// forward-declaration of window (platform/window/window.h)
typedef struct File File;

typedef enum {
    Read, Write, ReadWrite, 
} FilePermissions;

File* open_file(String name, FilePermissions perms, Allocator* alloc);
void close_file(File* file);

bool read_byte(File* file, uint8_t* out);
U8Array read_chunk(File* file, uint64_t max_size, Allocator* region);


#endif
