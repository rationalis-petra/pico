#ifndef __PLATFORM_FILESYSTEM_FILESYSTEM_H
#define __PLATFORM_FILESYSTEM_FILESYSTEM_H

#include "platform/memory/allocator.h"
#include "data/string.h"
#include <stdbool.h>
#include <stdint.h>

// forward-declaration of window (platform/window/window.h)
typedef struct File File;

typedef enum {
    Read, Write, ReadWrite, Append, ReadAppend
} FilePermissions;

File* open_file(String name, FilePermissions perms, Allocator* alloc);
File* open_tempfile(Allocator* alloc);
void close_file(File* file);

const char* get_tmpdir();

bool read_byte(File* file, uint8_t* out);
U8Array read_chunk(File* file, bool limit, uint64_t size_limit, Allocator* region);

bool write_byte(File* file, uint8_t out);
bool write_chunk(File* file, U8Array arr);



#endif
