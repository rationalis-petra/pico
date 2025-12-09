#ifndef __PLATFORM_FILESYSTEM_FILESYSTEM_H
#define __PLATFORM_FILESYSTEM_FILESYSTEM_H

#include <stdbool.h>
#include <stdint.h>

#include "platform/memory/allocator.h"
#include "data/meta/array_header.h"

#include "data/string.h"

// ---------------------------------------------------------------------------
//     Directories 
// ---------------------------------------------------------------------------

typedef struct Directory Directory;

typedef struct {
    String dirname;
} DirectoryEntry;

ARRAY_HEADER(DirectoryEntry, dirent, DirEnt)

Directory* open_directory(String name, Allocator* alloc);
void close_directory(Directory* directory);

DirEntArray list_entries(Directory* dir, Allocator* alloc);

// ---------------------------------------------------------------------------
//     Files 
// ---------------------------------------------------------------------------

typedef struct File File;

typedef enum {
    Read, Write, ReadWrite, Append, ReadAppend
} FilePermissions;

File* open_file(String name, FilePermissions perms, Allocator* alloc);
File* open_tempfile(Allocator* alloc);
void close_file(File* file);

String get_tmpdir(Allocator* a);

bool read_byte(File* file, uint8_t* out);
U8Array read_chunk(File* file, bool limit, uint64_t size_limit, Allocator* region);
U8Array read_chunk(File* file, bool limit, uint64_t size_limit, Allocator* region);

bool write_byte(File* file, uint8_t out);
bool write_chunk(File* file, U8Array arr);



#endif
