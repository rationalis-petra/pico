#ifndef __PLATFORM_FILESYSTEM_FILESYSTEM_H
#define __PLATFORM_FILESYSTEM_FILESYSTEM_H

#include <stdbool.h>
#include <stdint.h>

#include "data/meta/array_header.h"
#include "data/result.h"
#include "data/string.h"

#include "platform/memory/allocator.h"

// ---------------------------------------------------------------------------
//     Errors 
// ---------------------------------------------------------------------------

typedef enum : uint64_t {
    ErrFileDoesNotExist,
    ErrFilePermissionDenied,
} FileOpenError;

// ---------------------------------------------------------------------------
//     Paths 
// ---------------------------------------------------------------------------

String path_cat(String path1, String path2, Allocator* alloc);

// ---------------------------------------------------------------------------
//     Directories 
// ---------------------------------------------------------------------------

typedef struct Directory Directory;

typedef struct {
    Result_t type;
    union {
        Directory* directory;
        FileOpenError error;
    };
} DirectoryResult;

typedef struct {
    String name;
    bool is_directory;
} DirectoryEntry;

ARRAY_HEADER(DirectoryEntry, dirent, DirEnt)

DirectoryResult open_directory(String name, Allocator* alloc);
void close_directory(Directory* directory);

// List all entries in the current directory
//  WARNING: if using this to traverse a directory, beware the possibility
//   of getting stuck in an infinite loop, as the current directory (.) is
//   also listed.
DirEntArray list_entries(Directory* dir, Allocator* alloc);

// List all entries in the curent directory - excluding the 'self' entry (.)
//   and the 'parent' entry (..).
DirEntArray list_children(Directory* dir, Allocator* alloc);

String get_current_directory(Allocator* a);
void set_current_directory(String path);

// ---------------------------------------------------------------------------
//     Files 
// ---------------------------------------------------------------------------

typedef struct File File;

typedef struct {
    Result_t type;
    union {
        File* file;
        FileOpenError error;
    };
} FileResult;

typedef enum {
    Read, Write, ReadWrite, Append, ReadAppend
} FilePermissions;

FileResult open_file(String name, FilePermissions perms, Allocator* alloc);
FileResult open_tempfile(Allocator* alloc);
void close_file(File* file);

String get_tmpdir(Allocator* a);

bool read_byte(File* file, uint8_t* out);
U8Array read_chunk(File* file, bool limit, uint64_t size_limit, Allocator* region);
U8Array read_chunk(File* file, bool limit, uint64_t size_limit, Allocator* region);

bool write_byte(File* file, uint8_t out);
bool write_chunk(File* file, U8Array arr);


#endif
