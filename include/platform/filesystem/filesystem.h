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
    ErrDoesNotExist,
    ErrAlreadyExists,
    ErrPermissionDenied,
    ErrInvalidArgument,
} RecordError;

typedef struct {
    Result_t type;
    RecordError error;
} RecordResult;

// ---------------------------------------------------------------------------
//     Paths 
// ---------------------------------------------------------------------------

String path_cat(String path1, String path2, Allocator* alloc);
String path_name(String path);

// ---------------------------------------------------------------------------
//     Directories 
// ---------------------------------------------------------------------------

typedef struct Directory Directory;

typedef struct {
    Result_t type;
    union {
        Directory* directory;
        RecordError error;
    };
} DirectoryResult;

// TODO (INVESTIGATE): replace with record info?
typedef struct {
    String name;
    bool is_directory;
} DirectoryEntry;

ARRAY_HEADER(DirectoryEntry, dirent, DirEnt)

RecordResult create_directory(String dirname);
RecordResult delete_directory(String dirname, bool recursive);
RecordResult copy_directory(String source, String dest);

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
        RecordError error;
    };
} FileResult;

typedef enum {
    Read, Write, ReadWrite, Append, ReadAppend
} FileMode;

RecordResult copy_file(String source, String dest);
RecordResult delete_file(String path);

FileResult open_file(String name, FileMode mode, Allocator* alloc);
FileResult open_tempfile(Allocator* alloc);
void close_file(File* file);

String get_tmpdir(Allocator* a);

bool read_byte(File* file, uint8_t* out);
U8Array read_chunk(File* file, bool limit, uint64_t size_limit, Allocator* region);
U8Array read_chunk(File* file, bool limit, uint64_t size_limit, Allocator* region);

bool write_byte(File* file, uint8_t out);
bool write_chunk(File* file, U8Array arr);

// ---------------------------------------------------------------------------
//     Records 
// ---------------------------------------------------------------------------

typedef enum : uint8_t {
    FRead = 0x4,
    FWrite = 0x2,
    FExecute = 0x1,
} FilePermission;

typedef struct {
    FilePermission user;
    FilePermission group;
    FilePermission other;
} FilePermissions;

RecordResult set_permissions(String file, FilePermissions perms);


bool record_exists(String path);

typedef enum {
    RINotExists,
    RIFile,
    RIDirectory,
} RecordType;

typedef struct {
    size_t file_size;
} FileInfo;

typedef struct {
} DirectoryInfo;

typedef struct {
    RecordType type;
    union {
        FileInfo file;
        DirectoryInfo dir;
    };
} RecordInfo;

RecordInfo record_info(String path);

#endif
