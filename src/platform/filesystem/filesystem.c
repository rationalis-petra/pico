#include <stdio.h>
#include <string.h>

#include "platform/machine_info.h"
#include "platform/filesystem/filesystem.h"
#include "platform/signals.h"

#include "data/string.h"
#include "data/meta/array_impl.h"


//#define PICO_ARRAY_COMMON_IMPL(type, fprefix, tprefix) 
ARRAY_COMMON_IMPL(DirectoryEntry, dirent, DirEnt)

#if OS_FAMILY == WINDOWS
#include <windows.h>
struct Directory {
    DIR* handle;
    Allocator* gpa;
};

#else
#include <dirent.h>

struct Directory {
    DIR* handle;
    Allocator* gpa;
};
#endif


Directory* open_directory(String name, Allocator* alloc) {
    // TODO: what encoding to filenames use?
    DIR* handle = opendir((char*)name.bytes);
    
    if (handle) {
        Directory* dir = mem_alloc(sizeof(Directory), alloc);
        *dir = (Directory) {
            .handle = handle,
            .gpa = alloc,
        };
        return dir;
    } else {
        return NULL;
    }
}

void close_directory(Directory* directory) {
    closedir(directory->handle);
    mem_free(directory, directory->gpa);
}

DirEntArray list_entries(Directory* dir, Allocator* alloc) {
    struct dirent *ep;
    DirEntArray entries = mk_dirent_array(8, alloc);
    while ((ep = readdir(dir->handle)) != NULL) {
        DirectoryEntry entry = {
            .dirname = mv_string(ep->d_name),
        };
        push_dirent(entry, &entries);
    }

    return entries;
}

/* struct File { */
/*     FILE* handle;  */
/*     Allocator gpa; */
/* }; */

File *open_file(String name, FilePermissions perms, Allocator *alloc) {
    const char *mode = NULL;
    switch (perms) {
    case Read:
        mode = "rb";
        break;
    case Write:
        mode = "wb";
        break;
    case ReadWrite:
        mode = "wb+";
        break;
    case Append:
        mode = "ab";
        break;
    case ReadAppend:
        mode = "ab+";
        break;
    default:
        panic(mv_string("Bad filemode"));
    }

    // TODO (BUG): string is utf-8, but this isn't (necessarily) what
    //    the plaform supports/uses. This should be checked.
    return (File*)fopen((char*)name.bytes, mode);
}

File *open_tempfile(Allocator *alloc) {
    return (File*) tmpfile();
}

void close_file(File *file) {
    fclose((FILE*)file);
}

String get_tmpdir(Allocator* a) {
#if OS_FAMILY == UNIX

    const char str[] = "/tmp";
    String out = (String) {
        .memsize = sizeof(str),
        .bytes = mem_alloc(sizeof(str), a),
    };
    memcpy(out.bytes, str, sizeof(str));
    return out;

#elif OS_FAMILY == WINDOWS

    uint64_t pathlen = GetTempPath(0, NULL);
    String out = (String) {
        .memsize = pathlen,
        .bytes = mem_alloc(pathlen, a),
    };
    GetTempPath(out.memsize, (char*) out.bytes);
    return out;

#else
#error "get_tmpdir not supported for this os"
#endif
}

// return true on failure
bool read_byte(File *file, uint8_t *out) {
    return !fread(out, sizeof(char), 1, (FILE*)file);
}

U8Array read_chunk(File *file, bool limit, uint64_t max_size, Allocator *region) {
    if (limit) {
        U8Array bytes = mk_u8_array(max_size, region);

        // TODO (BUG): update this method to return error on read failure.
        bytes.len = fread(bytes.data, sizeof(uint8_t), max_size, (FILE*)file);
        return bytes;
    } else {
        fseek((FILE*)file, 0, SEEK_END);
        long fsize = ftell((FILE*)file);
        fseek((FILE*)file, 0, SEEK_SET);  /* same as rewind(f); */
        U8Array bytes = mk_u8_array(fsize, region);

        bytes.len = fread(bytes.data, sizeof(uint8_t), fsize, (FILE*)file);
        return bytes;
    }

}

bool write_byte(File *file, uint8_t out) {
    return !fwrite(&out, sizeof(uint8_t), 1, (FILE*)file);
}

bool write_chunk(File* file, U8Array arr) {
    return !fwrite(arr.data, sizeof(uint8_t), arr.len, (FILE*)file);
}
