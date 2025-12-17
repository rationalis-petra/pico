#include <stdio.h>
#include <string.h>

#include "platform/machine_info.h"
#include "platform/filesystem/filesystem.h"
#include "platform/signals.h"

#include "data/string.h"
#include "data/meta/array_impl.h"

// ---------------------------------------------------------------------------
//     Paths 
// ---------------------------------------------------------------------------

String path_cat(String path1, String path2, Allocator *alloc){
#if OS_FAMILY == WINDOWS
    return string_ncat(alloc, 3, path1, mv_string("\\"), path2);
#else
    return string_ncat(alloc, 3, path1, mv_string("/"), path2);
#endif
}

// ---------------------------------------------------------------------------
//     Directories
// ---------------------------------------------------------------------------

// #define PICO_ARRAY_COMMON_IMPL(type, fprefix, tprefix)
ARRAY_COMMON_IMPL(DirectoryEntry, dirent, DirEnt)

#if OS_FAMILY == WINDOWS
#include <windows.h>
    struct Directory {
    HANDLE handle;
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
#if OS_FAMILY == WINDOWS
// TODO: the name here is ascii, but our strings are UTF-8!
    HANDLE handle = CreateFileA((const char*)name.bytes,
        0, // Windows is weird, so we don't need to requirest any permissions!
        0, // Don't share
        NULL, // Default security attributes
        OPEN_EXISTING, // Expect the directory to already exist
        FILE_FLAG_BACKUP_SEMANTICS, // Needed for directories
        NULL // No template
    );

    if (handle != INVALID_HANDLE_VALUE) {
        Directory* dir = mem_alloc(sizeof(Directory), alloc);
        *dir = (Directory) {
            .handle = handle,
            .gpa = alloc,
        };
        return dir;
    } else {
        return NULL;
    }
#else
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
#endif
    
}

void close_directory(Directory* directory) {
#if OS_FAMILY == WINDOWS
    CloseHandle(directory->handle);
#else
    closedir(directory->handle);
#endif
    mem_free(directory, directory->gpa);
}

DirEntArray list_entries(Directory* dir, Allocator* alloc) {
    DirEntArray entries = mk_dirent_array(8, alloc);

#if OS_FAMILY == WINDOWS
// TODO: error handling
// TODO: appropriate treatment of MAX_PATH
    char buf[512];
    DWORD bufsize = GetFinalPathNameByHandleA(
        dir->handle,
        buf,
        500, // less than 512 so we can append '\\'
        FILE_NAME_NORMALIZED);
    buf[bufsize] = '\\';
    buf[bufsize + 1] = '*';
    buf[bufsize + 2] = 0;

    WIN32_FIND_DATA ffd;
    HANDLE hFind = FindFirstFile(buf, &ffd);
 
    do {
        DirectoryEntry entry = {
            .name = mk_string(ffd.cFileName, alloc),
            .is_directory = (ffd.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) && 1,
        };
        push_dirent(entry, &entries);
    } while (FindNextFile(hFind, &ffd) != 0);
#else

    struct dirent *ep;
    while ((ep = readdir(dir->handle)) != NULL)
    {
        DirectoryEntry entry = {
            .name = mk_string(ep->d_name, alloc),
            .is_directory = ep->d_type == DT_DIR,
        };
        push_dirent(entry, &entries);
    }
#endif

        return entries;
}

DirEntArray list_children(Directory* dir, Allocator* alloc) {
    DirEntArray entries = mk_dirent_array(8, alloc);

#if OS_FAMILY == WINDOWS
// TODO: error handling
// TODO: appropriate treatment of MAX_PATH
    char buf[512];
    DWORD bufsize = GetFinalPathNameByHandleA(
        dir->handle,
        buf,
        500, // less than 512 so we can append '\\'
        FILE_NAME_NORMALIZED);
    buf[bufsize] = '\\';
    buf[bufsize + 1] = '*';
    buf[bufsize + 2] = 0;

    WIN32_FIND_DATA ffd;
    HANDLE hFind = FindFirstFile(buf, &ffd);
 
    do {
        String name = mv_string(ffd.cFileName);
        if ((string_cmp(name, mv_string(".")) != 0) && string_cmp(name, mv_string("..")) != 0) {
            DirectoryEntry entry = {
                .name = mk_string(ffd.cFileName, alloc),
                .is_directory = (ffd.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) && 1,
            };
            push_dirent(entry, &entries);
        }
    } while (FindNextFile(hFind, &ffd) != 0);
#else
    struct dirent *ep;
    while ((ep = readdir(dir->handle)) != NULL) {
            String name = mv_string(cp->d_name);
        if ((string_cmp(name, mv_string(".")) != 0) && (string_cmp(name, mv_string("..")) != 0)) {
            DirectoryEntry entry = {
                .name = mk_string(ep->d_name, alloc),
                .is_directory = ep->d_type == DT_DIR,
            };
            push_dirent(entry, &entries);
        }
    }
#endif

    return entries;
}

String get_current_directory(Allocator* a) {
    // TODO: error handling!
#if OS_FAMILY == WINDOWS
    size_t mem_required = GetCurrentDirectory(0, NULL);
    String out = {
        .memsize = mem_required,
        .bytes = mem_alloc(mem_required, a),
    };
    // TODO: convert to valid path (consider encoding)
    GetCurrentDirectory(mem_required, (char*)out.bytes);
    return out;
#else
    size_t mem_required = getcwd(0, NULL);
    String out = {
        .memsize = mem_required,
        .bytes = mem_alloc(mem_required, a),
    };
    getcwd(mem_required, out.bytes);
    return out;
#endif
}

void set_current_directory(String path) {
// TODO: convert to valid path (consider encoding)
#if OS_FAMILY == WINDOWS
    SetCurrentDirectory((const char*)path.bytes);
#else
    setcwd(path.bytes);
#endif
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
