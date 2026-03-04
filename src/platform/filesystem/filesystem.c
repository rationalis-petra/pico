#include <stdio.h>
#include <string.h>

#include "platform/machine_info.h"
#include "platform/filesystem/filesystem.h"
#include "platform/signals.h"

#include "data/string.h"
#include "data/meta/array_impl.h"

#if OS_FAMILY == UNIX
// Unix Generic
#include <fcntl.h>
#include <unistd.h>

// Linux specific
#include <limits.h>
#include <sys/sendfile.h>
#include <sys/stat.h>
#endif

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
#include <unistd.h>
#include <errno.h>
#include <linux/limits.h>

struct Directory {
    DIR* handle;
    Allocator* gpa;
};
#endif

FileOpenError get_file_error_code() {
    // TODO: account for all documented possible error codes.
#if OS_FAMILY == WINDOWS
  switch (GetLastError()) {
  default:
      // TODO: surely there's a better solution than to panic?
      panic(mv_string("Unrecognized error code."));
  }
#else
  switch (errno) {
  case EACCES:
      return ErrFilePermissionDenied;
  case ENOENT:
      return ErrFileDoesNotExist;
  default:
      // TODO: surely there's a better solution than to panic?
      panic(mv_string("Unrecognized error code."));
  }
#endif
}

DirectoryResult open_directory(String name, Allocator* alloc) {
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
        return (DirectoryResult) {.type = Ok, .directory = dir};
    } else {
        return (DirectoryResult) {.type = Err, .error = get_file_error_code()};
    }
#else
    DIR* handle = opendir((char*)name.bytes);
    if (handle) {
        Directory* dir = mem_alloc(sizeof(Directory), alloc);
        *dir = (Directory) {
            .handle = handle,
            .gpa = alloc,
        };
        return (DirectoryResult) {.type = Ok, .directory = dir};
    } else {
        return (DirectoryResult) {.type = Err, .error = get_file_error_code()};
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
            String name = mv_string(ep->d_name);
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
    char cwd[PATH_MAX];
    getcwd(cwd, PATH_MAX);
    String dir = mv_string(cwd);
    return copy_string(dir, a);
#endif
}

void set_current_directory(String path) {
// TODO: convert to valid path (consider encoding)
#if OS_FAMILY == WINDOWS
    SetCurrentDirectory((const char*)path.bytes);
#else
    chdir((char*)path.bytes);
#endif
}

// ---------------------------------------------------------------------------
//     Directories 
// ---------------------------------------------------------------------------

FileResult open_file(String name, FileMode mode, Allocator *alloc) {
    const char *mode_str = NULL;
    switch (mode) {
    case Read:
        mode_str = "rb";
        break;
    case Write:
        mode_str = "wb";
        break;
    case ReadWrite:
        mode_str = "wb+";
        break;
    case Append:
        mode_str = "ab";
        break;
    case ReadAppend:
        mode_str = "ab+";
        break;
    default:
        panic(mv_string("Bad filemode"));
    }

    // TODO (BUG): string is utf-8, but this isn't (necessarily) what
    //    the plaform supports/uses. This should be checked.
    File* file = (File*)fopen((char*)name.bytes, mode_str);
    if (file) {
        return (FileResult) {.type = Ok, .file = file};
    } else {
        return (FileResult) {.type = Err, .error = get_file_error_code()};
    }

}

FileResult open_tempfile(Allocator *alloc) {
    File* file = (File*)tmpfile();
    if (file) {
        return (FileResult) {.type = Ok, .file = file};
    } else {
        return (FileResult) {.type = Err, .error = get_file_error_code()};
    }
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
        long start_pos = ftell((FILE*)file);
        fseek((FILE*)file, 0, SEEK_END);
        long fsize = ftell((FILE*)file);
        fseek((FILE*)file, start_pos, SEEK_SET);  /* same as rewind(f); */
        U8Array bytes = mk_u8_array(fsize - start_pos, region);

        bytes.len = fread(bytes.data, sizeof(uint8_t), fsize - start_pos, (FILE*)file);
        return bytes;
    }

}

bool write_byte(File *file, uint8_t out) {
    return !fwrite(&out, sizeof(uint8_t), 1, (FILE*)file);
}

bool write_chunk(File* file, U8Array arr) {
    return !fwrite(arr.data, sizeof(uint8_t), arr.len, (FILE*)file);
}

Result copy_file(String source, String dest) {
#if OS_FAMILY == WINDOWS
    if (CopyFile((LPCSTR)source.bytes, (LPCSTR)dest.bytes, false)) {
        return (Result){.type = Ok};
    } else {
        return (Result){.type = Err, .error_message = mv_string("Failed to copy file!")};
    }
#elif OS_FAMILY == UNIX
    // TODO (PORT): see https://stackoverflow.com/questions/2180079/how-can-i-copy-a-file-on-unix-using-c
    // for non-linux support!
    Result result = {.type = Ok};
    int input, output;
    if ((input = open((char*)source.bytes, O_RDONLY)) == -1)
    {
        return (Result) {.type =Err, .error_message = mv_string("Failed to open source file.")};
    }
    // Create new or truncate existing at destination
    if ((output = creat((char*)dest.bytes, 0660)) == -1)
    {
        close(input);
        return (Result) {.type =Err, .error_message = mv_string("Failed to create destination file.")};
    }
    // sendfile will work with non-socket output (i.e. regular file) under
    // Linux 2.6.33+ and some other unixy systems.
    struct stat file_stat = {0};
    if (fstat(input, &file_stat) != 0) {
        result = (Result) {.type = Err, .error_message = mv_string("Fstat file in copy file.")};
    }
    off_t copied = 0;
    while (result.type == Ok && copied < file_stat.st_size) {
        ssize_t written = sendfile(output, input, &copied, SSIZE_MAX);
        copied += written;
        if (written == -1) {
            result = (Result){.type = Err, .error_message = mv_string("Error while copying file.")};
        }
    }
    close(input);
    close(output);

    return result;
#endif
}

Result set_permissions(String file, FilePermissions perms) {
#if OS_FAMILY == WINDOWS
    Result res = {.type = Ok};
    return res;
#elif OS_FAMILY == UNIX
    Result res = {.type = Ok};
    mode_t unix_perms = (perms.user << 6) | (perms.group << 3) | perms.other;
    if (chmod((char *)file.bytes, unix_perms)) {
        res = (Result) {.type = Err, .error_message = mv_string("Failed to change permissions for file.")};
    }
    return res;
#endif
}

Result create_directory(String dirname) {
    Result res = {.type = Ok};
#if OS_FAMILY == WINDOWS
    if (!CreateDirectory((char*)dirname.bytes, NULL)) {
        res = (Result){.type = Err, .error_message = mv_string("Failed to create directory.")};
    }
    return res;
#elif OS_FAMILY == UNIX
    return res;
#endif
}

bool file_exists(String path) {
#if OS_FAMILY == WINDOWS
  DWORD dwAttrib = GetFileAttributes((char*)path.bytes);
  return (dwAttrib != INVALID_FILE_ATTRIBUTES);
#elif OS_FAMILY == UNIX
    #error "Not implemented in unix: file_exists!"
#endif
}