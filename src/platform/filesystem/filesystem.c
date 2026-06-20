#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "platform/machine_info.h"
#include "platform/filesystem/filesystem.h"
#include "platform/memory/std_allocator.h"
#include "platform/signals.h"

#include "data/string.h"
#include "data/stringify.h"
#include "data/meta/array_impl.h"

#if OS_FAMILY == UNIX
// Unix Generic
#include <fcntl.h>
#include <unistd.h>

// Linux specific
#include <ftw.h>
#include <limits.h>
#include <sys/sendfile.h>
#include <sys/stat.h>
#endif


String error_description(RecordError err) {
    switch (err) {
    case ErrDoesNotExist:
        return mv_string("File Does Not Exist");
        break;
    case ErrAlreadyExists:
        return mv_string("File Already Denied");
        break;
    case ErrPermissionDenied:
        return mv_string("Permission Denied");
        break;
    case ErrFileInUse:
        return mv_string("File In Use");
        break;
    case ErrInvalidArgument:
        return mv_string("Invalid Argument");
        break;
    default:
        return mv_string("Invalid Record Error");
        break;
    }
}

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

String path_name(String path);

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

RecordError get_record_error_code() {
    // TODO: account for all documented possible error codes.
#if OS_FAMILY == WINDOWS
  switch (GetLastError()) {
  case ERROR_FILE_NOT_FOUND:
      return ErrDoesNotExist;
  case ERROR_ACCESS_DENIED:
      return ErrPermissionDenied;
  case ERROR_BUSY:
      return ErrFileInUse; // TODO (INVESTIGATE) is this correct?
  case ERROR_ALREADY_EXISTS:
      return ErrAlreadyExists;
  default:
      // TODO: surely there's a better solution than to panic?
      panic(mv_string("Unrecognized error code."));
  }
#else
  switch (errno) {
  case EACCES:
      return ErrPermissionDenied;
  case ENOENT:
      return ErrDoesNotExist;
  case ETXTBSY:
      return ErrFileInUse;
  case EINVAL:
      return ErrInvalidArgument;
  default: {
      // TODO: surely there's a better solution than to panic?
      Allocator* a = get_std_allocator();
      panic(string_cat(mv_string("Unrecognized error code: "), string_i32(errno, a), a));
  }
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
        return (DirectoryResult) {.type = Err, .error = get_record_error_code()};
    }
#else
    char* dirstr = to_c_string(name, get_std_allocator());
    DIR* handle = opendir(dirstr);
    mem_free(dirstr, get_std_allocator());
    if (handle) {
        Directory* dir = mem_alloc(sizeof(Directory), alloc);
        *dir = (Directory) {
            .handle = handle,
            .gpa = alloc,
        };
        return (DirectoryResult) {.type = Ok, .directory = dir};
    } else {
        return (DirectoryResult) {.type = Err, .error = get_record_error_code()};
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

// TODO: add error return type 
void set_current_directory(String path) {
    // TODO: convert to valid path (consider encoding)
    char* c_path = to_c_string(path, get_std_allocator());
#if OS_FAMILY == WINDOWS
    SetCurrentDirectory(c_path);
#else
    chdir(c_path);
#endif
    mem_free(c_path, get_std_allocator());
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
    // TODO (PERFORMANCE). Try use API primitives that don't require the null byte.
    char* c_name = to_c_string(name, get_std_allocator());
    File* file = (File*)fopen(c_name, mode_str);
    mem_free(c_name, get_std_allocator());
    if (file) {
        return (FileResult) {.type = Ok, .file = file};
    } else {
        return (FileResult) {.type = Err, .error = get_record_error_code()};
    }

}

FileResult open_tempfile(Allocator *alloc) {
    File* file = (File*)tmpfile();
    if (file) {
        return (FileResult) {.type = Ok, .file = file};
    } else {
        return (FileResult) {.type = Err, .error = get_record_error_code()};
    }
}

void close_file(File *file) {
    fclose((FILE*)file);
}

String get_tmpdir(Allocator* a) {
#if OS_FAMILY == UNIX

    return mk_string("/tmp", a);

#elif OS_FAMILY == WINDOWS

    // TODO: account for the fact that we expect NO null terminator!
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

File* std_in() {
    return (File*) (void*)stdin;
}

File* std_out() {
    return (File*) (void*)stdout;
}

RecordResult copy_file(String source, String dest) {
#if OS_FAMILY == UNIX
    // TODO (PORT): see https://stackoverflow.com/questions/2180079/how-can-i-copy-a-file-on-unix-using-c
    // for non-linux support!
    RecordResult result = {.type = Ok};
    int input, output;
    char* c_source = to_c_string(source, get_std_allocator());
    if ((input = open(c_source, O_RDONLY)) == -1)
    {
        mem_free(c_source, get_std_allocator());
        return (RecordResult) {.type = Err, .error = get_record_error_code()};
    }
    mem_free(c_source, get_std_allocator());
    // Create new or truncate existing at destination
    char* c_dest = to_c_string(dest, get_std_allocator());
    if ((output = creat(c_dest, 0660)) == -1)
    {
        close(input);
        mem_free(c_dest, get_std_allocator());
        return (RecordResult) {.type = Err, .error = get_record_error_code()};
    }
    mem_free(c_dest, get_std_allocator());
    // sendfile will work with non-socket output (i.e. regular file) under
    // Linux 2.6.33+ and some other unixy systems.
    struct stat file_stat = {0};
    if (fstat(input, &file_stat) != 0) {
        result = (RecordResult) {.type = Err, .error = get_record_error_code()};
    }
    off_t copied = 0;
    while (result.type == Ok && copied < file_stat.st_size) {
        ssize_t written = sendfile(output, input, &copied, SSIZE_MAX);
        copied += written;
        if (written == -1) {
            result = (RecordResult) {.type = Err, .error = get_record_error_code()};
        }
    }
    close(input);
    close(output);

    return result;
#elif OS_FAMILY == WINDOWS
    if (CopyFile((LPCSTR)source.bytes, (LPCSTR)dest.bytes, false)) {
        return (RecordResult){.type = Ok};
    } else {
        return (RecordResult){.type = Err, .error = get_record_error_code()};
    }
#else
#error "copy file not supported for this os"
#endif
}

RecordResult delete_file(String path) {
    char* c_path = to_c_string(path, get_std_allocator());
#if OS_FAMILY == UNIX
    if (unlink(c_path) == 0) {
        mem_free(c_path, get_std_allocator());
        return (RecordResult){.type = Ok};
    } else {
        mem_free(c_path, get_std_allocator());
        return (RecordResult){.type = Err, .error = get_record_error_code()};
    }
#elif OS_FAMILY == WINDOWS
    if (DeleteFile(c_path)) {
        mem_free(c_path, get_std_allocator());
        return (RecordResult){.type = Ok};
    } else {
        mem_free(c_path, get_std_allocator());
        return (RecordResult){.type = Err, .error = get_record_error_code()};
    }
#else
#error "delete file not supported for this os"
#endif
}

RecordResult set_permissions(String file, FilePermissions perms) {
#if OS_FAMILY == UNIX
    RecordResult res = {.type = Ok};
    mode_t unix_perms = (perms.user << 6) | (perms.group << 3) | perms.other;
    char* c_filename = to_c_string(file, get_std_allocator());
    if (chmod(c_filename, unix_perms)) {
        res = (RecordResult) {.type = Err, .error = get_record_error_code()};
    }
    mem_free(c_filename, get_std_allocator());
    return res;
#elif OS_FAMILY == WINDOWS
    RecordResult res = {.type = Ok};
    return res;
#else
#error "delete file not supported for this os"
#endif
}


RecordResult create_directory(String dirname) {
    char* c_dirname = to_c_string(dirname, get_std_allocator());
    RecordResult res = {.type = Ok};
#if OS_FAMILY == WINDOWS
    if (!CreateDirectory(c_dirname, NULL)) {
        res = (RecordResult) {.type = Err, .error = get_record_error_code()};
    }
#elif OS_FAMILY == UNIX
    // TODO: add error checking
    mkdir(c_dirname, 0700);
#endif
    mem_free(c_dirname, get_std_allocator());
    return res;
}

RecordResult copy_directory_recur(String source, String dest) {
    Allocator* a = get_std_allocator();
    RecordInfo info = record_info(source);
    switch (info.type) {
    case RINotExists:
        return (RecordResult){.type = Err, .error = ErrDoesNotExist};
    case RIFile:
        return copy_file(source, dest);
    case RIDirectory: {
        RecordResult res = create_directory(dest);
        if (res.type == Err) {
            return (RecordResult) {.type = Err, .error = get_record_error_code()};
        }

        DirectoryResult dres = open_directory(source, a);
        if (dres.type == Err) {
            return (RecordResult) {.type = Err, .error = dres.error};
        }

        //create_direcotry
        Directory* dir = dres.directory;
        DirEntArray children = list_children(dir, a);
        for (size_t i = 0; i < children.len; i++) {
            String record_name = children.data[i].name;
            String sub_source_name = path_cat(source, record_name, a);
            String sub_dest_name = path_cat(dest, record_name, a);
            RecordResult res = copy_directory_recur(sub_source_name, sub_dest_name);
            mem_free(sub_source_name.bytes, a);
            mem_free(sub_dest_name.bytes, a);
            if (res.type == Err) {
                sdelete_dirent_array(children);
                close_directory(dir);
                return res;
            }
        }

        sdelete_dirent_array(children);
        close_directory(dir);
        return (RecordResult){.type = Ok};
    }
    }
    panic(mv_string("Internal bug in filesystem: copy_directory_recur shouldhave received a valid record info."));
}
 
RecordResult copy_directory(String source, String dest) {
    if (record_exists(dest)) {
        return (RecordResult){.type = Err, .error = ErrAlreadyExists};
    }
    return copy_directory_recur(source, dest);
}


#if OS_FAMILY == UNIX
static int unlink_cb(const char *fpath, const struct stat *sb, int typeflag, struct FTW *ftwbuf) {
    int rv = remove(fpath);

    if (rv)
        perror(fpath); // TODO(BUG): report error appropriately!

    return rv;
}
#endif


RecordResult delete_directory(String dirname, bool recursive) {
    char* c_dirname = to_c_string(dirname, get_std_allocator());
#if OS_FAMILY == UNIX
  if (!recursive) {
    int res = remove(c_dirname);
    mem_free(c_dirname, get_std_allocator());
    if (res == 0) {
        return (RecordResult){.type = Ok};
    } else {
        return (RecordResult){.type = Err, .error = get_record_error_code()};
    }
  } else {
      int res = nftw(c_dirname, unlink_cb, 64, FTW_DEPTH | FTW_PHYS);
      mem_free(c_dirname, get_std_allocator());
      if (res == 0) {
          return (RecordResult){.type = Ok};
      } else {
          return (RecordResult){.type = Err, .error = get_record_error_code()};
      }
  }
#elif OS_FAMILY == WINDOWS
    if (!recursive) {
        int res = RemoveDirectoryA(c_dirname);
        mem_free(c_dirname, get_std_allocator());
        if (res) {
            return (RecordResult){.type = Ok};
        } else {
            return (RecordResult){.type = Err, .error = get_record_error_code()};
        }
    } else {
        mem_free(c_dirname, get_std_allocator());

        Allocator* a = get_std_allocator();
        DirectoryResult res = open_directory(dirname, a);
        if (res.type == Err) return (RecordResult){.type = Err, .error = res.error};
        Directory* dir = res.directory;
        DirEntArray children = list_children(dir, a);

        for (size_t i = 0; i < children.len; i++) {
            String record_name = children.data[i].name;
            String record_fullname = path_cat(dirname, record_name, a);
            RecordInfo info = record_info(record_fullname);
            RecordResult current_res;
            switch (info.type) {
                case RINotExists:
                    current_res = (RecordResult){.type = Err, .error = ErrDoesNotExist};
                    break;
                case RIFile:
                    current_res = delete_file(record_fullname);
                break;
                case RIDirectory:
                    current_res = delete_directory(record_fullname, true);
                break;
            }

            mem_free(record_fullname.bytes, a);
            if (current_res.type == Err) {
                sdelete_dirent_array(children);
                close_directory(dir);
                return current_res;
            }
        }

        sdelete_dirent_array(children);
        close_directory(dir);
        return delete_directory(dirname, false);
        }
#else
#error "delete-directory not supported on this systsem"
#endif
}

bool record_exists(String path) {
  char* c_path = to_c_string(path, get_std_allocator());
#if OS_FAMILY == WINDOWS
  DWORD dwAttrib = GetFileAttributes(c_path);
  bool res = (dwAttrib != INVALID_FILE_ATTRIBUTES);
#elif OS_FAMILY == UNIX
  bool res = access(c_path, F_OK) == 0;
#endif
  mem_free(c_path, get_std_allocator());
  return res;
}

RecordInfo record_info(String path) {
  char* c_path = to_c_string(path, get_std_allocator());

#if OS_FAMILY == WINDOWS
  DWORD attributes = GetFileAttributesA(c_path);
  mem_free(c_path, get_std_allocator());
    
  if (attributes == INVALID_FILE_ATTRIBUTES) {
    // TODO (BUG): properly report errors
    return (RecordInfo){.type = RINotExists};
  }

  if (attributes & FILE_ATTRIBUTE_DIRECTORY) {
    return (RecordInfo) {
      .type = RIDirectory,
    };
  } else  {
    return (RecordInfo) {
      .type = RIFile,
      // TODO: file size...
    };
  } 

#elif OS_FAMILY == UNIX
  //return access((char*)path.bytes, F_OK) == 0;

  int input;
  if ((input = open(c_path, O_RDONLY)) == -1) {
      mem_free(c_path, get_std_allocator());
      // TODO (BUG): properly report errors!
      return (RecordInfo){.type = RINotExists};
  }
  mem_free(c_path, get_std_allocator());

  struct stat record_stat = {};
  if (fstat(input, &record_stat) != 0) {
      return (RecordInfo){.type = RINotExists};
  }

  switch (record_stat.st_mode & S_IFMT) {
  case S_IFDIR:
      return (RecordInfo) {
          .type = RIDirectory,
      };
  case S_IFREG:
      return (RecordInfo) {
          .type = RIFile,
          .file.file_size = record_stat.st_size,
      };
  default:
      // TODO: handle remaining modes...
      panic(mv_string("not able to handle this st_mode yet!"));
  }
#endif
}
