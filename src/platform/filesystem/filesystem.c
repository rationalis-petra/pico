#include <stdio.h>

#include "platform/machine_info.h"
#include "platform/filesystem/filesystem.h"
#include "platform/signals.h"
#include "data/string.h"

struct File {
    FILE* handle; 
    Allocator gpa;
};

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
    FILE* handle = fopen((char*)name.bytes, mode);
    if (handle == NULL) return NULL;

    File* file = mem_alloc(sizeof(File), alloc);
    *file = (File) {
        .handle = handle,
        .gpa = *alloc,
    };
    return file;
}

void close_file(File *file) {
    fclose(file->handle);
    mem_free(file, &file->gpa);
}

// return true on failure
bool read_byte(File *file, uint8_t *out) {
    return !fread(out, sizeof(char), 1, file->handle);
}

U8Array read_chunk(File *file, bool limit, uint64_t max_size, Allocator *region) {
    if (limit) {
        U8Array bytes = mk_u8_array(max_size, region);

        // TODO (BUG): update this method to return error on read failure.
        bytes.len = fread(bytes.data, sizeof(uint8_t), max_size, file->handle);
        return bytes;
    } else {
        fseek(file->handle, 0, SEEK_END);
        long fsize = ftell(file->handle);
        fseek(file->handle, 0, SEEK_SET);  /* same as rewind(f); */
        U8Array bytes = mk_u8_array(fsize, region);

        bytes.len = fread(bytes.data, sizeof(uint8_t), fsize, file->handle);
        return bytes;
    }

}

bool write_byte(File *file, uint8_t out) {
    return !fwrite(&out, sizeof(uint8_t), 1, file->handle);
}

bool write_chunk(File* file, U8Array arr) {
    return !fwrite(arr.data, sizeof(uint8_t), arr.len, file->handle);
}
