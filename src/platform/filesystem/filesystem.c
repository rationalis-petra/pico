#include <stdio.h>

#include "platform/machine_info.h"
#include "platform/filesystem/filesystem.h"
#include "platform/signals.h"
#include "data/string.h"

struct File {
    FILE* handle; 
    Allocator* gpa;
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
    }

    FILE* handle = fopen(name.bytes, mode);
    if (handle == NULL) return NULL;

    File* file = mem_alloc(sizeof(File), alloc);
    *file = (File) {
        file->handle = handle,
        file->gpa = alloc,
    };
    return file;
}

void close_file(File *file) {
    fclose(file->handle);
    mem_free(file, file->gpa);
}

bool read_byte(File *file, uint8_t *out) {
    return !fread(out, sizeof(char), 1, file->handle);
}

U8Array read_chunk(File *file, uint64_t max_size, Allocator *region) {
    U8Array bytes = mk_u8_array(max_size, region);

    // TODO (BUG): update this method to return error on read failure.
    size_t nread = fread(bytes.data, sizeof(uint8_t), max_size, file->handle);
    bytes.len = nread;
    return bytes;
}
