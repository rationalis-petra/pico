#include "platform/dynamic_library.h"
#include "platform/machine_info.h"

#if OS_FAMILY == UNIX 
#include <dlfcn.h>

struct DynLib {
    void* ptr;
};

Result open_lib(DynLib **out, String path, Allocator* a) {
    dlerror();

    *out = mem_alloc(sizeof(DynLib),a);
    **out = (DynLib) {
        .ptr = dlopen((char*)path.bytes, RTLD_LAZY),
    };

    char* error_message = dlerror();
    if (error_message) {
      return (Result) {
          .type = Err,
          .error_message = mk_string(error_message, a),
      };
    } else {
        return (Result) {.type = Ok};
    }
}

void close_lib(DynLib *lib) {
    dlclose(lib);
}

Result lib_sym(void **out, DynLib *lib, String symbol) {
    dlerror();
    *out = dlsym(lib->ptr, (char*)symbol.bytes);

    char* error_message = dlerror();
    if (error_message) {
      return (Result) {
          .type = Err,
          .error_message = mv_string(error_message),
      };
    } else {
        return (Result) {.type = Ok};
    }
}

#elif OS_FAMILY == WINDOWS
#include <windows.h>

struct DynLib {
    HINSTANCE instance;
};

Result open_lib(DynLib **out, String path, Allocator* a) {
    *out = mem_alloc(sizeof(DynLib), a);
    // TODO: use encoding change rather than raw cast
    HINSTANCE lib = LoadLibrary((char*)path.bytes);

    **out = (DynLib) {
        .instance = lib,
    };

    if (lib) {
        return (Result) {.type = Ok};
    } else {
        return (Result){
            .type = Err,
            // TODO: use GetLastError()
            .error_message = mv_string("Can't load dynamic lib"),
        };
    }
}

void close_lib(DynLib* lib) {
    // Note: returns bool true/false as succes/fail, probably want to return error
    //
    FreeLibrary(lib->instance);
}

Result lib_sym(void** out, DynLib* lib, String symbol) {
    // TODO: use encoding change rather than raw cast
    *out = GetProcAddress(lib->instance, (char*)symbol.bytes);
    if (*out) {
        return (Result) { .type = Ok };
    } else {
        return (Result) {
            .type = Err,
            // TODO (IMPROVEMENT): use GetLastError()
            .error_message = mv_string("Can't find symbol in dynamic lib"),
        };
    }
}

#else 
  #error "Not implemented: dynamic library for non linux/windows systems"
#endif
