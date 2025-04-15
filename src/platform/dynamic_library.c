#include <stdio.h> 
#include "platform/dynamic_library.h"
#include "platform/machine_info.h"

#if OS_FAMILY == UNIX 
#include <dlfcn.h>

struct DynLib {
    void* ptr;
    Allocator* from;
};

Result open_lib(DynLib **out, String path, Allocator* a) {
    dlerror();

    *out = mem_alloc(sizeof(DynLib),a);
    **out = (DynLib) {
        .ptr = dlopen((char*)path.bytes, RTLD_LAZY),
        .from = a,
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
    *out = dlsym(lib, (char*)symbol.bytes);
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
    HInstance instance;
    Allocator* from;
};

Result open_lib(DynLib **out, String path) {
    *out = mem_alloc()
    HINSTANCE hinstLib = LoadLibrary(TEXT("MyPuts.dll"));;

    **out = (DynLib) {
        .instance = dlopen((char*)path.bytes, RTLD_LAZY),
        .from = a,
    };
}

void close_lib(DynLib* lib) {
    bool fFreeDLL = FreeLibrary(lib->instance);
}

Result lib_sym(void** out, DynLib* lib, String symbol) {
    *out = GetProcAddress(lib->instance, symbol.bytes);
    if (*out) {
        return (Result) { .type = Ok };
    } else {
        return (Result) {
            .type = Err,
            .error_message = mv_string("Can't find symbol in dynamic lib"),
        };
    }
}

#else 
  #error "Not implemented: dynamic library for non linux/windows systems"
#endif
