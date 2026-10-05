#ifndef __ATLAS_EVAL_TARGET_H
#define __ATLAS_EVAL_TARGET_H

#include "platform/memory/region.h"
#include "data/meta/array_header.h"

#include "pico/values/values.h"
#include "pico/values/modular.h"
#include "pico/data/string_array.h"

#include "rune/eval/values.h"
#include "atlas/data/error.h"

typedef enum {
    DepSubmoduleFile,
    DepExternalFile,
    DepTarget,
} DependencyType;

typedef struct AtlasTarget AtlasTarget;

typedef struct {
    DependencyType type;
    union {
        String filename;
        AtlasTarget* target;
    };
} Dependency;
ARRAY_HEADER(Dependency, dep, Dependency)

typedef struct {
    NameOption name;
    String path;
    StringOption filename;
    NameOption entrypoint;
    DependencyArray target_dependencies;
    StringArray file_dependencies;
    Module* module;
} PicoTarget;

typedef struct {
    StringArray provides; 
    DependencyArray target_dependencies;
    ValRef monad;
} GenericTarget;

struct AtlasTarget {
    bool is_generic;
    union {
        PicoTarget pico;
        GenericTarget generic;
    };
};

typedef enum : uint32_t {
    ActDependency,
    ActPicoTarget,
    ActGenericTarget,
    ActExecShell,
} AtlasAction;

AtlasTarget translate_from_rune(ValRef val, ValueHeap* heap, RegionAllocator* region, AtErrorPoint* point);

#endif
