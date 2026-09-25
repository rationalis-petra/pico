#ifndef __ATLAS_EVAL_TARGET_H
#define __ATLAS_EVAL_TARGET_H

#include "data/meta/array_header.h"
#include "pico/values/values.h"
#include "pico/values/modular.h"
#include "pico/data/string_array.h"

#include "rune/eval/values.h"

typedef enum {
    Named,
    SubmoduleFile,
    ExternalFile,
} DependencyType;

typedef struct {
    DependencyType type;
    union {
        String filename;
        Name name;
    };
} Dependency;
ARRAY_HEADER(Dependency, dep, Dependency)

typedef struct {
    Name name;
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

typedef struct {
} Target;

#endif
