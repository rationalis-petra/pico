#ifndef __ATLAS_EVAL_TARGET_H
#define __ATLAS_EVAL_TARGET_H

#include "platform/memory/region.h"
#include "data/meta/array_header.h"
#include "data/slice.h"

#include "pico/values/values.h"
#include "pico/values/modular.h"
#include "pico/data/string_array.h"

#include "rune/eval/eval.h"
#include "atlas/data/error.h"

typedef enum {
    DepSubmoduleFile,
    DepExternalFile,
    DepTarget,
} DependencyType;

typedef struct {uint64_t ref;} TargetRef;

typedef struct {
    DependencyType type;
    union {
        String filename;
        TargetRef target;
    };
} Dependency;
ARRAY_HEADER(Dependency, dep, Dependency)

typedef enum {
    TPico,
    TGeneric,
    TFile,
} TargetType;

typedef struct {
    NameOption name;
    String path;
    StringOption filename;
    NameOption entrypoint;
    DependencyArray target_dependencies;
    StringSlice file_dependencies;
    Module* module;
} PicoTarget;

/** run ::
 *   pure <any>
 *   bind run (fn [<any>] run)
 *   shell (list string)
 */
typedef struct Run Run;
typedef enum : uint64_t {
    RPure, RBind, RShell
} RunType;

typedef struct {
    Run* run;
    ValRef closure;
} RunBind;

struct Run {
    RunType type;
    union {
        ValRef pure;
        RunBind bind;
        StringSlice shell; 
    };
};

typedef struct {
    String path;
    StringSlice provides; 
    DependencyArray target_dependencies;
    Run monad;
} GenericTarget;

typedef struct {
    String path; 
    String name;
} FileTarget;

typedef struct {
    TargetType type;
    union {
        PicoTarget pico;
        GenericTarget generic;
        FileTarget file;
    };
} AtlasTarget;

typedef struct TargetMap TargetMap;

TargetMap* mk_target_map(Allocator* a);
void delete_target_map(TargetMap* map);

TargetRef translate_from_rune(ValRef val, RuneEnv* env, TargetMap* map, RegionAllocator* region, AtErrorPoint* point);
AtlasTarget deref_target(TargetRef ref, TargetMap* map); 
Module* target_module(TargetRef ref, TargetMap* map); 
void set_target_module(TargetRef ref, Module* module, TargetMap* map); 

#endif
