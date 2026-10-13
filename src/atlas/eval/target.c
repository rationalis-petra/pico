#include <stdalign.h>

#include "platform/signals.h"
#include "data/meta/array_impl.h"

#include "rune/eval/values.h"
#include "atlas/eval/target.h"

ARRAY_COMMON_IMPL(Dependency, dep, Dependency);

typedef struct TargetCell TargetCell;
struct TargetCell { 
    uint64_t hash;
    TargetRef target;
    TargetCell* next;
};

struct TargetMap {
    size_t target_len;
    size_t target_capacity;
    AtlasTarget* targets;
    Module** modules; 

    size_t hash_size;
    TargetCell* cells;
    bool* cells_set;
    Allocator* gpa;
};

TargetMap* mk_target_map(Allocator* a) {
  const size_t initial_cap = 32;
  TargetMap* map = mem_alloc(sizeof(TargetMap), a);
  *map = (TargetMap) {
    .target_len = 0,
    .target_capacity = initial_cap,
    .targets = mem_alloc(initial_cap * sizeof(AtlasTarget), a),
    .modules = mem_alloc(initial_cap * sizeof(Module*), a),
      
    .hash_size = initial_cap,
    .cells = mem_alloc(initial_cap * sizeof(TargetCell), a),
    .cells_set = mem_alloc(initial_cap * sizeof(bool), a),
    .gpa = a,
  };
  memset(map->modules, 0, initial_cap * sizeof(Module*));
  memset(map->cells_set, 0, initial_cap * sizeof(bool));
  return map;
}

void delete_target_map(TargetMap* map) {
    Allocator* a = map->gpa;
    mem_free(map->targets, a);
    mem_free(map->modules, a);
    mem_free(map->cells, a);
    mem_free(map->cells_set, a);
    mem_free(map, a);
}

static const uint64_t FNV_prime = 1099511628211; 
static const uint64_t hash_start = 14695981039346656037ULL;

static uint64_t hash_num(uint64_t num, uint64_t hash) {
    hash = hash ^ num;
    hash = hash * FNV_prime;
    return hash;
}

static uint64_t hash_string(String string, uint64_t hash) {
    // 64-bit FNV offsets and primes

    for (size_t i = 0; i < string.memsize; i++) {
        hash = hash ^ string.bytes[i];
        hash = hash * FNV_prime;
    }
    
    return hash;
}

static uint64_t hash_string_option(StringOption opt, uint64_t hash) {
    hash = hash_num(opt.type, hash);
    if (opt.type == Some) {
        hash = hash_string(opt.val, hash);
    }
    return hash;
}

static uint64_t hash_name_option(NameOption opt, uint64_t hash) {
    hash = hash_num(opt.type, hash);
    if (opt.type == Some) {
        hash = hash_num(opt.val, hash);
    }
    return hash;
}

static uint64_t hash_string_array(StringArray arr, uint64_t hash) {
    for (size_t i = 0; i < arr.len; i++) {
        hash = hash_string(arr.data[i], hash);
    }
    return hash;
}

static uint64_t hash_dependency_array(DependencyArray arr, uint64_t hash) {
    for (size_t i = 0; i < arr.len; i++) {
        Dependency dep = arr.data[i]; 
        hash = hash_num(dep.type, hash);
        switch (dep.type) {
        case DepSubmoduleFile:
        case DepExternalFile:
            hash = hash_string(dep.filename, hash);
            break;
        case DepTarget:
            hash = hash_num(dep.target.ref, hash);
            break;
        }
    }
    return hash;
}

static uint64_t finalize_hash(uint64_t hash) {
    /**
     * This finalization step was taken from path.c It may not be relevant to us?
     */
    hash ^= (hash >> 33);
    hash = (hash * 0xff51afd7ed558ccd);
    hash ^= (hash >> 33);
    
    return hash;
}

uint64_t hash_target(AtlasTarget target) {
    uint64_t hash = hash_start;
    hash = hash_num (target.type, hash);
    switch (target.type) {
    case TGeneric: {
        GenericTarget generic = target.generic;
        hash = hash_string_array(generic.provides, hash);
        hash = hash_dependency_array(generic.target_dependencies, hash);
        hash_num(generic.monad.ref, hash);
        break;
    }
    case TPico: {
        PicoTarget pico = target.pico;
        hash = hash_name_option(pico.name, hash);
        hash = hash_string(pico.path, hash);
        hash = hash_string_option(pico.filename, hash);
        hash = hash_name_option(pico.entrypoint, hash);
        hash = hash_dependency_array(pico.target_dependencies, hash);
        hash = hash_string_array(pico.file_dependencies, hash);
        hash = hash_num((uintptr_t)pico.module, hash);
        break;
    }
    case TFile: {
        FileTarget file = target.file;
        hash = hash_string(file.path, hash);
        hash = hash_string(file.name, hash);
    }
    }
    return finalize_hash(hash);
};

bool dep_array_eq(DependencyArray lhs, DependencyArray rhs) {
  if (lhs.len != rhs.len) return false;
  for (size_t i = 0; i < lhs.len; i++)  {
    Dependency lhd = lhs.data[i];
    Dependency rhd = rhs.data[i];
    if (lhd.type != rhd.type) return false;
    switch (lhd.type) {
    case DepSubmoduleFile:
    case DepExternalFile:
      return string_eq(lhd.filename, rhd.filename);
    case DepTarget:
      return lhd.target.ref == rhd.target.ref;
    }
  }
  return false;
}

bool target_eq(AtlasTarget lhs, AtlasTarget rhs) {
  if (lhs.type != rhs.type) return false;
  switch (lhs.type) {
  case TGeneric: {
    if (lhs.generic.provides.len != rhs.generic.provides.len) return false;
    for (size_t i = 0; i < lhs.generic.provides.len; i++) {
      if (!string_eq(lhs.generic.provides.data[i], rhs.generic.provides.data[i])) return false;
    }
    if (!dep_array_eq(lhs.generic.target_dependencies, rhs.generic.target_dependencies)) return false;
    return lhs.generic.monad.ref == rhs.generic.monad.ref;
  }
  case TPico: {
    return name_option_eq(lhs.pico.name, rhs.pico.name)
      && string_eq(lhs.pico.path, rhs.pico.path)
      && string_option_eq(lhs.pico.filename, rhs.pico.filename)
      && name_option_eq(lhs.pico.entrypoint, rhs.pico.entrypoint)
      && dep_array_eq(lhs.pico.target_dependencies, rhs.pico.target_dependencies)
      && string_array_eq(lhs.pico.file_dependencies, rhs.pico.file_dependencies)
      && lhs.pico.module == rhs.pico.module;
  } 
  case TFile: {
      return string_eq(lhs.file.path, rhs.file.path)
          && string_eq(lhs.file.name, rhs.file.name);
  }
  }
  panic(mv_string("invalid targets provided to target_eq."));
}

typedef struct {
    TargetMap* map;
    RuneEnv *env;
    RegionAllocator *region;
    AtErrorPoint* point;
} TranslateCbData;


/*
struct TargetMap {
    size_t len;
    size_t capacity;
    TargetCell* targets;
    uint64_t* hashes;
    Allocator* gpa;
};
*/

static BridgeCallback translate_cb;
static bool translate_cb(ValRef val, void* cb_data, size_t size, void* out) {
    TranslateCbData data = *(TranslateCbData*)(cb_data);
    TargetRef target = translate_from_rune(val, data.env, data.map, data.region, data.point);
    TargetRef* ref = out;
    *ref = target;
    return true;
}

static TargetRef target_to_ref(AtlasTarget target, TargetMap* map) {
    uint64_t target_hash = hash_target(target);
    size_t index = target_hash % map->hash_size;
    TargetCell* cell = &map->cells[index];
    bool is_set = map->cells_set[index];
    size_t path_len = 0;
    if (is_set) {
      while (true) {
        path_len++;
        TargetRef extant = cell->target;
        if (target_eq(target, map->targets[extant.ref])) {
          return extant;
        }
        if (cell->next) 
          cell = cell->next;
        else 
          break;
      }
    }

    // If we get here, it means that we did not find this target in the
    // hash-table and therefore did not return early. Insert the target into the
    // table and return.
    // TODO: size up map when path len > threshold (~4-8?)

    if (map->target_len >= map->target_capacity)  {
      map->target_capacity *= 2;
      map->targets = mem_realloc(map->targets, map->target_capacity * sizeof(AtlasTarget), map->gpa);
      map->modules = mem_realloc(map->modules, map->target_capacity * sizeof(Module*), map->gpa);
      memset(map->modules + (map->target_capacity/2), 0, (map->target_capacity/2));
    }
    TargetRef new_target = {map->target_len};
    map->target_len++;
    map->targets[new_target.ref] = target;

    if (is_set) {
      TargetCell* new_cell = mem_alloc(sizeof(TargetCell), map->gpa);
      *new_cell = (TargetCell) {
        .hash = target_hash,
        .target = new_target,
      };
      cell->next = new_cell;
    } else {
      map->cells_set[index] = true;
      *cell = (TargetCell) {
        .hash = target_hash,
        .target = new_target,
      };
    }
    return new_target;
}

typedef struct RuneTarget RuneTarget;
typedef enum : uint64_t {
    RTLibrary,
    RTExecutable,
    RTGeneric,
    RTFile,
} RuneTargetType;

typedef struct {
    size_t len;
    String* data;
} StringList;

typedef struct {
    size_t len;
    TargetRef* data;
} DependencyList;

typedef struct {
    String path;
    DependencyList deps;
    StringOption filename;
    StringList submodules;
} RuneLibrary;

typedef struct {
    String path;
    DependencyList deps;
    String filename;
    String entry_point;
} RuneExecutable;

    // run ::
    //  pure <any>
    //  bind run (fn [<any>] run)
    //  shell (list string)
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
        StringList shell; 
    };
};

typedef struct {
    String path;
    DependencyList deps;
    StringList provides;
    ValRef run;
} RuneGeneric;

typedef struct {
    String path;
    String name;
} RuneFile;

struct RuneTarget {
    RuneTargetType type;
    union {
        RuneLibrary library;
        RuneExecutable executable;
        RuneGeneric generic;
        RuneFile file;
    };
};

static AtlasTarget rune_to_atlas(RuneTarget target, Allocator* a);
static DependencyArray convert_dependency_list(DependencyList list, Allocator* a) {
    DependencyArray deps = mk_dep_array(list.len, a);
    for (size_t i = 0; i < list.len; i++) {
        Dependency dep = {
            .type = DepTarget,
            .target = list.data[i],
        };
        push_dep(dep, &deps);
    }
    return deps;
}

static AtlasTarget rune_to_atlas(RuneTarget target, Allocator* a) {
    switch (target.type) {
    case RTLibrary:
        return (AtlasTarget) {
            .type = TPico,
            .pico = {
                .path = target.library.path,
                .filename = target.library.filename,
                .target_dependencies = convert_dependency_list(target.library.deps, a),
                .file_dependencies = {
                    .len = target.library.submodules.len,
                    .size = target.library.submodules.len,
                    .data = target.library.submodules.data,
                },
            },
        };
    case RTExecutable:
        return (AtlasTarget) {
          .type = TPico,
          .pico = {
              .path = target.executable.path,
              .filename = {.type = Some, .val = target.executable.filename},
              .entrypoint = {
                  .type = Some,
                  .val = string_to_name(target.executable.entry_point)
              },
              .target_dependencies = convert_dependency_list(target.executable.deps, a),
            },
        };
    case RTGeneric:
        return (AtlasTarget) {
            .type = TGeneric,
            .generic = {
                .provides = {
                    .len = target.generic.provides.len,
                    .size = target.generic.provides.len,
                    .data = target.generic.provides.data,
                },
                .target_dependencies = convert_dependency_list(target.generic.deps, a),
                .monad = target.generic.run,
            }
        };
    case RTFile: 
        return (AtlasTarget) {
            .type = TFile,
            .file = {
                .path = target.file.path,
                .name = target.file.name,
            }
        };
    }
    panic(mv_string("Invalid rune target provided to rune_to_atlas"));
}

TargetRef translate_from_rune(ValRef val, RuneEnv* env, TargetMap* map, RegionAllocator* region, AtErrorPoint* point)  {
    Allocator a = ra_to_gpa(region);

    TranslateCbData trn_data = {
        .map = map,
        .env = env,
        .region = region,
        .point = point,
    };

    Bridge bstring = {.type = BString};
    Bridge bstring_list = {.type = BList, .element = &bstring};
    Bridge btarget = {
        .type = BCustom,
        .custom.c_size = sizeof(TargetRef),
        .custom.c_align = alignof(TargetRef),
        .custom.data = &trn_data,
        .custom.fn = translate_cb,
    };
    Bridge btarget_list = {.type = BList, .element = &btarget};

    BRecCell lib_types[] = {
        {.name = string_to_name(mv_string("path")),
         .val = bstring},
        {.name = string_to_name(mv_string("depends-on")),
         .val = btarget_list},
        {.name = string_to_name(mv_string("file")),
         .val = bstring,},
        {.name = string_to_name(mv_string("submodules")),
         .val = bstring_list,},
    };
    Bridge library_bridge = {
        .type = BRecord,
        .record = {
            .len = sizeof(lib_types) / sizeof(BRecCell),
            .types = lib_types,
        }
    };
    BRecCell exec_types[] = {
        {.name = string_to_name(mv_string("path")),
         .val = bstring},
        {.name = string_to_name(mv_string("depends-on")),
         .val = btarget_list},
        {.name = string_to_name(mv_string("file")),
            .val = bstring,},
        {.name = string_to_name(mv_string("entry-point")),
            .val = bstring,},
    };
    Bridge executable_bridge = {
        //   file :: string 
        //   entry-point :: symbol
        //   dependencies :: dependency list
        .type = BRecord,
        .record = {
            .len = sizeof(exec_types) / sizeof(BRecCell),
            .types = exec_types,
        }
    };

    // run ::
    //  pure <any>
    //  bind run (fn [<any>] run)
    //  shell (list string)
    Bridge run_bridge;
    Bridge bind_types[] = {
        {.type = BPtrWrap, .wrapped = &run_bridge},
        {.type = BPassthrough},
    };
    Bridge pass_bridge = {.type = BPassthrough};
    BridgeVariant run_variants[] = {
        {.name = string_to_name(mv_string("pure")),
         .len = 1,
         .types = &pass_bridge},
        {.name = string_to_name(mv_string("bind")),
         .len = 2,
         .types = bind_types,},
        {.name = string_to_name(mv_string("shell")),
         .len = 1,
         .types = &bstring_list,},
    };
    run_bridge = (Bridge) {
        .type = BData,
        .data = {
            .len = sizeof(run_variants) / sizeof(BridgeVariant),
            .variants = run_variants,
        },
    };

    BRecCell generic_types[] = {
        {.name = string_to_name(mv_string("path")),
         .val = bstring},
        {.name = string_to_name(mv_string("depends-on")),
         .val = btarget_list},
        {.name = string_to_name(mv_string("provides")),
            .val = bstring_list,},
        {.name = string_to_name(mv_string("run")),
            .val = run_bridge,},
    };
    Bridge generic_bridge = {
        .type = BRecord,
        .record = {
            .len = sizeof(exec_types) / sizeof(BRecCell),
            .types = generic_types,
        }
    };


    BRecCell file_types[] = {
        {.name = string_to_name(mv_string("path")),
         .val = bstring},
        {.name = string_to_name(mv_string("name")),
         .val = bstring},
    };
    Bridge file_bridge = {
        .type = BRecord,
        .record = {
            .len = sizeof(file_types) / sizeof(BRecCell),
            .types = file_types,
        }
    };

    BridgeVariant variants[] = {
        {.name = string_to_name(mv_string("library")),
            .len = 1,
            .types = &library_bridge,},
        {.name = string_to_name(mv_string("executable")),
            .len = 1,
            .types = &executable_bridge,},
        {.name = string_to_name(mv_string("generic")),
            .len = 1,
            .types = &generic_bridge,},
        {.name = string_to_name(mv_string("file")),
            .len = 1,
            .types = &file_bridge,},
    };
    Bridge target_bridge = {
        .type = BData,
        .data = {
            .len = sizeof(variants) / sizeof(BridgeVariant),
            .variants = variants,
        },
    };

    RuneTarget target;
    
    if (!get_as_host_val(val, target_bridge, &target, &a)) {
        PtrArray nodes = mk_ptr_array(4, &a);
        push_ptr(mv_cstr_doc("Value is not a target:", &a), &nodes);
        push_ptr(pretty_rune_value(val, env, &a), &nodes);
        AtlasError error = { 
            .message = mv_sep_doc(nodes, &a)
        };
        throw_at_error(point, error);
    }
    AtlasTarget atlas_target = rune_to_atlas(target, &a);
    return target_to_ref(atlas_target, map);
}

AtlasTarget deref_target(TargetRef ref, TargetMap* map) {
    return map->targets[ref.ref];
}

Module* target_module(TargetRef ref, TargetMap* map) {
    return map->modules[ref.ref];
}

void set_target_module(TargetRef ref, Module* module, TargetMap* map) {
    map->modules[ref.ref] = module;
}
