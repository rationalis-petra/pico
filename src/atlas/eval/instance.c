#include "pico/data/sym_ptr_amap.h"
#include "pico/values/modular.h"
#include "pico/eval/call.h"

#include "atlas/eval/instance.h"

struct AtlasInstance {
    SymPtrAMap targets;
    bool project_set;
    Project project;
    Allocator* gpa;
};

typedef struct {
    Symbol name;
    StringOption filename;
    SymbolOption entrypoint;
    SymbolArray dependent_targets;
    StringArray dependent_files;
} AtlasTarget;


AtlasInstance* make_atlas_instance(Allocator* a) {
    AtlasInstance* instance = mem_alloc(sizeof(AtlasInstance), a);
    *instance = (AtlasInstance) {
        .targets = mk_sym_ptr_amap(32, a),
        .project_set = false,
        .gpa = a,
    };
    return instance;
}

void delete_atlas_instance(AtlasInstance* instance) {
    Allocator* a = instance->gpa;
    for (size_t i = 0; i < instance->targets.len; i++) {
        AtlasTarget* target = instance->targets.data[i].val;
        if (target->filename.type == Some) {
            mem_free(target->filename.value.bytes, a);
        }
        for (size_t i = 0; i < target->dependent_files.len; i++) {
            delete_string(target->dependent_files.data[i], a);
        }
        sdelete_string_array(target->dependent_files);
        sdelete_symbol_array(target->dependent_targets);
        mem_free(target, a);
    }

    sdelete_sym_ptr_amap(instance->targets);
    mem_free(instance, a);
}

static Module* atlas_load_target(AtlasInstance* instance, AtlasTarget* target, RegionAllocator* region, PiErrorPoint* point);

void atlas_run(AtlasInstance* instance, String target_name, RegionAllocator* region, PiErrorPoint* point) {
    Allocator ra = ra_to_gpa(region);
    Symbol sym = string_to_symbol(target_name);

    if (!instance->project_set) {
        PicoError err = {
            .range = (Range) {},
            .message = mk_str_doc(mv_string("No atlas project was found, please ensure there is an 'atlas-project' file in the current directory."), &ra),
        };
        throw_pi_error(point, err);
    }

    size_t tidx;
    if (sym_ptr_find(&tidx, sym, instance->targets)) {
        AtlasTarget* target = instance->targets.data[tidx].val;
        SymbolOption entry = target->entrypoint;
        if (entry.type == None) {
            PtrArray nodes = mk_ptr_array(5, &ra);
            push_ptr(mk_str_doc(mv_string("Target '"), &ra), &nodes);
            push_ptr(mk_str_doc(target_name, &ra), &nodes);
            push_ptr(mk_str_doc(mv_string("' has no entry-point and is therefore is not runnable."), &ra), &nodes);

            PicoError err = {
                .range = (Range) {},
                .message = mv_cat_doc(nodes, &ra),
            };
            throw_pi_error(point, err);
        }

        Module* module = atlas_load_target(instance, target, region, point);
        //ModuleEntry* e = get_def(entry.value, module);
        ModuleEntry* e = NULL;
        if (!e) {
            PtrArray nodes = mk_ptr_array(5, &ra);
            push_ptr(mk_str_doc(mv_string("Entry Point '"), &ra), &nodes);
            push_ptr(mk_str_doc(view_symbol_string(target->entrypoint.value), &ra), &nodes);
            push_ptr(mk_str_doc(mv_string("' in target '"), &ra), &nodes);
            push_ptr(mk_str_doc(target_name, &ra), &nodes);
            push_ptr(mk_str_doc(mv_string("' could not be found."), &ra), &nodes);

            PicoError err = {
                .range = (Range) {},
                .message = mv_cat_doc(nodes, &ra),
            };
            throw_pi_error(point, err);
        }

        // TODO: check that function has appropriate type, i.e. (Proc [] Unit)
        call_unit_fn(e->value, &ra);
    } else {
        PtrArray nodes = mk_ptr_array(5, &ra);
        push_ptr(mk_str_doc(mv_string("Unrecognized target: '"), &ra), &nodes);
        push_ptr(mk_str_doc(target_name, &ra), &nodes);
        push_ptr(mk_str_doc(mv_string("'"), &ra), &nodes);
        PicoError err = {
            .message = mv_cat_doc(nodes, &ra),
        };
        throw_pi_error(point, err);
    }
}

Module* atlas_load_target(AtlasInstance* instance, AtlasTarget* target, RegionAllocator* region, PiErrorPoint* point) {
    /* Loading algorithm
     *  - For now, assume that dependencies form not just a DAG, but a tree
     *  - Therefore, we do NOT need to check for duplicates and recursion
     *  - This will need to change as projects and dependencies get more complex;
     */

    // Does the target have an entry-point? If so, it is an executable,
    // otherwise it is a module.
    Module* out = NULL;
    if (target->entrypoint.type == Some) {
        // Load all dependencies first
    } else {
        // Create new Module
    }
    
    
    return out;
}

void set_instance_project(AtlasInstance *instance, Project project) {
    instance->project_set = true;
    instance->project = project;
}

void add_library(Library library, AtlasInstance* instance) {
    AtlasTarget* target = mem_alloc(sizeof(AtlasTarget), instance->gpa);

    StringOption filename = {.type = None};
    if (library.filename.type == Some) {
        filename = (StringOption) {
            .type = Some,
            .value = copy_string(library.filename.value, instance->gpa),
        };
    }

    *target = (AtlasTarget) {
        .name = library.name,
        .filename = filename,
        .entrypoint = (SymbolOption) {.type = None},
        .dependent_targets = mk_symbol_array(0, instance->gpa),
        .dependent_files = copy_string_array(library.submodules, copy_string, instance->gpa),
    };
    sym_ptr_insert(library.name, target, &instance->targets);
}

void add_executable(Executable executable, AtlasInstance* instance) {
    AtlasTarget* target = mem_alloc(sizeof(AtlasTarget), instance->gpa);

    *target = (AtlasTarget) {
        .name = executable.name,
        .filename = (StringOption) {
            .type = Some,
            .value = copy_string(executable.filename, instance->gpa),
        },
        .entrypoint = (SymbolOption) {.type = Some, .value = executable.entry_point },
        .dependent_targets = scopy_symbol_array(executable.dependencies, instance->gpa),
        .dependent_files = mk_string_array(0, instance->gpa),
    };
    sym_ptr_insert(executable.name, target, &instance->targets);
}

