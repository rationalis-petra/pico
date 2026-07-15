#include "platform/memory/allocator.h"

#include "pico/binding/build_env.h"

struct PathEnv {
    Environment* env;
    Allocator gpa;
};

PathEnv* mk_build_env(Allocator* a, Environment* env) {
    PathEnv* out = mem_alloc(sizeof(PathEnv), a);
    *out = (PathEnv) {
        .env = env,
        .gpa = *a,
    };
    return out;
}

void delete_path_env(PathEnv* env, Allocator* a) {
    mem_free(env, a);
}

PathEntry path_env_lookup(Symbol sym, PathEnv* env) {
    EnvEntry entry = env_lookup(sym, env->env);
    if (entry.type != ICValid) return (PathEntry) {.type = PEErr,};
    if (entry.is_module) return (PathEntry) {.type = PEErr,};

    Path path = mk_u64_array(8, &env->gpa);
    push_u64(sym.name, &path);

    Module* module;
    Module* parent = env_module(env->env);
    do {
        module = parent;
        push_u64(module_name(module).name, &path);
    } while ((parent = get_parent(module)) != NULL);
    Package* package = get_package(module);
    push_u64(package_name(package), &path);
    
    return (PathEntry) {
        .type = PEPath,
        .path = path,
    };
}
