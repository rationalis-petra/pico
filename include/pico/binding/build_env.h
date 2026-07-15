#ifndef __PICO_BINDING_BUILD_ENV_H
#define __PICO_BINDING_BUILD_ENV_H

#include "platform/memory/allocator.h"
#include "pico/binding/environment.h"
#include "pico/data/path.h"

typedef struct PathEnv PathEnv;

typedef enum PathEntry_t {
  PEPath,
  PEErr,
} PathEntry_t;

typedef struct {
  PathEntry_t type;
  Path path;
} PathEntry;

PathEnv* mk_build_env(Allocator* a, Environment* env);
void delete_path_env(PathEnv* env, Allocator* a);

PathEntry path_env_lookup(Symbol s, PathEnv* env);

#endif
