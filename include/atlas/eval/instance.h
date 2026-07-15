#ifndef __ATLAS_INSTANCE_H
#define __ATLAS_INSTANCE_H

#include "platform/memory/region.h"

#include "pico/data/error.h"
#include "pico/values/modular.h"

#include "atlas/data/error.h"
#include "atlas/syntax/stanza.h"
#include "atlas/syntax/project.h"

// ----------------------------------------------------------------------------
//    Atlas Instance
// ----------------------------------------------------------------------------
//  Store values
//
// ----------------------------------------------------------------------------


typedef struct AtlasInstance AtlasInstance;

AtlasInstance* make_atlas_instance(Allocator* a);
void delete_atlas_instance(AtlasInstance* instance);

/**
 * Given that a project has been loaded, lookup the 'target' in the atlas
 * configuration. If it is an executable, call the function which is designated
 * as the entry point. Otherwise, throw an error.
 */
void atlas_run(AtlasInstance* instance, String target, RegionAllocator* region, AtErrorPoint* point);

/**
 * Given that a project has been loaded, lookup the 'target' in the atlas
 * configuration. If it is an executable, produce a file whose execution starts
 * at the entry point, place it in the 'build/' directory, and call it either
 * '<target>' or '<target>.exe', where <target> is the value of the target
 * parameter. Otherwise, throw an error.
 */
void atlas_build(AtlasInstance* instance, String target, RegionAllocator* region, AtErrorPoint* point);

/**
 * Register a package so that projects can use it as a dependency.
 */
void register_package(AtlasInstance* instance, Package* package);


void set_instance_package(AtlasInstance* instance, Package* package);
void set_instance_project(AtlasInstance* instance, Project project);
void add_library(Library library, String path, AtlasInstance* instance);
void add_executable(Executable executable, String path, AtlasInstance* instance);

#endif
