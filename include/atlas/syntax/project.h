#ifndef __ATLAS_SYNTAX_PROJECT_H
#define __ATLAS_SYNTAX_PROJECT_H

#include "pico/data/symbol_array.h"

typedef struct {
    Symbol name;
    SymbolArray dependencies;
} AtlPackage;

typedef struct {
    AtlPackage package;
} Project;

typedef struct {
    bool package;
} ProjectRecord;

Document* pretty_project(Project projet, Allocator* a);

#endif
