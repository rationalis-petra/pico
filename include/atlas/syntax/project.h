#ifndef __ATLAS_SYNTAX_PROJECT_H
#define __ATLAS_SYNTAX_PROJECT_H

#include "pico/values/values.h"

typedef struct {
    Name name;
    NameArray dependencies;

    NameOption default_test;
    NameOption default_build;
    NameOption default_run;
} AtlPackage;

typedef struct {
    AtlPackage package;
} Project;

typedef struct {
    bool package;
} ProjectRecord;

Document* pretty_project(Project projet, Allocator* a);

#endif
