#ifndef __PICO_DATA_RENAME_ARRAY_H
#define __PICO_DATA_RENAME_ARRAY_H

#include "data/meta/array_header.h"
#include "pico/values/values.h"

typedef struct {
    Symbol l_name;
    Symbol r_name;
} Rename;

ARRAY_HEADER(Rename, rename, Rename)

#endif
