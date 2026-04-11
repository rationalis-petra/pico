#ifndef __PICO_DATA_CLIENT_LIST_H
#define __PICO_DATA_CLIENT_LIST_H

#include <stdbool.h>
#include <stdint.h>
#include "pico/data/client/meta/list_header.h"

PICO_LIST_HEADER(void*, addr, Addr)

PICO_LIST_HEADER(uint8_t, U8, U8)
    
PICO_LIST_HEADER(int64_t, I64, I64)

#endif
