#ifndef __PICO_DATA_SYMBOL_TABLE_H
#define __PICO_DATA_SYMBOL_TABLE_H

/* The symbol table is a global data-structure used by pico to map 
 * strings (slow to compare) to symbols (fast to compare). 
 * 
 * The table is string-specfic, so takes advantage of foreknowledge of string layout 
 */

#include <string.h>

#include "platform/memory/allocator.h"
#include "pico/values/values.h"

typedef struct Cell Cell;
struct Cell {
    uint64_t string_index;
    Cell* next_node;
};

typedef struct {
    // String Data
    void* string_memory;
    size_t string_len;
    size_t string_capacity;

    // Table Data
    uint64_t capacity;
    Cell* cells;

    Allocator gpa;
} SymbolTable;

void init_symbol_table(SymbolTable* table, Allocator* a);

Name get_table_name(String name, SymbolTable* table);

String get_table_string(Name name, SymbolTable* table, Allocator* a);

void delete_table(SymbolTable *table);

void clear_table(SymbolTable* table);

#endif
