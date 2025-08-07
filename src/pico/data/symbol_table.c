#include "platform/signals.h"

#include "pico/data/symbol_table.h"

#define STR_CAPACITY 1048576

void init_symbol_table(SymbolTable* table, Allocator* a) {
    *table = (SymbolTable) {
        // Start with 1MB memory for strings
        .string_memory = mem_alloc(STR_CAPACITY, a),
        .string_len = 8, // 8, so we can use 0 as a null value :)
        .string_capacity = 1048576,

        .capacity = 1024,
        .cells = mem_alloc(sizeof(Cell) * 1024, a),
        .gpa = *a,
    };
    memset(table->cells, 0, sizeof(Cell) * 1024);
}

Name get_table_name(String name, SymbolTable* table) {
    // Calculate the FNV-1a hash
    // TODO: possibly use hash: https://www.reddit.com/r/rust/comments/17xgn0t/gxhash_a_new_extremely_fast_and_robust_hashing/

    #define FNV_PRIME 1099511628211LU
    #define FNV_OFFSET_BASIS 14695981039346656037LU

    uint64_t hash = FNV_OFFSET_BASIS;
    for (size_t i = 0; i < name.memsize; i++) {
        hash ^= name.bytes[i]; 
        hash *= FNV_PRIME;
    }
    
    uint64_t index = hash % table->capacity;
    
    Cell* cell = &table->cells[index];
    if (cell->string_index) {
        void* strptr = table->string_memory + cell->string_index;
        while (!(*(size_t*)strptr == name.memsize &&
                 memcmp(name.bytes, strptr+ sizeof(size_t), name.memsize) == 0)) {
            if (cell->next_node) {
                cell = cell->next_node;
                strptr = table->string_memory + cell->string_index;
            } else {
                // Add value
                cell->next_node = mem_alloc(sizeof(Cell), &table->gpa);
                cell = cell->next_node;

                size_t index = table->string_len;
                void* strptr = table->string_memory + index;
                *(size_t*)strptr = name.memsize;
                memcpy(strptr + sizeof(uint64_t), name.bytes, name.memsize);
                table->string_len += name.memsize + sizeof(uint64_t);
                table->string_len = (table->string_len + 7) & ~7;
                if (table->string_len > table->string_capacity)
                    panic(mv_string("table string cap exceeded"));

                *cell = (Cell){.string_index = index, .next_node = NULL };
                break;
            }
        }

    } else {
        // Add toplevel value
        size_t index = table->string_len;
        void* strptr = table->string_memory + index;
        *(size_t*)strptr = name.memsize;
        memcpy(strptr + sizeof(uint64_t), name.bytes, name.memsize);
        table->string_len += name.memsize + sizeof(uint64_t);
        table->string_len = (table->string_len + 7) & ~7;
        if (table->string_len > table->string_capacity)
            panic(mv_string("table string cap exceeded"));

        *cell = (Cell){.string_index = index, .next_node = NULL };
    }
    return cell->string_index;
}

String get_table_string(Name name, SymbolTable* table, Allocator* a) {
    void* strptr = table->string_memory + name;
    size_t size = *(size_t*)strptr;
    unsigned char* str = mem_alloc(size, a);
    memcpy(str, strptr + sizeof(size), size);
    return (String) {
        .memsize = size,
        .bytes = str,
    };
}

void delete_table(SymbolTable *table) {
    Allocator* a = &table->gpa;
    for (size_t i = 0; i < table->capacity; i++) {
        Cell cell = table->cells[i];
        if (cell.next_node) {
            do {
                Cell* ptr = cell.next_node;
                cell = *ptr;
                mem_free(ptr, a);
            } while (cell.next_node);
        }
    }

    mem_free(table->cells, a);
    mem_free(table->string_memory, a);
}


void clear_table(SymbolTable* table) {
    table->string_len = 1;
    memset(table->cells, 0, sizeof(Cell) * table->capacity);
}
