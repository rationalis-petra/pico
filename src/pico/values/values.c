#include <stdio.h>

#include "data/meta/amap_impl.h"
#include "data/meta/assoc_impl.h"
#include "data/amap.h"
#include "data/array.h"
#include "memory/std_allocator.h"
#include "pico/values/values.h"

AMAP_IMPL(pi_symbol, void*, sym_ptr)
ASSOC_IMPL(pi_symbol, void*, sym_ptr)


// The global symbol table
     static bool initialized = false;
static str_u64_amap symbol_table;
static ptr_array symbol_names;


void init_symtable(allocator a) {
    str_u64_amap map = mk_str_u64_amap(100, a);
    symbol_table.capacity = map.capacity;
    symbol_table.len = map.len;
    symbol_table.data = map.data;

    ptr_array arr = mk_ptr_array(100, a);
    symbol_names.size = arr.size;
    symbol_names.len = arr.len;
    symbol_names.data = arr.data;
    initialized = true;
}

void delete_string_pointer(string* ptr, allocator a) {
    delete_string(*ptr, a);
    mem_free(ptr, a);
}

void clear_symbols() {
    allocator a = get_std_allocator();
    delete_ptr_array(symbol_names, (void(*)(void*, allocator))delete_string_pointer, a);
    // The strings have already been deleted from the above array, so we use a shallow 
    // delete to avoid double-free
    // TODO: it's possible we are missing some deletes here (memory leak!)
    sdelete_str_u64_amap(symbol_table, a);
}

string* symbol_to_string(pi_symbol symbol) {
    allocator a = get_std_allocator();
    if (!initialized) init_symtable(a);
    return (string*)aref_ptr(symbol, symbol_names);
}

pi_symbol string_to_symbol(string str) {
    allocator a = get_std_allocator();
    if (!initialized) init_symtable(a);

    uint64_t new_symbol_id = symbol_names.len;
    pi_symbol* sym = str_u64_lookup(str, symbol_table);
    if (!sym) {

        string* map_str = (string*)mem_alloc(sizeof(string), a);
        string tmp_str = copy_string(str, a);
        map_str->memsize = tmp_str.memsize;
        map_str->bytes = tmp_str.bytes;
        push_ptr(map_str, &symbol_names, a);
        str_u64_insert(tmp_str, new_symbol_id, &symbol_table, a);
        sym = &new_symbol_id;
    }
    return *sym;
}

document* pretty_primop(pi_primop_t op, allocator a) {
    document* out = NULL;
    switch(op) {
    case AddI64: {
        out = mv_str_doc(mk_string("+", a), a);
        break;
    }
    case SubI64: {
        out = mv_str_doc(mk_string("-", a), a);
        break;
    }
    case MulI64: {
        out = mv_str_doc(mk_string("*", a), a);
        break;
    }
    case QuotI64: {
        out = mv_str_doc(mk_string("quot", a), a);
        break;
    }
    }
    return out;
}

