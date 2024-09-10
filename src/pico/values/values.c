#include <stdio.h>

#include "data/amap.h"
#include "data/array.h"
#include "memory/std_allocator.h"
#include "pico/values/values.h"


// The global symbol table
     static bool initialized = false;
static StrU64AMap symbol_table;
static PtrArray symbol_names;


void init_symtable(Allocator* a) {
    StrU64AMap map = mk_str_u64_amap(100, a);
    symbol_table.capacity = map.capacity;
    symbol_table.len = map.len;
    symbol_table.data = map.data;

    PtrArray arr = mk_ptr_array(100, a);
    symbol_names.size = arr.size;
    symbol_names.len = arr.len;
    symbol_names.data = arr.data;
    initialized = true;
}

void delete_string_pointer(String* ptr, Allocator* a) {
    delete_string(*ptr, a);
    mem_free(ptr, a);
}

void clear_symbols() {
    Allocator* a = get_std_allocator();
    for (size_t i = 0; i < symbol_names.len; i++) {
        delete_string_pointer(symbol_names.data[i], a);
    }
    // The strings have already been deleted from the above array, so we use a shallow 
    // delete to avoid double-free
    sdelete_str_u64_amap(symbol_table);
}

void delete_symbol(Symbol s) {};
Symbol copy_symbol(Symbol s, Allocator* a) { return s; };

String* symbol_to_string(Symbol symbol) {
    Allocator* a = get_std_allocator();
    if (!initialized) init_symtable(a);
    return (String*)aref_ptr(symbol, symbol_names);
}

Symbol string_to_symbol(String str) {
    Allocator* a = get_std_allocator();
    if (!initialized) init_symtable(a);

    uint64_t new_symbol_id = symbol_names.len;
    Symbol* sym = str_u64_lookup(str, symbol_table);
    if (!sym) {

        String* map_str = (String*)mem_alloc(sizeof(String), a);
        String tmp_str = copy_string(str, a);
        map_str->memsize = tmp_str.memsize;
        map_str->bytes = tmp_str.bytes;
        push_ptr(map_str, &symbol_names);
        str_u64_insert(tmp_str, new_symbol_id, &symbol_table);
        sym = &new_symbol_id;
    }
    return *sym;
}

Document* pretty_former(TermFormer op, Allocator* a) {
    Document* out = NULL;
    switch(op) {
    case FDefine:
        out = mk_str_doc(mv_string("::Define"), a);
        break;
    case FApplication:
        out = mk_str_doc(mv_string("::Application"), a);
        break;
    case FProcedure:
        out = mk_str_doc(mv_string("::Procedure"), a);
        break;
    case FVariant:
        out = mk_str_doc(mv_string("::Variant"), a);
        break;
    case FMatch:
        out = mk_str_doc(mv_string("::Match"), a);
        break;
    case FStructure:
        out = mk_str_doc(mv_string("::Structure"), a);
        break;
    case FProjector:
        out = mk_str_doc(mv_string("::Projector"), a);
        break;
    case FIf:
        out = mk_str_doc(mv_string("::If"), a);
        break;
    case FLet:
        out = mk_str_doc(mv_string("::Let"), a);
        break;

        // Type formers
    case FStructType:
        out = mk_str_doc(mv_string("::StructureType"), a);
        break;
    case FEnumType:
        out = mk_str_doc(mv_string("::EnumType"), a);
        break;
    case FProcType:
        out = mk_str_doc(mv_string("::ProcedureType"), a);
        break;
    }
    return out;
}
