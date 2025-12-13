#include <stdio.h>
#include <string.h>

#include "platform/threads.h"
#include "data/array.h"
#include "components/pretty/standard_types.h"

#include "pico/values/values.h"
#include "pico/data/symbol_table.h"

// The global symbol table
static SymbolTable symbol_table;
size_t distinct_counter = 0;

// Helper functions
void init_symbols(Allocator* a) {
    init_symbol_table(&symbol_table, a);
    distinct_counter = 0;
}

void delete_string_pointer(String* ptr, Allocator* a) {
    delete_string(*ptr, a);
    mem_free(ptr, a);
}

String symbol_to_string(Symbol symbol, Allocator* a) {
    return get_table_string(symbol.name, &symbol_table, a);
}

String view_symbol_string(Symbol symbol) {
    return view_table_string(symbol.name, &symbol_table);
}

Symbol string_to_symbol(String string) {
    return (Symbol){ .name = string_to_name(string), .did = 0 };
}

Symbol string_to_unique_symbol(String string) {
    return (Symbol){ .name = string_to_name(string), .did = distinct_counter++ };
}

Name string_to_name(String string) {
    return get_table_name(string, &symbol_table);
}
String name_to_string(Name name, Allocator* a) {
    return get_table_string(name, &symbol_table, a);
}

void clear_symbols() {
    delete_table(&symbol_table);
}

bool symbol_eq(Symbol lhs, Symbol rhs) {
    return lhs.name == rhs.name && lhs.did == rhs.did;
}

int64_t symbol_cmp(Symbol lhs, Symbol rhs) {
    int64_t r1 = lhs.name - rhs.name;
    return r1 == 0 ? (int64_t)(lhs.did - rhs.did) : r1;
}

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-parameter"
void delete_symbol(Symbol s) {};
Symbol copy_symbol(Symbol s, Allocator* a) { return s; };
void delete_name(Name n) {};
Name copy_name(Name n, Allocator* a) { return n; };
#pragma GCC diagnostic pop

// Helper functions for dynamic variables
// Need to maintain a set of current/valid vars + default values
// so new threads know what to copy!
// 
// Implementation of dynamic variables
// Each thread has an array, sized for # of dynamic vars
//static 
_Thread_local PtrArray thread_dynamic_vars;
static Allocator* dynamic_var_allocator;
static PtrArray dynamic_var_metadata;
static PtrArray all_dynamic_vars;
static Mutex dynamic_var_lock;

// Metadata: 
typedef struct {
    bool free;
    size_t size;
    void* default_value;
} DVarMetadata;

void init_dynamic_vars(Allocator* a) {
    dynamic_var_allocator = a;
    mutex_init(&dynamic_var_lock); 
    all_dynamic_vars = mk_ptr_array(256, a);
    dynamic_var_metadata = mk_ptr_array(256, a);
}

void clear_dynamic_vars() {
    // Clear dynamic vars in all arrays
    for (size_t i = 0; i < all_dynamic_vars.len; i++) {
        PtrArray* arr = all_dynamic_vars.data[i];

        // Only delete the array if it hasn't already been deleted by thread_clear_dynamic_vars
        if (arr->data) {
            for (size_t j = 0; j < dynamic_var_metadata.len; j++) {
                DVarMetadata* data = dynamic_var_metadata.data[i];
                if (!data->free) mem_free(arr->data[j], dynamic_var_allocator);
            }
            sdelete_ptr_array(*arr);
        }
    }
    for (size_t i = 0; i < dynamic_var_metadata.len; i++) {
        DVarMetadata* data = dynamic_var_metadata.data[i];
        mem_free(data->default_value, dynamic_var_allocator);
        mem_free(data, dynamic_var_allocator);
    }

    sdelete_ptr_array(all_dynamic_vars);
    sdelete_ptr_array(dynamic_var_metadata);
}

void thread_init_dynamic_vars() {
    // Init just for this thread
    mutex_lock(&dynamic_var_lock);
    thread_dynamic_vars = mk_ptr_array(dynamic_var_metadata.size, dynamic_var_allocator);
    for (size_t i = 0; i < dynamic_var_metadata.len; i++) {
        DVarMetadata* meta = dynamic_var_metadata.data[i];
        thread_dynamic_vars.data[i] = mem_alloc(meta->size, dynamic_var_allocator);
        memcpy(thread_dynamic_vars.data[i], meta->default_value, meta->size);
    }
    push_ptr(&thread_dynamic_vars, &all_dynamic_vars);
    mutex_unlock(&dynamic_var_lock);
}

void thread_clear_dynamic_vars () {
    mutex_lock(&dynamic_var_lock);
    for (size_t i = 0; i < thread_dynamic_vars.len; i++) {
        mem_free(thread_dynamic_vars.data[i], dynamic_var_allocator);
    }
    sdelete_ptr_array(thread_dynamic_vars);

    thread_dynamic_vars.data = NULL;
    thread_dynamic_vars.len = 0;
    thread_dynamic_vars.size = 0;

    mutex_unlock(&dynamic_var_lock);
}

uint64_t mk_dynamic_var(size_t size, void* default_val) {
    mutex_lock(&dynamic_var_lock);
    // If there is space in the array, push back
    uint64_t dvar = dynamic_var_metadata.len;
    bool used_free = false;
    if (dynamic_var_metadata.len == dynamic_var_metadata.size) {
        for (size_t i = 0; i < dynamic_var_metadata.len; i++) {
            DVarMetadata* data = dynamic_var_metadata.data[i];
            if (data->free) {
                used_free = true;
                dvar = i;
                break;
            }
        }
    }

    // Appropriately setup the dynamic variable metadata
    DVarMetadata* data;
    if (used_free) {
        data = dynamic_var_metadata.data[dvar];
    } else {
        data = mem_alloc(sizeof(DVarMetadata), dynamic_var_allocator);
        push_ptr(data, &dynamic_var_metadata);
    }

    *data = (DVarMetadata) {
        .free = false,
        .size = size,
        .default_value = mem_alloc(size, dynamic_var_allocator),
    };
    memcpy(data->default_value, default_val, size);
    
    // Now, populate all existing dynamic variable arrays:
    if (used_free) {
        for (size_t i = 0; i < all_dynamic_vars.len; i++) {
            // TODO (UB): when a thread gets deleted/ends,
            PtrArray* dvars = all_dynamic_vars.data[i];
            dvars->data[dvar] = mem_alloc(size, dynamic_var_allocator);
            memcpy(dvars->data[dvar], default_val, size);
        }
    } else {
        for (size_t i = 0; i < all_dynamic_vars.len; i++) {
            // TODO (UB): when a thread gets deleted/ends,
            PtrArray* dvars = all_dynamic_vars.data[i];
            push_ptr(mem_alloc(size, dynamic_var_allocator), dvars);
            memcpy(dvars->data[dvar], default_val, size);
        }
    }

    mutex_unlock(&dynamic_var_lock);
    return dvar;
}

void* get_dynamic_val(uint64_t dvar) {
    return thread_dynamic_vars.data[dvar];
}

void set_dynamic_val(uint64_t dvar, void* data, size_t size) {
    void* ptr = thread_dynamic_vars.data[dvar];
    memcpy(ptr, data, size);
}

void delete_dynamic_var(uint64_t var) {
    DVarMetadata* data = dynamic_var_metadata.data[var];
    mem_free(data->default_value, dynamic_var_allocator);
    data->free = true;

    for (size_t i = 0; i < all_dynamic_vars.len; i++) {
        PtrArray* dvars = all_dynamic_vars.data[i];
        mem_free(dvars->data[var], dynamic_var_allocator);
    }
}

void* get_dynamic_memory() {
    return thread_dynamic_vars.data;
}

Document* pretty_former(TermFormer op, Allocator* a) {
    Document* out = NULL;
    switch(op) {
    case FDefine:
        out = mk_str_doc(mv_string("::define"), a);
        break;
    case FDeclare:
        out = mk_str_doc(mv_string("::declare"), a);
        break;
    case FImport:
        out = mk_str_doc(mv_string("::import"), a);
        break;

    case FApplication:
        out = mk_str_doc(mv_string("::application"), a);
        break;
    case FUnseal:
        out = mk_str_doc(mv_string("::unseal"), a);
        break;
    case FProcedure:
        out = mk_str_doc(mv_string("::procedure"), a);
        break;
    case FAll:
        out = mk_str_doc(mv_string("::all"), a);
        break;
    case FSeal:
        out = mk_str_doc(mv_string("::seal"), a);
        break;
    case FMacro:
        out = mk_str_doc(mv_string("::macro"), a);
        break;
    case FVariant:
        out = mk_str_doc(mv_string("::variant"), a);
        break;
    case FMatch:
        out = mk_str_doc(mv_string("::match"), a);
        break;
    case FStructure:
        out = mk_str_doc(mv_string("::structure"), a);
        break;
    case FProjector:
        out = mk_str_doc(mv_string("::projector"), a);
        break;
    case FDynamic:
        out = mk_str_doc(mv_string("::dynamic"), a);
        break;
    case FDynamicUse:
        out = mk_str_doc(mv_string("::use"), a);
        break;
    case FDynamicLet:
        out = mk_str_doc(mv_string("::bind"), a);
        break;
    case FDynamicSet:
        out = mk_str_doc(mv_string("::set"), a);
        break;
    case FInstance:
        out = mk_str_doc(mv_string("::instance"), a);
        break;
    case FGenArray:
        out = mk_str_doc(mv_string("::gen-array"), a);
        break;
    case FWith:
        out = mk_str_doc(mv_string("::with"), a);
        break;

    case FLet:
        out = mk_str_doc(mv_string("::let"), a);
        break;
    case FIf:
        out = mk_str_doc(mv_string("::if"), a);
        break;
    case FLabels:
        out = mk_str_doc(mv_string("::labels"), a);
        break;
    case FGoTo:
        out = mk_str_doc(mv_string("::goto"), a);
        break;
    case FWithReset:
        out = mk_str_doc(mv_string("::with-reset"), a);
        break;
    case FResetTo:
        out = mk_str_doc(mv_string("::reset-to"), a);
        break;
    case FSequence:
        out = mk_str_doc(mv_string("::sequence"), a);
        break;
    case FModule:
        out = mk_str_doc(mv_string("::module"), a);
        break;

    case FIs:
        out = mk_str_doc(mv_string("::is"), a);
        break;
    case FInTo:
        out = mk_str_doc(mv_string("::into"), a);
        break;
    case FOutOf:
        out = mk_str_doc(mv_string("::out-of"), a);
        break;
    case FName:
        out = mk_str_doc(mv_string("::name"), a);
        break;
    case FUnName:
        out = mk_str_doc(mv_string("::unname"), a);
        break;
    case FWiden:
        out = mk_str_doc(mv_string("::widen"), a);
        break;
    case FNarrow:
        out = mk_str_doc(mv_string("::narrow"), a);
        break;
    case FSizeOf:
        out = mk_str_doc(mv_string("::size-of"), a);
        break;
    case FAlignOf:
        out = mk_str_doc(mv_string("::align-of"), a);
        break;
    case FOffsetOf:
        out = mk_str_doc(mv_string("::offset-of"), a);
        break;

        // Type formers
    case FArrayType:
        out = mk_str_doc(mv_string("::Array"), a);
        break;
    case FStructType:
        out = mk_str_doc(mv_string("::Struct"), a);
        break;
    case FEnumType:
        out = mk_str_doc(mv_string("::enum"), a);
        break;
    case FProcType:
        out = mk_str_doc(mv_string("::Proc"), a);
        break;
    case FResetType:
        out = mk_str_doc(mv_string("::Reset"), a);
        break;
    case FDynamicType:
        out = mk_str_doc(mv_string("::Dynamic"), a);
        break;
    case FNamedType:
        out = mk_str_doc(mv_string("::Named"), a);
        break;
    case FDistinctType:
        out = mk_str_doc(mv_string("::Distinct"), a);
        break;
    case FOpaqueType:
        out = mk_str_doc(mv_string("::Opaque"), a);
        break;
    case FTraitType:
        out = mk_str_doc(mv_string("::Trait"), a);
        break;
    case FAllType:
        out = mk_str_doc(mv_string("::All"), a);
        break;
    case FSealedType:
        out = mk_str_doc(mv_string("::Sealed"), a);
        break;
    case FFamily:
        out = mk_str_doc(mv_string("::Family"), a);
        break;
    case FLiftCType:
        out = mk_str_doc(mv_string("::LiftCType"), a);
        break;

    case FReinterpretNative:
        out = mk_str_doc(mv_string("::reinterpret-native"), a);
        break;
    case FReinterpretRelic:
        out = mk_str_doc(mv_string("::reinterpret-relic"), a);
        break;
    case FConvertNative:
        out = mk_str_doc(mv_string("::convert-native"), a);
        break;
    case FConvertRelic:
        out = mk_str_doc(mv_string("::convert-relic"), a);
        break;
    case FTypeOf:
        out = mk_str_doc(mv_string("::type-of"), a);
        break;
    case FDescribe:
        out = mk_str_doc(mv_string("::describe"), a);
        break;
    case FQuote:
        out = mk_str_doc(mv_string("::quote"), a);
        break;
    case FCapture:
        out = mk_str_doc(mv_string("::capture"), a);
        break;
    }

    if (out == NULL) {
        PtrArray vals = mk_ptr_array(2, a);
        push_ptr(mk_str_doc(mv_string("Error printing former: invalid enum value:"), a), &vals);
        // TODO: pretty int (default enum size?): 
        push_ptr(pretty_i32(op, a), &vals);
        out = mv_sep_doc(vals, a);
    }

    return out;
}
