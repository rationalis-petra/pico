#include "platform/signals.h"
#include "data/meta/array_impl.h"

#include "rune/eval/values.h"
#include "atlas/eval/target.h"

ARRAY_COMMON_IMPL(Dependency, dep, Dependency)

/*
bool extract_dependency_array(ValRef ref, ValueHeap* heap, RegionAllocator* arena, DependencyArray* out) {
    Allocator a = ra_to_gpa(arena);
    Value value = get_val(ref, heap);
    if (value.type != ValString) return false;
    *out = get_string(value.string, heap, &a);
    return true;
}

bool extract_string_option(ValRef ref, ValueHeap* heap, RegionAllocator* arena, StringOption* out) {
    Allocator a = ra_to_gpa(arena);
    Value value = get_val(ref, heap);
    if (value.type != ValString) return false;
    *out = get_string(value.string, heap, &a);
    return true;
}

bool extract_string(ValRef ref, ValueHeap* heap, RegionAllocator* arena, String* out) {
    Allocator a = ra_to_gpa(arena);
    Value value = get_val(ref, heap);
    if (value.type != ValString) return false;
    *out = get_string(value.string, heap, &a);
    return true;
}

bool extract_name(ExprRef ref, ExprPool* pool, Name* out) {
    Expr expr = get_expr(ref, pool);
    if (expr.type != EVar) return false;
    *out = expr.var;
    return true;
}

// Error functions
_Noreturn static void not_target(ValRef val, ValueHeap* heap, RegionAllocator* region, AtErrorPoint* point);

AtlasTarget translate_from_rune(ValRef val, ValueHeap* heap, RegionAllocator* region, AtErrorPoint* point) {
    Allocator ra = ra_to_gpa(region);
    Value value = get_val(val, heap);
    if (value.type != ValData) {
        not_target(val, heap, region, point);
    }
    // TODO: Check that the data value's type is correct.

    // Check the tag
    if (value.data.tag == string_to_name(mv_string("library"))) {
        // Library : 
        //   dependencies, file, submodules
        PicoTarget ptarget = {};

        // Rec ought to be of type Record {dependencies file submodules}
        Value rec = get_val(get_val_elt(value.data.vals, 0, heap), heap);
        if (rec.type != ValRecord) {
            panic(mv_string("Expect system to have type-checked this as a record!"));
        }
        if (!extract_dependency_array(get_val_elt(rec.record.vals, 0, heap), &ptarget.target_dependencies)) {
        }
        if (!extract_string_option(get_val_elt(rec.record.vals, 2, heap), ptarget.filenme)) {
        }
        if (!extract_string_array(get_val_elt(rec.record.vals, 2, heap), &ptarget.file_dependencies)) {
        }

        return (AtlasTarget) { 
            .is_generic = false,
            .pico = ptarget
        };
    } else if (value.data.tag == string_to_name(mv_string("executable"))) {
        // Executable : 
        //   dependencies, file, submodules

        PicoTarget ptarget = {};

        return (AtlasTarget) { 
            .is_generic = false,
            .pico = ptarget
        };
    } else if (value.data.tag == string_to_name(mv_string("target"))) {
        
    } else {
        PtrArray nodes = mk_ptr_array(2, &ra);
        push_ptr(mk_str_doc(mv_string("Invalid target data constructor used: "), &ra), &nodes);
        push_ptr(pretty_rune_value(val, heap, &ra), &nodes);
        AtlasError err = {
            .message = mv_cat_doc(nodes, &ra),
        };
        throw_at_error(point, err);
    }
}

_Noreturn static void not_target(ValRef val, ValueHeap* heap, RegionAllocator* region, AtErrorPoint* point) {
    PtrArray nodes = mk_ptr_array(2, &ra);
    push_ptr(mk_str_doc(mv_string("Attempting to load value which is not target: "), &ra), &nodes);
    push_ptr(pretty_rune_value(val, heap, &ra), &nodes);
    AtlasError err = {
        .message = mv_cat_doc(nodes, &ra),
    };
    throw_at_error(point, err);
}

*/

AtlasTarget translate_from_rune(ValRef val, ValueHeap* heap, RegionAllocator* region, AtErrorPoint* point)  {
    panic(mv_string("Not implemented!!"));
}
