#include <string.h>

#include "platform/signals.h"
#include "data/meta/assoc_impl.h"
#include "rune/eval/values.h"

ASSOC_IMPL(Name, ValRef, name_val, NameVal);

/**
 * The 'Value Info' struct contains information necessary to interpret and
 * manage the raw bytes of a value. Importantly, it contains:
 *  • The next value on the heap, as a pointer.
 *  • Whether the current value is a root (pinned) value.
 *  
 *  • The sort of the value.
 */
typedef struct ValueInfo ValueInfo;
struct ValueInfo {
    bool is_root;
    ValueInfo* next;
    ValueSort sort;
};

typedef struct {
    ValueInfo info;
    RuneClosure closure;
} ClosureRepr;

typedef struct {
    ValueInfo info;
    RuneData data;
} DataRepr;

typedef struct {
    ValueInfo info;
    RuneList list;
} ListRepr;

typedef struct {
    ValueInfo info;
    String string;
} StringRepr;

typedef struct {
    ValueInfo info;
    int64_t num;
} IntRepr;

struct ValueHeap {
    ValueInfo* begin;
    ValueInfo* end;

    Allocator* gpa;
};

ValueHeap* mk_value_heap(Allocator* a) {
    ValueHeap* vals = mem_alloc(sizeof(ValueHeap), a);
    *vals = (ValueHeap) {
        .begin = NULL,
        .end = NULL,
        .gpa = a,
    };
    return vals;
}

void delete_value_heap(ValueHeap* heap) {
    ValueInfo* info = heap->begin;
    while (info) {
        switch (info->sort) {
        case ValString: {
            StringRepr* string = (void*)info;
            mem_free(string->string.bytes, heap->gpa);
            break;
        }
        case ValList: {
            ListRepr* list = (void*)info;
            mem_free(list->list.data, heap->gpa);
            break;
        }
        default:
            break;
        }
        ValueInfo* next = info->next;
        mem_free(info, heap->gpa);
        info = next;
    };
    mem_free(heap, heap->gpa);
}

ValueSort get_sort(ValRef ref) {
    ValueInfo* repr = (void*)ref.ref;
    return repr->sort;
}

static void* create_obj(size_t size, ValueSort sort, ValueHeap* heap) {
    ValueInfo* repr = mem_alloc(size, heap->gpa);
    *repr = (ValueInfo) {
        .is_root = true,
        .next = NULL,
        .sort = sort,
    };
    if (heap->begin == NULL) {
        heap->begin = repr;
        heap->end = repr;
    } else {
        heap->end->next = repr;
        heap->end = repr;
    }
    return repr;
}

// Builtin Values
ValRef mk_rune_list(size_t num_elements, ValueHeap* heap) {
    ListRepr* repr = create_obj(sizeof(ListRepr), ValList, heap);
    repr->list = (RuneList) { 
        .len = num_elements,
        .data = mem_alloc(sizeof(ValRef) * num_elements, heap->gpa),
    };
    return (ValRef){(uintptr_t)repr};
}

RuneList get_rune_list(ValRef ref) {
    ListRepr* repr = (void*)ref.ref;
#ifdef DEBUG_ASSERT
    if (repr->info.sort != ValList) {
        panic(mv_string("getting rune list from non-list object!"));
    }
#endif
    return repr->list;
}

ValRef get_elt(RuneList list, uint32_t idx, ValueHeap* heap) {
    return (ValRef) {(uintptr_t)(list.data + idx)};
}

void set_elt(RuneList list, uint32_t idx, ValRef val, ValueHeap* heap) {
    panic(mv_string("implement set_elt"));
}

ValRef mk_rune_string(size_t memsize, ValueHeap* heap) {
    StringRepr* repr = create_obj(sizeof(StringRepr), ValString, heap);
    repr->string = (String) { 
        .memsize = memsize,
        .bytes = mem_alloc(memsize, heap->gpa),
    };
    return (ValRef){(uintptr_t)repr};
}

String get_rune_string(ValRef ref) {
    StringRepr* repr = (void*)ref.ref;
    return repr->string;
}

// Function.
ValRef mk_rune_closure(RuneClosureEnv environment, ExprRef ref, ValueHeap* heap) {
    ClosureRepr* repr = create_obj(sizeof(ClosureRepr), ValClosure, heap);
    repr->closure = (RuneClosure) { 
        .expr = ref,
        .env = environment,
    };
    return (ValRef){(uintptr_t)repr};
}

ValRef mk_rune_curried_closure(RuneClosureEnv environment, ExprRef ref, size_t num_args, ValueHeap* heap) {
    ClosureRepr* repr = create_obj(sizeof(ClosureRepr), ValClosure, heap);
    repr->closure = (RuneClosure) { 
        .expr = ref,
        .env = environment,
        .num_curried = num_args,
        .curried = mem_alloc(sizeof(ValRef) * num_args, heap->gpa),
    };
    return (ValRef){(uintptr_t)repr};
}

RuneClosure get_rune_closure(ValRef ref) {
    ClosureRepr* repr = (void*)ref.ref;
#ifdef DEBUG_ASSERT
    if (repr->info.sort != ValClosure) {
        panic(mv_string("getting rune closure from non-closure object!"));
    }
#endif
    return repr->closure;
}

// Data
ValRef mk_rune_data(Name tag, ValRefOption src_type, size_t capacity, ValueHeap* heap) {
    DataRepr* repr = create_obj(sizeof(DataRepr) + sizeof(ValRef) * capacity, ValData, heap);
    repr->data = (RuneData) { 
        .name = tag,
        .type = src_type,
        .len = capacity,
    };
    return (ValRef){(uintptr_t)repr};
}

RuneData* get_rune_data(ValRef ref) {
    DataRepr* repr = (void*)ref.ref;
#ifdef DEBUG_ASSERT
    if (repr->info.sort != ValData) {
        panic(mv_string("getting rune data from non-data object!"));
    }
#endif
    return &repr->data;
}

ValRef mk_rune_int(int64_t val, ValueHeap* heap) {
    IntRepr* repr = create_obj(sizeof(IntRepr), ValInt, heap);
    repr->num = val;
    return (ValRef){(uintptr_t)repr};
}

int64_t get_int(ValRef ref) {
    IntRepr* repr = (void*)ref.ref;
#ifdef DEBUG_ASSERT
    if (repr->info.sort != ValInt) {
        panic(mv_string("getting rune int from non-int object!"));
    }
#endif
    return repr->num;
}

bool rune_value_eql(ValRef actual, ValRef expected, ValueHeap* heap, Allocator* a) {
    ValueSort actual_sort = get_sort(actual);
    ValueSort expected_sort = get_sort(expected);

    if (actual_sort != expected_sort) {
        return false;
    }

    switch (actual_sort) {
    case ValClosure:
        return actual.ref == expected.ref;
    case ValData: {
        RuneData* actual_data = get_rune_data(actual);
        RuneData* expected_data = get_rune_data(expected);
        if (actual_data->len != expected_data->len) return false;
        if (actual_data->type.type != expected_data->type.type) return false;
        if (actual_data->type.type == Some) {
            if (!rune_value_eql(actual_data->type.val, expected_data->type.val, heap, a)) {
                return false;
            }
        }
        for (size_t i = 0; i < actual_data->len; i++) {
            if (!rune_value_eql(actual_data->values[i], expected_data->values[i], heap, a)) {
                return false;
            }
        }
        return true;
    }
    case ValInt:
        return get_int(actual) == get_int(expected);
    case ValString: {

        String actual_string = get_rune_string(actual);
        String expected_string = get_rune_string(expected);
        return string_eq(actual_string, expected_string);
    }
    case ValList: {
        RuneList actual_list = get_rune_list(actual);
        RuneList expected_list = get_rune_list(expected);
        if (actual_list.len != expected_list.len) {
            return false;
        }
        for (size_t i = 0; i < actual_list.len; i++) {
            if (!rune_value_eql(actual_list.data[i],
                                expected_list.data[i],
                                heap, a)) {
                return false;
            }
        }
        return true;
    }
    default:
        return actual.ref == expected.ref;
    }
}
