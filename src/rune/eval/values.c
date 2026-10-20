#include <stdalign.h>
#include <string.h>

#include "platform/signals.h"
#include "data/meta/assoc_impl.h"
#include "rune/eval/values.h"
#include "rune/eval/eval.h"

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
    RuneRecord record;
} RecordRepr;

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

typedef struct {
    ValueInfo info;
    Bridge bridge;
    void* data;
} HostRepr;

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
        case ValHost: {
            HostRepr* host = (void*)info;
            mem_free(host->data, heap->gpa);
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
        .data = mem_alloc(sizeof(ValRef) * num_elements, heap->gpa),
        .len = num_elements,
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

ValRef mk_rune_record(ValRefOption src_type, size_t capacity, ValueHeap* heap) {
    RecordRepr* repr = create_obj(sizeof(RecordRepr) + sizeof(NameValPr) * capacity, ValRecord, heap);
    repr->record = (RuneRecord) { 
        .type = src_type,
        .len = capacity,
    };
    return (ValRef){(uintptr_t)repr};
}

RuneRecord* get_rune_record(ValRef ref) {
    RecordRepr* repr = (void*)ref.ref;
#ifdef DEBUG_ASSERT
    if (repr->info.sort != ValRecord) {
        panic(mv_string("getting rune record from non-record object!"));
    }
#endif
    return &repr->record;
}

ValRef mk_rune_int(int64_t val, ValueHeap* heap) {
    IntRepr* repr = create_obj(sizeof(IntRepr), ValInt, heap);
    repr->num = val;
    return (ValRef){(uintptr_t)repr};
}

int64_t get_rune_int(ValRef ref) {
    IntRepr* repr = (void*)ref.ref;
#ifdef DEBUG_ASSERT
    if (repr->info.sort != ValInt) {
        panic(mv_string("getting rune int from non-int object!"));
    }
#endif
    return repr->num;
}

size_t bridge_host_align(Bridge bridge) {
    switch (bridge.type) {
    case BPassthrough:
        return sizeof(ValRef);
    case BPtrWrap:
        return sizeof(void*);
    case BFn:
        return sizeof(void*);
    case BData: {
        size_t max_align = alignof(uint64_t);
        for (size_t i = 0; i < bridge.data.len; i++) {
            BridgeVariant variant = bridge.data.variants[i];
            for (size_t j = 0; j < variant.len; j++) {
                size_t align = bridge_host_align(variant.types[j]);
                max_align = align > max_align ? align : max_align;
            }
        }
        return max_align;
    }
    case BRecord: {
        size_t max_align = 0;
        for (size_t i = 0; i < bridge.record.len; i++) {
            size_t align = bridge_host_align(bridge.record.types[i].val);
            max_align = align > max_align ? align : max_align;
        }
        return max_align;
    }
    case BInt:
        return alignof(int64_t);
    case BString:
        return alignof(String);
    case BList:
        return alignof(RuneList);
    case BCustom:
        return bridge.custom.c_align;
    }
    panic(mv_string("Getting align of invalid bridge "));
}

static size_t align_to(size_t size, size_t align) {
    size_t rem = size % align;
    size_t pad = rem == 0 ? 0 : align - rem;
    return size + pad;
}

size_t bridge_host_size(Bridge bridge) {
    switch (bridge.type) {
    case BPassthrough:
        return alignof(ValRef);
    case BPtrWrap:
        return alignof(void*);
    case BFn:
        return sizeof(void*);
    case BData: {
        size_t max_size = 0;
        for (size_t i = 0; i < bridge.data.len; i++) {
            BridgeVariant variant = bridge.data.variants[i];
            size_t total = sizeof(uint64_t);
            for (size_t j = 0; j < variant.len; j++) {
                size_t size = bridge_host_size(variant.types[j]);
                size_t align = bridge_host_align(variant.types[j]);
                total = align_to(total, align);
                total += size;
            }
            max_size = total > max_size ? total : max_size;
        }
        return max_size;
    }
    case BRecord: {
        size_t total = 0;
        for (size_t i = 0; i < bridge.record.len; i++) {
            size_t size = bridge_host_size(bridge.record.types[i].val);
            size_t align = bridge_host_align(bridge.record.types[i].val);
            total = align_to(total, align);
            total += size;
        }
        return total;
    }
    case BInt:
        return sizeof(int64_t);
    case BString: {
        return sizeof(String);
    }
    case BList: {
        return sizeof(RuneList);
    }
    case BCustom: {
        return bridge.custom.c_size;
    }
    }
    panic(mv_string("Getting size of invalid bridge "));
}

ValRef mk_host_val(Bridge bridge, void* data, ValueHeap* heap) {
    HostRepr* repr = create_obj(sizeof(HostRepr), ValHost, heap);
    // TODO: add copying for more complex (non-function) objects.
    repr->data = mem_alloc(sizeof(void*), heap->gpa);
    memcpy(repr->data, data, sizeof(void*));
    repr->bridge = bridge;
    return (ValRef){(uintptr_t)repr};
}

Bridge get_host_bridge(ValRef ref) {
    HostRepr* repr = (void*)ref.ref;
#ifdef DEBUG_ASSERT
    if (repr->info.sort != ValHost) {
        panic(mv_string("getting rune host from non-host object!"));
    }
#endif
    return repr->bridge;
}
void get_host_val(ValRef ref, void* data) {
    HostRepr* repr = (void*)ref.ref;
#ifdef DEBUG_ASSERT
    if (repr->info.sort != ValHost) {
        panic(mv_string("getting rune host from non-host object!"));
    }
#endif
    // TODO: account for non-functions using bridge-size!
    memcpy(data, repr->data, sizeof(void*));
}

typedef struct {
    void* data;
    size_t len;
} List;

typedef struct {
    uintptr_t closure_data;
    size_t nargs;
    RuneEvalResult (*fn)(uintptr_t closure_data, ...) ;
} Callback;

bool get_as_host_val(ValRef ref, Bridge template, void* data, Allocator* a) {
    ValueSort sort = get_sort(ref);
    switch (template.type) {
    case BPassthrough:
        // passthrough
        *(ValRef*)data = ref;
        return true;
    case BPtrWrap: {
        Bridge wrapped = *template.wrapped;
        size_t size = bridge_host_size(wrapped);
        void* new_data = mem_alloc(size, a);
        bool out = get_as_host_val(ref, wrapped, new_data, a);
        *(void**)data = new_data;
        return out;
    }
    case BFn:
        if (sort != ValClosure) return false;
        // 'Host' Function is actually a callback: 
        // struct Callback {
        //   void* closur_data;
        //   size_t nargs;
        //   RuneEvalResult fn(void* closure_data, ...) 
        // }
        // with the ... being all ValRef's
        // Get number of arguments
        return false;
    case BData:
        if (sort != ValData) return false;
        RuneData* rdata = get_rune_data(ref);
        bool found = false;
        for (size_t i = 0; i < template.data.len; i++) {
            BridgeVariant variant = template.data.variants[i];
            if (rdata->name == variant.name) {
                if (rdata->len != variant.len) return false;
                uint64_t* tag = data;
                *tag = i;
                size_t offset = sizeof(uint64_t);
                for (size_t j = 0; j < variant.len; j++) {
                    offset = align_to(offset, bridge_host_align(variant.types[j]));
                    if (!get_as_host_val(rdata->values[j] ,variant.types[j], data + offset, a)) return false;
                    offset += bridge_host_size(variant.types[j]);
                }
                found = true;
                break;
            }
        }
        return found;
        break;
    case BRecord: {
        if (sort != ValRecord) return false;
        RuneRecord* record = get_rune_record(ref);
        if (template.record.len != record->len) return false;
        size_t offset = 0;
        for (size_t i = 0; i < template.record.len; i++) {
            Bridge bfield = template.record.types[i].val;
            size_t align =  bridge_host_align(bfield);
            offset = align_to(offset, align);
            if (!get_as_host_val(record->values[i].val, bfield, data + offset, a)) return false;
            size_t size = bridge_host_size(bfield);
            offset += size;
        }
        return true;
        break;
    }
    case BString: {
        if (sort != ValString) return false;
        String string = get_rune_string(ref);
        *(String*)data = string;
        return true;
    }
    case BInt: {
        if (sort != ValInt) return false;
        int64_t val = get_rune_int(ref);
        *(int64_t*)data = val;
        return true;
    }
    case BList: {
        if (sort != ValList) return false;
        RuneList list = get_rune_list(ref);
        size_t offset = 0;
        size_t elt_offset = align_to(bridge_host_size(*template.element), bridge_host_align(*template.element));
        void* dest = mem_alloc(elt_offset * list.len, a);
        for (size_t i = 0; i < list.len; i++) {
            if (!get_as_host_val(list.data[i], *template.element, dest + offset, a)) return false;
            offset += elt_offset;
        }
        *(List*)data = (List) {
            .data = dest,
            .len = list.len,
        };
        
        return true;
    }
    // Callback
    case BCustom: {
        return template.custom.fn(ref, template.custom.data, template.custom.c_size, data);
    }
    }
    return false;
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
    case ValRecord: {
        RuneRecord* actual_record = get_rune_record(actual);
        RuneRecord* expected_record = get_rune_record(expected);
        if (actual_record->len != expected_record->len) return false;
        if (actual_record->type.type != expected_record->type.type) return false;
        if (actual_record->type.type == Some) {
            if (!rune_value_eql(actual_record->type.val, expected_record->type.val, heap, a)) {
                return false;
            }
        }
        for (size_t i = 0; i < actual_record->len; i++) {
            if (actual_record->values[i].name != expected_record->values[i].name) {
                return false;
            }
            if (!rune_value_eql(actual_record->values[i].val, expected_record->values[i].val, heap, a)) {
                return false;
            }
        }
        return true;
    }
    case ValInt:
        return get_rune_int(actual) == get_rune_int(expected);
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
