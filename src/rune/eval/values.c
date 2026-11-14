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
    RuneCoCase cocase;
} CoCaseRepr;

typedef struct {
    ValueInfo info;
    RuneDataType type;
} DataTypeRepr;

typedef struct {
    ValueInfo info;
    uint64_t level;
} UniverseRepr;

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
        case ValDataType: {
            DataTypeRepr* type = (void*)info;
            mem_free(type->type.names.data, heap->gpa);
            break;
        }
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
ValRef mk_rune_closure(RuneClosureEnv environment, Expr expr, ValueHeap* heap) {
    ClosureRepr* repr = create_obj(sizeof(ClosureRepr), ValClosure, heap);
    repr->closure = (RuneClosure) { 
        .expr = expr,
        .env = environment,
    };
    return (ValRef){(uintptr_t)repr};
}

ValRef mk_rune_curried_closure(RuneClosureEnv environment, Expr expr, size_t num_args, ValueHeap* heap) {
    ClosureRepr* repr = create_obj(sizeof(ClosureRepr), ValClosure, heap);
    repr->closure = (RuneClosure) { 
        .expr = expr,
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

ValRef mk_rune_cocase(ValRefOption src_type, RuneClosureEnv env, PatternMap fields, ValueHeap* heap) {
    CoCaseRepr* repr = create_obj(sizeof(CoCaseRepr), ValCoCase, heap);
    repr->cocase = (RuneCoCase) { 
        .type = src_type,
        .env = env,
        .fields = fields,
    };
    return (ValRef){(uintptr_t)repr};
}

RuneCoCase* get_rune_cocase(ValRef ref) {
    CoCaseRepr* repr = (void*)ref.ref;
#ifdef DEBUG_ASSERT
    if (repr->info.sort != ValCoCase) {
        panic(mv_string("getting rune record from non-record object!"));
    }
#endif
    return &repr->cocase;
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

ValRef mk_rune_data_type(ValRef universe, Name self, bool is_record, uint64_t num_names, uint64_t num_ops, ValueHeap* heap) {
    DataTypeRepr* repr = create_obj(sizeof(DataTypeRepr) + num_ops * sizeof(AlgOpDescriptor), ValDataType, heap);
    repr->type = (RuneDataType) {
      .universe = universe,
      .self = self,
      .is_record = is_record,
      .names = {
          .len = num_names,
          .data = mem_alloc(num_names * sizeof(Name), heap->gpa),
      },
      .len = num_ops,
    };
    return (ValRef){(uintptr_t)repr};
}

RuneDataType* get_rune_data_type(ValRef ref) {
    DataTypeRepr* repr = (void*)ref.ref;
#ifdef DEBUG_ASSERT
    if (repr->info.sort != ValDataType) {
        panic(mv_string("getting rune int from non-int object!"));
    }
#endif
    return &repr->type;
}

ValRef mk_rune_universe(uint64_t level, ValueHeap* heap) {
    UniverseRepr* repr = create_obj(sizeof(UniverseRepr), ValUniverse, heap);
    repr->level = level;
    return (ValRef){(uintptr_t)repr};
}

uint64_t get_rune_universe(ValRef ref) {
    UniverseRepr* repr = (void*)ref.ref;
#ifdef DEBUG_ASSERT
    if (repr->info.sort != ValUniverse) {
        panic(mv_string("getting rune universe from non-universe object!"));
    }
#endif
    return repr->level;
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
        if (sort != ValData) return false;
        RuneData* record = get_rune_data(ref);
        if (template.record.len != record->len) return false;
        size_t offset = 0;
        for (size_t i = 0; i < template.record.len; i++) {
            Bridge bfield = template.record.types[i].val;
            size_t align =  bridge_host_align(bfield);
            offset = align_to(offset, align);
            if (!get_as_host_val(record->values[i], bfield, data + offset, a)) return false;
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

