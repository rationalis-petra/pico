#include "rune/eval/values.h"


struct ValueHeap {
    uint32_t closure_len;
    uint32_t closure_capacity;
    RuneClosure* closures;

    uint32_t prim_len;
    uint32_t prim_capacity;
    uint64_t* prims;

    Allocator* gpa;
};

ValueHeap* mk_value_heap(Allocator* a) {
    ValueHeap* vals = mem_alloc(sizeof(ValueHeap), a);
    *vals = (ValueHeap) {
        .closure_len = 0,
        .closure_capacity = 32,
        .closures = mem_alloc(sizeof(RuneClosure) * 32, a),

        .prim_len = 0,
        .prim_capacity = 128,
        .prims = mem_alloc(sizeof(uint64_t) * 128, a),

        .gpa = a,
    };
    return vals;
}

// Handle to some value not managed by rune.
ValRef mk_rune_handle(void* data); 

// Builtin Values
ValRef mk_rune_int(int64_t val); 
ValRef mk_rune_list(size_t num_elements); 
ValRef mk_rune_string(size_t memsize);

// Function.
ValRef create_rune_closure(RuneClosureEnv environment, ExprFn fn); 
