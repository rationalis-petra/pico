#include <string.h>
#include "platform/signals.h"

#include "components/pretty/standard_types.h"
#include "pico/data/name_ptr_amap.h"

#include "rune/eval/eval.h"

RuneEvalResult bi_rune_join(ValSlice values, RuneEnv* env, RegionAllocator* region) {
    Allocator ra = ra_to_gpa(region);
    Allocator* a = &ra;
    size_t output_len = 0;
    for (size_t i = 0; i < values.len; i++) {
        ValRef ref = values.data[i];
        if (get_sort(ref) != ValString) {
            return (RuneEvalResult) {
                .type = AError,
                .error_message = mv_cstr_doc("join expects all string arguments", a),
            };
        }
        String string = get_rune_string(ref);
        output_len += string.memsize;
    }

    ValueHeap* heap = get_pools(env).value;
    ValRef ret = mk_rune_string(output_len, heap);
    String output = get_rune_string(ret);

    size_t output_offset = 0;
    for (size_t i = 0; i < values.len; i++) {
        ValRef ref = values.data[i];
        String string = get_rune_string(ref);
        memcpy(output.bytes + output_offset, string.bytes, string.memsize);
        output_offset += string.memsize;
    }
    return (RuneEvalResult) {
        .type = AValue,
        .value = ret,
    };
}

void populate_std_builtins(RuneEnv* env) {
    ValueHeap* heap = get_pools(env).value;

    Bridge fn_bridge = {
        .type = BFn,
        .fn.variadic = true,
    };
    void* fn = bi_rune_join;
    ValRef ref = mk_host_val(fn_bridge, &fn, heap);
    rune_add_val_def(string_to_name(mv_string("join")), ref, env);
}
