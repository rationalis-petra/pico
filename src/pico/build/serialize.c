#include "data/meta/array_impl.h"

#include "pico/data/meta/path_table_header.h"
#include "pico/data/meta/path_table_impl.h"
#include "pico/build/serialize.h"
#include "pico/values/modular_build.h"

U8Array serialize_fragments(ProgramFragments fragments, Allocator* a) {
    U8Array out = mk_u8_array(fragments.code_size + fragments.data_size, a);

    for (size_t i = 0; i < fragments.code_frags.len; i++) {
        CodeFragment frag = fragments.code_frags.data[i];
        add_u8_chunk(frag.binary, frag.code_size, &out);
    }
    for (size_t i = 0; i < fragments.data_frags.len; i++) {
        DataFragment frag = fragments.data_frags.data[i];
        add_u8_chunk(frag.data, frag.data_size, &out);
    }

    return out;
}
