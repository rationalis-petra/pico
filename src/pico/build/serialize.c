#include "data/meta/array_impl.h"

#include "pico/data/meta/path_table_header.h"
#include "pico/data/meta/path_table_impl.h"
#include "pico/build/serialize.h"
#include "pico/values/modular_build.h"

U8Array serialize_fragments(ProgramFragments fragments, Allocator* a) {
    return mk_u8_array(8, a);
}
