#include "platform/machine_info.h"
#if ARCH == AARCH64

#include "platform/signals.h"

#include "pico/codegen/backend-direct/internal.h"

void generate_c_call(void* cfn, Assembler* ass, Allocator* a, ErrorPoint* point) {
    // TODO: why are modules not in codegen depending on codegen internals???
    panic(mv_string("Not implemented: generate_c_call"));
}

#endif
