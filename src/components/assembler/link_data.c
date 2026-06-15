#include "components/assembler/link_data.h"

ClosureLink copy_closure_link(ClosureLink link, PiAllocator* a) {
    return (ClosureLink) {
        .fn_start = link.fn_start,
        .defsite = link.defsite,
        .closure_type = copy_pi_type_p(link.closure_type, a),
        .inner_type = copy_pi_type_p(link.inner_type, a),
    };
}

void delete_closure_link(ClosureLink link, PiAllocator* a) {
  delete_pi_type_p(link.closure_type, a);
  delete_pi_type_p(link.inner_type, a);
}
