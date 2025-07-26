#include "pico/stdlib/extra.h"
#include "pico/values/array.h"

void free_array(Array* arr) {
    Allocator perm = get_std_perm_allocator();
    mem_free(arr->data, &perm);
    mem_free(arr->shape.data, &perm);
}

Document* recursive_array_doc(const size_t depth, const size_t elem_size, size_t* index, Array arr, PrettyElem pelem, Allocator* a) { 
    if (depth == arr.shape.len) {
        Document* out = pelem.print_elem(arr.data + (*index * elem_size), pelem.context, a);
        index++;
        return out;
    } else {
        size_t len = arr.shape.data[depth];
        PtrArray nodes = mk_ptr_array(len, a);
        for (size_t i = 0; i < len; i++) {
            push_ptr(recursive_array_doc(depth + 1, elem_size, index, arr, pelem, a), &nodes);
        }
        return mk_paren_doc("⟨", "⟩", mv_group_doc(mv_sep_doc(nodes, a), a), a);
    }
}


Document* pretty_array(Array arr, uint64_t elem_size, PrettyElem pelem, Allocator* a) {
    return recursive_array_doc(0, elem_size, arr.data, arr, pelem, a);
}
