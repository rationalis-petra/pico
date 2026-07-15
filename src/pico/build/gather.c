#include "data/meta/array_impl.h"

#include "pico/data/meta/path_table_header.h"
#include "pico/data/meta/path_table_impl.h"
#include "pico/build/gather.h"
#include "pico/values/modular_build.h"

ARRAY_COMMON_IMPL(CodeFragment, cf, CodeFragment);
ARRAY_COMMON_IMPL(DataFragment, df, DataFragment);

PATH_TABLE_HEADER(uint64_t, u64, U64);
PATH_TABLE_IMPL(uint64_t, u64, U64);

PATH_TABLE_HEADER(void*, ptr, Ptr);
PATH_TABLE_IMPL(void*, ptr, Ptr);

typedef struct {
  CodeFragmentArray code_frags;
  DataFragmentArray data_frags;
  size_t code_size;
  size_t data_size;

  PathPtrTable terms_to_link;
  PathU64Table built_fragments;
  BuildErrorPoint* point;
  Allocator* a;
} GatherState;

void gather_module(Module* module, GatherState* state, PtrArray* modules_to_process) {
  ModuleIndex index = start_iterating(module);
  ModuleFragment fragment;
  while (next_iterator(&index, &fragment, module, state->point, state->a)) {
      /** TODO: use module fragment to 
       */
      switch (fragment.type) {
      case CodeFragment_t: {
          CodeFragment frag = {
              .code_size = fragment.data.len,
              .binary = fragment.data.data,
          };
          state->code_size += frag.code_size;
          push_cf(frag, &state->code_frags);
          break;
      }
      case DataFragment_t: {
          DataFragment frag = {
              .type = DFString,
              .data_size = fragment.data.len,
              .data = fragment.data.data,
          };
          state->data_size += frag.data_size;
          push_df(frag, &state->data_frags);
          break;
      }
      case ModuleFragment_t: {
          push_ptr(fragment.module, modules_to_process);
          break;
      }
      default:
          panic(mv_string("Invalid module fragment generated during build gather process"));
          break;
      }
  }
}

void gather_package(Package* package, GatherState* state) {
    Module* root = package_root_module(package);
    PtrArray modules_to_process = mk_ptr_array(32, state->a);
    gather_module(root, state, &modules_to_process);
    while (modules_to_process.len != 0) {
        Module* module = pop_ptr(&modules_to_process);
        gather_module(module, state, &modules_to_process);
    }
}

ProgramFragments gather_fragments(Symbol entry_point, Module* module, BuildErrorPoint* point, Allocator* a) {

  /**
   * General overview of the gathering algorithm
   * -------------------------------------------
   * The global links in a module rely on there being for any given symbol
   * a single term that it maps to in the import map of a module. As we are
   * flattening all modules into a single array, we cannot rely on this
   * mapping. Instead, we use two mappings:
   * • For values that are still in modules, we use *paths*, which are
   *   guaranteed to be unique.
   * • For values that are in the fragment array, we use the index in that
   *   array as a unique identifier
   * 
   * To establish the mapping of module definitions to fragments, we keep two
   * hashmaps whose keys are paths:
   * • The first maps terms which have been fragmented into their fragment(s) 
   * • The sectond maps terms which have NOT been fragmented to which fragments
   *   need to be linked to them.
   * 
   */
  GatherState state = {
    .code_frags = mk_cf_array(4096, a),
    .data_frags = mk_df_array(4096, a),
    .code_size = 0,
    .data_size = 0,
    .terms_to_link = mk_ptr_path_table(4096, a),
    .built_fragments = mk_u64_path_table(4096, a),
    .point = point,
    .a = a,
  };

  Package* package = get_package(module);
  gather_package(package, &state);

  return (ProgramFragments) {
    .code_frags = state.code_frags,
    .data_frags = state.data_frags,
    .code_size = state.code_size,
    .data_size = state.data_size,
  };
}
