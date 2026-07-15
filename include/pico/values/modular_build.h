#ifndef __PICO_VALUES_MODULAR_BULID_H
#define __PICO_VALUES_MODULAR_BULID_H

#include "data/meta/assoc_header.h"

#include "pico/values/modular.h"
#include "pico/data/build_error.h"
#include "pico/data/path.h"

/**
 * Modular Build Interface
 * ========================
 * This is an interface that allows the build module (responsible for generating
 * executable/linkable files) access to the binary and linkage data needed to
 * produce a full executable.
 *
 * The Interface works in *fragments*. A fragment may consist whole or part of a
 * module definition, and corresponds precisely to:
 * • 1 code fragment = 1 proc
 * • 1 data fragment = 1 string or 1 literal syntax tree
 *
 * For example, the definition:
 * (def add-2 proc [x]
 *   (let [add-1 proc [y] seq
 *           (write-line "adding 1")
 *           (+ y 1)]
 *     (add-1 (add-1 x))))
 * 
 * Would break down into 3 fragments:
 *  • Code-fragment: binary for add-2
 *  • Code-fragment: binary for add-1
 *  • Data-fragment: string "adding 1"
 * 
 * The way that the builer gathers these fragments from the module is via the
 * iterator interface: the 'ModuleIndex' type represents the current fragment
 * that the module is at, and is intended to ONLY be modified by code in this
 * module - to create an index, use start_iterator, and to get the next
 * fragment, use next_iterator, which will update the provided index, write the
 * contents of the next fragment into the provided fragment pointer. The
 * function will return true if there was another fragment, or false if all
 * fragments have been iterated through and no value was written to the output
 * pointers.
 * 
 */

typedef struct {
  size_t entry;           // module entry index
  size_t component;       // is code/data?
  size_t value;           // 1st/2nd/3rd function/string/...
} ModuleIndex;

typedef enum : uint8_t {
  CodeFragment_t,
  DataFragment_t,
  ModuleFragment_t,
} FragmentType;

ASSOC_HEADER(uint64_t, Path, u64_path, U64Path);

typedef struct {
  FragmentType type;
  union {
    U8Array data;
    Module* module;
  };
} ModuleFragment;


/**
 * Iterator Functions 
 * ===================
 * For a full rundown on the iteration interface, see the comment at the top of
 * the file. 
 * 
 * As a quick overview, the intended is as follows:
 * ------------------------------------------------
 * ModuleIndex index = start_iterating(module);
 * BuildFragment frag;
 * while (next_iterator(&index, &frag, module)) {
 *   // Loop Body
 * }
 * ------------------------------------------------
 */
ModuleIndex start_iterating(Module* module);
bool next_iterator(ModuleIndex* index, ModuleFragment* fragment, Module* module, BuildErrorPoint* point, Allocator* a);

#endif
