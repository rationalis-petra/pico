#ifndef __ATLAS_EVAL_VALUES_H
#define __ATLAS_EVAL_VALUES_H

#include <stdint.h>
#include "data/meta/assoc_header.h"
#include "pico/values/values.h"

#include "rune/syntax/expression.h"

typedef struct {
  uint32_t handle;
} ValRef;
ASSOC_HEADER(Name, ValRef, name_val, NameVal);

/**
 * To know how to run a function, we need the following lexical information:
 * 1. The Global environment: all atlas files share a single, global
 *    environment. We always have access to this, so it is not needed in a
 *    closure.
 * 2. The (local) lexical context. For now, we eschew 'fancy' representations like
 *    De-Brunjin indices/levels in favour of a simble maps of name -> value ref.
 */

typedef struct {
  String file_path;
  NameValAssoc local_vars;
} RuneClosureEnv;

typedef struct {
  RuneClosureEnv env;
  ExprFn fn;
} RuneClosure;

typedef struct {
  ValRef start;
  uint32_t len;
} RuneList;

typedef struct {
  uint32_t start;
  uint32_t len;
} RuneString;

/**
 * Notes on values/evaluation
 * =============================
 * • Instead of a garbage collector, all 'values' live in a single pool that is
 *   allocated upon instance creation, and destroyed with the instance. Given
 *   that all values share a well-typed representation, we may easily implement
 *   either mark-sweep garbage collection, or (more likely) reference counting
 *   on values, as the atlas language is likely to be immutable with guaranteed
 *   termination (meaning that we don't need a gc's ability to handle cycles).
 * • 
 */

typedef struct Value Value;
typedef struct ValueHeap ValueHeap;

ValueHeap* mk_value_heap(Allocator* a);

// Handle to some value not managed by rune.
ValRef mk_rune_handle(void* data); 

// Builtin Values
ValRef mk_rune_int(int64_t val); 
ValRef mk_rune_list(size_t num_elements); 
ValRef mk_rune_string(size_t memsize);

// Function.
ValRef create_rune_closure(RuneClosureEnv environment, ExprFn fn); 

#endif
