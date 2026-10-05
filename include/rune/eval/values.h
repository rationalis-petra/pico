#ifndef __ATLAS_EVAL_VALUES_H
#define __ATLAS_EVAL_VALUES_H

#include <stdint.h>
#include "data/meta/assoc_header.h"
#include "pico/values/values.h"

#include "rune/syntax/expression.h"

OPTION_TYPE(ValRef, ValRef)

/**
 * Rune values are all garbage-collected. To C, we expose only an opaque
 * reference/pointer. Upon creation, all values are pinned (roots). 
 * A C Program and any clients that use it must take care to unpin the value
 * when it is no longer needed.
 */

ASSOC_HEADER(Name, ValRef, name_val, NameVal);

/**
 * To know how to run a function, we need the following lexical information:
 * 1. The Global environment: all atlas files share a single, global
 *    environment. We always have access to this, so it is not needed in a
 *    closure.
 * 2. The (local) lexical context. For now, we eschew 'fancy' representations like
 *    De-Brunjin indices/levels in favour of a simple maps of name -> value ref.
 */

typedef struct {
  String file_path;
  NameValAssoc local_vars;
} RuneClosureEnv;

typedef struct {
    RuneClosureEnv env;
    ExprRef expr;
    size_t num_curried;
    ValRef* curried;
} RuneClosure;

// Note: for all user data-types, 
// • The type-tag is present to denote 
// • If the value is from a nominal type, and, if so, what type.
typedef struct {
    Option_t type;
    ValRef ref;
} TypeTag;

/**
 * We want our types to support: 
 *  Quotient Inductive Inductive Types (QIITs)
 * 
 * mutual
 *   Type Declarations
 *   Point Constructors
 *   Path Constructors
 *   Elimination / Transport Schemas
 */

typedef struct {
    // TODO: support QIITs
} RuneDataType;

/**
 * From an evaluation perspective, the only relevant details are 
 * 1. The nominal type (optional)
 * 2. The constructor tag (Name)
 * 3. The values in the constructor (may grow)
 */
typedef struct {
    Name name; 
    ValRefOption type;
    uint64_t len;
    ValRef values[];
} RuneData;

typedef struct {
} RuneCoData;

typedef struct {
    // TODO: support QIITs
} RuneRecordType;

typedef struct {
    Name tag;
    ValRef* values;
} RuneRecord;

// Express as composite data?
typedef struct {
  uint64_t len;
  ValRef* data;
} RuneList;

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

typedef enum {
  // User-defined data
  ValClosure,
  ValData,
  ValCoData,
  ValRecord,
  ValCoRecord,

  // User-Defined Types
  ValFnType,
  ValDataType,
  ValCoDataType,
  ValRecordType,
  ValCoRecordType,

  // Builtin Values
  ValInt,
  ValString,
  ValList,

  // Builtin Types
} ValueSort;

typedef struct ValueHeap ValueHeap;

ValueHeap* mk_value_heap(Allocator* a);
void delete_value_heap(ValueHeap* heap);

ValueSort get_sort(ValRef ref);

// Function (& Thunk)
ValRef mk_rune_closure(RuneClosureEnv environment, ExprRef ref, ValueHeap* heap);
ValRef mk_rune_curried_closure(RuneClosureEnv environment, ExprRef ref, size_t num_args, ValueHeap* heap);
RuneClosure get_rune_closure(ValRef val);

// QIIT (Data) 
ValRef mk_rune_data(Name tag, ValRefOption src_type, size_t capacity, ValueHeap* heap);
RuneData* get_rune_data(ValRef ref);

// Builtin Values

// Get/set elements of a list. (set should only be used during construction)
ValRef mk_rune_list(size_t num_elements, ValueHeap* heap); 
RuneList get_rune_list(ValRef ref);

// Create strings (& set string memory)
ValRef mk_rune_string(size_t memsize, ValueHeap* heap);
String get_rune_string(ValRef ref);

// Create Integers
ValRef mk_rune_int(int64_t val, ValueHeap* heap); 
int64_t get_int(ValRef ref);

/* 
 * Value Helper Functions
 */
bool rune_value_eql(ValRef actual, ValRef expected, ValueHeap* heap, Allocator* a);

#endif
