#ifndef __ATLAS_ANALYSIS_PROPCHECKER_H
#define __ATLAS_ANALYSIS_PROPCHECKER_H

#include "platform/memory/region.h"
#include "pico/data/error.h"

#include "pico/syntax/concrete.h"
#include "rune/syntax/expression.h"
#include "rune/analysis/abstraction.h"

typedef struct PropSet PropSet;

PropSet* make_prop_set(size_t numprops, Allocator* a);
void delete_prop_set(PropSet* set);

void add_name_prop(String propname, Name* location, PropSet* props);
void add_name_option_prop(String propname, NameOption* location, PropSet* props);
void add_name_array_prop(String propname, NameArray* location, PropSet* props);

// Expressions
void add_expr_prop(String propname, Expr* location, PropSet* props);
void add_expr_option_prop(String propname, Expr* location, PropSet* props);
void add_expr_array_prop(String propname, Expr* location, PropSet* props);

typedef void (*PropCb)(RawTree raw, PiErrorPoint *point, void* in, void* out);
void add_callback_prop(String propname, PropCb callback, void* in, void* out, PropSet* props);

void parse_prop(RawTree term, PropSet* props, bool checks[], HostCallbackData host_data, ExprPool* pool, PiErrorPoint* point, RegionAllocator* a);

void check_props(PropSet* props, bool checks[], Range range, PiErrorPoint* point, RegionAllocator* a);

#endif
