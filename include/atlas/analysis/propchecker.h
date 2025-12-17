#ifndef __ATLAS_ANALYSIS_PROPCHECKER_H
#define __ATLAS_ANALYSIS_PROPCHECKER_H

#include "pico/data/error.h"
#include "pico/data/symbol_array.h"

#include "atlas/syntax/concrete.h"
#include "atlas/syntax/stanza.h"

typedef struct PropSet PropSet;

PropSet* make_prop_set(size_t numprops, Allocator* a);
void delete_prop_set(PropSet* set);

void add_string_prop(String propname, String* location, PropSet* props);
void add_string_option_prop(String propname, StringOption* location, PropSet* props);
void add_string_array_prop(String propname, StringArray* location, PropSet* props);

void add_symbol_prop(String propname, Symbol* location, PropSet* props);
void add_symbol_option_prop(String propname, SymbolOption* location, PropSet* props);
void add_symbol_array_prop(String propname, SymbolArray* location, PropSet* props);

void parse_prop(RawAtlas term, PropSet* props, bool checks[], PiErrorPoint* point, Allocator* a);

void check_props(PropSet* props, bool checks[], Range range, PiErrorPoint* point, Allocator* a);

#endif
