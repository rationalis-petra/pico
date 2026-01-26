#ifndef __COMPONENTS_LOGGING_STRUCTURED_LOGGING_H
#define __COMPONENTS_LOGGING_STRUCTURED_LOGGING_H

#include "data/string.h"

#include "platform/memory/allocator.h"

#include "components/pretty/document.h"

// -----------------------------------------------------------------------------
// 
//  Structured Logging
// -------------------- 
// 
// The structured logger enables a type of logging which keeps track of context
//   such as entering/leaving functions, binding variables and so on.
//
// The structured logger can then either write human-readable text or log to 
//  a format intended to be read and then analysed by a graphical inspector.
//
// -----------------------------------------------------------------------------

typedef struct Logger Logger;

Logger* make_logger(Allocator* a);
void delete_logger(Logger* logger);

void log_to_ostream(Logger* logger, OStream* ostream);
void log_to_formatted_ostream(Logger* logger, uint16_t max_width, FormattedOStream* ostream);

void start_section(String name, Logger* logger);
void end_section(Logger* logger);

void log_str(String str, Logger* logger);
void log_doc(Document* doc, Logger* logger);

#endif
