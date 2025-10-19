#ifndef __COMPONENTS_LOGGING_STRUCTURED_LOGGING_H
#define __COMPONENTS_LOGGING_STRUCTURED_LOGGING_H

#include "data/string.h"

#include "platform/memory/allocator.h"

#include "components/pretty/document.h"

typedef struct Logger Logger;

Logger* make_logger(Allocator* a);

void set_threadlocal_logger(Logger* logger);

void start_section(String name);
void end_section();

void log_str(String str);
void log_doc(Document* str);

#endif
