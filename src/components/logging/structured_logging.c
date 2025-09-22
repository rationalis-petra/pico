#include "components/logging/structured_logging.h"

typedef enum { LESection, LEString, LEDocument } LogEntryType;

typedef struct {
    String name;
    PtrArray entries;
} LogSection;

typedef struct {
    LogEntryType type;
    union {
        LogSection section;
        String string;
        Document* doc;
    };
} LogEntry;

struct Logger {
    PtrArray log_entries;
};

Logger *make_logger(Allocator *a) {
    Logger* logger = mem_alloc(sizeof(Logger), a);
    *logger = (Logger) {
        .log_entries = mk_ptr_array(32, a),
    };

    return logger;
}

_Thread_local Logger logger;

void set_threadlocal_logger(Logger *logger) {
}

void start_section(String name) {
}

void end_section() {
}

void log_str(String str) {
}

void log_doc(Document* str) {
}
