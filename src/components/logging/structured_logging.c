#include "components/logging/structured_logging.h"
#include "components/pretty/stream_printer.h"

typedef enum { LESection, LEString, LEDocument } LogEntryType;

typedef struct LogEntry LogEntry;
typedef struct {
    String name;
    PtrArray entries;
    LogEntry* parent;
} LogSection;

struct LogEntry {
    LogEntryType type;
    union {
        LogSection section;
        String string;
        Document* doc;
    };
};

struct Logger {
    LogEntry root_entry;
    LogEntry* current_entry;
    Allocator* gpa;
};

Logger* make_logger(Allocator* a) {
    LogEntry root = {
        .type = LESection,
        .section.name = mv_string("root"),
        .section.entries = mk_ptr_array(32, a),
    };

    Logger* logger = mem_alloc(sizeof(Logger), a);
    *logger = (Logger) {
        .root_entry = root,
        .current_entry = &logger->root_entry,
        .gpa = a,
    };

    return logger;
}

void delete_logentry(LogEntry* entry, Allocator* a) {
  if (entry->type == LESection) {
      for (size_t i = 0; i < entry->section.entries.len; i++) {
          delete_logentry(entry->section.entries.data[i], a);
          mem_free(entry->section.entries.data[i], a);
      }
      sdelete_ptr_array(entry->section.entries);
  }
}

void delete_logger(Logger* logger) {
    delete_logentry(&logger->root_entry, logger->gpa);
    mem_free(logger, logger->gpa);
}

void start_section(String name, Logger* logger) {
    LogEntry* entry = mem_alloc(sizeof(LogEntry), logger->gpa);
    *entry = (LogEntry) {
        .type = LESection,
        .section.name = name,
        .section.parent = logger->current_entry,
        .section.entries = mk_ptr_array(32, logger->gpa),
    };
    
    push_ptr(entry, &logger->current_entry->section.entries);
    logger->current_entry = entry;
}

void end_section(Logger* logger) {
    logger->current_entry = logger->current_entry->section.parent;
}

void log_str(String str, Logger* logger) {
    PtrArray* arr = &logger->current_entry->section.entries;
    LogEntry* entry = mem_alloc(sizeof(LogEntry), logger->gpa);
    *entry = (LogEntry) {
        .type = LEString,
        .string = str,
    };
    push_ptr(entry, arr);
}

void log_doc(Document* doc, Logger* logger) {
    PtrArray* arr = &logger->current_entry->section.entries;
    LogEntry* entry = mem_alloc(sizeof(LogEntry), logger->gpa);
    *entry = (LogEntry) {
        .type = LEDocument,
        .doc = doc,
    };
    push_ptr(entry, arr);
}

void log_to_ostream(Logger *logger, OStream *ostream) {
}

void write_formatted_entry(LogEntry entry, uint16_t indent, FormattedOStream* ostream, Allocator* a) {
    switch (entry.type) {
    case LEString:
        for (size_t i = 0; i < indent; i++) {
            write_fstring(mv_string(" "), ostream);
        }
        write_fstring(entry.string, ostream);
        write_fstring(mv_string("\n"), ostream);
        break;
    case LEDocument: {
        Document* doc = mv_nest_doc(indent, entry.doc, a);
        write_doc_formatted(doc, 120, ostream);
        mem_free(doc, a);
        write_fstring(mv_string("\n"), ostream);
        break;
    }
    case LESection:
        for (size_t i = 0; i < indent; i++) {
            write_fstring(mv_string(" "), ostream);
        }
        write_fstring(entry.section.name, ostream);
        write_fstring(mv_string("\n"), ostream);
        for (size_t i = 0; i < entry.section.entries.len; i++) {
            LogEntry* child = entry.section.entries.data[i];
            write_formatted_entry(*child, indent + 2, ostream, a);
        }
        break;
    }
}

void log_to_formatted_ostream(Logger* logger, uint16_t max_width, FormattedOStream* ostream) {
    PtrArray entries = logger->root_entry.section.entries;
    for (size_t i = 0; i < entries.len; i++) {
        LogEntry* child = entries.data[i];
        write_formatted_entry(*child, 0, ostream, logger->gpa);
    }
}
