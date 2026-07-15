#include "platform/machine_info.h"
#if OS_FAMILY == WINDOWS

#include "platform/filesystem/filesystem.h"
#include "platform/signals.h"

#include "pico/build/build.h"
#include "pico/build/gather.h"
#include "pico/build/serialize.h"
#include "components/object_files/coff_pe.h"

struct RelicProgram {
    CoffHeader coff_header;
    U8Array segments;
};

RelicProgram* build_program(Module* module, Symbol entry_point, BuildErrorPoint* point, Allocator* a) {
    RelicProgram* program = mem_alloc(sizeof(RelicProgram), a);
    program->coff_header = (CoffHeader) {
        // TODO: also allow AARCH64
        .machine = CoffAMD64,
        .num_sections = 0,
        .timestamp = 0, // TODO: populate with current time as time_t
        .symtable_offset = 0,
        .num_symbols = 0,
        .optional_header_size = 0,
        .characteristics = LargeAddressAware | NoDebugInfo,
    };

    /**
     * Two *segment* entries
     * • Code (executable)
     * • Data (readable)
     */

    /**
     * ?? *section* entries
     * • Code (executable)
     * • Data (readable)
     */

    /**
     * Build Code and Data Segments
     */

    /** Step 1: Gather fragments
     *  Decompose the program into a flat list of functions or data entries which are linked to either:
     *  • Other functions
     *  • Other data entries 
     * Each of these entries stores link data with them
     */

    ProgramFragments fragments = gather_fragments(entry_point, module, point, a);

    /** Step 2: Compress fragments
     *  Not that we have decomposed the program into fragments with link data,
     *  we can start compressing these fragments into a continuous array of
     *  binary data, starting at the  
     *  • Other functions
     *  • Other data entries 
     * Each of these entries stores link data with them
     */

    program->segments = serialize_fragments(fragments, a);

    return program;
}

void write_program(RelicProgram* program, String filename, Allocator* a) {
    FileResult result = open_file(filename, Write, a);
    File* file = result.file;

    U8Array header_bytes = {
        .data = (void*)&program->coff_header,
        .len = sizeof(CoffHeader),
        .size = sizeof(CoffHeader),
    };

    write_chunk(file, header_bytes);
    write_chunk(file, program->segments);

    close_file(file);
}

void link_program(String program, String lib, String out_name) {
    panic(mv_string("Not yet implemented: link_program"));
}

#endif
