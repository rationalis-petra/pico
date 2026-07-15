#include "platform/machine_info.h"
#if OS_FAMILY == UNIX

#include "platform/filesystem/filesystem.h"
#include "platform/signals.h"

#include "pico/build/build.h"
#include "pico/build/gather.h"
#include "pico/build/serialize.h"
#include "components/object_files/elf.h"

struct RelicProgram {
    Elf64_Header elf_header;
    Elf64ProgramHeader code_segment_header;
    Elf64ProgramHeader data_segment_header;
    U8Array segments;
};

RelicProgram* build_program(Module* module, Symbol entry_point, BuildErrorPoint* point, Allocator* a) {
    RelicProgram* program = mem_alloc(sizeof(RelicProgram), a);
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

    program->elf_header = (Elf64_Header) {
        .magic = {0x7f, 'E', 'L', 'F'},
        .class = Elf_64bit,
        .endianness = Elf_Little_Endian,
        .version = 1, // There is only one version of elf
        // TODO: determine ABI based on machine_info
        .osabi = SystemV,
        .osabi_extra = 0,
        .padding = {0, 0, 0, 0, 0, 0, 0},

        // Object file that can be linked with others
        .type = Elf_Relocatable,
        // TODO: update based on machine_info!
        .architecture = AMD64,
        .version_2 = 1, // There is only one version of elf

        /** The program header is immediately after the elf header, so the
            program header table offset is the size of the elf header */
        .program_header_table = sizeof(Elf64_Header),
        .section_header_table = 0,

        .flags = 0,
        // Skip a bunch of fields that we fill in later...
        .header_size = sizeof(Elf64_Header),

        .program_header_entry_size = sizeof(Elf64ProgramHeader),
        .num_program_header_entries = 2,

        .section_hedaer_entry_size = 0,
        .num_section_header_entries = 0,

        .section_names = 0,
    };


    program->code_segment_header = (Elf64ProgramHeader) {
        .type = PTLoad,
        .flags = PFExecutable,
        .offset = sizeof(Elf64_Header) + 2 * sizeof(Elf64ProgramHeader),
        .virtual_address = 0x0,
        .physical_address = 0x0,
        .file_size = fragments.code_size,
        .memory_size = fragments.code_size,
        .alignment = 0,
    };
    program->data_segment_header = (Elf64ProgramHeader) {
        .type = PTLoad,
        .flags = PFReadable | PFWritable,
        .offset = sizeof(Elf64_Header) + 2 * sizeof(Elf64ProgramHeader) + fragments.code_size,
        .virtual_address = 0x0,
        .physical_address = 0x0,
        .file_size = fragments.data_size,
        .memory_size = fragments.data_size,
        .alignment = 0,
    };

    return program;
}

void write_program(RelicProgram* program, String filename, Allocator* a) {
    FileResult result = open_file(filename, Write, a);
    File* file = result.file;

    size_t header_size = sizeof(Elf64_Header) + 2 * sizeof(Elf64ProgramHeader);
    U8Array header_bytes = {
        .data = (void*)&program->elf_header,
        .len = header_size,
        .size = header_size,
    };

    write_chunk(file, header_bytes);
    write_chunk(file, program->segments);

    close_file(file);
}

void link_program(String program, String lib, String out_name) {
    panic(mv_string("Not yet implemented: link_program"));
}

#endif
