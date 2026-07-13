#include "platform/machine_info.h"
#if OS_FAMILY == UNIX

#include "pico/build/build.h"
#include "components/object_files/elf.h"

struct RelicProgram {
    Elf64_Header elf_header;
};

RelicProgram* build_program(Module* module, Symbol entry_point, Allocator* a) {
    RelicProgram* program = mem_alloc(sizeof(RelicProgram), a);
    program->elf_header = (Elf64_Header) {
        .magic = {0x1, 'E', 'L', 'F'},
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

        // Skip a bunch of fields that we fill in later...
        .header_size = sizeof(Elf64_Header),
    };

    /**
     * Two *segment* entries
     * - Code (executable)
     * - Data (readable)
     */

    /**
     * ?? *section* entries
     * - Code (executable)
     * - Data (readable)
     */

    /**
     * Build Code and Data Segments
     */

    /** Step 1: gather dependencies
     *  Decompose the program into a flat list of functions or data entries which are linked to either:
     *  - Other functions
     *  - Other data entries 
     * Each of these entries stores link data with them
     */

    return program;
}

void write_program(RelicProgram* program, String filename) {
    // 
}

void link_program(String program, String lib, String out_name) {

}

#endif
