#ifndef __OBJECT_FILES_ELF_H
#define __OBJECT_FILES_ELF_H

#include <stdint.h>


/* Step1: Representation of ELF file in memory:
 *  For now, we assume a 64-bit ELF file format
 */

/* Overview of the ELF format - ELF files are divided into N components
 * 
 * ELF HEADER
 *  The elf header
 * 
 *
 *
 * SECTION_HEADER_TABLE
 * PROGRAM_HEADER_TABLE
 */

#define ELF_N_IDENTIFICATION_BYTES 16

typedef enum : uint16_t {
    Elf_None = 0x0,
    Elf_Relocatable = 0x1,
    Elf_Executable = 0x2,
    Elf_DynLib = 0x3,
    Elf_Core = 0x4,
} ElfFileType;

typedef enum : uint16_t {
    Elf_AMD64 = 0x3E,
} ElfArchitecture;

typedef struct {
    // The set of identification bytes. Begins with
    // [ 0x7, 'E', 'L', 'F' ]
    // followed by class (32 or 64 bit), endianness, 
    uint8_t     identification[ELF_N_IDENTIFICATION_BYTES];

    ElfFileType type;
    ElfArchitecture architecture;
    uint32_t version;

    // Entry is the address at which to start execution (if one exists)
    // or NULL otherwise.
    uint64_t entry;

    // Offset ofthe program header table.
    uint64_t program_header_table;

    // Offset ofthe section header table.
    uint64_t section_header_table;

    // The interpretation of this field is dependent on architecture
    uint32_t flags; 

    // The size of this header, in bytes. Will be either 64 (64 bit) or 52 (32 bit)
    uint16_t header_size;

    // 
    uint16_t program_hedaer_entry_size;
    uint16_t num_program_header_entries;

    uint16_t section_hedaer_entry_size;
    uint16_t num_section_header_entries;

    // Which section header table entry contains section names?
    uint16_t section_names;
} Elf64_Header;


#endif
