#ifndef __COMPONENTS_OBJECT_FILES_ELF_H
#define __COMPONENTS_OBJECT_FILES_ELF_H

#include <stdint.h>

/* Step 1: Representation of ELF file in memory:
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

#define ELF_N_MAGI_BYTES 4

typedef enum : uint8_t {
    Elf_32bit = 1,
    Elf_64bit = 2,
} ElfClass;

typedef enum : uint8_t {
    Elf_Little_Endian = 1,
    Elf_Big_Endian = 2,
} ElfEndianness;

typedef enum : uint8_t {
    SystemV  = 0x00,
    NetBSD   = 0x02,
    Linux    = 0x03,
    GNUHurd  = 0x03,
    FreeBSD  = 0x09,
    OpenBSD  = 0x0C,
    OpenVMS  = 0x0D,
    AROS     = 0x0F,
    CloudABI = 0x11,
} ElfOSABI;

typedef enum : uint16_t {
    Elf_None = 0x0,
    Elf_Relocatable = 0x1,
    Elf_Executable = 0x2,
    Elf_DynLib = 0x3,
    Elf_Core = 0x4,
} ElfFileType;

typedef enum : uint16_t {
    Elf_AMD64   = 0x3E,
    Elf_AARCH64 = 0xB7,
    Elf_RiscV   = 0xF3,
} ElfArchitecture;


typedef struct __attribute__((packed)) {
    /**
     * The first 16 bytes of the header are referred to as the 
     * 'identification bytes'
     */
    // The set of identification bytes. Begins with
    // [ 0x7, 'E', 'L', 'F' ]
    uint8_t        magic [ELF_N_MAGI_BYTES];
    ElfClass       class;
    ElfEndianness  endianness;
    uint8_t        version;
    ElfOSABI       osabi;
    uint8_t        osabi_extra;
    uint8_t        padding [7];

    /**
     * The rest of the Elf header information follows after the 7 padding bytes.
     */
    ElfFileType type;
    ElfArchitecture architecture;
    uint32_t version_2;

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
    uint16_t program_header_entry_size;
    uint16_t num_program_header_entries;

    uint16_t section_hedaer_entry_size;
    uint16_t num_section_header_entries;

    // Which section header table entry contains section names?
    uint16_t section_names;
} Elf64_Header;

typedef enum : uint32_t {
    PTNull    = 0x0,
    PTLoad    = 0x1,
    PTDynamic = 0x2,
    PTInterp  = 0x3,
    PTNote    = 0x4,
    PTSHLIB   = 0x5,
    PTPHDR    = 0x6,
    PTTLS     = 0x7,
} ProgHeaderType;

typedef enum : uint32_t {
    PFExecutable = 0x1,
    PFWritable   = 0x2,
    PFReadable   = 0x4,
} ProgHeaderFlags;

typedef struct {
    ProgHeaderType type; 
    ProgHeaderFlags flags;
    uint64_t offset;
    uint64_t virtual_address;
    uint64_t physical_address;
    uint64_t file_size;
    uint64_t memory_size;
    uint64_t alignment;
} Elf64ProgramHeader;


#endif
