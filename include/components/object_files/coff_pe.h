#ifndef __COMPONENTS_OBJECT_FILES_COFF_PE_H
#define __COMPONENTS_OBJECT_FILES_COFF_PE_H

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

#define N_DOS_EXE_BYTES 4

typedef enum : uint16_t {
    Unknown = 0,
    Coff_AMD64 = 0x8664,
    Coff_AARCH64 = 0xaa64,
} CoffMachine;

typedef enum : uint16_t {
    NoRelocation      = 0x0001, /** Tell the loader that addresses cannot be
                                    relocated (image only). */
    ExecutableImage   = 0x0002, /** Indicates that can be run */
    LargeAddressAware = 0x0020, /** Can handle addresses > 2GB*/
    Is32BitArch       = 0x0100, /** Set if architecture is 32-bit */
    NoDebugInfo       = 0x0200, /** Set if there is no debug info in the file */
    MediaRunFromSwap  = 0x0400, /** If set and file is on removable media, load
                                    it into swap before executing */
    NetRunFromSwap    = 0x0800, /** If set and file is on network, load it into
                                    swap before executing */
    IsSystemFile      = 0x1000, /** Set if executable is system file (not user
                                    program) */
    IsDLLFile         = 0x2000, /** Set if file is a DLL file. Note that */
    UniProcessorOnly  = 0x4000, /** Set if the file should only be run on a
                                    uniprocessor machine. */
} Characteristics;


typedef struct __attribute__((packed)) {
    CoffMachine machine;
    uint16_t num_sections;
    uint32_t timestamp;
    uint32_t symtable_offset;
    uint32_t num_symbols;
    uint16_t optional_header_size;
    Characteristics characteristics;
} COFF_Header;

typedef struct __attribute__((packed)) {

} PE_Header;

typedef struct __attribute__((packed)) {
  /**
     Layout of PE File

    MS-DOS 2.0 Compatible EXE Header
    unused
    OEM Identifier
    OEM Information
    Offset to PE Header
    MS-DOS 2.0 Stub Program and Relocation Table
    unused

    PE Header (aligned on 8-byte boundary)
    Section Headers
    Image Pages:
    import info
    export info
    base relocations
    resource info

   */
    uint8_t        dos_executable [N_DOS_EXE_BYTES];
} ImageHeader;


#endif
