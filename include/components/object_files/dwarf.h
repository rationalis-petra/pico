#ifndef __COMPONENTS_OBJECT_FILES_DWARF_H
#define __COMPONENTS_OBJECT_FILES_DWARF_H

/* DWARF File Format Handling
 * ----------------------------
 * This header refers to types and functions to help generate and
 * manipulate data in the DWARF format.
 */

/* Overview of the DWARF format
 * The DWARF format is intended to be embedded in some other object file
 * (usually ELF), and it is assumed tha this object file consists of sections.
 * The primary data-structure of the DWARF format is the Debug information entry (DIE).
 * 
 * A DIE forms a tree-structure, with each DIE having  
 * - A tag, indicating the type of the DIE
 * - A series of attributes. The attributes present, and therefore the size of
 *   the DIE depend on the tag.
 * - Some number of children.
 * 
 * The bulk of the DIEs live in the `debug_info` 
 * 
 * .debug_aranges  : Lookup table for mapping addresses to compilation units
 * .debug_frame    : Call frame information
 * .debug_info     : Core DWARF information section
 * .debug_line     : Line number information
 * .debug_loc      : Location lists used in the DW_AT_location attributes
 * .debug_macinfo  : Macro information
 * .debug_pubnames : Lookup table for global objects and functions
 * .debug_pubtypes : Lookup table for global types
 * .debug_ranges   : Address ranges used in the DW_AT_ranges attributes
 * .debug_str      : String table used in .debug_info
 * .debug_types    : Type descriptions 
 */




#endif
