cmake_minimum_required(VERSION 3.16)

project(65c02 LANGUAGES NONE)

# ------------------------------------------------------------
# Tools
# ------------------------------------------------------------
find_program(CA65 ca65 REQUIRED)
find_program(LD65 ld65 REQUIRED)

# ------------------------------------------------------------
# Output directories
# ------------------------------------------------------------
set(OBJ_DIR ${CMAKE_BINARY_DIR}/obj)
file(MAKE_DIRECTORY ${OBJ_DIR})

# ------------------------------------------------------------
# Global properties to collect objects & targets
# ------------------------------------------------------------
define_property(GLOBAL PROPERTY ALL_OBJECTS
    BRIEF_DOCS "All assembled object files"
    FULL_DOCS "Object files produced by ca65"
)

define_property(GLOBAL PROPERTY ALL_OBJECT_TARGETS
    BRIEF_DOCS "All object-producing targets"
    FULL_DOCS "Targets that assemble .s files"
)

# ------------------------------------------------------------
# Assemble function
# Usage:
#   assemble_65c02(bios SOURCES bios.s)
#   assemble_65c02(bios SOURCES bios.s foo.s bar.s
#                  DEFINES FOO=1 BAR
#                  DEPENDS linker.cfg)
# ------------------------------------------------------------
function(assemble_65c02 TARGET)
    set(options)
    set(oneValueArgs)
    set(multiValueArgs SOURCES DEFINES DEPENDS)

    cmake_parse_arguments(ASM "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})

    if (NOT ASM_SOURCES)
        message(FATAL_ERROR "assemble_65c02: no SOURCES specified")
    endif()

    # Build -D flags
    set(CA65_DEFINES "")
    foreach(DEF ${ASM_DEFINES})
        list(APPEND CA65_DEFINES "-D${DEF}")
    endforeach()

    foreach(SOURCE ${ASM_SOURCES})
        get_filename_component(SRC_NAME ${SOURCE} NAME_WE)

        set(OBJ ${OBJ_DIR}/${SRC_NAME}.o)
        set(OBJ_TARGET ${TARGET}_${SRC_NAME})

        add_custom_command(
            OUTPUT ${OBJ}
            COMMAND ${CA65}
                ${CA65_DEFINES}
                -o ${OBJ}
                -I${CMAKE_SOURCE_DIR}/include
                ${CMAKE_CURRENT_SOURCE_DIR}/${SOURCE}
            DEPENDS ${SOURCE} ${ASM_DEPENDS}
            COMMENT "Assembling ${SOURCE}"
            VERBATIM
        )

        add_custom_target(${OBJ_TARGET} DEPENDS ${OBJ})

        # Register globally
        set_property(GLOBAL APPEND PROPERTY ALL_OBJECTS ${OBJ})
        set_property(GLOBAL APPEND PROPERTY ALL_OBJECT_TARGETS ${OBJ_TARGET})
    endforeach()
endfunction()

# ------------------------------------------------------------
# Subdirectories
# ------------------------------------------------------------
add_subdirectory(vectors)
add_subdirectory(bios)
add_subdirectory(wozmon)
add_subdirectory(msbasic)

# ------------------------------------------------------------
# Collect global lists (AFTER subdirs!)
# ------------------------------------------------------------
get_property(ALL_OBJECTS GLOBAL PROPERTY ALL_OBJECTS)
get_property(ALL_OBJECT_TARGETS GLOBAL PROPERTY ALL_OBJECT_TARGETS)

# ------------------------------------------------------------
# Link ROM
# ------------------------------------------------------------
add_custom_command(
    OUTPUT rom.bin
    COMMAND ${LD65}
        -C ${CMAKE_SOURCE_DIR}/linker.cfg
        -o rom.bin
        -Ln rom.lbl
        -m rom.map
        -v
        ${ALL_OBJECTS}
    DEPENDS ${ALL_OBJECTS}
    COMMENT "Linking ROM image"
)

add_custom_target(rom ALL DEPENDS rom.bin)

# Ensure ROM builds after all object targets
if(ALL_OBJECT_TARGETS)
    add_dependencies(rom ${ALL_OBJECT_TARGETS})
endif()
