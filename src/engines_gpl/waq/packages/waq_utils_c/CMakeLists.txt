# Set the directory which contains the source code
set(src_path src)

# Gather source files
set(source  ${src_path}/f902c.cpp
            ${src_path}/getuuid.c)

set(library_name waq_utils_c)

add_library(${library_name} ${source})

# Set additional compilation properties, specific for Debug mode
target_compile_options(${library_name} PRIVATE "$<$<CONFIG:Debug>:${check_bounds_flag}>")

# Define how the files should be structured in Visual studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source} )
set_target_properties (${library_name} PROPERTIES FOLDER utils_gpl/wq_processes)

