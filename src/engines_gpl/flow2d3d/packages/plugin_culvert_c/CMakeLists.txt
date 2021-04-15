# Set directory which contains all source files
set(src_path src)

# Gather source files
file(GLOB source_files ${src_path}/*.c)

# Define library
set(library_name plugin_culvert_c)
add_library(${library_name} ${source_files})

# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files})
set_target_properties (${library_name} PROPERTIES FOLDER engines_gpl/flow2d3d)
