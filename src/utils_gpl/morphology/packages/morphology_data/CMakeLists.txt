set(library_name morphology_data)
get_fortran_source_files(src source_files)
add_library(${library_name} ${source_files})


# Set dependencies
set(oss_dependencies    deltares_common)
oss_include_libraries(${library_name} oss_dependencies)

if(UNIX)
    target_link_libraries(${library_name} ${oss_dependencies})
endif(UNIX)

# Set additional compilation flags
target_compile_options(${library_name} PRIVATE "${debug_bounds_flag}")

# Create the folder structure in vfproj
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files})
set_target_properties (${library_name} PROPERTIES FOLDER utils_gpl/morphology)
