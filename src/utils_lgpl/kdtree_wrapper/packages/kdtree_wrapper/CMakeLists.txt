set(library_name kdtree_wrapper)
get_fortran_source_files(src source_files)
add_library(${library_name} ${source_files})

set (dependecies deltares_common kdtree2)
oss_include_libraries(${library_name} dependecies)

if (UNIX)
    target_link_libraries(${library_name} ${dependecies})
endif(UNIX)

# Set additional compilation properties
target_compile_options(${library_name} PRIVATE "${check_bounds_flag}")

# Create the folder structure in vfproj
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files})
set_target_properties (${library_name} PROPERTIES FOLDER utils_lgpl/kdtree_wrapper)
