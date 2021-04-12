set(library_name morphology_io)
get_fortran_source_files(src source_files)
add_library(${library_name} ${source_files})

# Set dependencies
set(oss_dependencies    deltares_common
                        deltares_common_c
                        deltares_common_mpi
                        ec_module
                        morphology_data
                        morphology_kernel)
oss_include_libraries(${library_name} oss_dependencies)

if(UNIX)
    target_link_libraries(${library_name} ${oss_dependencies})
    target_compile_options(${library_name} PRIVATE ${c_compiler_flags})
endif()

include_directories(${checkout_src_root}
                   "${CMAKE_CURRENT_SOURCE_DIR}/../../include")

# Set additional compilation flags
target_compile_options(${library_name} PRIVATE "${debug_bounds_flag}")
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files})
set_target_properties (${library_name} PROPERTIES FOLDER utils_gpl/morphology)