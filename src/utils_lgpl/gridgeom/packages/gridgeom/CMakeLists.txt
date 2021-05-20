# Set the path where the source files are located
set(src_path src) 

# Note that the CMAKE_CURRENT_SOURCE_DIR has a path to the location of where this CMakeLists file is defined in.
set(ini_version_file "${CMAKE_CURRENT_SOURCE_DIR}/include/version_number.ini")
set(fortran_version_file "${CMAKE_CURRENT_SOURCE_DIR}/${src_path}/gridgeom_version.F90")

# Generate the F90 version file
generate_version_files(${fortran_version_file} ${checkout_src_root} ${ini_version_file})

# Gather source files
get_fortran_source_files(${src_path} source_files)

set(library_name gridgeom)
add_library(${library_name} ${source_files}
                            ${fortran_version_file}
                            ${ini_version_file})

set_source_files_properties(${fortran_version_file}
                            PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")

# Add prebuild events
prebuild_version_number(${library_name} ${fortran_version_file} 
                                        ${checkout_src_root} 
                                        ${ini_version_file})

# Set the dependencies of this library
set(oss_dependencies    deltares_common
                        ec_module
                        io_netcdf
                        kdtree_wrapper
                        kdtree2)
oss_include_libraries(${library_name} oss_dependencies)

if (UNIX)
    target_link_libraries(${library_name} triangle_c)
    message(STATUS "Setting oss_target_link_libraries in Unix")   
    target_link_libraries(${library_name} ${oss_dependencies})
endif(UNIX)

if (WIN32)
    target_link_libraries(${library_name} kdtree2
                                          triangle_c)
endif(WIN32)

# Set additional compilation properties, specific for Debug mode
target_compile_options(${library_name} PRIVATE "$<$<CONFIG:Debug>:${check_bounds_flag}>")

# Create the folder structure in vfproj
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files} ${fortran_version_file})
set_target_properties (${library_name} PROPERTIES FOLDER utils_lgpl/gridgeom)
