# Set directory which contains all source files
set(src_path src)

# Set version file variables
# Note that the CMAKE_CURRENT_SOURCE_DIR has a path to THIS CMakeLists location
set(svn_version_path ${CMAKE_CURRENT_SOURCE_DIR}/../) # Trace back to the part path
set(ini_version_file ${CMAKE_CURRENT_SOURCE_DIR}/../../res/version_number.ini)
set(fortran_version_file ${CMAKE_CURRENT_SOURCE_DIR}/${src_path}/part_version.F90)

generate_version_files(${fortran_version_file} 
                       ${checkout_src_root}
                       ${ini_version_file} 
                       ON)

# Gather source files
get_fortran_source_files(${src_path} source_files)

# Define library
set(library_name part_utils_f)
add_library(${library_name} ${source_files})

# Set dependencies
set(oss_dependencies    deltares_common
                        part_data_f) 
oss_include_libraries(${library_name} oss_dependencies)

if(UNIX)
    target_link_libraries(${library_name} ${oss_dependencies})
endif(UNIX)

# Define additional compilation flags
set_source_files_properties(${fortran_version_file}
                            PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")

# Add prebuild events
prebuild_version_number(${library_name} ${fortran_version_file} 
                                        ${checkout_src_root}
                                        ${ini_version_file}
                                        ON)

# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files})
set_target_properties (${library_name} PROPERTIES FOLDER engines_gpl/part)
