# Set directory which contains all source files
set(src_path src)

# Set version file variables
set(svn_version_root "${CMAKE_CURRENT_SOURCE_DIR}/../") # Check the SVN revision of WAQ directory
set(ini_version_file "${CMAKE_CURRENT_SOURCE_DIR}/../../version/version_number.ini")
set(fortran_version_file "${CMAKE_CURRENT_SOURCE_DIR}/src/wq_processes_version.F90")
set(rc_version_file "${CMAKE_CURRENT_SOURCE_DIR}/../../version/version_number.rc")

# Generate the F90 version files
generate_version_files(${rc_version_file} 
                       ${checkout_src_root}
                       ${ini_version_file} 
                       ON)

generate_version_files(${fortran_version_file} 
                       ${checkout_src_root}
                       ${ini_version_file} 
                       ON)

# Gather source files
get_fortran_source_files(${src_path} source)

# Define library
set(library_name wq_processes)
add_library(${library_name} ${source})

# Define prebuild events
prebuild_version_number(${library_name} ${rc_version_file} 
                                        ${checkout_src_root}
                                        ${ini_version_file})

prebuild_version_number(${library_name} ${fortran_version_file} 
                                        ${checkout_src_root}
                                        ${ini_version_file})

# Define additional compilation flags
set_source_files_properties(${fortran_version_file}
                            PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")

# Set dependencies
if (UNIX)
    set(oss_dependencies    deltares_common
                            waq_process
                            waq_utils_c
                            waq_utils_f)
endif(UNIX)
if (WIN32)
    set(oss_dependencies    deltares_common
                            netcdf4
                            netcdff
                            waq_process
                            waq_utils_c
                            waq_utils_f)
endif(WIN32)
oss_include_libraries(${library_name} oss_dependencies)

if(UNIX)
    target_link_libraries(${library_name} ${oss_dependencies})
    target_compile_options(${library_name}  PRIVATE ${c_compiler_flags})
endif()

target_include_directories(${library_name}  PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/../../include")

# Define how the files should be structured in Visual studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES  ${source})
set_target_properties (${library_name} PROPERTIES FOLDER utils_gpl/wq_processes)

