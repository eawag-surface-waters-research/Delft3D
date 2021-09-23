set(wave_version_path ${CMAKE_CURRENT_SOURCE_DIR}) # Wave uses this path to determine the svn revision

# Set version file variables
set(wave_version_files_path ${CMAKE_CURRENT_SOURCE_DIR}/src)

set(ini_version_file ${CMAKE_CURRENT_SOURCE_DIR}/include/version_number.ini)
set(fortran_version_file ${wave_version_files_path}/wave_version.F90)
set(rc_version_file ${wave_version_files_path}/wave_version.rc)

# Generate version files
generate_version_files(${rc_version_file} 
                       ${wave_version_path}
                       ${ini_version_file})

generate_version_files(${fortran_version_file} 
                       ${wave_version_path}
                       ${ini_version_file})

# Set directory which contains all source files
set(src_path src)

# Gather source files
get_fortran_source_files(${src_path} source_files)

# Define library
set(library_name wave_manager)
add_library(${library_name} ${source_files})

# Define prebuild events
prebuild_version_number(${library_name} ${rc_version_file} 
                                        ${wave_version_path}
                                        ${ini_version_file})

prebuild_version_number(${library_name} ${fortran_version_file} 
                                        ${wave_version_path}
                                        ${ini_version_file})

set_source_files_properties(${fortran_version_file}
                            PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")

# Set dependencies
set(oss_dependencies    wave_data
                        delftio
                        deltares_common
                        ec_module
                        wave_io
                        wave_kernel) 

oss_include_libraries(${library_name} oss_dependencies)
target_link_libraries(${library_name} ${oss_dependencies})

include_directories(${mpich2_path})

# Set additional compilation properties, specific for Debug mode
target_compile_options(${library_name} PRIVATE "$<$<CONFIG:Debug>:${check_bounds_flag}>")
target_compile_options(${library_name} PRIVATE "$<$<CONFIG:Debug>:${check_pointer}>")

# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files})
set_target_properties (${library_name} PROPERTIES FOLDER engines_gpl/wave)
