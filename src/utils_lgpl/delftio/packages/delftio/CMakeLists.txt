# Set path for include files 
# Note that the CMAKE_CURRENT_SOURCE_DIR has a path to THIS CMakeLists location
set(delftio_include_path ${CMAKE_CURRENT_SOURCE_DIR}/../../include)
include_directories(${delftio_include_path})

# Set include path to have it accessible for other components
set(public_include_path ${delftio_include_path})

# Set version file variables
set(ini_version_file ${CMAKE_CURRENT_SOURCE_DIR}/include/version_number.ini)
set(fortran_version_file ${CMAKE_CURRENT_SOURCE_DIR}/src/diof90/delftio_version.F90)

# Generate the F90 version file
generate_version_files(${fortran_version_file} ${ini_version_file})

# Set directory which contains all source files
set(src_path src)

# Gather source files
get_fortran_source_files(${src_path}/delftio_sync delftio_sync)
get_fortran_source_files(${src_path}/diof90 diof90)
get_fortran_source_files(${src_path}/diof90nefis diof90nefis)

# Define library
set(library_name delftio)
add_library(${library_name} ${delftio_sync}
                            ${diof90}
                            ${diof90nefis})

# Set additional compilation properties
set_target_properties(${library_name} PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")

# Set dependencies
set(oss_dependencies    deltares_common
                        deltares_common_c
                        delftio_shm) 
oss_include_libraries(${library_name} oss_dependencies)

if(UNIX)
    target_link_libraries(${library_name} ${oss_dependencies})
endif(UNIX)

# Define prebuild events
prebuild_version_number(${library_name} ${fortran_version_file} 
                                        ${checkout_src_root}
                                        ${ini_version_file}
                                        ON)

# Set preprocessor definitions
if(WIN32)
    target_compile_definitions(${library_name} PRIVATE "WIN32")
endif(WIN32)

# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${delftio_sync}
                                                    ${diof90}
                                                    ${diof90nefis})
set_target_properties (${library_name} PROPERTIES FOLDER utils_lgpl/delft_io)
