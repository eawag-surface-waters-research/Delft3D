# Set path for include files
# Note that the CMAKE_CURRENT_SOURCE_DIR has a path to THIS CMakeLists location
set(waq_delftio_include_path ${CMAKE_CURRENT_SOURCE_DIR}/../../include)
include_directories(${waq_delftio_include_path})

# Set include path to have it accessible for other components
set(public_include_path ${waq_delftio_include_path})

# Set version file variables
set(ini_version_file ${CMAKE_CURRENT_SOURCE_DIR}/include/version_number.ini)
set(fortran_version_file ${CMAKE_CURRENT_SOURCE_DIR}/src/diof90/delftio_version.F90)

# Generate the F90 version file
generate_version_files(${fortran_version_file} ${ini_version_file})

# Set directory which contains all source files
set(src_path src)

# Gather source files
get_fortran_source_files(${src_path}/diof90 diof90)

# Define library
set(library_name waq_delftio)
add_library(${library_name} ${diof90})

# Set additional compilation properties
set_target_properties(${library_name} PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")

# Set dependencies
set(oss_dependencies    deltares_common
                        deltares_common_c)
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
if(UNIX)
    target_compile_definitions(${library_name} PRIVATE "HAVE_CONFIG_H")
endif(UNIX)

# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${diof90})
set_target_properties (${library_name} PROPERTIES FOLDER engines_gpl/waq)