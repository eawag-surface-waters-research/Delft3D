# Set version file variables
set(include_path include)
include_directories(${include_path})

# Note that the CMAKE_CURRENT_SOURCE_DIR has a path to THIS CMakeLists location
set(svn_version_path ${CMAKE_CURRENT_SOURCE_DIR}/../../) # Trace back to the esmfsm path
set(ini_version_file ${CMAKE_CURRENT_SOURCE_DIR}/${include_path}/version_number.ini)
set(h_version_file ${CMAKE_CURRENT_SOURCE_DIR}/${include_path}/version_number.h)
set(rc_version_file ${CMAKE_CURRENT_SOURCE_DIR}/${include_path}/version_number.rc)

# Generate version files
generate_version_files(${rc_version_file} 
                       ${svn_version_path}
                       ${ini_version_file} 
                       ON)

generate_version_files(${h_version_file} 
                       ${svn_version_path}
                       ${ini_version_file} 
                       ON)

# Set directory which contains all source files
set(src_path src)

# Gather source files
file(GLOB source_files ${src_path}/*.c)

# Define library
set(library_name esmfsm_version_number)
add_library(${library_name} ${source_files}
                            ${h_version_file})

# Define prebuild events
prebuild_version_number(${library_name} ${rc_version_file} 
                                        ${svn_version_path}
                                        ${ini_version_file}
                                        ON)

prebuild_version_number(${library_name} ${h_version_file} 
                                        ${svn_version_path}
                                        ${ini_version_file}
                                        ON)

if(UNIX)
    target_compile_options(${library_name}  PRIVATE ${c_compiler_flags})
endif()

# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files}
                                                    ${h_version_file})
set_target_properties (${library_name} PROPERTIES FOLDER utils_lgpl/esmfsm)