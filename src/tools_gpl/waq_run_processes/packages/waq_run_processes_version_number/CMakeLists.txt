# Set include directories
set(waq_run_processes_include_path ${CMAKE_CURRENT_SOURCE_DIR}/include)
include_directories(${waq_run_processes_include_path})

# Note that the CMAKE_CURRENT_SOURCE_DIR has a path to THIS CMakeLists location
set(waq_run_processes_root_path ${CMAKE_CURRENT_SOURCE_DIR}/../../) # Trace back to the waq_run_processes root path
set(ini_version_file ${CMAKE_CURRENT_SOURCE_DIR}/${include_path}/version_number.ini)
set(h_version_file ${CMAKE_CURRENT_SOURCE_DIR}/${include_path}/version_number.h)
set(rc_version_file ${CMAKE_CURRENT_SOURCE_DIR}/${include_path}/version_number.rc)

# Generate version files
generate_version_files(${rc_version_file} 
                       ${checkout_src_root}
                       ${ini_version_file} 
                       ON)

generate_version_files(${h_version_file} 
                       ${checkout_src_root}
                       ${ini_version_file} 
                       ON)

# Set directory which contains all source files
set(src_path src)

# Gather source files
file(GLOB source_files  ${src_path}/*.c)
file(GLOB headers       ${waq_run_processes_include_path}/*.h)

# Define library
set(library_name waq_run_processes_version_number)
add_library(${library_name} ${source_files}
                            ${h_version_file}
                            ${headers})

# Define prebuild events
prebuild_version_number(${library_name} ${rc_version_file} 
                                        ${checkout_src_root}
                                        ${ini_version_file}
                                        ON)

prebuild_version_number(${library_name} ${h_version_file} 
                                        ${checkout_src_root}
                                        ${ini_version_file}
                                        ON)

# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files}
                                                    ${h_version_file}
                                                    ${headers})
set_target_properties (${library_name} PROPERTIES FOLDER tools_gpl/waq_run_processes)
