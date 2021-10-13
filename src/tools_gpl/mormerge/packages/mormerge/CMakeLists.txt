# Set directory which contains all source files
set(src_path src)

# Set version file variables
# Note that the CMAKE_CURRENT_SOURCE_DIR has a path to THIS CMakeLists location
set(mormerge_root_path ${CMAKE_CURRENT_SOURCE_DIR}/../../) # Trace back to the Mormerge path
set(res_path ${mormerge_root_path}/res)

set(ini_version_file ${res_path}/version_number.ini)
set(fortran_version_file ${CMAKE_CURRENT_SOURCE_DIR}/${src_path}/mormerge_version.F90)
set(rc_version_file ${res_path}/version_number.rc)

# Generate version files
generate_version_files(${rc_version_file} 
                       ${checkout_src_root}
                       ${ini_version_file})

generate_version_files(${fortran_version_file} 
                       ${checkout_src_root}
                       ${ini_version_file})

# Gather source files
get_fortran_source_files(${src_path} source_files)

# Define executable
set(executable_name mormerge)
add_executable(${executable_name}   ${source_files}
                                    ${rc_version_file})

# Define prebuild events
prebuild_version_number(${executable_name}  ${rc_version_file} 
                                            ${mormerge_root_path}
                                            ${ini_version_file})

prebuild_version_number(${executable_name}  ${fortran_version_file} 
                                            ${mormerge_root_path}
                                            ${ini_version_file})

# Define precompilation flags
set_source_files_properties(${fortran_version_file}
                            PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")

# Set dependencies
set(oss_dependencies    deltares_common
                        deltares_common_c) 
oss_include_libraries(${executable_name} oss_dependencies)
target_link_libraries(${executable_name} ${oss_dependencies})

if (WIN32)
    # Set linker properties
    message(STATUS "Setting linker properties in windows")
    target_link_directories(${executable_name}
                            PRIVATE
                            "${checkout_src_root}/third_party_open/pthreads/bin/x64")

    target_link_libraries(${executable_name}                                                   
                            "pthreadVC2.lib" 
                            "wsock32.lib"
                            "netapi32.lib" 
                            "comctl32.lib"
                            "user32.lib" 
                            "advapi32.lib" 
                            "comdlg32.lib" 
                            "gdi32.lib" 
                            "winspool.lib")

    # Set linker options
    message(STATUS "Setting target_link_options in windows")
    target_link_options(${executable_name} PRIVATE ${nologo_flag})
endif(WIN32)

if(UNIX)
    target_link_libraries(${executable_name} ${oss_dependencies})
endif()

# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files})
source_group(Resources FILES ${rc_version_file})

set_target_properties (${executable_name} PROPERTIES FOLDER tools_gpl/mormerge)
