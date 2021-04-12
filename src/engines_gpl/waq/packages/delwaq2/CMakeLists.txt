# Define include directories
# Note that the CMAKE_CURRENT_SOURCE_DIR has a path to THIS CMakeLists location
set(waq_root_path ${CMAKE_CURRENT_SOURCE_DIR}/../../) # Trace back to the WAQ path

# Set version file variables
set(waq_version_path ${waq_root_path}/version)

set(ini_version_file ${waq_version_path}/version_number.ini)
set(fortran_version_file ${CMAKE_CURRENT_SOURCE_DIR}/src/delwaq2_version.F90)
set(rc_version_file ${waq_version_path}/version_number_delwaq2.rc)

# Set icon file
set(icon_file ${waq_version_path}/waq_icon.rc)

# Generate version files
generate_version_files(${rc_version_file} 
                       ${waq_root_path}
                       ${ini_version_file} 
                       ON)

generate_version_files(${fortran_version_file} 
                       ${waq_root_path}
                       ${ini_version_file} 
                       ON)

# Set directory which contains all source files
set(src_path src)

# Gather source files
get_fortran_source_files(${src_path} source_files)

# Define executable
set(executable_name delwaq2)
add_executable(${executable_name} ${source_files}
                                  ${icon_file}
                                  ${rc_version_file})

# Define prebuild events
prebuild_version_number(${executable_name} ${rc_version_file} 
                                        ${waq_root_path}
                                        ${ini_version_file}
                                        ON)

prebuild_version_number(${executable_name} ${fortran_version_file} 
                                        ${waq_root_path}
                                        ${ini_version_file}
                                        ON)

# Set additional compilation properties
target_compile_options(${executable_name} PRIVATE "${extend_source132_flag}")

set_source_files_properties(${fortran_version_file}
                            ${preprocessor_files}
                            PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")

# Set dependencies
set(oss_dependencies    delwaq_dll) 
oss_include_libraries(${executable_name} oss_dependencies)

if(UNIX)
    target_link_libraries(${executable_name} ${oss_dependencies})
    target_compile_options(${executable_name}  PRIVATE ${c_compiler_flags})
endif()

# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files})
source_group(Resources FILES    ${icon_file} 
                                ${rc_version_file})
set_target_properties (${executable_name} PROPERTIES FOLDER engines_gpl/waq)

# Set post-build step
set(install_dir ${CMAKE_BINARY_DIR})
set(build_dir ${CMAKE_BINARY_DIR})

post_build_target (${executable_name}
                   ${install_dir} 
                   ${build_dir} 
                   ${checkout_src_root} 
                   ${executable_name})

install(TARGETS ${executable_name} RUNTIME  DESTINATION bin)
