# Set version file variables
# Note that the CMAKE_CURRENT_SOURCE_DIR has a path to THIS CMakeLists location
set(waq_root_path ${CMAKE_CURRENT_SOURCE_DIR}/../..) # Trace back to the WAQ path
set(waq_version_path ${waq_root_path}/version)

set(ini_version_file ${waq_version_path}/version_number.ini)
set(fortran_version_file ${CMAKE_CURRENT_SOURCE_DIR}/src/waq_plugin_wasteload_version.F90)
set(rc_version_file ${waq_version_path}/version_number_waq_plugin_wasteload_version.rc.svn)

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

# Define library
set(library_name waq_plugin_wasteload)
add_library(${library_name} SHARED ${source_files})

# Define prebuild events
prebuild_version_number(${library_name} ${rc_version_file} 
                                        ${waq_root_path}
                                        ${ini_version_file}
                                        ON)

prebuild_version_number(${library_name} ${fortran_version_file} 
                                        ${waq_root_path}
                                        ${ini_version_file}
                                        ON)

# Define additional compilation flags
set_source_files_properties(${fortran_version_file}
                            PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")

# Set additional compilation properties
target_compile_options(${library_name} PRIVATE "${extend_source132_flag}")

# Set dependencies
set(oss_dependencies    deltares_common
                        waq_utils_f) 
oss_include_libraries(${library_name} oss_dependencies)
target_link_libraries(${library_name} ${oss_dependencies})

if(UNIX)
    target_link_libraries(${library_name} ${oss_dependencies})
endif(UNIX)

# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files})
set_target_properties (${library_name} PROPERTIES FOLDER engines_gpl/waq)

# Set post-build step
set(install_dir ${CMAKE_BINARY_DIR})
set(build_dir ${CMAKE_BINARY_DIR})

post_build_target (${library_name}
                   ${install_dir} 
                   ${build_dir} 
                   ${checkout_src_root} 
                   ${library_name})

install(TARGETS ${library_name} DESTINATION lib)
