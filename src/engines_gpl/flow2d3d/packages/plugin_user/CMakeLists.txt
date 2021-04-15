set(library_name plugin_user)

# Define include directories
get_module_include_path(${checkout_src_root}/${deltares_common_module} 
                        ${library_name}
                        deltares_common_include_path)
get_module_include_path(${checkout_src_root}/${esmfsm_module} 
                        ${library_name} 
                        esmfsm_module_include_path)

message(STATUS "Configuring '${library_name}' with the following deltares_common include path: '${deltares_common_include_path}'")
message(STATUS "Configuring '${library_name}' with the following esmfsm include path: '${esmfsm_module_include_path}'")

include_directories(${deltares_common_include_path}
                    ${esmfsm_module_include_path})

# Set directory which contains all source files
set(src_path src)

# Gather source files
get_fortran_source_files(${src_path} source_files)

# Define library
add_library(${library_name} ${source_files})

# Set dependencies
set(oss_dependencies    data
                        deltares_common
                        ec_module) 
oss_include_libraries(${library_name} oss_dependencies)

if(UNIX)
    target_link_libraries(${library_name} ${oss_dependencies})
endif(UNIX)

# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files})
set_target_properties (${library_name} PROPERTIES FOLDER engines_gpl/flow2d3d)
