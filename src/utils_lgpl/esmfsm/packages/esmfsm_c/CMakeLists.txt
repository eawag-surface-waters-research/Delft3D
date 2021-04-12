# Set include paths 
set(esfsm_include_path ${CMAKE_CURRENT_SOURCE_DIR}/../../include) # Trace back to the esmfsm path
set(include_path include)
include_directories(${include_path}
                    ${esfsm_include_path}
                    ${pthreads_path})

# Set directory which contains all source files
set(src_path src)

# Gather source files
file(GLOB header_files ${include_path}/*.h)
file(GLOB esm_global_header_files ${esfsm_include_path}/*.h)
file(GLOB esm_files ${src_path}/esm/*.cpp)
file(GLOB fsm_files ${src_path}/fsm/*.cpp)

# Define library
set(library_name esmfsm_c)
add_library(${library_name} ${esm_files}
                            ${fsm_files}
                            ${header_files}
                            ${esm_global_header_files})

# Set dependencies
set(oss_dependencies    esmfsm_version_number) 
oss_include_libraries(${library_name} oss_dependencies)

if(UNIX)
    target_link_libraries(${library_name} ${oss_dependencies})
    target_compile_options(${library_name}  PRIVATE ${c_compiler_flags})
endif()

# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${esm_files}
                                                    ${fsm_files}
                                                    ${header_files})
set_target_properties (${library_name} PROPERTIES FOLDER utils_lgpl/esmfsm)