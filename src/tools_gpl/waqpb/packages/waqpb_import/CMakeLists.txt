# Define include directory
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../include) # Note that the CMAKE_CURRENT_SOURCE_DIR represents THIS directory

# Set source directory
set(src_path src)

# Gather source files 
get_fortran_source_files(${src_path} source_files)

# Define library
set(executable_name waqpb_import)
add_executable(${executable_name} ${source_files})

# Set additional compilation properties
target_compile_options(${executable_name} PRIVATE "${extend_source132_flag}")

# Set dependencies
set(oss_dependencies    waqpb_lib) 
oss_include_libraries(${executable_name} oss_dependencies)
target_link_libraries(${executable_name} ${oss_dependencies})

if(UNIX)
    target_link_libraries(${executable_name} ${oss_dependencies})
endif()

# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files})
set_target_properties (${executable_name} PROPERTIES FOLDER tools_gpl/waqpb)
