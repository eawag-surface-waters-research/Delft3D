# Define include directory
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../include) # Note that the CMAKE_CURRENT_SOURCE_DIR represents THIS directory

# Set source directory
set(src_path src)

# Gather source files 
get_fortran_source_files(${src_path} source_files)

# Define executable
set(executable_name waqpb_export)
add_executable(${executable_name} ${source_files})

# Set dependencies
set(oss_dependencies    deltares_common
                        deltares_common_c
                        nefis
                        waq_utils_f
                        waqpb_lib) 
oss_include_libraries(${executable_name} oss_dependencies)
target_link_libraries(${executable_name} ${oss_dependencies})


# Set additional compilation properties
target_compile_options(${executable_name} PRIVATE "${extend_source132_flag}")
target_compile_options(${executable_name} PRIVATE "$<$<CONFIG:Debug>:${check_bounds_flag}>")
target_compile_options(${executable_name} PRIVATE "$<$<CONFIG:Debug>:${check_pointer}>")

if(UNIX)
    set_property(TARGET ${executable_name} PROPERTY LINKER_LANGUAGE Fortran)
endif(UNIX)

# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files})
set_target_properties (${executable_name} PROPERTIES FOLDER tools_gpl/waqpb)

# Set post-build step
set(install_dir ${CMAKE_BINARY_DIR})
set(build_dir ${CMAKE_BINARY_DIR})

post_build_target (${executable_name}
                   ${install_dir} 
                   ${build_dir} 
                   ${checkout_src_root} 
                   ${executable_name})

install(TARGETS ${executable_name} RUNTIME  DESTINATION bin)

