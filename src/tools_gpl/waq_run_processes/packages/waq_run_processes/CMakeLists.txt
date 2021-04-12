# Set directory which contains all source files
set(src_path src)

# Gather source files
get_fortran_source_files(${src_path} source_files)

# Define executable
set(executable_name waq_run_processes)
add_executable(${executable_name} ${source_files})

# Set dependencies
set(oss_dependencies    deltares_common
                        deltares_common_c
                        waq_kernel
                        waq_process
                        waq_utils_f
                        waq_run_processes_version_number) 
oss_include_libraries(${executable_name} oss_dependencies)

if(UNIX)
    target_link_libraries(${executable_name} ${oss_dependencies})
    target_compile_options(${executable_name}  PRIVATE ${c_compiler_flags})
endif()

# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files})
set_target_properties (${executable_name} PROPERTIES FOLDER tools_gpl/waq_run_processes)