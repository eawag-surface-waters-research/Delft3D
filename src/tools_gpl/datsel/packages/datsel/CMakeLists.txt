set(executable_name datsel)

# Retrieve the .rc file for writing the version metadata into the executable
get_module_include_path(${checkout_src_root}/${datsel_version_number_module} 
                        ${executable_name}
                        datsel_version_number_include_path)

message(STATUS "Configuring '${executable_name}' with the following datsel_version_number_module include path: '${datsel_version_number_include_path}'")

# Set directory which contains all source files
set(src_path src)

# Gather source files
file(GLOB source_files ${src_path}/*.cpp)

file(GLOB version_files ${datsel_version_number_include_path}/*.rc) 

# Define executable
add_executable(${executable_name} ${source_files}
                                  ${version_files})

# Set dependencies
set(oss_dependencies    deltares_common
                        deltares_common_c
                        deltares_common_mpi
                        datsel_f
                        datsel_version_number
                        esmfsm
                        esmfsm_c
                        esmfsm_version_number
                        nefis)
oss_include_libraries(${executable_name} oss_dependencies)

if (WIN32)
    # Set linker options
    message(STATUS "Setting target_link_options in windows")
    target_link_options(${executable_name} PRIVATE ${nologo_flag})

    if(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
        target_link_options(${executable_name} PRIVATE "/LIBPATH: $(IFORT_COMPILER19)/compiler/lib/intel64;")
    endif()
endif(WIN32)

# Explicitly link the libraries to the target or the compilation will fail as the linker cannot find the Fortran libs
target_link_libraries(${executable_name} ${oss_dependencies})

if(UNIX)
    target_compile_options(${executable_name}  PRIVATE ${c_compiler_flags})
endif()

# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files})
source_group("Resource" FILES ${version_files})
set_target_properties (${executable_name} PROPERTIES FOLDER tools_gpl/datsel)