set(library_name data)

# Define include directories
get_module_include_path(${checkout_src_root}/${deltares_common_module} 
                        ${library_name}
                        deltares_common_include_path)
get_module_include_path(${checkout_src_root}/${delftio_module} 
                        ${library_name}
                        delftio_include_path)
get_module_include_path(${checkout_src_root}/${esmfsm_module} 
                        ${library_name} 
                        esmfsm_module_include_path)

message(STATUS "Configuring '${library_name}' with the following deltares_common include path: '${deltares_common_include_path}'")
message(STATUS "Configuring '${library_name}' with the following delftio include path: '${delftio_include_path}'")
message(STATUS "Configuring '${library_name}' with the following esmfsm include path: '${esmfsm_module_include_path}'")

# Note that the CMAKE_CURRENT_SOURCE_DIR has a path to THIS CMakeLists location
set(include_path ${CMAKE_CURRENT_SOURCE_DIR}/include)
include_directories(${include_path}
                    ${mpich2_path}
                    ${deltares_common_include_path}
                    ${delftio_include_path}
                    ${esmfsm_module_include_path})

# Set public include path to make it accessible for other components
set(public_include_path ${include_path})

# Set directory which contains all source files
set(src_path src)

# Gather include files 
file(GLOB include_files ${include_path}/*.igs
                        ${include_path}/*.igd) 

# Gather source files
get_fortran_source_files(${src_path}/allocation allocation_files)
get_fortran_source_files(${src_path}/basics basics_files)
get_fortran_source_files(${src_path}/gdp gdp_files)
get_fortran_source_files(${src_path}/general general_files)
get_fortran_source_files(${src_path}/parallel_mpi parallel_mpi_files)

# Define library
add_library(${library_name} ${include_files}
                            ${allocation_files}
                            ${basics_files}
                            ${gdp_files}
                            ${general_files}
                            ${parallel_mpi_files})

# Set additional compilation properties
set_source_files_properties(${parallel_mpi_files}
                            PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")

# Set dependencies
set(oss_dependencies    delftio
                        deltares_common
                        deltares_common_mpi
                        ec_module
                        morphology_data
                        morphology_kernel
                        netcdf4
                        netcdff) 
oss_include_libraries(${library_name} oss_dependencies)

if(UNIX)
    target_link_libraries(${library_name} ${oss_dependencies})
endif()

# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${include_files}
                                                    ${allocation_files}
                                                    ${basics_files}
                                                    ${gdp_files}
                                                    ${general_files}
                                                    ${parallel_mpi_files})
set_target_properties (${library_name} PROPERTIES FOLDER engines_gpl/flow2d3d)
