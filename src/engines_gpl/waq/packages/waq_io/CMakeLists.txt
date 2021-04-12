# Set path for include files 
# Note that the CMAKE_CURRENT_SOURCE_DIR has a path to THIS CMakeLists location
set(waq_io_include_path ${CMAKE_CURRENT_SOURCE_DIR}/../../include)
include_directories(${waq_io_include_path})

# Set directory which contains all source files
set(src_path src)

# Gather source files
get_fortran_source_files(${src_path}/mpidum mpidum_files)
get_fortran_source_files(${src_path}/ods ods_files)
get_fortran_source_files(${src_path}/waq_io waq_io_files)

# Define the library
set(library_name waq_io)
add_library(${library_name} ${mpidum_files}
                            ${ods_files}
                            ${waq_io_files})

# Set additional compilation properties
target_compile_options(${library_name} PRIVATE "${extend_source132_flag}")

# Set dependencies
set(oss_dependencies    deltares_common
                        netcdf4
                        netcdff
                        part_data_f
                        part_io_f
                        waq_kernel
                        waq_plugin_wasteload
                        waq_utils_c
                        waq_utils_f) 
oss_include_libraries(${library_name} oss_dependencies)

if(UNIX)
    target_link_libraries(${library_name} ${oss_dependencies})
    target_compile_options(${library_name}  PRIVATE ${c_compiler_flags})
endif()

# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${mpidum_files}
                                                    ${ods_files}
                                                    ${waq_io_files})
set_target_properties (${library_name} PROPERTIES FOLDER engines_gpl/waq)