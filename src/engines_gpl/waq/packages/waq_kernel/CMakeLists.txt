# Set path for include files 
# Note that the CMAKE_CURRENT_SOURCE_DIR has a path to THIS CMakeLists location
set(waq_kernel_include_path ${CMAKE_CURRENT_SOURCE_DIR}/../../include)
include_directories(${waq_kernel_include_path})

# Set directory which contains all source files
set(src_path src)

# Gather source files
get_fortran_source_files(${src_path} source_files)
list(REMOVE_ITEM source_files   ${CMAKE_CURRENT_SOURCE_DIR}/${src_path}/dlwqtr_template.f)

# Define library
set(library_name waq_kernel)
add_library(${library_name} ${source_files})

# Set dependencies
if(UNIX)
    # the `pkg_check_modules` function is created with this call
    find_package(PkgConfig REQUIRED)

    # these calls create special `PkgConfig::<MODULE>` variables
    pkg_check_modules(NETCDF REQUIRED IMPORTED_TARGET netcdf)
    pkg_check_modules(NETCDF_FTN REQUIRED IMPORTED_TARGET netcdf-fortran)

    set(oss_dependencies    deltares_common
                            delftio
                            part_data_f
                            part_io_f
                            part_kernel_f
                            part_utils_f
                            waq_data
                            waq_plugin_wasteload
                            waq_process
                            waq_utils_c
                            waq_utils_f
                            io_netcdf
                            io_hyd)
    oss_include_libraries(${library_name} oss_dependencies)

    target_link_libraries(${library_name}
         ${oss_dependencies}
         PkgConfig::NETCDF
         PkgConfig::NETCDF_FTN)
endif(UNIX)

if(WIN32)
    set(oss_dependencies    deltares_common
                            delftio
                            netcdf4
                            netcdff
                            part_data_f
                            part_io_f
                            part_kernel_f
                            part_utils_f
                            waq_data
                            waq_plugin_wasteload
                            waq_process
                            waq_utils_c
                            waq_utils_f
                            io_netcdf
                            io_hyd
                            netcdf4
                            netcdff)
    oss_include_libraries(${library_name} oss_dependencies)

    target_link_libraries(${library_name} ${oss_dependencies})
endif(WIN32)


# Set additional compilation properties
target_compile_options(${library_name} PRIVATE "${openmp_flag}")
target_compile_options(${library_name} PRIVATE "${qauto_threaded_flags}")
target_compile_options(${library_name} PRIVATE "${waq_default_flags}")
target_compile_options(${library_name} PRIVATE "$<$<CONFIG:Debug>:${waq_debug_flags}>")

# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files}
                                                    ${preproccessor_files}
                                                    ${openmq_files}
                                                    ${proces_file})
set_target_properties (${library_name} PROPERTIES FOLDER engines_gpl/waq)
