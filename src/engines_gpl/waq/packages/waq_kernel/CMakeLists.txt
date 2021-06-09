# Set path for include files 
# Note that the CMAKE_CURRENT_SOURCE_DIR has a path to THIS CMakeLists location
set(waq_kernel_include_path ${CMAKE_CURRENT_SOURCE_DIR}/../../include)
include_directories(${waq_kernel_include_path})

# Set directory which contains all source files
set(src_path src)

# Gather source files
get_fortran_source_files(${src_path} source_files)
list(REMOVE_ITEM source_files   ${CMAKE_CURRENT_SOURCE_DIR}/${src_path}/dlwqtr_template.f)

# Gather preprocessing files
file(GLOB preproccessor_files   ${CMAKE_CURRENT_SOURCE_DIR}/${src_path}/delpar00.F
                                ${CMAKE_CURRENT_SOURCE_DIR}/${src_path}/dlwq2_hydro_set.F)

# Gather OpenMQ files
set(openmq_files    ${CMAKE_CURRENT_SOURCE_DIR}/${src_path}/dlwqnf.f
                    ${CMAKE_CURRENT_SOURCE_DIR}/${src_path}/dlwqng.f
                    ${CMAKE_CURRENT_SOURCE_DIR}/${src_path}/dlwqnm.f)

# Gather OpenMQ and reentrancy files
set(proces_file ${CMAKE_CURRENT_SOURCE_DIR}/${src_path}/proces.f)

# Define library
set(library_name waq_kernel)
add_library(${library_name} ${source_files}
                            ${preproccessor_files}
                            ${openmq_files}
                            ${proces_file})

# Add dependencies
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
                            waq_plugin_wasteload
                            waq_process
                            waq_utils_c
                            waq_utils_f) 
    oss_include_libraries(${library_name} oss_dependencies)
    target_link_libraries(${library_name} ${oss_dependencies}
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
                            waq_plugin_wasteload
                            waq_process
                            waq_utils_c
                            waq_utils_f) 
    oss_include_libraries(${library_name} oss_dependencies)
    target_link_libraries(${library_name} ${oss_dependencies})
endif(WIN32)

# Define additional compilation flags
set_property(SOURCE ${preproccessor_files}
             PROPERTY COMPILE_OPTIONS "${file_preprocessor_flag}")

# Set additional compilation properties
target_compile_options(${library_name} PRIVATE "${extend_source132_flag}")
target_compile_options(${library_name} PRIVATE "$<$<CONFIG:Debug>:${check_bounds_flag}>")

if(WIN32)
    set_property(SOURCE ${openmq_files}
            PROPERTY COMPILE_OPTIONS "${openmp_flag}")
    set_property(SOURCE ${proces_file}
             PROPERTY COMPILE_OPTIONS "${openmp_flag}" "${generate_reentrancy_threaded_flag}" "${extend_source132_flag}")
endif(WIN32)

if(UNIX)
    set_property(SOURCE ${proces_file}
             PROPERTY COMPILE_OPTIONS "${generate_reentrancy_threaded_flag}" "${extend_source132_flag}")
endif(UNIX)

# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files}
                                                    ${preproccessor_files}
                                                    ${openmq_files}
                                                    ${proces_file})
set_target_properties (${library_name} PROPERTIES FOLDER engines_gpl/waq)

