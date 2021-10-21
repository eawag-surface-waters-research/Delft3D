# Set directory which contains all source files
set(src_path src)

set(dlwq_netcdf_path ${src_path}/dlwq_netcdf)
get_fortran_source_files(${dlwq_netcdf_path} dlwq_netcdf_source)

set(mod_couplib_path ${src_path}/mod_couplib)
get_fortran_source_files(${mod_couplib_path} mod_couplib_source)
list(REMOVE_ITEM mod_couplib_source ${CMAKE_CURRENT_SOURCE_DIR}/${src_path}/mod_couplib/sndrcv_wrappers.F90)

set(mod_timings_path ${src_path}/mod_timings)
get_fortran_source_files(${mod_timings_path} mod_timings_source)

set(waq_utils_path ${src_path}/waq_utils)
get_fortran_source_files(${waq_utils_path} waq_utils_source)
list(REMOVE_ITEM waq_utils_source ${CMAKE_CURRENT_SOURCE_DIR}/${src_path}/waq_utils/gregor.f)

if(UNIX)
   list(REMOVE_ITEM waq_utils_source ${CMAKE_CURRENT_SOURCE_DIR}/${src_path}/waq_utils/avundf.f)
else(UNIX)
   list(REMOVE_ITEM waq_utils_source ${CMAKE_CURRENT_SOURCE_DIR}/${src_path}/waq_utils/avundf_linux.f)
endif(UNIX)

set(library_name waq_utils_f)
add_library(${library_name} ${dlwq_netcdf_source}
                            ${mod_couplib_source}
                            ${mod_timings_source}
                            ${waq_utils_source})

# Set dependencies
if (UNIX)
    # the `pkg_check_modules` function is created with this call
    find_package(PkgConfig REQUIRED)

    # these calls create special `PkgConfig::<MODULE>` variables
    pkg_check_modules(NETCDF     REQUIRED IMPORTED_TARGET netcdf)
    pkg_check_modules(NETCDF_FTN REQUIRED IMPORTED_TARGET netcdf-fortran)

    # WARNING: waq_process and waq_util_f are (weakly) circular dependent
    # Just add waq_process to "target_lib_libraries", nothing more
    set(oss_dependencies    deltares_common
                            io_netcdf)
    target_link_libraries(${library_name}
         ${oss_dependencies}
         waq_process
         PkgConfig::NETCDF
         PkgConfig::NETCDF_FTN)
endif(UNIX)
if (WIN32)
    set(oss_dependencies    deltares_common
                            io_netcdf
                            netcdf4
                            netcdff)
endif(WIN32)

oss_include_libraries(${library_name} oss_dependencies)


# Set additional compilation properties
target_compile_options(${library_name} PRIVATE "${extend_source132_flag}")
target_compile_options(${library_name} PRIVATE "$<$<CONFIG:Debug>:${check_bounds_flag}>")
target_compile_options(${library_name} PRIVATE "$<$<CONFIG:Debug>:${check_pointer}>")

set_source_files_properties(${src_path}/mod_couplib/m_couplib.F90
                            ${src_path}/mod_couplib/m_globcomm.F90
                            ${src_path}/mod_couplib/m_sndrcv.F90
                            PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")

target_include_directories(${library_name}  PRIVATE ${mpi_include_path}/
                                                    "${CMAKE_CURRENT_SOURCE_DIR}/../../include")

# Define how the files should be structured in Visual studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES  ${dlwq_netcdf_source}
                                                     ${mod_couplib_source}
                                                     ${mod_timings_source}
                                                     ${waq_utils_source})

set_target_properties (${library_name} PROPERTIES FOLDER engines_gpl/waq)

