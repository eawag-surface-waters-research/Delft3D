# Set directory which contains all source files
set(src_path src)

file(GLOB dlwq_netcdf_source    ${src_path}/dlwq_netcdf/*.f90
                                ${src_path}/dlwq_netcdf/*.f)

file(GLOB mod_couplib_source    ${src_path}/mod_couplib/*.f90
                                ${src_path}/mod_couplib/*.f)

file(GLOB mod_timings_source    ${src_path}/mod_timings/*.f90
                                ${src_path}/mod_timings/*.f)

file(GLOB waq_utils_source  ${src_path}/waq_utils/*.f90
                            ${src_path}/waq_utils/*.f
                            ${src_path}/waq_utils/*.F)
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
    set(oss_dependencies    deltares_common
                            io_netcdf)
endif(UNIX)
if (WIN32)
    set(oss_dependencies    deltares_common
                            io_netcdf
                            netcdf4
                            netcdff)
endif(WIN32)

oss_include_libraries(${library_name} oss_dependencies)

if(UNIX)
   target_link_libraries(${library_name} waq_process deltares_common)
   target_compile_options(${library_name}  PRIVATE ${c_compiler_flags})
endif()

# Set additional compilation properties
target_compile_options(${library_name} PRIVATE "${extend_source132_flag}")
set_source_files_properties(${src_path}/mod_couplib/m_couplib.F90
                            ${src_path}/mod_couplib/m_globcomm.F90
                            ${src_path}/mod_couplib/m_sndrcv.F90
                            PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")

target_include_directories(${library_name}  PRIVATE ${mpich2_path}/
                                                    "${CMAKE_CURRENT_SOURCE_DIR}/../../include")

# Define how the files should be structured in Visual studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES  ${dlwq_netcdf_source}
                                                     ${mod_couplib_source}
                                                     ${mod_timings_source}
                                                     ${waq_utils_source})

set_target_properties (${library_name} PROPERTIES FOLDER utils_gpl/wq_processes)

