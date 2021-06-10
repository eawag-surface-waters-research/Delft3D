# Set directory which contains all source files
set(src_path src)

# Gather source files
get_fortran_source_files(${src_path}/bloom bloom_source)

# Gather source files
get_fortran_source_files(${src_path}/charon charon_source)
                                    
# Gather source files
get_fortran_source_files(${src_path}/proc_preprocess proc_preprocess_source)

# Gather source files
get_fortran_source_files(${src_path}/waq_process waq_process_source)
list(REMOVE_ITEM waq_process_source ${CMAKE_CURRENT_SOURCE_DIR}/${src_path}/waq_process/dradio.f)

file(GLOB waq_solvesaphe_source ${src_path}/waq_process/solvesaphe/mod_precision.f90
                                ${src_path}/waq_process/solvesaphe/mod_chemconst.f90
                                ${src_path}/waq_process/solvesaphe/mod_acbw_phsolvers.F90)

get_fortran_source_files(${src_path}/waq_tables waq_tables_source)

# Define library
set(library_name waq_process)
add_library(${library_name} ${bloom_source} 
                            ${charon_source} 
                            ${proc_preprocess_source}
                            ${waq_solvesaphe_source}
                            ${waq_process_source}
                            ${waq_tables_source})

# Set dependencies
if (UNIX)
    # the `pkg_check_modules` function is created with this call
    find_package(PkgConfig REQUIRED)

    # these calls create special `PkgConfig::<MODULE>` variables
    pkg_check_modules(NETCDF REQUIRED IMPORTED_TARGET netcdf)
    pkg_check_modules(NETCDF_FTN REQUIRED IMPORTED_TARGET netcdf-fortran)

    set(oss_dependencies    deltares_common
                            nefis
                            waq_utils_c
                            waq_utils_f)
    oss_include_libraries(${library_name} oss_dependencies)
    target_link_libraries(${library_name} ${oss_dependencies}
         PkgConfig::NETCDF
         PkgConfig::NETCDF_FTN)
endif(UNIX)

if (WIN32)
    set(oss_dependencies    deltares_common
                            nefis
                            netcdf4
                            netcdff
                            waq_utils_c
                            waq_utils_f)
    oss_include_libraries(${library_name} oss_dependencies)
    target_link_libraries(${library_name} ${oss_dependencies})
endif(WIN32)


target_include_directories(${library_name}  PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/../../include")

# Set additional compilation properties
target_compile_options(${library_name} PRIVATE "${extend_source132_flag}")
target_compile_options(${library_name} PRIVATE "$<$<CONFIG:Debug>:${check_bounds_flag}>")

# Define how the files should be structured in Visual studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${bloom_source} 
                                                    ${charon_source} 
                                                    ${proc_preprocess_source}
                                                    ${waq_solvesaphe_source}
                                                    ${waq_process_source}
                                                    ${waq_tables_source})

set_target_properties (${library_name} PROPERTIES FOLDER engines_gpl/waq)

