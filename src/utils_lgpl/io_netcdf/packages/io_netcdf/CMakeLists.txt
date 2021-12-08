# Set version file variables
# Note that the CMAKE_CURRENT_SOURCE_DIR has a path to THIS CMakeLists location
set(ini_version_file ${CMAKE_CURRENT_SOURCE_DIR}/include/version_number.ini)
set(fortran_version_file ${CMAKE_CURRENT_SOURCE_DIR}/src/io_netcdf_version.F90)

if(UNIX)
    # the `pkg_check_modules` function is created with this call
    find_package(PkgConfig REQUIRED)

    # these calls create special `PkgConfig::<MODULE>` variables
    pkg_check_modules(NETCDF REQUIRED IMPORTED_TARGET netcdf)
    pkg_check_modules(NETCDF_FTN REQUIRED IMPORTED_TARGET netcdf-fortran)
endif(UNIX)

# Generate the F90 version file
generate_version_files(${fortran_version_file} ${ini_version_file})
            
set(src_path src)
get_fortran_source_files(${src_path} sources)

###########
# IO_NETCDF
###########
set(library_name io_netcdf)
add_library(${library_name} ${sources} ${fortran_version_file})

# Create the folder structure in vfproj
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${sources} ${fortran_version_file})
set_target_properties (${library_name} PROPERTIES FOLDER utils_lgpl/io_netcdf)


if(UNIX)
    # Add dependency
    set(oss_dependencies deltares_common)
    target_compile_definitions(${library_name} PRIVATE LINUX64)    
    oss_include_libraries(${library_name} oss_dependencies)

    target_link_libraries(${library_name} ${oss_dependencies} PkgConfig::NETCDF PkgConfig::NETCDF_FTN)
endif(UNIX)

if (WIN32)
    # Add dependency
    set(oss_dependencies deltares_common
                        netcdf4
                        netcdff)
    target_compile_definitions(${library_name} PRIVATE WIN64)
    oss_include_libraries(${library_name} oss_dependencies)
    
    target_compile_options(${library_name} PRIVATE "${heap_arrays_one_flag}")
endif(WIN32)

# Set additional compilation properties, specific for Debug mode
target_compile_options(${library_name} PRIVATE "$<$<CONFIG:Debug>:${check_bounds_flag}>")

# Prebuild events
prebuild_version_number(${library_name} ${fortran_version_file} 
                                        ${checkout_src_root} 
                                        ${ini_version_file})

# Set additional compilation properties
set_source_files_properties(${fortran_version_file}
                            ${src_path}/coordinate_reference_system.F90
                            ${src_path}/io_ugrid.F90
                            PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")


###############
# IO_NETCDF_DLL
###############
string(TOUPPER ${CONFIGURATION_TYPE} configuration_type)
if ( ${configuration_type} STREQUAL "DFLOWFM" OR 
     ${configuration_type} STREQUAL "ALL" OR 
     ${configuration_type} STREQUAL "TOOLS" )
    set(library_name io_netcdf_dll)
    # Add dynamic io_netcdf library
    add_library(${library_name} SHARED ${src_path}/io_netcdf_api.F90)

    if(UNIX)
        target_link_directories(${library_name} PRIVATE ${NETCDF_LIBRARY_DIRS}) 
        set(oss_dependencies deltares_common  io_netcdf)
        oss_include_libraries(${library_name} oss_dependencies)
        target_link_libraries(${library_name} deltares_common
                                              io_netcdf
                                              PkgConfig::NETCDF
                                              PkgConfig::NETCDF_FTN)
    endif(UNIX)
    if (WIN32)
        target_link_directories(${library_name}
                                PRIVATE
                                "${checkout_src_root}/third_party_open/netcdf/netCDF 4.6.1/lib")
        set(oss_dependencies deltares_common
                             io_netcdf
                             netcdf4
                             netcdff)
        oss_include_libraries(${library_name} oss_dependencies)
        target_link_libraries(${library_name} deltares_common
                                              io_netcdf
                                              netcdf
                                              netcdf4
                                              netcdff)
    endif(WIN32)

    # Set additional compilation properties, specific for Debug mode
    target_compile_options(${library_name} PRIVATE "$<$<CONFIG:Debug>:${check_bounds_flag}>")

    # Create the folder structure in vfproj
    source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${src_path}/io_netcdf_api.F90)
    set_target_properties (${library_name} PROPERTIES FOLDER utils_lgpl/io_netcdf)

    # Set post-build step
    set(install_dir ${CMAKE_BINARY_DIR})
    set(build_dir ${CMAKE_BINARY_DIR})

    post_build_target (${library_name}
                       ${install_dir} 
                       ${build_dir} 
                       ${checkout_src_root} 
                       ${library_name})
    install(TARGETS ${library_name} DESTINATION lib)
endif()

