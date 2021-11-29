# Set directory which contains all source files
set(src_path src)

# Gather source files
get_fortran_source_files(${src_path} source_files)

# Define executable
set(executable_name waqmerge)
add_executable(${executable_name} ${source_files})

# Add dependencies
if (UNIX)
    # the `pkg_check_modules` function is created with this call
    find_package(PkgConfig REQUIRED)

    # these calls create special `PkgConfig::<MODULE>` variables
    pkg_check_modules(NETCDF REQUIRED IMPORTED_TARGET netcdf)
    pkg_check_modules(NETCDF_FTN REQUIRED IMPORTED_TARGET netcdf-fortran)
    pkg_check_modules(PROJ REQUIRED IMPORTED_TARGET proj)

    set(oss_dependencies deltares_common
                         deltares_common_c
                         io_netcdf
                         io_hyd
                         waq_utils_f
                         waqmerge_version_number)

    oss_include_libraries(${executable_name} oss_dependencies)

    target_link_libraries(${executable_name}
         ${oss_dependencies}
         PkgConfig::NETCDF
         PkgConfig::NETCDF_FTN
         PkgConfig::PROJ)
endif(UNIX)

if (WIN32)
    set(oss_dependencies deltares_common
                         deltares_common_c
                         io_netcdf
                         io_hyd
                         netcdf4
                         netcdff
                         waq_utils_f
                         waqmerge_version_number)

    oss_include_libraries(${executable_name} oss_dependencies)
    target_link_libraries(${executable_name} ${oss_dependencies})
endif(WIN32)

if(UNIX)
    message(STATUS "netcdf lib dir is ${NETCDF_LIBRARY_DIRS}")
    target_link_directories(${executable_name} PRIVATE ${NETCDF_LIBRARY_DIRS})

    message(STATUS "Setting target_compile_definitions in Unix")
    set_property(TARGET ${executable_name} PROPERTY LINKER_LANGUAGE Fortran)
endif(UNIX)

if (WIN32)
   target_compile_definitions(${executable_name} PRIVATE WIN64)
endif(WIN32)

target_link_libraries(${executable_name} netcdf)

if (WIN32)
    # Set linker properties
    message(STATUS "Setting linker properties in windows")
    target_link_directories(${executable_name}
                            PRIVATE
                            "${checkout_src_root}/third_party_open/netcdf/netCDF 4.6.1/lib")

    target_link_libraries(${executable_name}
                          "netcdf.lib")
endif(WIN32)


# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files})
set_target_properties (${executable_name} PROPERTIES FOLDER tools_gpl/waqmerge)

# Set post-build step
set(install_dir ${CMAKE_BINARY_DIR})
set(build_dir ${CMAKE_BINARY_DIR})

post_build_target (${executable_name}
                   ${install_dir}
                   ${build_dir}
                   ${checkout_src_root}
                   ${executable_name})

install(TARGETS ${executable_name} RUNTIME  DESTINATION bin)

