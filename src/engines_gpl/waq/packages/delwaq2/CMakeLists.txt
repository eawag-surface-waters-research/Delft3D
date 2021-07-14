# Define include directories
# Note that the CMAKE_CURRENT_SOURCE_DIR has a path to THIS CMakeLists location
set(waq_root_path ${CMAKE_CURRENT_SOURCE_DIR}/../../) # Trace back to the WAQ path

# Set version file variables
set(waq_version_path ${waq_root_path}/version)
set(waq_include_path ${waq_root_path}/include)

set(ini_version_file ${waq_version_path}/version_number.ini)
set(fortran_version_file ${CMAKE_CURRENT_SOURCE_DIR}/src/delwaq2_version.F90)
set(fortran_version_file_delwaq ${waq_root_path}/packages/delwaq/src/delwaq_version.F90)
set(rc_version_file ${waq_version_path}/version_number_delwaq2.rc)
set(delwaq_files ${waq_root_path}/packages/delwaq/src/delwaq2.f
                 ${waq_root_path}/packages/delwaq/src/delwaq2_global_data.f90
                 ${waq_root_path}/packages/delwaq/src/getidentification.f
                 ${waq_root_path}/packages/delwaq/src/waq_mpi_processes_stubs.f90)

# Set icon file
set(icon_file ${waq_version_path}/waq_icon.rc)

# Generate version files
generate_version_files(${rc_version_file} 
                       ${checkout_src_root}
                       ${ini_version_file} 
                       ON)

generate_version_files(${fortran_version_file} 
                       ${checkout_src_root}
                       ${ini_version_file} 
                       ON)

generate_version_files(${fortran_version_file_delwaq} 
                       ${waq_root_path}
                       ${ini_version_file} 
                       ON)

# Set directory which contains all source files
set(src_path src)

# Gather source files
get_fortran_source_files(${src_path} source_files)

# Define executable
set(executable_name delwaq2)
add_executable(${executable_name} ${source_files}
                                  ${icon_file}
                                  ${rc_version_file}
                                  ${fortran_version_file_delwaq}
                                  ${delwaq_files})

# Define prebuild events
prebuild_version_number(${executable_name} ${rc_version_file} 
                                        ${checkout_src_root}
                                        ${ini_version_file}
                                        ON)

prebuild_version_number(${executable_name} ${fortran_version_file} 
                                        ${checkout_src_root}
                                        ${ini_version_file}
                                        ON)

prebuild_version_number(${executable_name} ${fortran_version_file_delwaq} 
                                        ${waq_root_path}
                                        ${ini_version_file}
                                        ON)

set_source_files_properties(${fortran_version_file} ${fortran_version_file_delwaq}
                            PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")

# Add dependencies
if(UNIX)
    # the `pkg_check_modules` function is created with this call
    find_package(PkgConfig REQUIRED)

    # these calls create special `PkgConfig::<MODULE>` variables
    pkg_check_modules(NETCDF     REQUIRED IMPORTED_TARGET netcdf)
    pkg_check_modules(NETCDF_FTN REQUIRED IMPORTED_TARGET netcdf-fortran)

    set(oss_dependencies    deltares_common
                            deltares_common_c
                            delftio
                            delftio_shm
                            io_netcdf
                            nefis
                            waq_io
                            waq_kernel
                            waq_plugin_wasteload
                            waq_process
                            waq_utils_c
                            waq_utils_f 
                            esmfsm)
    target_link_libraries(${executable_name}
         ${oss_dependencies}
         PkgConfig::NETCDF
         PkgConfig::NETCDF_FTN
         uuid)
endif(UNIX)

if(WIN32)
    set(oss_dependencies    delwaq_dll
                            netcdf4
                            netcdff) 
    target_link_libraries(${executable_name} ${oss_dependencies})
endif(WIN32)

oss_include_libraries(${executable_name} oss_dependencies)


# Set additional compilation properties
target_compile_options(${executable_name} PRIVATE "${extend_source132_flag}")
target_compile_options(${executable_name} PRIVATE "$<$<CONFIG:Debug>:${check_bounds_flag}>")
target_compile_options(${executable_name} PRIVATE "$<$<CONFIG:Debug>:${check_pointer}>")

if(UNIX)
    target_include_directories(${executable_name} PRIVATE ${waq_include_path})

    message(STATUS "netcdf lib dir is ${NETCDF_LIBRARY_DIRS}")
    target_link_directories(${executable_name} PRIVATE ${NETCDF_LIBRARY_DIRS})
    
    target_link_options(${executable_name} PRIVATE ${openmp_flag})
    set_property(TARGET ${executable_name} PROPERTY LINKER_LANGUAGE Fortran)
endif(UNIX)

if (WIN32)
    target_include_directories(${executable_name} PRIVATE ${waq_include_path} ${mpich2_path})

    # Set linker properties
    message(STATUS "Setting linker properties in windows")
    target_link_directories(${executable_name}
                            PRIVATE
                            "${checkout_src_root}/third_party_open/mpich2/x64/lib"
                            "${checkout_src_root}/third_party_open/netcdf/netCDF 4.6.1/lib"
                            "${checkout_src_root}/third_party_open/pthreads/bin/x64")


    target_link_libraries(${executable_name}   
                            "comctl32.lib"
                            "wsock32.lib"
                            "netapi32.lib"
                            "kernel32.lib"
                            "user32.lib" 
                            "Shlwapi.lib"
                            "Gdi32.lib"
                            "Winspool.lib"
                            "netcdf.lib"
                            "fmpich2.lib"
                            "pthreadVC2.lib")

    # Set linker options
    message(STATUS "Setting target_link_options in windows")
    target_link_options(${executable_name} PRIVATE ${nologo_flag})
endif(WIN32)

# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files})
source_group(Resources FILES    ${icon_file} 
                                ${rc_version_file})
set_target_properties(${executable_name} PROPERTIES COMPILE_FLAGS ${openmp_flag})
set_target_properties (${executable_name} PROPERTIES FOLDER engines_gpl/waq)
if (WIN32)
    set_target_properties(${executable_name} PROPERTIES LINK_FLAGS "/LARGEADDRESSAWARE /STACK:20000000")
endif(WIN32)

# Set post-build step
set(install_dir ${CMAKE_BINARY_DIR})
set(build_dir ${CMAKE_BINARY_DIR})

post_build_target (${executable_name}
                   ${install_dir} 
                   ${build_dir} 
                   ${checkout_src_root} 
                   ${executable_name})

install(TARGETS ${executable_name} RUNTIME  DESTINATION bin)

