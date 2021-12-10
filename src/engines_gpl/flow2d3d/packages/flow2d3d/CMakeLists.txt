set(library_name flow2d3d)

# Set directory which contains all source files
set(src_path src)
set(include_path include)

set(dd_path ${src_path}/dd)
set(dd_path ${src_path}/dd)
set(dd_iterators_path ${src_path}/dd/iterators)
set(dd_mapper_path ${src_path}/dd/mapper)

# Define include directories
get_module_include_path(${checkout_src_root}/${d_hydro_lib_module} 
                        ${library_name}
                        d_hydro_lib_include_path)

get_module_include_path(${checkout_src_root}/${deltares_common_c_module} 
                        ${library_name}
                        deltares_common_c_include_path)

get_module_include_path(${checkout_src_root}/${esmfsm_module} 
                        ${library_name} 
                        esmfsm_module_include_path)

message(STATUS "Configuring '${library_name}' with the following deltares_common_c include path: '${deltares_common_c_include_path}'")
message(STATUS "Configuring '${library_name}' with the following d_hydro_lib include path: '${d_hydro_lib_include_path}'")
message(STATUS "Configuring '${library_name}' with the following esmfsm include path: '${esmfsm_module_include_path}'")

include_directories(${include_path}
                    ${deltares_common_c_include_path}
                    ${d_hydro_lib_include_path}
                    ${esmfsm_module_include_path}
                    ${dd_path}
                    ${dd_iterators_path}
                    ${dd_mapper_path}
                    ${pthreads_path}
                    ${mpi_include_path}
                    ${expat_path})

# Set version file variables
set(version_path ${CMAKE_CURRENT_SOURCE_DIR}/../) # Path to determine the version number from
set(flow2d3d_version_path ${CMAKE_CURRENT_SOURCE_DIR}/${include_path})

set(ini_version_file ${flow2d3d_version_path}/version_number.ini)
set(h_version_file ${flow2d3d_version_path}/flow2d3d_version.h)

# Generate version files
generate_version_files(${h_version_file} 
                       ${ini_version_file})

# Gather source files 
file(GLOB header_files ${include_path}/*.h)
file(GLOB header_dd_files ${dd_path}/*.h)
file(GLOB header_dd_iterators_files ${dd_iterators_path}/*.h)
file(GLOB header_dd_mapper_files ${dd_mapper_path}/*.h)

file(GLOB source_root_files ${src_path}/*.cpp)
file(GLOB source_dd_files ${dd_path}/*.cpp)
file(GLOB source_dd_iterators_files ${dd_iterators_path}/*.cpp)
file(GLOB source_dd_mapper_files ${dd_mapper_path}/*.cpp)

# Define library
add_library(${library_name} SHARED  ${header_files}
                                    ${header_dd_files}
                                    ${header_dd_iterators_files}
                                    ${header_dd_mapper_files}
                                    ${source_root_files}
                                    ${source_dd_files}
                                    ${source_dd_iterators_files}
                                    ${source_dd_mapper_files})

# Define prebuild events
prebuild_version_number(${library_name} ${h_version_file} 
                                        ${version_path}
                                        ${ini_version_file})

# Set dependencies
set(oss_dependencies    data
                        delftio
                        delftio_shm
                        deltares_common
                        deltares_common_mpi
                        deltares_common_c
                        d_hydro_lib
                        gridgeom
                        ec_module
                        esmfsm
                        esmfsm_c
                        esmfsm_version_number
                        io
                        io_dol_f
                        io_hyd
                        io_netcdf
                        kdtree_wrapper
                        kdtree2
                        kernel
                        kernel_dd_f
                        manager
                        morphology_data
                        morphology_io
                        morphology_kernel
                        morphology_plugins_c
                        nefis
                        netcdf4
                        netcdff
                        plugin_culvert_c
                        plugin_user
                        triangle_c) 
oss_include_libraries(${library_name} oss_dependencies)
target_link_libraries(${library_name} ${oss_dependencies})

if (WIN32)
    # Set linker properties
    message(STATUS "Setting linker properties in windows")
    target_link_directories(${library_name}
                            PRIVATE
                            "${checkout_src_root}/third_party_open/netcdf/netCDF 4.6.1/lib"
                            "${mpi_library_path}"
                            "${checkout_src_root}/third_party_open/expat/x64/x64"
                            "${checkout_src_root}/third_party_open/pthreads/bin/x64")

    target_link_libraries(${library_name}
                            "libexpat.lib"
                            "${mpi_fortran_library}"
                            "pthreadVC2.lib"
                            "wsock32.lib"
                            "netapi32.lib"
                            "comctl32.lib"
                            "user32.lib"
                            "advapi32.lib"
                            "comdlg32.lib"
                            "gdi32.lib"
                            "winspool.lib" 
                            "netcdf.lib")

    # Set linker options
    message(STATUS "Setting target_link_options in windows")
    target_link_options(${library_name} PRIVATE ${nologo_flag})
endif(WIN32)

# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${header_files}
                                                    ${header_dd_files}
                                                    ${header_dd_iterators_files}
                                                    ${header_dd_mapper_files}
                                                    ${source_root_files}
                                                    ${source_dd_files}
                                                    ${source_dd_iterators_files}
                                                    ${source_dd_mapper_files})
set_target_properties (${library_name} PROPERTIES FOLDER engines_gpl/flow2d3d)

# Set post-build step
set(install_dir ${CMAKE_BINARY_DIR})
set(build_dir ${CMAKE_BINARY_DIR})

post_build_target (${library_name}
                   ${install_dir} 
                   ${build_dir} 
                   ${checkout_src_root} 
                   ${library_name})
