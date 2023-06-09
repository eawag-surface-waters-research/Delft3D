set(library_name flow2d3d_io_dol_f)

# Define include directories
get_module_include_path(${checkout_src_root}/${deltares_common_module} 
                        ${library_name}
                        deltares_common_include_path)
get_module_include_path(${checkout_src_root}/${esmfsm_module} 
                        ${library_name} 
                        esmfsm_module_include_path)
get_module_include_path(${checkout_src_root}/${flow2d3d_data_module} 
                        ${library_name}
                        flow2d3d_data_include_path)

message(STATUS "Configuring '${library_name}' with the following deltares_common include path: '${deltares_common_include_path}'")
message(STATUS "Configuring '${library_name}' with the following esmfsm include path: '${esmfsm_module_include_path}'")
message(STATUS "Configuring '${library_name}' with the following data include path: '${flow2d3d_data_include_path}'")

# Note that the CMAKE_CURRENT_SOURCE_DIR has a path to THIS CMakeLists location
set(include_path ${CMAKE_CURRENT_SOURCE_DIR}/include)
include_directories(${include_path}
                    ${deltares_common_include_path}
                    ${esmfsm_module_include_path}
                    ${morphology_kernel_module_include_path})

# Set directory which contains all source files
set(src_path src)

# Gather source files
get_fortran_source_files(${src_path} source_files)

# Define library
add_library(${library_name} ${source_files})

# Set dependencies
set(oss_dependencies_general    deltares_common
                                deltares_common_mpi
                                ec_module
                                flow2d3d_data
                                io_hyd
                                io_netcdf
                                morphology_io
                                morphology_kernel) 
# Add dependencies
if (UNIX)
    # the `pkg_check_modules` function is created with this call
    find_package(PkgConfig REQUIRED)

    # these calls create special `PkgConfig::<MODULE>` variables
    pkg_check_modules(NETCDF REQUIRED IMPORTED_TARGET netcdf)
    pkg_check_modules(NETCDF_FTN REQUIRED IMPORTED_TARGET netcdf-fortran)

    set(oss_dependencies ${oss_dependencies_general})

    target_link_libraries(${library_name}
         ${oss_dependencies}
         PkgConfig::NETCDF
         PkgConfig::NETCDF_FTN)
    
endif(UNIX)
if (WIN32)
    set(oss_dependencies ${oss_dependencies_general}
                        netcdf4
                        netcdff)

    target_link_libraries(${library_name} ${oss_dependencies})
endif(WIN32)
oss_include_libraries(${library_name} oss_dependencies)

if(UNIX)
    target_link_libraries(${library_name} ${oss_dependencies})
endif()

# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files})
set_target_properties (${library_name} PROPERTIES FOLDER engines_gpl/flow2d3d)

# Change project flags at the end of this file
target_compile_options(${library_name} PRIVATE "${qauto_threaded_flags}")
target_compile_options(${library_name} PRIVATE "$<$<CONFIG:Debug>:${check_nobounds_flag}>")
