# Shared definitions
# Set directory which contains all source files
set(src_path ../src)

# Gather source files
set(headers ../include/swanout1_deltares.inc)

get_fortran_source_files(${src_path} swan_sourcefiles)
list(FILTER swan_sourcefiles EXCLUDE REGEX ".*HottifySWAN.f90$")
list(FILTER swan_sourcefiles EXCLUDE REGEX ".*swanhcat.for$")

set(executable_files ${swan_sourcefiles})

# Define executable
set(executable_name swan_omp_exe)
add_executable(${executable_name}   ${headers}
                                    ${executable_files})

# Set dependencies
set(exe_dependencies    netcdf4
                        netcdff) 
oss_include_libraries(${executable_name} exe_dependencies)
target_link_libraries(${executable_name} ${exe_dependencies})

if (WIN32)
    include_directories("${CMAKE_CURRENT_SOURCE_DIR}/../include")
    # Set linker properties
    message(STATUS "Setting linker properties in windows")
    target_link_directories(${executable_name}
                            PRIVATE
                            "${checkout_src_root}/third_party_open/netcdf/netCDF 4.6.1/lib")

    target_link_libraries(${executable_name}
                            "netcdf.lib")

    # Set linker options
    message(STATUS "Setting target_link_options in windows")
    target_link_options(${executable_name} PRIVATE ${nologo_flag})
endif(WIN32)

if(UNIX)
    target_link_libraries(${executable_name} ${exe_dependencies})
endif(UNIX)

# Define additional compilation flags
set_source_files_properties(${checkout_src_root}/third_party_open/swan/src/ocpids.F
                            ${checkout_src_root}/third_party_open/swan/src/swancom1.F
                            ${checkout_src_root}/third_party_open/swan/src/swanmain.F
                            ${checkout_src_root}/third_party_open/swan/src/swanparll.F
                            ${checkout_src_root}/third_party_open/swan/src/swmod2.F
                            PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")

# Set additional compilation properties, specific for Debug mode
target_compile_options(${executable_name} PRIVATE "$<$<CONFIG:Debug>:${check_bounds_flag}>")

# Define preprocessor definitions 
if (WIN32)
    target_compile_definitions(${executable_name} PRIVATE "WIN32;SWANEXE")
endif(WIN32)

if (UNIX)
    # Not needed (yet): target_compile_definitions(${executable_name} PRIVATE "SWANEXE")
endif(UNIX)




# Define how the files should be structured within Visual Studio
source_group(TREE ${checkout_src_root}/third_party_open/swan FILES ${executable_files})
set_target_properties (${executable_name} PROPERTIES FOLDER third_party_open/swan_omp)

if (WIN32)
    set_target_properties(${executable_name} PROPERTIES LINK_FLAGS "/LARGEADDRESSAWARE /STACK:20000000")
endif(WIN32)

# Set post-build step
set(install_dir ${CMAKE_BINARY_DIR})
set(build_dir ${CMAKE_BINARY_DIR})

# post_build_target (${executable_name}
#                    ${install_dir} 
#                    ${build_dir} 
#                    ${checkout_src_root} 
#                    ${executable_name})
