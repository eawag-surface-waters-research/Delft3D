# Set the directory of where the source code is located.
set(src_path src)
set(header_path include)

# Set variable to trace to the dimr_lib package as some files are included from dimr_lib for this project
set(dimr_lib_path "${CMAKE_CURRENT_SOURCE_DIR}/../dimr_lib/")

# Set version file variables
set(absolute_header_path "${CMAKE_CURRENT_SOURCE_DIR}/${header_path}") 
set(ini_version_file "${absolute_header_path}/version_number.ini")
set(rc_version_file "${absolute_header_path}/dimr_exe_version.rc")
set(header_version_file "${absolute_header_path}/dimr_exe_version.h")

if (UNIX)    
    include_directories(${NETCDF_INCLUDE_DIRS}) 
    add_definitions(-DD3D_PLUGIN_EXT=".so")
endif(UNIX)

# Generate the version files
generate_version_files(${rc_version_file} ${checkout_src_root} ${ini_version_file})
generate_version_files(${header_version_file} ${checkout_src_root} ${ini_version_file})

# Gather source files
file(GLOB source ${src_path}/*.cpp)

set(dimr_src_path "${dimr_lib_path}/src")
set(dimr_lib_sources "${dimr_src_path}/clock.cpp"
                     "${dimr_src_path}/exception.cpp"
                     "${dimr_src_path}/log.cpp")

# Define the executable
set(executable_name dimr)
add_executable(${executable_name}   ${source}
                                    ${dimr_lib_sources}
                                    ${rc_version_file} 
                                    ${header_version_file} 
                                    ${ini_version_file})

# Define prebuild events
prebuild_version_number(${executable_name}  ${rc_version_file} 
                                            ${checkout_src_root}
                                            ${ini_version_file})
prebuild_version_number(${executable_name}  ${header_version_file} 
                                            ${checkout_src_root}
                                            ${ini_version_file})

# Common UNIX and WIN32 link library dependency
target_link_libraries(${executable_name} netcdf)

if(UNIX)

   set(oss_dependencies    deltares_common_c
                           dimr_lib
                           expat)

   oss_include_libraries(${executable_name} oss_dependencies) 
   

   target_include_directories(${executable_name} 
                               PRIVATE  
                               ${header_path}
                              "${dimr_lib_path}/include"
                              "${checkout_src_root}/third_party_open/expat/x64/include")

    target_link_libraries(${executable_name} ${oss_dependencies}) 
    target_link_directories(${executable_name} PRIVATE ${NETCDF_LIBRARY_DIRS})
    
    set(THREADS_PREFER_PTHREAD_FLAG ON)
    find_package(Threads REQUIRED)
    target_link_libraries(${executable_name} Threads::Threads)


endif(UNIX)

if (WIN32)

   set(oss_dependencies    deltares_common_c
                           dimr_lib) 
    # Define preprocessor definitions
    target_compile_definitions(${executable_name} PRIVATE WIN32;_CONSOLE)
    target_compile_definitions(${executable_name} PRIVATE $<$<CONFIG:DEBUG>:_DEBUG>)
    
    target_include_directories(${executable_name} 
                                PRIVATE  
                                ${header_path}
                                "${dimr_lib_path}/include"
                                "${checkout_src_root}/third_party_open/expat/x64/include"
                                "${checkout_src_root}/third_party_open/mpich2/include"
                                "${checkout_src_root}/third_party_open/netcdf/netCDF 4.6.1/include"
                                "${checkout_src_root}/third_party_open/pthreads/include/x64")

    # Set the linker properties
    target_link_directories(${executable_name}
                            PRIVATE
                            "${checkout_src_root}/third_party_open/netcdf/netCDF 4.6.1/lib"
                            "${checkout_src_root}/third_party_open/expat/x64/x64/"
                            "${checkout_src_root}/third_party_open/mpich2/x64/lib"
                            "${checkout_src_root}/third_party_open/pthreads/bin/x64")

    target_link_libraries(${executable_name}    ${oss_dependencies}
                                                "fmpich2.lib"
                                                "libexpat.lib"
                                                "mpi.lib"
                                                "pthreadVC2.lib")
endif(WIN32)

# Create the folder structure in vfproj
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source} ${dimr_lib_sources} ${header_version_file}  ${rc_version_file})
set_target_properties (${executable_name} PROPERTIES FOLDER engines_gpl/dimr)
if (WIN32)
    set_target_properties(${executable_name} PROPERTIES LINK_FLAGS "/LARGEADDRESSAWARE /STACK:20000000")
endif(WIN32)

# post-build
set(install_dir ${CMAKE_BINARY_DIR})
set(build_dir ${CMAKE_BINARY_DIR})

 message(STATUS "build_config ${build_config}")

post_build_target( ${executable_name}
                   ${install_dir} 
                   ${build_dir} 
                   ${checkout_src_root} 
                   ${executable_name}  )

install(TARGETS ${executable_name} RUNTIME  DESTINATION bin)
if (UNIX)
    install(PROGRAMS ${CMAKE_SOURCE_DIR}/../engines_gpl/dimr/scripts/generic/lnx64/run_dimr.sh             DESTINATION bin)
    install(PROGRAMS ${CMAKE_SOURCE_DIR}/../engines_gpl/dimr/scripts/generic/lnx64/submit_dimr.sh          DESTINATION bin)
endif(UNIX)

if (WIN32)
    install(PROGRAMS ${CMAKE_SOURCE_DIR}/../engines_gpl/dimr/scripts/generic/win64/run_dimr.bat            DESTINATION bin)
    install(PROGRAMS ${CMAKE_SOURCE_DIR}/../engines_gpl/dimr/scripts/generic/win64/run_dimr_parallel.bat   DESTINATION bin)
endif(WIN32)

