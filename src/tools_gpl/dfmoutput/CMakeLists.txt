# Set directories
# Set the directory of where the source code is located.
set(src_path src)
# Set version file variables
# Note that the CMAKE_CURRENT_SOURCE_DIR has a path to THIS CMakeLists location
set(ini_version_file ${CMAKE_CURRENT_SOURCE_DIR}/res/version_number.ini)

# Generate the version files
set(fortran_version_file ${CMAKE_CURRENT_SOURCE_DIR}/src/dfmoutput_version.F90)
generate_version_files(${fortran_version_file} ${checkout_src_root} ${ini_version_file})

# Gather the source files
get_fortran_source_files(${src_path} sources)

                            
# Define the executable
set(executable_name dfmoutput)
add_executable(${executable_name} ${sources} ${fortran_version_file})

# Define prebuild event
prebuild_version_number(${executable_name} ${fortran_version_file} 
                                        ${checkout_src_root} 
                                        ${ini_version_file})

# Add dependencies
if (UNIX)
    # the `pkg_check_modules` function is created with this call
    find_package(PkgConfig REQUIRED)

    # these calls create special `PkgConfig::<MODULE>` variables
    pkg_check_modules(NETCDF REQUIRED IMPORTED_TARGET netcdf)
    pkg_check_modules(NETCDF_FTN REQUIRED IMPORTED_TARGET netcdf-fortran)
    pkg_check_modules(PROJ REQUIRED IMPORTED_TARGET proj)

    set(oss_dependencies deltares_common 
                         io_netcdf 
                         FLAP 
                         fortranc 
                         fortrangis
                         shp)

    oss_include_libraries(${executable_name} oss_dependencies)

    target_link_libraries(${executable_name}
         ${oss_dependencies}
         PkgConfig::NETCDF
         PkgConfig::NETCDF_FTN
         PkgConfig::PROJ)
endif(UNIX)

if (WIN32)
    set(oss_dependencies deltares_common
                         io_netcdf
                         FLAP
                         fortranc
                         fortrangis
                         netcdf4
                         netcdff
                         proj
                         shp)

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
   target_compile_options(${executable_name} PRIVATE "${check_bounds_flag}")
endif(WIN32)

# Define additional compilation flags
set_source_files_properties(${fortran_version_file}
                            PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")

target_link_libraries(${executable_name} netcdf)

if (WIN32)
    include_directories(${gisinternals_path}
                        ${mpich2_path}
                        ${tecplot_path})
                        

    # Set linker properties
    message(STATUS "Setting linker properties in windows")
    target_link_directories(${executable_name}
                            PRIVATE
                            ${gisinternals_path}
                            "${checkout_src_root}/third_party_open/mpich2/x64/lib"
                            "${checkout_src_root}/third_party_open/netcdf/netCDF 4.6.1/lib"
                            "${checkout_src_root}/third_party_open/pthreads/bin/x64"
                            "${checkout_src_root}/third_party_open/Tecplot/lib/x64")

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
                          "pthreadVC2.lib"
                          "fmpich2.lib"
                          "gdal_i.lib"
                          "tecio.lib")

    message(STATUS "Setting target_compile_definitions in windows")
    target_compile_definitions(${executable_name} PRIVATE "WIN32;HAVE_DISPLAY=0;HAVE_METIS;HAVE_POLYPACK;HAVE_MPI;HAVE_PROJ;HAVE_SHAPELIB;HAVE_GDAL;HAVE_PETSC")

    # Set linker options
    message(STATUS "Setting target_link_options in windows")
    target_link_options(${executable_name} PRIVATE ${nologo_flag})
endif(WIN32)

# Create the folder structure in vfproj
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${sources} ${fortran_version_file})
set_target_properties (${executable_name} PROPERTIES FOLDER tools_gpl/dfmoutput)
install(TARGETS ${executable_name} RUNTIME  DESTINATION bin)


# Set post-build step
set(install_dir ${CMAKE_BINARY_DIR})
set(build_dir ${CMAKE_BINARY_DIR})

post_build_target (${executable_name}
                   ${install_dir} 
                   ${build_dir} 
                   ${checkout_src_root} 
                   ${executable_name})
