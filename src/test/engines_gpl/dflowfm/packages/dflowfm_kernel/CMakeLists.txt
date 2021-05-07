set(executable_name dflowfm_kernel_test)

file(GLOB source    src/*.f90)

add_executable(${executable_name} ${source})

if(UNIX)
    set(THREADS_PREFER_PTHREAD_FLAG ON)
    find_package(Threads REQUIRED)

    # the `pkg_check_modules` function is created with this call
    find_package(PkgConfig REQUIRED)

    # these calls create special `PkgConfig::<MODULE>` variables
    pkg_check_modules(NETCDF     REQUIRED IMPORTED_TARGET netcdf)
    pkg_check_modules(NETCDF_FTN REQUIRED IMPORTED_TARGET netcdf-fortran)
    pkg_check_modules(PETSC      REQUIRED IMPORTED_TARGET PETSc)
    pkg_check_modules(PROJ       REQUIRED IMPORTED_TARGET proj)

    set(oss_dependencies    deltares_common
                            deltares_common_c
                            deltares_common_mpi
                            dhydrology_kernel
                            ec_module
                            flow1d
                            flow1d_core
                            flow1d_io
                            ftnunit
                            gridgeom
                            io_netcdf
                            interacter_stub
                            kdtree_wrapper
                            kdtree2
                            fortranc
                            fortrangis
                            libsigwatch
                            md5digest
                            md5
                            metisoptions
                            morphology_data
                            morphology_io
                            morphology_kernel
                            morphology_plugins_c
                            nefis
                            polypack
                            shp
                            trachytopes_io
                            trachytopes_kernel
                            triangle_c
                            waq_process
                            waq_utils_c
                            waq_utils_f
                            wq_processes
                            dflowfm_kernel)

    oss_include_libraries(${executable_name} oss_dependencies)

    message(STATUS "netcdf lib dir is ${NETCDF_LIBRARY_DIRS}")
    target_link_directories(${executable_name} PRIVATE ${NETCDF_LIBRARY_DIRS})


    target_link_libraries(${executable_name}
         ${oss_dependencies}
         Threads::Threads
         PkgConfig::NETCDF
         PkgConfig::NETCDF_FTN
         PkgConfig::PETSC
         PkgConfig::PROJ
         "metis")

    set_property(TARGET ${executable_name} PROPERTY LINKER_LANGUAGE Fortran)
    set_target_properties(${executable_name} PROPERTIES LINK_FLAGS ${openmp_flag})
endif(UNIX)



if(WIN32)
    set(oss_dependencies deltares_common
                         deltares_common_c
                         deltares_common_mpi
                         dhydrology_kernel
                         ec_module
                         flow1d
                         flow1d_core
                         flow1d_io
                         ftnunit
                         gridgeom
                         io_netcdf
                         kdtree_wrapper
                         kdtree2
                         fortranc
                         fortrangis
                         libsigwatch
                         md5digest
                         md5
                         metis
                         metisoptions
                         morphology_data
                         morphology_io
                         morphology_kernel
                         morphology_plugins_c
                         nefis
                         netcdf4
                         netcdff
                         petsc
                         polypack
                         shp
                         trachytopes_io
                         trachytopes_kernel
                         triangle_c
                         waq_process
                         waq_utils_c
                         waq_utils_f
                         wq_processes
                         interacter_stub
                         dflowfm_kernel)


target_link_directories(${executable_name} PRIVATE
                        ${gisinternals_path}
                        "${checkout_src_root}/third_party_open/mpich2/x64/lib"
                        "${checkout_src_root}/third_party_open/netcdf/netCDF 4.6.1/lib"
                        "${checkout_src_root}/third_party_open/petsc/petsc-3.10.2/lib/x64/Release"
                        "${checkout_src_root}/third_party_open/pthreads/bin/x64"
                        "${checkout_src_root}/third_party_open/Tecplot/lib/x64")

oss_include_libraries(${executable_name}   oss_dependencies)
target_link_libraries(${executable_name} ${oss_dependencies}
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
                          "libpetsc.lib"
                          "gdal_i.lib"
                          "tecio.lib")


endif(WIN32)

include_directories(${gisinternals_path}
                    ${mpich2_path}
                    ${petsc_path}
                    ${tecplot_path})

# Create the folder structure in vfproj
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source})
set_target_properties (${executable_name} PROPERTIES FOLDER tests/engines_gpl/dflowfm)

# post-build
set(install_dir ${CMAKE_BINARY_DIR})
set(build_dir ${CMAKE_BINARY_DIR})
set(build_project ${executable_name})

post_build_target( ${executable_name}
                   ${install_dir} 
                   ${build_dir} 
                   ${checkout_src_root} 
                   ${build_project} )

install(TARGETS ${executable_name} RUNTIME  DESTINATION bin)

