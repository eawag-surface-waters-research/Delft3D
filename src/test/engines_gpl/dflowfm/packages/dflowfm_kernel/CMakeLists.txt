set(executable_name dflowfm_kernel_test)

file(GLOB source    src/*.f90)

add_executable(${executable_name} ${source})

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

include_directories(${gisinternals_path}
                    ${mpich2_path}
                    ${petsc_path}
                    ${tecplot_path})


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



