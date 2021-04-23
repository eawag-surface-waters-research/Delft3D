set(executable_name waq_utils_f_test)

file(GLOB source    src/waq_utils/*.f90)

add_executable(${executable_name} ${source})

set(oss_dependencies deltares_common
                     deltares_common_c
                     ec_module
                     ftnunit
                     gridgeom
                     io_netcdf
                     kdtree_wrapper
                     kdtree2
                     netcdf4
                     netcdff
                     waq_utils_f)

target_link_directories(${executable_name} PRIVATE
                            "${checkout_src_root}/third_party_open/netcdf/netCDF 4.6.1/lib")

oss_include_libraries(${executable_name}   oss_dependencies)
target_link_libraries(${executable_name} ${oss_dependencies}
                                         "netcdf.lib")


