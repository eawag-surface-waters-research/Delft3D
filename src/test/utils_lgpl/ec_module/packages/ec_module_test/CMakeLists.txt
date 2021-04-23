set(executable_name ec_module_test)

file(GLOB source    src/*.f90
                    src/*.F90)

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
                     netcdff)

set_source_files_properties(${CMAKE_CURRENT_SOURCE_DIR}/src/ec_module_test.F90
                            ${CMAKE_CURRENT_SOURCE_DIR}/src/m_ec_module_test.F90
                            PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")

target_link_directories(${executable_name} PRIVATE
                            "${checkout_src_root}/third_party_open/netcdf/netCDF 4.6.1/lib")

oss_include_libraries(${executable_name}   oss_dependencies)
target_link_libraries(${executable_name} ${oss_dependencies}
                                         "netcdf.lib")


