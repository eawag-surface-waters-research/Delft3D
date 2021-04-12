file(GLOB source    src/test_bilin5.f90
                    src/test_combinepaths.f90
                    src/test_deltares_common.f90
                    src/test_distance.f90
                    src/test_geometry_module.f90
                    src/test_hash_search.f90
                    src/test_precision_basics.f90
                    src/test_properties.f90
                    src/test_sorting_algorithms.f90
                    src/test_string_module.f90)

add_executable(test_deltares_common ${source})

set(oss_dependencies deltares_common  ftnunit)
oss_include_libraries(test_deltares_common oss_dependencies)

target_link_libraries(test_deltares_common  deltares_common 
                                            ftnunit)