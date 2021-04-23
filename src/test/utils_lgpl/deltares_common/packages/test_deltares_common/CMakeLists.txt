file(GLOB source    src/*.f90)

add_executable(test_deltares_common ${source})

set(oss_dependencies deltares_common  ftnunit)
oss_include_libraries(test_deltares_common oss_dependencies)

target_link_libraries(test_deltares_common  deltares_common 
                                            ftnunit)