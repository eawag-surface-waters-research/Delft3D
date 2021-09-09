project(swan)

# Specify the modules to be included
add_subdirectory(${checkout_src_root}/${swan_mpi_lib_module} swan_mpi_lib)
add_subdirectory(${checkout_src_root}/${swan_mpi_module} swan_mpi)
add_subdirectory(${checkout_src_root}/${swan_omp_module} swan_omp)

# netcdf
if(WIN32)
    if(NOT TARGET netcdff)
        add_subdirectory(${checkout_src_root}/${netcdf_module} netcdff)
    endif()
endif()
