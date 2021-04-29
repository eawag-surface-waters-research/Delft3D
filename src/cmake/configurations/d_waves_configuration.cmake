project(D-Waves)

# Specify the modules to be included
add_subdirectory(${checkout_src_root}/${wave_data_module} wave_data)
add_subdirectory(${checkout_src_root}/${wave_io_module} wave_io)
add_subdirectory(${checkout_src_root}/${wave_kernel_module} wave_kernel)
add_subdirectory(${checkout_src_root}/${wave_manager_module} wave_manager)
add_subdirectory(${checkout_src_root}/${wave_module} wave)

# Deltares common 
add_subdirectory(${checkout_src_root}/${deltares_common_module} deltares_common)
add_subdirectory(${checkout_src_root}/${deltares_common_c_module} deltares_common_c)
add_subdirectory(${checkout_src_root}/${deltares_common_mpi_module} deltares_common_mpi)

# delftio
add_subdirectory(${checkout_src_root}/${delftio_shm_module} delftio_shm)
add_subdirectory(${checkout_src_root}/${delftio_module} delftio)

# Third party libraries
# kdtree2
add_subdirectory(${checkout_src_root}/${kdtree_module} kdtree2)
add_subdirectory(${checkout_src_root}/${kdtree_wrapper_module} kdtree_wrapper)

# triangle
add_subdirectory(${checkout_src_root}/${triangle_c_module} triangle_c)

# netcdf
if(WIN32)
    add_subdirectory(${checkout_src_root}/${netcdf_module} netcdff)
endif()

# io_netcdf
add_subdirectory(${checkout_src_root}/${io_netcdf_module} io_netcdf)

# ec_module
add_subdirectory(${checkout_src_root}/${ec_module} ec_module)

# gridgeom
add_subdirectory(${checkout_src_root}/${gridgeom_module} gridgeom)

# Nefis
add_subdirectory(${checkout_src_root}/${nefis_module} nefis)