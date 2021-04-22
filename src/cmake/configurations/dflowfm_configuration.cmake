project(dflowfm)

# Specify the modules to be included
add_subdirectory(${checkout_src_root}/${deltares_common_module} deltares_common)
add_subdirectory(${checkout_src_root}/${deltares_common_c_module} deltares_common_c)
add_subdirectory(${checkout_src_root}/${deltares_common_mpi_module} deltares_common_mpi)

# Trachytopes
add_subdirectory(${checkout_src_root}/${trachytopes_kernel_module} trachytopes_kernel)
add_subdirectory(${checkout_src_root}/${trachytopes_io_module} trachytopes_io)

# Flow1d
add_subdirectory(${checkout_src_root}/${flow1d_core_module} flow1d_core)
add_subdirectory(${checkout_src_root}/${flow1d_io_module} flow1d_io)
add_subdirectory(${checkout_src_root}/${flow1d_module} flow1d)

# Waq
add_subdirectory(${checkout_src_root}/${waq_utils_c_module} waq_utils_c)
add_subdirectory(${checkout_src_root}/${waq_utils_f_module} waq_utils_f)
add_subdirectory(${checkout_src_root}/${waq_process_module} waq_process)
add_subdirectory(${checkout_src_root}/${wq_processes_module} wq_processes)

# Morphology
add_subdirectory(${checkout_src_root}/${morphology_plugins_c_module} morphology_plugins_c)
add_subdirectory(${checkout_src_root}/${morphology_data_module} morphology_data)
add_subdirectory(${checkout_src_root}/${morphology_kernel_module} morphology_kernel)
add_subdirectory(${checkout_src_root}/${morphology_io_module} morphology_io)

# Hydrology
add_subdirectory(${checkout_src_root}/${hydrology_kernel_module} dhydrology_kernel)

# Dflowfm modules 
add_subdirectory(${checkout_src_root}/${dflowfm_kernel_module} dflowfm_kernel)
add_subdirectory(${checkout_src_root}/${dflowfm_cli_exe_module} dflowfm_cli_exe)
add_subdirectory(${checkout_src_root}/${dflowfm_lib_module} dflowfm_lib)

# Tools_gpl
# DFMoutput
add_subdirectory(${checkout_src_root}/${dfmoutput_module} dfmoutput)

# Third party libraries
# kdtree2
add_subdirectory(${checkout_src_root}/${kdtree_module} kdtree2)
add_subdirectory(${checkout_src_root}/${kdtree_wrapper_module} kdtree_wrapper)

# md5
add_subdirectory(${checkout_src_root}/${md5_module} md5)

# metis
if(WIN32)
    add_subdirectory(${checkout_src_root}/${metis_module} metis)
endif(WIN32)
add_subdirectory(${checkout_src_root}/${metisoptions_module} metisoptions) # Note that the metisoptions should be loaded AFTER metis is loaded, as it depends on settings set by the CMakeLists.txt of the metis library

# petsc
if(WIN32)
    add_subdirectory(${checkout_src_root}/${petsc_module} petsc)
endif(WIN32)

# triangle
add_subdirectory(${checkout_src_root}/${triangle_c_module} triangle_c)

# libsigwatch
add_subdirectory(${checkout_src_root}/${libsigwatch_module} libsigwatch)

# FLAP
add_subdirectory(${checkout_src_root}/${FLAP_module} FLAP)

# fortrangis
add_subdirectory(${checkout_src_root}/${fortrangis_module} fortrangis)
add_subdirectory(${checkout_src_root}/${shp_module} shp)
if(WIN32)
    add_subdirectory(${checkout_src_root}/${proj_module} proj)
endif(WIN32)

# netcdf
if(WIN32)
    add_subdirectory(${checkout_src_root}/${netcdf_module} netcdff)
endif(WIN32)

# io_netcdf
add_subdirectory(${checkout_src_root}/${io_netcdf_module} io_netcdf)

# ec_module
add_subdirectory(${checkout_src_root}/${ec_module} ec_module)

# gridgeom
add_subdirectory(${checkout_src_root}/${gridgeom_module} gridgeom)

# interacter_stub
add_subdirectory(${checkout_src_root}/${interacter_stub_module} interacter_stub)

# polypack
add_subdirectory(${checkout_src_root}/${polypack_module} polypack)

# Nefis
add_subdirectory(${checkout_src_root}/${nefis_module} nefis)

if(UNIX)
    # install
    add_subdirectory(${checkout_src_root}/${install_dflowfm_module} install_dflowfm)
endif()
