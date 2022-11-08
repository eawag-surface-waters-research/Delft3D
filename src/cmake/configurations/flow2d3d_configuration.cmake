project(dflow2d3d)

# Specify the modules to be included

# Deltares common
if(NOT TARGET deltares_common)
    add_subdirectory(${checkout_src_root}/${deltares_common_module} deltares_common)
endif()

if(NOT TARGET deltares_common_c)
    add_subdirectory(${checkout_src_root}/${deltares_common_c_module} deltares_common_c)
endif()

if(NOT TARGET deltares_common_mpi)
    add_subdirectory(${checkout_src_root}/${deltares_common_mpi_module} deltares_common_mpi)
endif()

# Morphology
if(NOT TARGET morphology_plugins_c)
    add_subdirectory(${checkout_src_root}/${morphology_plugins_c_module} morphology_plugins_c)
endif()

if(NOT TARGET morphology_data)
    add_subdirectory(${checkout_src_root}/${morphology_data_module} morphology_data)
endif()

if(NOT TARGET morphology_kernel)
    add_subdirectory(${checkout_src_root}/${morphology_kernel_module} morphology_kernel)
endif()

if(NOT TARGET morphology_io)
    add_subdirectory(${checkout_src_root}/${morphology_io_module} morphology_io)
endif()

# delftio
if(NOT TARGET delftio_shm)
    add_subdirectory(${checkout_src_root}/${delftio_shm_module} delftio_shm)
endif()

if(NOT TARGET delftio)
    add_subdirectory(${checkout_src_root}/${delftio_module} delftio)
endif()

# Third party libraries
# kdtree2
if(NOT TARGET kdtree2)
    add_subdirectory(${checkout_src_root}/${kdtree_module} kdtree2)
endif()

if(NOT TARGET kdtree_wrapper)
    add_subdirectory(${checkout_src_root}/${kdtree_wrapper_module} kdtree_wrapper)
endif()

# triangle
if(NOT TARGET triangle_c)
    add_subdirectory(${checkout_src_root}/${triangle_c_module} triangle_c)
endif()

if(WIN32)
    if(NOT TARGET netcdff)
        add_subdirectory(${checkout_src_root}/${netcdf_module} netcdff)
    endif()
endif()

# io_netcdf
if(NOT TARGET io_netcdf)
    add_subdirectory(${checkout_src_root}/${io_netcdf_module} io_netcdf)
endif()

# ec_module
if(NOT TARGET ec_module)
    add_subdirectory(${checkout_src_root}/${ec_module} ec_module)
endif()

# io_hyd
if(NOT TARGET io_hyd)
    add_subdirectory(${checkout_src_root}/${io_hyd_module} io_hyd)
endif()

# Nefis
if(NOT TARGET nefis)
    add_subdirectory(${checkout_src_root}/${nefis_module} nefis)
endif()

# emfsm
if(NOT TARGET esmfsm_version_number)
    add_subdirectory(${checkout_src_root}/${esmfsm_version_number_module} esmfsm_version_number)
endif()

if(NOT TARGET esmfsm_c)
    add_subdirectory(${checkout_src_root}/${esmfsm_c_module} esmfsm_c)
endif()

if(NOT TARGET esmfsm)
    add_subdirectory(${checkout_src_root}/${esmfsm_module} esmfsm)
endif()

# D-Hydro lib
if(NOT TARGET d_hydro_lib)
    add_subdirectory(${checkout_src_root}/${d_hydro_lib_module} d_hydro_lib)
endif()

# Flow2d3d
# Include the flow2d3d components as last as it depends on the properties set in the other components
# Data needs to be loaded prior before the other flow2d3d modules as its include directory needs to be set
add_subdirectory(${checkout_src_root}/${flow2d3d_data_module} flow2d3d_data) 
add_subdirectory(${checkout_src_root}/${flow2d3d_plugin_culvert_c_module} flow2d3d_plugin_culvert_c) 
add_subdirectory(${checkout_src_root}/${flow2d3d_plugin_user_module} flow2d3d_plugin_user) 
add_subdirectory(${checkout_src_root}/${flow2d3d_io_dol_f_module} flow2d3d_io_dol_f) 
add_subdirectory(${checkout_src_root}/${flow2d3d_io_module} flow2d3d_io)
add_subdirectory(${checkout_src_root}/${flow2d3d_kernel_dd_f_module} flow2d3d_kernel_dd_f)
add_subdirectory(${checkout_src_root}/${flow2d3d_kernel_module} flow2d3d_kernel)
add_subdirectory(${checkout_src_root}/${flow2d3d_manager_module} flow2d3d_manager)
add_subdirectory(${checkout_src_root}/${flow2d3d_module} flow2d3d)