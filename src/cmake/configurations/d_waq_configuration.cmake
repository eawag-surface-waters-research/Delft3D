project(D-Waq)

# Specify the modules to be included
if(NOT TARGET deltares_common) 
    add_subdirectory(${checkout_src_root}/${deltares_common_module} deltares_common)
endif()

if(NOT TARGET deltares_common_c)
    add_subdirectory(${checkout_src_root}/${deltares_common_c_module} deltares_common_c)
endif()

# Waq
if(NOT TARGET waq_plugin_wasteload)
    add_subdirectory(${checkout_src_root}/${waq_plugin_wasteload_module} waq_plugin_wasteload)
endif()

if(NOT TARGET waq_utils_c)
    add_subdirectory(${checkout_src_root}/${waq_utils_c_module} waq_utils_c)
endif()

if(NOT TARGET waq_utils_f)
    add_subdirectory(${checkout_src_root}/${waq_utils_f_module} waq_utils_f)
endif()

if(NOT TARGET waq_process)
    add_subdirectory(${checkout_src_root}/${waq_process_module} waq_process)
endif()

if(NOT TARGET waq_kernel)
    add_subdirectory(${checkout_src_root}/${waq_kernel_module} waq_kernel)
endif()

if(NOT TARGET waq_io)
    add_subdirectory(${checkout_src_root}/${waq_io_module} waq_io)
endif()

if(NOT TARGET delwaq)
    add_subdirectory(${checkout_src_root}/${delwaq_module} delwaq)
endif()

if(NOT TARGET delwaq1)
    add_subdirectory(${checkout_src_root}/${delwaq1_module} delwaq1)
endif()

if(NOT TARGET delwaq2)
    add_subdirectory(${checkout_src_root}/${delwaq2_module} delwaq2)
endif()


# netcdf
if(WIN32)
    if(NOT TARGET netcdff)
        add_subdirectory(${checkout_src_root}/${netcdf_module} netcdff)
    endif()
endif()

# io_netcdf
if(NOT TARGET io_netcdf)
    add_subdirectory(${checkout_src_root}/${io_netcdf_module} io_netcdf)
endif()

# Nefis
if(NOT TARGET nefis)
    add_subdirectory(${checkout_src_root}/${nefis_module} nefis)
endif()

# delftio
if(NOT TARGET delftio_shm)
    add_subdirectory(${checkout_src_root}/${delftio_shm_module} delftio_shm)
endif()

if(NOT TARGET delftio)
    add_subdirectory(${checkout_src_root}/${delftio_module} delftio)
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

# part
if(NOT TARGET part_data_f)
    add_subdirectory(${checkout_src_root}/${part_data_f_module} part_data_f)
endif()
if(NOT TARGET part_utils_f)
    add_subdirectory(${checkout_src_root}/${part_utils_f_module} part_utils_f)
endif()
if(NOT TARGET part_io_f)
    add_subdirectory(${checkout_src_root}/${part_io_f_module} part_io_f)
endif()
if(NOT TARGET part_kernel_f)
    add_subdirectory(${checkout_src_root}/${part_kernel_f_module} part_kernel_f)
endif()

if(UNIX)
    add_subdirectory(${checkout_src_root}/cmake/install_wq install_wq)
endif()
