project(tools)

# Specify the modules to be included
add_subdirectory(${checkout_src_root}/${deltares_common_module} deltares_common)
add_subdirectory(${checkout_src_root}/${deltares_common_c_module} deltares_common_c)
add_subdirectory(${checkout_src_root}/${deltares_common_mpi_module} deltares_common_mpi)

# Waq
add_subdirectory(${checkout_src_root}/${waq_utils_f_module} waq_utils_f)
add_subdirectory(${checkout_src_root}/${waq_process_module} waq_process)
add_subdirectory(${checkout_src_root}/${waq_plugin_wasteload_module} waq_plugin_wasteload)
add_subdirectory(${checkout_src_root}/${waq_kernel_module} waq_kernel)
add_subdirectory(${checkout_src_root}/${waq_utils_c_module} waq_utils_c)

# Tools_gpl
# Mormerge
add_subdirectory(${checkout_src_root}/${mormerge_module} mormerge)

# dfmoutput
add_subdirectory(${checkout_src_root}/${dfmoutput_module} dfmoutput)

# Waqpb
add_subdirectory(${checkout_src_root}/${waqpb_lib_module} waqpb_lib)
add_subdirectory(${checkout_src_root}/${waqpb_import_module} waqpb_import)
add_subdirectory(${checkout_src_root}/${waqpb_export_module} waqpb_export)

# Waq_run_processes
add_subdirectory(${checkout_src_root}/${waq_run_processes_version_number_module} waq_run_processes_version_number)
add_subdirectory(${checkout_src_root}/${waq_run_processes_module} waq_run_processes)

# duprol2delwaq
add_subdirectory(${checkout_src_root}/${duprol2delwaq_module} duprol2delwaq)

# Datsel
add_subdirectory(${checkout_src_root}/${datsel_version_number_module} datsel_version_number)
add_subdirectory(${checkout_src_root}/${datsel_f_module} datsel_f)
add_subdirectory(${checkout_src_root}/${datsel_module} datsel)

# Third party
# FLAP
add_subdirectory(${checkout_src_root}/${FLAP_module} FLAP)

# fortrangis
add_subdirectory(${checkout_src_root}/${fortrangis_module} fortrangis)
add_subdirectory(${checkout_src_root}/${shp_module} shp)
if(WIN32)
    add_subdirectory(${checkout_src_root}/${proj_module} proj)
endif(WIN32)

# netcdf
add_subdirectory(${checkout_src_root}/${netcdf_module} netcdff)

# io_netcdf
add_subdirectory(${checkout_src_root}/${io_netcdf_module} io_netcdf)

# Nefis
add_subdirectory(${checkout_src_root}/${nefis_module} nefis)

# Part
add_subdirectory(${checkout_src_root}/${part_data_f_module} part_data_f)
add_subdirectory(${checkout_src_root}/${part_utils_f_module} part_utils_f)
add_subdirectory(${checkout_src_root}/${part_io_f_module} part_io_f)
add_subdirectory(${checkout_src_root}/${part_kernel_f_module} part_kernel_f)

# delftio
add_subdirectory(${checkout_src_root}/${delftio_shm_module} delftio_shm)
add_subdirectory(${checkout_src_root}/${delftio_module} delftio)

# emfsm
add_subdirectory(${checkout_src_root}/${esmfsm_version_number_module} esmfsm_version_number)
add_subdirectory(${checkout_src_root}/${esmfsm_c_module} esmfsm_c)
add_subdirectory(${checkout_src_root}/${esmfsm_module} esmfsm)
