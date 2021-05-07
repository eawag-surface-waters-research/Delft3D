project(tools)

# Specify the modules to be included

# Deltares_common
if(NOT TARGET deltares_common)
    add_subdirectory(${checkout_src_root}/${deltares_common_module} deltares_common)
endif()

if(NOT TARGET deltares_common_c)
    add_subdirectory(${checkout_src_root}/${deltares_common_c_module} deltares_common_c)
endif()

if(NOT TARGET deltares_common_mpi)
    add_subdirectory(${checkout_src_root}/${deltares_common_mpi_module} deltares_common_mpi)
endif()

# Waq
if(NOT TARGET waq_utils_c)
    add_subdirectory(${checkout_src_root}/${waq_utils_c_module} waq_utils_c)
endif()

if(NOT TARGET waq_utils_f)
    add_subdirectory(${checkout_src_root}/${waq_utils_f_module} waq_utils_f)
endif()

if(NOT TARGET waq_kernel)
    add_subdirectory(${checkout_src_root}/${waq_kernel_module} waq_kernel)
endif()

if(NOT TARGET wq_processes)
    add_subdirectory(${checkout_src_root}/${wq_processes_module} wq_processes)
endif()

if(NOT TARGET waq_plugin_wasteload)
    add_subdirectory(${checkout_src_root}/${waq_plugin_wasteload_module} waq_plugin_wasteload)
endif()



# Tools_gpl
# Mormerge
if(NOT TARGET mormerge)
    add_subdirectory(${checkout_src_root}/${mormerge_module} mormerge)
endif()

# dfmoutput
if(NOT TARGET dfmoutput)
    add_subdirectory(${checkout_src_root}/${dfmoutput_module} dfmoutput)
endif()

# Waqpb
if(NOT TARGET waqpb_lib)
    add_subdirectory(${checkout_src_root}/${waqpb_lib_module} waqpb_lib)
endif()

if(NOT TARGET waqpb_import)
    add_subdirectory(${checkout_src_root}/${waqpb_import_module} waqpb_import)
endif()

if(NOT TARGET waqpb_export)
    add_subdirectory(${checkout_src_root}/${waqpb_export_module} waqpb_export)
endif()

# Waq_run_processes
if(NOT TARGET waq_run_processes_version_number)
    add_subdirectory(${checkout_src_root}/${waq_run_processes_version_number_module} waq_run_processes_version_number)
endif()

if(NOT TARGET waq_run_processes)
    add_subdirectory(${checkout_src_root}/${waq_run_processes_module} waq_run_processes)
endif()

# duprol2delwaq
if(NOT TARGET duprol2delwaq)
    add_subdirectory(${checkout_src_root}/${duprol2delwaq_module} duprol2delwaq)
endif()

# Datsel
if(NOT TARGET datsel_version_number)
    add_subdirectory(${checkout_src_root}/${datsel_version_number_module} datsel_version_number)
endif()

if(NOT TARGET datsel_f)
    add_subdirectory(${checkout_src_root}/${datsel_f_module} datsel_f)
endif()

if(NOT TARGET datsel)
    add_subdirectory(${checkout_src_root}/${datsel_module} datsel)
endif()

# Third party
# FLAP
if(NOT TARGET FLAP)
    add_subdirectory(${checkout_src_root}/${FLAP_module} FLAP)
endif()

# fortrangis
if(NOT TARGET fortrangis)
    add_subdirectory(${checkout_src_root}/${fortrangis_module} fortrangis)
endif()

if(NOT TARGET shp)
    add_subdirectory(${checkout_src_root}/${shp_module} shp)
endif()

if(WIN32)
    if(NOT TARGET proj)
        add_subdirectory(${checkout_src_root}/${proj_module} proj)
    endif()
endif(WIN32)
# netcdf
if(WIN32)
    if(NOT TARGET netcdff)
        add_subdirectory(${checkout_src_root}/${netcdf_module} netcdff)
    endif()
endif(WIN32)

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
