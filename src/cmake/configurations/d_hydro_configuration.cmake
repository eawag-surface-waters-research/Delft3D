project(d_hydro)

# D_HYDRO specific components
add_subdirectory(${checkout_src_root}/${d_hydro_module} d_hydro)

if(UNIX)
  # install
  add_subdirectory(${checkout_src_root}/${install_dimr_module} install_dimr)
endif()

# Specify the modules to be included
if(NOT TARGET deltares_common)
  add_subdirectory(${checkout_src_root}/${deltares_common_module} deltares_common)
endif()

if(NOT TARGET deltares_common_c)
  add_subdirectory(${checkout_src_root}/${deltares_common_c_module} deltares_common_c)
endif()

# for unix, we need to build expat xml parser
if(UNIX)
  if(NOT TARGET expat)
    add_subdirectory(${checkout_src_root}/${expat_module} expat)
  endif()
endif(UNIX)

# DIMR specific components
# D-Hydro lib
if(NOT TARGET d_hydro_lib)
    add_subdirectory(${checkout_src_root}/${d_hydro_lib_module} d_hydro_lib)
endif()

if(UNIX)
  # install
  add_subdirectory(${checkout_src_root}/${install_dimr_module} install_d_hydro)
endif()
