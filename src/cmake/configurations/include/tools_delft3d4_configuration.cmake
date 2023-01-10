# Tools for Delft3D4

# UTILS_LGPL

if(NOT TARGET deltares_common)
    add_subdirectory(${checkout_src_root}/${deltares_common_module} deltares_common)
endif()
if(NOT TARGET deltares_common_c)
    add_subdirectory(${checkout_src_root}/${deltares_common_c_module} deltares_common_c)
endif()
if(NOT TARGET deltares_common_mpi)
    add_subdirectory(${checkout_src_root}/${deltares_common_mpi_module} deltares_common_mpi)
endif()


# TOOLS_GPL

if(NOT TARGET datsel)
    add_subdirectory(${checkout_src_root}/${datsel_module} datsel)
    add_subdirectory(${checkout_src_root}/${datsel_f_module} datsel_f)
    add_subdirectory(${checkout_src_root}/${datsel_version_number_module} datsel_version_number)
endif()

if(NOT TARGET kubint)
    add_subdirectory(${checkout_src_root}/${kubint_module} kubint)
    add_subdirectory(${checkout_src_root}/${kubint_f_module} kubint_f)
    add_subdirectory(${checkout_src_root}/${kubint_version_number_module} kubint_version_number)
endif()

if(NOT TARGET lint)
    add_subdirectory(${checkout_src_root}/${lint_module} lint)
    add_subdirectory(${checkout_src_root}/${lint_f_module} lint_f)
    add_subdirectory(${checkout_src_root}/${lint_version_number_module} lint_version_number)
endif()

if(NOT TARGET nesthd1)
    add_subdirectory(${checkout_src_root}/${nesthd1_module} nesthd1)
endif()

if(NOT TARGET nesthd2)
    add_subdirectory(${checkout_src_root}/${nesthd2_module} nesthd2)
endif()
