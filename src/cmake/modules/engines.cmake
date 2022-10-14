# File to define the engines components and their corresponding tests
set(engines_path engines)

# D-Flow Flexible Mesh
set(dflowfm_interacter_module ${engines_path}/dflowfm/packages/dflowfm_interacter)

# DRR Rainfall Runoff
set(rr_module_path "${engines_path}/rr/packages")
set(rr_dll_module "${rr_module_path}/rr_dll")
set(rr_kernel_c_module "${rr_module_path}/rr_kernel_c")
set(rr_kernel_f_module "${rr_module_path}/rr_kernel_f")
set(rr_walrus_c_module "${rr_module_path}/rr_walrus_c")
set(rr_module "${rr_module_path}/rr")


# set(install_rr_module cmake/install_rr)
