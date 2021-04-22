# File to define the engines_gpl components and their corresponding tests
set(engines_gpl_path engines_gpl)

# D-Flow Flexible Mesh
set(dflowfm_kernel_module ${engines_gpl_path}/dflowfm/packages/dflowfm_kernel)
set(dflowfm_cli_exe_module ${engines_gpl_path}/dflowfm/packages/dflowfm-cli_exe)
set(dflowfm_lib_module ${engines_gpl_path}/dflowfm/packages/dflowfm_lib)

# Waq
set(waq_module_path "${engines_gpl_path}/waq/packages")
set(waq_utils_c_module "${waq_module_path}/waq_utils_c")
set(waq_utils_f_module "${waq_module_path}/waq_utils_f")
set(waq_process_module "${waq_module_path}/waq_process")
set(wq_processes_module "${waq_module_path}/wq_processes")
set(waq_plugin_wasteload_module "${waq_module_path}/waq_plugin_wasteload")
set(waq_kernel_module "${waq_module_path}/waq_kernel")
set(waq_io_module "${waq_module_path}/waq_io")
set(delwaq_module "${waq_module_path}/delwaq")
set(delwaq1_module "${waq_module_path}/delwaq1")
set(delwaq2_module "${waq_module_path}/delwaq2")

# Waves
set(wave_module_path "${engines_gpl_path}/wave/packages")
set(wave_data_module "${wave_module_path}/data")
set(wave_io_module "${wave_module_path}/io")
set(wave_kernel_module "${wave_module_path}/kernel")
set(wave_manager_module "${wave_module_path}/manager")
set(wave_module "${wave_module_path}/wave")

# Flow2D3D
set(flow2d3d_module_path "${engines_gpl_path}/flow2d3d/packages")
set(flow2d3d_data_module "${flow2d3d_module_path}/data")
set(flow2d3d_plugin_culvert_c_module "${flow2d3d_module_path}/plugin_culvert_c")
set(flow2d3d_plugin_user_module "${flow2d3d_module_path}/plugin_user")
set(flow2d3d_io_dol_f_module "${flow2d3d_module_path}/io_dol_f")
set(flow2d3d_io_module "${flow2d3d_module_path}/io")
set(flow2d3d_kernel_dd_f_module "${flow2d3d_module_path}/kernel_dd_f")
set(flow2d3d_kernel_module "${flow2d3d_module_path}/kernel")
set(flow2d3d_manager_module "${flow2d3d_module_path}/manager")
set(flow2d3d_module "${flow2d3d_module_path}/flow2d3d")

# Part
set(part_module_path "${engines_gpl_path}/part/packages")
set(part_data_f_module "${part_module_path}/data_f")
set(part_utils_f_module "${part_module_path}/utils_f")
set(part_io_f_module "${part_module_path}/io_f")
set(part_kernel_f_module "${part_module_path}/kernel_f")

# Dimr
set(dimr_module_path "${engines_gpl_path}/dimr/packages")
set(dimr_lib_module "${dimr_module_path}/dimr_lib")
set(dimr_module "${dimr_module_path}/dimr")

# Tests
set(dflowfm_kernel_test_module test/engines_gpl/dflowfm/packages/dflowfm_kernel)

# Install
set(install_dflowfm_module cmake/install_fm)
set(install_dimr_module cmake/install_dimr)
