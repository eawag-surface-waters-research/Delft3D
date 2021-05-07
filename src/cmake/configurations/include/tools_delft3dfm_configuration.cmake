# Tools for Delft3D-FM

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
