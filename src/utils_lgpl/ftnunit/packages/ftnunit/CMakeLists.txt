set(library_name ftnunit)
create_library(${library_name} ${visual_studio_source_group_name})

# Set additional compilation properties
target_compile_options(${library_name} PRIVATE "${check_bounds_flag}")