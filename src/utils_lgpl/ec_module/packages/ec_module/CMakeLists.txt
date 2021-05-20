set(source_path src)

file(GLOB meteo_sources ${source_path}/meteo/*.f90)
file(GLOB sources ${source_path}/*.f90 ${source_path}/*.F90)

# Remove files that should not be compiled
list(REMOVE_ITEM sources "${CMAKE_CURRENT_SOURCE_DIR}/${source_path}/ec_stringbuffer.f90"
                         "${CMAKE_CURRENT_SOURCE_DIR}/${source_path}/ec_module_api.f90"
                         "${CMAKE_CURRENT_SOURCE_DIR}/${source_path}/ec_module_version.F90")

add_compile_options("${automatic_local_variable_storage_flag}")
set(library_name ec_module)
add_library(${library_name} ${meteo_sources}
                            ${sources})

# Add dependencies
if (UNIX)
    set(oss_dependencies deltares_common
                         kdtree2
                         kdtree_wrapper
                         io_netcdf)
endif(UNIX)
if (WIN32)
    set(oss_dependencies deltares_common
                         kdtree2
                         kdtree_wrapper
                         io_netcdf
                         netcdf4
                         netcdff)
endif(WIN32)
oss_include_libraries(${library_name} oss_dependencies)

if(UNIX)
    target_link_libraries(${library_name} ${oss_dependencies})
endif(UNIX)

# Set additional compilation properties, specific for Debug mode
target_compile_options(${library_name} PRIVATE "$<$<CONFIG:Debug>:${check_bounds_flag}>")

set_source_files_properties(${source_path}/ec_parameters.F90
                            PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")

# Create the folder structure in vfproj
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${sources} ${meteo_sources})
set_target_properties (${library_name} PROPERTIES FOLDER utils_lgpl/ec_module)

