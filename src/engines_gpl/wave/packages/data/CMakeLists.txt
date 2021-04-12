# Set directory which contains all source files
set(src_path src)

# Gather source files
get_fortran_source_files(${src_path} source_files)

# Define library
set(library_name data)
add_library(${library_name} ${source_files})

# Set dependencies
set(oss_dependencies    delftio
                        deltares_common
                        ec_module
                        gridgeom
                        io_netcdf
                        nefis
                        netcdf4
                        netcdff) 
oss_include_libraries(${library_name} oss_dependencies)

if(UNIX)
    target_link_libraries(${library_name} ${oss_dependencies})
    target_compile_options(${library_name}  PRIVATE ${c_compiler_flags})
endif()

# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files})
set_target_properties (${library_name} PROPERTIES FOLDER engines_gpl/wave)