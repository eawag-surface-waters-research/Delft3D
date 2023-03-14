# Note that the CMAKE_CURRENT_SOURCE_DIR has a path to THIS CMakeLists location
set(wave_root_path ${CMAKE_CURRENT_SOURCE_DIR}/../../) # Trace back to the D-Waves root

# Set version file variables
set(wave_version_path ${wave_root_path}/version)

set(fortran_version_file ${wave_root_path}/packages/manager/src/wave_version.F90)
set(rc_version_file ${wave_version_path}/wave_version.rc)
set(version_include_dir ${CMAKE_SOURCE_DIR}/../version_includes)

# Set directory which contains all source files
set(src_path src)

# Gather source files
get_fortran_source_files(${src_path} source_files)

# Define library
set(library_name wave_manager)
add_library(${library_name} ${source_files}
                            ${rc_version_file}
                            ${fortran_version_file})

set_source_files_properties(${fortran_version_file}
                            PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")

# Set dependencies
set(oss_dependencies    wave_data
                        delftio
                        deltares_common
                        ec_module
                        wave_io
                        wave_kernel) 

oss_include_libraries(${library_name} oss_dependencies)
target_link_libraries(${library_name} ${oss_dependencies})

include_directories(${mpi_include_path} ${version_include_dir})

# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files})
set_target_properties (${library_name} PROPERTIES FOLDER engines_gpl/wave)
