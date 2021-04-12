include_directories(${mpich2_path}/)

set(library_name deltares_common_mpi)
get_fortran_source_files(src source_files)
add_library(${library_name} ${source_files})

# Set dependencies
set(oss_dependencies deltares_common)
oss_include_libraries(deltares_common_mpi oss_dependencies)

# Set preprocessor definitions
target_compile_definitions(${library_name} PRIVATE HAVE_MPI)

# Set additional compilation properties
set(source_path src)
set_source_files_properties(${source_path}/dfbroadc.f90
                            ${source_path}/dfreduce.f90
                            PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")

if (WIN32)
   target_compile_options(${library_name} PRIVATE "${check_bounds_flag}")
endif(WIN32)

if (UNIX)
   target_link_libraries(${library_name} ${oss_dependencies})
   target_compile_options(${library_name}  PRIVATE ${c_compiler_flags})
endif(UNIX)

# Create the folder structure in vfproj
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files})
set_target_properties (${library_name} PROPERTIES FOLDER utils_lgpl/deltares_common)
