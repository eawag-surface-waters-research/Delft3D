# Set directory which contains all source files
set(src_path src)

# Gather source files
get_fortran_source_files(${src_path} source_files)
file(GLOB preprocessor_files ${src_path}/pldep.F90)

# Define library
set(library_name datsel_f)
add_library(${library_name} ${source_files}
                            ${preprocessor_files})

# Set additional compilation flags
set_source_files_properties(${preprocessor_files}
                            PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")

# Set dependencies
set(oss_dependencies    deltares_common
                        deltares_common_c
                        nefis) 
oss_include_libraries(${library_name} oss_dependencies)

if(UNIX)
    target_link_libraries(${library_name} ${oss_dependencies})
endif()

# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files}
                                                    ${preprocessor_files})
set_target_properties (${library_name} PROPERTIES FOLDER tools_gpl/datsel)