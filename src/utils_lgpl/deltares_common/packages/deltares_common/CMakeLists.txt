# Set include path 
set(deltares_common_include_path ${CMAKE_CURRENT_SOURCE_DIR}/include)

# Set version file variables
# Note that the CMAKE_CURRENT_SOURCE_DIR has a path to THIS CMakeLists location
set(ini_version_file "${deltares_common_include_path}/version_number.ini")
set(fortran_version_file "${CMAKE_CURRENT_SOURCE_DIR}/src/deltares_common_version.F90")

# Set include path to have it accessible for other components
set(public_include_path ${deltares_common_include_path})

# Set the directory of where the source code is located
set(src_path src) 

# Generate the F90 version file
generate_version_files(${fortran_version_file} ${checkout_src_root} ${ini_version_file})
set(svn_version_file ${fortran_version_file}.svn)

# Set the source files
file(GLOB rd_token_source   ${src_path}/rd_token/*.f90
                            ${src_path}/rd_token/*.f)

file(GLOB source    ${src_path}/*.f90
                    ${src_path}/*.f
                    ${src_path}/*.F90)

set(library_name deltares_common)
add_library(${library_name} ${source} 
                            ${rd_token_source} 
                            ${fortran_version_file}
                            ${ini_version_file}
                            ${svn_version_file})

if(UNIX)
   set(oss_dependencies deltares_common_c)
   oss_include_libraries(${library_name} oss_dependencies)
   target_link_libraries(${library_name} ${oss_dependencies})
   target_compile_options(${library_name}  PRIVATE ${c_compiler_flags})
endif(UNIX)

# Set additional compilation properties
target_compile_options(${library_name} PRIVATE "${check_bounds_flag}")

set_source_files_properties(${fortran_version_file}
                            ${src_path}/system_utils.f90
                            PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")


# Prebuild events
prebuild_version_number(deltares_common ${fortran_version_file} 
                                        ${checkout_src_root} 
                                        ${ini_version_file})
           
           
# Create the folder structure in vfproj
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source}  
                                                    ${rd_token_source} 
                                                    ${fortran_version_file}  
                                                    ${ini_version_file} 
                                                    ${svn_version_file})
                                                    
 set_target_properties (${library_name} PROPERTIES FOLDER utils_lgpl/deltares_common)