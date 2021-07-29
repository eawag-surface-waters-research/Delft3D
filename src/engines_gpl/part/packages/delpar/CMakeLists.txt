# Define include directories
# Note that the CMAKE_CURRENT_SOURCE_DIR has a path to THIS CMakeLists location
set(part_root_path ${CMAKE_CURRENT_SOURCE_DIR}/../../) # Trace back to the WAQ path

# Set version file variables
set(part_version_path ${part_root_path}/version)

set(ini_version_file ${part_version_path}/version_number.ini)
set(fortran_version_file ${part_root_path}/packages/utils_f/src/part_version.F90)

# Set icon file
set(icon_file ${part_version_path}/part_icon.rc)

generate_version_files(${fortran_version_file} 
                       ${checkout_src_root}
                       ${ini_version_file} 
                       ON)

# Set directory which contains all source files
set(src_path src)

# Gather source files
get_fortran_source_files(${src_path} source_files)

# Define executable
set(executable_name delpar)
add_executable(${executable_name} ${source_files}
                                  ${icon_file}
                                  ${fortran_version_file})

# Define prebuild events
prebuild_version_number(${executable_name} ${fortran_version_file} 
                                        ${checkout_src_root}
                                        ${ini_version_file}
                                        ON)

set_source_files_properties(${fortran_version_file} ${fortran_version_file}
                            PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")

# Add dependencies
    set(oss_dependencies    deltares_common
                            deltares_common_c
                            esmfsm 
                            nefis
                            part_data_f
                            part_io_f
                            part_kernel_f
                            part_utils_f)
    target_link_libraries(${executable_name} ${oss_dependencies})

oss_include_libraries(${executable_name} oss_dependencies)


# Set additional compilation properties
target_compile_options(${executable_name} PRIVATE "${extend_source132_flag}")
target_compile_options(${executable_name} PRIVATE "$<$<CONFIG:Debug>:${check_bounds_flag}>")
target_compile_options(${executable_name} PRIVATE "$<$<CONFIG:Debug>:${check_pointer}>")


if (WIN32)
    # Set linker properties
    message(STATUS "Setting linker properties in windows")
    target_link_directories(${executable_name}
                            PRIVATE
                            "${checkout_src_root}/third_party_open/pthreads/bin/x64")

    target_link_libraries(${executable_name}   
                            "pthreadVC2.lib")

    # Set linker options
    message(STATUS "Setting target_link_options in windows")
    target_link_options(${executable_name} PRIVATE ${nologo_flag})
endif(WIN32)

# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files})
source_group(Resources FILES    ${icon_file} 
                                ${rc_version_file})
set_target_properties(${executable_name} PROPERTIES COMPILE_FLAGS ${openmp_flag})
set_target_properties(${executable_name} PROPERTIES FOLDER engines_gpl/part)
if (WIN32)
    set_target_properties(${executable_name} PROPERTIES LINK_FLAGS "/LARGEADDRESSAWARE /STACK:20000000")
endif(WIN32)

# Set post-build step
set(install_dir ${CMAKE_BINARY_DIR})
set(build_dir ${CMAKE_BINARY_DIR})

post_build_target (${executable_name}
                   ${install_dir} 
                   ${build_dir} 
                   ${checkout_src_root} 
                   ${executable_name})

install(TARGETS ${executable_name} RUNTIME  DESTINATION bin)

