# Set directory which contains all source files
set(src_path src)

# Note that the CMAKE_CURRENT_SOURCE_DIR has a path to THIS CMakeLists location
set(d_hydro_lib_include_path ${CMAKE_CURRENT_SOURCE_DIR}/../../include)

# Set public include path to make it accessible for other components
set(public_include_path ${d_hydro_lib_include_path})

# Set version file variables
set(version_path ${CMAKE_CURRENT_SOURCE_DIR}/../) # Path to determine the version number from
set(d_hydro_lib_version_path ${CMAKE_CURRENT_SOURCE_DIR}/include)

set(ini_version_file ${d_hydro_lib_version_path}/version_number.ini)
set(h_version_file ${d_hydro_lib_version_path}/d_hydro_lib_version.h)

# Generate version files
generate_version_files(${h_version_file} 
                       ${ini_version_file})

# Set include directories
include_directories(${d_hydro_lib_include_path}
                    ${d_hydro_lib_version_path}
                    ${pthreads_path}
                    ${expat_path})

# Gather source files
file(GLOB header_files ${d_hydro_lib_include_path}/*.h)
file(GLOB source_files ${src_path}/*.cpp)

# Define library
set(library_name d_hydro_lib)
add_library(${library_name} ${header_files}
                            ${h_version_file}
                            ${source_files})

# Define prebuild events
prebuild_version_number(${library_name} ${h_version_file} 
                                        ${version_path}
                                        ${ini_version_file})

# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files})
source_group(Include FILES  ${header_files}
                            ${h_version_file})
set_target_properties (${library_name} PROPERTIES FOLDER utils_lgpl/d_hydro_lib)