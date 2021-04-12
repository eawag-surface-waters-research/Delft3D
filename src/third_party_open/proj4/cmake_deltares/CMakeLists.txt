# Set directories
set(source_path "${CMAKE_CURRENT_SOURCE_DIR}/../src")

# Gather source files
file(GLOB headers ${source_path}/*.h)
list(REMOVE_ITEM headers    "${source_path}/geod_interface.h"
                            "${source_path}/nad_list.h"
                            "${source_path}/org_proj4_PJ.h"
                            "${source_path}/org_proj4_Projections.h")

file(GLOB source ${source_path}/*.c)
list(REMOVE_ITEM source "${source_path}/hypot.c"
                        "${source_path}/nad2bin.c"
                        "${source_path}/PJ_minimal.c"
                        "${source_path}/PJ_misrsom.c"
                        "${source_path}/cs2cs.c"
                        "${source_path}/gen_cheb.c"
                        "${source_path}/geod.c"
                        "${source_path}/geod_interface.c"
                        "${source_path}/geod_set.c"
                        "${source_path}/geodtest.c"
                        "${source_path}/jniproj.c"
                        "${source_path}/multistresstest.c"
                        "${source_path}/p_series.c"
                        "${source_path}/proj.c"
                        "${source_path}/test228.c")

# Define library
set(library_name proj)
add_library(${library_name} ${headers}
                            ${source})

# Add additional includes
target_include_directories(${library_name} PRIVATE ${source_path})

# Set preprocessor definitions
if (WIN32)
    target_compile_definitions(${library_name} PRIVATE MUTEX_win32;PROJ_LIB="C:/OSGeo4W/share")
endif(WIN32)

# Create the folder structure in Visual Studio
source_group(TREE "${CMAKE_CURRENT_SOURCE_DIR}/.." FILES ${headers} ${source} )
set_target_properties (${library_name} PROPERTIES FOLDER third_party_open/fortrangis)