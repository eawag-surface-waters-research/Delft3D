# Include directories that are required as header references
set(include_path include)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/${include_path})

# Set directory which contains all source files
set(src_path src/diof90)

# Gather source files 
file(GLOB source_files    ${src_path}/*.cpp)
file(GLOB headers         include/*.h)

list(REMOVE_ITEM source_files ${CMAKE_CURRENT_SOURCE_DIR}/${src_path}/dio-sync-ux.cpp)
list(REMOVE_ITEM headers ${CMAKE_CURRENT_SOURCE_DIR}/${include_path}/dio-sync-ux.h)

# Define library
set(library_name delftio_shm)
add_library(${library_name} ${source_files}
                            ${headers})

# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files}
                                                    ${headers})
set_target_properties (${library_name} PROPERTIES FOLDER utils_lgpl/delft_io)
