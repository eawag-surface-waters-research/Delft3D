# Set the directory of where the source code is located.
set(src_path src/diof90)
set(header_path include)

if (UNIX)
    #name mangling, these functions are generated by the FortranCInterface cmake package and might be system dependent
    include(FortranCInterface)
    FortranCInterface_HEADER("${CMAKE_CURRENT_SOURCE_DIR}/include/FortranCInterface.h" MACRO_NAMESPACE "FC_")
    #produce a config.h to be included
    configure_file(${CMAKE_CURRENT_SOURCE_DIR}/config.h.cmake.in ${CMAKE_CURRENT_SOURCE_DIR}/include/config.h)  
    set(configFile "${CMAKE_CURRENT_SOURCE_DIR}/include/config.h") 
    
    include_directories(${NETCDF_INCLUDEDIR}) 
    add_definitions(-DHAVE_CONFIG_H)
    add_definitions(-DD3D_PLUGIN_EXT=".so")
    add_definitions(-DHAVE_STRUCT_TIMESPEC)
endif(UNIX)

# Gather source files 
file(GLOB source ${src_path}/*.cpp)
file(GLOB headers ${header_path}/*.h)
if(WIN32)
    list(REMOVE_ITEM source ${CMAKE_CURRENT_SOURCE_DIR}/${src_path}/dio-sync-ux.cpp)
    list(REMOVE_ITEM headers ${CMAKE_CURRENT_SOURCE_DIR}/${header_path}/dio-sync-ux.h)
endif(WIN32)

# Define library
include_directories(${header_path} ${checkout_src_root}/utils_lgpl/esmfsm/include)
set(library_name delftio_shm)
add_library(${library_name} ${source}
                            ${headers})
if(UNIX)
    target_compile_options(${library_name} PRIVATE ${cpp_compiler_flags})        
endif(UNIX)

# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files}
                                                    ${headers})
set_target_properties (${library_name} PROPERTIES FOLDER utils_lgpl/delft_io)
