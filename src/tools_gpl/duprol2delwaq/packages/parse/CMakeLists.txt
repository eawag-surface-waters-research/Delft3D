# Set directory which contains all source files
set(src_path src)

# Gather source files
file(GLOB source_files  ${src_path}/*.c)

# Define executable name
set(executable_name duprol2delwaq)
add_executable(${executable_name} ${source_files})

if(UNIX)
    target_compile_options(${executable_name} PRIVATE ${c_compiler_flags})
endif()

# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source_files})
set_target_properties (${executable_name} PROPERTIES FOLDER tools_gpl/duprol2delwaq)