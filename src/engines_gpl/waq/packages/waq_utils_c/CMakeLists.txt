# Set the directory which contains the source code
set(src_path src)

# Gather source files
set(source  ${src_path}/f902c.cpp
            ${src_path}/getuuid.c)

add_library(waq_utils_c ${source})

if(UNIX)
   target_compile_options(waq_utils_c  PRIVATE ${c_compiler_flags})
endif()

# Define how the files should be structured in Visual studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source} )
set_target_properties (waq_utils_c PROPERTIES FOLDER utils_gpl/wq_processes)