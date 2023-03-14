# Shared definitions
set(wave_root_path ${CMAKE_CURRENT_SOURCE_DIR}/../../) # Trace back to the D-Waves root
set(rc_version_file ${wave_version_path}/wave_version.rc)

# Set directory which contains all source files
set(src_path src)

# Wave Library definition
include(wave_dll.cmake)

# Wave Executable definition
include(wave_exe.cmake)
