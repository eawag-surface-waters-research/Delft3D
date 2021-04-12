# Set the directory which contains all the source files
set(src_path "${CMAKE_CURRENT_SOURCE_DIR}/..")

# Define the fortranc library
set(fortranc_path ${src_path}/libfortranc)

add_compile_options(-fpic)
set(fortranc_sources ${fortranc_path}/fortranc.F90
                        ${fortranc_path}/iso_varying_string.F90)
                  
if (UNIX)
    configure_file(${CMAKE_CURRENT_SOURCE_DIR}/config.h.cmake.in ${fortranc_path}/config.h)  
    set(configFile ${fortranc_path}/config.h) 
endif(UNIX)

set(fortranc_library_name fortranc)
add_library(${fortranc_library_name} ${fortranc_sources})

# Add dependencies
if (UNIX)
    # the `pkg_check_modules` function is created with this call
    find_package(PkgConfig REQUIRED)

    # these calls create special `PkgConfig::<MODULE>` variables
    pkg_check_modules(PROJ REQUIRED IMPORTED_TARGET proj)

    target_link_libraries(${fortranc_library_name} PkgConfig::PROJ) # Note that this is an optional dependency. It was introduced to remain consistent with the tests.sln
endif(UNIX)
if (WIN32)
    add_dependencies(${fortranc_library_name} proj) # Note that this is an optional dependency. It was introduced to remain consistent with the tests.sln
endif(WIN32)

# Set additional compilation properties
target_compile_options(${fortranc_library_name} PRIVATE "${check_bounds_flag}")

# Create the folder structure in project
source_group("${visual_studio_source_group_name}" FILES ${fortranc_sources})

# Create the folder structure in vfproj
source_group(TREE "${CMAKE_CURRENT_SOURCE_DIR}/.." FILES ${fortranc_sources})
set_target_properties (${fortranc_library_name} PROPERTIES FOLDER third_party_open/fortrangis)


# Define the fortrangis library
set(fortrangis_path ${src_path}/libfortrangis)
set(fortrangis_sources  ${fortrangis_path}/gdal.F90
                        ${fortrangis_path}/proj.F90
                        ${fortrangis_path}/shapelib.F90)

set(fortrangis_library_name fortrangis)
add_library(${fortrangis_library_name} ${fortrangis_sources})

# Include directories that are necessary
target_include_directories(${fortrangis_library_name} PRIVATE ${src_path}/include)

# Add dependencies
add_dependencies(${fortrangis_library_name} shp) # Note that this is an optional dependency. It was introduced to remain consistent with the tests.sln
add_dependencies(${fortrangis_library_name} ${fortranc_library_name})

# Set additional compilation properties
target_compile_options(${fortrangis_library_name} PRIVATE "${check_bounds_flag}")

# Create the folder structure in vfproj
source_group(TREE "${CMAKE_CURRENT_SOURCE_DIR}/.." FILES ${fortran_sources})
set_target_properties (${fortrangis_library_name} PROPERTIES FOLDER third_party_open/fortrangis)

