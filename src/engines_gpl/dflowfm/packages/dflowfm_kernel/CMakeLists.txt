# Set version file variables
# Note that the CMAKE_CURRENT_SOURCE_DIR has a path to THIS CMakeLists location
set(svn_version_path ${CMAKE_CURRENT_SOURCE_DIR}/../../) # Trace back to the DFlowFM path
set(ini_version_file ${CMAKE_CURRENT_SOURCE_DIR}/../../res/version_number.ini)
set(fortran_version_file ${CMAKE_CURRENT_SOURCE_DIR}/src/unstruc_version.F90)

# Include directories that are required as references
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/include)

# Set the directory of where the source code is located
set(src_path src) 

# Generate the F90 version file
generate_version_files(${fortran_version_file} ${checkout_src_root} ${ini_version_file} TRUE)
set(svn_version_file ${fortran_version_file}.svn)

# Gather the source files
set(headers include/blasfm.h)

get_fortran_source_files(${src_path} source)


list(REMOVE_ITEM source ${CMAKE_CURRENT_SOURCE_DIR}/${src_path}/io_openfoam.f90
                        ${CMAKE_CURRENT_SOURCE_DIR}/${src_path}/read_forcings.f90
                        ${CMAKE_CURRENT_SOURCE_DIR}/${src_path}/solve_parms.F90
                        ${CMAKE_CURRENT_SOURCE_DIR}/${src_path}/xbeach_wave_boundary_init.f90
                        ${CMAKE_CURRENT_SOURCE_DIR}/${src_path}/solve_petsc_v34.F90)

# Define library
set(library_name dflowfm_kernel)
set(library_source_list     ${fortran_version_file}
                            ${headers}
                            ${source})
                            
add_library(${library_name}  ${svn_version_file} ${ini_version_file} ${library_source_list})

# Add dependencies
if (UNIX)
    # the `pkg_check_modules` function is created with this call
    find_package(PkgConfig REQUIRED)

    # these calls create special `PkgConfig::<MODULE>` variables
    pkg_check_modules(NETCDF REQUIRED IMPORTED_TARGET netcdf)
    pkg_check_modules(NETCDF_FTN REQUIRED IMPORTED_TARGET netcdf-fortran)
    pkg_check_modules(GDAL REQUIRED IMPORTED_TARGET gdal)
    pkg_check_modules(PETSC REQUIRED IMPORTED_TARGET PETSc)
    pkg_check_modules(PROJ REQUIRED IMPORTED_TARGET proj)



    set(oss_dependencies    deltares_common
                            deltares_common_c
                            deltares_common_mpi
                            dhydrology_kernel
                            ec_module
                            flow1d
                            flow1d_core
                            flow1d_io
                            gridgeom
                            io_netcdf
                            kdtree_wrapper
                            kdtree2
                            fortranc
                            fortrangis
                            libsigwatch
                            md5digest
                            md5
                            metisoptions
                            morphology_data
                            morphology_io
                            morphology_kernel
                            morphology_plugins_c
                            polypack
                            shp
                            trachytopes_io
                            trachytopes_kernel
                            triangle_c
                            waq_process
                            waq_utils_c
                            waq_utils_f
                            wq_processes
                            interacter_stub)

    oss_include_libraries(${library_name} oss_dependencies)
    target_link_libraries(${library_name} ${oss_dependencies}
         PkgConfig::GDAL
         PkgConfig::NETCDF
         PkgConfig::NETCDF_FTN
         PkgConfig::PETSC
         PkgConfig::PROJ)


    include_directories("${CMAKE_CURRENT_SOURCE_DIR}/include"
                        ${mpich2_path}
                        ${tecplot_path})
endif(UNIX)

if (WIN32)
    set(oss_dependencies    deltares_common
                            deltares_common_c
                            deltares_common_mpi
                            dhydrology_kernel
                            ec_module
                            flow1d
                            flow1d_core
                            flow1d_io
                            gridgeom
                            io_netcdf
                            kdtree_wrapper
                            kdtree2
                            fortrangis
                            libsigwatch
                            md5digest
                            md5
                            metis
                            metisoptions
                            morphology_data
                            morphology_io
                            morphology_kernel
                            morphology_plugins_c
                            netcdf4
                            netcdff
                            petsc
                            polypack
                            shp
                            trachytopes_io
                            trachytopes_kernel
                            triangle_c
                            waq_process
                            waq_utils_c
                            waq_utils_f
                            wq_processes
                            interacter_stub)

    oss_include_libraries(${library_name} oss_dependencies)
    target_link_libraries(${library_name} ${oss_dependencies})

    include_directories("${CMAKE_CURRENT_SOURCE_DIR}/include"
                        ${mpich2_path}
                        ${tecplot_path}
                        ${petsc_path})
endif(WIN32)


# Define prebuild event
prebuild_version_number(${library_name} ${fortran_version_file} 
                                        ${checkout_src_root} 
                                        ${ini_version_file} 
                                        ON)

# Define additional compilation flags

set_source_files_properties(${fortran_version_file}
                            ${CMAKE_CURRENT_SOURCE_DIR}/${src_path}/dfm_signals.F90
                            ${CMAKE_CURRENT_SOURCE_DIR}/${src_path}/filter.F90
                            ${CMAKE_CURRENT_SOURCE_DIR}/${src_path}/gui.F90
                            ${CMAKE_CURRENT_SOURCE_DIR}/${src_path}/net.F90
                            ${CMAKE_CURRENT_SOURCE_DIR}/${src_path}/part.F90
                            ${CMAKE_CURRENT_SOURCE_DIR}/${src_path}/partition.F90
                            ${CMAKE_CURRENT_SOURCE_DIR}/${src_path}/rest.F90
                            ${CMAKE_CURRENT_SOURCE_DIR}/${src_path}/saadf.F
                            ${CMAKE_CURRENT_SOURCE_DIR}/${src_path}/saadf90.F90
                            ${CMAKE_CURRENT_SOURCE_DIR}/${src_path}/solve_guus.F90
                            ${CMAKE_CURRENT_SOURCE_DIR}/${src_path}/solve_petsc.F90
                            ${CMAKE_CURRENT_SOURCE_DIR}/${src_path}/Tecplot.F90
                            ${CMAKE_CURRENT_SOURCE_DIR}/${src_path}/unstruc.F90
                            ${CMAKE_CURRENT_SOURCE_DIR}/${src_path}/unstruc_api.F90
                            ${CMAKE_CURRENT_SOURCE_DIR}/${src_path}/unstruc_display.F90
                            ${CMAKE_CURRENT_SOURCE_DIR}/${src_path}/unstruc_opengl.F90
                            ${CMAKE_CURRENT_SOURCE_DIR}/${src_path}/unstruc_shapefile.F90
                            ${CMAKE_CURRENT_SOURCE_DIR}/${src_path}/unstruc_version.F90
                            ${CMAKE_CURRENT_SOURCE_DIR}/${src_path}/wrwaq.F90
                            ${CMAKE_CURRENT_SOURCE_DIR}/${src_path}/xbeach_filefunctions.F90
                            PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")

# Define preprocessor definitions 
set_target_properties(${library_name} PROPERTIES COMPILE_FLAGS ${openmp_flag})
if (WIN32) 
    target_compile_definitions(${library_name} PRIVATE "WIN32;HAVE_TECPLOT;HAVE_OPENGL;HAVE_METIS;HAVE_POLYPACK;HAVE_MPI;HAVE_PROJ;HAVE_SHAPELIB;HAVE_GDAL;HAVE_PETSC")
endif(WIN32)

if (UNIX)
    target_link_libraries(${library_name} ${oss_dependencies})
    target_compile_definitions(${library_name} PRIVATE "HAVE_METIS;HAVE_POLYPACK;HAVE_MPI;HAVE_PROJ;HAVE_SHAPELIB;HAVE_GDAL;HAVE_PETSC")
endif(UNIX)

# Create the folder structure in vfproj
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${library_source_list})
set_target_properties (${library_name} PROPERTIES FOLDER dflowfm)

