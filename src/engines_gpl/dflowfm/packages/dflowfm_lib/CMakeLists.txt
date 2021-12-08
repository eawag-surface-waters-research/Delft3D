# Set directories
# Set the directory of where the source code is located.
set(src_path src) 
set(include_path include)

# Set the DFlow FM component path 
set(dflowfm_path "${CMAKE_CURRENT_SOURCE_DIR}/../../")

# Set the directory referring to the ref folder
set(res_path ${dflowfm_path}/res)

# Set version file variables
set(ini_version_file ${res_path}/version_number.ini)
set(rc_version_file ${res_path}/version_number_dll.rc)

# Generate the version files
generate_version_files(${rc_version_file} ${checkout_src_root} ${ini_version_file})
set(svn_version_file ${rc_version_file}.svn)

if(UNIX)
   include_directories("${CMAKE_CURRENT_SOURCE_DIR}/${include_path}")
endif(UNIX)

# Gather the source files
set(source ${src_path}/unstruc_bmi.F90)
file(GLOB includes ${include_path}/*.inc)

# Define the executable
set(library_name dflowfm_dll)
add_library(${library_name} SHARED  ${source}
                                    ${includes}
                                    ${rc_version_file} 
                                    ${svn_version_file} 
                                    ${ini_version_file})

# Define prebuild event
prebuild_version_number(${library_name} ${rc_version_file} 
                                        ${checkout_src_root}
                                        ${ini_version_file})

# Add dependencies
if (UNIX)
    # the `pkg_check_modules` function is created with this call
    find_package(PkgConfig REQUIRED)

    # these calls create special `PkgConfig::<MODULE>` variables
    pkg_check_modules(NETCDF REQUIRED IMPORTED_TARGET netcdf)
    pkg_check_modules(NETCDF_FTN REQUIRED IMPORTED_TARGET netcdf-fortran)
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
                            interacter_stub
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
                            nefis
                            polypack
                            shp
                            trachytopes_io
                            trachytopes_kernel
                            triangle_c
                            waq_process
                            waq_utils_c
                            waq_utils_f
                            wq_processes
                            dflowfm_kernel)

    oss_include_libraries(${library_name} oss_dependencies)

    target_link_libraries(${library_name}
         ${oss_dependencies}
         PkgConfig::NETCDF
         PkgConfig::NETCDF_FTN
         PkgConfig::PETSC
         PkgConfig::PROJ)
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
                            interacter_stub
                            kdtree_wrapper
                            kdtree2
                            fortranc
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
                            nefis
                            netcdf4
                            netcdff
                            proj
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
                            dflowfm_kernel)

    oss_include_libraries(${library_name} oss_dependencies)

    target_link_libraries(${library_name} ${oss_dependencies})
endif(WIN32)


if (WIN32)
    set_target_properties(${library_name} PROPERTIES LINK_FLAGS ${openmp_flag})
endif(WIN32)

if(UNIX)
    message(STATUS "Setting target_compile_definitions in Unix")    
    target_compile_definitions(${library_name} PRIVATE "HAVE_DISPLAY=0;HAVE_METIS;HAVE_POLYPACK;HAVE_MPI;HAVE_PROJ;HAVE_SHAPELIB;HAVE_GDAL;HAVE_PETSC")
endif(UNIX)

if(WIN32)
    message(STATUS "Setting target_compile_definitions in windows")
    target_compile_definitions(${library_name} PRIVATE "WIN32;HAVE_DISPLAY=0;HAVE_METIS;HAVE_POLYPACK;HAVE_MPI;HAVE_PROJ;HAVE_SHAPELIB;HAVE_GDAL;HAVE_PETSC;HAVE_OPENGL")  
endif(WIN32)

# Set additional compilation properties, specific for Debug mode
target_compile_options(${library_name} PRIVATE "$<$<CONFIG:Debug>:${check_bounds_flag}>")
target_compile_options(${library_name} PRIVATE "$<$<CONFIG:Debug>:${check_pointer}>")
    
# Define additional compilation flags
set_source_files_properties(${source}
                            PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")


if (WIN32)

    target_include_directories(${library_name} PRIVATE  ${include_path}
                                                        ${gisinternals_path}
                                                        ${mpi_include_path}
                                                        ${petsc_path}
                                                        ${tecplot_path})

    # Set linker properties
    message(STATUS "Setting linker properties in windows")
    target_link_directories(${library_name}
                            PRIVATE
                            ${gisinternals_path}
                            "${mpi_library_path}"
                            "${checkout_src_root}/third_party_open/netcdf/netCDF 4.6.1/lib"
                            "${checkout_src_root}/third_party_open/petsc/petsc-3.10.2/lib/x64/Release"
                            "${checkout_src_root}/third_party_open/pthreads/bin/x64"
                            "${checkout_src_root}/third_party_open/Tecplot/lib/x64")


    target_link_libraries(${library_name}   
                            "comctl32.lib"
                            "wsock32.lib"
                            "netapi32.lib"
                            "kernel32.lib"
                            "user32.lib" 
                            "Shlwapi.lib"
                            "Gdi32.lib"
                            "Winspool.lib"
                            "netcdf.lib"
                            "pthreadVC2.lib"
                            "${mpi_fortran_library}"
                            "libpetsc.lib"
                            "gdal_i.lib"
                            "tecio.lib")


    # Set linker options
    message(STATUS "Setting target_link_options in windows")
    target_link_options(${library_name} PRIVATE ${nologo_flag})
endif(WIN32)

# Change the name of the target library to dflowfm.dll
set_target_properties (${library_name} PROPERTIES OUTPUT_NAME  dflowfm)

# Create the folder structure in vfproj
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${source} ${includes})
set_target_properties (${library_name} PROPERTIES FOLDER engines_gpl/dflowfm)

# post-build
set(install_dir ${CMAKE_BINARY_DIR})
set(build_dir ${CMAKE_BINARY_DIR})

post_build_target (${library_name}
                   ${install_dir} 
                   ${build_dir} 
                   ${checkout_src_root} 
                   ${library_name})

install(TARGETS ${library_name} DESTINATION lib)

