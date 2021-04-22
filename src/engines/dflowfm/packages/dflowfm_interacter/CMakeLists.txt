# Interacter is only available on Windows
if(WIN32)

    # Set directories
    # Set the directory of where the source code is located.
    set(src_path src) 

    # Set the DFlow FM component path 
    set(dflowfm_path "${CMAKE_CURRENT_SOURCE_DIR}/../../../../engines_gpl/dflowfm/")

    # Set the directory referring to the res folder
    set(res_path ${dflowfm_path}/res)

    # Set version file variables
    set(ini_version_file ${res_path}/version_number.ini)
    set(rc_version_file ${res_path}/version_number-cli.rc)

    # Generate the version files
    generate_version_files(${rc_version_file} ${checkout_src_root} ${ini_version_file} TRUE)
    set(svn_version_file ${rc_version_file}.svn)

    # Gather the source files
    file(GLOB source    ${dflowfm_path}/packages/dflowfm-cli_exe/src/net_main.F90)

    set(icon_file ${res_path}/dflowfm_icon.rc)

    # Define the executable
    set(executable_name dflowfm_interacter)
    add_executable(${executable_name}   ${source}
                                        ${icon_file}
                                        ${rc_version_file} 
                                        ${svn_version_file} 
                                        ${ini_version_file})

    # Define prebuild event
    prebuild_version_number(${executable_name}  ${rc_version_file} 
                                                ${checkout_src_root}
                                                ${ini_version_file} 
                                                ON)

    # Add dependencies
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

    oss_include_libraries(${executable_name} oss_dependencies)

    target_link_libraries(${executable_name} ${oss_dependencies})

    set_target_properties(${executable_name} PROPERTIES COMPILE_FLAGS ${openmp_flag})
    set(build_config $<CONFIG>) 
    if(${build_config} STREQUAL "Debug")
        target_compile_options(${executable_name} PRIVATE "${check_bounds_flag} ${check_pointer}" )
    endif()   
    
    set_target_properties (${executable_name} PROPERTIES RUNTIME_OUTPUT_DIRECTORY_DEBUG "${CMAKE_BINARY_DIR}/x64/Debug")

    # Define additional compilation flags
    set_source_files_properties(${dflowfm_path}/packages/dflowfm-cli_exe/src/net_main.F90
                                PROPERTIES COMPILE_OPTIONS "${file_preprocessor_flag}")



    target_link_libraries(${executable_name} netcdf)


    include_directories(${gisinternals_path}
                        ${mpich2_path}
                        ${petsc_path}
                        ${tecplot_path})

    # Set linker properties
    message(STATUS "Setting linker properties in windows")
    target_link_directories(${executable_name}
                            PRIVATE
                            ${gisinternals_path}
                            "${checkout_src_root}/third_party_open/mpich2/x64/lib"
                            "${checkout_src_root}/third_party_open/netcdf/netCDF 4.6.1/lib"
                            "${checkout_src_root}/third_party_open/petsc/petsc-3.10.2/lib/x64/Release"
                            "${checkout_src_root}/third_party_open/pthreads/bin/x64"
                            "${checkout_src_root}/third_party_open/Tecplot/lib/x64"
                            "${checkout_src_root}/third_party/interacter/bin/win32/x64")

    target_link_libraries(${executable_name}
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
                          "fmpich2.lib"
                          "libpetsc.lib"
                          "gdal_i.lib"
                          "tecio.lib"
                          "intmwi64.lib")
                      
    message(STATUS "Setting target_compile_definitions in windows")
    target_compile_definitions(${executable_name} PRIVATE "HAVE_DISPLAY=1;WIN32;HAVE_OPENGL;HAVE_METIS;HAVE_POLYPACK;HAVE_MPI;HAVE_PROJ;HAVE_SHAPELIB;HAVE_GDAL")

    # Set linker options
    message(STATUS "Setting target_link_options in windows")
    target_link_options(${executable_name} PRIVATE ${nologo_flag})

    # Create the folder structure in Visual Studio
    source_group("Version Files" FILES  ${ini_version_file} ${rc_version_file} ${svn_version_file})
    source_group("Resources" FILES  ${icon_file})

    # Create the folder structure in vfproj
    source_group(TREE ${dflowfm_path}/packages FILES ${source})

    # Set the folder for storing the target. 
    set_target_properties (${executable_name} PROPERTIES FOLDER dflowfm_interacter)

    # post-build
    set(install_dir ${CMAKE_BINARY_DIR})
    set(build_dir ${CMAKE_BINARY_DIR})
    set(build_project dflowfm_interacter)

    post_build_target( ${executable_name}
                       ${install_dir} 
                       ${build_dir} 
                       ${checkout_src_root} 
                       ${build_project} )

    install(TARGETS ${executable_name} RUNTIME  DESTINATION bin)

endif(WIN32)