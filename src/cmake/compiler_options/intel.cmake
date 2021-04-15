# Set Intel compiler specific flags:
enable_language (Fortran)

if (WIN32)
    message(STATUS "Setting global Intel Fortran compiler flags in Windows")

    # Set global Fortran compiler flags that apply for each Fortran project
    set(nologo_flag /nologo)
    set(compiler_flags "/W1 ${nologo_flag} /libs:dll /threads")
    set(debug_flags "/check:uninit /check:stack /traceback")

    # Set optional flags:
    message(STATUS "Setting optional Intel Fortran compiler flags in Windows")
    set(file_preprocessor_flag /fpp)
    set(automatic_local_variable_storage_flag /Qauto)
    set(extend_source132_flag /extend_source:132)
    set(heap_arrays_one_flag /heap-arrays:1)
    set(heap_arrays_100_flag /heap-arrays:100)
    set(real_size_64_flag /real_size:64)

    set(check_bounds_flag /check:bounds)
    set(linker_debug_flag /debug)
    set(check_pointer /check:pointer)
    set(openmp_flag /Qopenmp)
    set(generate_reentrancy_threaded_flag /reentrancy:threaded)
endif(WIN32)

if (UNIX)
    # Set optional flags:
    message(STATUS "Setting optional Fortran compiler flags in Unix")
    # On Linux preprocessing is on by default, but the flag is inserted for
    # at least one C file as well (netCDF). Use a neutral flag to avoid problems
    set(CMAKE_CXX_FLAGS_RELEASE  "-O2")
    set(CMAKE_C_FLAGS_RELEASE    "-O2")
    set(CMAKE_CXX_FLAGS_DEBUG    "-O0")
    set(CMAKE_C_FLAGS_DEBUG      "-O0")
    set (CMAKE_Fortran_FLAGS_RELEASE "-O2")
    set (CMAKE_Fortran_FLAGS_DEBUG   "-O0")

    set(c_compiler_flags -fPIC)
    set(cpp_compiler_flags "-std=c++11")
    set(file_preprocessor_flag -v)
    set(extend_source132_flag -extend_source 132)
    set(real_size_64_flag -r8)

    set(file_preprocessor_flag -fpp)
    set(check_bounds_flag -check bounds)
    set(openmp_flag -qopenmp)
endif(UNIX)

