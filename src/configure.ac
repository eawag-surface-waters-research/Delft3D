#-------------------------------------------------------------------------------
#   d_hydro top-level automake configutation
#   configure.ac (see also common.am)
#
#   Fedor.Baart@Deltares.NL
#   Adri.MouritsDeltares.NL
#   Irv.ElshoffDeltares.NL
#   31 may 11
#-------------------------------------------------------------------------------


AC_PREREQ(2.59)
AC_INIT(Delft3D, 0.0, delft3d.support@deltares.nl)
# Store generated files in config dir
AC_CONFIG_AUX_DIR(config)
#  Define a unique file to check if we're in the right directory
AC_CONFIG_SRCDIR([engines_gpl/flow2d3d/packages/kernel/src/compute/adi.f90])
AC_CONFIG_HEADER([config.h])


# Initialize automake
AM_INIT_AUTOMAKE

# C, C++ and C preprocessor
AC_PROG_CXX
AC_PROG_CC
AC_PROG_CPP

# These are required for java
#AC_PROG_JAVA
#AC_PROG_JAVA_CC
#AC_PROG_JAVAC
# This is needed when Sun Java is unavailable
#AM_PROG_GCJ

# Other programs
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET
AC_PROG_RANLIB
AC_PROG_LEX
AC_PROG_YACC


## This is copied from the libtool examples

#
##
###
#### Question from Irv: Which version?  Will it last?

## -------------------------- ##
## Fortran90 compiler checks. ##
## -------------------------- ##
AC_LANG_PUSH([Fortran])
# http://www.astro.gla.ac.uk/users/norman/star/ssn78/ssn78.htx/N-x1a1b12.html
AC_PROG_FC([ifort gfortran g95])
# Check the flags needed to link fc programs with ld (i.e. cc)
AC_FC_LIBRARY_LDFLAGS
# Check for openmp for fortran
AX_OPENMP
# Prepend the found flags
FCFLAGS="$FCFLAGS $OPENMP_FCFLAGS"

# Check for MPI (sets MPIFC, MPILIBS, HAVE_MPI)
ACX_MPI

# Check for underscoring of external names
AC_FC_WRAPPERS
# We need to use .f90 and not .f to enable Automake FC support
# Some Intel ifc/ifort do not understand .f95.  :-/
AC_FC_SRCEXT([f90])
# Yes, we want free form Fortran!
AC_FC_FREEFORM
# Set unlimited line length (don't create lines longer than 132, because it breaks on some compilers,
# but because ifort and gfortran allow it let's not raise errors by default)
AC_FC_LINE_LENGTH([unlimited])
AC_LANG_POP

## -------------------------- ##
## Fortran77 compiler checks. ##
## -------------------------- ##
AC_LANG_PUSH([Fortran 77])
AC_PROG_F77([ifort gfortran g77])
AC_F77_LIBRARY_LDFLAGS
AX_OPENMP
FFLAGS="$FFLAGS $OPENMP_FFLAGS"
AC_LANG_POP

# Fortran compiler defaults (this could be a separate m4 file)
AC_MSG_CHECKING([whether using Fortran preset flags])
AC_ARG_ENABLE(fortran-flags-preset,
AS_HELP_STRING([--enable-fortran-flags-preset],
[Enable Fortran compiler flags preset @<:@default yes@:>@]),[],[enableval='yes'])
# Set compiler specific options here: 
if test "$enableval" = yes ; then
        AC_MSG_RESULT([yes])
	case "$F77" in
                *gfortran*) dnl GNU Fortran 95 compiler
		dnl no custom flags
                ;;
                *ifort*) dnl Intel 8.x Fortran compiler
                        FFLAGS="$FFLAGS -recursive -reentrancy threaded -traceback"
                ;;
        esac
	case "$FC" in
	     *gfortran*) dnl GNU Fortran 95 compiler
	     dnl no custom flags
	     ;;
	     *ifort*) dnl Intel 8.x Fortran compiler
		      FCFLAGS="$FCFLAGS -recursive -reentrancy threaded -traceback"
             ;;
	esac
else
        AC_MSG_RESULT([no])
fi

			

   


# As of the writing of this demo, GNU Autoconf's AC_OBJEXT and
# AC_EXEEXT macros only works for C compilers!
# Libtool's setup macro calls AC_OBJEXT and AC_EXEEXT without setting
# the test language to C.  We do it before any libtool setup macros are
# called so that the proper values are cached beforehand.  We also do
# it before any linker flags (LDFLAGS) are set so that fortran specific
# ones don't break the tests.
AC_LANG_PUSH([C])
AC_OBJEXT
AC_LANG_POP


# Executables will get an .exe extension 
AC_EXEEXT([.exe])


## ----------------------- ##
## Libtool initialization. ##
## ----------------------- ##
# Libtool is disabled (static linking issues)
#LT_INIT -> new in 2.0
AC_LIBLTDL_CONVENIENCE
AC_PROG_LIBTOOL
AC_SUBST([LIBTOOL_DEPS])
AC_CONFIG_SUBDIRS(libltdl)
AC_SUBST(LIBLTDL)
AC_SUBST(INCLTDL)


# Define options
# Use double support
AC_MSG_CHECKING([whether to disable double support])
AC_ARG_ENABLE(double,
[  --disable-double           Disable double support],
   [ AC_MSG_RESULT([yes]) ],
   [ AC_MSG_RESULT([no]); AC_DEFINE(WITH_DOUBLE, 1, [Use double precision]) ]
)


# Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS([arpa/inet.h fcntl.h netdb.h netinet/in.h stdlib.h string.h sys/socket.h sys/time.h unistd.h malloc.h])

# Check for jni.h (for later)
#AC_JNI_INCLUDE_DIR

# Check for C++ standard library
AC_CHECK_LIB([stdc++], [main],, [AC_MSG_ERROR([Compiler can't find libstdc++])])
AC_CHECK_TYPES([struct shm_info])

# Check for expat
AC_CHECK_HEADER(expat.h,, [AC_MSG_ERROR([Compiler can't find <expat.h>])])
AC_CHECK_LIB(expat, XML_Parse,, [AC_MSG_ERROR([Compiler can't find libexpat])])

# Checks for pthreads
ACX_PTHREAD([enable_threads="pthread"],[enable_threads="no"])
if test $enable_threads != "pthread"; then
    AC_MSG_ERROR([unable to find pthreads, currently this is required])
else
    AC_DEFINE(HAVE_PTHREAD,1,[Define if you have POSIX threads libraries and header files.])
    LIBS="$PTHREAD_LIBS $LIBS"
    CFLAGS="$PTHREAD_CFLAGS $CFLAGS"
    CXXFLAGS="$PTHREAD_CFLAGS $CXXFLAGS"
fi


# Checks for typedefs, structures, and compiler characteristics.
AC_HEADER_STDBOOL
AC_C_CONST
AC_TYPE_PID_T
AC_TYPE_SIZE_T
AC_HEADER_TIME

# Checks for library functions.
AC_FUNC_ERROR_AT_LINE
AC_FUNC_FORK
AC_FUNC_MALLOC
AC_HEADER_MAJOR
AC_FUNC_REALLOC
AC_FUNC_VPRINTF
AC_CHECK_FUNCS([gethostname gettimeofday memset strerror])

# Checks for QT library functions.
#gw_CHECK_QT


#-------------------------------------------------------------------------------
#   List of makefiles to generate.  This list can be created with the command:
#   find . -name Makefile.am | egrep '/.+/' | sort | sed 's/.am$//'

AC_CONFIG_FILES([Makefile
    ./engines_gpl/Makefile
    ./engines_gpl/d_hydro/Makefile
    ./engines_gpl/d_hydro/packages/Makefile
    ./engines_gpl/d_hydro/packages/d_hydro/Makefile
    ./engines_gpl/d_hydro/packages/d_hydro/src/Makefile
    ./engines_gpl/flow2d3d/Makefile
    ./engines_gpl/flow2d3d/packages/Makefile
    ./engines_gpl/flow2d3d/packages/data/Makefile
    ./engines_gpl/flow2d3d/packages/data/src/Makefile
    ./engines_gpl/flow2d3d/packages/data/src/allocation/Makefile
    ./engines_gpl/flow2d3d/packages/data/src/basics/Makefile
    ./engines_gpl/flow2d3d/packages/data/src/gdp/Makefile
    ./engines_gpl/flow2d3d/packages/data/src/general/Makefile
    ./engines_gpl/flow2d3d/packages/data/src/parallel_mpi/Makefile
    ./engines_gpl/flow2d3d/packages/flow2d3d/Makefile
    ./engines_gpl/flow2d3d/packages/flow2d3d/src/Makefile
    ./engines_gpl/flow2d3d/packages/flow2d3d/src/dd/Makefile
    ./engines_gpl/flow2d3d/packages/flow2d3d/src/dd/iterators/Makefile
    ./engines_gpl/flow2d3d/packages/flow2d3d/src/dd/mapper/Makefile
    ./engines_gpl/flow2d3d/packages/flow2d3d_openda/Makefile
    ./engines_gpl/flow2d3d/packages/flow2d3d_openda/src/Makefile
    ./engines_gpl/flow2d3d/packages/io/Makefile
    ./engines_gpl/flow2d3d/packages/io/src/Makefile
    ./engines_gpl/flow2d3d/packages/io/src/input/Makefile
    ./engines_gpl/flow2d3d/packages/io/src/output/Makefile
    ./engines_gpl/flow2d3d/packages/io/src/preprocessor/Makefile
    ./engines_gpl/flow2d3d/packages/io_dol_f/Makefile
    ./engines_gpl/flow2d3d/packages/io_dol_f/src/Makefile
    ./engines_gpl/flow2d3d/packages/kernel/Makefile
    ./engines_gpl/flow2d3d/packages/kernel/src/Makefile
    ./engines_gpl/flow2d3d/packages/kernel/src/compute/Makefile
    ./engines_gpl/flow2d3d/packages/kernel/src/compute_nearfar/Makefile
    ./engines_gpl/flow2d3d/packages/kernel/src/compute_roller/Makefile
    ./engines_gpl/flow2d3d/packages/kernel/src/compute_sediment/Makefile
    ./engines_gpl/flow2d3d/packages/kernel/src/dd/Makefile
    ./engines_gpl/flow2d3d/packages/kernel/src/general/Makefile
    ./engines_gpl/flow2d3d/packages/kernel/src/inichk/Makefile
    ./engines_gpl/flow2d3d/packages/kernel/src/main/Makefile
    ./engines_gpl/flow2d3d/packages/kernel/src/non_hydro/Makefile
    ./engines_gpl/flow2d3d/packages/kernel/src/taylor/Makefile
    ./engines_gpl/flow2d3d/packages/kernel/src/timedep/Makefile
    ./engines_gpl/flow2d3d/packages/kernel_dd_f/Makefile
    ./engines_gpl/flow2d3d/packages/kernel_dd_f/src/Makefile
    ./engines_gpl/flow2d3d/packages/manager/Makefile
    ./engines_gpl/flow2d3d/packages/manager/src/Makefile
    ./engines_gpl/flow2d3d/packages/plugin_3dmortrafrm_c/Makefile
    ./engines_gpl/flow2d3d/packages/plugin_3dmortrafrm_c/src/Makefile
    ./engines_gpl/flow2d3d/packages/plugin_user/Makefile
    ./engines_gpl/flow2d3d/packages/plugin_user/src/Makefile
    ./engines_gpl/wave/Makefile
    ./engines_gpl/wave/packages/Makefile
    ./engines_gpl/wave/packages/data/Makefile
    ./engines_gpl/wave/packages/data/src/Makefile
    ./engines_gpl/wave/packages/io/Makefile
    ./engines_gpl/wave/packages/io/src/Makefile
    ./engines_gpl/wave/packages/kernel/Makefile
    ./engines_gpl/wave/packages/kernel/src/Makefile
    ./engines_gpl/wave/packages/manager/Makefile
    ./engines_gpl/wave/packages/manager/src/Makefile
    ./engines_gpl/wave/packages/wave/Makefile
    ./engines_gpl/wave/packages/wave/src/Makefile
    ./plugins_lgpl/Makefile
    ./plugins_lgpl/plugin_culvert/Makefile
    ./plugins_lgpl/plugin_culvert/src/Makefile
    ./plugins_lgpl/plugin_delftflow_traform/Makefile
    ./plugins_lgpl/plugin_delftflow_traform/src/Makefile
    ./third_party_open/Makefile
    ./third_party_open/version_number/Makefile
    ./third_party_open/version_number/packages/Makefile
    ./third_party_open/version_number/packages/version_number/Makefile
    ./third_party_open/version_number/packages/version_number/src/Makefile
    ./tools_gpl/Makefile
    ./tools_gpl/datsel/Makefile
    ./tools_gpl/datsel/packages/Makefile
    ./tools_gpl/datsel/packages/datsel/Makefile
    ./tools_gpl/datsel/packages/datsel/src/Makefile
    ./tools_gpl/datsel/packages/datsel_f/Makefile
    ./tools_gpl/datsel/packages/datsel_f/src/Makefile
    ./tools_gpl/datsel/packages/datsel_version_number/Makefile
    ./tools_gpl/datsel/packages/datsel_version_number/src/Makefile
    ./tools_gpl/kubint/Makefile
    ./tools_gpl/kubint/packages/Makefile
    ./tools_gpl/kubint/packages/kubint/Makefile
    ./tools_gpl/kubint/packages/kubint/src/Makefile
    ./tools_gpl/kubint/packages/kubint_f/Makefile
    ./tools_gpl/kubint/packages/kubint_f/src/Makefile
    ./tools_gpl/kubint/packages/kubint_version_number/Makefile
    ./tools_gpl/kubint/packages/kubint_version_number/src/Makefile
    ./tools_gpl/lint/Makefile
    ./tools_gpl/lint/packages/Makefile
    ./tools_gpl/lint/packages/lint/Makefile
    ./tools_gpl/lint/packages/lint/src/Makefile
    ./tools_gpl/lint/packages/lint_f/Makefile
    ./tools_gpl/lint/packages/lint_f/src/Makefile
    ./tools_gpl/lint/packages/lint_version_number/Makefile
    ./tools_gpl/lint/packages/lint_version_number/src/Makefile
    ./tools_gpl/mormerge/Makefile
    ./tools_gpl/mormerge/packages/Makefile
    ./tools_gpl/mormerge/packages/mormerge/Makefile
    ./tools_gpl/mormerge/packages/mormerge/src/Makefile
    ./tools_gpl/mormerge/packages/mormerge_version_number/Makefile
    ./tools_gpl/mormerge/packages/mormerge_version_number/src/Makefile
    ./tools_gpl/vs/Makefile
    ./tools_gpl/vs/packages/Makefile
    ./tools_gpl/vs/packages/vs/Makefile
    ./tools_gpl/vs/packages/vs/src/Makefile
    ./tools_gpl/vs/packages/vs_version_number/Makefile
    ./tools_gpl/vs/packages/vs_version_number/src/Makefile
    ./utils_lgpl/Makefile
    ./utils_lgpl/constants/Makefile
    ./utils_lgpl/constants/packages/Makefile
    ./utils_lgpl/constants/packages/mathconsts/Makefile
    ./utils_lgpl/constants/packages/mathconsts/src/Makefile
    ./utils_lgpl/delftio/Makefile
    ./utils_lgpl/delftio/packages/Makefile
    ./utils_lgpl/delftio/packages/delftio/Makefile
    ./utils_lgpl/delftio/packages/delftio/src/Makefile
    ./utils_lgpl/delftio/packages/delftio/src/diof90/Makefile
    ./utils_lgpl/delftio/packages/delftio/src/diof90Nefis/Makefile
    ./utils_lgpl/delftio/packages/delftio_shm/Makefile
    ./utils_lgpl/delftio/packages/delftio_shm/src/Makefile
    ./utils_lgpl/delftio/packages/delftio_shm/src/diof90/Makefile
    ./utils_lgpl/delftio/packages/delftio_version_number/Makefile
    ./utils_lgpl/delftio/packages/delftio_version_number/src/Makefile
    ./utils_lgpl/delftio_sync/Makefile
    ./utils_lgpl/delftio_sync/packages/Makefile
    ./utils_lgpl/delftio_sync/packages/delftio_sync/Makefile
    ./utils_lgpl/delftio_sync/packages/delftio_sync/src/Makefile
    ./utils_lgpl/delftio_sync/packages/delftio_sync_version_number/Makefile
    ./utils_lgpl/delftio_sync/packages/delftio_sync_version_number/src/Makefile
    ./utils_lgpl/d_hydro_lib/Makefile
    ./utils_lgpl/d_hydro_lib/packages/Makefile
    ./utils_lgpl/d_hydro_lib/packages/d_hydro_lib/Makefile
    ./utils_lgpl/d_hydro_lib/packages/d_hydro_lib/src/Makefile
    ./utils_lgpl/ec_module/Makefile
    ./utils_lgpl/ec_module/packages/Makefile
    ./utils_lgpl/ec_module/packages/ec_module/Makefile
    ./utils_lgpl/ec_module/packages/ec_module/src/Makefile
    ./utils_lgpl/ec_module/packages/ec_module/src/ectest/Makefile
    ./utils_lgpl/ec_module/packages/ec_module_version_number/Makefile
    ./utils_lgpl/ec_module/packages/ec_module_version_number/src/Makefile
    ./utils_lgpl/esmfsm/Makefile
    ./utils_lgpl/esmfsm/packages/Makefile
    ./utils_lgpl/esmfsm/packages/esmfsm/Makefile
    ./utils_lgpl/esmfsm/packages/esmfsm/src/Makefile
    ./utils_lgpl/esmfsm/packages/esmfsm/src/fsm/Makefile
    ./utils_lgpl/esmfsm/packages/esmfsm_c/Makefile
    ./utils_lgpl/esmfsm/packages/esmfsm_c/src/Makefile
    ./utils_lgpl/esmfsm/packages/esmfsm_c/src/esm/Makefile
    ./utils_lgpl/esmfsm/packages/esmfsm_c/src/fsm/Makefile
    ./utils_lgpl/esmfsm/packages/esmfsm_version_number/Makefile
    ./utils_lgpl/esmfsm/packages/esmfsm_version_number/src/Makefile
    ./utils_lgpl/esmfsm/tests/Makefile
    ./utils_lgpl/esmfsm/tests/allocate_local/Makefile
    ./utils_lgpl/esmfsm/tests/esm_create/Makefile
    ./utils_lgpl/esmfsm/tests/esm_delete/Makefile
    ./utils_lgpl/esmfsm/tests/esm_info/Makefile
    ./utils_lgpl/esmfsm/tests/test_01/Makefile
    ./utils_lgpl/esmfsm/tests/test_02/Makefile
    ./utils_lgpl/esmfsm/tests/test_03/Makefile
    ./utils_lgpl/esmfsm/tests/testesm/Makefile
    ./utils_lgpl/esmfsm/tests/testfill/Makefile
    ./utils_lgpl/esmfsm/tests/testfsm/Makefile
    ./utils_lgpl/ftnunit/Makefile
    ./utils_lgpl/ftnunit/packages/Makefile
    ./utils_lgpl/ftnunit/packages/ftnunit/Makefile
    ./utils_lgpl/ftnunit/packages/ftnunit/src/Makefile
    ./utils_lgpl/inifiles/Makefile
    ./utils_lgpl/inifiles/packages/Makefile
    ./utils_lgpl/inifiles/packages/inifiles/Makefile
    ./utils_lgpl/inifiles/packages/inifiles/src/Makefile
    ./utils_lgpl/inifiles/tests/01_initest/Makefile
    ./utils_lgpl/inifiles/tests/02_treetest/Makefile
    ./utils_lgpl/inifiles/tests/03_dredgedumptest/Makefile
    ./utils_lgpl/meteo/Makefile
    ./utils_lgpl/meteo/packages/Makefile
    ./utils_lgpl/meteo/packages/meteo/Makefile
    ./utils_lgpl/meteo/packages/meteo/src/Makefile
    ./utils_lgpl/meteo/packages/meteo_version_number/Makefile
    ./utils_lgpl/meteo/packages/meteo_version_number/src/Makefile
    ./utils_lgpl/nefis/Makefile
    ./utils_lgpl/nefis/packages/Makefile
    ./utils_lgpl/nefis/packages/nefis/Makefile
    ./utils_lgpl/nefis/packages/nefis/src/Makefile
    ./utils_lgpl/nefis/packages/nefis/src_so/Makefile
    ./utils_lgpl/nefis/packages/nefis_version_number/Makefile
    ./utils_lgpl/nefis/packages/nefis_version_number/src/Makefile
    ./utils_lgpl/nefis/packages/nefis_version_number/src_so/Makefile
    ./utils_lgpl/nefis/tests/Makefile
    ./utils_lgpl/nefis/tests/demo_00/Makefile
    ./utils_lgpl/nefis/tests/demo_01/Makefile
    ./utils_lgpl/nefis/tests/demo_02/Makefile
    ./utils_lgpl/nefis/tests/observations/Makefile
    ./utils_lgpl/nefis/tests/test_00/Makefile
    ./utils_lgpl/nefis/tests/test_01/Makefile
    ./utils_lgpl/nefis/tests/test_02/Makefile
    ./utils_lgpl/nefis/tests/test_03/Makefile
    ./utils_lgpl/nefis/tests/test_04/Makefile
    ./utils_lgpl/nefis/tests/test_05/Makefile
    ./utils_lgpl/nefis/tests/test_06/Makefile
    ./utils_lgpl/nefis/tests/test_07/Makefile
    ./utils_lgpl/nefis/tests/test_08/Makefile
    ./utils_lgpl/nefis/tests/test_09/Makefile
    ./utils_lgpl/nefis/tests/test_10/Makefile
    ./utils_lgpl/nefis/tests/test_11/Makefile
    ./utils_lgpl/nefis/tests/test_12/Makefile
    ./utils_lgpl/nefis/tests/test_13/Makefile
    ./utils_lgpl/nefis/tests/test_14/Makefile
    ./utils_lgpl/nefis/tests/test_15/Makefile
    ./utils_lgpl/nefis/tests/test_16/Makefile
    ./utils_lgpl/nefis/tests/test_17/Makefile
    ./utils_lgpl/nefis/tests/test_18/Makefile
    ./utils_lgpl/nefis/tests/test_19/Makefile
    ./utils_lgpl/precision/Makefile
    ./utils_lgpl/precision/packages/Makefile
    ./utils_lgpl/precision/packages/precision/Makefile
    ./utils_lgpl/precision/packages/precision/src/Makefile
    ./utils_lgpl/semaphore/Makefile
    ./utils_lgpl/semaphore/packages/Makefile
    ./utils_lgpl/semaphore/packages/semaphore/Makefile
    ./utils_lgpl/semaphore/packages/semaphore/src/Makefile
    ./utils_lgpl/stream/Makefile
    ./utils_lgpl/stream/packages/Makefile
    ./utils_lgpl/stream/packages/stream/Makefile
    ./utils_lgpl/stream/packages/stream/src/Makefile
    ./utils_lgpl/stream/packages/stream_version_number/Makefile
    ./utils_lgpl/stream/packages/stream_version_number/src/Makefile
    ./utils_lgpl/stream/tests/01-smalltest/Makefile
    ./utils_lgpl/stream/tests/02-streamtest/Makefile
    ./utils_lgpl/stream/tests/Makefile
    ./utils_lgpl/utils_flow/Makefile
    ./utils_lgpl/utils_flow/packages/Makefile
    ./utils_lgpl/utils_flow/packages/utils_flow/Makefile
    ./utils_lgpl/utils_flow/packages/utils_flow/src/Makefile
    ./utils_lgpl/utils_flow/packages/utils_flow_c/Makefile
    ./utils_lgpl/utils_flow/packages/utils_flow_c/src/Makefile
    ./utils_lgpl/utils_flow/packages/utils_flow_version_number/Makefile
    ./utils_lgpl/utils_flow/packages/utils_flow_version_number/src/Makefile
    ])

AC_OUTPUT
