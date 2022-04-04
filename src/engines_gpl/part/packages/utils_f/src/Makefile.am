#-------------------------------------------------------------------------------
#   Makefile for delpar
#
#   jan.mooiman@deltares.nl
#   09 Feb 2009
#
#   Copyright (C) 2009, Deltares
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
noinst_LTLIBRARIES = libutils_f.la

AM_FCFLAGS += $(OPENMP_FCFLAGS)
AM_FFLAGS += $(OPENMP_FFLAGS)

#-------------------------------------------------------------------------------
#Includes
FINCLUDES += \
    -I$(top_builddir)/utils_lgpl/io_netcdf/packages/io_netcdf/src \
    -I$(top_builddir)/utils_lgpl/io_hyd/packages/io_hyd/src \
    -I$(NETCDFROOT)/include

FMODULES += \
	-I../../data_f/src \
        -I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src/rd_token \
        -I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src \
        -I$(top_builddir)/utils_lgpl/io_netcdf/packages/io_netcdf/src \
        -I$(top_builddir)/utils_lgpl/io_hyd/packages/io_hyd/src \
        -I$(NETCDF_FORTRAN_LIBS)

AM_FCFLAGS += $(FINCLUDES) $(FMODULES) $(NETCDF_CFLAGS) $(NETCDF_FORTRAN_CFLAGS)
AM_FFLAGS  += $(FINCLUDES) $(FMODULES) ${NETCDF_CFLAGS} ${NETCDF_FORTRAN_CFLAGS}  

#-------------------------------------------------------------------------------
#Sources
libutils_f_la_SOURCES = \
         part_version.F90 \
         write_version.f90 \
         stop_exit.f90 \
         normal_mod.f90 \
         lower_case.f90 \
         get_key.f90 \
         pinpok.f90 \
         polpart.f90 \
         boombounce.f90 \
         show_error.f90 \
         noextspaces.f90 \
         get_index.f90 \
         strip_inputfile.f90 \
         random_gen.f90 \
         part11.f90 \
         part11sp.f90 \
         grid_search.f90 \
         skip_comments.f90\
         skip_substances.f90

#-------------------------------------------------------------------------------
#Libraries

BUILT_SOURCES = ./always \
	./part_version.F90

./always:
	rm -rf ./part_version.F90

./part_version.F90:
	$(top_srcdir)/scripts_lgpl/linux/update_version.sh $(srcdir)/part_version.F90 $(srcdir) $(top_srcdir)/engines_gpl/part/version/version_number.ini $(top_builddir)

