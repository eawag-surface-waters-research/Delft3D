#-------------------------------------------------------------------------------
#   Makefile for delpar
#
#   jan.mooiman@deltares.nl
#   09 Feb 2009
#
#   Copyright (C) 2009, Deltares
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
noinst_LTLIBRARIES = libdata_f.la

AM_FCFLAGS += -fopenmp $(OPENMP_FCFLAGS)
AM_FFLAGS += -fopenmp $(OPENMP_FFLAGS)

#-------------------------------------------------------------------------------
#Includes
FINCLUDES += \
    -I$(top_builddir)/utils_lgpl/io_netcdf/packages/io_netcdf/src \
    -I$(top_builddir)/utils_lgpl/io_hyd/packages/io_hyd/src \
    -I$(NETCDFROOT)/include

FMODULES += \
        -I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src/rd_token \
        -I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src \
        -I$(top_builddir)/utils_lgpl/io_netcdf/packages/io_netcdf/src \
        -I$(top_builddir)/utils_lgpl/io_hyd/packages/io_hyd/src \
        -I$(NETCDF_FORTRAN_LIBS)

AM_FCFLAGS += $(FINCLUDES) $(FMODULES) $(NETCDF_CFLAGS) $(NETCDF_FORTRAN_CFLAGS)
AM_FFLAGS  += $(FINCLUDES) $(FMODULES) ${NETCDF_CFLAGS} ${NETCDF_FORTRAN_CFLAGS}

#-------------------------------------------------------------------------------
#Sources
libdata_f_la_SOURCES = \
         precision.f90 \
         m_part_modeltypes.f90 \
         typos_mod.f90 \
         file_info.f90 \
         modeldim.f90 \
         partmem.f90 \
         partfmmem.f90 \
         alloc.f90

#-------------------------------------------------------------------------------
#Libraries

