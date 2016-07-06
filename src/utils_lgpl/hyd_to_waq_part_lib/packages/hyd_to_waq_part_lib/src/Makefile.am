#-------------------------------------------------------------------------------
#   Makefile io_netcdf
#
#   14 jun 2016
#
#   Copyright (C)  Stichting Deltares, 2011-2016.
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
noinst_LTLIBRARIES = libHydToWaqPartLib.la

#-------------------------------------------------------------------------------
#Includes
AM_FCFLAGS += $(NETCDF_CFLAGS) $(NETCDF_FORTRAN_CFLAGS)

FMODULES += \
	-I$(NETCDFROOT)/include \
    -I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src \
    -I$(top_builddir)/utils_lgpl/io_netcdf/packages/io_netcdf/src

#-------------------------------------------------------------------------------
#Sources
libHydToWaqPartLib_la_SOURCES = \
         agwaqgeom.f90 \
         wrwaqgeomcl.f90

#-------------------------------------------------------------------------------
#Libraries

