#-------------------------------------------------------------------------------
#   Makefile io_netcdf
#
#   14 jun 2016
#
#   Copyright (C)  Stichting Deltares, 2011-2016.
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
noinst_LTLIBRARIES = libIONetcdf.la

#-------------------------------------------------------------------------------
#Includes
AM_FCFLAGS += $(NETCDF_CFLAGS) $(NETCDF_FORTRAN_CFLAGS)

FMODULES += \
	-I$(NETCDFROOT)/include

#-------------------------------------------------------------------------------
#Sources
libIONetcdf_la_SOURCES = \
         io_ugrid.f90

#-------------------------------------------------------------------------------
#Libraries

