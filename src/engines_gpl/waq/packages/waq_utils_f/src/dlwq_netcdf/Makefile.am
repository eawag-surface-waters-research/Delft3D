#-------------------------------------------------------------------------------
#   Makefile for delwaq
#
#   arjen.markus@deltares.nl
#   21 august 2008
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
noinst_LTLIBRARIES = libdlwq_netcdf.la

#-------------------------------------------------------------------------------
#Includes
FMODULES += \
        -I${NETCDFROOT}/include \
        -I$(top_builddir)/engines_gpl/waq/include 

AM_FCFLAGS += $(FINCLUDES) $(FMODULES) ${NETCDF_CFLAGS} ${NETCDF_FORTRAN_CFLAGS}

#-------------------------------------------------------------------------------
#Sources
libdlwq_netcdf_la_SOURCES = \
       dlwq_netcdf.F90

#-------------------------------------------------------------------------------
# Libraries
