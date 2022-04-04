#-------------------------------------------------------------------------------
#   Makefile for delpar
#
#   jan.mooiman@deltares.nl
#   09 Feb 2009
#
#   Copyright (C) 2009, Deltares
#-------------------------------------------------------------------------------
include $(top_srcdir)/common.am

MYC_INCLUDES=$(top_srcdir)/include

bin_PROGRAMS= delpar

AM_FCFLAGS += -fopenmp $(OPENMP_FCFLAGS)
AM_FFLAGS += -fopenmp $(OPENMP_FFLAGS)
LDFLAGS += -fopenmp

#-------------------------------------------------------------------------------
#Includes
FINCLUDES += \
    -I$(top_builddir)/utils_lgpl/io_netcdf/packages/io_netcdf/src \
    -I$(top_builddir)/utils_lgpl/io_hyd/packages/io_hyd/src \
    -I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common_c/src \
    -I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src \
    -I$(top_builddir)/engines_gpl/waq/packages/waq_utils_f/src/waq_utils \ 
    -I$(top_builddir)/utils_lgpl/io_hyd/packages/io_hyd/src \
    -I$(NETCDFROOT)/include

FMODULES += \
    -I$(top_builddir)/utils_lgpl/io_netcdf/packages/io_netcdf/src \
    -I$(top_builddir)/utils_lgpl/io_hyd/packages/io_hyd/src \
    -I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common_c/src \
    -I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src \
    -I$(top_builddir)/engines_gpl/waq/packages/waq_utils_f/src/waq_utils \ 
    -I$(NETCDF_FORTRAN_LIBS)

AM_FCFLAGS += $(FINCLUDES) $(FMODULES) $(NETCDF_CFLAGS) $(NETCDF_FORTRAN_CFLAGS)
AM_FFLAGS  += $(FINCLUDES) $(FMODULES) ${NETCDF_CFLAGS} ${NETCDF_FORTRAN_CFLAGS}  


delpar_SOURCES= \
	main.f

# some libraries are loaded twice to find all undefined symbols

delpar_LDADD =\
   $(top_srcdir)/engines_gpl/waq/packages/waq_utils_f/src/waq_utils/libwaq_utils.la \
   $(top_srcdir)/utils_lgpl/io_netcdf/packages/io_netcdf/src/libIONetcdf.la \
   $(top_srcdir)/utils_lgpl/io_hyd/packages/io_hyd/src/libIOHyd.la \
   $(top_srcdir)/engines_gpl/part/packages/kernel_f/src/libkernel_f.la \
   $(top_srcdir)/engines_gpl/part/packages/io_f/src/libio_f.la \
   $(top_srcdir)/engines_gpl/part/packages/utils_f/src/libutils_f.la \
   $(top_srcdir)/engines_gpl/part/packages/data_f/src/libdata_f.la \
   $(top_srcdir)/utils_lgpl/deltares_common/packages/deltares_common/src/rd_token/libRDToken.la \
   $(top_srcdir)/utils_lgpl/deltares_common/packages/deltares_common/src/libDeltaresCommon.la \
   $(top_srcdir)/utils_lgpl/nefis/packages/nefis/src/libNefis.la \
   -ldl \
   $(NETCDF_LIBS) $(NETCDF_FORTRAN_LIBS) -lnetcdff \
   $(FCLIBS) \
    -lpthread

