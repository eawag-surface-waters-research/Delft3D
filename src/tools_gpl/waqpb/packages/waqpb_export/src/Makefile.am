#-------------------------------------------------------------------------------
#   Makefile for delwaq
#
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
lib_LTLIBRARIES = libwaqpb.la

#AM_FCFLAGS += -fopenmp $(OPENMP_FCFLAGS)
#AM_FFLAGS += -fopenmp $(OPENMP_FFLAGS)
#LDFLAGS += -fopenmp

#-------------------------------------------------------------------------------
#Includes
FMODULES += \
        -I$(top_builddir)/tools_gpl/waqpb/packages/include

#-------------------------------------------------------------------------------
#Sources
waqpb_export_SOURCES = \
       waqpb_export.f

#-------------------------------------------------------------------------------
# Libraries

waqpb_export_LIBADD = \
    $(top_builddir)/tools_lgpl/waqpb/packages/waqpb_lib/src/libwaqpb.la \
    $(top_builddir)/utils_lgpl/nefis/packages/nefis/src/libNefis.la
