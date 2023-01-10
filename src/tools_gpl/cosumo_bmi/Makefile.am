#-------------------------------------------------------------------------------
#   Makefile for delftflow
#
#   Copyright (C)  Stichting Deltares, 2011-2013.
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am

lib_LTLIBRARIES = libcosumo_bmi.la


#-------------------------------------------------------------------------------
#Includes
FMODULES += \
	-I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src \
	-I$(top_builddir)/utils_lgpl/ec_module/packages/ec_module/src

AM_CPPFLAGS = \
    $(STANDARD_C_INCLUDES) \
    -I../include

#-------------------------------------------------------------------------------
#Sources
libcosumo_bmi_la_SOURCES = \
	nfl_data.f90 \
	coupled.f90 \
	determine_densprof.f90 \
	read_cosumo_file.f90 \
	write_ff2nf_files.f90 \
	read_nf2ff_files.f90 \
	cosumo_bmi.f90


#-------------------------------------------------------------------------------
#Libraries

EXTRALIBS = \
$(top_builddir)/utils_lgpl/ec_module/packages/ec_module/src/libECModule.la \
$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src/libDeltaresCommon.la \
$(top_builddir)/utils_lgpl/kdtree_wrapper/packages/kdtree_wrapper/src/libkdtreewrapper.la \
$(top_builddir)/third_party_open/kdtree2/src-f90/libkdtree2f.la \
$(top_builddir)/third_party_open/triangle/libtriangle_c.la \
$(FCLIBS) $(CCLIBS)

#$(EXTERNALSOURCES)

libcosumo_bmi_la_LIBADD   = \
        $(EXTRALIBS)

