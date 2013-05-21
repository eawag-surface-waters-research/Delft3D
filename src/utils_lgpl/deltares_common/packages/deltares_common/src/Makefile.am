#-------------------------------------------------------------------------------
#   Makefile for delftflow
#
#   Irv.Elshoff@deltares.nl
#   Adri.Mourits@deltares.nl
#   Fedor.Baart@deltares.nl
#   Gennadii.Donchyts@deltares.nl
#   11 jan 2007
#
#   Copyright (C)  Stichting Deltares, 2011-2013.
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am

SUBDIRS = \
    rd_token

noinst_LTLIBRARIES = libDeltaresCommon.la

#-------------------------------------------------------------------------------
#Includes


#-------------------------------------------------------------------------------
#Sources
libDeltaresCommon_la_SOURCES = \
         precision_basics.F90 \
         precision.F90 \
         precision_sp.F90 \
         mathconsts.f90 \
         tree_struct.f90 \
         buffer.f90 \
         bilin5.f90 \
         combinepaths.f90 \
         deltares_common_version.F90 \
         distance.f90 \
         juldat.f90 \
         handles.f90 \
         malloc.f90 \
         MessageHandling.f90 \
         nan_check_module.f90 \
         newunit.f90 \
         properties.f90 \
         scannr.f90 \
         skipstarlines.f90 \
         small.f90 \
         stringsequal.f90 \
         tables.f90 \
         table_handles.f90 \
         timers.f \
         util.f90


EXTRA_DIST = \
         deltares_common_version.F90.svn

#-------------------------------------------------------------------------------
#Libraries

libDeltaresCommon_la_LIBADD=\
	$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common_c/src/libDeltaresCommonC.la \
	$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src/rd_token/libRDToken.la


BUILT_SOURCES = ./always \
	./deltares_common_version.F90

./always:
	rm -rf ./deltares_common_version.F90

./deltares_common_version.F90:
	$(top_srcdir)/scripts_lgpl/linux/update_version.sh $(srcdir)/deltares_common_version.F90 $(srcdir) ../include/version_number.ini $(top_buildir)
