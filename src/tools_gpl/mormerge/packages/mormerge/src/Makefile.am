#-------------------------------------------------------------------------------
#   Makefile for delftflow
#
#   Copyright (C)  Stichting Deltares, 2011-2013.
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am

bin_PROGRAMS = mormerge

#-------------------------------------------------------------------------------
#Includes

FMODULES += \
	-I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src 

#-------------------------------------------------------------------------------
#Sources
mormerge_SOURCES = \
         mormerge_version.F90 \
         mormerge.f90 \
         merge.f90

#-------------------------------------------------------------------------------
#Libraries

mormerge_LDADD   = \
$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src/libDeltaresCommon.la  \
$(FCLIBS) $(CCLIBS)

mormerge_version.F90: mormerge_version.F90.svn .FORCE
	$(top_builddir)/scripts_lgpl/linux/update_version.sh \
	$(builddir)/mormerge_version.F90 $(srcdir) $(builddir)/../../../res/version_number.ini $(abs_top_builddir)
	
.FORCE: 

.PHONY: .FORCE
